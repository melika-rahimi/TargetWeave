json_http_response <- function(payload, url, status = 200L) {
  httr2::response(
    status_code = as.integer(status),
    url = url,
    method = "GET",
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")))
  )
}

egfr_identity_perform <- function(ensembl_delay = 0.15, fail_ensembl = FALSE) {
  uniprot <- read_fixture("uniprot_egfr.json")
  ensembl <- read_fixture("ensembl_egfr.json")
  perform <- function(req) {
    url <- as.character(req$url %||% "")
    if (grepl("ensembl\\.org", url, ignore.case = TRUE)) {
      return(promises::promise(function(resolve, reject) {
        later::later(function() {
          if (isTRUE(fail_ensembl)) {
            reject(simpleError("Timeout was reached"))
          } else {
            resolve(json_http_response(ensembl, url))
          }
        }, delay = ensembl_delay)
      }))
    }
    json_http_response(uniprot, url)
  }
  list(perform = perform)
}

open_session <- function() {
  list(isClosed = function() FALSE)
}

reactive_inflight <- function() {
  shiny::reactiveVal(list())
}

start_key <- function(inflight, key) {
  isolate(inflight_start(inflight, key))
}

# Documents the exact Docker failure: Shiny GET of reactiveVal from a later
# callback has no reactive consumer. inflight_matches() does store() GET.
test_that("inflight_matches reads reactiveVal and throws without isolate", {
  inflight <- shiny::reactiveVal(list(egfr = list(token = "tok-egfr")))
  expect_error(
    inflight_matches(inflight, "egfr", "tok-egfr"),
    "active reactive context"
  )
  expect_true(isolate(inflight_matches(inflight, "egfr", "tok-egfr")))
})

test_that("A promise completion outside an active reactive consumer does not throw", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  done <- FALSE
  err <- NULL
  tryCatch(
    {
      bind_external_task(
        promises::promise(function(resolve, reject) {
          later::later(function() resolve(list(status = "ok")), 0.05)
        }),
        open_session(),
        function(result) {
          stopifnot(inflight_matches(inflight, "egfr-id", token))
          done <<- identical(result$status, "ok")
        },
        inflight = inflight,
        key = "egfr-id",
        token = token
      )
      drain_tw_later(2)
    },
    error = function(e) {
      err <<- conditionMessage(e)
    }
  )
  expect_null(err)
  expect_false(grepl("Unhandled promise error", err %||% "", fixed = TRUE))
  expect_true(done)
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
})

test_that("B EGFR and KRAS concurrent identity tasks both settle", {
  inflight <- reactive_inflight()
  selected <- shiny::reactiveVal("kras-id")
  stored <- list()
  egfr_token <- start_key(inflight, "egfr-id")
  kras_token <- start_key(inflight, "kras-id")

  bind_identity <- function(tid, token, delay) {
    mock <- egfr_identity_perform(ensembl_delay = delay)
    bind_external_task(
      resolve_target_identity("EGFR", db_pool = NULL, perform = mock$perform),
      open_session(),
      function(result) {
        if (!inflight_matches(inflight, tid, token)) {
          return()
        }
        inflight_clear(inflight, tid, token)
        stored[[tid]] <<- result
        apply <- identity_task_apply(selected(), tid)
        if (isTRUE(apply$update_visible_panel)) {
          selected(tid)
        }
      },
      error_message = "Identity lookup could not be completed.",
      inflight = inflight,
      key = tid,
      token = token
    )
  }

  bind_identity("egfr-id", egfr_token, 0.25)
  bind_identity("kras-id", kras_token, 0.08)
  drain_tw_later(4)

  expect_false(isolate(inflight_has(inflight, "egfr-id")))
  expect_false(isolate(inflight_has(inflight, "kras-id")))
  expect_true(!is.null(stored[["egfr-id"]]))
  expect_true(!is.null(stored[["kras-id"]]))
})

test_that("C late EGFR does not overwrite current KRAS selection", {
  inflight <- reactive_inflight()
  selected <- shiny::reactiveVal("kras-id")
  egfr_token <- start_key(inflight, "egfr-id")
  kras_token <- start_key(inflight, "kras-id")

  bind_identity <- function(tid, token, delay) {
    mock <- egfr_identity_perform(ensembl_delay = delay)
    bind_external_task(
      resolve_target_identity("EGFR", db_pool = NULL, perform = mock$perform),
      open_session(),
      function(result) {
        if (!inflight_matches(inflight, tid, token)) {
          return()
        }
        inflight_clear(inflight, tid, token)
        apply <- identity_task_apply(selected(), tid)
        if (isTRUE(apply$update_visible_panel)) {
          selected(tid)
        }
      },
      inflight = inflight,
      key = tid,
      token = token
    )
  }

  bind_identity("egfr-id", egfr_token, 0.2)
  bind_identity("kras-id", kras_token, 0.05)
  drain_tw_later(3)
  expect_identical(isolate(selected()), "kras-id")
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
})

test_that("D success clears inflight", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  mock <- egfr_identity_perform(ensembl_delay = 0.05)
  bind_external_task(
    resolve_target_identity("EGFR", db_pool = NULL, perform = mock$perform),
    open_session(),
    function(result) {
      if (!inflight_matches(inflight, "egfr-id", token)) {
        return()
      }
      inflight_clear(inflight, "egfr-id", token)
    },
    inflight = inflight,
    key = "egfr-id",
    token = token
  )
  drain_tw_later(3)
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
})

test_that("E HTTP failure clears inflight and uses the user-facing error", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  got <- NULL
  bind_external_task(
    promises::promise(function(resolve, reject) {
      later::later(function() reject(simpleError("HTTP 503 Service Unavailable")), 0.05)
    }),
    open_session(),
    function(result) {
      got <<- result
    },
    error_message = "Identity lookup could not be completed.",
    inflight = inflight,
    key = "egfr-id",
    token = token
  )
  drain_tw_later(2)
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
  expect_true(is_task_error(got))
  expect_identical(got$message, "Identity lookup could not be completed.")
  expect_false(grepl("HTTP 503", got$message, fixed = TRUE))
})

test_that("F timeout clears inflight", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  bind_external_task(
    promises::promise(function(resolve, reject) {
      later::later(function() reject(simpleError("Timeout was reached")), 0.05)
    }),
    open_session(),
    function(result) NULL,
    error_message = "Identity lookup could not be completed.",
    inflight = inflight,
    key = "egfr-id",
    token = token
  )
  drain_tw_later(2)
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
})

test_that("G partial_uniprot clears inflight", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  holder <- NULL
  mock <- egfr_identity_perform(ensembl_delay = 0.05, fail_ensembl = TRUE)
  bind_external_task(
    resolve_target_identity("EGFR", db_pool = NULL, perform = mock$perform),
    open_session(),
    function(result) {
      holder <<- result
      if (!inflight_matches(inflight, "egfr-id", token)) {
        return()
      }
      inflight_clear(inflight, "egfr-id", token)
    },
    inflight = inflight,
    key = "egfr-id",
    token = token
  )
  drain_tw_later(3)
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
  expect_false(is_task_error(holder))
  cross <- vapply(holder$candidates, function(c) c$cross_check %||% "", character(1))
  expect_true(any(cross == "partial_uniprot"))
})

test_that("H stale token completion cannot clear a newer task", {
  inflight <- reactive_inflight()
  old_token <- start_key(inflight, "egfr-id")
  new_token <- start_key(inflight, "egfr-id")
  expect_false(identical(old_token, new_token))

  bind_external_task(
    promises::promise(function(resolve, reject) {
      later::later(function() resolve(list(status = "stale")), 0.05)
    }),
    open_session(),
    function(result) {
      if (!inflight_matches(inflight, "egfr-id", old_token)) {
        return()
      }
      inflight_clear(inflight, "egfr-id", old_token)
    },
    inflight = inflight,
    key = "egfr-id",
    token = old_token
  )
  drain_tw_later(2)
  expect_true(isolate(inflight_has(inflight, "egfr-id")))
  expect_identical(isolate(inflight_token(inflight, "egfr-id")), new_token)
})

test_that("I promise rejection is handled and does not surface as unhandled", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  drain_error <- NULL
  tryCatch(
    {
      bind_external_task(
        promises::promise(function(resolve, reject) {
          later::later(function() reject(simpleError("curl failed")), 0.05)
        }),
        open_session(),
        function(result) {
          stopifnot(is_task_error(result))
        },
        error_message = "Identity lookup could not be completed.",
        inflight = inflight,
        key = "egfr-id",
        token = token
      )
      drain_tw_later(2)
    },
    error = function(e) {
      drain_error <<- conditionMessage(e)
    }
  )
  expect_null(drain_error)
  expect_false(grepl("Unhandled promise error", drain_error %||% "", fixed = TRUE))
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
})

test_that("on_value throw still settles inflight", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  drain_error <- NULL
  tryCatch(
    {
      bind_external_task(
        promises::promise(function(resolve, reject) {
          later::later(function() resolve(list(ok = TRUE)), 0.05)
        }),
        open_session(),
        function(result) {
          inflight()
          stop("callback exploded")
        },
        inflight = inflight,
        key = "egfr-id",
        token = token
      )
      drain_tw_later(2)
    },
    error = function(e) {
      drain_error <<- conditionMessage(e)
    }
  )
  expect_null(drain_error)
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
})

test_that("J literature completion can read reactiveVal inflight outside a consumer", {
  skip_if_not_installed("later")
  inflight <- reactive_inflight()
  token <- start_key(inflight, "literature")
  done <- FALSE
  project <- nsclc_literature_project()
  targets <- rbind(
    confirmed_reactome_target("t1", "EGFR", "P00533", ensembl = "ENSG00000146648")
  )
  inner <- ncbi_fixture_perform()
  delayed <- function(req) {
    promises::promise(function(resolve, reject) {
      later::later(function() resolve(inner(req)), 0.08)
    })
  }
  bind_external_task(
    retrieve_project_literature(
      project,
      targets,
      db_pool = NULL,
      perform = delayed,
      uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
    ),
    open_session(),
    function(result) {
      if (!inflight_matches(inflight, "literature", token)) {
        return()
      }
      inflight_clear(inflight, "literature", token)
      done <<- is.list(result)
    },
    error_message = "Literature could not be retrieved.",
    inflight = inflight,
    key = "literature",
    token = token
  )
  drain_tw_later(6)
  expect_true(done)
  expect_false(isolate(inflight_has(inflight, "literature")))
})

test_that("closed session still clears inflight", {
  inflight <- reactive_inflight()
  token <- start_key(inflight, "egfr-id")
  applied <- FALSE
  bind_external_task(
    promises::promise(function(resolve, reject) {
      later::later(function() resolve(list(status = "ok")), 0.05)
    }),
    list(isClosed = function() TRUE),
    function(result) {
      applied <<- TRUE
    },
    inflight = inflight,
    key = "egfr-id",
    token = token
  )
  drain_tw_later(2)
  expect_false(applied)
  expect_false(isolate(inflight_has(inflight, "egfr-id")))
})
