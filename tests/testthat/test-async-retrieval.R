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
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  perform <- function(req) {
    calls$n <- calls$n + 1L
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
  list(perform = perform, calls = calls)
}

delayed_ncbi_perform <- function(delay = 0.2) {
  inner <- ncbi_fixture_perform()
  started <- FALSE
  function(req) {
    if (isTRUE(started)) {
      return(inner(req))
    }
    started <<- TRUE
    promises::promise(function(resolve, reject) {
      later::later(function() resolve(inner(req)), delay = delay)
    })
  }
}

test_that("A/B delayed Ensembl does not change selected target; EGFR stores only for EGFR", {
  mock <- egfr_identity_perform(ensembl_delay = 0.2)
  selected <- "kras-id"
  egfr_id <- "egfr-id"
  stored <- list()

  pending <- resolve_target_identity("EGFR", db_pool = NULL, perform = mock$perform)
  expect_true(is_tw_promise(pending))
  selected <- "kras-id"

  bind_external_task(
    pending,
    list(isClosed = function() FALSE),
    function(result) {
      apply <- identity_task_apply(selected, egfr_id)
      stored[[apply$store_for_target]] <<- result
      if (isTRUE(apply$update_visible_panel)) {
        selected <<- egfr_id
      }
    }
  )

  drain_tw_later(3)
  expect_identical(selected, "kras-id")
  expect_true(!is.null(stored[["egfr-id"]]))
  expect_null(stored[["kras-id"]])
  expect_true(stored[["egfr-id"]]$status %in% c("unresolved", "ambiguous", "failed"))
  expect_true(length(stored[["egfr-id"]]$candidates) >= 1L)
})

test_that("C duplicate in-flight identity lookup launches one retrieval", {
  box <- new.env(parent = emptyenv())
  box$data <- list()
  store <- function(value) {
    if (missing(value)) {
      box$data
    } else {
      box$data <<- value
      box$data
    }
  }
  mock <- egfr_identity_perform(ensembl_delay = 0.2)
  started <- 0L
  start_once <- function() {
    if (inflight_has(store, "egfr-id")) {
      return()
    }
    inflight_start(store, "egfr-id")
    started <<- started + 1L
    resolve_target_identity("EGFR", db_pool = NULL, perform = mock$perform)
  }
  start_once()
  start_once()
  expect_equal(started, 1L)
  drain_tw_later(3)
})

test_that("D Ensembl timeout keeps partial_uniprot and does not block later work", {
  mock <- egfr_identity_perform(ensembl_delay = 0.1, fail_ensembl = TRUE)
  selected <- "kras-id"
  done <- FALSE
  result_holder <- NULL
  bind_external_task(
    resolve_target_identity("EGFR", db_pool = NULL, perform = mock$perform),
    list(isClosed = function() FALSE),
    function(result) {
      result_holder <<- result
      done <<- TRUE
    }
  )
  selected <- "kras-id"
  drain_tw_later(3)
  expect_true(done)
  expect_identical(selected, "kras-id")
  expect_false(is_task_error(result_holder))
  cross <- vapply(result_holder$candidates, function(c) c$cross_check %||% "", character(1))
  expect_true(any(cross == "partial_uniprot"))
})

test_that("E slow literature retrieval leaves unrelated navigation state writable", {
  skip_if_not_installed("later")
  project <- nsclc_literature_project()
  targets <- rbind(
    confirmed_reactome_target("t1", "EGFR", "P00533", ensembl = "ENSG00000146648")
  )
  workspace_panel <- "literature"
  done <- FALSE
  pending <- retrieve_project_literature(
    project,
    targets,
    db_pool = NULL,
    perform = delayed_ncbi_perform(0.25),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  expect_true(is_tw_promise(pending))
  workspace_panel <- "overview"
  bind_external_task(
    pending,
    list(isClosed = function() FALSE),
    function(result) {
      done <<- is.list(result)
    }
  )
  drain_tw_later(6)
  expect_true(done)
  expect_identical(workspace_panel, "overview")
})

test_that("closed session does not apply a late identity result", {
  applied <- FALSE
  bind_external_task(
    resolve_target_identity(
      "EGFR",
      db_pool = NULL,
      perform = egfr_identity_perform(0.05)$perform
    ),
    list(isClosed = function() TRUE),
    function(result) {
      applied <<- TRUE
    }
  )
  drain_tw_later(2)
  expect_false(applied)
})
