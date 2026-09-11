# Session-safe async for external HTTP.
# httr2::req_perform_promise uses curl's multi interface so the Shiny event loop
# stays free. It does NOT honor req_retry() or req_throttle(); retries are
# applied here from the request policies. Cache/DB writes stay on the main
# process (do not pass pool to a worker).

is_tw_promise <- function(x) {
  requireNamespace("promises", quietly = TRUE) && promises::is.promising(x)
}

http_retry_policy <- function(req) {
  policies <- req$policies %||% list()
  list(
    max_tries = as.integer(policies$retry_max_tries %||% 1L),
    retry_on_failure = isTRUE(policies$retry_on_failure),
    is_transient = policies$retry_is_transient,
    backoff = policies$retry_backoff,
    after = policies$retry_after
  )
}

http_retry_wait_seconds <- function(policy, resp, failed_tries) {
  if (!is.null(resp) && is.function(policy$after)) {
    after <- tryCatch(policy$after(resp), error = function(e) NA_real_)
    after <- suppressWarnings(as.numeric(after)[1])
    if (is.finite(after) && after >= 0) {
      return(after)
    }
  }
  wait <- 0
  if (is.function(policy$backoff)) {
    wait <- tryCatch(policy$backoff(failed_tries), error = function(e) 0)
  }
  wait <- suppressWarnings(as.numeric(wait)[1])
  if (!is.finite(wait) || wait < 0) {
    0
  } else {
    wait
  }
}

http_response_is_transient <- function(policy, resp) {
  if (!is.function(policy$is_transient)) {
    return(FALSE)
  }
  isTRUE(tryCatch(policy$is_transient(resp), error = function(e) FALSE))
}

# Applies req_retry policies that req_perform_promise ignores. Backoff uses
# later/promises, never Sys.sleep.
http_async_perform <- function(req) {
  policy <- http_retry_policy(req)
  max_tries <- max(1L, policy$max_tries)

  attempt <- function(try_i) {
    tw_then(
      httr2::req_perform_promise(req),
      function(resp) {
        if (try_i < max_tries && http_response_is_transient(policy, resp)) {
          wait <- http_retry_wait_seconds(policy, resp, try_i)
          return(tw_then(tw_delay(wait), function(...) attempt(try_i + 1L)))
        }
        resp
      },
      function(e) {
        if (try_i < max_tries && isTRUE(policy$retry_on_failure)) {
          wait <- http_retry_wait_seconds(policy, NULL, try_i)
          return(tw_then(tw_delay(wait), function(...) attempt(try_i + 1L)))
        }
        stop(e)
      }
    )
  }

  attempt(1L)
}

tw_then <- function(x, on_ok, on_error = NULL) {
  if (is_tw_promise(x)) {
    return(promises::then(x, on_ok, on_error))
  }
  tryCatch(
    on_ok(x),
    error = function(e) {
      if (is.null(on_error)) {
        stop(e)
      }
      on_error(e)
    }
  )
}

# `local_i <- i` inside a for-loop does not capture by value. Async tw_then
# callbacks all observe the final i (e.g. every target labeled as MET).
# Binding i as an argument creates a new environment per iteration.
tw_then_at <- function(state, i, fn) {
  i <- as.integer(i)[[1]]
  tw_then(state, function(state) fn(state, i))
}

new_submit_guard <- function(min_interval_sec = 1) {
  last <- -Inf
  busy <- FALSE
  list(
    try_start = function() {
      now <- proc.time()[["elapsed"]]
      if (isTRUE(busy)) {
        return(FALSE)
      }
      if (is.finite(last) && (now - last) < min_interval_sec) {
        return(FALSE)
      }
      last <<- now
      busy <<- TRUE
      TRUE
    },
    finish = function() {
      busy <<- FALSE
      invisible(TRUE)
    }
  )
}

tw_delay <- function(seconds) {
  seconds <- max(0, as.numeric(seconds)[1])
  if (!is.finite(seconds) || seconds <= 0) {
    return(TRUE)
  }
  promises::promise(function(resolve, reject) {
    later::later(function() resolve(TRUE), delay = seconds)
  })
}

task_error_result <- function(message) {
  list(
    .task_error = TRUE,
    message = as.character(message[[1]] %||% "Retrieval could not be completed.")
  )
}

is_task_error <- function(value) {
  is.list(value) && isTRUE(value$.task_error)
}

inflight_has <- function(store, key) {
  key <- as.character(key %||% "")
  if (!nzchar(key)) {
    return(FALSE)
  }
  !is.null(store()[[key]])
}

inflight_token <- function(store, key) {
  key <- as.character(key %||% "")
  store()[[key]]$token %||% NA_character_
}

inflight_start <- function(store, key, extra = list()) {
  key <- as.character(key)
  cur <- store()
  token <- uuid::UUIDgenerate()
  entry <- c(list(token = token, started_at = Sys.time()), extra)
  cur[[key]] <- entry
  store(cur)
  token
}

inflight_matches <- function(store, key, token) {
  key <- as.character(key %||% "")
  identical(store()[[key]]$token, token)
}

inflight_clear <- function(store, key, token = NULL) {
  key <- as.character(key)
  cur <- store()
  if (!is.null(token) && !identical(cur[[key]]$token, token)) {
    return(invisible(FALSE))
  }
  cur[[key]] <- NULL
  store(cur)
  invisible(TRUE)
}

inflight_keys <- function(store) {
  names(store()) %||% character()
}

session_is_active <- function(session) {
  if (is.null(session)) {
    return(FALSE)
  }
  if (is.function(session$isClosed)) {
    return(!isTRUE(session$isClosed()))
  }
  TRUE
}

# Associate a completed identity lookup with the target that started it.
identity_task_apply <- function(current_target_id, completed_target_id) {
  list(
    store_for_target = as.character(completed_target_id),
    update_visible_panel = identical(
      as.character(current_target_id %||% ""),
      as.character(completed_target_id %||% "")
    )
  )
}

invoke_retrieve <- function(retrieve, ...) {
  args <- list(...)
  if ("perform" %in% names(formals(retrieve))) {
    args$perform <- http_async_perform
  }
  do.call(retrieve, args)
}

# Promise callbacks run on later's event loop, not inside a Shiny reactive
# consumer. reactiveVal GET throws there; isolate() allows a one-shot read.
bind_external_task <- function(
  result,
  session,
  on_value,
  error_message = "Retrieval could not be completed.",
  inflight = NULL,
  key = NULL,
  token = NULL
) {
  settled <- FALSE
  delivered <- FALSE
  settle <- function() {
    if (isTRUE(settled)) {
      return(invisible(FALSE))
    }
    settled <<- TRUE
    if (is.null(inflight) || is.null(key)) {
      return(invisible(FALSE))
    }
    isolate(inflight_clear(inflight, key, token))
    invisible(TRUE)
  }

  deliver <- function(value) {
    if (!isTRUE(delivered)) {
      delivered <<- TRUE
      tryCatch(
        {
          if (session_is_active(session)) {
            isolate(on_value(value))
          }
        },
        error = function(e) {
          invisible(NULL)
        }
      )
    }
    settle()
    invisible(NULL)
  }

  chain <- tw_then(
    result,
    deliver,
    function(e) {
      deliver(task_error_result(error_message))
    }
  )

  if (is_tw_promise(chain)) {
    chain <- promises::catch(chain, function(e) {
      deliver(task_error_result(error_message))
      NULL
    })
    chain <- promises::finally(chain, settle)
  }

  invisible(chain)
}

drain_tw_later <- function(timeout_seconds = 2) {
  deadline <- Sys.time() + timeout_seconds
  repeat {
    later::run_now(timeout = 0)
    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (remaining <= 0) {
      break
    }
    later::run_now(timeout = min(0.05, remaining))
    if (!later::loop_empty()) {
      next
    }
    later::run_now(timeout = 0)
    if (later::loop_empty()) {
      break
    }
  }
  invisible(TRUE)
}
