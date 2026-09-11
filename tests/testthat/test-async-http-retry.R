retry_json_response <- function(req, status, payload = list(ok = TRUE)) {
  httr2::response(
    status_code = as.integer(status),
    url = as.character(req$url %||% "https://example.test/retry"),
    method = "GET",
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")))
  )
}

async_retry_request <- function(
  url = "https://example.test/retry",
  max_tries = 3L,
  backoff = ~ 0,
  transient = c(429, 500, 502, 503, 504)
) {
  req <- httr2::request(url)
  req <- httr2::req_retry(
    req,
    max_tries = max_tries,
    backoff = backoff,
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% transient
    }
  )
  httr2::req_error(req, is_error = function(resp) FALSE)
}

await_async_resp <- function(pending, timeout = 3) {
  box <- new.env(parent = emptyenv())
  box$resp <- NULL
  box$err <- NULL
  promises::then(
    pending,
    function(resp) box$resp <- resp,
    function(e) box$err <- e
  )
  drain_tw_later(timeout)
  if (!is.null(box$err)) {
    stop(box$err)
  }
  box$resp
}

test_that("async 503 retries then succeeds without Sys.sleep", {
  n <- 0L
  loop_ran <- FALSE
  later::later(function() loop_ran <<- TRUE, delay = 0)
  resp <- httr2::with_mocked_responses(
    function(req) {
      n <<- n + 1L
      if (n < 2L) {
        retry_json_response(req, 503L, list(error = "unavailable"))
      } else {
        retry_json_response(req, 200L, list(ok = TRUE))
      }
    },
    {
      await_async_resp(http_async_perform(async_retry_request()))
    }
  )
  expect_equal(n, 2L)
  expect_equal(httr2::resp_status(resp), 200L)
  expect_true(loop_ran)
})

test_that("async retries stop at configured maximum", {
  n <- 0L
  resp <- httr2::with_mocked_responses(
    function(req) {
      n <<- n + 1L
      retry_json_response(req, 503L, list(error = "unavailable"))
    },
    {
      await_async_resp(http_async_perform(async_retry_request(max_tries = 3L)))
    }
  )
  expect_equal(n, 3L)
  expect_equal(httr2::resp_status(resp), 503L)
})

test_that("permanent 4xx does not retry in async mode", {
  n <- 0L
  resp <- httr2::with_mocked_responses(
    function(req) {
      n <<- n + 1L
      retry_json_response(req, 404L, list(error = "missing"))
    },
    {
      await_async_resp(http_async_perform(async_retry_request()))
    }
  )
  expect_equal(n, 1L)
  expect_equal(httr2::resp_status(resp), 404L)
})

test_that("async JSON client retries 429/502 then returns success", {
  n <- 0L
  result <- httr2::with_mocked_responses(
    function(req) {
      n <<- n + 1L
      status <- if (n == 1L) 429L else if (n == 2L) 502L else 200L
      retry_json_response(req, status, list(value = n))
    },
    {
      pending <- http_get_json(
        "https://example.test/json-retry",
        perform = http_async_perform
      )
      box <- new.env(parent = emptyenv())
      box$result <- NULL
      promises::then(pending, function(value) box$result <- value)
      drain_tw_later(8)
      box$result
    }
  )
  expect_equal(n, 3L)
  expect_true(isTRUE(result$ok))
  expect_equal(result$status, 200L)
})

test_that("NCBI rate limits remain 3 rps without key and 10 rps with key", {
  old_key <- Sys.getenv("NCBI_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_key)) Sys.unsetenv("NCBI_API_KEY") else Sys.setenv(NCBI_API_KEY = old_key)
  }, add = TRUE)

  Sys.unsetenv("NCBI_API_KEY")
  expect_equal(ncbi_min_interval_seconds(), 0.40)
  expect_equal(ncbi_app_identity()$rate_limit_mode, "standard_3_rps")

  Sys.setenv(NCBI_API_KEY = "test-key-not-secret")
  expect_equal(ncbi_min_interval_seconds(), 0.12)
  expect_equal(ncbi_app_identity()$rate_limit_mode, "api_key_10_rps")
})

test_that("NCBI throttle wait is scheduled without blocking the event loop", {
  old_testthat <- Sys.getenv("TESTTHAT", unset = NA)
  old_key <- Sys.getenv("NCBI_API_KEY", unset = NA)
  old_last <- .ncbi_rate_env$last
  on.exit({
    if (is.na(old_testthat)) Sys.unsetenv("TESTTHAT") else Sys.setenv(TESTTHAT = old_testthat)
    if (is.na(old_key)) Sys.unsetenv("NCBI_API_KEY") else Sys.setenv(NCBI_API_KEY = old_key)
    .ncbi_rate_env$last <- old_last
  }, add = TRUE)

  Sys.unsetenv("TESTTHAT")
  Sys.unsetenv("NCBI_API_KEY")
  .ncbi_rate_env$last <- NULL

  first <- ncbi_throttle_wait()
  expect_equal(first, 0)
  second <- ncbi_throttle_wait()
  expect_gte(second, 0.35)
  expect_lte(second, 0.45)

  loop_ran <- FALSE
  later::later(function() loop_ran <<- TRUE, delay = 0)
  delayed <- tw_delay(second)
  expect_true(is_tw_promise(delayed))
  drain_tw_later(1)
  expect_true(loop_ran)
})
