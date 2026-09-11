new_api_result <- function(
  ok,
  status = NA_integer_,
  data = NULL,
  error = NULL,
  retrieved_at = Sys.time(),
  from_cache = FALSE,
  cache_status = NA_character_,
  source = NA_character_,
  url = NA_character_
) {
  list(
    ok = isTRUE(ok),
    status = status,
    data = data,
    error = error,
    retrieved_at = retrieved_at,
    from_cache = isTRUE(from_cache),
    cache_status = cache_status,
    source = source,
    url = url
  )
}

decode_cached_json <- function(text) {
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

http_user_agent <- function() {
  config <- get_app_config()
  sprintf(
    "TargetWeave/0.2 (research workspace; %s)",
    config$contact_email
  )
}

http_transient_status <- function(resp) {
  httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
}

parse_response_json <- function(resp) {
  text <- httr2::resp_body_string(resp)

  if (!nzchar(trimws(text))) {
    return(list(ok = FALSE, data = NULL, error = "Empty response body."))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    return(list(ok = FALSE, data = NULL, error = "Malformed JSON response."))
  }

  list(ok = TRUE, data = parsed, error = NULL)
}

http_error_result <- function(error, cache_source, url) {
  new_api_result(
    ok = FALSE,
    status = NA_integer_,
    data = NULL,
    error = conditionMessage(error),
    retrieved_at = Sys.time(),
    from_cache = FALSE,
    cache_status = "error",
    source = cache_source,
    url = url
  )
}

http_apply_stale_cache <- function(live, cached, cache_source, url) {
  if (!isTRUE(live$ok) && !is.null(cached)) {
    decoded <- tryCatch(
      decode_cached_json(cached$response[[1]]),
      error = function(e) NULL
    )
    if (!is.null(decoded)) {
      return(new_api_result(
        ok = TRUE,
        status = cached$http_status[[1]],
        data = decoded,
        error = live$error,
        retrieved_at = cached$retrieved_at[[1]],
        from_cache = TRUE,
        cache_status = "stale",
        source = cache_source,
        url = url
      ))
    }
  }

  if (!isTRUE(live$ok) && exists("tw_log", mode = "function", inherits = TRUE)) {
    status <- suppressWarnings(as.integer(live$status))
    if (isTRUE(is.na(status)) || isTRUE(status >= 400L)) {
      tw_log("external_api_failure", level = "warn", source = live$source, status = live$status)
    }
  }
  live
}

http_interpret_response <- function(
  resp,
  db_pool,
  cache_source,
  cache_key,
  ttl_seconds,
  url,
  include_graphql_errors = FALSE
) {
  status <- httr2::resp_status(resp)
  parsed <- parse_response_json(resp)

  if (status >= 400) {
    error_message <- sprintf("HTTP %s.", status)
    if (isTRUE(parsed$ok) && is.list(parsed$data) && !is.null(parsed$data$error)) {
      error_message <- as.character(parsed$data$error)
    }
    if (isTRUE(include_graphql_errors) && isTRUE(parsed$ok) && is.list(parsed$data) && !is.null(parsed$data$errors)) {
      error_message <- graphql_error_message(parsed$data$errors)
    }
    return(new_api_result(
      ok = FALSE,
      status = status,
      data = parsed$data,
      error = error_message,
      retrieved_at = Sys.time(),
      from_cache = FALSE,
      cache_status = "live",
      source = cache_source,
      url = url
    ))
  }

  if (!isTRUE(parsed$ok)) {
    return(new_api_result(
      ok = FALSE,
      status = status,
      data = NULL,
      error = parsed$error,
      retrieved_at = Sys.time(),
      from_cache = FALSE,
      cache_status = "live",
      source = cache_source,
      url = url
    ))
  }

  result <- new_api_result(
    ok = TRUE,
    status = status,
    data = parsed$data,
    error = NULL,
    retrieved_at = Sys.time(),
    from_cache = FALSE,
    cache_status = "live",
    source = cache_source,
    url = url
  )

  if (!is.null(db_pool) && nzchar(cache_source) && nzchar(cache_key)) {
    tryCatch(
      cache_put(
        db_pool = db_pool,
        source = cache_source,
        cache_key = cache_key,
        response = parsed$data,
        http_status = status,
        retrieved_at = result$retrieved_at,
        expires_at = result$retrieved_at + ttl_seconds,
        status = "ok"
      ),
      error = function(e) NULL
    )
  }
  result
}

http_run_request <- function(
  req,
  perform,
  cached,
  db_pool,
  cache_source,
  cache_key,
  ttl_seconds,
  url,
  include_graphql_errors = FALSE
) {
  finish <- function(resp = NULL, error = NULL) {
    live <- if (!is.null(error)) {
      http_error_result(error, cache_source, url)
    } else {
      http_interpret_response(
        resp,
        db_pool = db_pool,
        cache_source = cache_source,
        cache_key = cache_key,
        ttl_seconds = ttl_seconds,
        url = url,
        include_graphql_errors = include_graphql_errors
      )
    }
    http_apply_stale_cache(live, cached, cache_source, url)
  }

  raw <- tryCatch(perform(req), error = function(e) e)
  if (is_tw_promise(raw)) {
    return(promises::then(
      raw,
      function(resp) finish(resp = resp),
      function(e) finish(error = e)
    ))
  }
  if (inherits(raw, "error")) {
    return(finish(error = raw))
  }
  finish(resp = raw)
}

http_get_json <- function(
  url,
  query = list(),
  headers = list(),
  db_pool = NULL,
  cache_source = NULL,
  cache_key = NULL,
  ttl_seconds = NULL,
  timeout_seconds = NULL,
  perform = httr2::req_perform
) {
  config <- get_app_config()
  timeout_seconds <- timeout_seconds %||% config$http_timeout_seconds
  ttl_seconds <- ttl_seconds %||% (config$identity_cache_ttl_days * 86400L)

  cached <- NULL
  if (!is.null(db_pool) && nzchar(cache_source) && nzchar(cache_key)) {
    cached <- tryCatch(
      cache_get(db_pool, cache_source, cache_key),
      error = function(e) NULL
    )
  }

  if (!is.null(cached) && identical(cached$freshness, "fresh")) {
    decoded <- tryCatch(
      decode_cached_json(cached$response[[1]]),
      error = function(e) NULL
    )

    if (!is.null(decoded)) {
      return(new_api_result(
        ok = TRUE,
        status = cached$http_status[[1]],
        data = decoded,
        error = NULL,
        retrieved_at = cached$retrieved_at[[1]],
        from_cache = TRUE,
        cache_status = "fresh",
        source = cache_source,
        url = url
      ))
    }
  }

  req <- httr2::request(url)
  if (length(query) > 0) {
    req <- httr2::req_url_query(req, !!!query)
  }

  req <- httr2::req_headers(
    req,
    `User-Agent` = http_user_agent(),
    Accept = "application/json"
  )
  if (length(headers) > 0) {
    req <- httr2::req_headers(req, !!!headers)
  }
  req <- httr2::req_timeout(req, timeout_seconds)
  req <- httr2::req_retry(
    req,
    max_tries = 3,
    backoff = ~ pmin(2^(.x - 1), 8),
    is_transient = function(resp) {
      http_transient_status(resp)
    }
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  http_run_request(
    req,
    perform = perform,
    cached = cached,
    db_pool = db_pool,
    cache_source = cache_source,
    cache_key = cache_key,
    ttl_seconds = ttl_seconds,
    url = url
  )
}

http_post_json <- function(
  url,
  body = list(),
  headers = list(),
  db_pool = NULL,
  cache_source = NULL,
  cache_key = NULL,
  ttl_seconds = NULL,
  timeout_seconds = NULL,
  perform = httr2::req_perform
) {
  config <- get_app_config()
  timeout_seconds <- timeout_seconds %||% config$http_timeout_seconds
  ttl_seconds <- ttl_seconds %||% (config$identity_cache_ttl_days * 86400L)

  cached <- NULL
  if (!is.null(db_pool) && nzchar(cache_source) && nzchar(cache_key)) {
    cached <- tryCatch(
      cache_get(db_pool, cache_source, cache_key),
      error = function(e) NULL
    )
  }

  if (!is.null(cached) && identical(cached$freshness, "fresh")) {
    decoded <- tryCatch(
      decode_cached_json(cached$response[[1]]),
      error = function(e) NULL
    )

    if (!is.null(decoded)) {
      return(new_api_result(
        ok = TRUE,
        status = cached$http_status[[1]],
        data = decoded,
        error = NULL,
        retrieved_at = cached$retrieved_at[[1]],
        from_cache = TRUE,
        cache_status = "fresh",
        source = cache_source,
        url = url
      ))
    }
  }

  req <- httr2::request(url)
  req <- httr2::req_method(req, "POST")
  req <- httr2::req_body_json(req, body)
  req <- httr2::req_headers(
    req,
    `User-Agent` = http_user_agent(),
    Accept = "application/json",
    `Content-Type` = "application/json"
  )
  if (length(headers) > 0) {
    req <- httr2::req_headers(req, !!!headers)
  }
  req <- httr2::req_timeout(req, timeout_seconds)
  req <- httr2::req_retry(
    req,
    max_tries = 3,
    backoff = ~ pmin(2^(.x - 1), 8),
    is_transient = function(resp) {
      http_transient_status(resp)
    }
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  http_run_request(
    req,
    perform = perform,
    cached = cached,
    db_pool = db_pool,
    cache_source = cache_source,
    cache_key = cache_key,
    ttl_seconds = ttl_seconds,
    url = url,
    include_graphql_errors = TRUE
  )
}

graphql_error_message <- function(errors) {
  if (is.null(errors) || length(errors) == 0) {
    return("GraphQL error.")
  }
  messages <- vapply(
    errors,
    function(err) as.character(err$message %||% "GraphQL error."),
    character(1)
  )
  paste(messages, collapse = "; ")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (length(x) == 1 && is.atomic(x) && is.na(x)) {
    return(y)
  }
  x
}

# TRUE only for a single non-missing, non-blank value. Never returns NA.
# JSON-null fields become NULL after fromJSON; NA_character_ is common in
# in-memory candidates. Both must be FALSE here so `if (has_display_text(x))`
# cannot hit "missing value where TRUE/FALSE needed".
has_display_text <- function(x) {
  if (is.null(x) || length(x) != 1L || !is.atomic(x)) {
    return(FALSE)
  }

  if (is.na(x)) {
    return(FALSE)
  }

  nzchar(trimws(as.character(x)))
}

is_present_scalar <- function(x) {
  !is.null(x) && length(x) == 1L && is.atomic(x) && !is.na(x)
}
