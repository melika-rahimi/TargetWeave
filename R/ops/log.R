# Operational logging. Never log passwords, API keys, notes, support bodies,
# WebEnv/query_key, or full external payloads.

tw_log_session_id <- function(session = NULL) {
  if (is.null(session)) {
    return(NA_character_)
  }
  token <- tryCatch(session$token, error = function(e) NULL)
  if (is.null(token) || length(token) != 1 || is.na(token) || !nzchar(as.character(token))) {
    return(NA_character_)
  }
  substr(gsub("[^A-Za-z0-9]", "", as.character(token)), 1, 12)
}

tw_log <- function(event, ..., level = "info", session = NULL) {
  extra <- list(...)
  banned <- c(
    "password", "password_hash", "api_key", "pg_password", "note", "notes",
    "body", "message", "support", "webenv", "query_key", "payload"
  )
  keep <- extra
  if (length(keep) > 0 && !is.null(names(keep))) {
    drop <- tolower(names(keep)) %in% banned
    keep <- keep[!drop]
  }
  record <- c(
    list(
      ts = format(Sys.time(), tz = "UTC", usetz = TRUE),
      level = level,
      event = as.character(event),
      session = tw_log_session_id(session)
    ),
    keep
  )
  line <- tryCatch(
    jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", na = "null"),
    error = function(e) sprintf('{"event":"%s","level":"%s"}', event, level)
  )
  cat(line, "\n", file = stderr(), sep = "")
  invisible(TRUE)
}
