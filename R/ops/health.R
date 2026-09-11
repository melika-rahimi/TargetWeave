# shinyApp() only calls ui(req) when PATH_INFO matches uiPattern.
# The default "^/$" never sees /healthz, so httpuv returns 404.
HEALTHZ_UI_PATTERN <- "^/(healthz)?$"

is_healthz_request <- function(req) {
  path <- req$PATH_INFO %||% ""
  identical(path, "/healthz")
}

app_health_payload <- function(db_pool = NULL) {
  db_ok <- FALSE
  if (!is.null(db_pool)) {
    db_ok <- isTRUE(tryCatch({
      as.integer(DBI::dbGetQuery(db_pool, "SELECT 1 AS ok")$ok[[1]]) == 1L
    }, error = function(e) FALSE))
  }
  list(
    ok = isTRUE(db_ok),
    app = "TargetWeave",
    db = isTRUE(db_ok)
  )
}

app_health_http <- function(req, db_pool) {
  payload <- app_health_payload(db_pool)
  shiny::httpResponse(
    status = if (isTRUE(payload$ok)) 200L else 503L,
    content_type = "application/json",
    content = as.character(jsonlite::toJSON(payload, auto_unbox = TRUE))
  )
}
