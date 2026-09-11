# Application configuration.
# Secrets are read from environment variables, never committed to Git.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

load_local_renviron <- function() {
  candidates <- unique(c(
    ".Renviron",
    file.path("..", ".Renviron"),
    file.path("..", "..", ".Renviron"),
    if (exists("app_root", mode = "function")) file.path(app_root(), ".Renviron") else NULL
  ))

  for (path in candidates) {
    if (!is.null(path) && file.exists(path)) {
      readRenviron(path)
      break
    }
  }

  invisible(TRUE)
}

get_app_config <- function() {
  load_local_renviron()

  env <- Sys.getenv("TW_ENV", unset = "")
  if (!nzchar(env)) {
    env <- if (identical(Sys.getenv("TESTTHAT"), "true")) "test" else "development"
  }

  list(
    env = env,
    pg_host = Sys.getenv("PGHOST", "127.0.0.1"),
    pg_port = as.integer(Sys.getenv("PGPORT", "5432")),
    pg_database = Sys.getenv("PGDATABASE", "targetweave"),
    pg_user = Sys.getenv("PGUSER", "targetweave"),
    pg_password = Sys.getenv("PGPASSWORD", ""),
    pg_sslmode = Sys.getenv("PGSSLMODE", "disable"),
    pg_test_schema = Sys.getenv("TW_TEST_SCHEMA", "tw_test"),
    max_targets = 8L,
    min_targets = 1L,
    http_timeout_seconds = as.integer(Sys.getenv("TW_HTTP_TIMEOUT", "20")),
    identity_cache_ttl_days = 14L,
    disease_search_ttl_hours = 24L,
    ot_association_ttl_days = 7L,
    contact_email = Sys.getenv("UNIPROT_CONTACT_EMAIL", "targetweave@localhost"),
    ncbi_tool = Sys.getenv("NCBI_TOOL", "TargetWeave"),
    ncbi_email = Sys.getenv("NCBI_EMAIL", ""),
    ncbi_api_key = Sys.getenv("NCBI_API_KEY", ""),
    pubmed_ttl_hours = 24L,
    rcsb_search_ttl_days = 7L,
    rcsb_metadata_ttl_days = 14L,
    display_tz = Sys.getenv("TW_DISPLAY_TZ", "UTC")
  )
}

assert_required_config <- function(config = get_app_config()) {
  if (!nzchar(config$pg_password)) {
    stop(
      paste(
        "PGPASSWORD is empty.",
        "Copy .Renviron.example to .Renviron and set your local database password."
      ),
      call. = FALSE
    )
  }

  invisible(config)
}
