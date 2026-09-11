running_under_testthat <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

bootstrap_test_schema <- function(config) {
  schema <- config$pg_test_schema
  if (!nzchar(schema) || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", schema)) {
    stop("TW_TEST_SCHEMA is not a safe schema name.", call. = FALSE)
  }

  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = config$pg_host,
    port = config$pg_port,
    dbname = config$pg_database,
    user = config$pg_user,
    password = config$pg_password,
    sslmode = config$pg_sslmode
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    sprintf("CREATE SCHEMA IF NOT EXISTS %s", schema)
  )
  invisible(schema)
}

create_db_pool <- function() {
  if (running_under_testthat() && identical(Sys.getenv("TW_ENV"), "production")) {
    stop("Production mode cannot use the testthat PostgreSQL schema.", call. = FALSE)
  }

  config <- assert_required_config()
  if (identical(config$env, "production") && running_under_testthat()) {
    stop("Production mode cannot use the testthat PostgreSQL schema.", call. = FALSE)
  }

  connect_args <- list(
    drv = RPostgres::Postgres(),
    host = config$pg_host,
    port = config$pg_port,
    dbname = config$pg_database,
    user = config$pg_user,
    password = config$pg_password,
    sslmode = config$pg_sslmode,
    minSize = 1,
    maxSize = 5,
    idleTimeout = 60
  )

  if (running_under_testthat()) {
    if (identical(config$env, "production")) {
      stop("Refusing to attach tw_test while TW_ENV=production.", call. = FALSE)
    }
    schema <- bootstrap_test_schema(config)
    connect_args$options <- sprintf("-c search_path=%s", schema)
  }

  do.call(pool::dbPool, connect_args)
}
