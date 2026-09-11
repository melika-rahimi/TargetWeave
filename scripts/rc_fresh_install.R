# Isolated schema check. Uses schema tw_rc_fresh in the configured database.
# Drops that schema at the end. Does not use tw_test.

Sys.unsetenv("TESTTHAT")
Sys.setenv(TW_ENV = "development")

source("R/config.R")
source("R/ops/log.R")
source("R/ops/limits.R")
source("R/ops/async.R")
source("R/db/pool.R")
source("R/db/schema.R")
source("R/db/migrations.R")
source("R/db/users.R")
source("R/db/profiles.R")
source("R/db/projects.R")
source("R/db/targets.R")
source("R/db/authz.R")
source("R/db/cache.R")
source("R/db/snapshots.R")
source("R/db/notes.R")
source("R/process/ensembl_ids.R")
source("R/api/http_client.R")
source("R/process/process_overview.R")
source("R/process/process_snapshots.R")
source("R/process/process_invalidation.R")

stopifnot(!running_under_testthat())
config <- assert_required_config()
schema <- "tw_rc_fresh"

admin <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  host = config$pg_host,
  port = config$pg_port,
  dbname = config$pg_database,
  user = config$pg_user,
  password = config$pg_password,
  sslmode = config$pg_sslmode
)
DBI::dbExecute(admin, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", schema))
DBI::dbExecute(admin, sprintf("CREATE SCHEMA %s", schema))
DBI::dbDisconnect(admin)

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  host = config$pg_host,
  port = config$pg_port,
  dbname = config$pg_database,
  user = config$pg_user,
  password = config$pg_password,
  sslmode = config$pg_sslmode,
  minSize = 1,
  maxSize = 2,
  options = sprintf("-c search_path=%s", schema)
)
on.exit({
  try(pool::poolClose(pool), silent = TRUE)
  admin2 <- try(DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = config$pg_host,
    port = config$pg_port,
    dbname = config$pg_database,
    user = config$pg_user,
    password = config$pg_password,
    sslmode = config$pg_sslmode
  ), silent = TRUE)
  if (!inherits(admin2, "try-error")) {
    try(DBI::dbExecute(admin2, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", schema)), silent = TRUE)
    try(DBI::dbDisconnect(admin2), silent = TRUE)
  }
}, add = TRUE)

t0 <- proc.time()[["elapsed"]]
ensure_schema(pool)
ensure_schema(pool)
elapsed <- round(proc.time()[["elapsed"]] - t0, 2)
cat("ensure_schema x2 elapsed_s=", elapsed, "\n", sep = "")

suffix <- gsub("-", "", uuid::UUIDgenerate())
email <- sprintf("rc-fresh-%s@example.test", suffix)
reg <- register_user(pool, email, "correct-horse-battery")
stopifnot(isTRUE(reg$ok))
prof <- save_user_profile(
  pool,
  reg$user$id,
  display_name = "RC Fresh",
  research_role = "Researcher",
  research_field = "Cancer biology",
  institution = "Example Lab"
)
stopifnot(!is.null(prof$id))
created <- create_project(
  pool,
  reg$user$id,
  "RC fresh project",
  "Does a empty database persist a workspace?",
  "Non-small-cell lung cancer",
  c("EGFR", "KRAS")
)
stopifnot(isTRUE(created$ok))
note <- create_research_note(
  pool,
  reg$user$id,
  created$project_id,
  "Fresh-install note.",
  scope = "project"
)
stopifnot(isTRUE(note$ok))
snap <- create_evidence_snapshot(
  pool,
  reg$user$id,
  created$project_id,
  name = "RC snapshot"
)
stopifnot(isTRUE(snap$ok))

pool::poolClose(pool)
pool2 <- pool::dbPool(
  drv = RPostgres::Postgres(),
  host = config$pg_host,
  port = config$pg_port,
  dbname = config$pg_database,
  user = config$pg_user,
  password = config$pg_password,
  sslmode = config$pg_sslmode,
  minSize = 1,
  maxSize = 2,
  options = sprintf("-c search_path=%s", schema)
)
on.exit(try(pool::poolClose(pool2), silent = TRUE), add = TRUE)
ensure_schema(pool2)
user <- authenticate_user(pool2, email, "correct-horse-battery")
stopifnot(isTRUE(user$ok))
projects <- list_projects(pool2, user$user$id)
stopifnot(nrow(projects) == 1)
notes <- list_research_notes(pool2, created$project_id, user$user$id)
stopifnot(nrow(notes) == 1)
snaps <- list_project_snapshots(pool2, created$project_id, user$user$id)
stopifnot(nrow(snaps) == 1)
cat("fresh_install_ok=TRUE email=", email, " project=", created$project_id, "\n", sep = "")
