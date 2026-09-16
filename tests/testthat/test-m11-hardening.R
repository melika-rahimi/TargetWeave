test_that("presentation edits do not rewrite snapshot JSON", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Title freeze"
  )
  original <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  updated <- update_project_presentation(
    db_pool,
    seed$project_id,
    seed$owner$id,
    "Edited live title",
    "Edited live question"
  )
  expect_true(updated$ok)
  expect_equal(updated$invalidation$kind, "presentation")
  live <- get_owned_project(db_pool, seed$project_id, seed$owner$id)
  expect_equal(live$title[[1]], "Edited live title")
  frozen <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(frozen$project_context$model$title, original$project_context$model$title)
  expect_equal(frozen$project_context$model$research_question, original$project_context$model$research_question)
})

test_that("disease wording reset clears live identity but not snapshots or targets", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Disease freeze"
  )
  original <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  note <- create_research_note(
    db_pool,
    seed$owner$id,
    seed$project_id,
    "Keep this project note.",
    scope = "project"
  )
  expect_true(note$ok)
  result <- reset_project_disease(
    db_pool,
    seed$project_id,
    seed$owner$id,
    "lung adenocarcinoma"
  )
  expect_true(result$ok)
  expect_true("opentargets" %in% result$invalidation$scopes)
  expect_false("overview" %in% result$invalidation$scopes)
  live <- get_owned_project(db_pool, seed$project_id, seed$owner$id)
  expect_equal(live$disease_label[[1]], "lung adenocarcinoma")
  expect_equal(live$disease_resolution_status[[1]], "unresolved")
  expect_true(is.na(live$disease_ontology_id[[1]]) || !nzchar(as.character(live$disease_ontology_id[[1]])))
  targets <- list_project_targets(db_pool, seed$project_id, seed$owner$id)
  expect_true(any(targets$resolution_status == "confirmed"))
  frozen <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(frozen$project_context$model$disease_ontology_id, original$project_context$model$disease_ontology_id)
  notes <- list_research_notes(db_pool, seed$project_id, seed$owner$id, scope = "project")
  expect_equal(nrow(notes), 1)
})

test_that("removing a target requires confirmation and leaves snapshots and notes", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Target freeze"
  )
  original <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  create_research_note(
    db_pool,
    seed$owner$id,
    seed$project_id,
    "EGFR live note",
    scope = "target",
    project_target_id = seed$egfr_id
  )
  blocked <- remove_project_target(
    db_pool,
    seed$project_id,
    seed$owner$id,
    seed$egfr_id,
    confirm_note_deletion = FALSE
  )
  expect_false(blocked$ok)
  expect_true(isTRUE(blocked$needs_confirmation))
  removed <- remove_project_target(
    db_pool,
    seed$project_id,
    seed$owner$id,
    seed$egfr_id,
    confirm_note_deletion = TRUE
  )
  expect_true(removed$ok)
  live_targets <- list_project_targets(db_pool, seed$project_id, seed$owner$id)
  expect_false(seed$egfr_id %in% live_targets$id)
  frozen <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(
    frozen$target_identity$model$n_targets,
    original$target_identity$model$n_targets
  )
  expect_equal(nrow(list_research_notes(db_pool, seed$project_id, seed$owner$id, scope = "target")), 1)
})

test_that("reset confirmed identity does not mutate snapshots", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Identity freeze"
  )
  original <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  reset <- reset_confirmed_target(db_pool, seed$egfr_id, seed$owner$id)
  expect_true(reset$ok)
  row <- get_owned_target(db_pool, seed$egfr_id, seed$owner$id)
  expect_equal(row$resolution_status[[1]], "unresolved")
  frozen <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(frozen$overview, original$overview)
})

test_that("archive is reversible and is not delete", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  expect_true(archive_project(db_pool, seed$project_id, seed$owner$id))
  expect_equal(nrow(list_projects(db_pool, seed$owner$id, include_archived = FALSE)), 0)
  archived <- list_projects(db_pool, seed$owner$id, include_archived = TRUE)
  expect_equal(archived$status[[1]], "archived")
  expect_true(restore_project(db_pool, seed$project_id, seed$owner$id))
  active <- list_projects(db_pool, seed$owner$id, include_archived = FALSE)
  expect_equal(nrow(active), 1)
})

test_that("ensure_schema and named migrations are idempotent", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  ensure_schema(db_pool)
  rows <- DBI::dbGetQuery(db_pool, "SELECT name FROM schema_migrations ORDER BY version")
  expect_true("m11_indexes" %in% rows$name)
  expect_equal(sum(rows$name == "m11_indexes"), 1)
})

test_that("renv.lock lists every DESCRIPTION runtime Import and Dockerfile restores it", {
  desc <- read.dcf(file.path(app_root(), "DESCRIPTION"), fields = "Imports")[1, 1]
  imports <- trimws(unlist(strsplit(gsub("\\n", "", desc), ",")))
  imports <- sub("\\s*\\(.*$", "", imports)
  lock <- jsonlite::fromJSON(file.path(app_root(), "renv.lock"), simplifyVector = FALSE)
  locked <- names(lock$Packages)
  missing <- setdiff(imports, locked)
  expect_equal(missing, character())
  docker <- paste(readLines(file.path(app_root(), "Dockerfile")), collapse = "\n")
  expect_match(docker, "renv::restore")
  expect_match(docker, "check_renv_imports.R")
  expect_false(grepl("COPY renv/library", docker, fixed = TRUE))
  expect_false(grepl("install.packages\\(pkgs", docker))
  checker <- paste(readLines(file.path(app_root(), "scripts/check_renv_imports.R")), collapse = "\n")
  expect_match(checker, "renv restore missing")
})

test_that("evidence modules flush a retrieving state before HTTP", {
  files <- c(
    overview = "R/modules/mod_overview.R",
    evidence = "R/modules/mod_disease_evidence.R",
    compare = "R/modules/mod_ot_comparison.R",
    pathways = "R/modules/mod_pathways.R",
    literature = "R/modules/mod_literature.R",
    structures = "R/modules/mod_structures.R"
  )
  expected <- c(
    overview = "Retrieving identity",
    evidence = "Retrieving Open Targets evidence",
    compare = "Retrieving Open Targets evidence",
    pathways = "Retrieving Reactome pathways",
    literature = "Retrieving PubMed records",
    structures = "Retrieving experimental structures"
  )
  for (name in names(files)) {
    text <- paste(readLines(file.path(app_root(), files[[name]])), collapse = "\n")
    expect_match(text, "schedule_after_flush", info = name)
    expect_match(text, expected[[name]], info = name)
  }
})

test_that("TESTTHAT uses tw_test and production refuses it", {
  expect_true(running_under_testthat())
  config <- get_app_config()
  expect_equal(config$pg_test_schema, "tw_test")
  old <- Sys.getenv("TW_ENV", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TW_ENV") else Sys.setenv(TW_ENV = old)
  }, add = TRUE)
  Sys.setenv(TW_ENV = "production")
  expect_error(create_db_pool(), regexp = "Production mode", ignore.case = TRUE)
})

test_that("login messages do not enumerate accounts and throttle after failures", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("throttle-%s@example.com", suffix)
  register_user(db_pool, email, "correct-horse-battery")
  rm(list = ls(envir = .login_attempts), envir = .login_attempts)
  missing <- authenticate_user(db_pool, sprintf("missing-%s@example.com", suffix), "wrong-password-1")
  expect_equal(missing$message, "Incorrect email or password.")
  for (i in seq_len(LOGIN_MAX_ATTEMPTS)) {
    authenticate_user(db_pool, email, "wrong-password-1")
  }
  throttled <- authenticate_user(db_pool, email, "correct-horse-battery")
  expect_match(throttled$message, "Too many")
})

test_that("input limits reject oversized payloads", {
  expect_false(enforce_length(paste(rep("a", 300), collapse = ""), INPUT_LIMITS$project_title, "Project title")$ok)
  expect_true(enforce_length("ok", INPUT_LIMITS$project_title, "Project title")$ok)
})

test_that("health payload has no secrets", {
  payload <- app_health_payload(NULL)
  encoded <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  expect_false(grepl("password", encoded, ignore.case = TRUE))
  expect_false(grepl("api_key", encoded, ignore.case = TRUE))
  expect_false(isTRUE(payload$ok))
})

test_that("healthz HTTP handler is JSON-only and 503 without a database", {
  resp <- app_health_http(list(PATH_INFO = "/healthz", REQUEST_METHOD = "GET"), NULL)
  expect_s3_class(resp, "httpResponse")
  expect_equal(resp$status, 503L)
  expect_equal(resp$content_type, "application/json")
  body <- jsonlite::fromJSON(resp$content)
  expect_equal(sort(names(body)), c("app", "db", "ok"))
  expect_equal(body$app, "TargetWeave")
  expect_false(isTRUE(body$ok))
  expect_false(isTRUE(body$db))
  expect_false(grepl("password|postgres|PG|api_key|secret|stack", resp$content, ignore.case = TRUE))
})

test_that("healthz is registered on the Shiny PATH_INFO pattern used by runApp", {
  expect_true(grepl(HEALTHZ_UI_PATTERN, "/"))
  expect_true(grepl(HEALTHZ_UI_PATTERN, "/healthz"))
  expect_false(grepl(HEALTHZ_UI_PATTERN, "/styles.css"))
  expect_false(grepl(HEALTHZ_UI_PATTERN, "/healthz/extra"))
  expect_true(is_healthz_request(list(PATH_INFO = "/healthz")))
  expect_false(is_healthz_request(list(PATH_INFO = "/")))
  app_src <- paste(readLines(file.path(app_root(), "app.R")), collapse = "\n")
  expect_match(app_src, "uiPattern\\s*=\\s*HEALTHZ_UI_PATTERN")
  expect_match(app_src, "is_healthz_request")
})

test_that("independent source failures do not crash sibling retrieve functions", {
  target <- list(
    id = "t1",
    resolution_status = "confirmed",
    display_symbol = "EGFR",
    ensembl_gene_id = "ENSG00000146648",
    uniprot_accession = "P00533",
    input_text = "EGFR"
  )
  overview <- retrieve_target_overview(
    target,
    db_pool = NULL,
    uniprot_fetch = function(...) new_api_result(ok = FALSE, error = "UniProt down", source = "uniprot"),
    ensembl_fetch = function(...) new_api_result(
      ok = TRUE,
      status = 200,
      data = read_fixture("ensembl_overview_egfr.json"),
      source = "ensembl",
      cache_status = "live"
    )
  )
  expect_true(overview$status %in% c("partial", "ready", "unavailable"))
  expect_false(identical(overview$status, NULL))
  expect_true(has_display_text(overview$overview$identity$ensembl_gene_id) ||
    identical(overview$status, "unavailable") ||
    identical(overview$status, "partial"))
})
