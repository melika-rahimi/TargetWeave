test_that("workspace tour copy stays short and non-technical", {
  steps <- workspace_tour_steps()
  expect_length(steps, 5L)
  titles <- vapply(steps, `[[`, character(1), "title")
  expect_equal(
    titles,
    c("Project context", "Targets", "Identity first", "Target workspace", "Compare evidence")
  )
  joined <- paste(vapply(steps, `[[`, character(1), "body"), collapse = " ")
  expect_false(grepl("join key|normalized model|database", joined, ignore.case = TRUE))
  expect_match(joined, "downstream evidence")
  expect_match(as.character(overview_tip_step()$body), "protein annotation")
  expect_match(as.character(evidence_tip_step()$body), "Open Targets association scores")
  expect_match(as.character(compare_tip_step()$body), "dash")
})

test_that("new users need a workspace tour independently of optional profile", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("tour-%s@example.com", suffix),
    "correct-horse-battery"
  )
  expect_true(user_needs_onboarding(registered$user))
  expect_true(user_needs_workspace_tour(registered$user))

  skipped <- complete_onboarding(db_pool, registered$user$id)
  expect_false(user_needs_onboarding(skipped))
  expect_true(user_needs_workspace_tour(skipped))

  created <- create_project(
    db_pool,
    skipped$id,
    "First tour project",
    "Tour state must not depend on project count.",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  expect_true(created$ok)
  still <- load_user_by_id(db_pool, skipped$id)
  expect_true(user_needs_workspace_tour(still))

  after_skip <- mark_workspace_tour_completed(db_pool, skipped$id)
  expect_false(user_needs_workspace_tour(after_skip))
  again <- mark_workspace_tour_completed(db_pool, skipped$id)
  expect_equal(again$workspace_tour_completed_at, after_skip$workspace_tour_completed_at)

  logged_in <- authenticate_user(
    db_pool,
    sprintf("tour-%s@example.com", suffix),
    "correct-horse-battery"
  )
  expect_true(logged_in$ok)
  expect_false(user_needs_workspace_tour(logged_in$user))
})

test_that("skipping or finishing the tour does not change scientific state", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("tour-api-%s@example.com", suffix),
    "correct-horse-battery"
  )
  complete_onboarding(db_pool, registered$user$id)
  created <- create_project(
    db_pool,
    registered$user$id,
    "Tour safety",
    "Tour controls must not confirm identities or call APIs.",
    "Non-small-cell lung cancer",
    c("EGFR", "MET")
  )
  expect_true(created$ok)
  before_targets <- list_project_targets(db_pool, created$project_id, registered$user$id)
  before_project <- get_owned_project(db_pool, created$project_id, registered$user$id)
  before_key <- cache_key_opentargets_association("ENSG00000146648", "MONDO_0005233")

  mark_workspace_tour_completed(db_pool, registered$user$id)
  mark_workspace_tip_seen(db_pool, registered$user$id, "overview")
  mark_workspace_tip_seen(db_pool, registered$user$id, "evidence")
  mark_workspace_tip_seen(db_pool, registered$user$id, "compare")

  after_targets <- list_project_targets(db_pool, created$project_id, registered$user$id)
  after_project <- get_owned_project(db_pool, created$project_id, registered$user$id)
  expect_equal(after_targets$resolution_status, before_targets$resolution_status)
  expect_equal(after_project$disease_ontology_id, before_project$disease_ontology_id)
  expect_equal(
    cache_key_opentargets_association("ENSG00000146648", "MONDO_0005233"),
    before_key
  )
})

test_that("product_intro_seen_at migrates into workspace tour state", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("migrated-tour-%s@example.com", suffix)
  registered <- register_user(db_pool, email, "correct-horse-battery")
  DBI::dbExecute(
    db_pool,
    "ALTER TABLE users ADD COLUMN IF NOT EXISTS product_intro_seen_at TIMESTAMPTZ NULL"
  )
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET product_intro_seen_at = created_at,
        workspace_tour_completed_at = NULL
    WHERE id = $1::uuid
    ",
    params = list(registered$user$id)
  )
  expect_true(user_needs_workspace_tour(load_user_by_id(db_pool, registered$user$id)))
  ensure_workspace_tour_column(db_pool)
  migrated <- load_user_by_id(db_pool, registered$user$id)
  expect_false(user_needs_workspace_tour(migrated))
})

test_that("existing grandfathered users are not forced through the workspace tour", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("returning-tour-%s@example.com", suffix)
  registered <- register_user(db_pool, email, "correct-horse-battery")
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET onboarding_completed_at = created_at,
        workspace_tour_completed_at = created_at
    WHERE id = $1::uuid
    ",
    params = list(registered$user$id)
  )
  logged_in <- authenticate_user(db_pool, email, "correct-horse-battery")
  expect_false(user_needs_onboarding(logged_in$user))
  expect_false(user_needs_workspace_tour(logged_in$user))
})

test_that("workspace UI exposes tour anchors and About your research stays optional", {
  skip_if_not_installed("shiny")
  library(shiny)

  home <- paste(
    readLines(file.path(app_root(), "R/modules/mod_project_home.R")),
    collapse = "\n"
  )
  expect_match(home, "data-tour.*=.*project-header")
  expect_match(home, "data-tour.*=.*target-list")
  expect_match(home, "data-tour.*=.*identity")
  expect_match(home, "compare-nav")

  profile <- as.character(mod_onboarding_ui("onboarding"))
  expect_match(profile, "Display name")
  expect_match(profile, "Skip for now")
  expect_match(profile, "About your research")
  expect_match(profile, "First investigation")

  first <- as.character(mod_project_setup_ui("project_setup", variant = "onboarding"))
  expect_match(first, "Create your first investigation")
  expect_match(first, "autocomplete=\"off\"")
  expect_match(first, "e.g. Candidate targets in colorectal cancer")
  expect_match(first, "e.g. Which candidates deserve deeper investigation")
  expect_match(first, "e.g. colorectal cancer")
  expect_false(grepl('value="[^"]*(NSCLC|EGFR|KRAS|MET|TP53|Potential therapeutic)', first))
  expect_false(grepl(">EGFR<|>KRAS<|>MET<|>TP53<", first))
})

test_that("empty first investigation values cannot silently create the NSCLC demo", {
  expect_false(validate_project_input("", "", "", character())$ok)
  expect_false(
    grepl(
      "NSCLC",
      as.character(mod_project_setup_ui("project_setup", variant = "onboarding")),
      ignore.case = TRUE
    )
  )
})

test_that("mobile tour persistence uses the same user-level columns", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("mobile-tour-%s@example.com", suffix),
    "correct-horse-battery"
  )
  cols <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = current_schema()
      AND table_name = 'users'
      AND column_name IN ('workspace_tour_completed_at', 'workspace_tips_seen')
    ORDER BY column_name
    "
  )$column_name
  expect_equal(cols, c("workspace_tips_seen", "workspace_tour_completed_at"))
  expect_true(user_needs_workspace_tour(registered$user))
  done <- mark_workspace_tour_completed(db_pool, registered$user$id)
  expect_false(user_needs_workspace_tour(done))
})

test_that("tour payload stays an array of step objects under Shiny JSON options", {
  encoded <- tour_client_payload("start", mode = "tour", steps = workspace_tour_steps())
  parsed <- jsonlite::fromJSON(tour_payload_json(encoded), simplifyVector = FALSE)

  expect_equal(parsed$action, "start")
  expect_true(is.list(parsed$steps))
  expect_equal(length(parsed$steps), 5L)
  expect_equal(parsed$steps[[1]]$target, "[data-tour='project-header']")
  expect_equal(parsed$steps[[1]]$fallback, ".project-hero")
  expect_equal(parsed$steps[[5]]$target, "[data-tour='compare-nav']")
  expect_true(is.character(parsed$steps[[1]]$title))
  expect_length(parsed$steps[[1]]$title, 1L)
})

test_that("workspace tour starts only for eligible users on a ready project", {
  eligible <- list(workspace_tour_completed_at = NULL)
  completed <- list(workspace_tour_completed_at = Sys.time())

  expect_true(should_start_workspace_tour("project", eligible, "proj-1"))
  expect_false(should_start_workspace_tour("onboarding_project", eligible, "proj-1"))
  expect_false(should_start_workspace_tour("account", eligible, "proj-1"))
  expect_false(should_start_workspace_tour("project", completed, "proj-1"))
  expect_false(should_start_workspace_tour("project", eligible, NULL))
})

test_that("restart tour requires an open project instead of failing silently", {
  blocked <- restart_workspace_tour_plan(NULL)
  expect_false(blocked$ok)
  expect_equal(blocked$message, "Open a project to start the workspace tour.")

  ready <- restart_workspace_tour_plan("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
  expect_true(ready$ok)
  expect_equal(ready$view, "project")
})

test_that("clearing tour state makes the user eligible again", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("restart-tour-%s@example.com", suffix),
    "correct-horse-battery"
  )
  complete_onboarding(db_pool, registered$user$id)
  done <- mark_workspace_tour_completed(db_pool, registered$user$id)
  expect_false(user_needs_workspace_tour(done))

  restarted <- clear_workspace_tour(db_pool, registered$user$id)
  expect_true(user_needs_workspace_tour(restarted))
  expect_true(
    should_start_workspace_tour("project", restarted, "proj-1")
  )
})

test_that("tour runtime wiring waits for the first DOM hook and stays scientific-passive", {
  js <- paste(readLines(file.path(app_root(), "www/tour.js")), collapse = "\n")
  server_src <- paste(readLines(file.path(app_root(), "R/server.R")), collapse = "\n")
  home <- paste(
    readLines(file.path(app_root(), "R/modules/mod_project_home.R")),
    collapse = "\n"
  )

  expect_match(js, "addCustomMessageHandler\\(\"twTour\"")
  expect_match(js, "Wait for the first step")
  expect_match(js, "MutationObserver")
  expect_false(grepl("tries > 40", js))
  expect_false(grepl("uniprot|ensembl|opentargets", js, ignore.case = TRUE))

  expect_match(server_src, "tour_client_payload\\(\"start\"")
  expect_match(server_src, "should_start_workspace_tour")
  expect_match(server_src, "restart_workspace_tour_plan")
  expect_match(server_src, "account\\$restart_requested")
  expect_false(grepl("input\\$restart_tour", server_src))
  expect_false(grepl("fetch_uniprot|fetch_ensembl|fetch_opentargets", server_src))

  expect_match(home, "data-tour.*=.*project-header")
  expect_match(home, "data-tour.*=.*target-list")
  expect_match(home, "data-tour.*=.*identity")
  expect_match(home, "data-tour.*=.*target-workspace")
  expect_match(home, "compare-nav")
  expect_match(home, "workspace_ready")
})

test_that("optional shinytest2 tour appearance check is documented, not skipped as Postgres", {
  skip_if_not_installed("shinytest2")
  skip("Workspace tour appearance is verified manually in the browser for this sprint.")
})
