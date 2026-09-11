test_that("project setup tour is four instructional steps with examples only", {
  steps <- project_setup_tour_steps()
  expect_length(steps, 4L)
  titles <- vapply(steps, `[[`, character(1), "title")
  expect_equal(
    titles,
    c(
      "Name your investigation",
      "Define your research question",
      "Add the disease context",
      "Add candidate targets"
    )
  )
  expect_match(steps[[2]]$body, "question")
  expect_false(grepl("join key", paste(vapply(steps, `[[`, character(1), "body"), collapse = " "), ignore.case = TRUE))
  expect_equal(steps[[1]]$example, "Potential therapeutic targets in NSCLC")
  expect_match(steps[[4]]$example, "EGFR")
})

test_that("setup tour JSON stays an array and is distinct from the workspace tour", {
  encoded <- tour_client_payload("start", mode = "setup", steps = project_setup_tour_steps())
  parsed <- jsonlite::fromJSON(tour_payload_json(encoded), simplifyVector = FALSE)
  expect_equal(parsed$mode, "setup")
  expect_equal(length(parsed$steps), 4L)
  expect_equal(parsed$steps[[1]]$target, "[data-tour='setup-title']")
  expect_equal(parsed$steps[[1]]$placement, "below")

  workspace <- workspace_tour_steps()
  expect_length(workspace, 5L)
  expect_false(identical(
    vapply(workspace, `[[`, character(1), "target"),
    vapply(project_setup_tour_steps(), `[[`, character(1), "target")
  ))
})

test_that("setup tour starts on project forms independently of workspace tour state", {
  unseen <- list(
    project_setup_tour_seen_at = NULL,
    workspace_tour_completed_at = Sys.time()
  )
  seen <- list(
    project_setup_tour_seen_at = Sys.time(),
    workspace_tour_completed_at = NULL
  )

  expect_true(should_start_project_setup_tour("onboarding_project", unseen))
  expect_true(should_start_project_setup_tour("new_project", unseen))
  expect_false(should_start_project_setup_tour("project", unseen))
  expect_false(should_start_project_setup_tour("onboarding_project", seen))
  expect_true(user_needs_workspace_tour(seen))
  expect_false(user_needs_project_setup_tour(seen))
  expect_false(user_needs_workspace_tour(unseen))
  expect_true(user_needs_project_setup_tour(unseen))
})

test_that("restart setup guide always opens New project", {
  plan <- restart_project_setup_tour_plan()
  expect_true(plan$ok)
  expect_equal(plan$view, "new_project")
})

test_that("setup tour state is persisted separately on tw_test", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("setup-tour-%s@example.com", suffix),
    "correct-horse-battery"
  )
  expect_true(user_needs_project_setup_tour(registered$user))
  expect_true(user_needs_workspace_tour(registered$user))

  done_setup <- mark_project_setup_tour_seen(db_pool, registered$user$id)
  expect_false(user_needs_project_setup_tour(done_setup))
  expect_true(user_needs_workspace_tour(done_setup))

  again <- mark_project_setup_tour_seen(db_pool, registered$user$id)
  expect_equal(again$project_setup_tour_seen_at, done_setup$project_setup_tour_seen_at)

  done_workspace <- mark_workspace_tour_completed(db_pool, registered$user$id)
  expect_false(user_needs_workspace_tour(done_workspace))
  expect_false(user_needs_project_setup_tour(done_workspace))

  restarted <- clear_project_setup_tour(db_pool, registered$user$id)
  expect_true(user_needs_project_setup_tour(restarted))
  expect_false(user_needs_workspace_tour(restarted))

  cols <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = current_schema()
      AND table_name = 'users'
      AND column_name = 'project_setup_tour_seen_at'
    "
  )$column_name
  expect_equal(cols, "project_setup_tour_seen_at")
})

test_that("project form keeps blank values and helper copy after the setup guide", {
  skip_if_not_installed("shiny")
  library(shiny)

  first <- as.character(mod_project_setup_ui("project_setup", variant = "onboarding"))
  later <- as.character(mod_project_setup_ui("project_setup", variant = "standard"))

  for (html in list(first, later)) {
    expect_match(html, "data-tour=\"setup-title\"")
    expect_match(html, "data-tour=\"setup-question\"")
    expect_match(html, "data-tour=\"setup-disease\"")
    expect_match(html, "data-tour=\"setup-targets\"")
    expect_match(html, "A short name for this investigation.")
    expect_match(html, "The biological question you want this project to explore.")
    expect_match(html, "confirm its standardized identity later")
    expect_match(html, "gene/protein symbols or identifiers")
    expect_false(grepl('value="[^"]*(NSCLC|EGFR|KRAS|MET|TP53|Potential therapeutic)', html))
    expect_false(grepl(">EGFR<|>KRAS<|>MET<|>TP53<", html))
  }

  expect_match(first, "Create your first investigation")
  expect_match(later, "Define the investigation")
})

test_that("setup guide does not submit projects or call scientific APIs", {
  setup_src <- paste(readLines(file.path(app_root(), "R/modules/mod_project_setup.R")), collapse = "\n")
  server_src <- paste(readLines(file.path(app_root(), "R/server.R")), collapse = "\n")
  js <- paste(readLines(file.path(app_root(), "www/tour.js")), collapse = "\n")
  steps_src <- paste(readLines(file.path(app_root(), "R/ui/ui_tour.R")), collapse = "\n")

  expect_match(server_src, "mode = \"setup\"")
  expect_match(server_src, "mark_project_setup_tour_seen")
  expect_match(server_src, "restart_setup_requested")
  expect_match(server_src, "clear_project_setup_tour")
  expect_false(grepl("fetch_uniprot|fetch_ensembl|fetch_opentargets", steps_src))
  expect_false(grepl("create_project\\(", js))
  expect_false(grepl("field\\.value|input\\.title|create_project", js))
  expect_match(js, "Skip setup guide")
  expect_match(js, "is-passive")
  expect_match(setup_src, "create_project")
  expect_false(grepl("sendCustomMessage\\(\"twTour\".*create_project", server_src))
})
