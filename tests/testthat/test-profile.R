test_that("new registration stores a sodium hash and enters onboarding", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("onboard-%s@example.com", suffix)
  password <- "correct-horse-battery"

  registered <- register_user(db_pool, email, password)
  expect_true(registered$ok)
  expect_true(user_needs_onboarding(registered$user))
  expect_true(user_needs_workspace_tour(registered$user))

  stored <- DBI::dbGetQuery(
    db_pool,
    "SELECT email, password_hash FROM users WHERE email = $1",
    params = list(email)
  )
  expect_equal(nrow(stored), 1)
  expect_false(grepl(password, stored$password_hash[[1]], fixed = TRUE))
  expect_false(grepl(email, stored$password_hash[[1]], fixed = TRUE))
  expect_true(sodium::password_verify(stored$password_hash[[1]], password))

  logged_in <- authenticate_user(db_pool, email, password)
  expect_true(logged_in$ok)
  expect_true(user_needs_onboarding(logged_in$user))
  expect_true(user_needs_workspace_tour(logged_in$user))
})

test_that("profile can be saved with optional blank institution", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("profile-%s@example.com", suffix),
    "correct-horse-battery"
  )

  saved <- save_user_profile(
    db_pool,
    registered$user$id,
    display_name = "Melika",
    research_role = "PhD student",
    research_field = "Cancer biology",
    institution = "   "
  )

  expect_equal(saved$display_name, "Melika")
  expect_equal(saved$research_role, "PhD student")
  expect_equal(saved$research_field, "Cancer biology")
  expect_null(saved$institution)
  expect_false(user_needs_onboarding(saved))
})

test_that("onboarding can be skipped without blocking projects", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("skip-%s@example.com", suffix),
    "correct-horse-battery"
  )
  skipped <- complete_onboarding(db_pool, registered$user$id)
  expect_false(user_needs_onboarding(skipped))
  expect_null(blank_to_null(skipped$display_name))

  created <- create_project(
    db_pool,
    skipped$id,
    "Skipped profile project",
    "Does skipping onboarding still allow an investigation?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  expect_true(created$ok)
  owned <- get_owned_project(db_pool, created$project_id, skipped$id)
  expect_equal(owned$title, "Skipped profile project")
})

test_that("existing users without a profile are not blocked", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("legacy-%s@example.com", suffix)
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
  expect_true(logged_in$ok)
  expect_false(user_needs_onboarding(logged_in$user))
  expect_false(user_needs_workspace_tour(logged_in$user))
  expect_null(blank_to_null(logged_in$user$display_name))

  created <- create_project(
    db_pool,
    logged_in$user$id,
    "Legacy workspace",
    "Existing accounts keep working without a profile.",
    "Non-small-cell lung cancer",
    c("EGFR", "MET")
  )
  expect_true(created$ok)
})

test_that("profile metadata does not change Open Targets cache keys", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("noscientific-%s@example.com", suffix),
    "correct-horse-battery"
  )
  before <- cache_key_opentargets_association("ENSG00000146648", "MONDO_0005233")
  save_user_profile(
    db_pool,
    registered$user$id,
    display_name = "Not a scientific input",
    research_role = "Industry scientist",
    research_field = "Drug discovery",
    institution = "Example Lab"
  )
  after <- cache_key_opentargets_association("ENSG00000146648", "MONDO_0005233")
  expect_equal(before, after)
  expect_equal(before, "opentargets:association:ENSG00000146648:MONDO_0005233")
})

test_that("first investigation reuses project creation and owner checks", {
  skip_if_not_installed("shiny")
  library(shiny)

  html <- as.character(mod_project_setup_ui("project_setup", variant = "onboarding"))
  expect_match(html, "Create your first investigation")
  expect_match(html, "2–8 candidate genes|2-8 candidate genes")
  expect_false(grepl("EGFR\nKRAS\nMET\nTP53", html, fixed = TRUE) && grepl("value=\"EGFR", html))

  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("owner-%s@example.com", suffix), "correct-horse-battery")
  other <- register_user(db_pool, sprintf("other-%s@example.com", suffix), "correct-horse-battery")
  complete_onboarding(db_pool, owner$user$id)
  complete_onboarding(db_pool, other$user$id)

  created <- create_project(
    db_pool,
    owner$user$id,
    "First investigation",
    "Reuse the existing project module.",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  expect_true(created$ok)
  expect_null(get_owned_project(db_pool, created$project_id, other$user$id))
  expect_equal(
    get_owned_project(db_pool, created$project_id, owner$user$id)$user_id,
    owner$user$id
  )
})

test_that("automated tests use the isolated tw_test schema", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  schema <- DBI::dbGetQuery(db_pool, "SELECT current_schema() AS schema")$schema[[1]]
  expect_equal(schema, "tw_test")

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("isolated-%s@example.com", suffix)
  register_user(db_pool, email, "correct-horse-battery")

  in_test <- DBI::dbGetQuery(
    db_pool,
    "SELECT count(*)::int AS n FROM users WHERE email = $1",
    params = list(email)
  )$n[[1]]
  expect_equal(in_test, 1L)

  config <- get_app_config()
  public_con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = config$pg_host,
    port = config$pg_port,
    dbname = config$pg_database,
    user = config$pg_user,
    password = config$pg_password,
    sslmode = config$pg_sslmode
  )
  on.exit(DBI::dbDisconnect(public_con), add = TRUE)

  public_users <- DBI::dbGetQuery(
    public_con,
    "
    SELECT count(*)::int AS n
    FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name = 'users'
    "
  )$n[[1]]

  if (identical(public_users, 1L)) {
    leaked <- DBI::dbGetQuery(
      public_con,
      "SELECT count(*)::int AS n FROM public.users WHERE email = $1",
      params = list(email)
    )$n[[1]]
    expect_equal(leaked, 0L)
  }
})
