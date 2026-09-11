test_that("account center exposes profile, security, support, and guidance", {
  skip_if_not_installed("shiny")
  library(shiny)

  html <- as.character(mod_account_ui("account"))
  expect_match(html, "Account center")
  expect_match(html, ">Profile<")
  expect_match(html, "Account &amp; security|Account & security")
  expect_match(html, "Help &amp; support|Help & support")
  expect_match(html, "Product guidance")
  expect_match(html, "Account actions")
  expect_match(html, "Restart product tour")
  expect_match(html, "Restart project setup guide")
  expect_match(html, "Send a support request")
  expect_match(html, "Change password")
  expect_match(html, "Current password")
  expect_match(html, "Back to workspace")
  expect_match(html, "UniProt")
  expect_match(html, "Ensembl")
  expect_match(html, "Open Targets")
  expect_match(html, "Data &amp; privacy|Data & privacy")
  expect_false(grepl("Forgot password", html, ignore.case = TRUE))
  expect_false(grepl("Delete account", html, ignore.case = TRUE))
  expect_false(grepl("response time", html, ignore.case = TRUE))
})

test_that("header uses a single account trigger without duplicated identity", {
  server_src <- paste(readLines(file.path(app_root(), "R/server.R")), collapse = "\n")
  expect_match(server_src, "open_account")
  expect_match(server_src, "account-trigger")
  expect_match(server_src, "mod_account_ui")
  expect_false(grepl("account-meta", server_src))
  expect_false(grepl("tags\\$summary\\(\"Account\"\\)", server_src))
  expect_false(grepl("Restart tour", server_src))
  expect_false(grepl("actionButton\\(\\s*\"logout\"", server_src))
})

test_that("missing profile fields display Not provided", {
  expect_equal(profile_display_value(NULL), "Not provided")
  expect_equal(profile_display_value("  "), "Not provided")
  expect_equal(profile_display_value("Melika"), "Melika")
})

test_that("password eye control is reusable and does not rewrite the value", {
  skip_if_not_installed("shiny")
  library(shiny)

  html <- as.character(password_field_ui("secret", "Password"))
  expect_match(html, 'type="password"')
  expect_match(html, 'aria-label="Show password"')
  expect_match(html, "tw-eye-show")
  expect_match(html, "tw-eye-hide")
  expect_false(grepl(">Show<", html))

  js <- paste(readLines(file.path(app_root(), "www/auth.js")), collapse = "\n")
  expect_match(js, "field.type")
  expect_false(grepl("field\\.value", js))
  expect_false(grepl("Shiny.setInputValue\\([^)]*password", js))

  account <- as.character(mod_account_ui("account"))
  expect_equal(length(gregexpr("data-password-toggle", account)[[1]]), 3L)
})

test_that("profile loads and edits persist without completing onboarding again", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  registered <- register_user(
    db_pool,
    sprintf("acct-profile-%s@example.com", suffix),
    "correct-horse-battery"
  )
  complete_onboarding(db_pool, registered$user$id)
  before <- load_user_by_id(db_pool, registered$user$id)
  expect_equal(profile_display_value(before$display_name), "Not provided")

  saved <- save_user_profile(
    db_pool,
    registered$user$id,
    display_name = "Melika",
    research_role = "PhD student",
    research_field = "Cancer biology",
    institution = "Example Lab",
    complete_onboarding_flag = FALSE
  )
  expect_equal(saved$display_name, "Melika")
  expect_equal(saved$research_role, "PhD student")
  expect_equal(saved$institution, "Example Lab")
  expect_false(is.null(saved$profile_updated_at))
  expect_equal(saved$onboarding_completed_at, before$onboarding_completed_at)
})

test_that("password change verifies current password and hashes the new one", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("acct-pass-%s@example.com", suffix)
  old <- "correct-horse-battery"
  new <- "new-correct-battery"
  registered <- register_user(db_pool, email, old)

  wrong_current <- change_password(db_pool, registered$user$id, "nope-nope", new, new)
  expect_false(wrong_current$ok)
  expect_match(wrong_current$message, "Current password")

  mismatch <- change_password(db_pool, registered$user$id, old, new, "other-password")
  expect_false(mismatch$ok)
  expect_match(mismatch$message, "do not match")

  short <- change_password(db_pool, registered$user$id, old, "short", "short")
  expect_false(short$ok)

  ok <- change_password(db_pool, registered$user$id, old, new, new)
  expect_true(ok$ok)

  stored <- DBI::dbGetQuery(
    db_pool,
    "SELECT password_hash FROM users WHERE id = $1::uuid",
    params = list(registered$user$id)
  )
  expect_false(grepl(new, stored$password_hash[[1]], fixed = TRUE))
  expect_true(sodium::password_verify(stored$password_hash[[1]], new))
  expect_false(sodium::password_verify(stored$password_hash[[1]], old))

  old_login <- authenticate_user(db_pool, email, old)
  expect_false(old_login$ok)
  new_login <- authenticate_user(db_pool, email, new)
  expect_true(new_login$ok)
})

test_that("support requests are owner-scoped and start as submitted", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(
    db_pool,
    sprintf("support-owner-%s@example.com", suffix),
    "correct-horse-battery"
  )
  other <- register_user(
    db_pool,
    sprintf("support-other-%s@example.com", suffix),
    "correct-horse-battery"
  )

  created <- create_support_request(
    db_pool,
    owner$user$id,
    "Bug or technical issue",
    "Tour did not start",
    "New account reached the workspace without a coach mark."
  )
  expect_true(created$ok)

  owned <- list_owned_support_requests(db_pool, owner$user$id)
  expect_equal(nrow(owned), 1)
  expect_equal(owned$category[[1]], "Bug or technical issue")
  expect_equal(owned$subject[[1]], "Tour did not start")
  expect_equal(owned$status[[1]], "submitted")

  other_list <- list_owned_support_requests(db_pool, other$user$id)
  expect_equal(nrow(other_list), 0)

  stolen <- get_owned_support_request(db_pool, created$request_id, other$user$id)
  expect_null(stolen)
  visible <- get_owned_support_request(db_pool, created$request_id, owner$user$id)
  expect_equal(visible$message[[1]], "New account reached the workspace without a coach mark.")
})

test_that("account and support operations do not call scientific APIs", {
  account_src <- paste(readLines(file.path(app_root(), "R/modules/mod_account.R")), collapse = "\n")
  support_src <- paste(readLines(file.path(app_root(), "R/db/support.R")), collapse = "\n")
  expect_false(grepl("fetch_uniprot|fetch_ensembl|fetch_opentargets|confirm_target|confirm_disease", account_src))
  expect_false(grepl("fetch_uniprot|fetch_ensembl|fetch_opentargets", support_src))
})
