test_that("sodium password hashes verify correctly", {
  skip_if_not_installed("sodium")

  password <- "TargetWeave-test-password"
  stored <- sodium::password_store(password)

  expect_true(sodium::password_verify(stored, password))
  expect_false(sodium::password_verify(stored, "wrong-password"))
})

test_that("email normalization and validation are strict", {
  expect_equal(
    normalize_email("  Melika@Example.COM "),
    "melika@example.com"
  )

  expect_true(validate_email("melika@example.com"))
  expect_false(validate_email("not-an-email"))
  expect_false(validate_email("missing-domain@"))
})

test_that("register then a new database pool can authenticate the same user", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("relogin-%s@example.com", suffix)
  password <- "same-password-123"

  registered <- register_user(db_pool, email, password)
  expect_true(registered$ok)

  persisted <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT email,
           length(password_hash) AS hash_length,
           octet_length(password_hash) AS hash_bytes
    FROM users
    WHERE email = $1
    ",
    params = list(email)
  )
  expect_equal(nrow(persisted), 1)
  expect_equal(persisted$email, email)
  expect_gt(persisted$hash_length, 20)
  expect_equal(persisted$hash_length, persisted$hash_bytes)

  pool::poolClose(db_pool)

  db_pool <- create_db_pool()
  logged_in <- authenticate_user(db_pool, toupper(email), password)

  expect_true(logged_in$ok)
  expect_equal(logged_in$user$email, email)
  expect_equal(logged_in$user$id, registered$user$id)

  expect_false(authenticate_user(db_pool, email, "wrong-password")$ok)
  expect_false(authenticate_user(db_pool, email, "")$ok)
})

test_that("registration requires matching passwords and never stores confirm_password", {
  expect_false(validate_registration_passwords("short", "short")$ok)
  expect_equal(
    validate_registration_passwords("short", "short")$message,
    "Password must contain at least 8 characters."
  )
  expect_false(validate_registration_passwords("long-enough", "different1")$ok)
  expect_equal(
    validate_registration_passwords("long-enough", "different1")$message,
    "Passwords do not match."
  )
  expect_true(validate_registration_passwords("long-enough", "long-enough")$ok)

  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email <- sprintf("confirm-%s@example.com", suffix)
  password <- "correct-horse-battery"

  registered <- register_user(db_pool, email, password)
  expect_true(registered$ok)

  cols <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = current_schema()
      AND table_name = 'users'
    "
  )$column_name
  expect_false("confirm_password" %in% cols)
  expect_false("password" %in% cols)

  stored <- DBI::dbGetQuery(
    db_pool,
    "SELECT password_hash FROM users WHERE email = $1",
    params = list(email)
  )
  expect_equal(nrow(stored), 1)
  expect_false(grepl(password, stored$password_hash[[1]], fixed = TRUE))
  expect_true(sodium::password_verify(stored$password_hash[[1]], password))
})

test_that("password visibility toggle is client-side and labeled", {
  skip_if_not_installed("shiny")
  library(shiny)

  html <- as.character(auth_sign_in_ui(NS("auth")))
  expect_match(html, 'type="password"')
  expect_match(html, "Show password")
  expect_match(html, "data-password-toggle")
  expect_false(grepl("confirm_password", html))

  js <- paste(readLines(file.path(app_root(), "www/auth.js")), collapse = "\n")
  expect_match(js, "Hide password")
  expect_match(js, "field.type")
  expect_false(grepl("Shiny.setInputValue\\([^)]*password", js))
})

