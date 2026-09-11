normalize_email <- function(email) {
  tolower(trimws(email))
}

validate_email <- function(email) {
  grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", email)
}

validate_registration_passwords <- function(password, confirm_password) {
  if (is.null(password) || !nzchar(as.character(password))) {
    return(list(
      ok = FALSE,
      message = "Password must contain at least 8 characters."
    ))
  }
  password <- as.character(password)
  confirm_password <- as.character(confirm_password %||% "")

  if (nchar(password) < 8) {
    return(list(
      ok = FALSE,
      message = "Password must contain at least 8 characters."
    ))
  }
  pw_len <- enforce_length(password, INPUT_LIMITS$password, "Password")
  if (!isTRUE(pw_len$ok)) {
    return(pw_len)
  }

  if (!identical(password, confirm_password)) {
    return(list(
      ok = FALSE,
      message = "Passwords do not match."
    ))
  }

  list(ok = TRUE)
}

.login_attempts <- new.env(parent = emptyenv())
.dummy_password_hash <- new.env(parent = emptyenv())
LOGIN_WINDOW_SEC <- 15 * 60
LOGIN_MAX_ATTEMPTS <- 8L

dummy_password_hash <- function() {
  if (is.null(.dummy_password_hash$value)) {
    .dummy_password_hash$value <- sodium::password_store("tw-dummy-hash-never-used")
  }
  .dummy_password_hash$value
}

login_is_throttled <- function(email) {
  key <- normalize_email(email)
  rec <- .login_attempts[[key]]
  if (is.null(rec)) {
    return(FALSE)
  }
  if (as.numeric(difftime(Sys.time(), rec$first, units = "secs")) > LOGIN_WINDOW_SEC) {
    rm(list = key, envir = .login_attempts)
    return(FALSE)
  }
  rec$count >= LOGIN_MAX_ATTEMPTS
}

record_login_failure <- function(email) {
  key <- normalize_email(email)
  rec <- .login_attempts[[key]]
  now <- Sys.time()
  if (is.null(rec) || as.numeric(difftime(now, rec$first, units = "secs")) > LOGIN_WINDOW_SEC) {
    assign(key, list(count = 1L, first = now), envir = .login_attempts)
  } else {
    rec$count <- rec$count + 1L
    assign(key, rec, envir = .login_attempts)
  }
  invisible(TRUE)
}

clear_login_failures <- function(email) {
  key <- normalize_email(email)
  if (exists(key, envir = .login_attempts, inherits = FALSE)) {
    rm(list = key, envir = .login_attempts)
  }
  invisible(TRUE)
}

# Registration reports an existing email. Sign-in does not:
# unknown emails get the same "Incorrect email or password" message as bad
# passwords, after a dummy hash verify, to reduce account enumeration on login.
register_user <- function(db_pool, email, password) {
  email <- normalize_email(email)
  email_len <- enforce_length(email, INPUT_LIMITS$email, "Email")
  if (!isTRUE(email_len$ok)) {
    return(email_len)
  }

  if (!validate_email(email)) {
    return(list(ok = FALSE, message = "Please enter a valid email address."))
  }

  pw_check <- validate_registration_passwords(password, password)
  if (!isTRUE(pw_check$ok)) {
    return(pw_check)
  }

  existing <- DBI::dbGetQuery(
    db_pool,
    "SELECT id FROM users WHERE email = $1",
    params = list(email)
  )

  if (nrow(existing) > 0) {
    return(list(
      ok = FALSE,
      message = "An account with this email already exists."
    ))
  }

  user_id <- uuid::UUIDgenerate()
  password_hash <- sodium::password_store(password)

  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO users (id, email, password_hash)
    VALUES ($1::uuid, $2, $3)
    ",
    params = list(user_id, email, password_hash)
  )

  list(
    ok = TRUE,
    user = load_user_by_id(db_pool, user_id)
  )
}

authenticate_user <- function(db_pool, email, password) {
  email <- normalize_email(email)

  if (!nzchar(email) || is.null(password) || !nzchar(password)) {
    return(list(ok = FALSE, message = "Email and password are required."))
  }

  if (isTRUE(login_is_throttled(email))) {
    return(list(
      ok = FALSE,
      message = "Too many sign-in attempts. Try again later."
    ))
  }

  row <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT id::text AS id, email, password_hash
    FROM users
    WHERE email = $1
    ",
    params = list(email)
  )

  stored_hash <- if (nrow(row) != 1) {
    dummy_password_hash()
  } else {
    as.character(row$password_hash[[1]])
  }

  password_ok <- tryCatch(
    sodium::password_verify(stored_hash, as.character(password)),
    error = function(e) FALSE
  )

  if (nrow(row) != 1 || !isTRUE(password_ok)) {
    record_login_failure(email)
    return(list(ok = FALSE, message = "Incorrect email or password."))
  }

  clear_login_failures(email)
  list(
    ok = TRUE,
    user = load_user_by_id(db_pool, row$id[[1]])
  )
}

change_password <- function(db_pool, user_id, current_password, new_password, confirm_password) {
  check <- validate_registration_passwords(new_password, confirm_password)
  if (!isTRUE(check$ok)) {
    return(check)
  }

  if (is.null(current_password) || !nzchar(as.character(current_password))) {
    return(list(ok = FALSE, message = "Current password is required."))
  }

  row <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT password_hash
    FROM users
    WHERE id = $1::uuid
    ",
    params = list(user_id)
  )

  if (nrow(row) != 1) {
    return(list(ok = FALSE, message = "The password could not be updated."))
  }

  current_ok <- tryCatch(
    sodium::password_verify(as.character(row$password_hash[[1]]), as.character(current_password)),
    error = function(e) FALSE
  )

  if (!isTRUE(current_ok)) {
    return(list(ok = FALSE, message = "Current password is incorrect."))
  }

  if (identical(as.character(current_password), as.character(new_password))) {
    return(list(ok = FALSE, message = "Choose a different new password."))
  }

  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET password_hash = $2
    WHERE id = $1::uuid
    ",
    params = list(user_id, sodium::password_store(as.character(new_password)))
  )

  list(ok = TRUE)
}
