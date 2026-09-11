# Support-admin handling and email notifications are future work.
# Requests are persisted with status 'submitted' for later review.
support_request_categories <- function() {
  c(
    "Using TargetWeave",
    "Data / evidence question",
    "Bug or technical issue",
    "Account issue",
    "Feedback / feature request",
    "Other"
  )
}

sanitize_plain_text <- function(value) {
  text <- as.character(value %||% "")
  text <- gsub("<[^>]*>", " ", text)
  text <- gsub("[[:cntrl:]]", " ", text)
  trimws(gsub("\\s+", " ", text))
}

create_support_request <- function(db_pool, user_id, category, subject, message) {
  category <- blank_to_null(category)
  subject <- blank_to_null(sanitize_plain_text(subject))
  message <- blank_to_null(sanitize_plain_text(message))

  if (is.null(category) || !(category %in% support_request_categories())) {
    return(list(ok = FALSE, message = "Choose a support category."))
  }
  if (is.null(subject)) {
    return(list(ok = FALSE, message = "Subject is required."))
  }
  if (is.null(message)) {
    return(list(ok = FALSE, message = "Message is required."))
  }
  sub_len <- enforce_length(subject, INPUT_LIMITS$support_subject, "Subject")
  if (!isTRUE(sub_len$ok)) {
    return(sub_len)
  }
  msg_len <- enforce_length(message, INPUT_LIMITS$support_message, "Message")
  if (!isTRUE(msg_len$ok)) {
    return(msg_len)
  }

  request_id <- uuid::UUIDgenerate()
  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO support_requests (
      id, user_id, category, subject, message, status
    )
    VALUES ($1::uuid, $2::uuid, $3, $4, $5, 'submitted')
    ",
    params = list(request_id, user_id, category, subject, message)
  )

  list(ok = TRUE, request_id = request_id)
}

list_owned_support_requests <- function(db_pool, user_id, limit = 5L) {
  DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      id::text AS id,
      category,
      subject,
      status,
      created_at
    FROM support_requests
    WHERE user_id = $1::uuid
    ORDER BY created_at DESC
    LIMIT $2
    ",
    params = list(user_id, as.integer(limit))
  )
}

get_owned_support_request <- function(db_pool, request_id, user_id) {
  row <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      id::text AS id,
      user_id::text AS user_id,
      category,
      subject,
      message,
      status,
      created_at
    FROM support_requests
    WHERE id = $1::uuid
      AND user_id = $2::uuid
    ",
    params = list(request_id, user_id)
  )
  if (nrow(row) != 1) {
    return(NULL)
  }
  row[1, , drop = FALSE]
}
