NOTE_SCOPES <- c("project", "target", "snapshot")

note_from_row <- function(row) {
  if (is.null(row) || nrow(row) != 1L) {
    return(NULL)
  }
  list(
    id = as.character(row$id[[1]]),
    project_id = as.character(row$project_id[[1]]),
    user_id = as.character(row$user_id[[1]]),
    project_target_id = blank_to_null(row$project_target_id[[1]]),
    snapshot_id = blank_to_null(row$snapshot_id[[1]]),
    scope = as.character(row$scope[[1]]),
    title = blank_to_null(row$title[[1]]),
    body = as.character(row$body[[1]]),
    created_at = row$created_at[[1]],
    updated_at = row$updated_at[[1]]
  )
}

list_research_notes <- function(
  db_pool,
  project_id,
  user_id,
  scope = NULL,
  project_target_id = NULL,
  snapshot_id = NULL
) {
  rows <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      n.id::text AS id,
      n.project_id::text AS project_id,
      n.user_id::text AS user_id,
      n.project_target_id::text AS project_target_id,
      n.snapshot_id::text AS snapshot_id,
      n.scope,
      n.title,
      n.body,
      n.created_at,
      n.updated_at
    FROM research_notes n
    INNER JOIN projects p
      ON p.id = n.project_id
     AND p.user_id = $2::uuid
    WHERE n.project_id = $1::uuid
    ORDER BY n.updated_at DESC, n.created_at DESC
    ",
    params = list(project_id, user_id)
  )
  if (nrow(rows) == 0) {
    return(rows)
  }
  if (has_display_text(scope)) {
    rows <- rows[rows$scope == scope, , drop = FALSE]
  }
  if (has_display_text(project_target_id)) {
    rows <- rows[as.character(rows$project_target_id) == as.character(project_target_id), , drop = FALSE]
  }
  if (has_display_text(snapshot_id)) {
    rows <- rows[as.character(rows$snapshot_id) == as.character(snapshot_id), , drop = FALSE]
  }
  rows
}

get_owned_note <- function(db_pool, note_id, user_id) {
  result <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      n.id::text AS id,
      n.project_id::text AS project_id,
      n.user_id::text AS user_id,
      n.project_target_id::text AS project_target_id,
      n.snapshot_id::text AS snapshot_id,
      n.scope,
      n.title,
      n.body,
      n.created_at,
      n.updated_at
    FROM research_notes n
    INNER JOIN projects p
      ON p.id = n.project_id
     AND p.user_id = $2::uuid
    WHERE n.id = $1::uuid
    ",
    params = list(note_id, user_id)
  )
  if (nrow(result) != 1) {
    return(NULL)
  }
  note_from_row(result)
}

create_research_note <- function(
  db_pool,
  user_id,
  project_id,
  body,
  scope = "project",
  title = NULL,
  project_target_id = NULL,
  snapshot_id = NULL
) {
  project <- get_owned_project(db_pool, project_id, user_id)
  if (is.null(project)) {
    return(list(ok = FALSE, error = "Project was not found or you do not have access to it."))
  }
  text <- trimws(as.character(body %||% ""))
  if (!nzchar(text)) {
    return(list(ok = FALSE, error = "Write a research note before saving."))
  }
  body_len <- enforce_length(text, INPUT_LIMITS$note_body, "Note")
  if (!isTRUE(body_len$ok)) {
    return(list(ok = FALSE, error = body_len$message))
  }
  title_check <- enforce_length(title %||% "", INPUT_LIMITS$note_title, "Note title")
  if (!isTRUE(title_check$ok)) {
    return(list(ok = FALSE, error = title_check$message))
  }
  scope <- as.character(scope)
  if (!scope %in% NOTE_SCOPES) {
    return(list(ok = FALSE, error = "Unknown note scope."))
  }
  target_id <- NULL
  snap_id <- NULL
  if (identical(scope, "target")) {
    row <- get_owned_target(db_pool, project_target_id, user_id)
    if (is.null(row) || !identical(as.character(row$project_id[[1]]), as.character(project_id))) {
      return(list(ok = FALSE, error = "Target was not found in this project."))
    }
    target_id <- as.character(row$id[[1]])
  } else if (identical(scope, "snapshot")) {
    snap <- get_owned_snapshot(db_pool, snapshot_id, user_id)
    if (is.null(snap) || !identical(as.character(snap$project_id), as.character(project_id))) {
      return(list(ok = FALSE, error = "Snapshot was not found in this project."))
    }
    snap_id <- as.character(snap$id)
  }
  note_id <- uuid::UUIDgenerate()
  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO research_notes (
      id, project_id, user_id, project_target_id, snapshot_id, scope, title, body
    )
    VALUES ($1::uuid, $2::uuid, $3::uuid, $4::uuid, $5::uuid, $6, $7, $8)
    ",
    params = list(
      note_id,
      project_id,
      user_id,
      null_to_na(target_id),
      null_to_na(snap_id),
      scope,
      null_to_na(blank_to_null(title)),
      text
    )
  )
  list(ok = TRUE, note_id = note_id, note = get_owned_note(db_pool, note_id, user_id))
}

update_research_note <- function(db_pool, note_id, user_id, body, title = NULL) {
  existing <- get_owned_note(db_pool, note_id, user_id)
  if (is.null(existing)) {
    return(list(ok = FALSE, error = "Note was not found or you do not have access to it."))
  }
  text <- trimws(as.character(body %||% ""))
  if (!nzchar(text)) {
    return(list(ok = FALSE, error = "Write a research note before saving."))
  }
  body_len <- enforce_length(text, INPUT_LIMITS$note_body, "Note")
  if (!isTRUE(body_len$ok)) {
    return(list(ok = FALSE, error = body_len$message))
  }
  title_check <- enforce_length(title %||% "", INPUT_LIMITS$note_title, "Note title")
  if (!isTRUE(title_check$ok)) {
    return(list(ok = FALSE, error = title_check$message))
  }
  n <- DBI::dbExecute(
    db_pool,
    "
    UPDATE research_notes n
    SET body = $3,
        title = $4,
        updated_at = NOW()
    FROM projects p
    WHERE n.id = $1::uuid
      AND n.project_id = p.id
      AND p.user_id = $2::uuid
    ",
    params = list(note_id, user_id, text, null_to_na(title %||% existing$title))
  )
  list(ok = isTRUE(n == 1L), note = get_owned_note(db_pool, note_id, user_id))
}

delete_research_note <- function(db_pool, note_id, user_id) {
  n <- DBI::dbExecute(
    db_pool,
    "
    DELETE FROM research_notes n
    USING projects p
    WHERE n.id = $1::uuid
      AND n.project_id = p.id
      AND p.user_id = $2::uuid
    ",
    params = list(note_id, user_id)
  )
  isTRUE(n == 1L)
}
