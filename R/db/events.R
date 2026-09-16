PROJECT_EVENT_TYPES <- c(
  "PROJECT_CREATED",
  "TARGET_ADDED",
  "TARGET_REMOVED",
  "TARGET_IDENTITY_CONFIRMED",
  "DISEASE_IDENTITY_CONFIRMED",
  "SNAPSHOT_CREATED"
)

insert_project_event <- function(
  db,
  project_id,
  event_type,
  actor_user_id,
  project_target_id = NULL,
  metadata = list(),
  created_at = NULL
) {
  event_type <- as.character(event_type[[1]])
  if (!event_type %in% PROJECT_EVENT_TYPES) {
    stop(sprintf("Unsupported project event type: %s", event_type), call. = FALSE)
  }
  meta_json <- as.character(
    jsonlite::toJSON(metadata %||% list(), auto_unbox = TRUE, null = "null", na = "null")
  )
  target_id <- if (has_display_text(project_target_id)) {
    as.character(project_target_id)
  } else {
    NA_character_
  }
  if (is.null(created_at) || (length(created_at) == 1L && is.na(created_at[[1]]))) {
    DBI::dbExecute(
      db,
      "
      INSERT INTO project_events (
        id, project_id, event_type, actor_user_id, project_target_id, metadata, created_at
      )
      VALUES ($1::uuid, $2::uuid, $3, $4::uuid, $5::uuid, $6::jsonb, NOW())
      ",
      params = list(
        uuid::UUIDgenerate(),
        project_id,
        event_type,
        actor_user_id,
        target_id,
        meta_json
      )
    )
    return(invisible(TRUE))
  }
  DBI::dbExecute(
    db,
    "
    INSERT INTO project_events (
      id, project_id, event_type, actor_user_id, project_target_id, metadata, created_at
    )
    VALUES ($1::uuid, $2::uuid, $3, $4::uuid, $5::uuid, $6::jsonb, $7::timestamptz)
    ",
    params = list(
      uuid::UUIDgenerate(),
      project_id,
      event_type,
      actor_user_id,
      target_id,
      meta_json,
      created_at
    )
  )
  invisible(TRUE)
}

list_project_events <- function(db_pool, project_id, user_id) {
  DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      e.id::text AS id,
      e.project_id::text AS project_id,
      e.event_type,
      e.actor_user_id::text AS actor_user_id,
      e.project_target_id::text AS project_target_id,
      e.metadata::text AS metadata,
      e.created_at,
      COALESCE(NULLIF(BTRIM(up.display_name), ''), SPLIT_PART(u.email, '@', 1)) AS actor_label
    FROM project_events e
    INNER JOIN projects p ON p.id = e.project_id
    INNER JOIN users u ON u.id = e.actor_user_id
    LEFT JOIN user_profiles up ON up.user_id = u.id
    WHERE e.project_id = $1::uuid
      AND p.user_id = $2::uuid
    ORDER BY e.created_at DESC,
      CASE e.event_type
        WHEN 'SNAPSHOT_CREATED' THEN 1
        WHEN 'TARGET_REMOVED' THEN 2
        WHEN 'TARGET_IDENTITY_CONFIRMED' THEN 3
        WHEN 'DISEASE_IDENTITY_CONFIRMED' THEN 4
        WHEN 'TARGET_ADDED' THEN 5
        WHEN 'PROJECT_CREATED' THEN 6
        ELSE 7
      END ASC,
      e.id DESC
    ",
    params = list(project_id, user_id)
  )
}

decode_project_event_metadata <- function(value) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(list())
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(as.character(value[[1]]), simplifyVector = FALSE),
    error = function(e) list()
  )
  if (!is.list(parsed)) {
    return(list())
  }
  parsed
}

project_event_title <- function(event_type, metadata = list()) {
  label <- metadata$label %||% metadata$symbol %||% metadata$input_text %||% "Target"
  switch(
    as.character(event_type),
    PROJECT_CREATED = "Project created",
    TARGET_ADDED = sprintf("%s added", label),
    TARGET_REMOVED = sprintf("%s removed", label),
    TARGET_IDENTITY_CONFIRMED = sprintf("%s identity confirmed", label),
    DISEASE_IDENTITY_CONFIRMED = "Disease identity confirmed",
    SNAPSHOT_CREATED = "Evidence snapshot created",
    as.character(event_type)
  )
}

project_event_detail <- function(event_type, metadata = list()) {
  if (identical(event_type, "TARGET_IDENTITY_CONFIRMED")) {
    parts <- c(
      metadata$ensembl_gene_id %||% NULL,
      metadata$uniprot_accession %||% NULL
    )
    parts <- parts[vapply(parts, has_display_text, logical(1))]
    if (length(parts) == 0) {
      return(NULL)
    }
    return(paste(parts, collapse = " / "))
  }
  if (identical(event_type, "DISEASE_IDENTITY_CONFIRMED")) {
    name <- metadata$disease_name %||% metadata$disease_ontology_id
    if (has_display_text(name) && has_display_text(metadata$disease_ontology_id) &&
        !identical(as.character(name), as.character(metadata$disease_ontology_id))) {
      return(paste(name, metadata$disease_ontology_id, sep = " · "))
    }
    return(name)
  }
  if (identical(event_type, "SNAPSHOT_CREATED")) {
    return(metadata$name)
  }
  NULL
}

project_timeline_ui <- function(events) {
  if (is.null(events) || nrow(events) == 0) {
    return(p(class = "field-help", "No project history recorded yet."))
  }
  tags$ol(
    class = "project-timeline",
    lapply(seq_len(nrow(events)), function(i) {
      row <- events[i, ]
      meta <- decode_project_event_metadata(row$metadata)
      title <- project_event_title(row$event_type, meta)
      detail <- project_event_detail(row$event_type, meta)
      actor <- if (identical(as.character(row$event_type), "PROJECT_CREATED")) {
        NULL
      } else {
        sprintf("by %s", row$actor_label)
      }
      scientific <- identical(as.character(row$event_type), "TARGET_IDENTITY_CONFIRMED") ||
        identical(as.character(row$event_type), "DISEASE_IDENTITY_CONFIRMED")
      tags$li(
        class = paste("timeline-item", if (isTRUE(scientific)) "timeline-scientific" else "timeline-edit"),
        p(class = "timeline-when", format_user_timestamp(row$created_at)),
        p(class = "timeline-title", title),
        if (has_display_text(detail)) p(class = "timeline-detail identifier", detail),
        if (has_display_text(actor)) p(class = "timeline-actor", actor)
      )
    })
  )
}

bump_project_target_set_revision <- function(db, project_id) {
  DBI::dbExecute(
    db,
    "
    UPDATE projects
    SET target_set_revision = target_set_revision + 1,
        updated_at = NOW()
    WHERE id = $1::uuid
    ",
    params = list(project_id)
  )
  invisible(TRUE)
}
