validate_project_input <- function(
  title,
  research_question,
  disease_label,
  target_inputs,
  max_targets = 8L
) {
  title <- trimws(title)
  research_question <- trimws(research_question)
  disease_label <- trimws(disease_label)

  if (!nzchar(title)) {
    return(list(ok = FALSE, message = "Project title is required."))
  }
  title_len <- enforce_length(title, INPUT_LIMITS$project_title, "Project title")
  if (!isTRUE(title_len$ok)) {
    return(title_len)
  }

  if (!nzchar(research_question)) {
    return(list(ok = FALSE, message = "Research question is required."))
  }
  question_len <- enforce_length(
    research_question,
    INPUT_LIMITS$research_question,
    "Research question"
  )
  if (!isTRUE(question_len$ok)) {
    return(question_len)
  }

  if (!nzchar(disease_label)) {
    return(list(ok = FALSE, message = "Disease context is required."))
  }
  disease_len <- enforce_length(disease_label, INPUT_LIMITS$disease_label, "Disease context")
  if (!isTRUE(disease_len$ok)) {
    return(disease_len)
  }

  target_inputs <- dedupe_target_inputs(target_inputs)
  if (any(nchar(target_inputs) > INPUT_LIMITS$target_input)) {
    return(list(
      ok = FALSE,
      message = sprintf(
        "Each candidate target must be at most %s characters.",
        INPUT_LIMITS$target_input
      )
    ))
  }

  if (length(target_inputs) == 0) {
    return(list(
      ok = FALSE,
      message = "Add at least one candidate target."
    ))
  }

  if (length(target_inputs) > max_targets) {
    return(list(
      ok = FALSE,
      message = sprintf(
        "A project may contain at most %d candidate targets.",
        max_targets
      )
    ))
  }

  list(
    ok = TRUE,
    title = title,
    research_question = research_question,
    disease_label = disease_label,
    target_inputs = target_inputs
  )
}

list_projects <- function(db_pool, user_id, include_archived = FALSE) {
  sql <- "
    SELECT
      p.id::text AS id,
      p.title,
      p.research_question,
      p.organism,
      p.disease_label,
      p.status,
      p.created_at,
      p.updated_at,
      COUNT(pt.id)::integer AS target_count
    FROM projects p
    LEFT JOIN project_targets pt
      ON pt.project_id = p.id
     AND pt.removed_at IS NULL
    WHERE p.user_id = $1::uuid
  "

  params <- list(user_id)

  if (!include_archived) {
    sql <- paste0(sql, " AND p.status = 'active' ")
  }

  sql <- paste0(
    sql,
    "
    GROUP BY p.id
    ORDER BY p.updated_at DESC, p.created_at DESC
    "
  )

  DBI::dbGetQuery(db_pool, sql, params = params)
}

create_project <- function(
  db_pool,
  user_id,
  title,
  research_question,
  disease_label,
  target_inputs
) {
  config <- get_app_config()

  checked <- validate_project_input(
    title = title,
    research_question = research_question,
    disease_label = disease_label,
    target_inputs = target_inputs,
    max_targets = config$max_targets
  )

  if (!isTRUE(checked$ok)) {
    return(checked)
  }

  project_id <- uuid::UUIDgenerate()

  con <- pool::poolCheckout(db_pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  DBI::dbBegin(con)

  tryCatch(
    {
      DBI::dbExecute(
        con,
        "
        INSERT INTO projects (
          id,
          user_id,
          title,
          research_question,
          organism,
          disease_label
        )
        VALUES ($1::uuid, $2::uuid, $3, $4, 'Homo sapiens', $5)
        ",
        params = list(
          project_id,
          user_id,
          checked$title,
          checked$research_question,
          checked$disease_label
        )
      )

      insert_project_event(con, project_id, "PROJECT_CREATED", user_id)

      for (target_text in checked$target_inputs) {
        target_id <- uuid::UUIDgenerate()
        DBI::dbExecute(
          con,
          "
          INSERT INTO project_targets (
            id,
            project_id,
            input_text,
            resolution_status
          )
          VALUES ($1::uuid, $2::uuid, $3, 'unresolved')
          ",
          params = list(
            target_id,
            project_id,
            target_text
          )
        )
        insert_project_event(
          con,
          project_id,
          "TARGET_ADDED",
          user_id,
          project_target_id = target_id,
          metadata = list(label = target_text, input_text = target_text)
        )
      }

      DBI::dbCommit(con)

      list(
        ok = TRUE,
        project_id = project_id
      )
    },
    error = function(e) {
      try(DBI::dbRollback(con), silent = TRUE)

      list(
        ok = FALSE,
        message = unique_violation_message(
          e,
          fallback = paste(
            "The project could not be created.",
            conditionMessage(e)
          )
        )
      )
    }
  )
}

archive_project <- function(db_pool, project_id, user_id) {
  affected <- DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET status = 'archived',
        updated_at = NOW()
    WHERE id = $1::uuid
      AND user_id = $2::uuid
      AND status = 'active'
    ",
    params = list(project_id, user_id)
  )

  affected == 1
}

save_disease_lookup <- function(db_pool, project_id, user_id, status, payload) {
  payload_json <- as.character(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null")
  )

  affected <- DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET disease_resolution_status = $3,
        disease_resolution_payload = $4::jsonb,
        updated_at = NOW()
    WHERE id = $1::uuid
      AND user_id = $2::uuid
      AND (
        disease_resolution_status IS NULL
        OR disease_resolution_status <> 'confirmed'
      )
    ",
    params = list(project_id, user_id, status, payload_json)
  )

  affected == 1
}

confirm_project_disease <- function(
  db_pool,
  project_id,
  user_id,
  disease_ontology_id,
  disease_name
) {
  disease_ontology_id <- trimws(disease_ontology_id)
  disease_name <- trimws(disease_name)

  if (!has_display_text(disease_ontology_id) || !has_display_text(disease_name)) {
    return(list(
      ok = FALSE,
      message = "Confirmation requires an Open Targets disease identifier and name."
    ))
  }

  tx <- tryCatch(
    pool::poolWithTransaction(db_pool, function(con) {
      affected <- DBI::dbExecute(
        con,
        "
        UPDATE projects
        SET disease_ontology_id = $3,
            disease_name = $4,
            disease_resolution_status = 'confirmed',
            disease_confirmed_at = NOW(),
            updated_at = NOW()
        WHERE id = $1::uuid
          AND user_id = $2::uuid
          AND (
            disease_resolution_status IS NULL
            OR disease_resolution_status <> 'confirmed'
          )
        ",
        params = list(project_id, user_id, disease_ontology_id, disease_name)
      )
      if (affected != 1) {
        stop("confirm_project_disease_no_row", call. = FALSE)
      }
      insert_project_event(
        con,
        project_id,
        "DISEASE_IDENTITY_CONFIRMED",
        user_id,
        metadata = list(
          disease_name = disease_name,
          disease_ontology_id = disease_ontology_id
        )
      )
      TRUE
    }),
    error = function(e) e
  )
  if (inherits(tx, "error")) {
    return(list(ok = FALSE, message = "The disease context could not be confirmed."))
  }

  list(ok = TRUE)
}

update_project_presentation <- function(
  db_pool,
  project_id,
  user_id,
  title,
  research_question
) {
  checked <- validate_project_input(
    title = title,
    research_question = research_question,
    disease_label = "placeholder",
    target_inputs = c("placeholder"),
    max_targets = 8L
  )
  if (!isTRUE(checked$ok)) {
    return(checked)
  }

  affected <- DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET title = $3,
        research_question = $4,
        updated_at = NOW()
    WHERE id = $1::uuid
      AND user_id = $2::uuid
    ",
    params = list(project_id, user_id, checked$title, checked$research_question)
  )
  if (affected != 1) {
    return(list(ok = FALSE, message = "The project could not be updated."))
  }
  list(ok = TRUE, invalidation = live_invalidation_event("presentation"))
}

reset_project_disease <- function(db_pool, project_id, user_id, disease_label) {
  disease_label <- trimws(as.character(disease_label %||% ""))
  if (!nzchar(disease_label)) {
    return(list(ok = FALSE, message = "Disease context is required."))
  }
  disease_len <- enforce_length(disease_label, INPUT_LIMITS$disease_label, "Disease context")
  if (!isTRUE(disease_len$ok)) {
    return(disease_len)
  }

  current <- get_owned_project(db_pool, project_id, user_id)
  if (is.null(current)) {
    return(list(ok = FALSE, message = "The disease context could not be changed."))
  }
  same_wording <- identical(trimws(as.character(current$disease_label[[1]])), disease_label)
  if (isTRUE(same_wording) && identical(as.character(current$disease_resolution_status[[1]]), "unresolved")) {
    return(list(ok = TRUE, invalidation = live_invalidation_event("presentation")))
  }

  affected <- DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET disease_label = $3,
        disease_ontology_id = NULL,
        disease_name = NULL,
        disease_resolution_status = 'unresolved',
        disease_resolution_payload = NULL,
        disease_confirmed_at = NULL,
        updated_at = NOW()
    WHERE id = $1::uuid
      AND user_id = $2::uuid
    ",
    params = list(project_id, user_id, disease_label)
  )
  if (affected != 1) {
    return(list(ok = FALSE, message = "The disease context could not be changed."))
  }
  list(ok = TRUE, invalidation = live_invalidation_event("disease"))
}

restore_project <- function(db_pool, project_id, user_id) {
  affected <- DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET status = 'active',
        updated_at = NOW()
    WHERE id = $1::uuid
      AND user_id = $2::uuid
      AND status = 'archived'
    ",
    params = list(project_id, user_id)
  )
  affected == 1
}

project_disease_ontology_id <- function(project_row) {
  if (is.null(project_row)) {
    return(NA_character_)
  }
  as.character(project_row$disease_ontology_id[[1]])
}

project_disease_is_confirmed <- function(project_row) {
  !is.null(project_row) &&
    identical(as.character(project_row$disease_resolution_status[[1]]), "confirmed") &&
    has_display_text(project_disease_ontology_id(project_row))
}
