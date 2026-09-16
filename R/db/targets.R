normalize_target_input_key <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(character())
  }

  tolower(trimws(as.character(text)))
}

dedupe_target_inputs <- function(texts) {
  if (is.null(texts) || length(texts) == 0) {
    return(character())
  }

  pieces <- trimws(as.character(texts))
  pieces <- pieces[nzchar(pieces)]
  if (length(pieces) == 0) {
    return(character())
  }

  pieces[!duplicated(normalize_target_input_key(pieces))]
}

parse_target_input <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(character())
  }

  pieces <- unlist(
    strsplit(
      text,
      split = "[,;\\n\\r\\t]+",
      perl = TRUE
    )
  )

  dedupe_target_inputs(pieces)
}

list_project_targets <- function(db_pool, project_id, user_id, include_removed = FALSE) {
  removed_sql <- if (isTRUE(include_removed)) {
    ""
  } else {
    "AND pt.removed_at IS NULL"
  }
  DBI::dbGetQuery(
    db_pool,
    paste0(
      "
    SELECT
      pt.id::text AS id,
      pt.input_text,
      pt.display_symbol,
      pt.ensembl_gene_id,
      pt.uniprot_accession,
      pt.hgnc_id,
      pt.resolution_status,
      pt.resolution_payload::text AS resolution_payload,
      pt.confirmed_at,
      pt.created_at,
      pt.removed_at
    FROM project_targets pt
    INNER JOIN projects p ON p.id = pt.project_id
    WHERE pt.project_id = $1::uuid
      AND p.user_id = $2::uuid
      ",
      removed_sql,
      "
    ORDER BY pt.created_at ASC
    "
    ),
    params = list(project_id, user_id)
  )
}

get_owned_target <- function(db_pool, target_id, user_id) {
  result <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      pt.id::text AS id,
      pt.project_id::text AS project_id,
      p.user_id::text AS user_id,
      pt.input_text,
      pt.display_symbol,
      pt.ensembl_gene_id,
      pt.uniprot_accession,
      pt.hgnc_id,
      pt.resolution_status,
      pt.resolution_payload::text AS resolution_payload,
      pt.confirmed_at,
      pt.removed_at
    FROM project_targets pt
    INNER JOIN projects p ON p.id = pt.project_id
    WHERE pt.id = $1::uuid
      AND p.user_id = $2::uuid
      AND pt.removed_at IS NULL
    ",
    params = list(target_id, user_id)
  )

  if (nrow(result) != 1) {
    return(NULL)
  }

  result[1, , drop = FALSE]
}

unique_violation_message <- function(error, fallback) {
  text <- conditionMessage(error)
  if (grepl("idx_project_targets_input_normalized", text, fixed = TRUE)) {
    return("This project already has that target string.")
  }
  if (grepl("idx_project_targets_confirmed_ensembl", text, fixed = TRUE)) {
    return("This Ensembl gene is already confirmed in the project.")
  }
  fallback
}

db_execute_guarded <- function(db_pool, sql, params) {
  con <- pool::poolCheckout(db_pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  tryCatch(
    DBI::dbExecute(con, sql, params = params),
    error = function(e) {
      suppressWarnings(try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE))
      e
    }
  )
}

save_resolution_lookup <- function(db_pool, target_id, user_id, status, payload) {
  payload_json <- as.character(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null")
  )

  affected <- DBI::dbExecute(
    db_pool,
    "
    UPDATE project_targets pt
    SET resolution_status = $3,
        resolution_payload = $4::jsonb,
        display_symbol = NULL,
        ensembl_gene_id = NULL,
        uniprot_accession = NULL,
        hgnc_id = NULL,
        confirmed_at = NULL
    FROM projects p
    WHERE pt.project_id = p.id
      AND pt.id = $1::uuid
      AND p.user_id = $2::uuid
      AND pt.removed_at IS NULL
      AND pt.resolution_status <> 'confirmed'
    ",
    params = list(target_id, user_id, status, payload_json)
  )

  affected == 1
}

confirm_project_target <- function(
  db_pool,
  target_id,
  user_id,
  display_symbol,
  ensembl_gene_id,
  uniprot_accession,
  hgnc_id = NA_character_
) {
  display_symbol <- trimws(display_symbol)
  ensembl_gene_id <- canonical_ensembl_gene_id(ensembl_gene_id)
  uniprot_accession <- toupper(trimws(uniprot_accession))
  hgnc_id <- if (is.null(hgnc_id) || is.na(hgnc_id) || !nzchar(trimws(hgnc_id))) {
    NA_character_
  } else {
    trimws(hgnc_id)
  }

  if (!has_display_text(display_symbol) ||
      !has_display_text(ensembl_gene_id) ||
      !has_display_text(uniprot_accession)) {
    return(list(
      ok = FALSE,
      message = "Confirmation requires a symbol, Ensembl gene ID, and UniProt accession."
    ))
  }

  owned <- get_owned_target(db_pool, target_id, user_id)
  if (is.null(owned)) {
    return(list(ok = FALSE, message = "The target could not be confirmed."))
  }

  duplicate <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT pt.id::text AS id
    FROM project_targets pt
    INNER JOIN projects p ON p.id = pt.project_id
    WHERE p.user_id = $1::uuid
      AND pt.project_id = $2::uuid
      AND pt.id <> $3::uuid
      AND pt.removed_at IS NULL
      AND pt.resolution_status = 'confirmed'
      AND regexp_replace(upper(pt.ensembl_gene_id), '\\.[0-9]+$', '') = $4
    LIMIT 1
    ",
    params = list(user_id, owned$project_id[[1]], target_id, ensembl_gene_id)
  )
  if (nrow(duplicate) == 1) {
    return(list(ok = FALSE, message = "This Ensembl gene is already confirmed in the project."))
  }

  tx <- tryCatch(
    pool::poolWithTransaction(db_pool, function(con) {
      updated <- DBI::dbExecute(
        con,
        "
        UPDATE project_targets pt
        SET display_symbol = $3,
            ensembl_gene_id = $4,
            uniprot_accession = $5,
            hgnc_id = $6,
            resolution_status = 'confirmed',
            confirmed_at = NOW()
        FROM projects p
        WHERE pt.project_id = p.id
          AND pt.id = $1::uuid
          AND p.user_id = $2::uuid
          AND pt.removed_at IS NULL
          AND pt.resolution_status IN ('unresolved', 'ambiguous')
        ",
        params = list(
          target_id,
          user_id,
          display_symbol,
          ensembl_gene_id,
          uniprot_accession,
          hgnc_id
        )
      )
      if (updated != 1) {
        stop("confirm_project_target_no_row", call. = FALSE)
      }
      bumped <- DBI::dbExecute(
        con,
        "
        UPDATE projects
        SET target_set_revision = target_set_revision + 1,
            updated_at = NOW()
        WHERE id = $1::uuid
        ",
        params = list(owned$project_id[[1]])
      )
      if (bumped != 1) {
        stop("confirm_project_target_no_row", call. = FALSE)
      }
      insert_project_event(
        con,
        owned$project_id[[1]],
        "TARGET_IDENTITY_CONFIRMED",
        user_id,
        project_target_id = target_id,
        metadata = list(
          label = display_symbol,
          ensembl_gene_id = ensembl_gene_id,
          uniprot_accession = uniprot_accession
        )
      )
      TRUE
    }),
    error = function(e) e
  )
  if (inherits(tx, "error")) {
    if (identical(conditionMessage(tx), "confirm_project_target_no_row")) {
      return(list(ok = FALSE, message = "The target could not be confirmed."))
    }
    return(list(
      ok = FALSE,
      message = unique_violation_message(tx, "The target could not be confirmed.")
    ))
  }

  list(ok = TRUE)
}

update_target_input_text <- function(db_pool, target_id, user_id, input_text) {
  input_text <- trimws(input_text)

  if (!nzchar(input_text)) {
    return(list(ok = FALSE, message = "Target text cannot be empty."))
  }
  len <- enforce_length(input_text, INPUT_LIMITS$target_input, "Candidate target")
  if (!isTRUE(len$ok)) {
    return(len)
  }

  updated <- db_execute_guarded(
    db_pool,
    "
    UPDATE project_targets pt
    SET input_text = $3,
        resolution_status = 'unresolved',
        resolution_payload = NULL,
        display_symbol = NULL,
        ensembl_gene_id = NULL,
        uniprot_accession = NULL,
        hgnc_id = NULL,
        confirmed_at = NULL
    FROM projects p
    WHERE pt.project_id = p.id
      AND pt.id = $1::uuid
      AND p.user_id = $2::uuid
      AND pt.removed_at IS NULL
      AND pt.resolution_status <> 'confirmed'
    ",
    list(target_id, user_id, input_text)
  )

  if (inherits(updated, "error")) {
    return(list(
      ok = FALSE,
      message = unique_violation_message(
        updated,
        "The target string could not be updated."
      )
    ))
  }

  if (updated != 1) {
    return(list(ok = FALSE, message = "The target string could not be updated."))
  }

  list(ok = TRUE)
}

add_project_target <- function(db_pool, project_id, user_id, input_text) {
  project <- get_owned_project(db_pool, project_id, user_id)
  if (is.null(project)) {
    return(list(ok = FALSE, message = "Project was not found or you do not have access to it."))
  }
  input_text <- trimws(as.character(input_text %||% ""))
  if (!nzchar(input_text)) {
    return(list(ok = FALSE, message = "Target text cannot be empty."))
  }
  len <- enforce_length(input_text, INPUT_LIMITS$target_input, "Candidate target")
  if (!isTRUE(len$ok)) {
    return(len)
  }
  existing <- list_project_targets(db_pool, project_id, user_id)
  key <- normalize_target_input_key(input_text)
  if (any(normalize_target_input_key(existing$input_text) == key)) {
    return(list(ok = FALSE, message = "This project already has that target string."))
  }
  if (nrow(existing) >= get_app_config()$max_targets) {
    return(list(
      ok = FALSE,
      message = sprintf(
        "A project may contain at most %d candidate targets.",
        get_app_config()$max_targets
      )
    ))
  }

  removed <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT pt.id::text AS id
    FROM project_targets pt
    INNER JOIN projects p ON p.id = pt.project_id
    WHERE pt.project_id = $1::uuid
      AND p.user_id = $2::uuid
      AND pt.removed_at IS NOT NULL
      AND lower(btrim(pt.input_text)) = $3
    ORDER BY pt.removed_at DESC
    LIMIT 1
    ",
    params = list(project_id, user_id, key)
  )

  if (nrow(removed) == 1) {
    tx <- tryCatch(
      pool::poolWithTransaction(db_pool, function(con) {
        revived <- DBI::dbExecute(
          con,
          "
          UPDATE project_targets pt
          SET removed_at = NULL,
              removed_by = NULL,
              input_text = $4,
              resolution_status = 'unresolved',
              resolution_payload = NULL,
              display_symbol = NULL,
              ensembl_gene_id = NULL,
              uniprot_accession = NULL,
              hgnc_id = NULL,
              confirmed_at = NULL
          FROM projects p
          WHERE pt.project_id = p.id
            AND pt.id = $1::uuid
            AND p.id = $2::uuid
            AND p.user_id = $3::uuid
            AND pt.removed_at IS NOT NULL
          ",
          params = list(removed$id[[1]], project_id, user_id, input_text)
        )
        if (revived != 1) {
          stop("add_project_target_failed", call. = FALSE)
        }
        insert_project_event(
          con,
          project_id,
          "TARGET_ADDED",
          user_id,
          project_target_id = removed$id[[1]],
          metadata = list(label = input_text, input_text = input_text, revived = TRUE)
        )
        TRUE
      }),
      error = function(e) e
    )
    if (inherits(tx, "error")) {
      return(list(ok = FALSE, message = "The target could not be added."))
    }
    return(list(ok = TRUE, target_id = removed$id[[1]], revived = TRUE, invalidation = live_invalidation_event("target")))
  }

  new_id <- uuid::UUIDgenerate()
  tx <- tryCatch(
    pool::poolWithTransaction(db_pool, function(con) {
      inserted <- DBI::dbExecute(
        con,
        "
        INSERT INTO project_targets (id, project_id, input_text, resolution_status)
        SELECT $1::uuid, p.id, $3, 'unresolved'
        FROM projects p
        WHERE p.id = $2::uuid
          AND p.user_id = $4::uuid
        ",
        params = list(new_id, project_id, input_text, user_id)
      )
      if (inserted != 1) {
        stop("add_project_target_failed", call. = FALSE)
      }
      insert_project_event(
        con,
        project_id,
        "TARGET_ADDED",
        user_id,
        project_target_id = new_id,
        metadata = list(label = input_text, input_text = input_text)
      )
      TRUE
    }),
    error = function(e) e
  )
  if (inherits(tx, "error")) {
    return(list(
      ok = FALSE,
      message = unique_violation_message(tx, "The target could not be added.")
    ))
  }
  list(ok = TRUE, target_id = new_id, revived = FALSE, invalidation = live_invalidation_event("target"))
}

remove_project_target <- function(
  db_pool,
  project_id,
  user_id,
  target_id,
  confirm_note_deletion = FALSE,
  confirm_removal = NULL
) {
  confirm_ok <- isTRUE(confirm_removal) || isTRUE(confirm_note_deletion)
  row <- get_owned_target(db_pool, target_id, user_id)
  if (is.null(row) || !identical(as.character(row$project_id[[1]]), as.character(project_id))) {
    return(list(ok = FALSE, message = "Target was not found in this project."))
  }
  existing <- list_project_targets(db_pool, project_id, user_id)
  if (nrow(existing) <= get_app_config()$min_targets) {
    return(list(ok = FALSE, message = "A project must keep at least one candidate target."))
  }
  label <- if (identical(as.character(row$resolution_status[[1]]), "confirmed") &&
    has_display_text(row$display_symbol[[1]])) {
    row$display_symbol[[1]]
  } else {
    row$input_text[[1]]
  }
  notes <- list_research_notes(
    db_pool,
    project_id,
    user_id,
    scope = "target",
    project_target_id = target_id
  )
  if (!confirm_ok) {
    extra <- if (nrow(notes) > 0) {
      sprintf(
        " %s live target note%s will stay attached and return if this target is added again.",
        nrow(notes),
        if (nrow(notes) == 1L) "" else "s"
      )
    } else {
      ""
    }
    return(list(
      ok = FALSE,
      needs_confirmation = TRUE,
      note_count = nrow(notes),
      message = paste0(
        "This removes ",
        label,
        " from the live project. Snapshots and history are not changed.",
        extra
      )
    ))
  }

  was_confirmed <- identical(as.character(row$resolution_status[[1]]), "confirmed")
  tx <- tryCatch(
    pool::poolWithTransaction(db_pool, function(con) {
      removed <- DBI::dbExecute(
        con,
        "
        UPDATE project_targets pt
        SET removed_at = NOW(),
            removed_by = $3::uuid
        FROM projects p
        WHERE pt.project_id = p.id
          AND pt.id = $1::uuid
          AND p.id = $2::uuid
          AND p.user_id = $3::uuid
          AND pt.removed_at IS NULL
        ",
        params = list(target_id, project_id, user_id)
      )
      if (removed != 1) {
        stop("remove_project_target_failed", call. = FALSE)
      }
      if (isTRUE(was_confirmed)) {
        bumped <- DBI::dbExecute(
          con,
          "
          UPDATE projects
          SET target_set_revision = target_set_revision + 1,
              updated_at = NOW()
          WHERE id = $1::uuid
          ",
          params = list(project_id)
        )
        if (bumped != 1) {
          stop("remove_project_target_failed", call. = FALSE)
        }
      }
      insert_project_event(
        con,
        project_id,
        "TARGET_REMOVED",
        user_id,
        project_target_id = target_id,
        metadata = list(label = label)
      )
      TRUE
    }),
    error = function(e) e
  )
  if (inherits(tx, "error")) {
    return(list(ok = FALSE, message = "The target could not be removed."))
  }
  list(ok = TRUE, invalidation = live_invalidation_event("target"))
}

reset_confirmed_target <- function(db_pool, target_id, user_id) {
  updated <- DBI::dbExecute(
    db_pool,
    "
    UPDATE project_targets pt
    SET resolution_status = 'unresolved',
        resolution_payload = NULL,
        display_symbol = NULL,
        ensembl_gene_id = NULL,
        uniprot_accession = NULL,
        hgnc_id = NULL,
        confirmed_at = NULL
    FROM projects p
    WHERE pt.project_id = p.id
      AND pt.id = $1::uuid
      AND p.user_id = $2::uuid
      AND pt.removed_at IS NULL
      AND pt.resolution_status = 'confirmed'
    ",
    params = list(target_id, user_id)
  )
  if (updated != 1) {
    return(list(ok = FALSE, message = "The confirmed identity could not be reset."))
  }
  row <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT project_id::text AS project_id
    FROM project_targets
    WHERE id = $1::uuid
    ",
    params = list(target_id)
  )
  if (nrow(row) == 1) {
    bump_project_target_set_revision(db_pool, row$project_id[[1]])
  }
  list(ok = TRUE, invalidation = live_invalidation_event("target"))
}
