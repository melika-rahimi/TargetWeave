SNAPSHOT_SECTION_KEYS <- c(
  "overview",
  "disease_evidence",
  "comparison",
  "pathways",
  "literature",
  "structures"
)

empty_workspace_evidence <- function() {
  list(
    overviews = list(),
    disease_evidence = list(),
    comparison = NULL,
    pathways = NULL,
    literature = NULL,
    structures = NULL
  )
}

default_snapshot_name <- function(when = Sys.time()) {
  sprintf("Snapshot \u00b7 %s", format_user_timestamp(when))
}

is_auto_snapshot_name <- function(name) {
  text <- trimws(as.character(name %||% ""))
  grepl("^Snapshot \u00b7 [0-9]{1,2} [A-Za-z]{3} [0-9]{4}, [0-9]{2}:[0-9]{2} .+$", text)
}

align_snapshot_name <- function(name, created_at) {
  if (has_display_text(name) && !is_auto_snapshot_name(name)) {
    return(trimws(as.character(name)))
  }
  default_snapshot_name(created_at)
}

NOT_RETRIEVED_BEFORE_CAPTURE <- "Not retrieved before snapshot capture."

is_stale_cache_status <- function(status) {
  grepl("stale", as.character(status %||% ""), ignore.case = TRUE)
}

collect_cache_statuses <- function(value, acc = character()) {
  if (is.null(value)) {
    return(acc)
  }
  if (is.data.frame(value)) {
    if ("cache_status" %in% names(value)) {
      acc <- c(acc, as.character(value$cache_status))
    }
    return(acc)
  }
  if (!is.list(value)) {
    return(acc)
  }
  if (!is.null(value$cache_status)) {
    acc <- c(acc, as.character(value$cache_status))
  }
  for (item in value) {
    acc <- collect_cache_statuses(item, acc)
  }
  acc
}

section_is_stale <- function(model) {
  any(vapply(collect_cache_statuses(model), is_stale_cache_status, logical(1)))
}

section_envelope <- function(
  capture_status,
  model = NULL,
  message = NULL,
  stale_at_capture = FALSE
) {
  list(
    capture_status = capture_status,
    stale_at_capture = isTRUE(stale_at_capture),
    message = message,
    model = model
  )
}

classify_retrieve_result <- function(result, payload_name) {
  if (is.null(result)) {
    return(section_envelope("not_retrieved", message = NOT_RETRIEVED_BEFORE_CAPTURE))
  }
  status <- as.character(result$status %||% "")
  if (grepl("^blocked", status)) {
    return(section_envelope("unavailable", message = result$message %||% "Unavailable"))
  }
  if (identical(status, "error") || identical(status, "unavailable")) {
    return(section_envelope("unavailable", message = result$message %||% "Unavailable"))
  }
  model <- result[[payload_name]]
  if (is.null(model) && identical(payload_name, "overview")) {
    model <- result$overview
  }
  stale <- section_is_stale(result)
  failures <- result[[payload_name]]$failures %||% result$failures %||% list()
  if (identical(payload_name, "structures")) {
    failures <- c(failures, result$structures$unavailable_entities %||% list())
  }
  if (identical(payload_name, "literature")) {
    failures <- c(failures, result$literature$failures %||% list())
  }
  if (identical(payload_name, "pathways")) {
    failures <- c(failures, result$pathways$failures %||% list())
  }
  capture_status <- if (length(failures) > 0) {
    "partial"
  } else if (stale) {
    "stale_at_capture"
  } else {
    "captured"
  }
  section_envelope(
    capture_status,
    model = slim_snapshot_model(payload_name, model),
    message = result$message,
    stale_at_capture = stale
  )
}

slim_snapshot_model <- function(kind, model) {
  if (is.null(model)) {
    return(NULL)
  }
  if (identical(kind, "overview") && is.list(model$protein)) {
    model$protein$function_full <- NULL
  }
  if (identical(kind, "structures") && is.list(model$targets)) {
    model$targets <- lapply(model$targets, function(item) {
      if (!is.null(item$records)) {
        item$records <- lapply(item$records, function(rec) {
          rec$viewer_url <- NULL
          rec
        })
      }
      item
    })
  }
  model
}

classify_target_map <- function(store, owned_ids) {
  items <- list()
  stale_any <- FALSE
  captured_n <- 0L
  for (tid in owned_ids) {
    result <- store[[tid]]
    if (is.null(result)) {
      items[[tid]] <- section_envelope("not_retrieved", message = NOT_RETRIEVED_BEFORE_CAPTURE)
      next
    }
    env <- if (!is.null(result$evidence)) {
      classify_retrieve_result(result, "evidence")
    } else {
      classify_retrieve_result(result, "overview")
    }
    items[[tid]] <- env
    if (identical(env$capture_status, "captured") || identical(env$capture_status, "stale_at_capture") || identical(env$capture_status, "partial")) {
      captured_n <- captured_n + 1L
    }
    stale_any <- stale_any || isTRUE(env$stale_at_capture)
  }
  overall <- if (length(owned_ids) == 0L) {
    "not_retrieved"
  } else if (captured_n == 0L && all(vapply(items, function(x) identical(x$capture_status, "not_retrieved"), logical(1)))) {
    "not_retrieved"
  } else if (captured_n < length(owned_ids)) {
    "partial"
  } else if (stale_any) {
    "stale_at_capture"
  } else {
    "captured"
  }
  list(
    capture_status = overall,
    stale_at_capture = stale_any,
    message = NULL,
    targets = items
  )
}

freeze_project_context <- function(project_row) {
  list(
    capture_status = "captured",
    stale_at_capture = FALSE,
    model = list(
      project_id = as.character(project_row$id[[1]]),
      title = as.character(project_row$title[[1]]),
      research_question = as.character(project_row$research_question[[1]]),
      organism = as.character(project_row$organism[[1]]),
      disease_label = as.character(project_row$disease_label[[1]]),
      disease_name = blank_to_null(project_row$disease_name[[1]]),
      disease_ontology_id = blank_to_null(project_row$disease_ontology_id[[1]]),
      disease_resolution_status = as.character(project_row$disease_resolution_status[[1]]),
      disease_confirmed = isTRUE(project_disease_is_confirmed(project_row))
    )
  )
}

freeze_target_identity <- function(target_rows) {
  rows <- lapply(seq_len(nrow(target_rows)), function(i) {
    row <- target_rows[i, , drop = FALSE]
    list(
      project_target_id = as.character(row$id[[1]]),
      input_text = as.character(row$input_text[[1]]),
      display_symbol = blank_to_null(row$display_symbol[[1]]),
      resolution_status = as.character(row$resolution_status[[1]]),
      ensembl_gene_id = blank_to_null(row$ensembl_gene_id[[1]]),
      uniprot_accession = blank_to_null(row$uniprot_accession[[1]]),
      hgnc_id = blank_to_null(row$hgnc_id[[1]]),
      confirmed = target_is_confirmed(row)
    )
  })
  list(
    capture_status = "captured",
    stale_at_capture = FALSE,
    model = list(
      n_targets = nrow(target_rows),
      n_confirmed = sum(target_rows$resolution_status == "confirmed"),
      targets = rows
    )
  )
}

build_source_manifest <- function(sections) {
  entries <- list()
  add <- function(...) {
    entries[[length(entries) + 1L]] <<- list(...)
  }
  overviews <- sections$overview$targets %||% list()
  for (tid in names(overviews)) {
    model <- overviews[[tid]]$model
    if (is.null(model)) {
      next
    }
    uniprot <- model$provenance$uniprot
    ensembl <- model$provenance$ensembl
    if (!is.null(uniprot)) {
      add(
        source = "UniProt",
        record_id = uniprot$record_id %||% model$identity$uniprot_accession,
        retrieved_at = uniprot$retrieved_at,
        cache_status = uniprot$cache_status,
        version = NA_character_,
        scope = "overview"
      )
    }
    if (!is.null(ensembl)) {
      add(
        source = "Ensembl",
        record_id = ensembl$record_id %||% model$identity$ensembl_gene_id,
        retrieved_at = ensembl$retrieved_at,
        cache_status = ensembl$cache_status,
        version = NA_character_,
        scope = "overview"
      )
    }
  }
  disease <- sections$disease_evidence$targets %||% list()
  for (tid in names(disease)) {
    model <- disease[[tid]]$model
    prov <- model$provenance
    if (is.null(prov)) {
      next
    }
    add(
      source = "Open Targets",
      record_id = paste(prov$target_id, prov$disease_id, sep = " / "),
      retrieved_at = prov$retrieved_at,
      cache_status = prov$cache_status,
      data_version = prov$data_version,
      api_version = prov$api_version,
      scope = "disease_evidence"
    )
  }
  comparison <- sections$comparison$model
  if (!is.null(comparison$provenance)) {
    prov <- comparison$provenance
    add(
      source = "Open Targets",
      record_id = prov$disease_id,
      retrieved_at = NA_character_,
      cache_status = NA_character_,
      data_version = prov$data_version,
      api_version = prov$api_version,
      scope = "comparison"
    )
  }
  pathways <- sections$pathways$model
  if (!is.null(pathways$provenance)) {
    prov <- pathways$provenance
    add(
      source = "Reactome",
      record_id = NA_character_,
      retrieved_at = prov$retrieved_at,
      cache_status = prov$cache_status,
      version = prov$reactome_release,
      membership_scope = prov$membership_scope,
      scope = "pathways"
    )
  }
  literature <- sections$literature$model
  if (!is.null(literature)) {
    targets <- literature$targets %||% list()
    for (item in targets) {
      prov <- item$provenance
      if (is.null(prov)) {
        next
      }
      add(
        source = "NCBI PubMed",
        record_id = prov$ncbi_gene_id,
        retrieved_at = prov$retrieved_at,
        cache_status = prov$cache_status,
        database_last_update = prov$database_last_update,
        disease_query = item$corpus$disease_query %||% literature$disease$disease_query,
        scope = "literature"
      )
    }
  }
  structures <- sections$structures$model
  if (!is.null(structures$targets)) {
    for (item in structures$targets) {
      prov <- item$provenance
      if (is.null(prov)) {
        next
      }
      add(
        source = "RCSB PDB",
        record_id = prov$identifier_used,
        retrieved_at = prov$retrieved_at,
        cache_status = prov$cache_status,
        query_scope = prov$query_scope %||% "experimental",
        version = NA_character_,
        scope = "structures"
      )
    }
  }
  entries
}

snapshot_preview_lines <- function(sections, target_identity) {
  captured <- character()
  missing <- character()
  n_confirmed <- target_identity$model$n_confirmed %||% 0L
  captured <- c(captured, "Project context")
  captured <- c(captured, sprintf("%s confirmed target%s", n_confirmed, if (identical(as.integer(n_confirmed), 1L)) "" else "s"))
  label_for <- function(key, captured_label, missing_label) {
    status <- sections[[key]]$capture_status
    if (status %in% c("captured", "partial", "stale_at_capture")) {
      captured <<- c(captured, captured_label)
    } else if (identical(status, "unavailable")) {
      missing <<- c(missing, paste0(missing_label, " — unavailable"))
    } else {
      missing <<- c(missing, paste0(missing_label, " — not retrieved"))
    }
  }
  n_ov <- sum(vapply(sections$overview$targets %||% list(), function(x) x$capture_status %in% c("captured", "partial", "stale_at_capture"), logical(1)))
  if (n_ov > 0) {
    captured <- c(captured, sprintf("Overview for %s target%s", n_ov, if (identical(n_ov, 1L)) "" else "s"))
  } else {
    missing <- c(missing, "Target overview — not retrieved")
  }
  n_ev <- sum(vapply(sections$disease_evidence$targets %||% list(), function(x) x$capture_status %in% c("captured", "partial", "stale_at_capture"), logical(1)))
  if (n_ev > 0) {
    captured <- c(captured, sprintf("Open Targets disease evidence for %s target%s", n_ev, if (identical(n_ev, 1L)) "" else "s"))
  } else {
    missing <- c(missing, "Open Targets disease evidence — not retrieved")
  }
  label_for("comparison", "Open Targets comparison", "Open Targets comparison")
  label_for("pathways", "Reactome pathways", "Reactome pathways")
  label_for("literature", "Literature", "Literature")
  label_for("structures", "Structures", "Structures")
  list(will_capture = unique(captured), not_captured = unique(missing))
}

build_snapshot_payload <- function(project_row, target_rows, workspace = empty_workspace_evidence()) {
  owned_ids <- as.character(target_rows$id)
  sections <- list(
    overview = classify_target_map(workspace$overviews %||% list(), owned_ids),
    disease_evidence = classify_target_map(workspace$disease_evidence %||% list(), owned_ids),
    comparison = classify_retrieve_result(workspace$comparison, "comparison"),
    pathways = classify_retrieve_result(workspace$pathways, "pathways"),
    literature = classify_retrieve_result(workspace$literature, "literature"),
    structures = classify_retrieve_result(workspace$structures, "structures")
  )
  project_context <- freeze_project_context(project_row)
  target_identity <- freeze_target_identity(target_rows)
  stale <- isTRUE(sections$overview$stale_at_capture) ||
    isTRUE(sections$disease_evidence$stale_at_capture) ||
    isTRUE(sections$comparison$stale_at_capture) ||
    isTRUE(sections$pathways$stale_at_capture) ||
    isTRUE(sections$literature$stale_at_capture) ||
    isTRUE(sections$structures$stale_at_capture)
  preview <- snapshot_preview_lines(sections, target_identity)
  captured_source_count <- length(build_source_manifest(sections))
  summary <- list(
    stale_at_capture = stale,
    captured_source_count = captured_source_count,
    captured_target_count = as.integer(target_identity$model$n_confirmed),
    section_status = lapply(sections, `[[`, "capture_status"),
    will_capture = preview$will_capture,
    not_captured = preview$not_captured
  )
  list(
    schema_version = SNAPSHOT_SCHEMA_VERSION,
    project_context = project_context,
    target_identity = target_identity,
    overview = sections$overview,
    disease_evidence = sections$disease_evidence,
    comparison = sections$comparison,
    pathways = sections$pathways,
    literature = sections$literature,
    structures = sections$structures,
    source_manifest = build_source_manifest(sections),
    capture_summary = summary
  )
}

create_evidence_snapshot <- function(
  db_pool,
  user_id,
  project_id,
  workspace = empty_workspace_evidence(),
  name = NULL,
  description = NULL
) {
  project <- get_owned_project(db_pool, project_id, user_id)
  if (is.null(project)) {
    return(list(ok = FALSE, error = "Project was not found or you do not have access to it."))
  }
  targets <- list_project_targets(db_pool, project_id, user_id)
  payload <- build_snapshot_payload(project, targets, workspace)
  payload$project_id <- as.character(project$id[[1]])
  payload$user_id <- as.character(project$user_id[[1]])
  payload$name <- if (has_display_text(name)) trimws(as.character(name)) else default_snapshot_name()
  payload$description <- blank_to_null(description)
  inserted <- insert_evidence_snapshot_row(db_pool, payload)
  if (!isTRUE(inserted$ok)) {
    if (exists("tw_log", mode = "function", inherits = TRUE)) {
      tw_log("snapshot_create_failed", level = "error")
    }
    return(inserted)
  }
  snap <- get_owned_snapshot(db_pool, inserted$snapshot_id, user_id)
  aligned <- align_snapshot_name(payload$name, snap$created_at)
  if (!identical(as.character(snap$name %||% ""), aligned)) {
    rename_evidence_snapshot(db_pool, inserted$snapshot_id, user_id, aligned)
    snap$name <- aligned
  }
  if (exists("tw_log", mode = "function", inherits = TRUE)) {
    tw_log("snapshot_created")
  }
  list(
    ok = TRUE,
    snapshot_id = inserted$snapshot_id,
    snapshot = snap
  )
}

freshness_label <- function(status) {
  text <- tolower(as.character(status %||% ""))
  if (grepl("snapshot", text)) {
    return("Snapshot")
  }
  if (grepl("stale", text)) {
    return("Stale")
  }
  if (grepl("live", text)) {
    return("Live")
  }
  if (grepl("cached|fresh", text)) {
    return("Cached")
  }
  if (!nzchar(text) || identical(text, "na") || identical(text, "null")) {
    return("Not provided")
  }
  as.character(status)
}

capture_status_label <- function(status) {
  switch(
    as.character(status %||% ""),
    captured = "Captured",
    not_retrieved = NOT_RETRIEVED_BEFORE_CAPTURE,
    unavailable = "Unavailable",
    partial = "Partial",
    stale_at_capture = "Stale at capture",
    as.character(status %||% "Unknown")
  )
}

section_status_display <- function(section) {
  if (!is.list(section)) {
    return(NOT_RETRIEVED_BEFORE_CAPTURE)
  }
  status <- as.character(section$capture_status %||% "")
  stale <- isTRUE(section$stale_at_capture) || identical(status, "stale_at_capture")
  msg <- trimws(as.character(section$message %||% ""))
  generic_missing <- !nzchar(msg) || grepl("not captured|not retrieved", msg, ignore.case = TRUE)
  if (identical(status, "not_retrieved") || !nzchar(status)) {
    return(NOT_RETRIEVED_BEFORE_CAPTURE)
  }
  if (identical(status, "unavailable")) {
    if (!generic_missing) {
      return(msg)
    }
    return("Unavailable")
  }
  if (identical(status, "partial")) {
    base <- if (!generic_missing) msg else "Partial"
    if (stale) {
      return(paste(base, "\u00b7 Stale at capture"))
    }
    return(base)
  }
  if (identical(status, "captured") || identical(status, "stale_at_capture")) {
    base <- "Captured"
    if (stale) {
      return(paste(base, "\u00b7 Stale at capture"))
    }
    return(base)
  }
  if (nzchar(msg)) {
    return(msg)
  }
  capture_status_label(status)
}

json_rows_to_df <- function(rows) {
  if (is.null(rows) || length(rows) == 0) {
    return(NULL)
  }
  if (is.data.frame(rows)) {
    return(rows)
  }
  if (!is.list(rows)) {
    return(NULL)
  }
  tryCatch(
    {
      if (!is.null(names(rows)) && !is.null(rows[[1]]) && !is.list(rows[[1]])) {
        return(as.data.frame(rows, stringsAsFactors = FALSE))
      }
      do.call(rbind, lapply(rows, function(row) {
        if (is.data.frame(row)) {
          return(row)
        }
        as.data.frame(row, stringsAsFactors = FALSE)
      }))
    },
    error = function(e) NULL
  )
}

load_snapshot_for_view <- function(db_pool, snapshot_id, user_id) {
  snap <- get_owned_snapshot(db_pool, snapshot_id, user_id)
  if (is.null(snap)) {
    return(NULL)
  }
  tryCatch(
    hydrate_snapshot_for_view(snap),
    error = function(e) {
      snap$view_message <- "This snapshot could not be displayed."
      snap
    }
  )
}

hydrate_snapshot_for_view <- function(snapshot) {
  if (is.null(snapshot)) {
    return(NULL)
  }
  cmp <- snapshot$comparison$model
  if (is.list(cmp)) {
    if (!is.null(cmp$targets)) cmp$targets <- json_rows_to_df(cmp$targets)
    if (!is.null(cmp$datatype_matrix)) cmp$datatype_matrix <- json_rows_to_df(cmp$datatype_matrix)
    snapshot$comparison$model <- cmp
  }
  paths <- snapshot$pathways$model
  if (is.list(paths)) {
    if (!is.null(paths$targets)) paths$targets <- json_rows_to_df(paths$targets)
    if (!is.null(paths$pathways)) paths$pathways <- json_rows_to_df(paths$pathways)
    if (!is.null(paths$membership_matrix)) paths$membership_matrix <- json_rows_to_df(paths$membership_matrix)
    snapshot$pathways$model <- paths
  }
  lit <- snapshot$literature$model
  if (is.list(lit)) {
    if (!is.null(lit$target_counts)) lit$target_counts <- json_rows_to_df(lit$target_counts)
    if (is.list(lit$targets) && !is.data.frame(lit$targets)) {
      lit$targets <- lapply(lit$targets, function(item) {
        if (!is.list(item)) {
          return(item)
        }
        if (!is.null(item$trend)) item$trend <- json_rows_to_df(item$trend)
        if (!is.null(item$recent_records)) item$recent_records <- json_rows_to_df(item$recent_records)
        item
      })
    }
    snapshot$literature$model <- lit
  }
  st <- snapshot$structures$model
  if (is.list(st)) {
    if (!is.null(st$summary)) st$summary <- json_rows_to_df(st$summary)
    if (!is.null(st$targets)) {
      st$targets <- lapply(st$targets, function(item) {
        if (!is.null(item$records)) {
          item$records <- lapply(item$records, function(rec) {
            if (!is.null(rec$coverage$covered_ranges)) {
              rec$coverage$covered_ranges <- json_rows_to_df(rec$coverage$covered_ranges)
            }
            if (!is.null(rec$ligands)) rec$ligands <- json_rows_to_df(rec$ligands)
            rec
          })
        }
        item
      })
    }
    snapshot$structures$model <- st
  }
  snapshot
}
