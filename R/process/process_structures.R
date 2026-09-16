structure_gate <- function(target_rows) {
  confirmed <- target_rows
  if (!is.null(confirmed) && nrow(confirmed) > 0) {
    confirmed <- confirmed[confirmed$resolution_status == "confirmed", , drop = FALSE]
  }
  n <- if (is.null(confirmed)) 0L else nrow(confirmed)
  if (n < 1L) {
    return(list(
      ok = FALSE,
      status = "blocked_targets",
      message = "Confirm at least one target identity before retrieving experimental PDB structures."
    ))
  }
  list(ok = TRUE, status = "ok", message = NULL)
}

structure_signature <- function(target_rows) {
  confirmed_scientific_target_set(target_rows)
}

should_retrieve_structures <- function(
  panel_active,
  target_rows,
  last_signature = NA_character_,
  force = FALSE
) {
  if (!isTRUE(panel_active)) {
    return(FALSE)
  }
  if (isTRUE(force)) {
    return(TRUE)
  }
  gate <- structure_gate(target_rows)
  if (!isTRUE(gate$ok)) {
    return(FALSE)
  }
  !identical(as.character(last_signature %||% NA_character_), structure_signature(target_rows))
}

sort_structure_records <- function(records) {
  if (length(records) == 0) {
    return(records)
  }
  keys <- do.call(rbind, lapply(records, function(item) {
    data.frame(
      coverage = as.numeric(item$coverage$coverage_fraction %||% -1),
      release = as.character(item$entry$release_date %||% ""),
      id = item$polymer_entity$entity_identifier,
      stringsAsFactors = FALSE
    )
  }))
  ord <- order(keys$coverage, keys$release, decreasing = c(TRUE, TRUE), method = "radix")
  records[ord]
}

empty_structure_summary <- function() {
  data.frame(
    project_target_id = character(),
    symbol = character(),
    uniprot_accession = character(),
    n_pdb_entries = integer(),
    n_polymer_entities = integer(),
    max_coverage_fraction = numeric(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

metadata_for_entities <- function(entity_ids, db_pool = NULL, perform = httr2::req_perform) {
  ids <- unique(as.character(entity_ids))
  if (length(ids) == 0) {
    return(list(records = list(), failures = character()))
  }
  starts <- seq(1L, length(ids), by = RCSB_METADATA_BATCH)
  state <- list(records = list(), failures = character())
  for (start in starts) {
    local_start <- start
    state <- tw_then(state, function(state) {
      batch <- ids[local_start:min(local_start + RCSB_METADATA_BATCH - 1L, length(ids))]
      tw_then(fetch_rcsb_entity_metadata(batch, db_pool = db_pool, perform = perform), function(result) {
        if (!isTRUE(result$ok)) {
          state$failures <- c(state$failures, batch)
          return(state)
        }
        parsed <- parse_rcsb_entity_metadata(result$data)
        if (!isTRUE(parsed$ok)) {
          state$failures <- c(state$failures, batch)
          return(state)
        }
        for (name in names(parsed$records)) {
          state$records[[name]] <- parsed$records[[name]]
        }
        state$failures <- c(state$failures, setdiff(batch, names(parsed$records)))
        state
      })
    })
  }
  tw_then(state, function(state) {
    list(records = state$records, failures = unique(state$failures))
  })
}

retrieve_target_structures <- function(
  target_row,
  db_pool = NULL,
  perform = httr2::req_perform
) {
  symbol <- target_workspace_label(target_row)
  accession <- blank_to_null(target_row$uniprot_accession[[1]])
  if (is.null(accession)) {
    return(list(
      status = "unavailable",
      message = "Structure mapping unavailable.",
      structures = NULL
    ))
  }

  tw_then(fetch_rcsb_uniprot_search(accession, db_pool = db_pool, perform = perform), function(search) {
  if (!isTRUE(search$ok)) {
    return(list(
      status = "error",
      message = sprintf("Structure retrieval unavailable for %s.", symbol),
      error = search$error,
      cache_status = search$cache_status
    ))
  }

  parsed_search <- parse_rcsb_search_entities(search$data)
  if (!isTRUE(parsed_search$ok)) {
    return(list(
      status = "error",
      message = sprintf("Structure retrieval unavailable for %s.", symbol),
      error = parsed_search$error
    ))
  }

  if (length(parsed_search$entities) == 0) {
    model <- list(
      target = list(
        project_target_id = as.character(target_row$id[[1]]),
        symbol = symbol,
        uniprot_accession = accession
      ),
      records = list(),
      n_pdb_entries = 0L,
      n_polymer_entities = 0L,
      max_coverage_fraction = NA_real_,
      uniprot_length = NA_integer_,
      search_was_empty = TRUE,
      unavailable_entities = character(),
      rejected_computed = parsed_search$rejected,
      provenance = list(
        source = "RCSB Protein Data Bank",
        identifier_used = accession,
        query_scope = RCSB_EXPERIMENTAL_SCOPE,
        retrieved_at = search$retrieved_at,
        cache_status = cache_status_label(search$cache_status),
        search_api = RCSB_SEARCH_URL,
        data_api = RCSB_DATA_GRAPHQL,
        sequence_coordinates_api = RCSB_SEQCOORDS_GRAPHQL
      )
    )
    return(list(status = "empty", message = NULL, structures = model))
  }

  allowed <- vapply(parsed_search$entities, `[[`, character(1), "identifier")
  tw_then(fetch_rcsb_uniprot_alignments(accession, db_pool = db_pool, perform = perform), function(alignments_result) {
  alignments <- list()
  uniprot_length <- NA_integer_
  if (isTRUE(alignments_result$ok)) {
    parsed_al <- parse_rcsb_alignments(alignments_result$data)
    if (isTRUE(parsed_al$ok)) {
      uniprot_length <- parsed_al$uniprot_length
      keep <- intersect(names(parsed_al$alignments), allowed)
      alignments <- parsed_al$alignments[keep]
    }
  }

  tw_then(metadata_for_entities(allowed, db_pool = db_pool, perform = perform), function(meta) {
  records <- list()
  for (ent in parsed_search$entities) {
    id <- ent$identifier
    base <- meta$records[[id]]
    if (is.null(base)) {
      next
    }
    coverage <- if (!is.null(alignments[[id]])) {
      normalize_entity_coverage(alignments[[id]], uniprot_length)
    } else {
      list(
        uniprot_length = uniprot_length,
        covered_ranges = data.frame(begin = integer(), end = integer(), stringsAsFactors = FALSE),
        covered_residue_count = NA_integer_,
        coverage_fraction = NA_real_,
        coverage_label = "Not provided"
      )
    }
    records[[length(records) + 1L]] <- list(
      pdb_id = base$pdb_id,
      target = list(
        project_target_id = as.character(target_row$id[[1]]),
        symbol = symbol,
        uniprot_accession = accession
      ),
      polymer_entity = base$polymer_entity,
      experiment = base$experiment,
      entry = base$entry,
      coverage = coverage,
      ligands = base$ligands,
      engineered = isTRUE(base$engineered),
      mutation_label = base$mutation_label,
      rcsb_url = rcsb_structure_page_url(base$pdb_id),
      viewer_url = rcsb_viewer_embed_url(base$pdb_id),
      provenance = list(
        source = "RCSB Protein Data Bank",
        pdb_id = base$pdb_id,
        polymer_entity = id,
        experimental_method = base$experiment$method
      )
    )
  }
  records <- sort_structure_records(records)
  pdb_ids <- unique(vapply(records, `[[`, character(1), "pdb_id"))
  fractions <- vapply(records, function(item) as.numeric(item$coverage$coverage_fraction %||% NA_real_), numeric(1))
  model <- list(
    target = list(
      project_target_id = as.character(target_row$id[[1]]),
      symbol = symbol,
      uniprot_accession = accession
    ),
    records = records,
    n_pdb_entries = length(pdb_ids),
    n_polymer_entities = length(records),
    max_coverage_fraction = if (all(is.na(fractions))) NA_real_ else max(fractions, na.rm = TRUE),
    uniprot_length = uniprot_length,
    search_was_empty = FALSE,
    unavailable_entities = meta$failures,
    rejected_computed = parsed_search$rejected,
    provenance = list(
      source = "RCSB Protein Data Bank",
      identifier_used = accession,
      query_scope = RCSB_EXPERIMENTAL_SCOPE,
      retrieved_at = search$retrieved_at,
      cache_status = cache_status_label(search$cache_status),
      search_api = RCSB_SEARCH_URL,
      data_api = RCSB_DATA_GRAPHQL,
      sequence_coordinates_api = RCSB_SEQCOORDS_GRAPHQL,
      sort_rule = "sequence coverage descending, then release date descending"
    )
  )
  list(status = "ok", message = NULL, structures = model)
  })
  })
  })
}

retrieve_project_structures <- function(
  project_row,
  target_rows,
  db_pool = NULL,
  perform = httr2::req_perform,
  on_progress = NULL
) {
  gate <- structure_gate(target_rows)
  if (!isTRUE(gate$ok)) {
    return(list(status = gate$status, message = gate$message, structures = NULL))
  }

  excluded <- list()
  failures <- list()
  items <- list()
  n <- nrow(target_rows)
  state <- list(excluded = excluded, failures = failures, items = items)
  for (i in seq_len(n)) {
    state <- tw_then_at(state, i, function(state, local_i) {
      row <- target_rows[local_i, , drop = FALSE]
      symbol <- target_workspace_label(row)
      if (!target_is_confirmed(row)) {
        state$excluded[[length(state$excluded) + 1L]] <- list(
          project_target_id = as.character(row$id[[1]]),
          symbol = symbol,
          input_text = as.character(row$input_text[[1]] %||% ""),
          reason = "unconfirmed",
          message = sprintf("%s is not confirmed, so experimental PDB structures were not retrieved.", symbol)
        )
        return(state)
      }
      if (is.function(on_progress)) {
        on_progress(local_i, n, symbol)
      }
      tw_then(retrieve_target_structures(row, db_pool = db_pool, perform = perform), function(result) {
        if (identical(result$status, "unavailable")) {
          state$excluded[[length(state$excluded) + 1L]] <- list(
            project_target_id = as.character(row$id[[1]]),
            symbol = symbol,
            reason = "missing_uniprot",
            message = result$message
          )
          return(state)
        }
        if (identical(result$status, "error")) {
          state$failures[[length(state$failures) + 1L]] <- list(
            project_target_id = as.character(row$id[[1]]),
            symbol = symbol,
            message = result$message
          )
          return(state)
        }
        state$items[[length(state$items) + 1L]] <- result$structures
        state
      })
    })
  }

  tw_then(state, function(state) {
    items <- state$items
    excluded <- state$excluded
    failures <- state$failures

  summary_rows <- lapply(items, function(item) {
    data.frame(
      project_target_id = item$target$project_target_id,
      symbol = item$target$symbol,
      uniprot_accession = item$target$uniprot_accession,
      n_pdb_entries = as.integer(item$n_pdb_entries),
      n_polymer_entities = as.integer(item$n_polymer_entities),
      max_coverage_fraction = as.numeric(item$max_coverage_fraction),
      status = if (identical(item$n_pdb_entries, 0L)) "empty" else "ok",
      stringsAsFactors = FALSE
    )
  })
  summary <- if (length(summary_rows) == 0) empty_structure_summary() else do.call(rbind, summary_rows)

  list(
    status = "ready",
    message = NULL,
    structures = list(
      targets = items,
      summary = summary,
      excluded_targets = excluded,
      failures = failures,
      provenance = list(
        source = "RCSB Protein Data Bank",
        query_scope = RCSB_EXPERIMENTAL_SCOPE,
        search_api = RCSB_SEARCH_URL,
        data_api = RCSB_DATA_GRAPHQL,
        sequence_coordinates_api = RCSB_SEQCOORDS_GRAPHQL,
        cache_key_family = "rcsb:search:<uniprot>:experimental"
      )
    )
  )
  })
}
