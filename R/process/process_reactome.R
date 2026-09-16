target_reactome_label <- function(row) {
  target_workspace_label(row) %||% as.character(row$input_text[[1]] %||% row$id[[1]])
}

pathway_gate <- function(target_rows) {
  confirmed <- target_rows
  if (!is.null(confirmed) && nrow(confirmed) > 0) {
    confirmed <- confirmed[confirmed$resolution_status == "confirmed", , drop = FALSE]
  }
  n <- if (is.null(confirmed)) 0L else nrow(confirmed)
  if (n < 1L) {
    return(list(
      ok = FALSE,
      status = "blocked_targets",
      message = "Confirm at least one target identity before retrieving Reactome pathway membership."
    ))
  }
  list(ok = TRUE, status = "ok", message = NULL)
}

should_retrieve_pathways <- function(
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
  gate <- pathway_gate(target_rows)
  if (!isTRUE(gate$ok)) {
    return(FALSE)
  }
  next_sig <- pathway_signature(target_rows)
  !identical(as.character(last_signature %||% NA_character_), next_sig)
}

pathway_signature <- function(target_rows) {
  confirmed_scientific_target_set(target_rows)
}

split_pathway_targets <- function(target_rows) {
  if (is.null(target_rows) || nrow(target_rows) == 0) {
    return(list(
      confirmed = target_rows[0, , drop = FALSE],
      excluded = target_rows[0, , drop = FALSE]
    ))
  }
  confirmed <- target_rows[vapply(seq_len(nrow(target_rows)), function(i) {
    target_is_confirmed(target_rows[i, , drop = FALSE])
  }, logical(1)), , drop = FALSE]
  excluded <- target_rows[!(target_rows$id %in% confirmed$id), , drop = FALSE]
  list(confirmed = confirmed, excluded = excluded)
}

retrieve_target_reactome_membership <- function(
  target_row,
  db_pool = NULL,
  reactome_release = NA_character_,
  perform = httr2::req_perform
) {
  symbol <- target_reactome_label(target_row)
  accession <- blank_to_null(target_row$uniprot_accession[[1]])
  if (is.null(accession)) {
    return(list(
      status = "unavailable",
      message = sprintf("%s has no confirmed UniProt accession for Reactome mapping.", symbol),
      membership = NULL
    ))
  }

  tw_then(
    fetch_reactome_uniprot_pathways(
      accession,
      db_pool = db_pool,
      reactome_release = reactome_release,
      perform = perform
    ),
    function(api_result) {

  if (reactome_mapping_is_empty(api_result)) {
    membership <- list(
      project_target_id = as.character(target_row$id[[1]]),
      symbol = symbol,
      ensembl_gene_id = blank_to_null(target_row$ensembl_gene_id[[1]]),
      uniprot_accession = accession,
      pathways = empty_reactome_pathways(),
      provenance = reactome_target_provenance(
        api_result,
        accession,
        reactome_release,
        n_pathways = 0L
      )
    )
    return(list(status = "empty", message = NULL, membership = membership))
  }

  if (!isTRUE(api_result$ok)) {
    return(list(
      status = "error",
      message = sprintf(
        "Pathway data unavailable for %s. %s",
        symbol,
        api_result$error %||% "Reactome request failed."
      ),
      membership = NULL,
      cache_status = api_result$cache_status,
      error = api_result$error
    ))
  }

  parsed <- parse_reactome_pathway_membership(api_result$data)
  if (!isTRUE(parsed$ok)) {
    return(list(
      status = "error",
      message = sprintf("Pathway data unavailable for %s. %s", symbol, parsed$error),
      membership = NULL
    ))
  }

  membership <- list(
    project_target_id = as.character(target_row$id[[1]]),
    symbol = symbol,
    ensembl_gene_id = blank_to_null(target_row$ensembl_gene_id[[1]]),
    uniprot_accession = accession,
    pathways = parsed$pathways,
    provenance = reactome_target_provenance(
      api_result,
      accession,
      reactome_release,
      n_pathways = nrow(parsed$pathways)
    )
  )

  status <- if (nrow(parsed$pathways) == 0) "empty" else "ok"
  list(status = status, message = NULL, membership = membership)
    }
  )
}

reactome_target_provenance <- function(api_result, accession, reactome_release, n_pathways) {
  cache_label <- switch(
    as.character(api_result$cache_status %||% ""),
    fresh = "cached",
    live = "live",
    stale = "stale",
    as.character(api_result$cache_status %||% "unknown")
  )
  list(
    source = "Reactome",
    reactome_release = reactome_release,
    retrieved_at = api_result$retrieved_at,
    cache_status = cache_label,
    identifier_used = accession,
    retrieval_method = "content_service_uniprot_mapping",
    membership_scope = REACTOME_MEMBERSHIP_SCOPE,
    source_url = api_result$url,
    cache_key_family = "reactome:membership:<release>:<uniprot>:lowest_level",
    n_pathways = as.integer(n_pathways)
  )
}

retrieve_project_pathways <- function(
  project_row,
  target_rows,
  db_pool = NULL,
  perform = httr2::req_perform,
  version_perform = perform,
  on_progress = NULL
) {
  gate <- pathway_gate(target_rows)
  if (!isTRUE(gate$ok)) {
    return(list(status = gate$status, message = gate$message, pathways = NULL))
  }

  split <- split_pathway_targets(target_rows)
  excluded <- list()
  if (!is.null(split$excluded) && nrow(split$excluded) > 0) {
    for (i in seq_len(nrow(split$excluded))) {
      row <- split$excluded[i, , drop = FALSE]
      excluded[[length(excluded) + 1L]] <- list(
        project_target_id = as.character(row$id[[1]]),
        symbol = target_reactome_label(row),
        input_text = as.character(row$input_text[[1]] %||% ""),
        reason = "unconfirmed",
        message = sprintf(
          "%s is not confirmed, so Reactome pathway membership was not retrieved.",
          target_reactome_label(row)
        )
      )
    }
  }

  tw_then(fetch_reactome_version(db_pool = db_pool, perform = version_perform), function(version) {
    release <- if (isTRUE(version$ok)) version$version else NA_character_
    state <- list(excluded = excluded, memberships = list(), failures = list())
    n <- nrow(split$confirmed)
    for (i in seq_len(n)) {
      state <- tw_then_at(state, i, function(state, local_i) {
        row <- split$confirmed[local_i, , drop = FALSE]
        if (is.function(on_progress)) {
          on_progress(local_i, n, target_reactome_label(row))
        }
        tw_then(
          retrieve_target_reactome_membership(
            row,
            db_pool = db_pool,
            reactome_release = release,
            perform = perform
          ),
          function(result) {
            if (identical(result$status, "unavailable")) {
              state$excluded[[length(state$excluded) + 1L]] <- list(
                project_target_id = as.character(row$id[[1]]),
                symbol = target_reactome_label(row),
                reason = "missing_uniprot",
                message = result$message
              )
              return(state)
            }
            if (identical(result$status, "error")) {
              state$failures[[length(state$failures) + 1L]] <- list(
                project_target_id = as.character(row$id[[1]]),
                symbol = target_reactome_label(row),
                message = result$message
              )
              return(state)
            }
            state$memberships[[length(state$memberships) + 1L]] <- result$membership
            state
          }
        )
      })
    }
    tw_then(state, function(state) {
      overlap <- build_pathway_overlap(
        state$memberships,
        excluded = state$excluded,
        failures = state$failures,
        reactome_release = release,
        version_provenance = version
      )
      list(
        status = "ready",
        message = NULL,
        pathways = overlap
      )
    })
  })
}
