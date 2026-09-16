# Milestone 5 comparison reuses the M4 pair-level Open Targets query and cache.
#
# Evaluated and rejected: a single disease.associatedTargets(Bs: [...]) batch.
# That would skip per-pair cache keys, couple failures across targets, and
# change payload semantics relative to the M4 evidence page.
#
# Chosen strategy (max 8 targets):
# 1. Inspect opentargets:association:<ENSG>:<DISEASE> freshness.
# 2. Call retrieve_target_disease_evidence for each confirmed target.
#    Fresh cache is served by http_post_json with no live HTTP.
# 3. Fetch live only for missing or expired pairs (and stale fallback on error).
# 4. Sequential, not concurrent. Partial failure keeps other targets.

comparison_signature <- function(project_row, target_rows) {
  if (is.null(project_row)) {
    return(NA_character_)
  }
  key <- if (is.data.frame(target_rows)) {
    confirmed_scientific_target_set(target_rows)
  } else {
    paste(sort(as.character(target_rows)), collapse = ",")
  }
  if (!nzchar(key)) {
    return(NA_character_)
  }
  paste(
    as.character(project_row$id[[1]]),
    toupper(trimws(project_disease_ontology_id(project_row))),
    key,
    sep = "|"
  )
}

should_retrieve_comparison <- function(
  panel_active,
  project_row,
  target_rows,
  last_signature = NA_character_,
  force = FALSE
) {
  if (!isTRUE(panel_active)) {
    return(FALSE)
  }
  gate <- comparison_gate(project_row, target_rows)
  if (!isTRUE(gate$ok)) {
    return(FALSE)
  }
  if (isTRUE(force)) {
    return(TRUE)
  }
  sig <- comparison_signature(project_row, target_rows)
  !identical(as.character(last_signature %||% NA_character_), sig)
}

comparison_gate <- function(project_row, target_rows) {
  if (is.data.frame(target_rows)) {
    n_confirmed <- if (nrow(target_rows) == 0) {
      0L
    } else {
      sum(vapply(seq_len(nrow(target_rows)), function(i) {
        target_is_confirmed(target_rows[i, , drop = FALSE])
      }, logical(1)))
    }
  } else {
    n_confirmed <- length(target_rows)
  }

  if (!isTRUE(project_disease_is_confirmed(project_row))) {
    return(list(
      ok = FALSE,
      status = "blocked_disease",
      message = "Confirm the project disease identity before comparing Open Targets evidence."
    ))
  }

  if (n_confirmed < 2L) {
    return(list(
      ok = FALSE,
      status = "blocked_targets",
      message = "Comparison needs at least two confirmed targets."
    ))
  }

  list(ok = TRUE, status = "ok", message = NULL)
}

split_comparison_targets <- function(target_rows) {
  if (is.null(target_rows) || !is.data.frame(target_rows) || nrow(target_rows) == 0) {
    empty <- data.frame(
      id = character(),
      resolution_status = character(),
      display_symbol = character(),
      ensembl_gene_id = character(),
      uniprot_accession = character(),
      input_text = character(),
      stringsAsFactors = FALSE
    )
    return(list(confirmed = empty, excluded = empty))
  }

  keep <- vapply(seq_len(nrow(target_rows)), function(i) {
    target_is_confirmed(target_rows[i, , drop = FALSE])
  }, logical(1))

  list(
    confirmed = target_rows[keep, , drop = FALSE],
    excluded = target_rows[!keep, , drop = FALSE]
  )
}

comparison_pair_plan <- function(confirmed_rows, project_row, db_pool = NULL) {
  disease_id <- project_disease_ontology_id(project_row)
  if (is.null(confirmed_rows) || nrow(confirmed_rows) == 0) {
    return(data.frame(
      project_target_id = character(),
      ensembl_gene_id = character(),
      cache_freshness = character(),
      needs_live_fetch = logical(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(seq_len(nrow(confirmed_rows)), function(i) {
    row <- confirmed_rows[i, , drop = FALSE]
    ensembl_id <- canonical_ensembl_gene_id(row$ensembl_gene_id[[1]])
    freshness <- association_cache_freshness(db_pool, ensembl_id, disease_id)
    data.frame(
      project_target_id = as.character(row$id[[1]]),
      ensembl_gene_id = ensembl_id %||% NA_character_,
      cache_freshness = freshness,
      needs_live_fetch = !identical(freshness, "fresh"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

empty_datatype_matrix <- function() {
  data.frame(
    project_target_id = character(),
    symbol = character(),
    ensembl_gene_id = character(),
    datatype_id = character(),
    datatype_label = character(),
    score = numeric(),
    is_missing = logical(),
    stringsAsFactors = FALSE
  )
}

comparison_datatype_order <- function(ids) {
  preferred <- c(
    "clinical",
    "genetic_association",
    "somatic_mutation",
    "affected_pathway",
    "literature",
    "genetic_literature",
    "rna_expression",
    "animal_model",
    "known_drug"
  )
  rest <- setdiff(ids, preferred)
  c(preferred[preferred %in% ids], sort(rest))
}

pair_direct_datatype_scores <- function(result) {
  evidence <- result$evidence
  if (is.null(evidence) || is.null(evidence$association)) {
    return(NULL)
  }
  if (!identical(evidence$association$association_scope, "direct")) {
    return(NULL)
  }
  evidence$association$datatype_scores
}

build_comparison_datatype_matrix <- function(pair_entries) {
  usable <- Filter(function(entry) {
    !is.null(entry$result$evidence) &&
      !identical(entry$result$status, "error") &&
      identical(entry$result$evidence$association$association_scope, "direct")
  }, pair_entries)

  datatype_ids <- unique(unlist(lapply(usable, function(entry) {
    scores <- pair_direct_datatype_scores(entry$result)
    if (is.null(scores) || nrow(scores) == 0) {
      return(character())
    }
    scores$datatype_id
  })))

  if (length(datatype_ids) == 0) {
    return(empty_datatype_matrix())
  }

  datatype_ids <- comparison_datatype_order(as.character(datatype_ids))
  rows <- list()

  for (entry in usable) {
    symbol <- entry$symbol
    scores <- pair_direct_datatype_scores(entry$result)
    for (datatype_id in datatype_ids) {
      hit <- scores[scores$datatype_id == datatype_id, , drop = FALSE]
      score <- if (nrow(hit) == 0) NA_real_ else hit$score[[1]]
      rows <- c(rows, list(data.frame(
        project_target_id = entry$project_target_id,
        symbol = symbol,
        ensembl_gene_id = entry$ensembl_gene_id,
        datatype_id = datatype_id,
        datatype_label = ot_datatype_label(datatype_id),
        score = score,
        is_missing = is.na(score),
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(rows) == 0) {
    return(empty_datatype_matrix())
  }
  do.call(rbind, rows)
}

normalize_comparison_selection <- function(selected_ids, confirmed_ids, previous_ids = NULL) {
  confirmed_ids <- as.character(confirmed_ids)
  selected <- unique(intersect(as.character(selected_ids), confirmed_ids))
  if (length(selected) >= 2L) {
    return(selected)
  }
  previous <- unique(intersect(as.character(previous_ids), confirmed_ids))
  if (length(previous) >= 2L) {
    return(previous)
  }
  confirmed_ids
}

filter_comparison_visual <- function(comparison, selected_ids) {
  selected_ids <- as.character(selected_ids)
  targets <- comparison$targets
  keep <- targets$project_target_id %in% selected_ids
  comparison$targets_visible <- targets[keep, , drop = FALSE]
  matrix <- comparison$datatype_matrix
  comparison$datatype_matrix_visible <- matrix[matrix$project_target_id %in% selected_ids, , drop = FALSE]
  comparison
}

comparison_status_label <- function(status, cache_status = NA_character_) {
  if (identical(status, "error")) {
    return("Evidence unavailable")
  }
  if (identical(status, "stale") || identical(cache_status, "stale")) {
    return("Stale evidence")
  }
  if (identical(status, "empty")) {
    return("No association returned")
  }
  if (identical(status, "cached") || identical(cache_status, "cached/fresh")) {
    return("Cached / fresh")
  }
  if (identical(status, "live")) {
    return("Live")
  }
  as.character(status %||% "unknown")
}

retrieve_project_comparison <- function(
  project_row,
  target_rows,
  db_pool = NULL,
  pair_retrieve = retrieve_target_disease_evidence,
  on_progress = NULL,
  perform = httr2::req_perform
) {
  split <- split_comparison_targets(target_rows)
  gate <- comparison_gate(project_row, target_rows)
  if (!isTRUE(gate$ok)) {
    return(list(
      status = gate$status,
      message = gate$message,
      comparison = NULL
    ))
  }

  confirmed <- split$confirmed
  excluded <- lapply(seq_len(nrow(split$excluded)), function(i) {
    row <- split$excluded[i, , drop = FALSE]
    list(
      project_target_id = as.character(row$id[[1]]),
      symbol = target_workspace_label(row),
      input_text = as.character(row$input_text[[1]]),
      reason = "Target is not confirmed"
    )
  })

  plan <- comparison_pair_plan(confirmed, project_row, db_pool)
  n <- nrow(confirmed)
  state <- list(pair_entries = vector("list", n))

  for (i in seq_len(n)) {
    state <- tw_then_at(state, i, function(state, local_i) {
      row <- confirmed[local_i, , drop = FALSE]
      symbol <- target_workspace_label(row)
      if (is.function(on_progress)) {
        on_progress(local_i, n, symbol)
      }
      call_pair <- function() {
        args <- list(row, project_row, db_pool = db_pool)
        if ("perform" %in% names(formals(pair_retrieve))) {
          args$perform <- perform
        }
        do.call(pair_retrieve, args)
      }
      tw_then(call_pair(), function(result) {
        ensembl_id <- canonical_ensembl_gene_id(row$ensembl_gene_id[[1]])
        state$pair_entries[[local_i]] <- list(
          project_target_id = as.character(row$id[[1]]),
          symbol = symbol,
          ensembl_gene_id = ensembl_id,
          result = result,
          planned_freshness = plan$cache_freshness[[local_i]],
          needs_live_fetch = isTRUE(plan$needs_live_fetch[[local_i]])
        )
        state
      })
    })
  }

  tw_then(state, function(state) {
    pair_entries <- Filter(function(entry) is.list(entry) && !is.null(entry$result), state$pair_entries)
    targets <- do.call(rbind, lapply(pair_entries, function(entry) {
      evidence <- entry$result$evidence
      assoc <- if (is.null(evidence)) NULL else evidence$association
      cache_raw <- if (is.null(evidence)) {
        entry$result$provenance$cache_status
      } else {
        evidence$provenance$cache_status
      }
      retrieved_raw <- if (is.null(evidence)) {
        NULL
      } else {
        evidence$provenance$retrieved_at
      }
      data.frame(
        project_target_id = as.character(entry$project_target_id[[1]] %||% NA_character_),
        symbol = as.character(entry$symbol[[1]] %||% NA_character_),
        ensembl_gene_id = {
          id <- as.character(entry$ensembl_gene_id %||% NA_character_)
          if (length(id) != 1L) NA_character_ else id
        },
        overall_direct_score = {
          val <- if (is.null(assoc)) NA_real_ else suppressWarnings(as.numeric(unlist(assoc$overall_score_direct)[1]))
          if (length(val) != 1L) NA_real_ else val
        },
        overall_inclusive_score = {
          val <- if (is.null(assoc)) NA_real_ else suppressWarnings(as.numeric(unlist(assoc$overall_score_inclusive)[1]))
          if (length(val) != 1L) NA_real_ else val
        },
        retrieval_status = {
          st <- as.character(entry$result$status %||% "error")
          if (length(st) != 1L) "error" else st
        },
        cache_status = {
          st <- as.character(cache_raw %||% NA_character_)
          if (length(st) != 1L) NA_character_ else st
        },
        retrieved_at = {
          st <- as.character(retrieved_raw %||% NA_character_)
          if (length(st) != 1L) NA_character_ else st
        },
        available = !identical(entry$result$status, "error") && !is.null(evidence),
        stringsAsFactors = FALSE
      )
    }))

    meta_source <- NULL
    for (entry in pair_entries) {
      if (!is.null(entry$result$evidence$provenance$data_version)) {
        meta_source <- entry$result$evidence$provenance
        break
      }
    }

    comparison <- list(
      disease = list(
        id = project_disease_ontology_id(project_row),
        name = project_row$disease_name[[1]] %||% project_row$disease_label[[1]],
        user_label = project_row$disease_label[[1]]
      ),
      targets = targets,
      datatype_matrix = build_comparison_datatype_matrix(pair_entries),
      excluded_targets = excluded,
      pair_plan = plan,
      provenance = list(
        source = "Open Targets",
        disease_id = project_disease_ontology_id(project_row),
        association_scope = "direct",
        data_version = meta_source$data_version %||% NA_character_,
        api_version = meta_source$api_version %||% NA_character_,
        query_name = "TargetWeaveTargetDiseaseEvidence",
        strategy = "M4 pair cache; sequential retrieve for missing or expired pairs"
      )
    )

    list(
      status = "ready",
      message = NULL,
      comparison = comparison
    )
  })
}
