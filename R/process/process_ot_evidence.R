can_fetch_ot_evidence <- function(target_row, project_row) {
  isTRUE(target_is_confirmed(target_row)) && isTRUE(project_disease_is_confirmed(project_row))
}

evidence_fetch_signature <- function(target_row, project_row) {
  if (!can_fetch_ot_evidence(target_row, project_row)) {
    return(NA_character_)
  }
  paste(
    as.character(target_row$id[[1]]),
    canonical_ensembl_gene_id(target_row$ensembl_gene_id[[1]]),
    toupper(trimws(project_disease_ontology_id(project_row))),
    sep = "|"
  )
}

should_retrieve_ot_evidence <- function(
  panel_active,
  target_row,
  project_row,
  last_signature = NA_character_,
  force = FALSE
) {
  if (!isTRUE(panel_active)) {
    return(FALSE)
  }
  if (!can_fetch_ot_evidence(target_row, project_row)) {
    return(FALSE)
  }
  if (isTRUE(force)) {
    return(TRUE)
  }
  sig <- evidence_fetch_signature(target_row, project_row)
  !identical(as.character(last_signature %||% NA_character_), sig)
}

blocked_ot_evidence <- function(target_row, project_row) {
  if (!target_is_confirmed(target_row)) {
    return(list(
      status = "blocked_target",
      message = "Confirm this target before retrieving disease evidence.",
      evidence = NULL
    ))
  }
  if (!project_disease_is_confirmed(project_row)) {
    return(list(
      status = "blocked_disease",
      message = "Confirm the project disease identity before retrieving Open Targets evidence.",
      evidence = NULL
    ))
  }
  NULL
}

unique_therapeutic_rows <- function(rows, max_rows = 12L) {
  seen <- character()
  out <- list()
  for (row in rows) {
    key <- row$drug_id
    if (!has_display_text(key)) {
      key <- row$drug_name
    }
    if (!has_display_text(key) || key %in% seen) {
      next
    }
    seen <- c(seen, key)
    out <- c(out, list(row))
    if (length(out) >= max_rows) {
      break
    }
  }
  out
}

parse_clinical_rows <- function(rows) {
  if (is.null(rows) || length(rows) == 0) {
    return(list())
  }

  lapply(rows, function(row) {
    drug <- row$drug %||% list()
    target <- row$target %||% list()
    disease <- row$disease %||% list()
    list(
      evidence_id = row$id %||% NA_character_,
      score = if (is.null(row$score)) NA_real_ else as.numeric(row$score),
      datasource_id = row$datasourceId %||% NA_character_,
      datatype_id = row$datatypeId %||% NA_character_,
      clinical_stage = row$clinicalStage %||% NA_character_,
      drug_name = drug$name %||% row$drugFromSource %||% NA_character_,
      drug_id = drug$id %||% NA_character_,
      target_id = target$id %||% NA_character_,
      target_symbol = target$approvedSymbol %||% NA_character_,
      disease_id = disease$id %||% NA_character_,
      disease_name = disease$name %||% NA_character_
    )
  })
}

clinical_row_matches_pair <- function(row, ensembl_gene_id, disease_id) {
  target_id <- canonical_ensembl_gene_id(row$target_id) %||% toupper(trimws(row$target_id %||% ""))
  expected_target <- canonical_ensembl_gene_id(ensembl_gene_id) %||% toupper(trimws(ensembl_gene_id))
  expected_disease <- toupper(trimws(disease_id))
  row_disease <- toupper(trimws(row$disease_id %||% ""))
  identical(target_id, expected_target) && identical(row_disease, expected_disease)
}

filter_clinical_rows_for_pair <- function(rows, ensembl_gene_id, disease_id) {
  Filter(function(row) clinical_row_matches_pair(row, ensembl_gene_id, disease_id), rows)
}

readable_drug_name <- function(name) {
  if (!has_display_text(name)) {
    return(NA_character_)
  }
  text <- trimws(as.character(name))
  if (!identical(text, toupper(text))) {
    return(text)
  }
  parts <- unlist(strsplit(tolower(text), "\\s+"))
  paste(
    vapply(parts, function(part) {
      if (!nzchar(part)) {
        return(part)
      }
      paste0(toupper(substr(part, 1, 1)), substring(part, 2))
    }, character(1)),
    collapse = " "
  )
}

format_clinical_stage <- function(stage) {
  if (!has_display_text(stage)) {
    return("Stage not provided")
  }
  raw <- toupper(gsub(" ", "_", trimws(as.character(stage))))
  switch(
    raw,
    APPROVAL = "Approval",
    PHASE_1 = "Phase 1",
    PHASE_2 = "Phase 2",
    PHASE_3 = "Phase 3",
    PHASE_4 = "Phase 4",
    PHASE1 = "Phase 1",
    PHASE2 = "Phase 2",
    PHASE3 = "Phase 3",
    PHASE4 = "Phase 4",
    gsub("_", " ", as.character(stage), fixed = TRUE)
  )
}

build_datatype_table <- function(assoc) {
  if (is.null(assoc) || is.null(assoc$datatype_scores) || nrow(assoc$datatype_scores) == 0) {
    return(data.frame(
      datatype_id = character(),
      datatype_label = character(),
      label_source = character(),
      score = numeric(),
      score_status = character(),
      stringsAsFactors = FALSE
    ))
  }

  scores <- assoc$datatype_scores
  data.frame(
    datatype_id = scores$id,
    datatype_label = vapply(scores$id, ot_datatype_label, character(1)),
    label_source = "targetweave_display_label",
    score = scores$score,
    score_status = ifelse(is.na(scores$score), "missing", "present"),
    stringsAsFactors = FALSE
  )
}

build_datasource_table <- function(assoc) {
  if (is.null(assoc) || is.null(assoc$datasource_scores) || nrow(assoc$datasource_scores) == 0) {
    return(data.frame(
      datasource_id = character(),
      datasource_label = character(),
      label_source = character(),
      score = numeric(),
      score_status = character(),
      stringsAsFactors = FALSE
    ))
  }

  scores <- assoc$datasource_scores
  labels <- vapply(scores$id, ot_datasource_label, character(1))
  sources <- ifelse(labels == scores$id, "opentargets_id", "targetweave_display_label")
  data.frame(
    datasource_id = scores$id,
    datasource_label = labels,
    label_source = sources,
    score = scores$score,
    score_status = ifelse(is.na(scores$score), "missing", "present"),
    stringsAsFactors = FALSE
  )
}

build_target_disease_evidence <- function(
  target_row,
  project_row,
  parsed,
  api_result
) {
  chart_scope <- "direct"
  assoc <- parsed$direct
  if (is.null(assoc)) {
    assoc <- parsed$inclusive
    chart_scope <- "inclusive_indirect"
  }

  state <- if (is.null(assoc)) {
    "empty"
  } else {
    "ready"
  }

  list(
    project_target_id = as.character(target_row$id[[1]]),
    target = list(
      ensembl_gene_id = canonical_ensembl_gene_id(target_row$ensembl_gene_id[[1]]),
      symbol = target_row$display_symbol[[1]] %||% parsed$target_symbol
    ),
    disease = list(
      id = project_disease_ontology_id(project_row) %||% parsed$disease_id,
      name = project_row$disease_name[[1]] %||% parsed$disease_name,
      user_label = project_row$disease_label[[1]]
    ),
    association = list(
      overall_score_direct = if (is.null(parsed$direct)) NA_real_ else parsed$direct$overall_score,
      overall_score_inclusive = if (is.null(parsed$inclusive)) NA_real_ else parsed$inclusive$overall_score,
      association_scope = chart_scope,
      association_scope_note = if (identical(chart_scope, "direct")) {
        "The chart shows evidence linked directly to the confirmed disease term."
      } else {
        "No direct association was returned. The chart uses broader ontology-aware scores, which can include related descendant disease terms."
      },
      datatype_scores = build_datatype_table(assoc),
      datasource_scores = build_datasource_table(assoc),
      component_score_note = "Open Targets returns datatypeScores and datasourceScores as independent scored components. TargetWeave does not map data sources onto data types."
    ),
    therapeutic_evidence = list(
      count = parsed$clinical_count %||% 0L,
      rows = unique_therapeutic_rows(filter_clinical_rows_for_pair(
        parse_clinical_rows(parsed$clinical_rows),
        canonical_ensembl_gene_id(target_row$ensembl_gene_id[[1]]),
        project_disease_ontology_id(project_row) %||% parsed$disease_id
      )),
      filter = "disease(efoId) + evidences(ensemblIds, enableIndirect: false, datasourceIds: clinical_precedence); rows must match target.id and disease.id"
    ),
    evidence_details = list(
      datasource_scores = build_datasource_table(assoc)
    ),
    state = state,
    provenance = list(
      source = "Open Targets",
      query_name = api_result$query_name %||% "TargetWeaveTargetDiseaseEvidence",
      target_id = parsed$target_id %||% canonical_ensembl_gene_id(target_row$ensembl_gene_id[[1]]),
      disease_id = parsed$disease_id %||% project_disease_ontology_id(project_row),
      retrieved_at = as.character(api_result$retrieved_at %||% NA_character_),
      cache_status = if (identical(api_result$cache_status, "fresh")) {
        "cached/fresh"
      } else if (identical(api_result$cache_status, "stale")) {
        "stale"
      } else if (isTRUE(api_result$from_cache)) {
        "cached/fresh"
      } else {
        api_result$cache_status %||% "live"
      },
      from_cache = isTRUE(api_result$from_cache),
      data_version = parsed$meta$data_version %||% NA_character_,
      api_version = parsed$meta$api_version %||% NA_character_,
      url = "https://api.platform.opentargets.org/api/v4/graphql",
      association_scope = chart_scope,
      graphql_scope_direct = "enableIndirect = false",
      graphql_scope_inclusive = "enableIndirect = true",
      error = api_result$error %||% NA_character_
    )
  )
}

retrieve_target_disease_evidence <- function(
  target_row,
  project_row,
  db_pool = NULL,
  fetch = ot_target_disease_evidence,
  perform = httr2::req_perform
) {
  blocked <- blocked_ot_evidence(target_row, project_row)
  if (!is.null(blocked)) {
    return(blocked)
  }

  ensembl_id <- canonical_ensembl_gene_id(target_row$ensembl_gene_id[[1]])
  disease_id <- as.character(project_disease_ontology_id(project_row))
  call_fetch <- function() {
    if ("perform" %in% names(formals(fetch))) {
      fetch(ensembl_id, disease_id, db_pool = db_pool, perform = perform)
    } else {
      fetch(ensembl_id, disease_id, db_pool = db_pool)
    }
  }

  tw_then(call_fetch(), function(result) {
    if (!isTRUE(result$ok) && !isTRUE(result$from_cache)) {
      return(list(
        status = "error",
        message = result$error %||% "Open Targets could not be reached.",
        evidence = NULL,
        provenance = list(
          source = "Open Targets",
          cache_status = result$cache_status,
          error = result$error,
          query_name = result$query_name
        )
      ))
    }

    parsed <- parse_ot_target_disease_evidence(result$data)
    if (!isTRUE(parsed$ok)) {
      return(list(
        status = "error",
        message = parsed$error %||% "Open Targets returned an unusable payload.",
        evidence = NULL
      ))
    }

    evidence <- build_target_disease_evidence(target_row, project_row, parsed, result)
    status <- if (identical(result$cache_status, "stale")) {
      "stale"
    } else if (identical(evidence$state, "empty")) {
      "empty"
    } else if (isTRUE(result$from_cache)) {
      "cached"
    } else {
      "live"
    }

    list(
      status = status,
      message = if (identical(status, "empty")) {
        "Open Targets returned no association for this confirmed target and disease."
      } else if (identical(status, "stale")) {
        "Showing a stale cached Open Targets result because the live request failed."
      } else {
        NULL
      },
      evidence = evidence
    )
  })
}
