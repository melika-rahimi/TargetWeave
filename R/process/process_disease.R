# TargetWeave display labels for Open Targets component ids. These are not
# API-returned names; the source payload provides ids only.
ot_datatype_label <- function(id) {
  if (!has_display_text(id)) {
    return("Unknown data type")
  }
  switch(
    as.character(id),
    genetic_association = "Genetic association",
    somatic_mutation = "Somatic mutation",
    known_drug = "Known drug",
    clinical = "Clinical",
    affected_pathway = "Affected pathway",
    rna_expression = "RNA expression",
    animal_model = "Animal model",
    literature = "Literature",
    genetic_literature = "Genetic literature",
    as.character(id)
  )
}

# Explicit TargetWeave display names for known Open Targets datasource ids.
# Unknown ids are shown as the raw id; do not invent a biological name.
ot_datasource_label <- function(id) {
  if (!has_display_text(id)) {
    return(NA_character_)
  }
  switch(
    as.character(id),
    clinical_precedence = "Clinical Precedence",
    eva = "EVA",
    eva_somatic = "EVA Somatic",
    gwas_credible_sets = "GWAS credible sets",
    gene2phenotype = "Gene2Phenotype",
    uniprot_literature = "UniProt literature",
    uniprot_variants = "UniProt variants",
    clingen = "ClinGen",
    cancer_gene_census = "Cancer Gene Census",
    intogen = "IntOGen",
    cancer_biomarkers = "Cancer Biomarkers",
    reactome = "Reactome",
    crispr = "CRISPR",
    slapenrich = "SLAPenrich",
    progeny = "PROGENy",
    europepmc = "Europe PMC",
    expression_atlas = "Expression Atlas",
    impc = "IMPC",
    chembl = "ChEMBL",
    as.character(id)
  )
}

disease_search_query_variants <- function(query_text) {
  query_text <- trimws(query_text %||% "")
  if (!nzchar(query_text)) {
    return(character())
  }

  dehyphenated <- gsub("\\s+", " ", gsub("-", " ", query_text))
  unique(c(query_text, dehyphenated)[nzchar(c(query_text, dehyphenated))])
}

normalize_disease_text <- function(x) {
  text <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(text)) {
    return("")
  }
  text <- gsub("[-_/]+", " ", text)
  text <- gsub("[^a-z0-9 ]+", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

disease_match_reason <- function(match_type, matched_synonym = NA_character_) {
  if (identical(match_type, "exact_canonical_name")) {
    return("Exact canonical name match")
  }
  if (identical(match_type, "exact_synonym") && has_display_text(matched_synonym)) {
    return(sprintf("Matched through synonym: \"%s\"", matched_synonym))
  }
  if (identical(match_type, "exact_synonym")) {
    return("Matched through a documented Open Targets synonym")
  }
  "Open Targets search match. Search score is a ranking signal, not biological confidence."
}

classify_disease_match <- function(hit, query_text) {
  query_norm <- normalize_disease_text(query_text)
  name_norm <- normalize_disease_text(hit$name)
  if (nzchar(query_norm) && identical(name_norm, query_norm)) {
    return(list(
      match_type = "exact_canonical_name",
      matched_synonym = NA_character_,
      match_reason = disease_match_reason("exact_canonical_name")
    ))
  }

  synonyms <- hit$synonyms %||% list()
  exact_synonym <- NULL
  for (syn in synonyms) {
    if (nzchar(query_norm) && identical(normalize_disease_text(syn$term), query_norm)) {
      exact_synonym <- syn
      if (identical(syn$relation, "hasExactSynonym")) {
        break
      }
    }
  }

  if (!is.null(exact_synonym)) {
    return(list(
      match_type = "exact_synonym",
      matched_synonym = exact_synonym$term,
      match_reason = disease_match_reason("exact_synonym", exact_synonym$term)
    ))
  }

  list(
    match_type = "search_text_match",
    matched_synonym = NA_character_,
    match_reason = disease_match_reason("search_text_match")
  )
}

normalize_disease_candidates <- function(parsed_search, query_text) {
  hits <- parsed_search$hits %||% list()
  diseases <- Filter(function(hit) {
    identical(tolower(as.character(hit$entity %||% "")), "disease")
  }, hits)

  diseases <- diseases[order(
    vapply(diseases, function(h) as.numeric(h$score %||% -Inf), numeric(1)),
    decreasing = TRUE
  )]

  candidates <- lapply(seq_along(diseases), function(i) {
    hit <- diseases[[i]]
    classified <- classify_disease_match(hit, query_text)
    list(
      candidate_key = hit$id,
      id = hit$id,
      name = hit$name,
      description = hit$description,
      synonyms = hit$synonyms %||% list(),
      search_score = hit$score,
      match_rank = i,
      match_type = classified$match_type,
      matched_synonym = classified$matched_synonym,
      match_reason = classified$match_reason
    )
  })

  canonical <- which(vapply(candidates, function(c) identical(c$match_type, "exact_canonical_name"), logical(1)))
  synonym <- which(vapply(candidates, function(c) identical(c$match_type, "exact_synonym"), logical(1)))

  unique_primary <- FALSE
  primary_index <- NA_integer_
  if (length(canonical) == 1L) {
    unique_primary <- TRUE
    primary_index <- canonical[[1]]
  } else if (length(canonical) == 0L && length(synonym) == 1L) {
    unique_primary <- TRUE
    primary_index <- synonym[[1]]
  }

  if (isTRUE(unique_primary) && length(candidates) > 1L && primary_index > 1L) {
    primary <- candidates[[primary_index]]
    others <- candidates[-primary_index]
    candidates <- c(list(primary), others)
    candidates <- lapply(seq_along(candidates), function(i) {
      candidates[[i]]$match_rank <- i
      candidates[[i]]
    })
  }

  status <- if (length(candidates) == 0) {
    "failed"
  } else if (length(candidates) == 1 || isTRUE(unique_primary)) {
    "unresolved"
  } else {
    "ambiguous"
  }

  list(
    query_text = query_text,
    status = status,
    unique_primary = isTRUE(unique_primary),
    candidates = candidates,
    total_hits = parsed_search$total %||% length(hits),
    meta = parsed_search$meta
  )
}

resolve_project_disease <- function(query_text, db_pool = NULL, search_fetch = ot_search_diseases) {
  query_text <- trimws(query_text)
  variants <- disease_search_query_variants(query_text)
  if (length(variants) == 0) {
    return(list(
      status = "failed",
      unique_primary = FALSE,
      candidates = list(),
      message = "Disease context is empty.",
      source_report = empty_source_report("No disease query.")
    ))
  }

  state <- list(last_failed = NULL, done = NULL)
  for (variant in variants) {
    local_variant <- variant
    state <- tw_then(state, function(state) {
      if (!is.null(state$done)) {
        return(state)
      }
      tw_then(search_fetch(local_variant, db_pool = db_pool), function(result) {
        parsed <- if (isTRUE(result$ok)) {
          parse_ot_disease_search(result$data)
        } else {
          list(ok = FALSE, error = result$error, hits = list(), total = 0L, meta = parse_ot_meta(NULL))
        }

        if (!isTRUE(parsed$ok)) {
          state$last_failed <- list(
            status = "failed",
            unique_primary = FALSE,
            candidates = list(),
            message = parsed$error %||% result$error %||% "Open Targets disease search failed.",
            source_report = source_report(result),
            query_name = result$query_name %||% "TargetWeaveDiseaseSearch",
            query_text = local_variant
          )
          return(state)
        }

        normalized <- normalize_disease_candidates(parsed, local_variant)
        normalized$source_report <- source_report(result)
        normalized$query_name <- result$query_name %||% "TargetWeaveDiseaseSearch"
        normalized$user_query_text <- query_text
        if (!identical(normalized$status, "failed")) {
          state$done <- normalized
          return(state)
        }
        last_failed <- normalized
        last_failed$message <- "No disease or phenotype candidates were returned."
        last_failed$query_name <- result$query_name %||% "TargetWeaveDiseaseSearch"
        last_failed$source_report <- source_report(result)
        state$last_failed <- last_failed
        state
      })
    })
  }

  tw_then(state, function(state) {
    state$done %||% state$last_failed
  })
}

decode_disease_payload <- function(text) {
  if (!has_display_text(text)) {
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.list(parsed) || is.null(parsed$status)) {
    return(NULL)
  }

  parsed
}
