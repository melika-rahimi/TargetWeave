literature_gate <- function(project_row, target_rows) {
  if (!isTRUE(project_disease_is_confirmed(project_row))) {
    return(list(
      ok = FALSE,
      status = "blocked_disease",
      message = "Confirm the project disease identity before retrieving PubMed literature."
    ))
  }
  confirmed <- if (is.null(target_rows) || nrow(target_rows) == 0) {
    0L
  } else {
    sum(target_rows$resolution_status == "confirmed")
  }
  if (confirmed < 1L) {
    return(list(
      ok = FALSE,
      status = "blocked_targets",
      message = "Confirm at least one target identity before retrieving PubMed literature."
    ))
  }
  list(ok = TRUE, status = "ok", message = NULL)
}

literature_signature <- function(project_row, target_rows) {
  confirmed <- target_rows
  if (!is.null(confirmed) && nrow(confirmed) > 0) {
    confirmed <- confirmed[confirmed$resolution_status == "confirmed", , drop = FALSE]
  }
  ids <- if (is.null(confirmed) || nrow(confirmed) == 0) character() else sort(as.character(confirmed$id))
  paste(
    project_disease_ontology_id(project_row) %||% "",
    paste(ids, collapse = "|"),
    sep = "::"
  )
}

should_retrieve_literature <- function(
  panel_active,
  project_row,
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
  gate <- literature_gate(project_row, target_rows)
  if (!isTRUE(gate$ok)) {
    return(FALSE)
  }
  !identical(as.character(last_signature %||% NA_character_), literature_signature(project_row, target_rows))
}

pubmed_quote_term <- function(term) {
  cleaned <- gsub("\"", "", trimws(as.character(term %||% "")), fixed = TRUE)
  sprintf("\"%s\"", cleaned)
}

literature_query_signature <- function(search_terms, field = PUBMED_TITLE_ABSTRACT_FIELD) {
  terms <- sort(unique(tolower(trimws(search_terms))))
  paste0(
    gsub("[^A-Za-z0-9]+", "-", field),
    ":",
    paste(gsub("[^a-z0-9]+", "-", terms), collapse = "+")
  )
}

build_disease_pubmed_query <- function(search_terms, field = PUBMED_TITLE_ABSTRACT_FIELD) {
  terms <- unique(trimws(search_terms))
  terms <- terms[nzchar(terms)]
  clauses <- sprintf("%s[%s]", vapply(terms, pubmed_quote_term, character(1)), field)
  if (length(clauses) == 1L) {
    return(clauses[[1]])
  }
  paste0("(", paste(clauses, collapse = " OR "), ")")
}

literature_corpus_definition <- function() {
  paste(
    "PubMed records linked by NCBI Gene to this target and matching the confirmed",
    "disease wording in title or abstract."
  )
}

as_disease_payload <- function(value) {
  if (is.list(value) && !is.null(value$status)) {
    return(value)
  }
  decode_disease_payload(value)
}

confirmed_disease_candidate <- function(project_row) {
  payload <- as_disease_payload(project_row$disease_resolution_payload[[1]])
  if (is.null(payload) || is.null(payload$candidates)) {
    return(NULL)
  }
  ontology_id <- project_disease_ontology_id(project_row)
  for (candidate in payload$candidates) {
    if (identical(as.character(candidate$id %||% ""), as.character(ontology_id))) {
      return(candidate)
    }
  }
  NULL
}

build_literature_disease_terms <- function(project_row) {
  canonical <- trimws(as.character(project_row$disease_name[[1]] %||% ""))
  user_label <- trimws(as.character(project_row$disease_label[[1]] %||% ""))
  terms <- character()
  if (nzchar(canonical)) {
    terms <- c(terms, canonical)
  }
  candidate <- confirmed_disease_candidate(project_row)
  matched <- as.character(candidate$matched_synonym %||% "")
  if (identical(candidate$match_type %||% "", "exact_synonym") &&
      nzchar(matched) &&
      !identical(normalize_disease_text(matched), normalize_disease_text(canonical))) {
    terms <- c(terms, matched)
  } else if (
    nzchar(user_label) &&
      !identical(normalize_disease_text(user_label), normalize_disease_text(canonical)) &&
      identical(candidate$match_type %||% "", "exact_synonym") &&
      identical(normalize_disease_text(user_label), normalize_disease_text(matched))
  ) {
    terms <- c(terms, user_label)
  }
  unique(terms[nzchar(terms)])
}

literature_disease_context <- function(project_row) {
  terms <- build_literature_disease_terms(project_row)
  list(
    ontology_id = project_disease_ontology_id(project_row),
    canonical_name = as.character(project_row$disease_name[[1]] %||% ""),
    user_label = as.character(project_row$disease_label[[1]] %||% ""),
    search_terms = terms,
    query_scope = PUBMED_TITLE_ABSTRACT_FIELD,
    disease_query = build_disease_pubmed_query(terms)
  )
}

read_api_cache_payload <- function(db_pool, source, cache_key) {
  if (is.null(db_pool)) {
    return(NULL)
  }
  cached <- tryCatch(cache_get(db_pool, source, cache_key), error = function(e) NULL)
  if (is.null(cached)) {
    return(NULL)
  }
  decoded <- tryCatch(decode_cached_json(cached$response[[1]]), error = function(e) NULL)
  if (is.null(decoded)) {
    return(NULL)
  }
  list(
    data = decoded,
    freshness = cached$freshness[[1]],
    retrieved_at = cached$retrieved_at[[1]]
  )
}

put_api_cache_payload <- function(db_pool, source, cache_key, payload, ttl_seconds) {
  if (is.null(db_pool)) {
    return(invisible(FALSE))
  }
  now <- Sys.time()
  tryCatch(
    cache_put(
      db_pool = db_pool,
      source = source,
      cache_key = cache_key,
      response = payload,
      http_status = 200L,
      retrieved_at = now,
      expires_at = now + ttl_seconds,
      status = "ok"
    ),
    error = function(e) FALSE
  )
}

records_from_cache <- function(rows) {
  if (is.null(rows) || length(rows) == 0) {
    return(empty_recent_records())
  }
  if (is.data.frame(rows)) {
    return(rows)
  }
  do.call(rbind, lapply(rows, function(row) {
    as.data.frame(row, stringsAsFactors = FALSE)
  }))
}

trend_from_cache <- function(rows) {
  if (is.null(rows) || length(rows) == 0) {
    return(empty_literature_trend())
  }
  if (is.data.frame(rows)) {
    return(rows)
  }
  do.call(rbind, lapply(rows, function(row) {
    data.frame(
      year = as.integer(row$year),
      record_count = as.integer(row$record_count %||% NA_integer_),
      is_partial_year = isTRUE(row$is_partial_year),
      status = as.character(row$status %||% "ok"),
      stringsAsFactors = FALSE
    )
  }))
}

zero_literature_trend <- function(end_year = as.integer(format(Sys.Date(), "%Y"))) {
  years <- seq.int(end_year - LITERATURE_TREND_YEARS + 1L, end_year)
  data.frame(
    year = years,
    record_count = 0L,
    is_partial_year = years == end_year,
    status = "ok",
    stringsAsFactors = FALSE
  )
}

resolve_literature_gene_id <- function(
  target_row,
  db_pool = NULL,
  uniprot_fetch = uniprot_get_accession,
  gene_fetch = fetch_ncbi_gene_summary
) {
  symbol <- target_workspace_label(target_row)
  accession <- blank_to_null(target_row$uniprot_accession[[1]])
  if (is.null(accession)) {
    return(list(
      ok = FALSE,
      status = "unavailable",
      message = sprintf("Literature mapping unavailable for %s.", symbol),
      gene_id = NA_character_
    ))
  }

  tw_then(uniprot_fetch(accession, db_pool = db_pool), function(uniprot_result) {
    if (!isTRUE(uniprot_result$ok)) {
      return(list(
        ok = FALSE,
        status = "error",
        message = sprintf("Literature retrieval unavailable for %s.", symbol),
        gene_id = NA_character_,
        error = uniprot_result$error
      ))
    }

    gene_ids <- uniprot_ncbi_gene_ids(uniprot_result$data)
    if (length(gene_ids) != 1L) {
      return(list(
        ok = FALSE,
        status = "unavailable",
        message = sprintf("Literature mapping unavailable for this target."),
        gene_id = NA_character_,
        reason = if (length(gene_ids) == 0L) "missing_geneid" else "ambiguous_geneid"
      ))
    }

    gene_id <- gene_ids[[1]]
    tw_then(gene_fetch(gene_id, db_pool = db_pool), function(summary_result) {
      if (!isTRUE(summary_result$ok)) {
        return(list(
          ok = FALSE,
          status = "unavailable",
          message = "Literature mapping unavailable for this target.",
          gene_id = gene_id,
          reason = "gene_verify_failed"
        ))
      }

      parsed <- parse_ncbi_gene_summary(summary_result$data, gene_id)
      verified <- verify_ncbi_gene(parsed, symbol)
      if (!isTRUE(verified$ok)) {
        return(list(
          ok = FALSE,
          status = "unavailable",
          message = "Literature mapping unavailable for this target.",
          gene_id = gene_id,
          reason = verified$reason
        ))
      }

      list(
        ok = TRUE,
        status = "ok",
        gene_id = gene_id,
        gene = parsed,
        uniprot_accession = accession,
        cache_status = summary_result$cache_status,
        retrieved_at = summary_result$retrieved_at
      )
    })
  })
}

history_intersection_term <- function(query_key, disease_query) {
  sprintf("#%s AND (%s)", query_key, disease_query)
}

retrieve_pubmed_corpus_live <- function(
  gene_id,
  disease_query,
  perform = httr2::req_perform
) {
  finish_with_recent <- function(history, term, counted, recent) {
    end_year <- as.integer(format(Sys.Date(), "%Y"))
    years <- seq.int(end_year - LITERATURE_TREND_YEARS + 1L, end_year)
    state <- list(trend_rows = list())
    for (year in years) {
      local_year <- year
      state <- tw_then(state, function(state) {
        tw_then(
          fetch_pubmed_esearch(
            term = term,
            webenv = history$webenv,
            rettype = "count",
            retmax = 0L,
            datetype = "pdat",
            mindate = as.character(local_year),
            maxdate = as.character(local_year),
            usehistory = TRUE,
            perform = perform
          ),
          function(year_result) {
            if (!isTRUE(year_result$ok)) {
              state$trend_rows[[length(state$trend_rows) + 1L]] <- data.frame(
                year = local_year,
                record_count = NA_integer_,
                is_partial_year = identical(local_year, end_year),
                status = "error",
                stringsAsFactors = FALSE
              )
              return(state)
            }
            year_count <- parse_esearch_count(year_result$data)
            if (!isTRUE(year_count$ok)) {
              state$trend_rows[[length(state$trend_rows) + 1L]] <- data.frame(
                year = local_year,
                record_count = NA_integer_,
                is_partial_year = identical(local_year, end_year),
                status = "error",
                stringsAsFactors = FALSE
              )
              return(state)
            }
            state$trend_rows[[length(state$trend_rows) + 1L]] <- data.frame(
              year = local_year,
              record_count = year_count$count,
              is_partial_year = identical(local_year, end_year),
              status = "ok",
              stringsAsFactors = FALSE
            )
            state
          }
        )
      })
    }
    tw_then(state, function(state) {
      list(
        ok = TRUE,
        total_count = counted$count,
        recent_records = recent,
        trend = do.call(rbind, state$trend_rows),
        error = NULL
      )
    })
  }

  tw_then(fetch_gene_pubmed_history(gene_id, perform = perform), function(elink) {
    if (!isTRUE(elink$ok)) {
      return(list(ok = FALSE, error = elink$error %||% "NCBI Gene to PubMed link failed."))
    }
    history <- parse_elink_history(elink$data)
    if (!isTRUE(history$ok)) {
      return(list(ok = FALSE, error = history$error))
    }
    if (isTRUE(history$empty)) {
      return(list(
        ok = TRUE,
        total_count = 0L,
        recent_records = empty_recent_records(),
        trend = zero_literature_trend(),
        error = NULL
      ))
    }

    term <- history_intersection_term(history$query_key, disease_query)
    tw_then(
      fetch_pubmed_esearch(
        term = term,
        webenv = history$webenv,
        rettype = "count",
        retmax = 0L,
        usehistory = TRUE,
        perform = perform
      ),
      function(count_result) {
        if (!isTRUE(count_result$ok)) {
          return(list(ok = FALSE, error = count_result$error %||% "ESearch count failed."))
        }
        counted <- parse_esearch_count(count_result$data)
        if (!isTRUE(counted$ok)) {
          return(list(ok = FALSE, error = counted$error))
        }

        recent_step <- if (counted$count > 0L) {
          tw_then(
            fetch_pubmed_esearch(
              term = term,
              webenv = history$webenv,
              retmax = min(LITERATURE_RECENT_N, PUBMED_UID_RETRIEVAL_LIMIT),
              sort = "pub date",
              usehistory = TRUE,
              perform = perform
            ),
            function(recent_result) {
              if (!isTRUE(recent_result$ok)) {
                return(list(ok = FALSE, error = recent_result$error %||% "Recent PubMed search failed."))
              }
              ids <- parse_esearch_ids(recent_result$data)
              if (!isTRUE(ids$ok)) {
                return(list(ok = FALSE, error = ids$error))
              }
              if (length(ids$ids) == 0) {
                return(list(ok = TRUE, records = empty_recent_records()))
              }
              tw_then(fetch_pubmed_esummary(ids$ids, perform = perform), function(summary_result) {
                if (!isTRUE(summary_result$ok)) {
                  return(list(ok = FALSE, error = summary_result$error %||% "PubMed ESummary failed."))
                }
                parsed_recent <- parse_pubmed_summaries(summary_result$data)
                if (!isTRUE(parsed_recent$ok)) {
                  return(list(ok = FALSE, error = parsed_recent$error))
                }
                list(ok = TRUE, records = parsed_recent$records)
              })
            }
          )
        } else {
          list(ok = TRUE, records = empty_recent_records())
        }

        tw_then(recent_step, function(recent_pack) {
          if (!isTRUE(recent_pack$ok)) {
            return(recent_pack)
          }
          finish_with_recent(history, term, counted, recent_pack$records)
        })
      }
    )
  })
}

cache_status_label <- function(status) {
  switch(
    as.character(status %||% ""),
    fresh = "cached",
    live = "live",
    stale = "stale",
    as.character(status %||% "unknown")
  )
}

build_literature_target_model <- function(
  target_row,
  gene_id,
  disease,
  corpus_live,
  cache_status,
  retrieved_at,
  database_last_update
) {
  symbol <- target_workspace_label(target_row)
  total <- as.integer(corpus_live$total_count)
  list(
    target = list(
      project_target_id = as.character(target_row$id[[1]]),
      symbol = symbol,
      uniprot_accession = as.character(target_row$uniprot_accession[[1]]),
      ncbi_gene_id = as.character(gene_id)
    ),
    disease = list(
      ontology_id = disease$ontology_id,
      canonical_name = disease$canonical_name,
      search_terms = disease$search_terms
    ),
    corpus = list(
      definition = literature_corpus_definition(),
      total_count = total,
      query_scope = disease$query_scope,
      disease_query = disease$disease_query,
      uid_retrieval_limit = PUBMED_UID_RETRIEVAL_LIMIT
    ),
    trend = corpus_live$trend,
    recent_records = corpus_live$recent_records,
    provenance = list(
      source = "NCBI PubMed",
      target_link_source = "NCBI Gene",
      ncbi_gene_id = as.character(gene_id),
      identifier_used = as.character(target_row$uniprot_accession[[1]]),
      retrieved_at = retrieved_at,
      cache_status = cache_status_label(cache_status),
      database_last_update = database_last_update,
      retrieval_method = "elink_gene_pubmed_neighbor_history+esearch_title_abstract",
      link_strategy = NCBI_GENE_PUBMED_LINK,
      rate_limit_mode = ncbi_app_identity()$rate_limit_mode,
      endpoint_family = NCBI_EUTILS,
      eutilities = "ELink, ESearch, ESummary, EInfo"
    )
  )
}

retrieve_target_literature <- function(
  target_row,
  disease,
  db_pool = NULL,
  perform = httr2::req_perform,
  uniprot_fetch = uniprot_get_accession,
  gene_fetch = fetch_ncbi_gene_summary,
  database_last_update = NA_character_
) {
  symbol <- target_workspace_label(target_row)
  gene_lookup <- gene_fetch
  tw_then(
    resolve_literature_gene_id(
      target_row,
      db_pool = db_pool,
      uniprot_fetch = uniprot_fetch,
      gene_fetch = function(gene_id, db_pool = NULL) {
        gene_lookup(gene_id, db_pool = db_pool, perform = perform)
      }
    ),
    function(mapped) {
  if (!isTRUE(mapped$ok)) {
    return(mapped)
  }

  gene_id <- mapped$gene_id
  query_sig <- literature_query_signature(disease$search_terms)
  corpus_key <- cache_key_pubmed_corpus(gene_id, query_sig)
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  start_year <- end_year - LITERATURE_TREND_YEARS + 1L
  trend_key <- cache_key_pubmed_trend(gene_id, query_sig, start_year, end_year)
  ttl <- get_app_config()$pubmed_ttl_hours * 3600L

  cached_corpus <- read_api_cache_payload(db_pool, PUBMED_SOURCE, corpus_key)
  cached_trend <- read_api_cache_payload(db_pool, PUBMED_SOURCE, trend_key)

  if (!is.null(cached_corpus) && identical(cached_corpus$freshness, "fresh") &&
      !is.null(cached_trend) && identical(cached_trend$freshness, "fresh")) {
    live_shaped <- list(
      ok = TRUE,
      total_count = as.integer(cached_corpus$data$total_count),
      recent_records = records_from_cache(cached_corpus$data$recent_records),
      trend = trend_from_cache(cached_trend$data$trend)
    )
    model <- build_literature_target_model(
      target_row,
      gene_id,
      disease,
      live_shaped,
      cache_status = "fresh",
      retrieved_at = cached_corpus$retrieved_at,
      database_last_update = database_last_update
    )
    status <- if (identical(live_shaped$total_count, 0L)) "empty" else "ok"
    return(list(status = status, message = NULL, literature = model))
  }

  tw_then(retrieve_pubmed_corpus_live(gene_id, disease$disease_query, perform = perform), function(live) {
  if (!isTRUE(live$ok)) {
    if (!is.null(cached_corpus) && !is.null(cached_trend)) {
      live_shaped <- list(
        ok = TRUE,
        total_count = as.integer(cached_corpus$data$total_count),
        recent_records = records_from_cache(cached_corpus$data$recent_records),
        trend = trend_from_cache(cached_trend$data$trend)
      )
      model <- build_literature_target_model(
        target_row,
        gene_id,
        disease,
        live_shaped,
        cache_status = "stale",
        retrieved_at = cached_corpus$retrieved_at,
        database_last_update = database_last_update
      )
      status <- if (identical(live_shaped$total_count, 0L)) "empty" else "ok"
      return(list(
        status = status,
        message = NULL,
        literature = model,
        warning = "Showing previously retrieved PubMed results because NCBI is unavailable."
      ))
    }
    return(list(
      ok = FALSE,
      status = "error",
      message = sprintf("Literature retrieval unavailable for %s.", symbol),
      error = live$error
    ))
  }

  put_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    corpus_key,
    list(
      total_count = live$total_count,
      recent_records = live$recent_records,
      disease_query = disease$disease_query
    ),
    ttl
  )
  put_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    trend_key,
    list(trend = live$trend, start_year = start_year, end_year = end_year),
    ttl
  )

  model <- build_literature_target_model(
    target_row,
    gene_id,
    disease,
    live,
    cache_status = "live",
    retrieved_at = Sys.time(),
    database_last_update = database_last_update
  )
  status <- if (identical(as.integer(live$total_count), 0L)) "empty" else "ok"
  list(status = status, message = NULL, literature = model)
  })
    }
  )
}

retrieve_project_literature <- function(
  project_row,
  target_rows,
  db_pool = NULL,
  perform = httr2::req_perform,
  uniprot_fetch = uniprot_get_accession,
  gene_fetch = fetch_ncbi_gene_summary,
  on_progress = NULL
) {
  gate <- literature_gate(project_row, target_rows)
  if (!isTRUE(gate$ok)) {
    return(list(status = gate$status, message = gate$message, literature = NULL))
  }

  disease <- literature_disease_context(project_row)
  if (length(disease$search_terms) == 0) {
    return(list(
      status = "blocked_disease",
      message = "Confirmed disease name is required for PubMed literature.",
      literature = NULL
    ))
  }

  tw_then(fetch_pubmed_einfo(db_pool = db_pool, perform = perform), function(einfo) {
  last_update <- NA_character_
  if (isTRUE(einfo$ok)) {
    parsed_info <- parse_pubmed_einfo(einfo$data)
    if (isTRUE(parsed_info$ok)) {
      last_update <- parsed_info$last_update
    }
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
          reason = "unconfirmed",
          message = sprintf("%s is not confirmed, so PubMed literature was not retrieved.", symbol)
        )
        return(state)
      }
      if (is.function(on_progress)) {
        on_progress(local_i, n, symbol)
      }
      tw_then(
        retrieve_target_literature(
          row,
          disease,
          db_pool = db_pool,
          perform = perform,
          uniprot_fetch = uniprot_fetch,
          gene_fetch = gene_fetch,
          database_last_update = last_update
        ),
        function(result) {
          if (identical(result$status, "unavailable")) {
            state$excluded[[length(state$excluded) + 1L]] <- list(
              project_target_id = as.character(row$id[[1]]),
              symbol = symbol,
              reason = result$reason %||% "mapping_unavailable",
              message = result$message %||% "Literature mapping unavailable for this target."
            )
            return(state)
          }
          if (identical(result$status, "error")) {
            state$failures[[length(state$failures) + 1L]] <- list(
              project_target_id = as.character(row$id[[1]]),
              symbol = symbol,
              message = result$message %||% sprintf("Literature retrieval unavailable for %s.", symbol)
            )
            return(state)
          }
          state$items[[length(state$items) + 1L]] <- result$literature
          state
        }
      )
    })
  }

  tw_then(state, function(state) {
    items <- state$items
    excluded <- state$excluded
    failures <- state$failures

  target_counts <- lapply(items, function(item) {
    data.frame(
      project_target_id = item$target$project_target_id,
      symbol = item$target$symbol,
      ncbi_gene_id = item$target$ncbi_gene_id,
      pubmed_record_count = as.integer(item$corpus$total_count),
      status = if (identical(as.integer(item$corpus$total_count), 0L)) "empty" else "ok",
      stringsAsFactors = FALSE
    )
  })
  counts_df <- if (length(target_counts) == 0) {
    data.frame(
      project_target_id = character(),
      symbol = character(),
      ncbi_gene_id = character(),
      pubmed_record_count = integer(),
      status = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, target_counts)
  }

  list(
    status = "ready",
    message = NULL,
    literature = list(
      disease = disease,
      targets = items,
      target_counts = counts_df,
      excluded_targets = excluded,
      failures = failures,
      provenance = list(
        source = "NCBI PubMed",
        target_link_source = "NCBI Gene",
        retrieved_at = Sys.time(),
        database_last_update = last_update,
        membership_note = NULL,
        query_scope = disease$query_scope,
        disease_query = disease$disease_query,
        cache_key_family = "pubmed:target-disease:<geneid>:<query-signature>",
        rate_limit_mode = ncbi_app_identity()$rate_limit_mode,
        endpoint_family = NCBI_EUTILS,
        eutilities = "ELink, ESearch, ESummary, EInfo",
        link_strategy = "gene_pubmed + neighbor_history",
        uid_retrieval_limit = PUBMED_UID_RETRIEVAL_LIMIT
      )
    )
  )
  })
  })
}
