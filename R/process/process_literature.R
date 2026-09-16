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
  key <- confirmed_scientific_target_set(target_rows)
  paste(
    project_disease_ontology_id(project_row) %||% "",
    key,
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

history_intersection_term <- function(query_key, disease_query) {
  sprintf("#%s AND (%s)", query_key, disease_query)
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
  years <- annual_literature_requested_years(end_year)
  data.frame(
    year = years,
    record_count = 0L,
    is_partial_year = years == as.integer(end_year)[[1]],
    status = "ok",
    stringsAsFactors = FALSE
  )
}

annual_literature_requested_years <- function(current_year = as.integer(format(Sys.Date(), "%Y"))) {
  current_year <- as.integer(current_year)[[1]]
  seq.int(current_year - (LITERATURE_TREND_YEARS - 1L), current_year)
}

annual_literature_result <- function(
  year_value,
  current_year,
  status,
  count = NA_integer_,
  error_safe_message = NULL,
  retrieved_at = Sys.time(),
  esearch_retmax = NA_integer_,
  esearch_id_count = NA_integer_,
  mindate = NULL,
  maxdate = NULL
) {
  year_value <- as.integer(year_value)[[1]]
  current_year <- as.integer(current_year)[[1]]
  status <- as.character(status[[1]] %||% "error")
  count <- if (identical(status, "ok")) as.integer(count)[[1]] else NA_integer_
  list(
    year = year_value,
    status = status,
    count = count,
    partial_year = identical(year_value, current_year),
    retrieved_at = retrieved_at,
    error_safe_message = error_safe_message,
    esearch_retmax = suppressWarnings(as.integer(esearch_retmax)[[1]]),
    esearch_id_count = suppressWarnings(as.integer(esearch_id_count)[[1]]),
    mindate = as.character(mindate %||% year_value),
    maxdate = as.character(maxdate %||% year_value)
  )
}

annual_results_from_trend <- function(trend) {
  if (is.null(trend) || nrow(trend) == 0) {
    return(list())
  }
  lapply(seq_len(nrow(trend)), function(i) {
    list(
      year = as.integer(trend$year[[i]]),
      status = as.character(trend$status[[i]] %||% "error"),
      count = as.integer(trend$record_count[[i]]),
      partial_year = isTRUE(trend$is_partial_year[[i]]),
      retrieved_at = Sys.time(),
      error_safe_message = NULL
    )
  })
}

annual_trend_from_results <- function(results) {
  if (length(results) == 0) {
    return(empty_literature_trend())
  }
  do.call(rbind, lapply(results, function(item) {
    data.frame(
      year = as.integer(item$year),
      record_count = as.integer(item$count %||% NA_integer_),
      is_partial_year = isTRUE(item$partial_year),
      status = as.character(item$status %||% "error"),
      stringsAsFactors = FALSE
    )
  }))
}

annual_series_invalid <- function() {
  list(
    ok = FALSE,
    error = "Annual publication activity could not be assembled correctly.",
    trend = empty_literature_trend()
  )
}

validate_annual_literature_series <- function(
  results,
  requested_years,
  current_year = max(as.integer(requested_years))
) {
  requested_years <- as.integer(requested_years)
  current_year <- as.integer(current_year)[[1]]
  if (length(requested_years) == 0L || anyNA(requested_years) || anyDuplicated(requested_years) > 0) {
    return(annual_series_invalid())
  }
  if (!is.list(results) || length(results) != length(requested_years)) {
    return(annual_series_invalid())
  }
  years <- vapply(results, function(item) {
    as.integer(item$year %||% NA_integer_)[[1]]
  }, integer(1))
  if (anyNA(years) || anyDuplicated(years) > 0) {
    return(annual_series_invalid())
  }
  if (!setequal(years, requested_years)) {
    return(annual_series_invalid())
  }
  for (item in results) {
    status <- as.character(item$status %||% "")
    count <- item$count
    year_value <- as.integer(item$year)[[1]]
    if (identical(status, "ok")) {
      count_n <- suppressWarnings(as.integer(count)[[1]])
      if (length(count_n) != 1L || is.na(count_n) || count_n < 0L) {
        return(annual_series_invalid())
      }
    } else {
      count_n <- suppressWarnings(as.integer(count %||% NA_integer_)[[1]])
      if (length(count_n) == 1L && !is.na(count_n)) {
        return(annual_series_invalid())
      }
    }
    expected_partial <- identical(year_value, current_year)
    if (!identical(isTRUE(item$partial_year), expected_partial)) {
      return(annual_series_invalid())
    }
  }
  ordered <- results[order(years)]
  list(
    ok = TRUE,
    error = NULL,
    trend = annual_trend_from_results(ordered)
  )
}

literature_year_esearch_query <- function(
  term,
  webenv,
  year,
  retmax = 0L,
  rettype = NULL,
  retstart = 0L,
  sort = NULL
) {
  year <- as.integer(year)[[1]]
  pubmed_esearch_query(
    term = term,
    webenv = webenv,
    rettype = rettype,
    retmax = retmax,
    retstart = retstart,
    sort = sort,
    datetype = "pdat",
    mindate = as.character(year),
    maxdate = as.character(year),
    usehistory = TRUE
  )
}

fetch_literature_year_esearch <- function(
  term,
  webenv,
  year,
  retmax = 0L,
  rettype = NULL,
  retstart = 0L,
  sort = NULL,
  perform = httr2::req_perform
) {
  fetch_pubmed_esearch(
    term = term,
    webenv = webenv,
    rettype = rettype,
    retmax = retmax,
    retstart = retstart,
    sort = sort,
    datetype = "pdat",
    mindate = as.character(as.integer(year)[[1]]),
    maxdate = as.character(as.integer(year)[[1]]),
    usehistory = TRUE,
    perform = perform
  )
}

fetch_pubmed_annual_count <- function(term, webenv, year_value, perform = httr2::req_perform) {
  fetch_literature_year_esearch(
    term = term,
    webenv = webenv,
    year = year_value,
    rettype = "count",
    retmax = 0L,
    perform = perform
  )
}

resolve_annual_count_task <- function(year_value, current_year, year_result) {
  year_value <- as.integer(year_value)[[1]]
  bound_year <- as.integer(year_result$bound_year %||% year_value)[[1]]
  if (!identical(bound_year, year_value)) {
    return(annual_literature_result(
      year_value,
      current_year,
      "error",
      error_safe_message = "Annual year identity was lost before result assembly.",
      mindate = as.character(year_value),
      maxdate = as.character(year_value)
    ))
  }
  payload_result <- year_result$payload %||% year_result
  if (!isTRUE(payload_result$ok)) {
    return(annual_literature_result(
      year_value,
      current_year,
      "error",
      error_safe_message = payload_result$error %||% "Annual PubMed count failed.",
      mindate = as.character(year_value),
      maxdate = as.character(year_value)
    ))
  }
  fields <- parse_esearch_count_fields(payload_result$data)
  counted <- parse_esearch_total_count(payload_result$data)
  if (!isTRUE(counted$ok)) {
    return(annual_literature_result(
      year_value,
      current_year,
      "error",
      error_safe_message = counted$error,
      esearch_retmax = fields$retmax,
      esearch_id_count = fields$id_count,
      mindate = as.character(year_value),
      maxdate = as.character(year_value)
    ))
  }
  annual_literature_result(
    year_value,
    current_year,
    "ok",
    count = counted$count,
    esearch_retmax = fields$retmax,
    esearch_id_count = fields$id_count,
    mindate = as.character(year_value),
    maxdate = as.character(year_value)
  )
}

make_annual_count_task <- function(term, webenv, year_value, current_year, perform) {
  year_value <- as.integer(year_value)[[1]]
  function(acc) {
    tw_then(
      fetch_pubmed_annual_count(term, webenv, year_value, perform = perform),
      function(year_result) {
        tagged <- list(bound_year = year_value, payload = year_result)
        row <- resolve_annual_count_task(year_value, current_year, tagged)
        acc[[as.character(year_value)]] <- row
        acc
      }
    )
  }
}

retrieve_annual_literature_counts <- function(
  term,
  webenv,
  requested_years,
  current_year,
  perform = httr2::req_perform
) {
  requested_years <- as.integer(requested_years)
  current_year <- as.integer(current_year)[[1]]
  state <- list()
  for (year_value in requested_years) {
    state <- tw_then(state, make_annual_count_task(term, webenv, year_value, current_year, perform))
  }
  tw_then(state, function(results) {
    ordered <- lapply(as.character(requested_years), function(key) results[[key]])
    validate_annual_literature_series(ordered, requested_years, current_year)
  })
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

LITERATURE_RUNTIME_MARKER <- "M12.5-literature-runtime-v3"

literature_annual_ui_model <- function(
  item,
  current_year = as.integer(format(Sys.Date(), "%Y"))
) {
  if (is.list(item) && is.list(item$annual_series) && !is.null(item$annual_series$ok)) {
    return(item$annual_series)
  }
  trend <- if (is.list(item) && !is.null(item$trend)) item$trend else item
  validate_annual_literature_series(
    annual_results_from_trend(trend),
    annual_literature_requested_years(current_year),
    current_year
  )
}

literature_annual_trend_for_ui <- function(item, current_year = as.integer(format(Sys.Date(), "%Y"))) {
  series <- literature_annual_ui_model(item, current_year)
  if (!isTRUE(series$ok)) {
    return(empty_literature_trend())
  }
  series$trend
}

literature_all_years_value <- function() {
  "all"
}

is_literature_all_years <- function(value) {
  identical(as.character(value %||% literature_all_years_value()), literature_all_years_value())
}

literature_year_choices <- function(trend, current_year = as.integer(format(Sys.Date(), "%Y"))) {
  choices <- c("All years" = literature_all_years_value())
  requested <- annual_literature_requested_years(current_year)
  assembled <- validate_annual_literature_series(
    annual_results_from_trend(trend),
    requested,
    current_year
  )
  if (!isTRUE(assembled$ok)) {
    return(choices)
  }
  ok <- assembled$trend[assembled$trend$status == "ok" & !is.na(assembled$trend$record_count), , drop = FALSE]
  if (nrow(ok) == 0) {
    return(choices)
  }
  ok <- ok[order(as.integer(ok$year)), , drop = FALSE]
  labels <- vapply(seq_len(nrow(ok)), function(i) {
    year <- as.integer(ok$year[[i]])
    if (isTRUE(ok$is_partial_year[[i]])) {
      sprintf("%s (partial)", year)
    } else {
      as.character(year)
    }
  }, character(1))
  c(choices, stats::setNames(as.character(as.integer(ok$year)), labels))
}

normalize_literature_year <- function(value, trend) {
  value <- as.character(value %||% literature_all_years_value())
  allowed <- unname(literature_year_choices(trend))
  if (value %in% allowed) {
    return(value)
  }
  literature_all_years_value()
}

literature_year_filter_label <- function(year) {
  if (is_literature_all_years(year)) {
    "No additional year filter"
  } else {
    as.character(as.integer(year))
  }
}

literature_year_pack <- function(
  year,
  records,
  total_count,
  disease_query,
  gene_id,
  cache_status = "live",
  message = NULL,
  status = "ok"
) {
  records <- if (is.null(records)) empty_recent_records() else records
  total_count <- as.integer(total_count %||% nrow(records))
  shown <- nrow(records)
  list(
    status = status,
    year = as.integer(year),
    total_count = total_count,
    records = records,
    shown = shown,
    truncated = shown < total_count,
    disease_query = disease_query,
    gene_id = as.character(gene_id),
    mindate = as.character(as.integer(year)),
    maxdate = as.character(as.integer(year)),
    datetype = "pdat",
    cache_status = cache_status,
    message = message
  )
}

year_records_from_cache <- function(cached) {
  if (is.null(cached) || is.null(cached$data)) {
    return(NULL)
  }
  data <- cached$data
  literature_year_pack(
    year = data$year,
    records = records_from_cache(data$records),
    total_count = data$total_count,
    disease_query = data$disease_query,
    gene_id = data$gene_id,
    cache_status = if (identical(cached$freshness, "fresh")) "cached" else "stale",
    status = if (identical(as.integer(data$total_count %||% 0L), 0L)) "empty" else "ok"
  )
}

retrieve_literature_year_records <- function(
  gene_id,
  disease,
  year,
  db_pool = NULL,
  perform = httr2::req_perform,
  already_have = 0L,
  page_size = LITERATURE_YEAR_PAGE_SIZE
) {
  year <- as.integer(year)[[1]]
  gene_id <- trimws(as.character(gene_id))
  disease_query <- disease$disease_query
  query_sig <- literature_query_signature(disease$search_terms)
  cache_key <- cache_key_pubmed_year_records(gene_id, query_sig, year)
  ttl <- get_app_config()$pubmed_ttl_hours * 3600L
  already_have <- max(0L, as.integer(already_have)[[1]])
  page_size <- max(1L, as.integer(page_size)[[1]])
  needed <- already_have + page_size

  cached <- read_api_cache_payload(db_pool, PUBMED_SOURCE, cache_key)
  cached_pack <- year_records_from_cache(cached)
  if (!is.null(cached_pack) && identical(cached$freshness, "fresh")) {
    if (nrow(cached_pack$records) >= min(needed, cached_pack$total_count) ||
        identical(cached_pack$total_count, 0L)) {
      cached_pack$records <- cached_pack$records[seq_len(min(needed, nrow(cached_pack$records))), , drop = FALSE]
      cached_pack$shown <- nrow(cached_pack$records)
      cached_pack$truncated <- cached_pack$shown < cached_pack$total_count
      return(list(ok = TRUE, literature = cached_pack, from_cache = TRUE))
    }
  }

  fetch_page <- function() {
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
          records = empty_recent_records()
        ))
      }
      term <- history_intersection_term(history$query_key, disease_query)
      tw_then(
        fetch_literature_year_esearch(
          term = term,
          webenv = history$webenv,
          year = year,
          rettype = "count",
          retmax = 0L,
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
          if (identical(counted$count, 0L) || already_have >= counted$count) {
            return(list(
              ok = TRUE,
              total_count = counted$count,
              records = empty_recent_records()
            ))
          }
          tw_then(
            fetch_literature_year_esearch(
              term = term,
              webenv = history$webenv,
              year = year,
              retmax = min(page_size, counted$count - already_have, PUBMED_UID_RETRIEVAL_LIMIT),
              retstart = already_have,
              sort = "pub date",
              perform = perform
            ),
            function(id_result) {
              if (!isTRUE(id_result$ok)) {
                return(list(ok = FALSE, error = id_result$error %||% "Year PubMed search failed."))
              }
              ids <- parse_esearch_ids(id_result$data)
              if (!isTRUE(ids$ok)) {
                return(list(ok = FALSE, error = ids$error))
              }
              if (length(ids$ids) == 0) {
                return(list(ok = TRUE, total_count = counted$count, records = empty_recent_records()))
              }
              tw_then(fetch_pubmed_esummary(ids$ids, perform = perform), function(summary_result) {
                if (!isTRUE(summary_result$ok)) {
                  return(list(ok = FALSE, error = summary_result$error %||% "PubMed ESummary failed."))
                }
                parsed <- parse_pubmed_summaries(summary_result$data)
                if (!isTRUE(parsed$ok)) {
                  return(list(ok = FALSE, error = parsed$error))
                }
                remaining <- max(0L, counted$count - already_have)
                records <- parsed$records
                keep <- min(nrow(records), page_size, remaining)
                if (keep < nrow(records)) {
                  records <- records[seq_len(keep), , drop = FALSE]
                }
                list(ok = TRUE, total_count = counted$count, records = records)
              })
            }
          )
        }
      )
    })
  }

  tw_then(fetch_page(), function(live) {
    if (!isTRUE(live$ok)) {
      if (!is.null(cached_pack)) {
        cached_pack$message <- "Showing previously retrieved records because NCBI is unavailable."
        return(list(ok = TRUE, literature = cached_pack, from_cache = TRUE))
      }
      return(list(
        ok = FALSE,
        status = "error",
        message = sprintf("Publications from %s could not be retrieved.", year),
        error = live$error
      ))
    }
    previous <- if (!is.null(cached_pack)) cached_pack$records else empty_recent_records()
    combined <- if (already_have > 0L && nrow(previous) > 0) {
      rbind(previous[seq_len(min(already_have, nrow(previous))), , drop = FALSE], live$records)
    } else {
      live$records
    }
    combined <- combined[!duplicated(combined$pmid), , drop = FALSE]
    pack <- literature_year_pack(
      year = year,
      records = combined,
      total_count = live$total_count,
      disease_query = disease_query,
      gene_id = gene_id,
      cache_status = "live",
      status = if (identical(as.integer(live$total_count), 0L)) "empty" else "ok"
    )
    put_api_cache_payload(
      db_pool,
      PUBMED_SOURCE,
      cache_key,
      list(
        year = year,
        total_count = pack$total_count,
        records = pack$records,
        disease_query = disease_query,
        gene_id = gene_id
      ),
      ttl
    )
    list(ok = TRUE, literature = pack, from_cache = FALSE)
  })
}

retrieve_pubmed_corpus_live <- function(
  gene_id,
  disease_query,
  perform = httr2::req_perform
) {
  finish_with_recent <- function(history, term, counted, recent) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    requested_years <- annual_literature_requested_years(current_year)
    tw_then(
      retrieve_annual_literature_counts(
        term = term,
        webenv = history$webenv,
        requested_years = requested_years,
        current_year = current_year,
        perform = perform
      ),
      function(assembled) {
        list(
          ok = TRUE,
          total_count = counted$count,
          recent_records = recent,
          trend = if (isTRUE(assembled$ok)) assembled$trend else empty_literature_trend(),
          trend_ok = isTRUE(assembled$ok),
          trend_error = assembled$error,
          annual_series = assembled,
          error = NULL
        )
      }
    )
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
        trend_ok = TRUE,
        trend_error = NULL,
        annual_series = validate_annual_literature_series(
          annual_results_from_trend(zero_literature_trend()),
          annual_literature_requested_years(),
          as.integer(format(Sys.Date(), "%Y"))
        ),
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
  series <- if (is.list(corpus_live$annual_series) && !is.null(corpus_live$annual_series$ok)) {
    corpus_live$annual_series
  } else {
    literature_annual_ui_model(list(trend = corpus_live$trend))
  }
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
    trend = if (isTRUE(series$ok)) series$trend else empty_literature_trend(),
    trend_ok = isTRUE(series$ok),
    trend_error = series$error,
    annual_series = series,
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

forced_literature_refresh_session <- function(force, current_result, current_signature) {
  if (!isTRUE(force)) {
    return(list(
      drop_session_result = FALSE,
      result = current_result,
      signature = current_signature
    ))
  }
  list(
    drop_session_result = TRUE,
    result = NULL,
    signature = NA_character_
  )
}

log_annual_trend_source <- function(source, series) {
  pairs <- "unavailable"
  trend <- NULL
  if (is.list(series) && isTRUE(series$ok) && is.data.frame(series$trend)) {
    trend <- series$trend
  } else if (is.data.frame(series)) {
    trend <- series
  }
  if (!is.null(trend) && nrow(trend) > 0 && "year" %in% names(trend) && "record_count" %in% names(trend)) {
    pairs <- paste(sprintf("%s=%s", trend$year, trend$record_count), collapse = " ")
  }
  tw_log("annual_trend_source", source = source, years = pairs)
  message(sprintf("ANNUAL TREND SOURCE: %s", source))
  message(pairs)
  invisible(TRUE)
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
  requested_years <- annual_literature_requested_years(end_year)
  cached_annual <- if (!is.null(cached_trend)) {
    validate_annual_literature_series(
      annual_results_from_trend(trend_from_cache(cached_trend$data$trend)),
      requested_years,
      end_year
    )
  } else {
    annual_series_invalid()
  }

  if (!is.null(cached_corpus) && identical(cached_corpus$freshness, "fresh") &&
      !is.null(cached_trend) && identical(cached_trend$freshness, "fresh") &&
      isTRUE(cached_annual$ok)) {
    live_shaped <- list(
      ok = TRUE,
      total_count = as.integer(cached_corpus$data$total_count),
      recent_records = records_from_cache(cached_corpus$data$recent_records),
      trend = cached_annual$trend,
      trend_ok = TRUE,
      trend_error = NULL,
      annual_series = cached_annual
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
    log_annual_trend_source("cache-v3", cached_annual)
    return(list(status = status, message = NULL, literature = model))
  }

  tw_then(retrieve_pubmed_corpus_live(gene_id, disease$disease_query, perform = perform), function(live) {
  if (!isTRUE(live$ok)) {
    if (!is.null(cached_corpus) && isTRUE(cached_annual$ok)) {
      live_shaped <- list(
        ok = TRUE,
        total_count = as.integer(cached_corpus$data$total_count),
        recent_records = records_from_cache(cached_corpus$data$recent_records),
        trend = cached_annual$trend,
        trend_ok = TRUE,
        trend_error = NULL,
        annual_series = cached_annual
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
  if (isTRUE(live$trend_ok)) {
    put_api_cache_payload(
      db_pool,
      PUBMED_SOURCE,
      trend_key,
      list(
        trend = live$trend,
        start_year = start_year,
        end_year = end_year,
        trend_ok = TRUE,
        cache_namespace = PUBMED_TREND_CACHE_NAMESPACE
      ),
      ttl
    )
  }

  log_annual_trend_source("live-api", live$annual_series %||% live$trend)

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
          input_text = as.character(row$input_text[[1]] %||% ""),
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
