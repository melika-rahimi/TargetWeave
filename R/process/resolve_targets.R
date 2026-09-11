classify_target_input <- function(text) {
  if (!has_display_text(text)) {
    return("unknown")
  }

  value <- trimws(as.character(text))
  upper <- toupper(value)

  if (grepl("^ENSG[0-9]{11}(\\.[0-9]+)?$", upper)) {
    return("ensembl_gene")
  }

  if (grepl("^[OPQ][0-9][A-Z0-9]{3}[0-9](-[0-9]+)?$", upper) ||
      grepl("^[A-NR-Z][0-9][A-Z][A-Z0-9]{2}[0-9](-[0-9]+)?$", upper)) {
    return("uniprot_accession")
  }

  if (grepl("^[A-Za-z][A-Za-z0-9-]*$", value) && nchar(value) <= 20) {
    return("gene_symbol")
  }

  "unknown"
}

format_location <- function(seq_region, start, end, strand) {
  if (!has_display_text(seq_region)) {
    return(NA_character_)
  }

  coords <- ""
  if (is_present_scalar(start) && is_present_scalar(end)) {
    coords <- sprintf(":%s-%s", start, end)
  }

  strand_label <- ""
  if (is_present_scalar(strand)) {
    strand_label <- if (as.integer(strand) < 0) " (-)" else " (+)"
  }

  sprintf("chr%s%s%s", seq_region, coords, strand_label)
}

source_report <- function(result) {
  list(
    ok = isTRUE(result$ok),
    status = result$status %||% NA_integer_,
    error = result$error,
    retrieved_at = as.character(result$retrieved_at),
    from_cache = isTRUE(result$from_cache),
    cache_status = result$cache_status %||% NA_character_,
    url = result$url %||% NA_character_
  )
}

empty_source_report <- function(error) {
  list(
    ok = FALSE,
    status = NA_integer_,
    error = error,
    retrieved_at = as.character(Sys.time()),
    from_cache = FALSE,
    cache_status = NA_character_,
    url = NA_character_
  )
}

new_identity_candidate <- function(
  input_text,
  display_symbol,
  name,
  organism,
  ensembl_gene_id,
  seq_region,
  start,
  end,
  strand,
  biotype,
  uniprot_accession,
  uniprot_reviewed,
  hgnc_id,
  cross_check,
  warning = NULL
) {
  list(
    candidate_key = paste(
      uniprot_accession %||% "NA",
      ensembl_gene_id %||% "NA",
      sep = "|"
    ),
    input_text = input_text,
    display_symbol = display_symbol %||% NA_character_,
    name = name %||% NA_character_,
    organism = organism %||% "Homo sapiens",
    ensembl_gene_id = ensembl_gene_id %||% NA_character_,
    seq_region = seq_region %||% NA_character_,
    start = start %||% NA_integer_,
    end = end %||% NA_integer_,
    strand = strand %||% NA_integer_,
    location_label = format_location(seq_region, start, end, strand),
    biotype = biotype %||% NA_character_,
    uniprot_accession = uniprot_accession %||% NA_character_,
    uniprot_reviewed = isTRUE(uniprot_reviewed),
    hgnc_id = hgnc_id %||% NA_character_,
    cross_check = cross_check,
    warning = warning,
    match_type = NA_character_,
    match_reason = NA_character_,
    matched_alias = NA_character_,
    match_rank = NA_integer_
  )
}

same_gene_symbol <- function(a, b) {
  has_display_text(a) &&
    has_display_text(b) &&
    identical(toupper(trimws(as.character(a))), toupper(trimws(as.character(b))))
}

matched_alias_from_source <- function(input_text, aliases) {
  if (is.null(aliases) || length(aliases) == 0) {
    return(NA_character_)
  }

  for (alias in aliases) {
    if (same_gene_symbol(input_text, alias)) {
      return(as.character(alias))
    }
  }

  NA_character_
}

classify_candidate_match <- function(
  input_text,
  input_class,
  display_symbol,
  ensembl_gene_id,
  uniprot_accession,
  gene_synonyms = character()
) {
  if (identical(input_class, "ensembl_gene")) {
    input_id <- canonical_ensembl_gene_id(input_text)
    cand_id <- canonical_ensembl_gene_id(ensembl_gene_id)
    if (has_display_text(input_id) && identical(input_id, cand_id)) {
      return(list(
        match_type = "identifier_exact",
        match_rank = 1L,
        match_reason = "Exact Ensembl gene ID match",
        matched_alias = NA_character_
      ))
    }
  }

  if (identical(input_class, "uniprot_accession") &&
      same_gene_symbol(input_text, uniprot_accession)) {
    return(list(
      match_type = "identifier_exact",
      match_rank = 1L,
      match_reason = "Exact UniProt accession match",
      matched_alias = NA_character_
    ))
  }

  if (same_gene_symbol(input_text, display_symbol)) {
    return(list(
      match_type = "exact_current_symbol",
      match_rank = 2L,
      match_reason = "Exact current gene symbol match",
      matched_alias = NA_character_
    ))
  }

  alias <- matched_alias_from_source(input_text, gene_synonyms)
  if (has_display_text(alias) && has_display_text(display_symbol)) {
    return(list(
      match_type = "synonym_or_alias",
      match_rank = 3L,
      match_reason = sprintf(
        "%s is listed as a synonym/alias for %s",
        alias,
        display_symbol
      ),
      matched_alias = alias
    ))
  }

  if (has_display_text(alias)) {
    return(list(
      match_type = "synonym_or_alias",
      match_rank = 3L,
      match_reason = sprintf("%s is listed as a synonym/alias", alias),
      matched_alias = alias
    ))
  }

  list(
    match_type = "other_valid_match",
    match_rank = 4L,
    match_reason = "Returned by UniProt or Ensembl search without a documented symbol or alias match for this input.",
    matched_alias = NA_character_
  )
}

apply_candidate_match <- function(
  candidate,
  input_text,
  input_class,
  gene_synonyms = character()
) {
  match <- classify_candidate_match(
    input_text = input_text,
    input_class = input_class,
    display_symbol = candidate$display_symbol,
    ensembl_gene_id = candidate$ensembl_gene_id,
    uniprot_accession = candidate$uniprot_accession,
    gene_synonyms = gene_synonyms
  )
  candidate$match_type <- match$match_type
  candidate$match_rank <- match$match_rank
  candidate$match_reason <- match$match_reason
  candidate$matched_alias <- match$matched_alias
  candidate
}

candidate_match_rank <- function(candidate) {
  rank <- candidate$match_rank
  if (is.null(rank) || length(rank) != 1L) {
    return(4L)
  }

  if (is.character(rank)) {
    if (!has_display_text(rank) || identical(rank, "NA")) {
      return(4L)
    }
    rank <- suppressWarnings(as.integer(rank))
  }

  if (!is.numeric(rank) || length(rank) != 1L || is.na(rank) || !is.finite(rank)) {
    return(4L)
  }

  as.integer(rank)
}

collect_ensembl_genes <- function(ids, db_pool, existing = list(), perform = httr2::req_perform) {
  state <- list(
    genes = existing,
    have = if (length(existing) > 0) {
      vapply(existing, function(g) g$ensembl_gene_id, character(1))
    } else {
      character()
    }
  )

  reduced <- Reduce(
    function(st, id) {
      tw_then(st, function(state) {
        if (id %in% state$have) {
          return(state)
        }
        tw_then(ensembl_lookup_id(id, db_pool = db_pool, perform = perform), function(result) {
          if (!isTRUE(result$ok)) {
            return(state)
          }
          parsed <- parse_ensembl_gene(result$data)
          if (!is.null(parsed) && has_display_text(parsed$ensembl_gene_id)) {
            parsed$lookup_report <- source_report(result)
            state$genes[[length(state$genes) + 1]] <- parsed
            state$have <- c(state$have, parsed$ensembl_gene_id)
          }
          state
        })
      })
    },
    canonical_ensembl_gene_ids(ids),
    init = state
  )
  tw_then(reduced, function(state) state$genes)
}

fetch_uniprot_for_input <- function(input_text, input_class, db_pool, perform = httr2::req_perform) {
  finish <- function(result) {
    parsed <- if (isTRUE(result$ok)) parse_uniprot_search(result$data) else NULL
    list(result = result, entries = parsed$entries %||% list(), parsed = parsed)
  }

  if (identical(input_class, "uniprot_accession")) {
    return(tw_then(uniprot_get_accession(input_text, db_pool = db_pool, perform = perform), finish))
  }

  if (identical(input_class, "ensembl_gene")) {
    return(tw_then(uniprot_search_ensembl_xref(input_text, db_pool = db_pool, perform = perform), finish))
  }

  symbol <- input_text
  tw_then(
    uniprot_search_symbol(symbol, db_pool = db_pool, reviewed = TRUE, perform = perform),
    function(result) {
      parsed <- if (isTRUE(result$ok)) parse_uniprot_search(result$data) else NULL
      entries <- parsed$entries %||% list()
      if (isTRUE(result$ok) && length(entries) == 0) {
        return(tw_then(
          uniprot_search_symbol(symbol, db_pool = db_pool, reviewed = FALSE, perform = perform),
          function(fallback) {
            fallback_parsed <- if (isTRUE(fallback$ok)) parse_uniprot_search(fallback$data) else NULL
            list(
              result = fallback,
              entries = fallback_parsed$entries %||% list(),
              parsed = fallback_parsed,
              reviewed_empty = TRUE
            )
          }
        ))
      }
      list(result = result, entries = entries, parsed = parsed)
    }
  )
}

fetch_ensembl_for_input <- function(
  input_text,
  input_class,
  db_pool,
  uniprot_entries,
  perform = httr2::req_perform
) {
  if (identical(input_class, "ensembl_gene")) {
    return(tw_then(ensembl_lookup_id(input_text, db_pool = db_pool, perform = perform), function(result) {
      reports <- list(lookup = source_report(result))
      genes <- list()
      if (isTRUE(result$ok)) {
        parsed <- parse_ensembl_gene(result$data)
        if (!is.null(parsed)) {
          parsed$lookup_report <- source_report(result)
          genes <- list(parsed)
        }
      }
      list(genes = genes, reports = reports, result = result)
    }))
  }

  if (identical(input_class, "uniprot_accession")) {
    xref_ids <- unique(unlist(lapply(uniprot_entries, function(e) e$ensembl_gene_ids)))
    return(tw_then(
      collect_ensembl_genes(xref_ids, db_pool, perform = perform),
      function(genes) {
        result <- if (length(genes) > 0) {
          list(
            ok = TRUE,
            status = 200,
            error = NULL,
            from_cache = FALSE,
            cache_status = "live",
            url = NA_character_,
            retrieved_at = Sys.time()
          )
        } else {
          list(
            ok = FALSE,
            status = NA_integer_,
            error = "No Ensembl gene xref on the UniProt record.",
            from_cache = FALSE,
            cache_status = NA_character_,
            url = NA_character_,
            retrieved_at = Sys.time()
          )
        }
        list(genes = genes, reports = list(xref_lookup = source_report(result)), result = result)
      }
    ))
  }

  tw_then(ensembl_lookup_symbol(input_text, db_pool = db_pool, perform = perform), function(lookup) {
    reports <- list(lookup = source_report(lookup))
    genes <- list()
    if (isTRUE(lookup$ok)) {
      parsed <- parse_ensembl_gene(lookup$data)
      if (!is.null(parsed)) {
        parsed$lookup_report <- source_report(lookup)
        genes <- list(parsed)
      }
    }

    continue_missing <- function(genes, reports) {
      xref_from_uniprot <- canonical_ensembl_gene_ids(
        unique(unlist(lapply(uniprot_entries, function(e) e$ensembl_gene_ids)))
      )
      have_ids <- if (length(genes) == 0) {
        character()
      } else {
        canonical_ensembl_gene_ids(
          vapply(genes, function(g) g$ensembl_gene_id, character(1))
        )
      }
      missing <- setdiff(xref_from_uniprot, have_ids)
      tw_then(
        collect_ensembl_genes(missing, db_pool, existing = genes, perform = perform),
        function(genes) {
          list(genes = genes, reports = reports, result = lookup)
        }
      )
    }

    if (length(genes) == 0) {
      return(tw_then(ensembl_xrefs_symbol(input_text, db_pool = db_pool, perform = perform), function(xrefs) {
        reports$xrefs <- source_report(xrefs)
        xref_ids <- if (isTRUE(xrefs$ok)) parse_ensembl_xrefs(xrefs$data) else character()
        tw_then(
          collect_ensembl_genes(xref_ids, db_pool, perform = perform),
          function(genes) continue_missing(genes, reports)
        )
      }))
    }
    continue_missing(genes, reports)
  })
}

build_candidates <- function(
  input_text,
  uniprot_entries,
  ensembl_genes,
  uniprot_ok,
  ensembl_ok,
  input_class = "gene_symbol"
) {
  candidates <- list()
  used_uniprot <- character()
  used_ensembl <- character()

  finish <- function(candidate, gene_synonyms = character()) {
    apply_candidate_match(
      candidate,
      input_text = input_text,
      input_class = input_class,
      gene_synonyms = gene_synonyms
    )
  }

  ensembl_by_id <- list()
  for (gene in ensembl_genes) {
    key <- canonical_ensembl_gene_id(gene$ensembl_gene_id)
    if (has_display_text(key)) {
      ensembl_by_id[[key]] <- gene
    }
  }

  for (entry in uniprot_entries) {
    xref_ids <- canonical_ensembl_gene_ids(entry$ensembl_gene_ids)
    if (length(xref_ids) == 0) {
      candidates[[length(candidates) + 1]] <- finish(
        new_identity_candidate(
          input_text = input_text,
          display_symbol = entry$display_symbol,
          name = entry$protein_name,
          organism = entry$organism,
          ensembl_gene_id = NA_character_,
          seq_region = NA_character_,
          start = NA_integer_,
          end = NA_integer_,
          strand = NA_integer_,
          biotype = NA_character_,
          uniprot_accession = entry$accession,
          uniprot_reviewed = entry$reviewed,
          hgnc_id = entry$hgnc_id,
          cross_check = "partial_uniprot",
          warning = "UniProt returned this protein without an Ensembl gene xref."
        ),
        entry$gene_synonyms %||% character()
      )
      used_uniprot <- c(used_uniprot, entry$accession)
      next
    }

    for (ensg in xref_ids) {
      gene <- ensembl_by_id[[ensg]]
      if (is.null(gene)) {
        candidates[[length(candidates) + 1]] <- finish(
          new_identity_candidate(
            input_text = input_text,
            display_symbol = entry$display_symbol,
            name = entry$protein_name,
            organism = entry$organism,
            ensembl_gene_id = ensg,
            seq_region = NA_character_,
            start = NA_integer_,
            end = NA_integer_,
            strand = NA_integer_,
            biotype = NA_character_,
            uniprot_accession = entry$accession,
            uniprot_reviewed = entry$reviewed,
            hgnc_id = entry$hgnc_id,
            cross_check = if (isTRUE(ensembl_ok)) "mismatch" else "partial_uniprot",
            warning = if (isTRUE(ensembl_ok)) {
              "UniProt lists this Ensembl ID, but Ensembl lookup did not return a matching human gene."
            } else {
              "Ensembl was unavailable. This identity is not cross-validated."
            }
          ),
          entry$gene_synonyms %||% character()
        )
      } else {
        symbol_mismatch <- has_display_text(entry$display_symbol) &&
          has_display_text(gene$display_symbol) &&
          !identical(toupper(entry$display_symbol), toupper(gene$display_symbol))

        warning <- NULL
        cross_check <- "matched"
        if (!isTRUE(uniprot_ok) || !isTRUE(ensembl_ok)) {
          cross_check <- "partial"
          warning <- "One source was unavailable or stale. Review before confirming."
        }
        if (symbol_mismatch) {
          cross_check <- "mismatch"
          warning <- paste(
            "UniProt gene name and Ensembl display name differ.",
            "They were not merged silently."
          )
        }

        candidates[[length(candidates) + 1]] <- finish(
          new_identity_candidate(
            input_text = input_text,
            display_symbol = gene$display_symbol %||% entry$display_symbol,
            name = entry$protein_name %||% gene$description,
            organism = "Homo sapiens",
            ensembl_gene_id = gene$ensembl_gene_id,
            seq_region = gene$seq_region,
            start = gene$start,
            end = gene$end,
            strand = gene$strand,
            biotype = gene$biotype,
            uniprot_accession = entry$accession,
            uniprot_reviewed = entry$reviewed,
            hgnc_id = entry$hgnc_id %||% gene$hgnc_id,
            cross_check = cross_check,
            warning = warning
          ),
          entry$gene_synonyms %||% character()
        )
        used_ensembl <- c(used_ensembl, canonical_ensembl_gene_id(gene$ensembl_gene_id))
      }

      used_uniprot <- c(used_uniprot, entry$accession)
    }
  }

  for (gene in ensembl_genes) {
    gene_key <- canonical_ensembl_gene_id(gene$ensembl_gene_id)
    if (has_display_text(gene_key) && gene_key %in% used_ensembl) {
      next
    }

    candidates[[length(candidates) + 1]] <- finish(
      new_identity_candidate(
        input_text = input_text,
        display_symbol = gene$display_symbol,
        name = gene$description,
        organism = "Homo sapiens",
        ensembl_gene_id = gene$ensembl_gene_id,
        seq_region = gene$seq_region,
        start = gene$start,
        end = gene$end,
        strand = gene$strand,
        biotype = gene$biotype,
        uniprot_accession = NA_character_,
        uniprot_reviewed = FALSE,
        hgnc_id = gene$hgnc_id,
        cross_check = if (isTRUE(uniprot_ok)) "partial_ensembl" else "partial_ensembl",
        warning = "Ensembl returned this gene without a matching UniProt accession."
      ),
      character()
    )
  }

  reviewed_first <- function(candidate) {
    if (isTRUE(candidate$uniprot_reviewed)) 0L else 1L
  }

  coding_first <- function(candidate) {
    if (identical(candidate$biotype, "protein_coding")) 0L else 1L
  }

  if (length(candidates) > 1) {
    order_idx <- order(
      vapply(candidates, candidate_match_rank, integer(1)),
      vapply(candidates, coding_first, integer(1)),
      vapply(candidates, reviewed_first, integer(1)),
      vapply(candidates, function(c) c$display_symbol %||% "", character(1))
    )
    candidates <- candidates[order_idx]
  }

  candidates
}

lookup_status_from_candidates <- function(candidates, uniprot_ok, ensembl_ok) {
  if (length(candidates) == 0) {
    return("failed")
  }

  ranks <- vapply(candidates, candidate_match_rank, integer(1))
  best <- min(ranks)
  if (sum(ranks == best) > 1L) {
    return("ambiguous")
  }

  "unresolved"
}

normalize_resolution_result <- function(
  input_text,
  input_class,
  uniprot_fetch,
  ensembl_fetch
) {
  uniprot_ok <- isTRUE(uniprot_fetch$result$ok)
  ensembl_ok <- isTRUE(ensembl_fetch$result$ok) || length(ensembl_fetch$genes) > 0

  if (!is.null(uniprot_fetch$parsed) && !isTRUE(uniprot_fetch$parsed$ok) && uniprot_ok) {
    uniprot_ok <- FALSE
  }

  candidates <- build_candidates(
    input_text = input_text,
    uniprot_entries = uniprot_fetch$entries,
    ensembl_genes = ensembl_fetch$genes,
    uniprot_ok = uniprot_ok,
    ensembl_ok = isTRUE(ensembl_fetch$result$ok) || length(ensembl_fetch$genes) > 0,
    input_class = input_class
  )

  status <- lookup_status_from_candidates(
    candidates,
    uniprot_ok,
    ensembl_ok
  )

  list(
    input_text = input_text,
    input_class = input_class,
    status = status,
    candidates = candidates,
    source_reports = list(
      uniprot = source_report(uniprot_fetch$result),
      ensembl = source_report(ensembl_fetch$result)
    )
  )
}

# Live-test helper: availability vs scientific cross-check.
# semantic is NA when a required source did not return a usable record.
# Do not treat source outages as identifier mismatches.
identity_live_assessment <- function(result) {
  uni <- result$source_reports$uniprot
  ens <- result$source_reports$ensembl
  primary <- if (length(result$candidates) >= 1) result$candidates[[1]] else NULL
  recovered_ensembl <- any(vapply(
    result$candidates %||% list(),
    function(c) identical(c$cross_check, "matched"),
    logical(1)
  ))
  uni_ok <- isTRUE(uni$ok)
  ens_ok <- isTRUE(ens$ok) || isTRUE(recovered_ensembl)
  unavailable <- c(
    if (!uni_ok) "UniProt" else NULL,
    if (!ens_ok) "Ensembl" else NULL
  )
  availability <- if (length(unavailable) == 0) {
    "sources_available"
  } else {
    "source_unavailable"
  }
  semantic <- NA_character_
  if (identical(availability, "sources_available")) {
    semantic <- if (!is.null(primary) && identical(primary$cross_check, "matched")) {
      "matched"
    } else {
      "mismatch"
    }
  }
  xref <- canonical_ensembl_gene_id(primary$ensembl_gene_id)
  ensembl_ensg <- if (ens_ok && identical(primary$cross_check, "matched")) xref else NA_character_
  list(
    availability = availability,
    unavailable_sources = unavailable,
    semantic = semantic,
    uniprot_accession = primary$uniprot_accession %||% NA_character_,
    uniprot_ensembl_gene_id = xref,
    canonical_ensg = xref,
    ensembl_ensg = ensembl_ensg,
    cross_check = primary$cross_check %||% NA_character_,
    n_candidates = length(result$candidates),
    uniprot_http_ok = isTRUE(uni$ok),
    ensembl_http_ok = isTRUE(ens$ok),
    uniprot_status = uni$status %||% NA_integer_,
    ensembl_status = ens$status %||% NA_integer_,
    uniprot_error = uni$error %||% NA_character_,
    ensembl_error = ens$error %||% NA_character_
  )
}

resolve_target_identity <- function(input_text, db_pool = NULL, perform = httr2::req_perform) {
  if (!has_display_text(input_text)) {
    return(list(
      input_text = if (is.null(input_text)) "" else as.character(input_text)[1],
      input_class = "unknown",
      status = "failed",
      candidates = list(),
      source_reports = list(
        uniprot = empty_source_report("Empty target string."),
        ensembl = empty_source_report("Empty target string.")
      )
    ))
  }

  input_text <- trimws(as.character(input_text))
  input_class <- classify_target_input(input_text)

  failed_uniprot <- function(e) {
    list(
      result = new_api_result(
        ok = FALSE,
        error = conditionMessage(e),
        source = "uniprot"
      ),
      entries = list()
    )
  }
  failed_ensembl <- function(e) {
    list(
      result = new_api_result(
        ok = FALSE,
        error = conditionMessage(e),
        source = "ensembl"
      ),
      genes = list(),
      reports = list()
    )
  }

  uniprot_step <- tryCatch(
    fetch_uniprot_for_input(input_text, input_class, db_pool, perform = perform),
    error = failed_uniprot
  )

  finish <- function(uniprot_fetch) {
    ensembl_step <- tryCatch(
      fetch_ensembl_for_input(
        input_text,
        input_class,
        db_pool,
        uniprot_fetch$entries %||% list(),
        perform = perform
      ),
      error = failed_ensembl
    )
    tw_then(
      ensembl_step,
      function(ensembl_fetch) {
        normalize_resolution_result(
          input_text = input_text,
          input_class = input_class,
          uniprot_fetch = uniprot_fetch,
          ensembl_fetch = ensembl_fetch
        )
      },
      function(e) {
        normalize_resolution_result(
          input_text = input_text,
          input_class = input_class,
          uniprot_fetch = uniprot_fetch,
          ensembl_fetch = failed_ensembl(e)
        )
      }
    )
  }

  tw_then(
    uniprot_step,
    finish,
    function(e) finish(failed_uniprot(e))
  )
}

candidate_is_confirmable <- function(candidate) {
  has_display_text(candidate$ensembl_gene_id) &&
    has_display_text(candidate$uniprot_accession)
}
