reviewed_status_label <- function(flag) {
  if (isTRUE(flag)) {
    return("Reviewed Swiss-Prot")
  }
  if (identical(flag, FALSE)) {
    return("Not reviewed Swiss-Prot")
  }
  "Review status not provided"
}

target_is_confirmed <- function(row) {
  !is.null(row) && identical(as.character(row$resolution_status[[1]]), "confirmed")
}

overview_fetch_signature <- function(row) {
  if (!target_is_confirmed(row)) {
    return(NA_character_)
  }

  paste(
    as.character(row$id[[1]]),
    toupper(trimws(as.character(row$uniprot_accession[[1]] %||% ""))),
    canonical_ensembl_gene_id(row$ensembl_gene_id[[1]]) %||% "",
    sep = "|"
  )
}

should_retrieve_overview <- function(
  panel_active,
  row,
  last_signature = NA_character_,
  force = FALSE
) {
  if (!isTRUE(panel_active)) {
    return(FALSE)
  }
  if (!target_is_confirmed(row)) {
    return(FALSE)
  }
  if (isTRUE(force)) {
    return(TRUE)
  }

  sig <- overview_fetch_signature(row)
  !identical(as.character(last_signature %||% NA_character_), sig)
}

not_confirmed_overview <- function(target_row, message = NULL) {
  list(
    ok = FALSE,
    status = "not_confirmed",
    message = message %||% "Confirm this target before retrieving biological annotations.",
    overview = NULL
  )
}

provenance_from_result <- function(result, record_id, query_id, page_url) {
  cache_status <- result$cache_status %||% NA_character_
  if (identical(cache_status, "fresh")) {
    cache_label <- "cached/fresh"
  } else if (identical(cache_status, "stale")) {
    cache_label <- "stale"
  } else if (isTRUE(result$from_cache)) {
    cache_label <- "cached/fresh"
  } else if (identical(cache_status, "live")) {
    cache_label <- "live"
  } else {
    cache_label <- "live"
  }

  list(
    source = result$source %||% NA_character_,
    record_id = record_id %||% NA_character_,
    query_id = query_id %||% NA_character_,
    retrieved_at = as.character(result$retrieved_at %||% NA_character_),
    cache_status = cache_label,
    from_cache = isTRUE(result$from_cache),
    url = page_url %||% result$url %||% NA_character_,
    ok = isTRUE(result$ok),
    error = result$error %||% NA_character_
  )
}

format_user_timestamp <- function(value, tz = NULL) {
  parsed <- parse_stored_time(value)
  if (length(parsed) != 1L || is.na(parsed[[1]])) {
    return(NA_character_)
  }

  tz <- resolve_display_tz(tz)
  instant <- parsed[[1]]
  local <- as.POSIXlt(instant, tz = tz)
  month <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )[[local$mon + 1L]]
  tz_label <- format(instant, tz = tz, format = "%Z")
  if (!nzchar(tz_label)) {
    tz_label <- format(instant, tz = tz, format = "%z")
  }
  sprintf(
    "%s %s %s, %02d:%02d %s",
    as.integer(local$mday),
    month,
    as.integer(local$year + 1900L),
    as.integer(local$hour),
    as.integer(local$min),
    tz_label
  )
}

resolve_display_tz <- function(tz = NULL) {
  candidate <- as.character(tz %||% "")[[1]]
  if (!nzchar(candidate)) {
    candidate <- Sys.getenv("TW_DISPLAY_TZ", unset = "")
  }
  if (!nzchar(candidate) || !(candidate %in% OlsonNames())) {
    return("UTC")
  }
  candidate
}

parse_stored_time <- function(value) {
  if (inherits(value, "POSIXt")) {
    return(as.POSIXct(as.numeric(as.POSIXct(value)), origin = "1970-01-01", tz = "UTC"))
  }
  if (!has_display_text(value)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  text <- trimws(as.character(value[[1]]))
  if (grepl("[Zz]$", text) || grepl(" UTC$", text)) {
    stripped <- sub(" UTC$", "", sub("[Zz]$", "", text))
    stripped <- gsub("T", " ", stripped)
    stripped <- sub("\\.[0-9]+$", "", stripped)
    return(suppressWarnings(as.POSIXct(stripped, tz = "UTC")))
  }
  if (grepl("[+-][0-9]{2}:?[0-9]{2}$", text)) {
    compact <- gsub("T", " ", text)
    compact <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", compact)
    parsed <- suppressWarnings(
      as.POSIXct(compact, format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC")
    )
    if (length(parsed) == 1L && !is.na(parsed)) {
      return(parsed)
    }
  }
  naive <- gsub("T", " ", text)
  naive <- sub("\\.[0-9]+$", "", naive)
  suppressWarnings(as.POSIXct(naive, tz = "UTC"))
}

format_utc_iso <- function(value) {
  parsed <- parse_stored_time(value)
  if (length(parsed) != 1L || is.na(parsed[[1]])) {
    return(NA_character_)
  }
  format(parsed[[1]], tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

extract_pubmed_ids <- function(text) {
  if (!has_display_text(text)) {
    return(character())
  }

  match_list <- gregexpr(
    "(?:PubMed|PMID)\\s*:?\\s*([0-9]+)",
    as.character(text),
    perl = TRUE,
    ignore.case = TRUE
  )[[1]]
  if (length(match_list) == 1L && match_list[[1]] == -1L) {
    return(character())
  }

  starts <- as.integer(match_list)
  lens <- attr(match_list, "match.length")
  captured <- substring(as.character(text), starts, starts + lens - 1L)
  ids <- gsub("[^0-9]", "", captured)
  unique(ids[nzchar(ids)])
}

readable_function_text <- function(text) {
  if (!has_display_text(text)) {
    return(NA_character_)
  }

  value <- as.character(text)
  value <- gsub(
    "\\(\\s*((?:PubMed|PMID)\\s*:?\\s*[0-9]+(?:\\s*,\\s*(?:PubMed|PMID)\\s*:?\\s*[0-9]+)*)\\s*\\)",
    "",
    value,
    perl = TRUE,
    ignore.case = TRUE
  )
  value <- gsub(
    "(?:PubMed|PMID)\\s*:\\s*[0-9]+",
    "",
    value,
    perl = TRUE,
    ignore.case = TRUE
  )
  value <- gsub("\\(\\s*\\)", "", value)
  value <- gsub("[[:space:]]+", " ", value)
  value <- gsub(" +([.,;:])", "\\1", value)
  value <- gsub("\\.([[:alpha:]])", ". \\1", value)
  trimws(value)
}

function_preview_text <- function(text, max_chars = 280L) {
  if (!has_display_text(text)) {
    return(NA_character_)
  }

  value <- trimws(as.character(text))
  if (nchar(value) <= max_chars) {
    return(value)
  }

  cut <- substr(value, 1L, max_chars)
  stops <- gregexpr("[.!?](\\s|$)", cut, perl = TRUE)[[1]]
  last_stop <- if (length(stops) == 1 && stops[[1]] == -1L) {
    -1L
  } else {
    max(stops)
  }
  if (last_stop >= 80L) {
    cut <- substr(value, 1L, last_stop)
  }

  paste0(trimws(cut), "\u2026")
}

combine_function_text <- function(function_comments) {
  if (is.null(function_comments) || length(function_comments) == 0) {
    return(NA_character_)
  }

  parts <- vapply(
    function_comments,
    function(item) {
      text <- item$text
      if (!has_display_text(text)) {
        return(NA_character_)
      }
      if (has_display_text(item$molecule)) {
        paste0(item$molecule, ": ", text)
      } else {
        as.character(text)
      }
    },
    character(1)
  )
  parts <- parts[vapply(parts, has_display_text, logical(1))]
  if (length(parts) == 0) {
    return(NA_character_)
  }
  paste(parts, collapse = "\n\n")
}

overview_disagreements <- function(target_row, uniprot_entry, ensembl_gene) {
  warnings <- list()
  confirmed_symbol <- target_row$display_symbol %||% NA_character_
  confirmed_ensg <- canonical_ensembl_gene_id(target_row$ensembl_gene_id)
  confirmed_acc <- toupper(trimws(target_row$uniprot_accession %||% ""))

  uniprot_symbol <- uniprot_entry$display_symbol %||% NA_character_
  ensembl_symbol <- ensembl_gene$display_symbol %||% NA_character_

  if (has_display_text(uniprot_symbol) && has_display_text(ensembl_symbol) &&
      !same_gene_symbol(uniprot_symbol, ensembl_symbol)) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "UniProt gene name (%s) and Ensembl display name (%s) differ.",
      uniprot_symbol,
      ensembl_symbol
    )
  }

  if (has_display_text(confirmed_symbol) && has_display_text(uniprot_symbol) &&
      !same_gene_symbol(confirmed_symbol, uniprot_symbol)) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "Confirmed symbol (%s) differs from the UniProt gene name (%s).",
      confirmed_symbol,
      uniprot_symbol
    )
  }

  if (has_display_text(confirmed_symbol) && has_display_text(ensembl_symbol) &&
      !same_gene_symbol(confirmed_symbol, ensembl_symbol)) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "Confirmed symbol (%s) differs from the Ensembl display name (%s).",
      confirmed_symbol,
      ensembl_symbol
    )
  }

  uniprot_org <- uniprot_entry$organism %||% NA_character_
  ensembl_species <- ensembl_gene$species %||% NA_character_
  if (has_display_text(uniprot_org) &&
      !identical(tolower(uniprot_org), "homo sapiens")) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "UniProt organism is %s, not Homo sapiens.",
      uniprot_org
    )
  }
  if (has_display_text(ensembl_species) &&
      !identical(tolower(ensembl_species), "homo_sapiens")) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "Ensembl species is %s, not homo_sapiens.",
      ensembl_species
    )
  }

  uniprot_genes <- uniprot_entry$ensembl_gene_ids %||% character()
  if (length(uniprot_genes) > 0 && has_display_text(confirmed_ensg) &&
      !(confirmed_ensg %in% uniprot_genes)) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "Confirmed Ensembl gene %s is not among UniProt Ensembl xrefs for this accession.",
      confirmed_ensg
    )
  }

  ensembl_id <- ensembl_gene$ensembl_gene_id %||% NA_character_
  if (has_display_text(confirmed_ensg) && has_display_text(ensembl_id) &&
      !identical(confirmed_ensg, ensembl_id)) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "Ensembl lookup returned %s, which does not match the confirmed gene %s.",
      ensembl_id,
      confirmed_ensg
    )
  }

  uniprot_acc <- uniprot_entry$accession %||% NA_character_
  if (has_display_text(confirmed_acc) && has_display_text(uniprot_acc) &&
      !identical(confirmed_acc, toupper(uniprot_acc))) {
    warnings[[length(warnings) + 1]] <- sprintf(
      "UniProt returned %s, which does not match the confirmed accession %s.",
      uniprot_acc,
      confirmed_acc
    )
  }

  warnings
}

empty_uniprot_entry <- function() {
  list(
    accession = NA_character_,
    display_symbol = NA_character_,
    protein_name = NA_character_,
    organism = NA_character_,
    reviewed = NA,
    ensembl_gene_ids = character(),
    hgnc_id = NA_character_,
    sequence_length = NA_integer_,
    molecular_weight = NA_integer_,
    function_comments = list(),
    subcellular_locations = list()
  )
}

empty_ensembl_gene <- function() {
  list(
    ensembl_gene_id = NA_character_,
    ensembl_gene_version = NA_integer_,
    display_symbol = NA_character_,
    description = NA_character_,
    biotype = NA_character_,
    seq_region = NA_character_,
    start = NA_integer_,
    end = NA_integer_,
    strand = NA_integer_,
    species = NA_character_,
    hgnc_id = NA_character_
  )
}

build_target_overview <- function(
  target_row,
  uniprot_entry,
  ensembl_gene,
  uniprot_result,
  ensembl_result
) {
  uniprot_entry <- uniprot_entry %||% empty_uniprot_entry()
  ensembl_gene <- ensembl_gene %||% empty_ensembl_gene()

  function_full <- combine_function_text(uniprot_entry$function_comments)
  function_readable <- readable_function_text(function_full)
  symbol <- target_row$display_symbol %||%
    ensembl_gene$display_symbol %||%
    uniprot_entry$display_symbol

  accession <- toupper(trimws(target_row$uniprot_accession %||% uniprot_entry$accession %||% ""))
  ensg <- canonical_ensembl_gene_id(target_row$ensembl_gene_id) %||%
    ensembl_gene$ensembl_gene_id

  list(
    project_target_id = as.character(target_row$id %||% NA_character_),
    identity = list(
      symbol = symbol %||% NA_character_,
      protein_name = uniprot_entry$protein_name %||% NA_character_,
      ensembl_gene_id = ensg %||% NA_character_,
      uniprot_accession = if (has_display_text(accession)) accession else NA_character_,
      hgnc_id = target_row$hgnc_id %||% uniprot_entry$hgnc_id %||% ensembl_gene$hgnc_id %||% NA_character_,
      organism = uniprot_entry$organism %||% NA_character_,
      input_text = target_row$input_text %||% NA_character_
    ),
    protein = list(
      length_aa = uniprot_entry$sequence_length %||% NA_integer_,
      molecular_weight = uniprot_entry$molecular_weight %||% NA_integer_,
      reviewed = uniprot_entry$reviewed,
      function_full = function_full,
      function_readable = function_readable,
      function_preview = function_preview_text(function_readable),
      function_pubmed_ids = extract_pubmed_ids(function_full),
      subcellular_locations = uniprot_entry$subcellular_locations %||% list()
    ),
    genomic = list(
      chromosome = ensembl_gene$seq_region %||% NA_character_,
      start = ensembl_gene$start %||% NA_integer_,
      end = ensembl_gene$end %||% NA_integer_,
      strand = ensembl_gene$strand %||% NA_integer_,
      biotype = ensembl_gene$biotype %||% NA_character_,
      description = ensembl_gene$description %||% NA_character_,
      gene_version = ensembl_gene$ensembl_gene_version %||% NA_integer_,
      location_label = format_location(
        ensembl_gene$seq_region,
        ensembl_gene$start,
        ensembl_gene$end,
        ensembl_gene$strand
      )
    ),
    disagreements = overview_disagreements(target_row, uniprot_entry, ensembl_gene),
    provenance = list(
      uniprot = provenance_from_result(
        uniprot_result,
        record_id = uniprot_entry$accession %||% accession,
        query_id = accession,
        page_url = if (has_display_text(accession)) {
          paste0("https://www.uniprot.org/uniprotkb/", accession)
        } else {
          NA_character_
        }
      ),
      ensembl = provenance_from_result(
        ensembl_result,
        record_id = ensembl_gene$ensembl_gene_id %||% ensg,
        query_id = ensg,
        page_url = if (has_display_text(ensg)) {
          paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", ensg)
        } else {
          NA_character_
        }
      )
    )
  )
}

retrieve_target_overview <- function(
  target_row,
  db_pool = NULL,
  uniprot_fetch = uniprot_get_accession,
  ensembl_fetch = ensembl_lookup_id,
  perform = httr2::req_perform
) {
  if (!target_is_confirmed(target_row)) {
    return(not_confirmed_overview(target_row))
  }

  accession <- toupper(trimws(target_row$uniprot_accession %||% ""))
  ensembl_id <- canonical_ensembl_gene_id(target_row$ensembl_gene_id)

  if (!has_display_text(accession) || !has_display_text(ensembl_id)) {
    return(not_confirmed_overview(
      target_row,
      "This confirmed target is missing a UniProt accession or Ensembl gene ID."
    ))
  }

  call_uniprot <- function(...) {
    if ("perform" %in% names(formals(uniprot_fetch))) {
      uniprot_fetch(..., perform = perform)
    } else {
      uniprot_fetch(...)
    }
  }
  call_ensembl <- function(...) {
    if ("perform" %in% names(formals(ensembl_fetch))) {
      ensembl_fetch(..., perform = perform)
    } else {
      ensembl_fetch(...)
    }
  }

  tw_then(call_uniprot(accession, db_pool = db_pool), function(uniprot_result) {
    tw_then(call_ensembl(ensembl_id, db_pool = db_pool), function(ensembl_result) {
      uniprot_parsed <- if (isTRUE(uniprot_result$ok)) {
        parse_uniprot_search(uniprot_result$data)
      } else {
        NULL
      }
      uniprot_entry <- if (!is.null(uniprot_parsed) && length(uniprot_parsed$entries) > 0) {
        uniprot_parsed$entries[[1]]
      } else {
        NULL
      }

      ensembl_gene <- if (isTRUE(ensembl_result$ok)) {
        parse_ensembl_gene(ensembl_result$data)
      } else {
        NULL
      }

      overview <- build_target_overview(
        target_row = target_row,
        uniprot_entry = uniprot_entry,
        ensembl_gene = ensembl_gene,
        uniprot_result = uniprot_result,
        ensembl_result = ensembl_result
      )

      list(
        ok = isTRUE(uniprot_result$ok) || isTRUE(ensembl_result$ok),
        status = "ready",
        message = NULL,
        overview = overview
      )
    })
  })
}
