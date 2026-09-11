UNIPROT_BASE <- "https://rest.uniprot.org"

uniprot_is_reviewed_entry <- function(entry) {
  identical(entry$entryType, "UniProtKB reviewed (Swiss-Prot)") ||
    isTRUE(entry$reviewed)
}

uniprot_protein_name <- function(entry) {
  desc <- entry$proteinDescription
  if (is.null(desc)) {
    return(NA_character_)
  }

  rec <- desc$recommendedName$fullName$value
  if (has_display_text(rec)) {
    return(as.character(rec))
  }

  alt <- desc$submissionNames[[1]]$fullName$value
  if (has_display_text(alt)) {
    return(as.character(alt))
  }

  NA_character_
}

uniprot_gene_symbol <- function(entry) {
  genes <- entry$genes
  if (is.null(genes) || length(genes) == 0) {
    return(NA_character_)
  }

  name <- genes[[1]]$geneName$value
  if (is.null(name)) {
    return(NA_character_)
  }

  as.character(name)
}

uniprot_gene_synonyms <- function(entry) {
  genes <- entry$genes
  if (is.null(genes) || length(genes) == 0) {
    return(character())
  }

  aliases <- character()
  for (gene in genes) {
    for (syn in gene$synonyms %||% list()) {
      value <- syn$value
      if (has_display_text(value)) {
        aliases <- c(aliases, as.character(value))
      }
    }
  }

  unique(aliases)
}

uniprot_xref_ids <- function(entry, database, id_property = NULL) {
  refs <- entry$uniProtKBCrossReferences
  if (is.null(refs)) {
    return(character())
  }

  ids <- character()

  for (ref in refs) {
    if (!identical(ref$database, database)) {
      next
    }

    if (!is.null(id_property) && !is.null(ref$properties)) {
      wanted <- tolower(as.character(id_property))
      for (prop in ref$properties) {
        key <- tolower(as.character(prop$key %||% ""))
        if (identical(key, wanted) && !is.null(prop$value)) {
          ids <- c(ids, as.character(prop$value))
        }
      }
    } else if (!is.null(ref$id)) {
      ids <- c(ids, as.character(ref$id))
    }
  }

  unique(ids[nzchar(ids)])
}

uniprot_comment_text <- function(comment) {
  texts <- comment$texts
  if (is.null(texts) || length(texts) == 0) {
    return(character())
  }

  out <- character()
  for (item in texts) {
    value <- item$value
    if (has_display_text(value)) {
      out <- c(out, as.character(value))
    }
  }
  out
}

uniprot_function_comments <- function(entry) {
  comments <- entry$comments
  if (is.null(comments) || length(comments) == 0) {
    return(list())
  }

  out <- list()
  for (comment in comments) {
    if (!identical(comment$commentType, "FUNCTION")) {
      next
    }
    texts <- uniprot_comment_text(comment)
    if (length(texts) == 0) {
      next
    }
    molecule <- comment$molecule
    out[[length(out) + 1]] <- list(
      text = paste(texts, collapse = " "),
      molecule = if (has_display_text(molecule)) as.character(molecule) else NA_character_
    )
  }
  out
}

uniprot_location_evidence <- function(node) {
  if (is.null(node) || is.null(node$evidences) || length(node$evidences) == 0) {
    return(NA_character_)
  }

  parts <- character()
  for (ev in node$evidences) {
    code <- ev$evidenceCode %||% ""
    source <- ev$source %||% ""
    id <- ev$id %||% ""
    label <- paste(c(source, id)[nzchar(c(as.character(source), as.character(id)))], collapse = ":")
    if (has_display_text(code) && has_display_text(label)) {
      parts <- c(parts, paste(code, label))
    } else if (has_display_text(label)) {
      parts <- c(parts, label)
    } else if (has_display_text(code)) {
      parts <- c(parts, as.character(code))
    }
  }

  if (length(parts) == 0) {
    return(NA_character_)
  }
  paste(unique(parts), collapse = "; ")
}

uniprot_subcellular_locations <- function(entry) {
  comments <- entry$comments
  if (is.null(comments) || length(comments) == 0) {
    return(list())
  }

  out <- list()
  for (comment in comments) {
    if (!identical(comment$commentType, "SUBCELLULAR LOCATION")) {
      next
    }

    molecule <- if (has_display_text(comment$molecule)) {
      as.character(comment$molecule)
    } else {
      NA_character_
    }
    note_texts <- character()
    if (!is.null(comment$note)) {
      note_texts <- uniprot_comment_text(comment$note)
    }
    note <- if (length(note_texts) == 0) NA_character_ else paste(note_texts, collapse = " ")

    locations <- comment$subcellularLocations
    if (is.null(locations) || length(locations) == 0) {
      next
    }

    for (loc in locations) {
      location_value <- loc$location$value
      topology_value <- loc$topology$value
      if (!has_display_text(location_value)) {
        next
      }

      evidence <- uniprot_location_evidence(loc$location)
      if (!has_display_text(evidence)) {
        evidence <- uniprot_location_evidence(loc$topology)
      }

      item <- list(
        location = as.character(location_value),
        topology = if (has_display_text(topology_value)) as.character(topology_value) else NA_character_,
        molecule = molecule,
        note = note,
        evidence = evidence
      )
      key <- paste(
        item$location,
        item$topology %||% "",
        item$molecule %||% "",
        sep = "|"
      )
      if (key %in% names(out)) {
        next
      }
      out[[key]] <- item
    }
  }
  unname(out)
}

parse_uniprot_entry <- function(entry) {
  if (!is.list(entry) || is.null(entry$primaryAccession)) {
    return(NULL)
  }

  ensembl_genes <- canonical_ensembl_gene_ids(
    uniprot_xref_ids(entry, "Ensembl", "GeneId")
  )
  hgnc_ids <- uniprot_xref_ids(entry, "HGNC")

  list(
    accession = as.character(entry$primaryAccession),
    uniprot_id = if (is.null(entry$uniProtkbId)) NA_character_ else as.character(entry$uniProtkbId),
    display_symbol = uniprot_gene_symbol(entry),
    gene_synonyms = uniprot_gene_synonyms(entry),
    protein_name = uniprot_protein_name(entry),
    organism = if (is.null(entry$organism$scientificName)) {
      NA_character_
    } else {
      as.character(entry$organism$scientificName)
    },
    taxon_id = entry$organism$taxonId %||% NA_integer_,
    reviewed = uniprot_is_reviewed_entry(entry),
    ensembl_gene_ids = ensembl_genes,
    hgnc_id = if (length(hgnc_ids) == 0) NA_character_ else hgnc_ids[[1]],
    sequence_length = entry$sequence$length %||% NA_integer_,
    molecular_weight = entry$sequence$molWeight %||% NA_integer_,
    function_comments = uniprot_function_comments(entry),
    subcellular_locations = uniprot_subcellular_locations(entry)
  )
}

parse_uniprot_search <- function(payload) {
  if (is.null(payload)) {
    return(list(ok = FALSE, error = "Empty UniProt payload.", entries = list()))
  }

  if (!is.list(payload)) {
    return(list(ok = FALSE, error = "Malformed UniProt payload.", entries = list()))
  }

  results <- payload$results
  if (is.null(results)) {
    entry <- parse_uniprot_entry(payload)
    if (is.null(entry)) {
      return(list(ok = FALSE, error = "Malformed UniProt payload.", entries = list()))
    }
    return(list(ok = TRUE, error = NULL, entries = list(entry)))
  }

  entries <- Filter(Negate(is.null), lapply(results, parse_uniprot_entry))
  list(ok = TRUE, error = NULL, entries = entries)
}

uniprot_search_symbol <- function(
  symbol,
  db_pool = NULL,
  reviewed = TRUE,
  organism_id = "9606",
  perform = httr2::req_perform
) {
  query <- sprintf(
    "(gene_exact:%s) AND (organism_id:%s)%s",
    symbol,
    organism_id,
    if (isTRUE(reviewed)) " AND (reviewed:true)" else ""
  )

  http_get_json(
    url = paste0(UNIPROT_BASE, "/uniprotkb/search"),
    query = list(
      query = query,
      format = "json",
      size = 25
    ),
    db_pool = db_pool,
    cache_source = "uniprot",
    cache_key = cache_key_uniprot_symbol(symbol, organism_id, reviewed),
    perform = perform
  )
}

uniprot_search_ensembl_xref <- function(
  ensembl_gene_id,
  db_pool = NULL,
  organism_id = "9606",
  perform = httr2::req_perform
) {
  stable <- canonical_ensembl_gene_id(ensembl_gene_id)
  if (!has_display_text(stable)) {
    return(new_api_result(
      ok = FALSE,
      error = "Not a canonical Ensembl gene ID.",
      source = "uniprot"
    ))
  }

  query <- sprintf(
    "(xref:%s) AND (organism_id:%s)",
    stable,
    organism_id
  )

  http_get_json(
    url = paste0(UNIPROT_BASE, "/uniprotkb/search"),
    query = list(
      query = query,
      format = "json",
      size = 25
    ),
    db_pool = db_pool,
    cache_source = "uniprot",
    cache_key = sprintf(
      "uniprot:ensembl:%s:%s",
      stable,
      organism_id
    ),
    perform = perform
  )
}

uniprot_get_accession <- function(accession, db_pool = NULL, perform = httr2::req_perform) {
  http_get_json(
    url = paste0(UNIPROT_BASE, "/uniprotkb/", toupper(accession), ".json"),
    db_pool = db_pool,
    cache_source = "uniprot",
    cache_key = cache_key_uniprot_accession(accession),
    perform = perform
  )
}
