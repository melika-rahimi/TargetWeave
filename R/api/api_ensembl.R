ENSEMBL_BASE <- "https://rest.ensembl.org"

parse_ensembl_gene <- function(payload) {
  if (!is.list(payload) || is.null(payload$id)) {
    return(NULL)
  }

  object_type <- payload$object_type %||% payload$type
  if (has_display_text(object_type) &&
      !identical(tolower(as.character(object_type)), "gene")) {
    return(NULL)
  }

  gene_id <- canonical_ensembl_gene_id(payload$id)
  if (!has_display_text(gene_id)) {
    return(NULL)
  }

  hgnc_id <- NA_character_
  description <- payload$description %||% NA_character_
  if (has_display_text(description)) {
    match <- regmatches(
      description,
      regexpr("HGNC:[0-9]+", description, perl = TRUE)
    )
    if (length(match) == 1 && nzchar(match)) {
      hgnc_id <- match
    }
  }

  parsed_id <- parse_ensembl_stable_id(payload$id)
  version <- payload$version
  if (!is_present_scalar(version) && !is.null(parsed_id)) {
    version <- parsed_id$version
  }
  if (is.character(version) && has_display_text(version)) {
    version <- suppressWarnings(as.integer(version))
  }
  if (!is_present_scalar(version)) {
    version <- NA_integer_
  } else {
    version <- as.integer(version)
  }

  list(
    ensembl_gene_id = gene_id,
    ensembl_gene_version = version,
    display_symbol = if (is.null(payload$display_name)) {
      NA_character_
    } else {
      as.character(payload$display_name)
    },
    description = if (is.null(payload$description)) {
      NA_character_
    } else {
      as.character(payload$description)
    },
    biotype = if (is.null(payload$biotype)) NA_character_ else as.character(payload$biotype),
    seq_region = {
      region <- payload$seq_region_name %||% payload$seqRegionName %||% payload$seq_region
      if (is.list(region)) {
        region <- region$name %||% region$id %||% NA_character_
      }
      if (is.null(region)) NA_character_ else as.character(region)
    },
    start = payload$start %||% NA_integer_,
    end = payload$end %||% NA_integer_,
    strand = payload$strand %||% NA_integer_,
    species = if (is.null(payload$species)) NA_character_ else as.character(payload$species),
    hgnc_id = hgnc_id
  )
}

parse_ensembl_xrefs <- function(payload) {
  if (is.null(payload) || !is.list(payload)) {
    return(character())
  }

  ids <- character()
  for (item in payload) {
    type <- tolower(as.character(item$type %||% item$object_type %||% ""))
    if (has_display_text(type) && !identical(type, "gene")) {
      next
    }
    if (!is.null(item$id)) {
      ids <- c(ids, as.character(item$id))
    }
  }

  unique(canonical_ensembl_gene_ids(ids))
}

ensembl_lookup_symbol <- function(symbol, db_pool = NULL, species = "homo_sapiens", perform = httr2::req_perform) {
  http_get_json(
    url = sprintf("%s/lookup/symbol/%s/%s", ENSEMBL_BASE, species, symbol),
    db_pool = db_pool,
    cache_source = "ensembl",
    cache_key = cache_key_ensembl_symbol(symbol, species),
    perform = perform
  )
}

ensembl_lookup_id <- function(ensembl_id, db_pool = NULL, perform = httr2::req_perform) {
  stable <- canonical_ensembl_gene_id(ensembl_id)
  if (!has_display_text(stable)) {
    return(new_api_result(
      ok = FALSE,
      error = "Not a canonical Ensembl gene ID.",
      source = "ensembl"
    ))
  }

  http_get_json(
    url = sprintf("%s/lookup/id/%s", ENSEMBL_BASE, stable),
    db_pool = db_pool,
    cache_source = "ensembl",
    cache_key = cache_key_ensembl_id(stable),
    perform = perform
  )
}

ensembl_xrefs_symbol <- function(symbol, db_pool = NULL, species = "homo_sapiens", perform = httr2::req_perform) {
  http_get_json(
    url = sprintf("%s/xrefs/symbol/%s/%s", ENSEMBL_BASE, species, symbol),
    query = list(object_type = "gene"),
    db_pool = db_pool,
    cache_source = "ensembl",
    cache_key = cache_key_ensembl_xrefs(symbol, species),
    perform = perform
  )
}
