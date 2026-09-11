RCSB_SEARCH_URL <- "https://search.rcsb.org/rcsbsearch/v2/query"
RCSB_DATA_GRAPHQL <- "https://data.rcsb.org/graphql"
RCSB_SEQCOORDS_GRAPHQL <- "https://sequence-coordinates.rcsb.org/graphql"
RCSB_SOURCE <- "rcsb"
RCSB_EXPERIMENTAL_SCOPE <- "experimental"
STRUCTURE_COVERAGE_ROW_CAP <- 20L
RCSB_SEARCH_PAGE_SIZE <- 1000L
RCSB_METADATA_BATCH <- 40L
RCSB_WATER_COMPONENTS <- c("HOH", "DOD", "WAT", "D2O", "H2O")

rcsb_search_ttl_seconds <- function() {
  get_app_config()$rcsb_search_ttl_days * 86400L
}

rcsb_metadata_ttl_seconds <- function() {
  get_app_config()$rcsb_metadata_ttl_days * 86400L
}

is_computed_structure_identifier <- function(identifier) {
  id <- toupper(trimws(as.character(identifier %||% "")))
  grepl("^(AF_|MA_)", id)
}

is_experimental_pdb_id <- function(pdb_id) {
  grepl("^[0-9][A-Za-z0-9]{3}$", trimws(as.character(pdb_id %||% "")))
}

parse_polymer_entity_id <- function(identifier) {
  id <- toupper(trimws(as.character(identifier %||% "")))
  if (!nzchar(id) || is_computed_structure_identifier(id)) {
    return(NULL)
  }
  parts <- strsplit(id, "_", fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(NULL)
  }
  pdb_id <- parts[[1]]
  entity_id <- parts[[2]]
  if (!is_experimental_pdb_id(pdb_id) || !grepl("^[0-9]+$", entity_id)) {
    return(NULL)
  }
  list(
    identifier = paste(pdb_id, entity_id, sep = "_"),
    pdb_id = pdb_id,
    entity_id = entity_id
  )
}

rcsb_uniprot_search_body <- function(uniprot_accession, start = 0L, rows = RCSB_SEARCH_PAGE_SIZE) {
  accession <- toupper(trimws(uniprot_accession))
  list(
    query = list(
      type = "group",
      logical_operator = "and",
      nodes = list(
        list(
          type = "terminal",
          service = "text",
          parameters = list(
            attribute = "rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_accession",
            operator = "exact_match",
            value = accession
          )
        ),
        list(
          type = "terminal",
          service = "text",
          parameters = list(
            attribute = "rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_name",
            operator = "exact_match",
            value = "UniProt"
          )
        )
      )
    ),
    return_type = "polymer_entity",
    request_options = list(
      paginate = list(start = as.integer(start), rows = as.integer(rows)),
      # I() keeps a JSON array under jsonlite auto_unbox (required; CSMs are not excluded by default).
      results_content_type = I(c(RCSB_EXPERIMENTAL_SCOPE))
    )
  )
}

RCSB_ENTITY_METADATA_QUERY <- paste(
  "query ($ids: [String!]!) {",
  "  polymer_entities(entity_ids: $ids) {",
  "    rcsb_id",
  "    rcsb_polymer_entity { pdbx_description pdbx_mutation }",
  "    entity_poly { pdbx_strand_id }",
  "    rcsb_polymer_entity_container_identifiers {",
  "      auth_asym_ids entity_id entry_id",
  "      reference_sequence_identifiers { database_accession database_name }",
  "    }",
  "    entry {",
  "      struct { title }",
  "      rcsb_accession_info { deposit_date initial_release_date }",
  "      rcsb_entry_info {",
  "        resolution_combined experimental_method structure_determination_methodology",
  "      }",
  "      exptl { method }",
  "      nonpolymer_entities {",
  "        pdbx_entity_nonpoly { comp_id name }",
  "        rcsb_nonpolymer_entity { pdbx_description }",
  "      }",
  "    }",
  "  }",
  "}",
  sep = "\n"
)

RCSB_ALIGNMENTS_QUERY <- paste(
  "query ($uniprot: String!, $first: Int) {",
  "  alignments(from: UNIPROT, to: PDB_ENTITY, queryId: $uniprot) {",
  "    target_alignments(first: $first) {",
  "      target_id",
  "      coverage { query_coverage query_length target_coverage target_length }",
  "      aligned_regions { query_begin query_end target_begin target_end }",
  "    }",
  "  }",
  "}",
  sep = "\n"
)

rcsb_post_json <- function(
  url,
  body,
  db_pool = NULL,
  cache_source = NULL,
  cache_key = NULL,
  ttl_seconds = NULL,
  perform = httr2::req_perform,
  empty_on_204 = FALSE
) {
  result <- http_post_json(
    url = url,
    body = body,
    db_pool = db_pool,
    cache_source = cache_source,
    cache_key = cache_key,
    ttl_seconds = ttl_seconds,
    perform = function(req) {
      inner <- perform
      tw_then(inner(req), function(resp) {
        if (isTRUE(empty_on_204) && identical(httr2::resp_status(resp), 204L)) {
          return(httr2::response(
            status_code = 200L,
            url = url,
            method = "POST",
            headers = list("Content-Type" = "application/json"),
            body = charToRaw('{"total_count":0,"result_set":[]}')
          ))
        }
        resp
      })
    }
  )
  if (!isTRUE(result$ok) && isTRUE(empty_on_204) && grepl("Empty response", result$error %||% "", ignore.case = TRUE)) {
    return(new_api_result(
      ok = TRUE,
      status = 204L,
      data = list(total_count = 0, result_set = list()),
      cache_status = "live",
      source = cache_source,
      url = url
    ))
  }
  result
}

fetch_rcsb_uniprot_search <- function(
  uniprot_accession,
  db_pool = NULL,
  perform = httr2::req_perform
) {
  accession <- toupper(trimws(uniprot_accession))
  rcsb_post_json(
    RCSB_SEARCH_URL,
    rcsb_uniprot_search_body(accession, 0L, RCSB_SEARCH_PAGE_SIZE),
    db_pool = db_pool,
    cache_source = RCSB_SOURCE,
    cache_key = cache_key_rcsb_search(accession),
    ttl_seconds = rcsb_search_ttl_seconds(),
    perform = perform,
    empty_on_204 = TRUE
  )
}

fetch_rcsb_entity_metadata <- function(
  entity_ids,
  db_pool = NULL,
  perform = httr2::req_perform
) {
  ids <- unique(as.character(entity_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) {
    return(new_api_result(ok = TRUE, status = 200L, data = list(data = list(polymer_entities = list()))))
  }
  rcsb_post_json(
    RCSB_DATA_GRAPHQL,
    list(query = RCSB_ENTITY_METADATA_QUERY, variables = list(ids = ids)),
    db_pool = db_pool,
    cache_source = RCSB_SOURCE,
    cache_key = cache_key_rcsb_metadata(ids),
    ttl_seconds = rcsb_metadata_ttl_seconds(),
    perform = perform
  )
}

fetch_rcsb_uniprot_alignments <- function(
  uniprot_accession,
  db_pool = NULL,
  perform = httr2::req_perform,
  first = 1000L
) {
  accession <- toupper(trimws(uniprot_accession))
  rcsb_post_json(
    RCSB_SEQCOORDS_GRAPHQL,
    list(
      query = RCSB_ALIGNMENTS_QUERY,
      variables = list(uniprot = accession, first = as.integer(first))
    ),
    db_pool = db_pool,
    cache_source = RCSB_SOURCE,
    cache_key = cache_key_rcsb_coverage(accession),
    ttl_seconds = rcsb_metadata_ttl_seconds(),
    perform = perform
  )
}

parse_rcsb_search_entities <- function(payload) {
  if (is.null(payload)) {
    return(list(ok = FALSE, entities = list(), error = "Missing RCSB Search payload."))
  }
  if (!is.null(payload$errors)) {
    return(list(ok = FALSE, entities = list(), error = paste(unlist(payload$errors), collapse = " ")))
  }
  rows <- payload$result_set %||% list()
  total <- suppressWarnings(as.integer(payload$total_count %||% length(rows)))
  parsed <- list()
  rejected <- character()
  for (item in rows) {
    identifier <- as.character(item$identifier %||% item %||% "")
    if (is_computed_structure_identifier(identifier)) {
      rejected <- c(rejected, identifier)
      next
    }
    rec <- parse_polymer_entity_id(identifier)
    if (is.null(rec)) {
      rejected <- c(rejected, identifier)
      next
    }
    parsed[[length(parsed) + 1L]] <- rec
  }
  list(
    ok = TRUE,
    total_count = if (is.na(total)) length(parsed) else total,
    entities = parsed,
    rejected = unique(rejected),
    error = NULL
  )
}

rcsb_display_text <- function(value, fallback = "Not provided") {
  text <- as.character(value %||% "")
  if (!nzchar(trimws(text)) || identical(tolower(text), "null") || identical(text, "NA")) {
    return(fallback)
  }
  text
}

rcsb_format_resolution <- function(values) {
  nums <- suppressWarnings(as.numeric(unlist(values)))
  nums <- nums[is.finite(nums)]
  if (length(nums) == 0) {
    return("Not provided")
  }
  sprintf("%.2f \u00c5", nums[[1]])
}

rcsb_format_date <- function(value) {
  text <- as.character(value %||% "")
  if (!nzchar(text)) {
    return("Not provided")
  }
  substr(text, 1, 10)
}

parse_rcsb_ligands <- function(nonpolymer_entities) {
  rows <- list()
  for (item in nonpolymer_entities %||% list()) {
    if (!is.list(item)) {
      next
    }
    nonpoly <- item$pdbx_entity_nonpoly %||% list()
    comp_id <- toupper(trimws(as.character(nonpoly$comp_id %||% "")))
    if (!nzchar(comp_id) || comp_id %in% RCSB_WATER_COMPONENTS) {
      next
    }
    name <- rcsb_display_text(
      nonpoly$name %||% item$rcsb_nonpolymer_entity$pdbx_description,
      fallback = comp_id
    )
    rows[[length(rows) + 1L]] <- data.frame(
      component_id = comp_id,
      name = name,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(component_id = character(), name = character(), stringsAsFactors = FALSE))
  }
  unique(do.call(rbind, rows))
}

parse_rcsb_entity_metadata <- function(payload) {
  if (!is.null(payload$errors)) {
    return(list(ok = FALSE, records = list(), error = paste(vapply(payload$errors, function(e) e$message %||% "GraphQL error", character(1)), collapse = " ")))
  }
  ents <- payload$data$polymer_entities %||% list()
  records <- list()
  for (ent in ents) {
    if (!is.list(ent)) {
      next
    }
    ids <- parse_polymer_entity_id(ent$rcsb_id)
    if (is.null(ids)) {
      next
    }
    methodology <- tolower(as.character(ent$entry$rcsb_entry_info$structure_determination_methodology %||% ""))
    if (identical(methodology, "computational")) {
      next
    }
    chains <- ent$rcsb_polymer_entity_container_identifiers$auth_asym_ids %||% list()
    if (length(chains) == 0) {
      strand <- as.character(ent$entity_poly$pdbx_strand_id %||% "")
      chains <- if (nzchar(strand)) strsplit(strand, ",\\s*")[[1]] else character()
    }
    chains <- as.character(unlist(chains))
    method <- NA_character_
    exptl <- ent$entry$exptl
    if (is.list(exptl) && length(exptl) > 0) {
      method <- as.character(exptl[[1]]$method %||% NA_character_)
    }
    if (!has_display_text(method)) {
      method <- as.character(ent$entry$rcsb_entry_info$experimental_method %||% "Not provided")
    }
    mutation <- as.character(ent$rcsb_polymer_entity$pdbx_mutation %||% "")
    records[[ids$identifier]] <- list(
      pdb_id = ids$pdb_id,
      polymer_entity = list(
        entity_id = ids$entity_id,
        entity_identifier = ids$identifier,
        description = rcsb_display_text(ent$rcsb_polymer_entity$pdbx_description, "Description not provided"),
        chains = chains
      ),
      experiment = list(
        method = rcsb_display_text(method),
        resolution_angstrom = rcsb_format_resolution(ent$entry$rcsb_entry_info$resolution_combined),
        methodology = rcsb_display_text(ent$entry$rcsb_entry_info$structure_determination_methodology, "experimental")
      ),
      entry = list(
        title = rcsb_display_text(ent$entry$struct$title, "Title not provided"),
        deposition_date = rcsb_format_date(ent$entry$rcsb_accession_info$deposit_date),
        release_date = rcsb_format_date(ent$entry$rcsb_accession_info$initial_release_date)
      ),
      ligands = parse_rcsb_ligands(ent$entry$nonpolymer_entities),
      engineered = nzchar(trimws(mutation)),
      mutation_label = if (nzchar(trimws(mutation))) mutation else NA_character_
    )
  }
  list(ok = TRUE, records = records, error = NULL)
}

parse_rcsb_alignments <- function(payload) {
  if (!is.null(payload$errors)) {
    return(list(ok = FALSE, alignments = list(), error = "Malformed Sequence Coordinates payload."))
  }
  items <- payload$data$alignments$target_alignments %||% list()
  alignments <- list()
  uniprot_length <- NA_integer_
  for (item in items) {
    if (!is.list(item)) {
      next
    }
    target_id <- as.character(item$target_id %||% "")
    if (!nzchar(target_id)) {
      next
    }
    qlen <- suppressWarnings(as.integer(item$coverage$query_length %||% NA_integer_))
    if (is.na(uniprot_length) && !is.na(qlen)) {
      uniprot_length <- qlen
    }
    regions <- item$aligned_regions %||% list()
    ranges <- list()
    for (reg in regions) {
      begin <- suppressWarnings(as.integer(reg$query_begin %||% NA_integer_))
      end <- suppressWarnings(as.integer(reg$query_end %||% NA_integer_))
      if (is.na(begin) || is.na(end) || end < begin) {
        next
      }
      ranges[[length(ranges) + 1L]] <- data.frame(
        begin = begin,
        end = end,
        stringsAsFactors = FALSE
      )
    }
    range_df <- if (length(ranges) == 0) {
      data.frame(begin = integer(), end = integer(), stringsAsFactors = FALSE)
    } else {
      do.call(rbind, ranges)
    }
    alignments[[target_id]] <- list(
      target_id = target_id,
      uniprot_length = qlen,
      ranges = range_df
    )
  }
  list(ok = TRUE, alignments = alignments, uniprot_length = uniprot_length, error = NULL)
}

rcsb_structure_page_url <- function(pdb_id) {
  sprintf("https://www.rcsb.org/structure/%s", toupper(pdb_id))
}

rcsb_viewer_embed_url <- function(pdb_id) {
  sprintf("https://www.rcsb.org/3d-view/%s", toupper(pdb_id))
}
