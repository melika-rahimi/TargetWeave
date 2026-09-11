DOSSIER_FORMAT_VERSION <- 1L

export_section_status <- function(section) {
  as.character(section$capture_status %||% "not_retrieved")
}

export_section_has_content <- function(section) {
  status <- export_section_status(section)
  status %in% c("captured", "partial", "stale_at_capture") &&
    (!is.null(section$model) || length(section$targets) > 0)
}

export_safe_stem <- function(name, captured_at = Sys.time()) {
  when <- parse_stored_time(captured_at)
  if (length(when) != 1L || is.na(when[[1]])) {
    when <- parse_stored_time(Sys.time())
  }
  date <- format(when[[1]], tz = resolve_display_tz(), format = "%Y-%m-%d")
  if (!nzchar(date) || identical(date, "NA")) {
    date <- format(Sys.Date(), "%Y-%m-%d")
  }
  raw <- gsub("[^A-Za-z0-9_-]+", "-", as.character(name %||% "snapshot"))
  raw <- gsub("^-+|-+$", "", raw)
  raw <- gsub("-{2,}", "-", raw)
  raw <- substr(raw, 1, 60)
  if (!nzchar(raw)) {
    raw <- "snapshot"
  }
  sprintf("TargetWeave_%s_%s", raw, date)
}

csv_missing <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }
  text <- as.character(value[[1]])
  if (!nzchar(trimws(text)) || identical(text, "NA") || identical(text, "NULL")) {
    return(NA_character_)
  }
  text
}

csv_score <- function(value) {
  suppressWarnings(as.numeric(value))
}

write_export_csv <- function(path, data) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  path
}

snapshot_target_rows <- function(snapshot) {
  items <- snapshot$target_identity$model$targets %||% list()
  if (length(items) == 0) {
    return(data.frame(
      symbol = character(),
      ensembl_gene_id = character(),
      uniprot_accession = character(),
      hgnc_id = character(),
      ncbi_gene_id = character(),
      organism = character(),
      resolution_status = character(),
      stringsAsFactors = FALSE
    ))
  }
  lit_gene <- list()
  if (export_section_has_content(snapshot$literature)) {
    for (item in snapshot$literature$model$targets %||% list()) {
      sym <- as.character(item$target$symbol %||% "")
      lit_gene[[sym]] <- as.character(item$target$ncbi_gene_id %||% NA_character_)
    }
  }
  organism <- snapshot$project_context$model$organism %||% "Homo sapiens"
  do.call(rbind, lapply(items, function(row) {
    symbol <- as.character(row$display_symbol %||% row$input_text %||% "")
    data.frame(
      symbol = symbol,
      ensembl_gene_id = csv_missing(row$ensembl_gene_id),
      uniprot_accession = csv_missing(row$uniprot_accession),
      hgnc_id = csv_missing(row$hgnc_id),
      ncbi_gene_id = csv_missing(lit_gene[[symbol]]),
      organism = organism,
      resolution_status = csv_missing(row$resolution_status),
      stringsAsFactors = FALSE
    )
  }))
}

export_targets_csv <- function(snapshot) {
  snapshot_target_rows(snapshot)[, c(
    "symbol", "ensembl_gene_id", "uniprot_accession", "hgnc_id", "ncbi_gene_id"
  ), drop = FALSE]
}

export_comparison_csv <- function(snapshot) {
  if (!export_section_has_content(snapshot$comparison)) {
    return(NULL)
  }
  targets <- snapshot$comparison$model$targets
  if (is.null(targets) || nrow(targets) == 0) {
    return(NULL)
  }
  data.frame(
    target = as.character(targets$symbol),
    direct_score = csv_score(targets$overall_direct_score),
    broader_score = if ("overall_inclusive_score" %in% names(targets)) {
      csv_score(targets$overall_inclusive_score)
    } else {
      NA_real_
    },
    retrieval_status = as.character(targets$retrieval_status %||% targets$cache_status %||% ""),
    stringsAsFactors = FALSE
  )
}

export_pathways_csv <- function(snapshot) {
  if (!export_section_has_content(snapshot$pathways)) {
    return(NULL)
  }
  rows <- snapshot$pathways$model$pathways
  mat <- snapshot$pathways$model$membership_matrix
  if ((is.null(rows) || nrow(rows) == 0) && (is.null(mat) || nrow(mat) == 0)) {
    return(NULL)
  }
  if (is.null(rows) || nrow(rows) == 0) {
    rows <- unique(mat[, c("pathway_id", "pathway_name"), drop = FALSE])
    rows$matched_target_count <- NA_integer_
  }
  matched <- as.character(rows$matched_targets %||% NA_character_)
  counts <- if ("matched_target_count" %in% names(rows)) as.integer(rows$matched_target_count) else NA_integer_
  if (!is.null(mat) && nrow(mat) > 0) {
    present <- mat[mat$is_member %in% TRUE, , drop = FALSE]
    if (nrow(present) > 0) {
      grouped <- split(as.character(present$symbol), as.character(present$pathway_id))
      ids <- as.character(rows$pathway_id)
      matched <- vapply(ids, function(id) {
        paste(unique(grouped[[id]] %||% character()), collapse = "; ")
      }, character(1))
      counts <- vapply(ids, function(id) length(unique(grouped[[id]] %||% character())), integer(1))
    }
  }
  data.frame(
    pathway_id = as.character(rows$pathway_id),
    pathway_name = as.character(rows$pathway_name),
    matched_targets = matched,
    matched_target_count = counts,
    membership_scope = snapshot$pathways$model$provenance$membership_scope %||% "lowest_level",
    stringsAsFactors = FALSE
  )
}

export_literature_csv <- function(snapshot) {
  if (!export_section_has_content(snapshot$literature)) {
    return(NULL)
  }
  empty <- data.frame(
    target = character(),
    pmid = character(),
    title = character(),
    first_author = character(),
    journal = character(),
    publication_date = character(),
    doi = character(),
    stringsAsFactors = FALSE
  )
  parts <- list()
  for (item in snapshot$literature$model$targets %||% list()) {
    recs <- item$recent_records
    if (is.null(recs) || nrow(recs) == 0) {
      next
    }
    parts[[length(parts) + 1L]] <- data.frame(
      target = as.character(item$target$symbol %||% ""),
      pmid = as.character(recs$pmid %||% ""),
      title = as.character(recs$title %||% ""),
      first_author = if ("first_author" %in% names(recs)) as.character(recs$first_author) else NA_character_,
      journal = if ("journal" %in% names(recs)) as.character(recs$journal) else NA_character_,
      publication_date = if ("publication_date" %in% names(recs)) as.character(recs$publication_date) else NA_character_,
      doi = if ("doi" %in% names(recs)) as.character(recs$doi) else NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(parts) == 0) {
    return(empty)
  }
  do.call(rbind, parts)
}

export_structures_csv <- function(snapshot) {
  if (!export_section_has_content(snapshot$structures)) {
    return(NULL)
  }
  parts <- list()
  for (item in snapshot$structures$model$targets %||% list()) {
    for (rec in item$records %||% list()) {
      parts[[length(parts) + 1L]] <- data.frame(
        target = as.character(item$target$symbol %||% ""),
        pdb_id = as.character(rec$pdb_id %||% ""),
      polymer_entity = as.character(rec$polymer_entity$entity_identifier %||% rec$polymer_entity$entity_id %||% ""),
      chains = paste(as.character(rec$polymer_entity$chains %||% rec$chains %||% character()), collapse = ", "),
        method = as.character(rec$experiment$method %||% NA_character_),
        resolution_angstrom = as.character(rec$experiment$resolution_angstrom %||% NA_character_),
        coverage_fraction = as.numeric(rec$coverage$coverage_fraction %||% NA_real_),
        release_date = as.character(rec$entry$release_date %||% NA_character_),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(parts) == 0) {
    return(NULL)
  }
  do.call(rbind, parts)
}

write_export_tables <- function(snapshot, tables_dir) {
  written <- list()
  add <- function(name, data) {
    if (is.null(data)) {
      return()
    }
    path <- file.path(tables_dir, name)
    write_export_csv(path, data)
    written[[name]] <<- path
  }
  add("targets.csv", export_targets_csv(snapshot))
  add("open_targets_comparison.csv", export_comparison_csv(snapshot))
  add("reactome_pathways.csv", export_pathways_csv(snapshot))
  if (export_section_has_content(snapshot$literature)) {
    add("literature_records.csv", export_literature_csv(snapshot))
  }
  add("structures.csv", export_structures_csv(snapshot))
  written
}
