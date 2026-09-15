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

json_unwrap_scalar <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  if (is.list(value) && !is.data.frame(value)) {
    if (length(value) == 0L) {
      return(NULL)
    }
    if (length(value) == 1L) {
      return(json_unwrap_scalar(value[[1]]))
    }
  }
  value
}

json_missing_text <- function(text) {
  if (!nzchar(text)) {
    return(TRUE)
  }
  lower <- tolower(text)
  lower %in% c("na", "null", "nan", "n/a", "undefined")
}

export_scalar_chr <- function(value, default = NA_character_) {
  value <- json_unwrap_scalar(value)
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  if (length(value) != 1L) {
    value <- value[[1]]
  }
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(default)
  }
  text <- trimws(as.character(value))
  if (json_missing_text(text)) {
    return(default)
  }
  text
}

export_scalar_num <- function(value) {
  value <- json_unwrap_scalar(value)
  if (is.null(value) || length(value) == 0) {
    return(NA_real_)
  }
  if (length(value) != 1L) {
    return(NA_real_)
  }
  if (is.numeric(value)) {
    return(as.numeric(value))
  }
  if (is.logical(value)) {
    if (is.na(value)) {
      return(NA_real_)
    }
    return(NA_real_)
  }
  if (is.character(value) || is.factor(value)) {
    text <- trimws(as.character(value))
    if (json_missing_text(text)) {
      return(NA_real_)
    }
    if (grepl("^-?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?$", text) ||
        grepl("^-?\\.[0-9]+([eE][+-]?[0-9]+)?$", text)) {
      parsed <- suppressWarnings(as.numeric(text))
      if (length(parsed) == 1L && !is.na(parsed)) {
        return(parsed)
      }
    }
    return(NA_real_)
  }
  NA_real_
}

export_chr_vec <- function(value, n = NULL) {
  if (is.null(value)) {
    n <- n %||% 0L
    return(rep(NA_character_, n))
  }
  if (is.null(n)) {
    n <- length(value)
  }
  if (n <= 0L) {
    return(character())
  }
  vapply(seq_len(n), function(i) {
    if (i > length(value)) {
      return(NA_character_)
    }
    export_scalar_chr(value[[i]])
  }, character(1))
}

export_num_vec <- function(value, n = NULL) {
  if (is.null(value)) {
    n <- n %||% 0L
    return(rep(NA_real_, n))
  }
  if (is.null(n)) {
    n <- length(value)
  }
  if (n <= 0L) {
    return(numeric())
  }
  vapply(seq_len(n), function(i) {
    if (i > length(value)) {
      return(NA_real_)
    }
    export_scalar_num(value[[i]])
  }, numeric(1))
}

csv_missing <- function(value) {
  export_scalar_chr(value)
}

csv_score <- function(value) {
  export_num_vec(value)
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
  n <- nrow(targets)
  data.frame(
    target = export_chr_vec(targets$symbol, n),
    direct_score = export_num_vec(targets$overall_direct_score, n),
    broader_score = if ("overall_inclusive_score" %in% names(targets)) {
      export_num_vec(targets$overall_inclusive_score, n)
    } else {
      rep(NA_real_, n)
    },
    retrieval_status = export_chr_vec(
      targets$retrieval_status %||% targets$cache_status,
      n
    ),
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
  n_rows <- nrow(rows)
  matched <- export_chr_vec(rows$matched_targets, n_rows)
  counts <- if ("matched_target_count" %in% names(rows)) {
    as.integer(export_num_vec(rows$matched_target_count, n_rows))
  } else {
    rep(NA_integer_, n_rows)
  }
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
    n_rec <- nrow(recs)
    parts[[length(parts) + 1L]] <- data.frame(
      target = rep(export_scalar_chr(item$target$symbol, default = ""), n_rec),
      pmid = export_chr_vec(recs$pmid, n_rec),
      title = export_chr_vec(recs$title, n_rec),
      first_author = if ("first_author" %in% names(recs)) export_chr_vec(recs$first_author, n_rec) else rep(NA_character_, n_rec),
      journal = if ("journal" %in% names(recs)) export_chr_vec(recs$journal, n_rec) else rep(NA_character_, n_rec),
      publication_date = if ("publication_date" %in% names(recs)) export_chr_vec(recs$publication_date, n_rec) else rep(NA_character_, n_rec),
      doi = if ("doi" %in% names(recs)) export_chr_vec(recs$doi, n_rec) else rep(NA_character_, n_rec),
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
      chains <- rec$polymer_entity$chains %||% rec$chains %||% character()
      if (is.list(chains)) {
        chains <- vapply(chains, export_scalar_chr, character(1), default = "")
        chains <- chains[nzchar(chains)]
      } else {
        chains <- export_chr_vec(chains)
        chains <- chains[!is.na(chains) & nzchar(chains)]
      }
      parts[[length(parts) + 1L]] <- data.frame(
        target = export_scalar_chr(item$target$symbol, default = ""),
        pdb_id = export_scalar_chr(rec$pdb_id, default = ""),
        polymer_entity = export_scalar_chr(
          rec$polymer_entity$entity_identifier %||% rec$polymer_entity$entity_id,
          default = ""
        ),
        chains = paste(chains, collapse = ", "),
        method = export_scalar_chr(rec$experiment$method),
        resolution_angstrom = export_scalar_chr(rec$experiment$resolution_angstrom),
        coverage_fraction = export_scalar_num(rec$coverage$coverage_fraction),
        release_date = export_scalar_chr(rec$entry$release_date),
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
