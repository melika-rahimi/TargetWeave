PATHWAY_MATRIX_ROW_CAP <- 25L

empty_overlap_pathways <- function() {
  data.frame(
    pathway_id = character(),
    pathway_name = character(),
    membership_scope = character(),
    is_in_disease = logical(),
    pathway_url = character(),
    matched_targets = character(),
    matched_target_count = integer(),
    stringsAsFactors = FALSE
  )
}

empty_membership_matrix <- function() {
  data.frame(
    pathway_id = character(),
    pathway_name = character(),
    project_target_id = character(),
    symbol = character(),
    is_member = logical(),
    stringsAsFactors = FALSE
  )
}

build_pathway_overlap <- function(
  memberships,
  excluded = list(),
  failures = list(),
  reactome_release = NA_character_,
  version_provenance = NULL
) {
  target_rows <- lapply(memberships, function(item) {
    data.frame(
      project_target_id = item$project_target_id,
      symbol = item$symbol,
      ensembl_gene_id = item$ensembl_gene_id %||% NA_character_,
      uniprot_accession = item$uniprot_accession,
      n_pathways = nrow(item$pathways),
      status = if (nrow(item$pathways) == 0) "empty" else "ok",
      stringsAsFactors = FALSE
    )
  })
  targets <- if (length(target_rows) == 0) {
    data.frame(
      project_target_id = character(),
      symbol = character(),
      ensembl_gene_id = character(),
      uniprot_accession = character(),
      n_pathways = integer(),
      status = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, target_rows)
  }

  pathway_map <- list()
  for (item in memberships) {
    if (is.null(item$pathways) || nrow(item$pathways) == 0) {
      next
    }
    for (i in seq_len(nrow(item$pathways))) {
      row <- item$pathways[i, , drop = FALSE]
      pid <- as.character(row$pathway_id[[1]])
      if (is.null(pathway_map[[pid]])) {
        pathway_map[[pid]] <- list(
          pathway_id = pid,
          pathway_name = as.character(row$pathway_name[[1]]),
          membership_scope = as.character(row$membership_scope[[1]]),
          is_in_disease = isTRUE(row$is_in_disease[[1]]),
          pathway_url = as.character(row$pathway_url[[1]]),
          members = character()
        )
      }
      pathway_map[[pid]]$members <- unique(c(
        pathway_map[[pid]]$members,
        as.character(item$symbol)
      ))
    }
  }

  if (length(pathway_map) == 0) {
    pathways <- empty_overlap_pathways()
  } else {
    pathways <- do.call(rbind, lapply(pathway_map, function(item) {
      members <- sort(unique(item$members))
      data.frame(
        pathway_id = item$pathway_id,
        pathway_name = item$pathway_name,
        membership_scope = item$membership_scope,
        is_in_disease = item$is_in_disease,
        pathway_url = item$pathway_url,
        matched_targets = paste(members, collapse = ", "),
        matched_target_count = length(members),
        stringsAsFactors = FALSE
      )
    }))
    pathways <- pathways[order(-pathways$matched_target_count, pathways$pathway_name, pathways$pathway_id), , drop = FALSE]
    rownames(pathways) <- NULL
  }

  matrix_rows <- list()
  for (pid in pathways$pathway_id) {
    meta <- pathway_map[[pid]]
    for (j in seq_len(nrow(targets))) {
      symbol <- as.character(targets$symbol[[j]])
      matrix_rows[[length(matrix_rows) + 1L]] <- data.frame(
        pathway_id = pid,
        pathway_name = meta$pathway_name,
        project_target_id = as.character(targets$project_target_id[[j]]),
        symbol = symbol,
        is_member = symbol %in% meta$members,
        stringsAsFactors = FALSE
      )
    }
  }
  membership_matrix <- if (length(matrix_rows) == 0) {
    empty_membership_matrix()
  } else {
    do.call(rbind, matrix_rows)
  }

  list(
    targets = targets,
    pathways = pathways,
    membership_matrix = membership_matrix,
    excluded_targets = excluded,
    failures = failures,
    provenance = list(
      source = "Reactome",
      reactome_release = reactome_release,
      membership_scope = REACTOME_MEMBERSHIP_SCOPE,
      retrieved_at = version_provenance$retrieved_at %||% Sys.time(),
      cache_status = version_provenance$cache_status %||% NA_character_,
      identifier_type = "UniProt accession",
      retrieval_method = "content_service_uniprot_mapping",
      source_url = REACTOME_CONTENT_SERVICE,
      cache_key_family = "reactome:membership:<release>:<uniprot>:lowest_level",
      version_url = reactome_version_url()
    )
  )
}

normalize_pathway_selection <- function(selected_ids, retrieved_ids, previous_ids = NULL) {
  retrieved_ids <- as.character(retrieved_ids)
  selected_ids <- intersect(as.character(selected_ids %||% character()), retrieved_ids)
  if (length(selected_ids) >= 1L) {
    return(selected_ids)
  }
  previous_ids <- intersect(as.character(previous_ids %||% character()), retrieved_ids)
  if (length(previous_ids) >= 1L) {
    return(previous_ids)
  }
  retrieved_ids
}

filter_pathway_visual <- function(
  overlap,
  selected_ids,
  shared_only = FALSE,
  max_rows = PATHWAY_MATRIX_ROW_CAP
) {
  selected_ids <- as.character(selected_ids)
  targets <- overlap$targets[overlap$targets$project_target_id %in% selected_ids, , drop = FALSE]
  matrix <- overlap$membership_matrix[
    overlap$membership_matrix$project_target_id %in% selected_ids,
    ,
    drop = FALSE
  ]

  if (nrow(matrix) == 0) {
    pathways <- overlap$pathways[0, , drop = FALSE]
    return(list(
      targets_visible = targets,
      pathways_visible = pathways,
      membership_matrix_visible = matrix,
      n_hidden = 0L
    ))
  }

  counts <- stats::aggregate(
    is_member ~ pathway_id,
    data = matrix,
    FUN = function(x) sum(isTRUE(x) | x)
  )
  names(counts)[2] <- "selected_count"
  matrix <- merge(matrix, counts, by = "pathway_id", all.x = TRUE)
  if (isTRUE(shared_only)) {
    matrix <- matrix[matrix$selected_count >= 2L, , drop = FALSE]
  }

  visible_ids <- unique(matrix$pathway_id[matrix$selected_count > 0])
  pathways <- overlap$pathways[overlap$pathways$pathway_id %in% visible_ids, , drop = FALSE]
  if (nrow(pathways) > 0) {
    selected_counts <- counts$selected_count[match(pathways$pathway_id, counts$pathway_id)]
    pathways$selected_count <- ifelse(is.na(selected_counts), 0L, selected_counts)
    pathways <- pathways[order(-pathways$selected_count, pathways$pathway_name, pathways$pathway_id), , drop = FALSE]
  }

  n_hidden <- max(nrow(pathways) - as.integer(max_rows), 0L)
  pathways_figure <- if (nrow(pathways) > max_rows) {
    pathways[seq_len(max_rows), , drop = FALSE]
  } else {
    pathways
  }

  figure_matrix <- matrix[matrix$pathway_id %in% pathways_figure$pathway_id, , drop = FALSE]
  list(
    targets_visible = targets,
    pathways_visible = pathways_figure,
    pathways_all_visible = pathways,
    membership_matrix_visible = figure_matrix,
    n_hidden = n_hidden
  )
}

is_shared_pathway <- function(matched_target_count) {
  as.integer(matched_target_count) >= 2L
}
