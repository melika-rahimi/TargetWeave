structure_coverage_plot_data <- function(records, uniprot_length, max_rows = STRUCTURE_COVERAGE_ROW_CAP) {
  if (length(records) == 0 || is.na(as.integer(uniprot_length)) || as.integer(uniprot_length) <= 0L) {
    return(NULL)
  }
  shown <- records[seq_len(min(length(records), as.integer(max_rows)))]
  n <- length(shown)
  rows <- list()
  labels <- character()
  for (i in seq_along(shown)) {
    item <- shown[[i]]
    label <- item$polymer_entity$entity_identifier %||% sprintf("%s_%s", item$pdb_id, item$polymer_entity$entity_id)
    labels <- c(labels, label)
    ranges <- item$coverage$covered_ranges
    y <- n - i + 1L
    if (is.null(ranges) || nrow(ranges) == 0) {
      next
    }
    for (j in seq_len(nrow(ranges))) {
      rows[[length(rows) + 1L]] <- data.frame(
        structure_label = label,
        ymin = y - 0.35,
        ymax = y + 0.35,
        xmin = ranges$begin[[j]],
        xmax = ranges$end[[j]] + 1,
        selected = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  data <- do.call(rbind, rows)
  list(
    segments = data,
    uniprot_length = as.integer(uniprot_length),
    n_shown = n,
    n_total = length(records),
    y_labels = labels
  )
}

plot_structure_coverage <- function(records, uniprot_length, selected_id = NULL, max_rows = STRUCTURE_COVERAGE_ROW_CAP) {
  payload <- structure_coverage_plot_data(records, uniprot_length, max_rows = max_rows)
  if (is.null(payload) || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  segs <- payload$segments
  if (has_display_text(selected_id)) {
    segs$selected <- as.character(segs$structure_label) == as.character(selected_id)
  }
  segs$fill <- ifelse(segs$selected, tw_plot_colors$accent, tw_plot_colors$primary)
  y_labels <- payload$y_labels
  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = data.frame(
        xmin = 1,
        xmax = payload$uniprot_length + 1,
        ymin = 0.5,
        ymax = length(y_labels) + 0.5
      ),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = tw_plot_colors$primary_soft,
      colour = NA
    ) +
    ggplot2::geom_rect(
      data = segs,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = segs$fill,
      colour = NA
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(y_labels),
      labels = rev(y_labels),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_x_continuous(
      name = sprintf("UniProt sequence (1\u2013%s)", payload$uniprot_length),
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::labs(y = NULL) +
    tw_plot_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 8)
    )
}

plot_structure_entry_counts <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0 || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  data <- summary
  data$symbol <- factor(data$symbol, levels = unique(as.character(data$symbol)))
  ggplot2::ggplot(data, ggplot2::aes(x = symbol, y = n_pdb_entries)) +
    ggplot2::geom_col(fill = tw_plot_colors$primary, width = 0.6, colour = NA) +
    ggplot2::labs(x = NULL, y = "Experimental PDB entries") +
    tw_plot_theme() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}
