plot_publication_trend <- function(trend) {
  if (is.null(trend) || nrow(trend) == 0 || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  data <- trend[trend$status == "ok", , drop = FALSE]
  if (nrow(data) == 0) {
    return(NULL)
  }
  data$year <- as.integer(data$year)
  data$record_count <- as.integer(data$record_count)
  data$year_label <- ifelse(isTRUE(data$is_partial_year), paste0(data$year, "*"), as.character(data$year))

  ggplot2::ggplot(data, ggplot2::aes(x = year, y = record_count)) +
    ggplot2::geom_col(
      fill = tw_plot_colors$primary,
      width = 0.72,
      colour = NA
    ) +
    ggplot2::geom_text(
      data = data[data$is_partial_year %in% TRUE, , drop = FALSE],
      ggplot2::aes(label = "partial year"),
      vjust = -0.4,
      size = 3,
      colour = tw_plot_colors$muted
    ) +
    ggplot2::scale_x_continuous(breaks = data$year, labels = data$year_label) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "PubMed records") +
    tw_plot_theme() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = tw_plot_colors$grid, linewidth = 0.3)
    )
}

plot_pubmed_counts_by_target <- function(target_counts) {
  if (is.null(target_counts) || nrow(target_counts) == 0 || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  data <- target_counts
  data$symbol <- factor(data$symbol, levels = unique(as.character(data$symbol)))
  ggplot2::ggplot(data, ggplot2::aes(x = symbol, y = pubmed_record_count)) +
    ggplot2::geom_col(fill = tw_plot_colors$primary, width = 0.6, colour = NA) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = NULL, y = "PubMed records") +
    tw_plot_theme() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = tw_plot_colors$grid, linewidth = 0.3)
    )
}
