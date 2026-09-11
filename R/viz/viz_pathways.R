build_pathway_membership_plot_data <- function(membership_matrix, pathway_order) {
  if (is.null(membership_matrix) || nrow(membership_matrix) == 0) {
    return(NULL)
  }
  data <- membership_matrix
  data$pathway_name <- factor(data$pathway_name, levels = rev(pathway_order))
  data$symbol <- factor(data$symbol, levels = unique(as.character(data$symbol)))
  data$cell_label <- ifelse(isTRUE(data$is_member) | data$is_member, "\u25CF", "\u2014")
  data
}

plot_pathway_membership_matrix <- function(membership_matrix, pathway_order) {
  data <- build_pathway_membership_plot_data(membership_matrix, pathway_order)
  if (is.null(data) || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  present <- data[data$is_member %in% TRUE, , drop = FALSE]
  absent <- data[!data$is_member %in% TRUE, , drop = FALSE]

  ggplot2::ggplot(data, ggplot2::aes(x = symbol, y = pathway_name)) +
    ggplot2::geom_tile(fill = tw_plot_colors$primary_soft, colour = tw_plot_colors$bg, width = 0.96, height = 0.96) +
    ggplot2::geom_point(
      data = present,
      colour = tw_plot_colors$primary,
      size = 3.1
    ) +
    ggplot2::geom_text(
      data = absent,
      ggplot2::aes(label = cell_label),
      colour = tw_plot_colors$muted,
      size = 3.2
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    tw_plot_theme() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9)
    )
}
