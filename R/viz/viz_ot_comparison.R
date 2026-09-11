build_ot_comparison_heatmap_data <- function(datatype_matrix) {
  if (is.null(datatype_matrix) || nrow(datatype_matrix) == 0) {
    return(NULL)
  }

  data <- datatype_matrix
  data$cell_label <- ifelse(
    data$is_missing,
    "\u2014",
    sprintf("%.2f", data$score)
  )
  data$fill_kind <- ifelse(data$is_missing, "missing", "score")
  data$label_colour <- vapply(
    seq_len(nrow(data)),
    function(i) heatmap_label_colour(data$score[[i]], isTRUE(data$is_missing[[i]])),
    character(1)
  )
  ordered_ids <- comparison_datatype_order(unique(as.character(data$datatype_id)))
  label_map <- unique(data[, c("datatype_id", "datatype_label"), drop = FALSE])
  type_levels <- label_map$datatype_label[match(ordered_ids, label_map$datatype_id)]
  type_levels <- type_levels[!is.na(type_levels)]
  data$datatype_label <- factor(data$datatype_label, levels = rev(type_levels))
  data$symbol <- factor(data$symbol, levels = unique(as.character(data$symbol)))
  data
}

plot_ot_comparison_heatmap <- function(datatype_matrix, disease_name = NA_character_) {
  data <- build_ot_comparison_heatmap_data(datatype_matrix)
  if (is.null(data) || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  scored <- data[data$fill_kind == "score", , drop = FALSE]
  missing <- data[data$fill_kind == "missing", , drop = FALSE]

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = symbol, y = datatype_label)) +
    ggplot2::geom_tile(data = missing, fill = tw_plot_colors$missing, colour = tw_plot_colors$bg, width = 0.96, height = 0.96)

  if (nrow(scored) > 0) {
    plot <- plot +
      ggplot2::geom_tile(
        data = scored,
        ggplot2::aes(fill = score),
        colour = tw_plot_colors$bg,
        width = 0.96,
        height = 0.96
      ) +
      ggplot2::scale_fill_gradientn(
        name = "Score",
        colours = tw_plot_colors$heatmap_stops,
        values = tw_plot_colors$heatmap_positions,
        limits = c(0, 1),
        na.value = tw_plot_colors$missing
      )
  }

  plot +
    ggplot2::geom_text(
      ggplot2::aes(label = cell_label, colour = label_colour),
      size = 3
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
    tw_plot_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

plot_ot_overall_scores <- function(targets) {
  if (is.null(targets) || nrow(targets) == 0 || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  data <- targets[!is.na(targets$overall_direct_score), , drop = FALSE]
  if (nrow(data) == 0) {
    return(NULL)
  }

  data <- data[order(data$overall_direct_score, data$symbol), , drop = FALSE]
  data$symbol <- factor(data$symbol, levels = as.character(data$symbol))

  ggplot2::ggplot(data, ggplot2::aes(x = overall_direct_score, y = symbol)) +
    ggplot2::geom_col(fill = tw_plot_colors$primary, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", overall_direct_score)),
      hjust = -0.15,
      size = 3.1,
      colour = tw_plot_colors$text
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0)) +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = "Open Targets overall association score",
      y = NULL
    ) +
    tw_plot_theme()
}
