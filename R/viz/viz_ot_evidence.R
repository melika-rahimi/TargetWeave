build_ot_evidence_plot_data <- function(datatype_scores) {
  if (is.null(datatype_scores) || nrow(datatype_scores) == 0) {
    return(NULL)
  }

  present <- datatype_scores[!is.na(datatype_scores$score), , drop = FALSE]
  if (nrow(present) == 0) {
    return(NULL)
  }

  present$datatype_label <- factor(
    present$datatype_label,
    levels = present$datatype_label[order(present$score, present$datatype_label)]
  )
  present
}

plot_ot_evidence_profile <- function(
  datatype_scores,
  symbol = NA_character_,
  disease_name = NA_character_
) {
  data <- build_ot_evidence_plot_data(datatype_scores)
  if (is.null(data)) {
    return(NULL)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  ggplot2::ggplot(data, ggplot2::aes(x = score, y = datatype_label)) +
    ggplot2::geom_col(fill = tw_plot_colors$primary, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", score)),
      hjust = -0.15,
      size = 3.1,
      colour = tw_plot_colors$text
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0)) +
    ggplot2::labs(x = "Association score", y = NULL, title = NULL, subtitle = NULL) +
    tw_plot_theme()
}
