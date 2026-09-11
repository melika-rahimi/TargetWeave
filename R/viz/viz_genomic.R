build_genomic_plot_data <- function(genomic, symbol = NA_character_) {
  chromosome <- genomic$chromosome %||% genomic$seq_region
  start <- genomic$start
  end <- genomic$end
  strand <- genomic$strand

  if (!has_display_text(chromosome) ||
      !is_present_scalar(start) ||
      !is_present_scalar(end)) {
    return(NULL)
  }

  start <- as.numeric(start)
  end <- as.numeric(end)
  if (!is.finite(start) || !is.finite(end) || end < start) {
    return(NULL)
  }

  strand_value <- if (is_present_scalar(strand)) as.integer(strand) else NA_integer_
  span_bp <- as.integer(end - start + 1)
  padding <- max(span_bp * 0.22, 1)
  direction <- if (is_present_scalar(strand_value) && strand_value < 0) {
    "negative"
  } else if (is_present_scalar(strand_value) && strand_value > 0) {
    "positive"
  } else {
    "unspecified"
  }

  list(
    chromosome = as.character(chromosome),
    start = start,
    end = end,
    strand = strand_value,
    symbol = symbol %||% NA_character_,
    span_bp = span_bp,
    direction = direction,
    axis_min = start - padding,
    axis_max = end + padding,
    start_label = sprintf("%.2f Mb", start / 1e6),
    end_label = sprintf("%.2f Mb", end / 1e6),
    start_bp_label = format_bp(start),
    end_bp_label = format_bp(end),
    arrow_ends = if (identical(direction, "negative")) "first" else "last"
  )
}

format_bp <- function(n) {
  if (!is_present_scalar(n)) {
    return("Not provided")
  }
  prettyNum(as.integer(n), big.mark = ",", scientific = FALSE)
}

plot_genomic_context <- function(genomic, symbol = NA_character_) {
  data <- build_genomic_plot_data(genomic, symbol)
  if (is.null(data)) {
    return(NULL)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  strand_label <- if (identical(data$direction, "negative")) {
    "negative strand  \u27f5"
  } else if (identical(data$direction, "positive")) {
    "positive strand  \u27f6"
  } else {
    "strand not provided"
  }

  gene_label <- if (has_display_text(data$symbol)) {
    paste(data$symbol, strand_label)
  } else {
    strand_label
  }

  ggplot2::ggplot() +
    ggplot2::annotate(
      "segment",
      x = data$axis_min,
      xend = data$axis_max,
      y = 1,
      yend = 1,
      colour = tw_plot_colors$grid,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "segment",
      x = data$start,
      xend = data$end,
      y = 1,
      yend = 1,
      colour = tw_plot_colors$primary,
      linewidth = 11,
      lineend = "butt"
    ) +
    ggplot2::annotate(
      "segment",
      x = data$start,
      xend = data$end,
      y = 1,
      yend = 1,
      colour = tw_plot_colors$accent,
      linewidth = 1.15,
      lineend = "round",
      arrow = ggplot2::arrow(
        length = grid::unit(0.28, "cm"),
        ends = data$arrow_ends,
        type = "closed"
      )
    ) +
    ggplot2::annotate(
      "text",
      x = (data$start + data$end) / 2,
      y = 1.42,
      label = gene_label,
      colour = tw_plot_colors$text,
      size = 3.8
    ) +
    ggplot2::annotate(
      "text",
      x = data$start,
      y = 0.72,
      label = data$start_label,
      colour = tw_plot_colors$text,
      size = 3.3,
      hjust = 0,
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = data$end,
      y = 0.72,
      label = data$end_label,
      colour = tw_plot_colors$text,
      size = 3.3,
      hjust = 1,
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = data$start,
      y = 0.48,
      label = data$start_bp_label,
      colour = tw_plot_colors$muted,
      size = 2.9,
      hjust = 0
    ) +
    ggplot2::annotate(
      "text",
      x = data$end,
      y = 0.48,
      label = data$end_bp_label,
      colour = tw_plot_colors$muted,
      size = 2.9,
      hjust = 1
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0.28, 1.68),
      xlim = c(data$axis_min, data$axis_max),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::labs(
      title = sprintf("Chromosome %s", data$chromosome),
      subtitle = sprintf(
        "Span %s bp \u00b7 %s",
        format_bp(data$span_bp),
        strand_label
      )
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        colour = tw_plot_colors$text,
        size = 15
      ),
      plot.subtitle = ggplot2::element_text(
        colour = tw_plot_colors$muted,
        size = 11,
        margin = ggplot2::margin(b = 10)
      ),
      plot.margin = ggplot2::margin(14, 28, 16, 28),
      plot.background = ggplot2::element_rect(fill = tw_plot_colors$bg, colour = NA)
    )
}
