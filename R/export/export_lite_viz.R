export_lite_figure <- function(inner_html, alt = "", caption = NULL) {
  if (!has_display_text(inner_html)) {
    return("")
  }
  started <- proc.time()[["elapsed"]]
  cap <- if (has_display_text(caption)) {
    sprintf("<figcaption>%s</figcaption>", html_esc(caption))
  } else {
    ""
  }
  html <- sprintf(
    "<figure class=\"tw-viz\" role=\"img\" aria-label=\"%s\">%s%s</figure>",
    html_esc_attr(alt),
    inner_html,
    cap
  )
  metrics <- getOption("tw.export.metrics")
  if (is.environment(metrics)) {
    elapsed_ms <- 1000 * (proc.time()[["elapsed"]] - started)
    metrics$plot_ms <- metrics$plot_ms + elapsed_ms
    metrics$plot_n <- metrics$plot_n + 1L
    metrics$ggsave_n <- as.integer(metrics$ggsave_n %||% 0L)
  }
  html
}

export_lite_bar_rows <- function(labels, values, formatted = NULL, max_value = 1, unit_suffix = "") {
  if (length(labels) == 0) {
    return("")
  }
  max_value <- suppressWarnings(as.numeric(max_value)[[1]])
  if (!is.finite(max_value) || max_value <= 0) {
    max_value <- 1
  }
  rows <- vapply(seq_along(labels), function(i) {
    val <- suppressWarnings(as.numeric(values[[i]])[[1]])
    label <- html_esc(labels[[i]])
    if (length(val) != 1L || is.na(val)) {
      shown <- if (!is.null(formatted)) as.character(formatted[[i]] %||% "Unavailable") else "Unavailable"
      return(sprintf(
        "<div class=\"tw-bar-row\"><span class=\"tw-bar-lab\">%s</span><span class=\"tw-bar-track tw-bar-missing\"></span><span class=\"tw-bar-val\">%s</span></div>",
        label,
        html_esc(shown)
      ))
    }
    pct <- max(0, min(100, 100 * val / max_value))
    shown <- if (!is.null(formatted)) {
      as.character(formatted[[i]])
    } else {
      paste0(trimws(format(val, digits = 4, scientific = FALSE)), unit_suffix)
    }
    sprintf(
      "<div class=\"tw-bar-row\"><span class=\"tw-bar-lab\">%s</span><span class=\"tw-bar-track\"><span class=\"tw-bar-fill\" style=\"width:%.2f%%\"></span></span><span class=\"tw-bar-val\">%s</span></div>",
      label,
      pct,
      html_esc(shown)
    )
  }, character(1))
  paste0("<div class=\"tw-bars\">", paste(rows, collapse = ""), "</div>")
}

export_lite_genomic_html <- function(genomic, symbol = NA_character_) {
  data <- build_genomic_plot_data(genomic, symbol)
  if (is.null(data)) {
    return("")
  }
  span <- data$axis_max - data$axis_min
  if (!is.finite(span) || span <= 0) {
    return("")
  }
  x <- function(bp) 40 + 560 * (bp - data$axis_min) / span
  x1 <- x(data$start)
  x2 <- x(data$end)
  if (x2 < x1) {
    tmp <- x1
    x1 <- x2
    x2 <- tmp
  }
  arrow <- if (identical(data$direction, "negative")) {
    sprintf("<polygon points=\"%.1f,28 %.1f,40 %.1f,28\" fill=\"%s\" />", x1, x1 - 8, x1, tw_plot_colors$accent)
  } else {
    sprintf("<polygon points=\"%.1f,28 %.1f,40 %.1f,28\" fill=\"%s\" />", x2, x2 + 8, x2, tw_plot_colors$accent)
  }
  gene_label <- if (has_display_text(data$symbol)) {
    paste(data$symbol, data$chromosome)
  } else {
    as.character(data$chromosome)
  }
  svg <- paste0(
    "<svg class=\"tw-svg\" viewBox=\"0 0 640 88\" xmlns=\"http://www.w3.org/2000/svg\" aria-hidden=\"true\">",
    sprintf("<line x1=\"40\" y1=\"34\" x2=\"600\" y2=\"34\" stroke=\"%s\" stroke-width=\"2\" />", tw_plot_colors$grid),
    sprintf("<rect x=\"%.1f\" y=\"24\" width=\"%.1f\" height=\"20\" fill=\"%s\" />", x1, max(x2 - x1, 2), tw_plot_colors$primary),
    arrow,
    sprintf("<text x=\"320\" y=\"16\" text-anchor=\"middle\" fill=\"%s\" font-size=\"13\">%s</text>", tw_plot_colors$text, html_esc(gene_label)),
    sprintf("<text x=\"%.1f\" y=\"62\" fill=\"%s\" font-size=\"12\">%s</text>", x1, tw_plot_colors$text, html_esc(data$start_label)),
    sprintf("<text x=\"%.1f\" y=\"62\" text-anchor=\"end\" fill=\"%s\" font-size=\"12\">%s</text>", x2, tw_plot_colors$text, html_esc(data$end_label)),
    sprintf("<text x=\"%.1f\" y=\"78\" fill=\"%s\" font-size=\"11\">%s</text>", x1, tw_plot_colors$muted, html_esc(data$start_bp_label)),
    sprintf("<text x=\"%.1f\" y=\"78\" text-anchor=\"end\" fill=\"%s\" font-size=\"11\">%s</text>", x2, tw_plot_colors$muted, html_esc(data$end_bp_label)),
    "</svg>"
  )
  export_lite_figure(
    svg,
    alt = sprintf("Genomic locus for %s", symbol %||% "target"),
    caption = "Genomic context from captured Ensembl coordinates. Source: Ensembl"
  )
}

export_lite_ot_evidence_html <- function(scores, symbol, disease_name) {
  data <- build_ot_evidence_plot_data(scores)
  if (is.null(data)) {
    return("")
  }
  data <- data[order(data$score, data$datatype_label), , drop = FALSE]
  inner <- export_lite_bar_rows(
    as.character(data$datatype_label),
    data$score,
    formatted = sprintf("%.2f", data$score),
    max_value = 1
  )
  export_lite_figure(
    inner,
    alt = sprintf("Open Targets data-type association scores for %s", symbol %||% "target"),
    caption = "Open Targets data-type association scores. Numeric labels are the captured scores."
  )
}

export_lite_ot_scores_html <- function(targets, symbol_order = NULL) {
  if (is.null(targets) || nrow(targets) == 0) {
    return("")
  }
  data <- export_reorder_symbol_df(targets, symbol_order)
  if (nrow(data) == 0) {
    return("")
  }
  inner <- export_lite_bar_rows(
    as.character(data$symbol),
    data$overall_direct_score,
    formatted = ifelse(
      is.na(data$overall_direct_score),
      "Unavailable",
      sprintf("%.3f", data$overall_direct_score)
    ),
    max_value = 1
  )
  export_lite_figure(
    inner,
    alt = "Open Targets overall direct association scores by target",
    caption = "Bars follow project target order. This is not a recommendation or ranking."
  )
}

export_lite_ot_heatmap_html <- function(datatype_matrix, disease_name = NA_character_, symbol_order = NULL) {
  data <- build_ot_comparison_heatmap_data(datatype_matrix)
  if (is.null(data)) {
    return("")
  }
  types <- levels(data$datatype_label)
  if (is.null(types)) {
    types <- unique(as.character(data$datatype_label))
  }
  types <- rev(types)
  symbols <- unique(as.character(data$symbol))
  if (length(symbol_order) > 0L) {
    symbols <- c(intersect(symbol_order, symbols), setdiff(symbols, symbol_order))
  }
  head <- paste0(
    "<th></th>",
    paste(sprintf("<th>%s</th>", vapply(symbols, html_esc, character(1))), collapse = "")
  )
  body <- paste(vapply(types, function(type) {
    cells <- paste(vapply(symbols, function(sym) {
      row <- data[as.character(data$datatype_label) == type & as.character(data$symbol) == sym, , drop = FALSE]
      if (nrow(row) < 1L) {
        return("<td class=\"tw-heat-missing\">\u2014</td>")
      }
      if (isTRUE(row$is_missing[[1]]) || identical(as.character(row$fill_kind[[1]]), "missing")) {
        return("<td class=\"tw-heat-missing\">\u2014</td>")
      }
      score <- suppressWarnings(as.numeric(row$score[[1]])[[1]])
      fill <- tw_heatmap_fill(score)
      ink <- heatmap_label_colour(score, FALSE)
      sprintf(
        "<td class=\"tw-heat-cell\" style=\"background:%s;color:%s\">%s</td>",
        fill,
        ink,
        html_esc(sprintf("%.2f", score))
      )
    }, character(1)), collapse = "")
    sprintf("<tr><th>%s</th>%s</tr>", html_esc(type), cells)
  }, character(1)), collapse = "")
  inner <- sprintf("<table class=\"tw-heat\"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>", head, body)
  export_lite_figure(
    inner,
    alt = "Open Targets comparison heatmap of data-type association scores",
    caption = "Missing values are shown as a dash, distinct from a returned score of zero. Source: Open Targets"
  )
}

export_lite_pathway_html <- function(membership_matrix, pathway_order, symbol_order = NULL) {
  data <- build_pathway_membership_plot_data(membership_matrix, pathway_order)
  if (is.null(data)) {
    return("")
  }
  pathways <- as.character(pathway_order)
  symbols <- unique(as.character(data$symbol))
  if (length(symbol_order) > 0L) {
    symbols <- c(intersect(symbol_order, symbols), setdiff(symbols, symbol_order))
  }
  head <- paste0(
    "<th>Pathway</th>",
    paste(sprintf("<th>%s</th>", vapply(symbols, html_esc, character(1))), collapse = "")
  )
  body <- paste(vapply(pathways, function(pw) {
    cells <- paste(vapply(symbols, function(sym) {
      row <- data[as.character(data$pathway_name) == pw & as.character(data$symbol) == sym, , drop = FALSE]
      member <- nrow(row) >= 1L && isTRUE(row$is_member[[1]])
      if (isTRUE(member)) {
        "<td class=\"tw-member-yes\">Present</td>"
      } else {
        "<td class=\"tw-member-no\">\u2014</td>"
      }
    }, character(1)), collapse = "")
    sprintf("<tr><th>%s</th>%s</tr>", html_esc(pw), cells)
  }, character(1)), collapse = "")
  inner <- sprintf("<table class=\"tw-matrix\"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>", head, body)
  export_lite_figure(
    inner,
    alt = "Reactome pathway membership matrix",
    caption = "Filled marker = membership present; dash = not present in the retrieved membership set. This is not enrichment."
  )
}

export_lite_trend_html <- function(trend, symbol, year_input_id = NULL, selected_year = NULL) {
  display <- literature_display_trend(trend)
  if (is.null(display) || nrow(display) == 0) {
    return("")
  }
  years <- as.integer(display$year)
  if (anyDuplicated(years) > 0) {
    return("")
  }
  ok <- literature_ok_trend_rows(display)
  n_ok <- nrow(ok)
  if (n_ok == 0L) {
    return("")
  }
  if (n_ok == 1L) {
    year <- as.integer(ok$year[[1]])
    count <- as.integer(ok$record_count[[1]])
    partial <- isTRUE(ok$is_partial_year[[1]])
    inner <- sprintf(
      "<p class=\"literature-single-year\">%s matched PubMed records in calendar year %s%s. Historical annual counts were not retrieved. This is an annual count, not a cumulative total.</p>",
      html_esc(format(as.integer(count), big.mark = ",", scientific = FALSE, trim = TRUE)),
      html_esc(as.character(year)),
      if (isTRUE(partial)) " (partial year)" else ""
    )
    return(export_lite_figure(
      inner,
      alt = sprintf("Annual PubMed record count for %s in %s", symbol, year),
      caption = "Annual PubMed record count, not cumulative. Source: NCBI PubMed"
    ))
  }
  inner_svg <- export_lite_annual_trend_svg(
    display,
    year_input_id = year_input_id,
    selected_year = selected_year
  )
  figure <- export_lite_figure(
    inner_svg,
    alt = sprintf("Annual matched PubMed records by calendar year for %s", symbol),
    caption = "Annual PubMed record count, not cumulative. Missing years are omitted, not shown as zero. Orange marks the current calendar year as partial, not higher importance. Source: NCBI PubMed"
  )
  table_html <- export_lite_annual_trend_table(display, year_input_id = year_input_id, selected_year = selected_year)
  paste0(
    figure,
    sprintf(
      "<details class=\"literature-annual-counts\"><summary>View annual counts</summary>%s</details>",
      table_html
    )
  )
}

literature_ok_trend_rows <- function(trend) {
  if (is.null(trend) || nrow(trend) == 0) {
    return(trend)
  }
  trend[
    trend$status == "ok" & !is.na(trend$record_count),
    ,
    drop = FALSE
  ]
}

literature_display_trend <- function(trend) {
  if (is.null(trend) || nrow(trend) == 0) {
    return(trend)
  }
  trend[order(as.integer(trend$year), trend$year), , drop = FALSE]
}

export_lite_annual_trend_svg <- function(trend, year_input_id = NULL, selected_year = NULL) {
  ok_trend <- literature_ok_trend_rows(trend)
  if (is.null(ok_trend) || nrow(ok_trend) == 0) {
    return("")
  }
  if (anyDuplicated(as.integer(ok_trend$year)) > 0) {
    return("")
  }
  years <- as.integer(ok_trend$year)
  counts <- suppressWarnings(as.numeric(ok_trend$record_count))
  xmin <- min(years, na.rm = TRUE)
  xmax <- max(years, na.rm = TRUE)
  span <- max(1L, xmax - xmin)
  ymax <- max(counts, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) {
    ymax <- 1
  }
  pad_l <- 48
  pad_r <- 12
  pad_t <- 14
  pad_b <- 36
  w <- 640
  h <- 220
  inner_w <- w - pad_l - pad_r
  inner_h <- h - pad_t - pad_b
  x_at <- function(year) pad_l + inner_w * as.numeric(year - xmin) / span
  y_at <- function(v) pad_t + inner_h * (1 - as.numeric(v) / ymax)
  bar_w <- min(36, max(10, inner_w / max(length(years), 1L) * 0.55))
  ticks <- pretty(c(0, ymax), n = 3)
  ticks <- ticks[ticks >= 0 & ticks <= ymax * 1.05]
  grid <- paste(vapply(ticks, function(tick) {
    y <- y_at(min(tick, ymax))
    sprintf(
      "<line x1=\"%s\" y1=\"%.1f\" x2=\"%s\" y2=\"%.1f\" class=\"tw-trend-grid\" />",
      pad_l,
      y,
      w - pad_r,
      y
    )
  }, character(1)), collapse = "")
  ylab <- paste(vapply(ticks, function(tick) {
    sprintf(
      "<text x=\"%s\" y=\"%.1f\" class=\"tw-trend-ylab\">%s</text>",
      pad_l - 8,
      y_at(min(tick, ymax)) + 4,
      html_esc(format(tick, big.mark = ",", scientific = FALSE, trim = TRUE))
    )
  }, character(1)), collapse = "")
  interactive <- has_display_text(year_input_id)
  selected_year_value <- as.character(selected_year %||% "")
  bars <- paste(vapply(seq_len(nrow(ok_trend)), function(i) {
    year <- as.integer(years[[i]])
    count <- counts[[i]]
    count_label <- format(as.integer(count), big.mark = ",", scientific = FALSE, trim = TRUE)
    hover <- sprintf("%s \u00b7 %s matched records", year, count_label)
    selected <- identical(as.character(year), selected_year_value)
    cls <- "tw-trend-bar"
    if (isTRUE(ok_trend$is_partial_year[[i]])) {
      cls <- paste(cls, "is-partial")
    }
    if (isTRUE(selected)) {
      cls <- paste(cls, "is-selected")
    }
    x <- x_at(year) - bar_w / 2
    y <- y_at(count)
    height <- max(0, pad_t + inner_h - y)
    rect <- sprintf(
      "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" class=\"%s\"></rect><title>%s</title>",
      x,
      y,
      bar_w,
      height,
      cls,
      html_esc(hover)
    )
    if (!isTRUE(interactive)) {
      return(rect)
    }
    sprintf(
      "<g class=\"tw-trend-hit%s\" role=\"button\" tabindex=\"0\" data-literature-year=\"%s\" data-tw-input=\"%s\" data-tw-value=\"%s\" aria-label=\"%s\">%s</g>",
      if (isTRUE(selected)) " is-selected" else "",
      year,
      html_esc(as.character(year_input_id)),
      year,
      html_esc(hover),
      rect
    )
  }, character(1)), collapse = "")
  xlabels <- paste(vapply(seq_len(nrow(ok_trend)), function(i) {
    label <- if (isTRUE(ok_trend$is_partial_year[[i]])) paste0(years[[i]], "*") else as.character(years[[i]])
    sprintf(
      "<text x=\"%.1f\" y=\"%s\" class=\"tw-trend-xlab\">%s</text>",
      x_at(years[[i]]),
      h - 10,
      html_esc(label)
    )
  }, character(1)), collapse = "")
  sprintf(
    "<svg class=\"tw-trend-svg\" viewBox=\"0 0 %s %s\" role=\"img\" %s>%s%s%s%s</svg>",
    w,
    h,
    if (isTRUE(interactive)) "focusable=\"true\"" else "aria-hidden=\"true\" focusable=\"false\"",
    grid,
    ylab,
    bars,
    xlabels
  )
}

export_lite_annual_trend_table <- function(trend, year_input_id = NULL, selected_year = NULL) {
  if (!is.null(trend) && nrow(trend) > 0 && anyDuplicated(as.integer(trend$year)) > 0) {
    return("")
  }
  interactive <- has_display_text(year_input_id)
  selected_year_value <- as.character(selected_year %||% "")
  rows <- paste(vapply(seq_len(nrow(trend)), function(i) {
    year <- as.integer(trend$year[[i]])
    ok <- identical(as.character(trend$status[[i]]), "ok") && !is.na(trend$record_count[[i]])
    count <- if (isTRUE(ok)) {
      format(as.integer(trend$record_count[[i]]), big.mark = ",", scientific = FALSE, trim = TRUE)
    } else {
      "Not retrieved"
    }
    note <- if (isTRUE(trend$is_partial_year[[i]])) "partial year" else "full calendar year"
    selected <- identical(as.character(year), selected_year_value)
    attrs <- if (isTRUE(interactive) && isTRUE(ok)) {
      sprintf(
        " class=\"tw-trend-year-row%s\" data-literature-year=\"%s\" data-tw-input=\"%s\" data-tw-value=\"%s\" tabindex=\"0\" role=\"button\"",
        if (isTRUE(selected)) " is-selected" else "",
        year,
        html_esc(as.character(year_input_id)),
        year
      )
    } else if (isTRUE(selected)) {
      " class=\"is-selected\""
    } else {
      ""
    }
    sprintf(
      "<tr%s><th scope=\"row\">%s</th><td>%s</td><td>%s</td></tr>",
      attrs,
      html_esc(as.character(year)),
      html_esc(count),
      html_esc(note)
    )
  }, character(1)), collapse = "")
  sprintf(
    "<table class=\"tw-trend-table\"><caption>Annual matched PubMed records</caption><thead><tr><th>Calendar year</th><th>Annual record count</th><th>Coverage</th></tr></thead><tbody>%s</tbody></table>",
    rows
  )
}

export_lite_pubmed_counts_html <- function(counts) {
  if (is.null(counts) || nrow(counts) == 0) {
    return("")
  }
  ymax <- max(as.numeric(counts$pubmed_record_count), na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) {
    ymax <- 1
  }
  inner <- export_lite_bar_rows(
    as.character(counts$symbol),
    counts$pubmed_record_count,
    formatted = as.character(as.integer(counts$pubmed_record_count)),
    max_value = ymax
  )
  export_lite_figure(
    inner,
    alt = "PubMed record counts by target",
    caption = "Literature volume is not target importance. Source: NCBI PubMed"
  )
}

export_lite_pdb_counts_html <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) {
    return("")
  }
  ymax <- max(as.numeric(summary$n_pdb_entries), na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) {
    ymax <- 1
  }
  inner <- export_lite_bar_rows(
    as.character(summary$symbol),
    summary$n_pdb_entries,
    formatted = as.character(summary$n_pdb_entries),
    max_value = ymax
  )
  export_lite_figure(
    inner,
    alt = "Experimental PDB entry counts by target",
    caption = "Experimental PDB entry counts from captured RCSB metadata. Source: RCSB PDB"
  )
}

export_lite_coverage_html <- function(records, uniprot_length, symbol) {
  payload <- structure_coverage_plot_data(records, uniprot_length)
  if (is.null(payload)) {
    return("")
  }
  n <- payload$n_shown
  len <- payload$uniprot_length
  row_h <- 18
  height <- 28 + n * row_h
  segs <- payload$segments
  y_labels <- payload$y_labels
  bg <- paste(vapply(seq_len(n), function(i) {
    y <- 20 + (i - 1) * row_h
    sprintf(
      "<rect x=\"120\" y=\"%s\" width=\"500\" height=\"14\" fill=\"%s\" />",
      y,
      tw_plot_colors$primary_soft
    )
  }, character(1)), collapse = "")
  labels <- paste(vapply(seq_len(n), function(i) {
    y <- 31 + (i - 1) * row_h
    sprintf(
      "<text x=\"116\" y=\"%s\" text-anchor=\"end\" font-size=\"10\" fill=\"%s\">%s</text>",
      y,
      tw_plot_colors$text,
      html_esc(y_labels[[i]])
    )
  }, character(1)), collapse = "")
  rects <- ""
  if (!is.null(segs) && nrow(segs) > 0) {
    rects <- paste(vapply(seq_len(nrow(segs)), function(i) {
      xmin <- segs$xmin[[i]]
      xmax <- segs$xmax[[i]]
      y_idx <- n - (segs$ymin[[i]] + segs$ymax[[i]]) / 2 + 1
      y_idx <- max(1, min(n, round(y_idx)))
      x <- 120 + 500 * (xmin - 1) / len
      w <- 500 * max(xmax - xmin, 1) / len
      y <- 20 + (y_idx - 1) * row_h
      sprintf(
        "<rect x=\"%.2f\" y=\"%s\" width=\"%.2f\" height=\"14\" fill=\"%s\" />",
        x,
        y,
        max(w, 1),
        tw_plot_colors$primary
      )
    }, character(1)), collapse = "")
  }
  svg <- paste0(
    sprintf("<svg class=\"tw-svg\" viewBox=\"0 0 640 %s\" xmlns=\"http://www.w3.org/2000/svg\" aria-hidden=\"true\">", height + 16),
    bg,
    rects,
    labels,
    sprintf(
      "<text x=\"370\" y=\"%s\" text-anchor=\"middle\" font-size=\"11\" fill=\"%s\">UniProt sequence (1\u2013%s)</text>",
      height + 10,
      tw_plot_colors$muted,
      len
    ),
    "</svg>"
  )
  export_lite_figure(
    svg,
    alt = sprintf("Sequence coverage of experimental structures for %s", symbol %||% "target"),
    caption = sprintf("Sequence coverage for %s. Coordinate files are not embedded.", symbol %||% "target")
  )
}
