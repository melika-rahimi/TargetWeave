tw_plot_colors <- list(
  bg = "#FFFFFF",
  text = "#171B28",
  muted = "#667085",
  primary = "#18243D",
  primary_dark = "#10192B",
  primary_soft = "#E9EDF5",
  accent = "#B83280",
  heatmap_stops = c("#EEF1F6", "#C7CFDD", "#909DB7", "#5E6F91", "#344563"),
  heatmap_positions = c(0, 0.25, 0.50, 0.75, 1),
  missing = "#C5CDD8",
  grid = "#E6EBEE"
)

tw_hex_luminance <- function(hex) {
  hex <- gsub("^#", "", hex)
  rgb <- strtoi(c(
    substr(hex, 1, 2),
    substr(hex, 3, 4),
    substr(hex, 5, 6)
  ), 16L) / 255
  channel <- ifelse(
    rgb <= 0.04045,
    rgb / 12.92,
    ((rgb + 0.055) / 1.055)^2.4
  )
  0.2126 * channel[[1]] + 0.7152 * channel[[2]] + 0.0722 * channel[[3]]
}

tw_heatmap_fill <- function(score) {
  stops <- tw_plot_colors$heatmap_stops
  pos <- tw_plot_colors$heatmap_positions
  if (!is.finite(score)) {
    return(tw_plot_colors$missing)
  }
  x <- min(max(as.numeric(score), 0), 1)
  hi <- match(TRUE, pos >= x)
  if (is.na(hi) || hi <= 1L) {
    return(stops[[1]])
  }
  lo <- hi - 1L
  span <- pos[[hi]] - pos[[lo]]
  t <- if (span <= 0) 0 else (x - pos[[lo]]) / span
  lo_rgb <- grDevices::col2rgb(stops[[lo]]) / 255
  hi_rgb <- grDevices::col2rgb(stops[[hi]]) / 255
  mix <- lo_rgb + t * (hi_rgb - lo_rgb)
  grDevices::rgb(mix[1], mix[2], mix[3])
}

heatmap_label_colour <- function(score, is_missing = FALSE) {
  if (isTRUE(is_missing) || is.na(score)) {
    return(tw_plot_colors$text)
  }
  fill <- tw_heatmap_fill(score)
  if (tw_hex_luminance(fill) < 0.20) {
    "#FFFFFF"
  } else {
    tw_plot_colors$text
  }
}

tw_plot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = tw_plot_colors$text),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = tw_plot_colors$grid, linewidth = 0.3),
      axis.title = ggplot2::element_text(colour = tw_plot_colors$muted, size = base_size - 1),
      axis.text = ggplot2::element_text(colour = tw_plot_colors$text),
      legend.title = ggplot2::element_text(colour = tw_plot_colors$muted, size = base_size - 1),
      legend.text = ggplot2::element_text(colour = tw_plot_colors$text),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = tw_plot_colors$bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = tw_plot_colors$bg, colour = NA)
    )
}
