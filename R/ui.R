build_app_ui <- function() {
  page_fillable(
    title = "TargetWeave",
    theme = bs_theme(
      version = 5,
      bg = "#F5F6FA",
      fg = "#171B28",
      primary = "#18243D",
      secondary = "#667085",
      success = "#1F6A4A",
      danger = "#9B3A36",
      base_font = font_collection(
        "system-ui",
        "-apple-system",
        "Segoe UI",
        "Roboto",
        "Arial",
        "sans-serif"
      ),
      heading_font = font_collection(
        "system-ui",
        "-apple-system",
        "Segoe UI",
        "Roboto",
        "Arial",
        "sans-serif"
      )
    ),
    tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "logo.svg"),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css?v=m12-5-pre-release-1"
      ),
      tags$script(src = "tour.js"),
      tags$script(src = "auth.js?v=enter-2"),
      tags$script(src = "modals.js"),
      tags$script(src = "literature.js?v=year-1"),
      tags$script(src = "landing.js?v=demo-start")
    ),
    shiny::useBusyIndicators(spinners = FALSE, pulse = FALSE, fade = FALSE),
    uiOutput("app_body")
  )
}
