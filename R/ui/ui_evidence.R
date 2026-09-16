evidence_page_header <- function(title, subtitle = NULL) {
  div(
    class = "evidence-page-header",
    h2(title),
    if (!is.null(subtitle)) p(class = "evidence-page-subtitle", subtitle)
  )
}

evidence_section_header <- function(title, help = NULL) {
  div(
    class = "evidence-section-header",
    h3(title),
    if (!is.null(help)) p(class = "field-help", help)
  )
}

evidence_metric <- function(label, value, hint = NULL, missing_label = "Not retrieved") {
  missing <- is.null(value) ||
    length(value) != 1L ||
    is.na(value) ||
    (is.character(value) && !nzchar(value))
  div(
    class = paste("evidence-metric", if (isTRUE(missing)) "is-missing"),
    span(class = "evidence-metric-label", label),
    span(
      class = "evidence-metric-value",
      if (isTRUE(missing)) missing_label else value
    ),
    if (!is.null(hint)) span(class = "evidence-metric-hint", hint)
  )
}

evidence_summary_strip <- function(..., aria_label = "Summary") {
  div(
    class = "evidence-summary-strip",
    role = "group",
    `aria-label` = aria_label,
    ...
  )
}

evidence_primary_surface <- function(...) {
  div(class = "evidence-primary-surface", ...)
}

evidence_details_disclosure <- function(summary_label, ..., class = NULL) {
  disclosure(
    summary_label,
    div(class = "evidence-details-body", ...),
    class = paste("evidence-details", class)
  )
}

evidence_target_picker <- function(...) {
  div(class = "evidence-target-picker evidence-target-switcher", ...)
}

evidence_target_switcher_ui <- function(input_id, label = "Target", choices, selected = NULL) {
  if (is.null(choices) || length(choices) == 0L) {
    return(NULL)
  }
  values <- unname(as.character(choices))
  labels <- names(choices)
  if (is.null(labels) || !all(nzchar(labels))) {
    labels <- values
  }
  selected <- as.character(selected %||% values[[1]])
  if (!(selected %in% values)) {
    selected <- values[[1]]
  }
  label_id <- paste0(input_id, "-label")
  div(
    id = input_id,
    class = "form-group shiny-input-radiogroup shiny-input-container evidence-target-picker evidence-target-switcher",
    role = "radiogroup",
    `aria-labelledby` = label_id,
    tags$span(id = label_id, class = "control-label", label),
    div(
      class = "target-switch",
      lapply(seq_along(values), function(i) {
        item_id <- paste0(input_id, "-", i)
        checked <- identical(values[[i]], selected)
        tags$label(
          class = "target-switch-item",
          `for` = item_id,
          tags$input(
            id = item_id,
            type = "radio",
            name = input_id,
            class = "target-switch-input",
            value = values[[i]],
            checked = if (isTRUE(checked)) NA else NULL
          ),
          tags$span(class = "target-switch-label", labels[[i]])
        )
      })
    )
  )
}

evidence_count_label <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return(NULL)
  }
  format(as.integer(value), big.mark = ",", scientific = FALSE, trim = TRUE)
}
