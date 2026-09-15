tw_logo_svg <- function(size = 28, decorative = FALSE) {
  px <- paste0(as.integer(size), "px")
  htmltools::HTML(sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="%s" height="%s" class="tw-logo"%s focusable="false">
  <rect width="32" height="32" rx="8" fill="#18243D"/>
  <path d="M7 22c4 0 4-12 9-12s5 6 9 6" fill="none" stroke="#EEF1F6" stroke-width="2.15" stroke-linecap="round"/>
  <path d="M7 10c4 0 4 12 9 12s5-6 9-6" fill="none" stroke="#EEF1F6" stroke-width="2.15" stroke-linecap="round"/>
  <circle cx="16" cy="16" r="3.15" fill="#B83280"/>
  <circle cx="16" cy="16" r="1.15" fill="#F5F6FA"/>
</svg>',
    px,
    px,
    if (isTRUE(decorative)) ' aria-hidden="true"' else ' role="img" aria-label="TargetWeave"'
  ))
}

tw_brand_lockup <- function(subtitle = NULL, size = 28) {
  div(
    class = "brand-block",
    span(class = "brand-mark", tw_logo_svg(size = size, decorative = TRUE)),
    div(
      strong("TargetWeave"),
      if (!is.null(subtitle)) span(subtitle)
    )
  )
}

public_action <- function(input_id, label, class = "btn-primary-quiet") {
  tags$button(
    type = "button",
    class = class,
    onclick = sprintf(
      "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
      input_id
    ),
    label
  )
}

password_eye_svg <- function() {
  htmltools::HTML(
    '<svg class="tw-eye-show" viewBox="0 0 24 24" width="18" height="18" aria-hidden="true" focusable="false">
      <path fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" d="M2.6 12s3.4-6.2 9.4-6.2S21.4 12 21.4 12 18 18.2 12 18.2 2.6 12 2.6 12z"/>
      <circle cx="12" cy="12" r="2.4" fill="none" stroke="currentColor" stroke-width="1.8"/>
    </svg>
    <svg class="tw-eye-hide" viewBox="0 0 24 24" width="18" height="18" aria-hidden="true" focusable="false">
      <path fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" d="M3 3l18 18"/>
      <path fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" d="M9.9 9.9A3 3 0 0012 15a3 3 0 002.1-.9"/>
      <path fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" d="M6.1 6.6C4 8 2.6 12 2.6 12s3.4 6.2 9.4 6.2c1.7 0 3.2-.4 4.5-1"/>
      <path fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" d="M10.7 5.9A10 10 0 0112 5.8c6 0 9.4 6.2 9.4 6.2a16 16 0 01-2.2 3.1"/>
    </svg>'
  )
}

password_field_ui <- function(input_id, label, placeholder = NULL, autocomplete = "current-password") {
  tags$div(
    class = "form-group shiny-input-container tw-password-field",
    tags$label(`for` = input_id, class = "control-label", label),
    tags$div(
      class = "tw-password-wrap",
      tags$input(
        id = input_id,
        type = "password",
        class = "form-control",
        placeholder = placeholder %||% "",
        autocomplete = autocomplete,
        spellcheck = "false"
      ),
      tags$button(
        type = "button",
        class = "tw-password-toggle",
        `aria-label` = "Show password",
        title = "Show password",
        `data-password-toggle` = input_id,
        password_eye_svg()
      )
    )
  )
}

profile_display_value <- function(value) {
  blank_to_null(value) %||% "Not provided"
}

investigation_text_input <- function(
  input_id,
  label,
  placeholder,
  help = NULL,
  data_tour = NULL
) {
  tags$div(
    class = "form-group shiny-input-container",
    `data-tour` = data_tour,
    tags$label(`for` = input_id, class = "control-label", label),
    tags$input(
      id = input_id,
      type = "text",
      class = "form-control",
      value = "",
      placeholder = placeholder,
      autocomplete = "off",
      spellcheck = "false"
    ),
    if (!is.null(help)) tags$p(class = "field-help", help)
  )
}

investigation_textarea_input <- function(
  input_id,
  label,
  placeholder,
  rows = 5,
  help = NULL,
  data_tour = NULL
) {
  tags$div(
    class = "form-group shiny-input-container",
    `data-tour` = data_tour,
    tags$label(`for` = input_id, class = "control-label", label),
    tags$textarea(
      id = input_id,
      class = "form-control",
      rows = rows,
      placeholder = placeholder,
      autocomplete = "off",
      spellcheck = "false"
    ),
    if (!is.null(help)) tags$p(class = "field-help", help)
  )
}

onboarding_stepper <- function(current = 1L) {
  tagList(
    div(
      class = "onboarding-complete",
      span(class = "onboarding-check", "\u2713"),
      "Account created"
    ),
    tags$ol(
      class = "onboarding-progress",
      tags$li(
        class = if (identical(as.integer(current), 1L)) "is-current" else NULL,
        span(class = "step-n", "1"),
        "About your research"
      ),
      tags$li(
        class = if (identical(as.integer(current), 2L)) "is-current" else NULL,
        span(class = "step-n", "2"),
        "First investigation"
      )
    )
  )
}


identifier_text <- function(value) {
  if (!has_display_text(value)) {
    return(span(class = "text-muted", "not provided"))
  }
  tags$code(class = "identifier", as.character(value))
}

resolution_status_label <- function(status) {
  switch(
    as.character(status),
    confirmed = "Confirmed",
    ambiguous = "Ambiguous",
    failed = "Failed",
    "Needs confirmation"
  )
}

resolution_status_class <- function(status) {
  switch(
    as.character(status),
    confirmed = "status-pill status-pill-success",
    ambiguous = "status-pill status-pill-warning",
    failed = "status-pill status-pill-danger",
    "status-pill status-pill-neutral"
  )
}

status_pill <- function(label, kind = "neutral") {
  tags$span(
    class = paste("status-pill", paste0("status-pill-", kind)),
    as.character(label)
  )
}

panel_state_ui <- function(kind, title, detail = NULL) {
  kind <- as.character(kind %||% "empty")
  div(
    class = paste("panel-state", paste0("panel-state-", kind)),
    role = if (identical(kind, "retrieving")) "status" else NULL,
    p(class = "panel-state-title", title),
    if (!is.null(detail) && nzchar(as.character(detail))) {
      p(class = "panel-state-detail", detail)
    }
  )
}

retrieval_status_kind <- function(label) {
  text <- tolower(as.character(label %||% ""))
  if (grepl("unavailable|error|fail", text)) {
    return("danger")
  }
  if (grepl("stale", text)) {
    return("warning")
  }
  if (grepl("cached|fresh|live", text)) {
    return("info")
  }
  "neutral"
}

# Paint busy UI before a blocking HTTP call. Shiny does not flush mid-observer.
schedule_after_flush <- function(session, fun) {
  session$onFlushed(function() {
    fun()
  }, once = TRUE)
  invisible(TRUE)
}

tab_panel_shell <- function(..., loading_label) {
  div(
    class = "tab-panel-shell",
    div(
      class = "tab-loading-msg",
      role = "status",
      `data-tab-loading` = "true",
      loading_label
    ),
    ...
  )
}

as_live_viz <- function(html) {
  if (!has_display_text(html)) {
    return(NULL)
  }
  HTML(html)
}

# Hidden tab outputs re-execute by default when shown again. Keep the last
# rendered HTML so revisiting Project/Pathways/etc. does not rebuild plots.
keep_tab_outputs_visible <- function(output, ids) {
  for (id in ids) {
    try(outputOptions(output, id, suspendWhenHidden = FALSE), silent = TRUE)
  }
  invisible(NULL)
}

tab_button <- function(input_id, value, label, active = FALSE, data_tour = NULL) {
  tags$button(
    type = "button",
    class = paste("tab-btn", if (isTRUE(active)) "is-active" else NULL),
    `aria-current` = if (isTRUE(active)) "page" else NULL,
    `data-tour` = data_tour,
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      input_id,
      value
    ),
    label
  )
}

disclosure <- function(summary_label, ..., class = "tw-disclosure") {
  tags$details(
    class = class,
    tags$summary(summary_label),
    ...
  )
}

# Named vector for radio/checkbox groups: values are immutable target IDs,
# names are confirmed symbols. Do not use the symbol as the unique key.
target_selector_choices <- function(ids, labels) {
  ids <- as.character(ids %||% character())
  labels <- as.character(labels %||% character())
  if (length(ids) == 0L) {
    return(NULL)
  }
  if (length(labels) != length(ids)) {
    labels <- ids
  }
  stats::setNames(ids, labels)
}
