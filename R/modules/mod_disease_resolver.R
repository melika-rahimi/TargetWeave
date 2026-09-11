mod_disease_resolver_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("body"))
}

mod_disease_resolver_server <- function(id, db_pool, user, project) {
  moduleServer(id, function(input, output, session) {
    inflight <- reactiveVal(list())
    lookup_error <- reactiveVal(NULL)
    payload <- reactiveVal(NULL)
    changed <- reactiveVal(0L)
    local_refresh <- reactiveVal(0L)

    project_row <- reactive({
      local_refresh()
      project()
    })

    observeEvent(project_row(), {
      row <- project_row()
      if (is.null(row)) {
        payload(NULL)
        return()
      }
      payload(decode_disease_payload(row$disease_resolution_payload[[1]]))
    }, ignoreNULL = TRUE)

    run_lookup <- function(row, token, user_id) {
      bind_external_task(
        resolve_project_disease(
          row$disease_label[[1]],
          db_pool = db_pool,
          search_fetch = function(query, db_pool = NULL) {
            ot_search_diseases(query, db_pool = db_pool, perform = http_async_perform)
          }
        ),
        session,
        function(result) {
          if (!inflight_matches(inflight, "disease", token)) {
            return()
          }
          inflight_clear(inflight, "disease", token)
          if (is_task_error(result)) {
            lookup_error(result$message)
            return()
          }
          saved <- save_disease_lookup(
            db_pool,
            row$id[[1]],
            user_id,
            result$status,
            result
          )
          if (!isTRUE(saved)) {
            lookup_error("Lookup finished, but the disease result could not be saved.")
            return()
          }
          payload(result)
          local_refresh(isolate(local_refresh()) + 1L)
          changed(isolate(changed()) + 1L)
        },
        error_message = "Disease lookup could not be completed.",
        inflight = inflight,
        key = "disease",
        token = token
      )
    }

    observeEvent(input$resolve_disease, {
      req(user())
      if (inflight_has(inflight, "disease")) {
        return()
      }
      row <- project_row()
      if (is.null(row)) {
        lookup_error("This project was not found or you do not have access to it.")
        return()
      }
      inflight_start(inflight, "disease")
      lookup_error(NULL)
      token <- inflight_token(inflight, "disease")
      user_id <- isolate(user())$id
      schedule_after_flush(session, function() run_lookup(row, token, user_id))
    })

    observeEvent(input$confirm_disease, {
      req(user(), input$confirm_disease)
      current <- payload()
      row <- project_row()
      if (is.null(current) || is.null(row) || length(current$candidates) == 0) {
        lookup_error("No disease candidate is available to confirm.")
        return()
      }

      selected <- NULL
      for (candidate in current$candidates) {
        if (identical(candidate$candidate_key, input$confirm_disease)) {
          selected <- candidate
          break
        }
      }
      if (is.null(selected) || !has_display_text(selected$id) || !has_display_text(selected$name)) {
        lookup_error("This candidate is missing an Open Targets disease identifier.")
        return()
      }

      result <- confirm_project_disease(
        db_pool,
        row$id[[1]],
        user()$id,
        selected$id,
        selected$name
      )
      if (!isTRUE(result$ok)) {
        lookup_error(result$message)
        return()
      }

      local_refresh(local_refresh() + 1L)
      changed(changed() + 1L)
    })

    output$body <- renderUI({
      ns <- session$ns
      row <- project_row()
      if (is.null(row)) {
        return(NULL)
      }

      if (project_disease_is_confirmed(row)) {
        return(
          div(
            class = "disease-resolver",
            div(class = "eyebrow", "Disease identity confirmed"),
            h3(row$disease_name[[1]]),
            p(class = "panel-intro", paste("Original wording:", row$disease_label[[1]])),
            div(class = "candidate-meta", span(project_disease_ontology_id(row))),
            p(
              class = "field-help",
              sprintf(
                "Confirmed at %s.",
                {
                  stamped <- format_user_timestamp(row$disease_confirmed_at[[1]])
                  if (has_display_text(stamped)) stamped else "time not provided"
                }
              )
            )
          )
        )
      }

      current <- payload()
      busy <- inflight_has(inflight, "disease")
      tagList(
        div(
          class = "disease-resolver",
          div(class = "eyebrow", "Disease context"),
          h3(row$disease_label[[1]]),
          p(class = "panel-intro", "Link this disease to a standardized ontology term before exploring evidence."),
          if (!is.null(lookup_error())) div(class = "form-message error", lookup_error()),
          actionButton(
            ns("resolve_disease"),
            if (busy) "Resolving\u2026" else "Resolve disease",
            class = "btn-primary-quiet"
          ),
          if (busy) {
            panel_state_ui("retrieving", "Searching Open Targets for disease candidates\u2026")
          }
        ),
        if (!is.null(current)) disease_candidates_ui(current, ns)
      )
    })

    list(changed = reactive(changed()))
  })
}

disease_candidates_ui <- function(current, ns) {
  candidates <- current$candidates %||% list()
  reports <- list(ot = current$source_report)

  tagList(
    div(
      class = "source-report-row",
      source_badge_ui("Open Targets", reports$ot)
    ),
    if (identical(current$status, "failed")) {
      div(
        class = "form-message error",
        current$message %||% "No disease or phenotype candidates were returned."
      )
    },
    if (identical(current$status, "ambiguous")) {
      div(
        class = "form-message",
        "Several disease candidates have comparable textual identity, so a primary match is not claimed. Choose the intended disease. Nothing is confirmed automatically."
      )
    },
    if (isTRUE(current$unique_primary) && length(candidates) >= 1) {
      tagList(
        div(
          class = "form-message",
          "A primary match is shown first because of an exact canonical-name or documented-synonym match to the project disease wording. Confirm the intended identity; nothing is selected automatically."
        ),
        div(
          class = "candidate-group",
          div(class = "eyebrow candidate-group-label", "Primary / best match"),
          disease_candidate_card(candidates[[1]], ns, prominence = "primary")
        ),
        if (length(candidates) > 1) {
          div(
            class = "candidate-group",
            div(class = "eyebrow candidate-group-label", "Other valid candidates"),
            lapply(candidates[-1], function(candidate) {
              disease_candidate_card(candidate, ns, prominence = "secondary")
            })
          )
        }
      )
    } else if (length(candidates) > 0) {
      div(
        class = "candidate-list",
        lapply(candidates, function(candidate) disease_candidate_card(candidate, ns))
      )
    }
  )
}

disease_candidate_card <- function(candidate, ns, prominence = "normal") {
  div(
    class = paste(
      "candidate-card",
      if (identical(prominence, "primary")) "candidate-card-primary" else if (identical(prominence, "secondary")) "candidate-card-secondary" else NULL
    ),
    h4(candidate$name),
    div(
      class = "candidate-meta",
      span(candidate$id),
      span("disease / phenotype")
    ),
    if (has_display_text(candidate$description)) {
      p(class = "field-help", candidate$description)
    },
    if (has_display_text(candidate$match_reason)) {
      p(class = "match-reason", candidate$match_reason)
    },
    tags$button(
      type = "button",
      class = "btn-primary-quiet",
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
        ns("confirm_disease"),
        candidate$candidate_key
      ),
      "Confirm disease"
    )
  )
}
