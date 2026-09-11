mod_ot_comparison_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "comparison-shell",
    uiOutput(ns("body"))
  )
}

mod_ot_comparison_server <- function(
  id,
  db_pool,
  user,
  project_id,
  identity_revision = reactive(0L),
  disease_revision = reactive(0L),
  panel_active = reactive(TRUE),
  retrieve = retrieve_project_comparison
) {
  moduleServer(id, function(input, output, session) {
    comparison_result <- reactiveVal(NULL)
    inflight <- reactiveVal(list())
    progress_text <- reactiveVal(NULL)
    last_fetch_signature <- reactiveVal(NA_character_)
    inspect_id <- reactiveVal(NULL)
    visual_ids <- reactiveVal(character())

    project_row <- reactive({
      disease_revision()
      req(user(), project_id())
      get_owned_project(db_pool, project_id(), user()$id)
    })

    target_rows <- reactive({
      identity_revision()
      req(user(), project_id())
      list_project_targets(db_pool, project_id(), user()$id)
    })

    fetch_comparison <- function(project, targets, token, force = FALSE) {
      bind_external_task(
        invoke_retrieve(
          retrieve,
          project,
          targets,
          db_pool = db_pool,
          on_progress = function(i, n, symbol) {
            progress_text(sprintf("Retrieving Open Targets evidence\u2026 %s of %s", i, n))
          }
        ),
        session,
        function(result) {
          if (!inflight_matches(inflight, "compare", token)) {
            return()
          }
          inflight_clear(inflight, "compare", token)
          progress_text(NULL)
          if (is_task_error(result)) {
            comparison_result(list(status = "error", message = result$message, comparison = NULL))
            return()
          }
          comparison_result(result)
          confirmed <- split_comparison_targets(targets)$confirmed
          last_fetch_signature(comparison_signature(project, confirmed$id))
          if (!is.null(result$comparison)) {
            visual_ids(as.character(result$comparison$targets$project_target_id))
          }
        },
        error_message = "Open Targets comparison could not be retrieved.",
        inflight = inflight,
        key = "compare",
        token = token
      )
    }

    start_comparison <- function(force = FALSE) {
      if (inflight_has(inflight, "compare")) {
        return()
      }
      project <- isolate(project_row())
      targets <- isolate(target_rows())
      if (is.null(project) || is.null(targets)) {
        comparison_result(NULL)
        last_fetch_signature(NA_character_)
        visual_ids(character())
        return()
      }

      gate <- comparison_gate(project, targets)
      if (!isTRUE(gate$ok)) {
        comparison_result(list(status = gate$status, message = gate$message, comparison = NULL))
        last_fetch_signature(NA_character_)
        visual_ids(character())
        return()
      }

      if (!should_retrieve_comparison(
        TRUE,
        project,
        targets,
        isolate(last_fetch_signature()),
        force = force
      )) {
        return()
      }

      inflight_start(inflight, "compare")
      progress_text("Retrieving Open Targets evidence\u2026")
      token <- inflight_token(inflight, "compare")
      schedule_after_flush(session, function() fetch_comparison(project, targets, token, force = force))
    }

    observeEvent(
      list(project_id(), identity_revision(), disease_revision(), panel_active()),
      {
        current_project <- project_id()
        if (is.null(current_project)) {
          comparison_result(NULL)
          last_fetch_signature(NA_character_)
          visual_ids(character())
          return()
        }

        previous <- last_fetch_signature()
        project <- project_row()
        targets <- target_rows()
        confirmed <- split_comparison_targets(targets)$confirmed
        next_sig <- comparison_signature(project, confirmed$id)
        if (live_signature_stale(previous, next_sig)) {
          comparison_result(NULL)
          last_fetch_signature(NA_character_)
          visual_ids(character())
        }

        if (!isTRUE(panel_active())) {
          return()
        }

        start_comparison()
      },
      ignoreNULL = TRUE
    )

    observeEvent(input$compare_include, {
      current <- comparison_result()
      if (is.null(current$comparison)) {
        return()
      }
      confirmed_ids <- as.character(current$comparison$targets$project_target_id)
      visual_ids(normalize_comparison_selection(
        input$compare_include,
        confirmed_ids,
        visual_ids()
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$inspect_target, {
      inspect_id(as.character(input$inspect_target))
    }, ignoreInit = TRUE)

    output$heatmap <- renderPlot({
      current <- comparison_result()
      comparison <- current$comparison
      if (is.null(comparison)) {
        return(invisible(NULL))
      }
      visible <- filter_comparison_visual(comparison, visual_ids())
      plot_ot_comparison_heatmap(
        visible$datatype_matrix_visible,
        disease_name = comparison$disease$name
      )
    }, height = function() {
      current <- comparison_result()
      comparison <- current$comparison
      if (is.null(comparison) || nrow(comparison$datatype_matrix) == 0) {
        return(220)
      }
      n_types <- length(unique(comparison$datatype_matrix$datatype_id))
      as.integer(90 + 34 * n_types)
    }, bg = "#FFFFFF")

    output$overall_plot <- renderPlot({
      current <- comparison_result()
      comparison <- current$comparison
      if (is.null(comparison)) {
        return(invisible(NULL))
      }
      visible <- filter_comparison_visual(comparison, visual_ids())
      plot_ot_overall_scores(visible$targets_visible)
    }, height = function() {
      n <- max(length(visual_ids()), 2L)
      as.integer(90 + 36 * n)
    }, bg = "#FFFFFF")

    output$body <- renderUI({
      ns <- session$ns
      if (inflight_has(inflight, "compare")) {
        return(panel_state_ui("retrieving", progress_text() %||% "Retrieving Open Targets evidence\u2026"))
      }

      current <- comparison_result()
      comparison_result_ui(current, ns, visual_ids())
    })

    list(
      inspect = reactive(inspect_id()),
      current = reactive(comparison_result())
    )
  })
}

comparison_result_ui <- function(current, ns, selected_ids = character()) {
  if (is.null(current)) {
    return(panel_state_ui(
      "empty",
      "Not retrieved",
      "Open Compare to retrieve Open Targets evidence for confirmed targets."
    ))
  }

  if (identical(current$status, "blocked_disease") || identical(current$status, "blocked_targets")) {
    return(panel_state_ui("blocked", current$message))
  }

  comparison <- current$comparison
  if (is.null(comparison)) {
    return(panel_state_ui("unavailable", current$message %||% "Comparison is not available."))
  }

  visible <- filter_comparison_visual(comparison, selected_ids)
  targets <- comparison$targets
  choices <- target_selector_choices(targets$project_target_id, targets$symbol)
  selected <- intersect(as.character(selected_ids), as.character(targets$project_target_id))
  if (length(selected) == 0) {
    selected <- as.character(targets$project_target_id)
  }

  heatmap_data <- visible$datatype_matrix_visible
  has_heatmap <- !is.null(heatmap_data) && nrow(heatmap_data) > 0
  has_overall <- any(!is.na(visible$targets_visible$overall_direct_score))

  tagList(
    div(
      class = "evidence-pair",
      h2("Compare evidence"),
      p(
        class = "panel-intro",
        tagList(
          comparison$disease$name %||% comparison$disease$user_label,
          " \u00b7 ",
          identifier_text(comparison$disease$id)
        )
      )
    ),
    p(
      class = "interpretation-note",
      "Scores shown here are Open Targets association scores. They summarize available evidence within the Platform's scoring framework and should not be interpreted as probabilities of causality, therapeutic success, or clinical validity."
    ),
    if (length(comparison$excluded_targets) > 0) {
      div(
        class = "form-message",
        paste(
          "Excluded from comparison (not confirmed):",
          paste(vapply(comparison$excluded_targets, function(row) row$symbol, character(1)), collapse = ", ")
        )
      )
    },
    checkboxGroupInput(
      ns("compare_include"),
      "Include confirmed targets",
      choices = choices,
      selected = selected,
      inline = TRUE
    ),
    p(class = "field-help", "At least two confirmed project targets are required. This is display selection, not a ranking."),
    if (has_overall) {
      div(
        class = "overview-section",
        h3("Open Targets overall association score"),
        p(class = "field-help", "Direct association, sorted by the Platform score. This is sorting, not a ranking."),
        plotOutput(ns("overall_plot"), height = "auto")
      )
    },
    if (has_heatmap) {
      div(
        class = "overview-section",
        `data-tour` = "compare-heatmap",
        h3("Evidence profile by data type"),
        p(class = "field-help", "Direct association scores. Grey — was not returned; 0.00 is a returned zero."),
        plotOutput(ns("heatmap"), height = "auto")
      )
    } else {
      div(
        class = "overview-section",
        h3("Evidence profile by data type"),
        p("No direct data-type association scores were returned for the selected targets.")
      )
    },
    comparison_summary_table(targets),
    comparison_inspect_actions(targets, ns),
    comparison_provenance_ui(comparison)
  )
}

comparison_summary_table <- function(targets) {
  div(
    class = "overview-section",
    h3("Comparison details"),
    div(
      class = "table-scroll",
      tags$table(
        class = "evidence-table",
        tags$thead(tags$tr(
          tags$th("Target"),
          tags$th("Ensembl ID"),
          tags$th("Overall direct score"),
          tags$th("Broader ontology-aware score"),
          tags$th("Retrieval status")
        )),
        tags$tbody(lapply(seq_len(nrow(targets)), function(i) {
          row <- targets[i, , drop = FALSE]
          status <- comparison_status_label(row$retrieval_status[[1]], row$cache_status[[1]])
          tags$tr(
            class = if (!isTRUE(row$available[[1]])) "comparison-unavailable" else NULL,
            tags$td(row$symbol[[1]]),
            tags$td(identifier_text(row$ensembl_gene_id[[1]])),
            tags$td(format_score(row$overall_direct_score[[1]])),
            tags$td(format_score(row$overall_inclusive_score[[1]])),
            tags$td(status_pill(status, retrieval_status_kind(status)))
          )
        }))
      )
    )
  )
}

comparison_inspect_actions <- function(targets, ns) {
  div(
    class = "overview-section",
    h3("Inspect target"),
    p(class = "field-help", "Open Disease evidence for one confirmed target."),
    div(
      class = "comparison-inspect-row",
      lapply(seq_len(nrow(targets)), function(i) {
        row <- targets[i, , drop = FALSE]
        tags$button(
          type = "button",
          class = "btn-text",
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            ns("inspect_target"),
            row$project_target_id[[1]]
          ),
          paste("Inspect", row$symbol[[1]])
        )
      })
    )
  )
}

comparison_provenance_ui <- function(comparison) {
  prov <- comparison$provenance
  targets <- comparison$targets
  div(
    class = "overview-section",
    h3("Source"),
    div(
      class = "table-scroll",
      tags$table(
        class = "evidence-table",
        tags$thead(tags$tr(
          tags$th("Target"),
          tags$th("Ensembl ID"),
          tags$th("Status"),
          tags$th("Retrieved")
        )),
        tags$tbody(lapply(seq_len(nrow(targets)), function(i) {
          row <- targets[i, , drop = FALSE]
          status <- comparison_status_label(row$retrieval_status[[1]], row$cache_status[[1]])
          tags$tr(
            tags$td(row$symbol[[1]]),
            tags$td(identifier_text(row$ensembl_gene_id[[1]])),
            tags$td(status_pill(status, retrieval_status_kind(status))),
            tags$td({
              stamped <- format_user_timestamp(row$retrieved_at[[1]])
              if (has_display_text(stamped)) stamped else row$retrieved_at[[1]] %||% "not provided"
            })
          )
        }))
      )
    ),
    tags$details(
      class = "about-scores",
      tags$summary("Technical provenance"),
      p(sprintf("Disease: %s", prov$disease_id %||% "not provided")),
      p("Association scope: direct (primary comparison)"),
      p(sprintf("Open Targets data version: %s", prov$data_version %||% "not provided")),
      p(sprintf("Open Targets API version: %s", prov$api_version %||% "not provided")),
      p(sprintf("Query: %s", prov$query_name %||% "TargetWeaveTargetDiseaseEvidence")),
      p(prov$strategy %||% "")
    )
  )
}
