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
          last_fetch_signature(comparison_signature(project, targets))
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

      if (suppress_auto_retrieve_while_stale(isolate(comparison_result()), "comparison", force = force)) {
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
      list(project_id(), disease_revision()),
      {
        current_project <- project_id()
        if (is.null(current_project)) {
          comparison_result(NULL)
          last_fetch_signature(NA_character_)
          visual_ids(character())
          return()
        }
        comparison_result(NULL)
        last_fetch_signature(NA_character_)
        visual_ids(character())
        if (isTRUE(isolate(panel_active()))) {
          start_comparison()
        }
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      list(project_id(), identity_revision()),
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
        next_sig <- comparison_signature(project, targets)
        held <- hold_stale_multi_target_result(
          comparison_result(),
          previous,
          next_sig,
          "comparison"
        )
        if (isTRUE(held$changed)) {
          comparison_result(held$current)
          if (is.null(held$current)) {
            last_fetch_signature(NA_character_)
            visual_ids(character())
          }
        }

        if (!isTRUE(isolate(panel_active()))) {
          return()
        }
        if (isTRUE(held$suppress_auto_retrieve)) {
          return()
        }
        start_comparison()
      },
      ignoreNULL = TRUE
    )

    observeEvent(panel_active(), {
      if (isTRUE(panel_active())) {
        start_comparison()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$refresh_comparison, {
      start_comparison(force = TRUE)
    }, ignoreInit = TRUE)

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

    output$body <- renderUI({
      ns <- session$ns
      if (inflight_has(inflight, "compare")) {
        return(panel_state_ui("retrieving", progress_text() %||% "Retrieving Open Targets evidence\u2026"))
      }

      current <- comparison_result()
      comparison_result_ui(current, ns, visual_ids())
    })

    keep_tab_outputs_visible(output, "body")

    list(
      inspect = reactive(inspect_id()),
      current = reactive(comparison_result())
    )
  })
}

comparison_evidence_dimension_count <- function(comparison) {
  matrix <- comparison$datatype_matrix
  n_types <- if (is.null(matrix) || nrow(matrix) == 0) {
    0L
  } else {
    length(unique(as.character(matrix$datatype_id)))
  }
  as.integer(1L + n_types)
}

comparison_datatype_ids <- function(matrix) {
  if (is.null(matrix) || nrow(matrix) == 0) {
    return(character())
  }
  comparison_datatype_order(unique(as.character(matrix$datatype_id)))
}

comparison_column_max <- function(values) {
  nums <- suppressWarnings(as.numeric(values))
  nums <- nums[is.finite(nums)]
  if (length(nums) == 0L) {
    return(0)
  }
  max(nums)
}

comparison_bar_width_pct <- function(value, column_max) {
  value <- suppressWarnings(as.numeric(value)[[1]])
  column_max <- suppressWarnings(as.numeric(column_max)[[1]])
  if (!is.finite(value) || !is.finite(column_max) || column_max <= 0) {
    return(0)
  }
  round(max(0, min(1, value / column_max)) * 100)
}

comparison_score_cell_state <- function(value, available = TRUE, retrieval_status = NA_character_) {
  if (!isTRUE(available) || identical(retrieval_status, "error")) {
    return("failed")
  }
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return("missing")
  }
  if (identical(as.numeric(value), 0)) {
    return("zero")
  }
  "ok"
}

comparison_score_cell <- function(
  value,
  column_max,
  available = TRUE,
  retrieval_status = NA_character_
) {
  state <- comparison_score_cell_state(value, available, retrieval_status)
  numeric_state <- state %in% c("ok", "zero")
  label <- switch(
    state,
    failed = "Evidence unavailable",
    missing = "not returned",
    format_score(value)
  )
  width <- if (isTRUE(numeric_state)) comparison_bar_width_pct(value, column_max) else NA_real_
  stale <- identical(retrieval_status, "stale")
  div(
    class = paste(
      "compare-metric-cell",
      paste0("is-", state),
      if (isTRUE(stale)) "is-stale" else NULL
    ),
    `data-compare-state` = state,
    span(class = "compare-metric-value", label),
    if (isTRUE(numeric_state)) {
      div(
        class = "compare-metric-bar",
        span(
          class = "compare-metric-bar-fill",
          style = sprintf("width:%s%%;", width)
        )
      )
    }
  )
}

comparison_lookup_datatype_score <- function(matrix, project_target_id, datatype_id) {
  if (is.null(matrix) || nrow(matrix) == 0) {
    return(NA_real_)
  }
  hit <- matrix[
    matrix$project_target_id == project_target_id & matrix$datatype_id == datatype_id,
    ,
    drop = FALSE
  ]
  if (nrow(hit) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(hit$score[[1]]))
}

comparison_matrix_ui <- function(targets, datatype_matrix) {
  datatype_ids <- comparison_datatype_ids(datatype_matrix)
  overall_max <- comparison_column_max(targets$overall_direct_score)
  type_max <- stats::setNames(
    vapply(
      datatype_ids,
      function(id) {
        comparison_column_max(datatype_matrix$score[as.character(datatype_matrix$datatype_id) == id])
      },
      numeric(1)
    ),
    datatype_ids
  )
  div(
    class = "compare-matrix-scroll",
    tags$table(
      class = "compare-matrix",
      tags$thead(tags$tr(
        tags$th(class = "compare-matrix-target", "Target"),
        tags$th(
          class = "compare-matrix-metric",
          span(class = "compare-matrix-heading", "Overall direct association score"),
          span(class = "compare-matrix-hint", "Open Targets overall association score for this confirmed pair.")
        ),
        lapply(datatype_ids, function(id) {
          tags$th(
            class = "compare-matrix-metric",
            span(class = "compare-matrix-heading", ot_datatype_label(id)),
            span(class = "compare-matrix-hint", "Open Targets data-type association score.")
          )
        })
      )),
      tags$tbody(lapply(seq_len(nrow(targets)), function(i) {
        row <- targets[i, , drop = FALSE]
        available <- isTRUE(row$available[[1]])
        status <- as.character(row$retrieval_status[[1]])
        tags$tr(
          class = paste(
            c(
              "compare-matrix-row",
              if (!isTRUE(available)) "comparison-unavailable",
              if (identical(status, "stale")) "is-stale"
            ),
            collapse = " "
          ),
          tags$th(
            class = "compare-matrix-target",
            scope = "row",
            span(class = "compare-target-symbol", row$symbol[[1]])
          ),
          tags$td(comparison_score_cell(
            row$overall_direct_score[[1]],
            overall_max,
            available = available,
            retrieval_status = status
          )),
          lapply(datatype_ids, function(id) {
            tags$td(comparison_score_cell(
              comparison_lookup_datatype_score(datatype_matrix, row$project_target_id[[1]], id),
              unname(type_max[[as.character(id)]]),
              available = available,
              retrieval_status = status
            ))
          })
        )
      }))
    )
  )
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
  visible_targets <- visible$targets_visible
  visible_matrix <- visible$datatype_matrix_visible
  choices <- target_selector_choices(targets$project_target_id, targets$symbol)
  selected <- intersect(as.character(selected_ids), as.character(targets$project_target_id))
  if (length(selected) == 0) {
    selected <- as.character(targets$project_target_id)
  }
  disease_name <- comparison$disease$name %||% comparison$disease$user_label
  n_targets <- nrow(targets)
  n_dimensions <- comparison_evidence_dimension_count(list(datatype_matrix = visible_matrix))
  shell_class <- paste(
    c("compare-shell", if (isTRUE(current$stale_target_set)) "is-stale"),
    collapse = " "
  )

  tagList(
    target_set_stale_banner(ns, current, "refresh_comparison", "Refresh comparison"),
    div(
      class = shell_class,
      evidence_page_header(
        "Compare evidence",
        tagList(
          "Confirmed Open Targets association scores for this project disease and target set.",
          if (has_display_text(disease_name)) {
            span(class = "evidence-context-mark", paste0(" \u00b7 ", disease_name))
          },
          " \u00b7 ",
          identifier_text(comparison$disease$id)
        )
      ),
      interpretation_guidance_ui(
        p("Scores shown here are Open Targets association scores. They summarize available evidence within the Platform's scoring framework and should not be interpreted as probabilities of causality, therapeutic success, or clinical validity."),
        p("Each column is a different evidence dimension. Values from different columns are not interchangeable and do not produce a ranking or a combined score."),
        p("A returned score of 0 is distinct from a score that was not returned.")
      ),
      exclusion_status_ui(comparison$excluded_targets),
      evidence_summary_strip(
        aria_label = "Comparison context",
        evidence_metric("Confirmed targets", n_targets, "Confirmed identities included in this comparison."),
        evidence_metric(
          "Disease context",
          if (has_display_text(disease_name)) as.character(disease_name) else NULL,
          "Confirmed project disease used for every target–disease pair."
        ),
        evidence_metric(
          "Evidence dimensions represented",
          n_dimensions,
          "Overall direct association score plus returned Open Targets data-type scores."
        )
      ),
      div(
        class = "compare-include",
        checkboxGroupInput(
          ns("compare_include"),
          "Include confirmed targets",
          choices = choices,
          selected = selected,
          inline = TRUE
        ),
        p(class = "field-help", "At least two confirmed project targets are required. This is display selection, not a ranking.")
      ),
      evidence_primary_surface(
        div(
          `data-tour` = "compare-heatmap",
          evidence_section_header(
            "Open Targets association comparison",
            "Rows are confirmed targets. Columns are Open Targets association scores. Bars are scaled only within each column."
          ),
          comparison_matrix_ui(visible_targets, visible_matrix)
        )
      ),
      evidence_details_disclosure(
        "View comparison details",
        comparison_summary_table(targets)
      ),
      comparison_inspect_actions(targets, ns),
      comparison_provenance_ui(comparison)
    )
  )
}

comparison_summary_table <- function(targets) {
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
}

comparison_inspect_actions <- function(targets, ns) {
  div(
    class = "evidence-secondary-block",
    evidence_section_header(
      "Inspect target",
      "Open Disease evidence for one confirmed target."
    ),
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
  evidence_details_disclosure(
    "Technical provenance",
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
    p(sprintf("Disease: %s", prov$disease_id %||% "not provided")),
    p("Association scope: direct (primary comparison)"),
    p(sprintf("Open Targets data version: %s", prov$data_version %||% "not provided")),
    p(sprintf("Open Targets API version: %s", prov$api_version %||% "not provided")),
    p(sprintf("Query: %s", prov$query_name %||% "TargetWeaveTargetDiseaseEvidence")),
    p(prov$strategy %||% "")
  )
}
