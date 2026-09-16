mod_disease_evidence_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "evidence-shell",
    uiOutput(ns("body"))
  )
}

mod_disease_evidence_server <- function(
  id,
  db_pool,
  user,
  target_id,
  project_id,
  identity_revision = reactive(0L),
  disease_revision = reactive(0L),
  panel_active = reactive(TRUE),
  retrieve = retrieve_target_disease_evidence
) {
  moduleServer(id, function(input, output, session) {
    evidence_result <- reactiveVal(NULL)
    results_by_target <- reactiveVal(list())
    inflight <- reactiveVal(list())
    last_fetch_signature <- reactiveVal(NA_character_)

    target_row <- reactive({
      identity_revision()
      req(user(), target_id())
      get_owned_target(db_pool, target_id(), user()$id)
    })

    project_row <- reactive({
      disease_revision()
      req(user(), project_id())
      get_owned_project(db_pool, project_id(), user()$id)
    })

    fetch_evidence <- function(row, project, token) {
      tid <- as.character(row$id)
      bind_external_task(
        invoke_retrieve(retrieve, row, project, db_pool = db_pool),
        session,
        function(result) {
          if (!inflight_matches(inflight, tid, token)) {
            return()
          }
          inflight_clear(inflight, tid, token)
          if (is_task_error(result)) {
            evidence_result(list(status = "error", message = result$message, evidence = NULL))
            return()
          }
          stored <- isolate(results_by_target())
          stored[[tid]] <- result
          results_by_target(stored)
          apply <- identity_task_apply(target_id(), tid)
          if (isTRUE(apply$update_visible_panel)) {
            evidence_result(result)
            last_fetch_signature(evidence_fetch_signature(row, project))
          }
        },
        error_message = "Open Targets evidence could not be retrieved.",
        inflight = inflight,
        key = tid,
        token = token
      )
    }

    start_evidence <- function(force = FALSE) {
      row <- isolate(target_row())
      project <- isolate(project_row())
      if (is.null(row) || is.null(project)) {
        evidence_result(NULL)
        last_fetch_signature(NA_character_)
        return()
      }
      blocked <- blocked_ot_evidence(row, project)
      if (!is.null(blocked)) {
        evidence_result(blocked)
        last_fetch_signature(NA_character_)
        return()
      }
      tid <- as.character(row$id)
      if (inflight_has(inflight, tid)) {
        return()
      }
      if (!should_retrieve_ot_evidence(
        TRUE,
        row,
        project,
        isolate(last_fetch_signature()),
        force = force
      )) {
        return()
      }
      inflight_start(inflight, tid)
      token <- inflight_token(inflight, tid)
      schedule_after_flush(session, function() fetch_evidence(row, project, token))
    }

    observeEvent(
      list(target_id(), identity_revision(), disease_revision()),
      {
        current_id <- target_id()
        if (is.null(current_id)) {
          evidence_result(NULL)
          last_fetch_signature(NA_character_)
          return()
        }

        previous <- last_fetch_signature()
        row <- target_row()
        project <- project_row()
        next_sig <- evidence_fetch_signature(row, project)
        stored <- results_by_target()[[as.character(current_id)]]
        if (!is.null(stored)) {
          evidence_result(stored)
          last_fetch_signature(next_sig)
        } else if (live_signature_stale(previous, next_sig)) {
          evidence_result(NULL)
          last_fetch_signature(NA_character_)
        }

        if (!isTRUE(isolate(panel_active()))) {
          return()
        }

        start_evidence()
      },
      ignoreNULL = TRUE
    )

    observeEvent(panel_active(), {
      if (isTRUE(panel_active())) {
        start_evidence()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$refresh_evidence, {
      req(user(), target_id(), isTRUE(panel_active()))
      start_evidence(force = TRUE)
    }, ignoreInit = TRUE)

    output$drilldown <- renderUI({
      current <- evidence_result()
      evidence <- current$evidence
      if (is.null(evidence)) {
        return(NULL)
      }
      selected <- input$datatype_pick
      scores <- evidence$association$datatype_scores
      sources <- evidence$association$datasource_scores
      if (is.null(scores) || nrow(scores) == 0) {
        return(p("No data-type association scores were returned for this pair."))
      }

      if (!has_display_text(selected) || !(selected %in% scores$datatype_id)) {
        selected <- scores$datatype_id[[1]]
      }
      row <- scores[scores$datatype_id == selected, , drop = FALSE][1, ]
      has_friendly <- !is.null(sources) && nrow(sources) > 0 &&
        any(sources$label_source == "targetweave_display_label")

      div(
        class = "overview-section",
        h3("Evidence by type"),
        h4(row$datatype_label[[1]]),
        p(sprintf(
          "Association score: %s",
          if (is.na(row$score[[1]])) "not returned" else sprintf("%.3f", row$score[[1]])
        )),
        h4("Source components"),
        if (is.null(sources) || nrow(sources) == 0) {
          p("No data-source association scores were returned for this pair.")
        } else {
          tags$table(
            class = "evidence-table",
            tags$thead(tags$tr(
              tags$th("Source"),
              tags$th("Association score")
            )),
            tags$tbody(lapply(seq_len(nrow(sources)), function(i) {
              src <- sources[i, ]
              tags$tr(
                tags$td(src$datasource_label[[1]]),
                tags$td(if (is.na(src$score[[1]])) "not returned" else sprintf("%.3f", src$score[[1]]))
              )
            }))
          )
        },
        tags$details(
          class = "about-scores",
          tags$summary("About these scores"),
          p("These association scores come from Open Targets. The selected evidence type and the source list are returned separately; a source is not shown as a child of a type unless Open Targets provided that link."),
          if (isTRUE(has_friendly)) {
            p("Friendly source names are maintained by TargetWeave.")
          },
          if (!is.null(sources) && nrow(sources) > 0) {
            p(sprintf(
              "Open Targets source identifiers: %s.",
              paste(sources$datasource_id, collapse = ", ")
            ))
          },
          p("A missing score is not the same as a score of 0, and neither is a probability of causality or therapeutic success.")
        )
      )
    })

    output$body <- renderUI({
      ns <- session$ns
      if (is.null(target_id())) {
        return(panel_state_ui(
          "empty",
          "No target selected",
          "Select a confirmed target after the project disease is confirmed."
        ))
      }

      row <- tryCatch(target_row(), error = function(e) NULL)
      project <- tryCatch(project_row(), error = function(e) NULL)
      if (is.null(row) || is.null(project)) {
        return(div(class = "form-message error", "This workspace item was not found or you do not have access to it."))
      }

      current <- evidence_result()
      busy <- inflight_has(inflight, as.character(row$id))

      tagList(
        evidence_pair_header_ui(row, project),
        if (busy) {
          div(
            class = "resolver-status",
            panel_state_ui("retrieving", "Retrieving Open Targets evidence\u2026")
          )
        } else if (is.null(current) && can_fetch_ot_evidence(row, project)) {
          div(
            class = "resolver-status",
            panel_state_ui("retrieving", "Retrieving Open Targets evidence\u2026")
          )
        } else if (!is.null(current)) {
          evidence_result_ui(current, ns)
        }
      )
    })

    keep_tab_outputs_visible(output, c("body", "drilldown"))

    list(current = reactive(evidence_result()))
  })
}

evidence_pair_header_ui <- function(target_row, project_row) {
  symbol <- target_workspace_label(target_row)
  disease <- if (project_disease_is_confirmed(project_row)) {
    project_row$disease_name[[1]]
  } else {
    project_row$disease_label[[1]]
  }

  div(
    class = "evidence-pair",
    div(class = "eyebrow", "Disease evidence"),
    h2(symbol),
    p(class = "evidence-times", "\u00d7"),
    h2(disease),
    p(
      class = "panel-intro",
      tagList(
        if (target_is_confirmed(target_row)) identifier_text(target_row$ensembl_gene_id[[1]]) else "Target not confirmed",
        "  \u00b7  ",
        if (project_disease_is_confirmed(project_row)) identifier_text(project_disease_ontology_id(project_row)) else "Disease not linked to an ontology term"
      )
    )
  )
}

evidence_result_ui <- function(current, ns) {
  if (identical(current$status, "blocked_target") || identical(current$status, "blocked_disease")) {
    return(panel_state_ui("blocked", current$message))
  }

  if (identical(current$status, "error") && is.null(current$evidence)) {
    return(panel_state_ui(
      "unavailable",
      current$message %||% "Source temporarily unavailable",
      "Open Targets evidence could not be shown. Other workspace views remain available."
    ))
  }

  evidence <- current$evidence
  if (is.null(evidence)) {
    return(panel_state_ui(
      "empty",
      "Not retrieved",
      "Open Disease evidence to retrieve Open Targets associations for this pair."
    ))
  }

  assoc <- evidence$association
  scores <- assoc$datatype_scores
  present <- if (is.null(scores)) 0L else sum(!is.na(scores$score))
  missing <- if (is.null(scores)) 0L else sum(is.na(scores$score))

  tagList(
    if (identical(current$status, "stale")) {
      div(class = "form-message warning", current$message %||% "Showing a stale cached Open Targets result.")
    },
    if (identical(current$status, "empty")) {
      div(class = "form-message", current$message)
    },
    div(
      class = "overview-section evidence-hero",
      `data-tour` = "evidence-chart",
      h3("Evidence profile"),
      if (present == 0) {
        p("No data-type association scores were returned. That is not the same as an API error.")
      } else {
        as_live_viz(export_lite_ot_evidence_html(
          scores,
          evidence$target$symbol,
          evidence$disease$name
        ))
      },
      if (missing > 0) {
        p(class = "field-help", "Some evidence types had no score returned. That is distinct from a returned score of 0.")
      },
      interpretation_guidance_ui(
        "These scores summarize aggregated Open Targets evidence. They are not probabilities of causality, therapeutic success, or clinical validity."
      )
    ),
    div(
      class = "overview-section",
      h3("Overall association"),
      h4("Direct association"),
      p(class = "association-score", format_score(assoc$overall_score_direct)),
      p("Evidence linked directly to the confirmed disease term."),
      h4("Broader ontology-aware association"),
      p(class = "association-score", format_score(assoc$overall_score_inclusive)),
      p("Also includes evidence propagated from related descendant disease terms."),
      p(class = "field-help", assoc$association_scope_note)
    ),
    if (!is.null(scores) && nrow(scores) > 0) {
      tagList(
        selectInput(
          ns("datatype_pick"),
          "Inspect an evidence type",
          choices = setNames(scores$datatype_id, scores$datatype_label),
          selected = scores$datatype_id[[1]]
        ),
        uiOutput(ns("drilldown"))
      )
    },
    therapeutic_evidence_ui(evidence$therapeutic_evidence),
    evidence_provenance_ui(evidence$provenance),
    actionButton(ns("refresh_evidence"), "Refresh evidence", class = "btn-text")
  )
}

format_score <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    "not returned"
  } else {
    sprintf("%.3f", as.numeric(x[[1]]))
  }
}

therapeutic_evidence_ui <- function(therapeutic) {
  rows <- therapeutic$rows %||% list()
  count <- therapeutic$count %||% 0L

  div(
    class = "overview-section",
    h3("Known therapeutic evidence"),
    p(
      class = "panel-intro",
      "Clinical and drug records in Open Targets for this confirmed target and disease. This is not a treatment recommendation and does not imply efficacy or suitability."
    ),
    if (count == 0 && length(rows) == 0) {
      p("No clinical or drug evidence was returned for this confirmed pair. That is not an API error.")
    } else {
      tagList(
        p(sprintf(
          "Open Targets reports %s clinical/drug evidence record(s) for this pair. Showing a small unique-drug sample.",
          count
        )),
        tags$table(
          class = "evidence-table",
          tags$thead(tags$tr(
            tags$th("Drug"),
            tags$th("ChEMBL identifier"),
            tags$th("Clinical stage"),
            tags$th("Open Targets evidence score")
          )),
          tags$tbody(lapply(rows, function(row) {
            tags$tr(
              tags$td(readable_drug_name(row$drug_name) %||% "Name not provided"),
              tags$td(row$drug_id %||% "ID not provided"),
              tags$td(format_clinical_stage(row$clinical_stage)),
              tags$td(format_score(row$score))
            )
          }))
        ),
        p(
          class = "field-help",
          "The Open Targets evidence score belongs to the evidence record in the Platform. It is not a measure of efficacy, clinical success, confidence, or treatment recommendation."
        )
      )
    }
  )
}

evidence_provenance_ui <- function(prov) {
  cache_label <- prov$cache_status %||% "not provided"
  div(
    class = "overview-section",
    h3("Source"),
    div(
      class = "provenance-grid",
      div(class = "provenance-card", strong("Source"), p(prov$source %||% "Open Targets")),
      div(class = "provenance-card", strong("Target"), p(identifier_text(prov$target_id))),
      div(class = "provenance-card", strong("Disease"), p(identifier_text(prov$disease_id))),
      div(class = "provenance-card", strong("Association shown"), p(
        if (identical(prov$association_scope, "direct")) {
          "Direct association"
        } else if (identical(prov$association_scope, "inclusive_indirect")) {
          "Broader ontology-aware association"
        } else {
          prov$association_scope %||% "not provided"
        }
      )),
      div(class = "provenance-card", strong("Retrieved"), p({
        stamped <- format_user_timestamp(prov$retrieved_at)
        if (has_display_text(stamped)) stamped else prov$retrieved_at %||% "not provided"
      })),
      div(class = "provenance-card", strong("Status"), p(status_pill(cache_label, retrieval_status_kind(cache_label)))),
      div(class = "provenance-card", strong("Data version"), p(prov$data_version %||% "not provided")),
      div(class = "provenance-card", strong("API version"), p(prov$api_version %||% "not provided"))
    ),
    tags$details(
      class = "about-scores",
      tags$summary("Technical provenance"),
      p(sprintf("Query: %s", prov$query_name %||% "not provided")),
      if (has_display_text(prov$graphql_scope_direct)) {
        p(sprintf(
          "Direct association argument: %s. Broader association argument: %s.",
          prov$graphql_scope_direct,
          prov$graphql_scope_inclusive %||% ""
        ))
      },
      p(prov$url %||% "")
    )
  )
}
