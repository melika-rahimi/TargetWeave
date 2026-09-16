mod_pathways_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "comparison-shell pathway-shell",
    uiOutput(ns("body"))
  )
}

mod_pathways_server <- function(
  id,
  db_pool,
  user,
  project_id,
  identity_revision = reactive(0L),
  panel_active = reactive(TRUE),
  retrieve = retrieve_project_pathways
) {
  moduleServer(id, function(input, output, session) {
    pathway_result <- reactiveVal(NULL)
    inflight <- reactiveVal(list())
    progress_text <- reactiveVal(NULL)
    last_fetch_signature <- reactiveVal(NA_character_)
    visual_ids <- reactiveVal(character())
    inspect_id <- reactiveVal(NULL)

    project_row <- reactive({
      req(user(), project_id())
      get_owned_project(db_pool, project_id(), user()$id)
    })

    target_rows <- reactive({
      identity_revision()
      req(user(), project_id())
      list_project_targets(db_pool, project_id(), user()$id)
    })

    fetch_pathways <- function(project, targets, token) {
      bind_external_task(
        invoke_retrieve(
          retrieve,
          project,
          targets,
          db_pool = db_pool,
          on_progress = function(i, n, symbol) {
            progress_text(sprintf("Retrieving Reactome pathways\u2026 %s of %s", i, n))
          }
        ),
        session,
        function(result) {
          if (!inflight_matches(inflight, "pathways", token)) {
            return()
          }
          inflight_clear(inflight, "pathways", token)
          progress_text(NULL)
          if (is_task_error(result)) {
            pathway_result(list(status = "error", message = result$message, pathways = NULL))
            return()
          }
          pathway_result(result)
          last_fetch_signature(pathway_signature(targets))
          if (!is.null(result$pathways) && nrow(result$pathways$targets) > 0) {
            visual_ids(as.character(result$pathways$targets$project_target_id))
          } else {
            visual_ids(character())
          }
        },
        error_message = "Reactome pathways could not be retrieved.",
        inflight = inflight,
        key = "pathways",
        token = token
      )
    }

    start_pathways <- function(force = FALSE) {
      if (inflight_has(inflight, "pathways")) {
        return()
      }
      project <- isolate(project_row())
      targets <- isolate(target_rows())
      if (is.null(project) || is.null(targets)) {
        pathway_result(NULL)
        last_fetch_signature(NA_character_)
        visual_ids(character())
        return()
      }

      gate <- pathway_gate(targets)
      if (!isTRUE(gate$ok)) {
        pathway_result(list(status = gate$status, message = gate$message, pathways = NULL))
        last_fetch_signature(NA_character_)
        visual_ids(character())
        return()
      }

      if (suppress_auto_retrieve_while_stale(isolate(pathway_result()), "pathways", force = force)) {
        return()
      }
      if (!should_retrieve_pathways(TRUE, targets, isolate(last_fetch_signature()), force = force)) {
        return()
      }

      inflight_start(inflight, "pathways")
      progress_text("Retrieving Reactome pathways\u2026")
      token <- inflight_token(inflight, "pathways")
      schedule_after_flush(session, function() fetch_pathways(project, targets, token))
    }

    observeEvent(
      list(project_id(), identity_revision()),
      {
        if (is.null(project_id())) {
          pathway_result(NULL)
          last_fetch_signature(NA_character_)
          visual_ids(character())
          return()
        }
        previous <- last_fetch_signature()
        next_sig <- pathway_signature(target_rows())
        held <- hold_stale_multi_target_result(
          pathway_result(),
          previous,
          next_sig,
          "pathways"
        )
        if (isTRUE(held$changed)) {
          pathway_result(held$current)
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
        start_pathways()
      },
      ignoreNULL = TRUE
    )

    observeEvent(panel_active(), {
      if (isTRUE(panel_active())) {
        start_pathways()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$refresh_pathways, {
      start_pathways(force = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$pathway_include, {
      current <- pathway_result()
      if (is.null(current$pathways)) {
        return()
      }
      retrieved <- as.character(current$pathways$targets$project_target_id)
      visual_ids(normalize_pathway_selection(
        input$pathway_include,
        retrieved,
        visual_ids()
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$inspect_pathway, {
      inspect_id(as.character(input$inspect_pathway))
    }, ignoreInit = TRUE)

    output$body <- renderUI({
      ns <- session$ns
      if (inflight_has(inflight, "pathways")) {
        return(panel_state_ui("retrieving", progress_text() %||% "Retrieving Reactome pathways\u2026"))
      }
      pathways_result_ui(
        pathway_result(),
        ns,
        visual_ids(),
        inspect_id(),
        filter_choice = input$pathway_filter,
        query = input$pathway_query
      )
    })

    keep_tab_outputs_visible(output, "body")

    list(
      ready = reactive(!is.null(pathway_result())),
      current = reactive(pathway_result())
    )
  })
}

pathways_result_ui <- function(
  current,
  ns,
  selected_ids = character(),
  inspect_id = NULL,
  filter_choice = NULL,
  query = NULL
) {
  if (is.null(current)) {
    return(panel_state_ui(
      "empty",
      "Not retrieved",
      "Open Pathways to retrieve Reactome membership for confirmed targets."
    ))
  }
  if (identical(current$status, "blocked_targets")) {
    return(panel_state_ui("blocked", current$message, "Confirm at least one target, then open Pathways."))
  }

  overlap <- current$pathways
  if (is.null(overlap)) {
    return(panel_state_ui(
      "unavailable",
      current$message %||% "Source temporarily unavailable",
      "Reactome membership could not be shown. Other workspace views remain available."
    ))
  }

  n_selected <- length(selected_ids)
  ids <- target_selector_choices(
    overlap$targets$project_target_id,
    overlap$targets$symbol
  )
  filter_choice <- filter_choice %||% if (n_selected < 2L) "all" else "shared"
  if (n_selected < 2L) {
    filter_choice <- "all"
  }
  visible <- filter_pathway_visual(
    overlap,
    selected_ids,
    shared_only = identical(filter_choice, "shared")
  )
  n_unique <- nrow(overlap$pathways)
  n_shared <- if (n_unique == 0L) {
    0L
  } else {
    sum(is_shared_pathway(overlap$pathways$matched_target_count))
  }
  shell_class <- paste(
    c("pathway-shell", if (isTRUE(current$stale_target_set)) "is-stale"),
    collapse = " "
  )

  tagList(
    target_set_stale_banner(ns, current, "refresh_pathways", "Refresh pathways"),
    div(
      class = shell_class,
      evidence_page_header(
        "Pathways",
        "Reactome lowest-level pathway membership for confirmed targets in this investigation."
      ),
      interpretation_guidance_ui(
        p("Reactome membership indicates that a target is annotated to a pathway in Reactome. It does not mean the pathway is active."),
        p("Pathway count is not a measure of target importance. Shared membership does not by itself imply direct interaction, disease causality, or therapeutic relevance."),
        p("A pathway that is not returned is not evidence that the biology is absent.")
      ),
      exclusion_status_ui(overlap$excluded_targets, overlap$failures),
      evidence_summary_strip(
        aria_label = "Pathway membership context",
        evidence_metric(
          "Confirmed targets included",
          nrow(overlap$targets),
          "Confirmed identities with a Reactome retrieval attempt in this result."
        ),
        evidence_metric(
          "Unique Reactome pathways",
          n_unique,
          "Distinct Reactome pathway identifiers in the retrieved membership set."
        ),
        evidence_metric(
          "Shared pathways across 2+ targets",
          n_shared,
          "Pathways with membership for at least two retrieved targets. Membership count only."
        )
      ),
      if (nrow(overlap$targets) > 0) {
        div(
          class = "pathway-include",
          checkboxGroupInput(
            ns("pathway_include"),
            "Include confirmed targets",
            choices = ids,
            selected = selected_ids,
            inline = TRUE
          )
        )
      },
      if (n_selected < 2L) {
        p(
          class = "field-help",
          "Overlap becomes available with at least two confirmed targets that have retrieved membership."
        )
      },
      if (n_selected >= 2L) {
        div(
          class = "pathway-matrix-filter",
          radioButtons(
            ns("pathway_filter"),
            "Matrix rows",
            choices = c(
              "Shared pathways" = "shared",
              "All memberships" = "all"
            ),
            selected = filter_choice,
            inline = TRUE
          )
        )
      },
      evidence_primary_surface(
        evidence_section_header(
          "Pathway membership overlap",
          paste(
            "Display sort is matched-target count, then pathway name. This is not a biological ranking.",
            "\u25CF = Reactome membership present; \u2014 = not present in the retrieved membership set."
          )
        ),
        pathway_overlap_matrix_ui(visible)
      ),
      pathway_detail_ui(overlap, inspect_id),
      evidence_details_disclosure(
        "View pathway list",
        pathway_table_ui(overlap, ns, query)
      ),
      pathway_provenance_ui(overlap)
    )
  )
}

pathway_member_lookup <- function(matrix, pathway_id, project_target_id) {
  if (is.null(matrix) || nrow(matrix) == 0) {
    return(NA)
  }
  hit <- matrix[
    matrix$pathway_id == pathway_id & matrix$project_target_id == project_target_id,
    ,
    drop = FALSE
  ]
  if (nrow(hit) == 0) {
    return(NA)
  }
  isTRUE(hit$is_member[[1]])
}

pathway_overlap_matrix_ui <- function(visible) {
  pathways <- visible$pathways_visible
  targets <- visible$targets_visible
  matrix <- visible$membership_matrix_visible
  n_hidden <- as.integer(visible$n_hidden %||% 0L)
  if (is.null(pathways) || nrow(pathways) == 0 || is.null(targets) || nrow(targets) == 0) {
    return(p(class = "field-help", "No Reactome pathway memberships were returned for this display selection."))
  }
  tagList(
    if (n_hidden > 0L) {
      p(
        class = "field-help",
        sprintf(
          "Showing %s of %s pathway rows. Remaining pathways stay in the list below.",
          nrow(pathways),
          nrow(pathways) + n_hidden
        )
      )
    },
    div(
      class = "pathway-matrix-scroll",
      tags$table(
        class = "pathway-overlap-matrix",
        tags$thead(tags$tr(
          tags$th(class = "pathway-overlap-name", "Pathway"),
          lapply(seq_len(nrow(targets)), function(j) {
            tags$th(class = "pathway-overlap-target", targets$symbol[[j]])
          }),
          tags$th(class = "pathway-overlap-shared", "Targets represented")
        )),
        tags$tbody(lapply(seq_len(nrow(pathways)), function(i) {
          row <- pathways[i, , drop = FALSE]
          shared_n <- as.integer(row$selected_count %||% row$matched_target_count[[1]])
          tags$tr(
            tags$th(
              class = "pathway-overlap-name",
              scope = "row",
              title = row$pathway_name[[1]],
              span(class = "pathway-overlap-title", row$pathway_name[[1]]),
              span(class = "pathway-overlap-id", row$pathway_id[[1]])
            ),
            lapply(seq_len(nrow(targets)), function(j) {
              present <- pathway_member_lookup(
                matrix,
                row$pathway_id[[1]],
                targets$project_target_id[[j]]
              )
              state <- if (isTRUE(present)) "present" else "absent"
              tags$td(
                class = paste("pathway-overlap-cell", paste0("is-", state)),
                `data-pathway-member` = state,
                span(
                  class = "pathway-overlap-mark",
                  title = if (isTRUE(present)) "Reactome membership present" else "Not present in the retrieved membership set",
                  if (isTRUE(present)) "\u25CF" else "\u2014"
                )
              )
            }),
            tags$td(
              class = "pathway-overlap-shared",
              if (identical(shared_n, 1L)) {
                "1 target"
              } else {
                sprintf("Shared by %s targets", shared_n)
              }
            )
          )
        }))
      )
    )
  )
}

pathway_detail_ui <- function(overlap, inspect_id) {
  if (!has_display_text(inspect_id) || nrow(overlap$pathways) == 0) {
    inspect_id <- if (nrow(overlap$pathways) > 0) overlap$pathways$pathway_id[[1]] else NULL
  }
  if (!has_display_text(inspect_id)) {
    return(NULL)
  }
  row <- overlap$pathways[overlap$pathways$pathway_id == inspect_id, , drop = FALSE]
  if (nrow(row) != 1) {
    return(NULL)
  }
  div(
    class = "evidence-secondary-block",
    evidence_section_header("Selected pathway"),
    p(class = "pathway-detail-name", row$pathway_name[[1]]),
    p(identifier_text(row$pathway_id[[1]])),
    p(sprintf("Targets mapped in this project: %s", row$matched_targets[[1]])),
    p(sprintf("Membership scope: %s", row$membership_scope[[1]])),
    p("Species: Homo sapiens"),
    p(
      sprintf(
        "Reactome disease pathway: %s",
        if (isTRUE(row$is_in_disease[[1]])) "yes (Reactome annotation; not matched to this project's disease)" else "no"
      )
    ),
    tags$a(
      href = row$pathway_url[[1]],
      target = "_blank",
      rel = "noopener noreferrer",
      "Open in Reactome"
    )
  )
}

pathway_table_ui <- function(overlap, ns, query = NULL) {
  if (nrow(overlap$pathways) == 0) {
    return(p("No Reactome pathway memberships were returned for the retrieved targets."))
  }
  rows <- overlap$pathways
  q <- tolower(trimws(as.character(query %||% "")))
  if (nzchar(q)) {
    rows <- rows[
      grepl(q, tolower(rows$pathway_name), fixed = TRUE) |
        grepl(q, tolower(rows$pathway_id), fixed = TRUE) |
        grepl(q, tolower(rows$matched_targets), fixed = TRUE),
      ,
      drop = FALSE
    ]
  }
  tagList(
    textInput(ns("pathway_query"), NULL, value = query %||% "", placeholder = "Search pathways"),
    if (nrow(rows) == 0) {
      p("No pathways match this search.")
    } else {
      div(
        class = "table-scroll",
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, , drop = FALSE]
          tags$button(
            type = "button",
            class = "btn-text pathway-row",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              ns("inspect_pathway"),
              row$pathway_id[[1]]
            ),
            span(class = "pathway-row-name", row$pathway_name[[1]]),
            span(class = "pathway-row-id", row$pathway_id[[1]]),
            span(
              class = "pathway-row-meta",
              sprintf(
                "%s · %s",
                row$matched_targets[[1]],
                if (identical(as.integer(row$matched_target_count[[1]]), 1L)) {
                  "1 target"
                } else {
                  sprintf("Shared by %s targets", row$matched_target_count[[1]])
                }
              )
            ),
            span(class = "pathway-row-scope", row$membership_scope[[1]]),
            if (isTRUE(row$is_in_disease[[1]])) span("Reactome disease pathway") else NULL
          )
        })
      )
    }
  )
}

pathway_provenance_ui <- function(overlap) {
  prov <- overlap$provenance
  cache_label <- switch(
    as.character(prov$cache_status %||% ""),
    live = "Fresh",
    fresh = "Cached",
    cached = "Cached",
    stale = "Stale",
    as.character(prov$cache_status %||% "Unknown")
  )
  evidence_details_disclosure(
    "Technical provenance",
    tags$table(
      class = "evidence-table",
      tags$tbody(
        tags$tr(tags$th("Source"), tags$td("Reactome")),
        tags$tr(tags$th("Reactome release"), tags$td(prov$reactome_release %||% "not retrieved")),
        tags$tr(tags$th("Identifier used"), tags$td("UniProt accession")),
        tags$tr(tags$th("Membership scope"), tags$td(prov$membership_scope)),
        tags$tr(tags$th("Retrieved"), tags$td(as.character(prov$retrieved_at))),
        tags$tr(tags$th("Cache"), tags$td(cache_label)),
        tags$tr(tags$th("Endpoint"), tags$td(prov$source_url)),
        tags$tr(tags$th("Strategy"), tags$td("Content Service UniProt-to-lowest-level pathway mapping")),
        tags$tr(tags$th("Cache key family"), tags$td(prov$cache_key_family)),
        tags$tr(tags$th("Version endpoint"), tags$td(prov$version_url))
      )
    )
  )
}
