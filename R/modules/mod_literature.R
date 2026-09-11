mod_literature_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "comparison-shell literature-shell",
    uiOutput(ns("body"))
  )
}

mod_literature_server <- function(
  id,
  db_pool,
  user,
  project_id,
  identity_revision = reactive(0L),
  disease_revision = reactive(0L),
  panel_active = reactive(TRUE),
  retrieve = retrieve_project_literature
) {
  moduleServer(id, function(input, output, session) {
    literature_result <- reactiveVal(NULL)
    inflight <- reactiveVal(list())
    progress_text <- reactiveVal(NULL)
    last_fetch_signature <- reactiveVal(NA_character_)
    selected_target_id <- reactiveVal(NULL)

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

    fetch_literature <- function(project, targets, token) {
      bind_external_task(
        invoke_retrieve(
          retrieve,
          project,
          targets,
          db_pool = db_pool,
          on_progress = function(i, n, symbol) {
      progress_text(sprintf("Retrieving PubMed records\u2026 %s of %s", i, n))
          }
        ),
        session,
        function(result) {
          if (!inflight_matches(inflight, "literature", token)) {
            return()
          }
          inflight_clear(inflight, "literature", token)
          progress_text(NULL)
          if (is_task_error(result)) {
            literature_result(list(status = "error", message = result$message, literature = NULL))
            return()
          }
          literature_result(result)
          last_fetch_signature(literature_signature(project, targets))
          counts <- result$literature$target_counts
          if (!is.null(counts) && nrow(counts) > 0) {
            current <- selected_target_id()
            if (!has_display_text(current) || !(current %in% counts$project_target_id)) {
              selected_target_id(as.character(counts$project_target_id[[1]]))
            }
          } else {
            selected_target_id(NULL)
          }
        },
        error_message = "Literature could not be retrieved.",
        inflight = inflight,
        key = "literature",
        token = token
      )
    }

    start_literature <- function(force = FALSE) {
      if (inflight_has(inflight, "literature")) {
        return()
      }
      project <- isolate(project_row())
      targets <- isolate(target_rows())
      if (is.null(project) || is.null(targets)) {
        literature_result(NULL)
        last_fetch_signature(NA_character_)
        selected_target_id(NULL)
        return()
      }

      gate <- literature_gate(project, targets)
      if (!isTRUE(gate$ok)) {
        literature_result(list(status = gate$status, message = gate$message, literature = NULL))
        last_fetch_signature(NA_character_)
        selected_target_id(NULL)
        return()
      }

      if (!should_retrieve_literature(TRUE, project, targets, isolate(last_fetch_signature()), force = force)) {
        return()
      }

      inflight_start(inflight, "literature")
      progress_text("Retrieving PubMed records\u2026")
      token <- inflight_token(inflight, "literature")
      schedule_after_flush(session, function() fetch_literature(project, targets, token))
    }

    observeEvent(
      list(project_id(), identity_revision(), disease_revision(), panel_active()),
      {
        if (is.null(project_id())) {
          literature_result(NULL)
          last_fetch_signature(NA_character_)
          selected_target_id(NULL)
          return()
        }
        previous <- last_fetch_signature()
        next_sig <- literature_signature(project_row(), target_rows())
        if (live_signature_stale(previous, next_sig)) {
          literature_result(NULL)
          last_fetch_signature(NA_character_)
          selected_target_id(NULL)
        }
        if (!isTRUE(panel_active())) {
          return()
        }
        start_literature()
      },
      ignoreNULL = TRUE
    )

    observeEvent(input$literature_target, {
      current <- literature_result()
      counts <- current$literature$target_counts
      if (is.null(counts) || nrow(counts) == 0) {
        return()
      }
      allowed <- as.character(counts$project_target_id)
      chosen <- as.character(input$literature_target %||% "")
      if (chosen %in% allowed) {
        selected_target_id(chosen)
      }
    }, ignoreInit = TRUE)

    selected_item <- reactive({
      current <- literature_result()
      items <- current$literature$targets
      if (is.null(items) || length(items) == 0) {
        return(NULL)
      }
      sid <- selected_target_id()
      for (item in items) {
        if (identical(item$target$project_target_id, sid)) {
          return(item)
        }
      }
      items[[1]]
    })

    output$trend_plot <- renderPlot({
      item <- selected_item()
      req(!is.null(item))
      plot_publication_trend(item$trend)
    }, height = 240)

    output$comparison_plot <- renderPlot({
      current <- literature_result()
      counts <- current$literature$target_counts
      req(!is.null(counts), nrow(counts) > 0)
      plot_pubmed_counts_by_target(counts)
    }, height = 200)

    output$body <- renderUI({
      ns <- session$ns
      if (inflight_has(inflight, "literature")) {
        return(div(
          class = "panel-status",
          panel_state_ui("retrieving", progress_text() %||% "Retrieving PubMed records\u2026")
        ))
      }
      current <- literature_result()
      if (is.null(current)) {
        return(panel_state_ui(
          "empty",
          "Not retrieved",
          "Open Literature to retrieve the PubMed landscape for confirmed targets."
        ))
      }
      if (!identical(current$status, "ready")) {
        return(panel_state_ui(
          if (grepl("unavailable|could not|fail", current$message %||% "", ignore.case = TRUE)) "unavailable" else "blocked",
          current$message %||% "Literature is not available."
        ))
      }
      literature_result_ui(
        ns,
        current,
        selected_item(),
        selected_target_id()
      )
    })

    list(current = reactive(literature_result()))
  })
}

literature_result_ui <- function(ns, current, selected, selected_id) {
  lit <- current$literature
  counts <- lit$target_counts
  choices <- if (!is.null(counts) && nrow(counts) > 0) {
    target_selector_choices(counts$project_target_id, counts$symbol)
  } else {
    NULL
  }

  tagList(
    div(
      class = "evidence-pair",
      h2("Literature"),
      p(
        class = "panel-intro",
        literature_corpus_definition()
      )
    ),
    p(
      class = "interpretation-note",
      "Publication counts describe literature volume within this search definition. They do not measure evidence quality, target importance, or therapeutic value."
    ),
    if (length(lit$excluded_targets) > 0) {
      div(
        class = "form-message",
        paste(vapply(lit$excluded_targets, function(item) item$message, character(1)), collapse = " ")
      )
    },
    if (length(lit$failures) > 0) {
      div(
        class = "form-message error",
        paste(vapply(lit$failures, function(item) item$message, character(1)), collapse = " ")
      )
    },
    if (!is.null(choices)) {
      radioButtons(
        ns("literature_target"),
        "Target",
        choices = choices,
        selected = selected_id %||% as.character(choices[[1]]),
        inline = TRUE
      )
    },
    if (is.null(selected)) {
      panel_state_ui(
        "blocked",
        "No confirmed target mapped to NCBI Gene",
        "Confirm identity so literature retrieval can use a verified NCBI Gene mapping."
      )
    } else {
      tagList(
        div(
          class = "overview-section",
          h3("Defined literature corpus"),
          tags$ul(
            class = "project-status-list",
            tags$li(sprintf("Target: %s", selected$target$symbol)),
            tags$li(sprintf("UniProt: %s", selected$target$uniprot_accession)),
            tags$li(sprintf("NCBI GeneID: %s", selected$target$ncbi_gene_id)),
            tags$li(sprintf("Disease: %s", selected$disease$canonical_name)),
            tags$li(sprintf("PubMed records: %s", format(selected$corpus$total_count, big.mark = ",")))
          ),
          if (selected$corpus$total_count > PUBMED_UID_RETRIEVAL_LIMIT) {
            p(
              class = "field-help",
              sprintf(
                "ESearch can retrieve at most %s UIDs. The total count above is the corpus size, not that retrieval limit.",
                format(PUBMED_UID_RETRIEVAL_LIMIT, big.mark = ",")
              )
            )
          }
        ),
        div(
          class = "overview-section",
          h3("Publication activity"),
          p(class = "field-help", "Annual PubMed record counts for the last 10 calendar years. The current year is a partial year."),
          plotOutput(ns("trend_plot"), height = "240px")
        ),
        literature_recent_ui(selected$recent_records),
        div(
          class = "overview-section",
          h3("PubMed records by target"),
          p(class = "field-help", "Literature volume is not target importance. Counts use the same corpus definition for every target."),
          if (nrow(counts) > 0) plotOutput(ns("comparison_plot"), height = "200px") else NULL
        ),
        literature_search_definition_ui(selected, lit),
        literature_provenance_ui(selected, lit)
      )
    }
  )
}

literature_recent_ui <- function(records) {
  div(
    class = "overview-section",
    h3("Recent PubMed records"),
    if (is.null(records) || nrow(records) == 0) {
      panel_state_ui(
        "empty",
        "No source result",
        "No PubMed records in this defined corpus. The search definition is unchanged."
      )
    } else {
      tags$table(
        class = "evidence-table literature-table",
        tags$thead(
          tags$tr(
            tags$th("Title"),
            tags$th("Metadata"),
            tags$th("PMID")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(records)), function(i) {
            row <- records[i, , drop = FALSE]
            tags$tr(
              tags$td(strong(row$title[[1]])),
              tags$td(
                class = "literature-meta",
                paste(
                  row$first_author[[1]],
                  row$journal[[1]],
                  row$publication_date[[1]],
                  sep = " · "
                )
              ),
              tags$td(
                tags$a(
                  href = row$pubmed_url[[1]],
                  target = "_blank",
                  rel = "noopener noreferrer",
                  sprintf("Open in PubMed (%s)", row$pmid[[1]])
                )
              )
            )
          })
        )
      )
    }
  )
}

literature_search_definition_ui <- function(selected, lit) {
  tags$details(
    class = "about-scores",
    tags$summary("Search definition"),
    tags$table(
      class = "evidence-table",
      tags$tbody(
        tags$tr(tags$th("Confirmed target"), tags$td(selected$target$symbol)),
        tags$tr(tags$th("UniProt accession"), tags$td(selected$target$uniprot_accession)),
        tags$tr(tags$th("NCBI GeneID"), tags$td(selected$target$ncbi_gene_id)),
        tags$tr(tags$th("Confirmed disease"), tags$td(selected$disease$canonical_name)),
        tags$tr(tags$th("Disease terms used"), tags$td(paste(selected$disease$search_terms, collapse = "; "))),
        tags$tr(tags$th("PubMed fields"), tags$td(selected$corpus$query_scope)),
        tags$tr(tags$th("NCBI Gene \u2192 PubMed"), tags$td("gene_pubmed")),
        tags$tr(tags$th("Exact disease query"), tags$td(selected$corpus$disease_query))
      )
    )
  )
}

literature_provenance_ui <- function(selected, lit) {
  prov <- selected$provenance
  cache_label <- switch(
    as.character(prov$cache_status %||% ""),
    live = "Fresh",
    cached = "Cached",
    fresh = "Cached",
    stale = "Stale",
    as.character(prov$cache_status %||% "Unknown")
  )
  tagList(
    div(
      class = "overview-section",
      h3("Source"),
      tags$table(
        class = "evidence-table",
        tags$tbody(
          tags$tr(tags$th("Source"), tags$td("NCBI PubMed")),
          tags$tr(tags$th("NCBI GeneID"), tags$td(prov$ncbi_gene_id)),
          tags$tr(tags$th("UniProt accession"), tags$td(prov$identifier_used)),
          tags$tr(tags$th("Confirmed disease"), tags$td(selected$disease$canonical_name)),
          tags$tr(tags$th("Search scope"), tags$td(selected$corpus$query_scope)),
          tags$tr(
            tags$th("PubMed database last update"),
            tags$td(prov$database_last_update %||% lit$provenance$database_last_update %||% "not retrieved")
          ),
          tags$tr(tags$th("Retrieved"), tags$td(as.character(prov$retrieved_at))),
          tags$tr(tags$th("Cache"), tags$td(cache_label))
        )
      ),
      tags$details(
        class = "about-scores",
        tags$summary("Technical provenance"),
        tags$table(
          class = "evidence-table",
          tags$tbody(
            tags$tr(tags$th("E-utilities"), tags$td(prov$eutilities)),
            tags$tr(tags$th("Link strategy"), tags$td(prov$link_strategy)),
            tags$tr(tags$th("Endpoint family"), tags$td(prov$endpoint_family)),
            tags$tr(tags$th("Rate-limit mode"), tags$td(prov$rate_limit_mode)),
            tags$tr(tags$th("Cache key family"), tags$td(lit$provenance$cache_key_family))
          )
        )
      )
    )
  )
}
