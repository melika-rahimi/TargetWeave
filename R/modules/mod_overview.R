mod_overview_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "overview-shell",
    uiOutput(ns("body"))
  )
}

mod_overview_server <- function(
  id,
  db_pool,
  user,
  target_id,
  identity_revision = reactive(0L),
  panel_active = reactive(TRUE),
  retrieve = retrieve_target_overview
) {
  moduleServer(id, function(input, output, session) {
    overview_result <- reactiveVal(NULL)
    results_by_target <- reactiveVal(list())
    inflight <- reactiveVal(list())
    lookup_error <- reactiveVal(NULL)
    last_fetch_signature <- reactiveVal(NA_character_)

    target_row <- reactive({
      identity_revision()
      req(user(), target_id())
      get_owned_target(db_pool, target_id(), user()$id)
    })

    fetch_overview <- function(row, force = FALSE, token = NULL) {
      tid <- as.character(row$id)
      bind_external_task(
        invoke_retrieve(retrieve, row, db_pool = db_pool),
        session,
        function(result) {
          if (!inflight_matches(inflight, tid, token)) {
            return()
          }
          inflight_clear(inflight, tid, token)
          if (is_task_error(result)) {
            lookup_error(result$message)
            return()
          }
          stored <- isolate(results_by_target())
          stored[[tid]] <- result
          results_by_target(stored)
          apply <- identity_task_apply(target_id(), tid)
          if (isTRUE(apply$update_visible_panel)) {
            overview_result(result)
            last_fetch_signature(overview_fetch_signature(row))
          }
        },
        error_message = "Annotations could not be retrieved.",
        inflight = inflight,
        key = tid,
        token = token
      )
    }

    start_overview <- function(force = FALSE) {
      row <- isolate(target_row())
      if (is.null(row)) {
        overview_result(NULL)
        last_fetch_signature(NA_character_)
        lookup_error("This target was not found or you do not have access to it.")
        return()
      }
      if (!target_is_confirmed(row)) {
        overview_result(not_confirmed_overview(row))
        last_fetch_signature(NA_character_)
        lookup_error(NULL)
        return()
      }
      tid <- as.character(row$id)
      if (inflight_has(inflight, tid)) {
        return()
      }
      if (!should_retrieve_overview(TRUE, row, isolate(last_fetch_signature()), force = force)) {
        return()
      }
      inflight_start(inflight, tid)
      lookup_error(NULL)
      token <- inflight_token(inflight, tid)
      schedule_after_flush(session, function() fetch_overview(row, force = force, token = token))
    }

    observeEvent(
      list(target_id(), identity_revision()),
      {
        current_id <- target_id()
        if (is.null(current_id)) {
          overview_result(NULL)
          last_fetch_signature(NA_character_)
          lookup_error(NULL)
          return()
        }

        previous <- last_fetch_signature()
        row <- target_row()
        next_sig <- overview_fetch_signature(row)
        stored <- results_by_target()[[as.character(current_id)]]
        if (!is.null(stored)) {
          overview_result(stored)
          last_fetch_signature(next_sig)
        } else if (live_signature_stale(previous, next_sig)) {
          overview_result(NULL)
          last_fetch_signature(NA_character_)
        }

        if (!isTRUE(isolate(panel_active()))) {
          return()
        }

        start_overview()
      },
      ignoreNULL = TRUE
    )

    observeEvent(panel_active(), {
      if (isTRUE(panel_active())) {
        start_overview()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$refresh_overview, {
      req(user(), target_id(), isTRUE(panel_active()))
      start_overview(force = TRUE)
    }, ignoreInit = TRUE)

    output$body <- renderUI({
      ns <- session$ns

      if (is.null(target_id())) {
        return(panel_state_ui(
          "empty",
          "No target selected",
          "Select a confirmed target to retrieve biological annotations."
        ))
      }

      row <- target_row()
      if (is.null(row)) {
        return(div(class = "form-message error", lookup_error() %||% "Target not found."))
      }

      if (!target_is_confirmed(row)) {
        return(panel_state_ui(
          "blocked",
          "Identity not confirmed",
          "Confirm this target before retrieving biological annotations."
        ))
      }

      if (inflight_has(inflight, as.character(row$id))) {
        return(panel_state_ui(
          "retrieving",
          "Retrieving identity\u2026",
          "UniProt and Ensembl lookup is in progress for this target."
        ))
      }

      if (is.null(overview_result()) ||
          is.null(overview_result()$overview) ||
          identical(overview_result()$status, "not_confirmed")) {
        if (identical(overview_result()$status, "not_confirmed")) {
        return(panel_state_ui(
          "blocked",
          "Identity not confirmed",
          "Confirm this target before retrieving annotations."
        ))
        }
        return(panel_state_ui(
          "empty",
          "Not retrieved",
          "Open Overview to retrieve annotations for this confirmed target."
        ))
      }

      current <- overview_result()
      if (!isTRUE(current$ok) && is.null(current$overview)) {
        return(panel_state_ui(
          "unavailable",
          lookup_error() %||% current$message %||% "Source temporarily unavailable",
          "Identity annotations for this target could not be shown. Other workspace views remain available."
        ))
      }

      overview_body_ui(current$overview, ns)
    })

    keep_tab_outputs_visible(output, "body")

    list(
      changed = reactive({
        target_id()
        overview_result()
        inflight()
      }),
      current = reactive(overview_result())
    )
  })
}

overview_display <- function(x, missing = "Not provided") {
  if (isTRUE(x) || identical(x, FALSE)) {
    return(as.character(x))
  }
  if (has_display_text(x)) {
    as.character(x)
  } else {
    missing
  }
}

overview_body_ui <- function(overview, ns) {
  identity <- overview$identity
  protein <- overview$protein
  genomic <- overview$genomic
  locations <- protein$subcellular_locations %||% list()
  disagreements <- overview$disagreements %||% list()

  tagList(
    if (length(disagreements) > 0) {
      div(
        class = "form-message error overview-disagreement",
        tags$strong("Source disagreement"),
        tags$ul(lapply(disagreements, tags$li))
      )
    },
    div(
      class = "overview-header",
      div(class = "eyebrow", "Target overview"),
      h2(overview_display(identity$symbol, "Unknown symbol")),
      p(class = "overview-protein-name", overview_display(identity$protein_name, "Name not provided"))
    ),
    div(
      class = "overview-facts",
      overview_fact("Ensembl gene", identifier_text(identity$ensembl_gene_id)),
      overview_fact("UniProt", identifier_text(identity$uniprot_accession)),
      overview_fact("HGNC", identifier_text(identity$hgnc_id)),
      overview_fact("Organism", overview_display(identity$organism)),
      overview_fact(
        "Length",
        if (is_present_scalar(protein$length_aa)) {
          paste(format_bp(protein$length_aa), "aa")
        } else {
          "Not provided by UniProt"
        }
      ),
      overview_fact(
        "Mass",
        if (is_present_scalar(protein$molecular_weight)) {
          paste(prettyNum(as.integer(protein$molecular_weight), big.mark = ","), "Da")
        } else {
          "Not provided by UniProt"
        }
      ),
      overview_fact("Review status", reviewed_status_label(protein$reviewed)),
      overview_fact("Biotype", overview_display(genomic$biotype, "Not provided by Ensembl")),
      overview_fact("Location", overview_display(genomic$location_label, "Not provided by Ensembl"))
    ),
    div(
      class = "overview-section",
      h3("Biological function"),
      overview_function_ui(protein, ns)
    ),
    div(
      class = "overview-section",
      h3("Genomic context"),
      p(
        class = "panel-intro",
        sprintf(
          "Chromosome %s · %s–%s · strand %s. Coordinates are from Ensembl and do not imply transcription level or disease mechanism.",
          overview_display(genomic$chromosome, "not provided"),
          overview_display(genomic$start),
          overview_display(genomic$end),
          if (is_present_scalar(genomic$strand) && as.integer(genomic$strand) < 0) {
            "negative (−)"
          } else if (is_present_scalar(genomic$strand) && as.integer(genomic$strand) > 0) {
            "positive (+)"
          } else {
            "not provided"
          }
        )
      ),
      if (is.null(build_genomic_plot_data(genomic, identity$symbol))) {
        p(class = "field-help", "Genomic coordinates were not provided by Ensembl.")
      } else {
        as_live_viz(export_lite_genomic_html(genomic, identity$symbol))
      }
    ),
    div(
      class = "overview-section",
      h3("Subcellular location"),
      overview_location_ui(locations, ns)
    ),
    div(
      class = "overview-section overview-provenance",
      h3("Source"),
      div(
        class = "provenance-grid",
        provenance_card_ui("UniProt", overview$provenance$uniprot),
        provenance_card_ui("Ensembl", overview$provenance$ensembl)
      ),
      tags$details(
        class = "about-scores",
        tags$summary("Technical provenance"),
        p("Identifiers and retrieval status above are used for scientific tracing. Endpoint URLs and query identifiers are listed here."),
        if (!is.null(overview$provenance$uniprot$url)) {
          p(tags$a(href = overview$provenance$uniprot$url, target = "_blank", rel = "noopener noreferrer", overview$provenance$uniprot$url))
        },
        if (!is.null(overview$provenance$ensembl$url)) {
          p(tags$a(href = overview$provenance$ensembl$url, target = "_blank", rel = "noopener noreferrer", overview$provenance$ensembl$url))
        }
      ),
      actionButton(
        ns("refresh_overview"),
        "Refresh annotations",
        class = "btn-text"
      )
    )
  )
}

overview_fact <- function(label, value) {
  div(
    class = "overview-fact",
    span(class = "overview-fact-label", label),
    strong(value)
  )
}

overview_function_ui <- function(protein, ns) {
  full <- protein$function_full
  readable <- protein$function_readable
  preview <- protein$function_preview
  pubmed_ids <- protein$function_pubmed_ids %||% character()
  display <- if (has_display_text(preview)) preview else readable
  if (!has_display_text(display)) {
    display <- full
  }

  if (!has_display_text(full)) {
    return(p("Not provided by UniProt"))
  }

  show_full <- has_display_text(full) &&
    has_display_text(display) &&
    !identical(full, display)

  tagList(
    p(class = "function-text", display),
    if (length(pubmed_ids) > 0) {
      disclosure(
        sprintf(
          "%s reference%s",
          length(pubmed_ids),
          if (length(pubmed_ids) == 1L) "" else "s"
        ),
        p(paste(sprintf("PubMed %s", pubmed_ids), collapse = " \u00b7 "))
      )
    },
    if (isTRUE(show_full)) {
      disclosure(
        "Show full UniProt annotation",
        p(class = "function-full", full)
      )
    }
  )
}

location_item_ui <- function(loc) {
  line <- loc$location
  if (has_display_text(loc$topology)) {
    line <- paste0(line, " · ", loc$topology)
  }
  if (has_display_text(loc$molecule)) {
    line <- paste0(loc$molecule, ": ", line)
  }
  tags$li(span(line))
}

location_list_ui <- function(locations) {
  tags$ul(
    class = "location-list",
    lapply(locations, location_item_ui)
  )
}

overview_location_ui <- function(locations, ns) {
  if (is.null(locations) || length(locations) == 0) {
    return(p("Not provided by UniProt"))
  }

  preview_n <- 8L
  preview <- if (length(locations) > preview_n) {
    locations[seq_len(preview_n)]
  } else {
    locations
  }

  has_evidence <- any(vapply(locations, function(loc) has_display_text(loc$evidence), logical(1)))

  tagList(
    location_list_ui(preview),
    if (length(locations) > preview_n) {
      disclosure(
        sprintf("Show all %s UniProt locations", length(locations)),
        location_list_ui(locations[seq(preview_n + 1L, length(locations))])
      )
    },
    if (isTRUE(has_evidence)) {
      with_evidence <- Filter(function(loc) has_display_text(loc$evidence), locations)
      disclosure(
        "Location evidence",
        if (length(with_evidence) == 0) {
          p(class = "field-help", "UniProt did not provide evidence tokens for these locations.")
        } else {
          tags$ul(
            class = "location-list",
            lapply(with_evidence, function(loc) {
              tags$li(
                span(loc$location),
                span(class = "location-evidence", loc$evidence)
              )
            })
          )
        }
      )
    }
  )
}

provenance_card_ui <- function(label, report) {
  if (is.null(report)) {
    return(div(class = "provenance-card", strong(label), p("Not queried")))
  }

  cache_label <- report$cache_status %||% "Not provided"
  retrieved <- overview_display(
    format_user_timestamp(report$retrieved_at),
    "Retrieval time not provided"
  )

  cache_kind <- if (grepl("stale", cache_label, ignore.case = TRUE)) {
    "warning"
  } else if (grepl("error|fail", cache_label, ignore.case = TRUE)) {
    "danger"
  } else {
    "success"
  }

  div(
    class = "provenance-card",
    strong(label),
    p(identifier_text(report$record_id)),
    p(status_pill(cache_label, cache_kind)),
    p(sprintf("Retrieved %s", retrieved)),
    if (has_display_text(report$error)) {
      p(class = "candidate-warning", report$error)
    }
  )
}
