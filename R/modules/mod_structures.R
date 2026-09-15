mod_structures_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "comparison-shell structure-shell",
    uiOutput(ns("body"))
  )
}

mod_structures_server <- function(
  id,
  db_pool,
  user,
  project_id,
  identity_revision = reactive(0L),
  panel_active = reactive(TRUE),
  retrieve = retrieve_project_structures
) {
  moduleServer(id, function(input, output, session) {
    structure_result <- reactiveVal(NULL)
    inflight <- reactiveVal(list())
    progress_text <- reactiveVal(NULL)
    last_fetch_signature <- reactiveVal(NA_character_)
    selected_target_id <- reactiveVal(NULL)
    selected_entity_id <- reactiveVal(NULL)

    project_row <- reactive({
      req(user(), project_id())
      get_owned_project(db_pool, project_id(), user()$id)
    })

    target_rows <- reactive({
      identity_revision()
      req(user(), project_id())
      list_project_targets(db_pool, project_id(), user()$id)
    })

    fetch_structures <- function(project, targets, token) {
      bind_external_task(
        invoke_retrieve(
          retrieve,
          project,
          targets,
          db_pool = db_pool,
          on_progress = function(i, n, symbol) {
            progress_text(sprintf("Retrieving experimental structures\u2026 %s of %s", i, n))
          }
        ),
        session,
        function(result) {
          if (!inflight_matches(inflight, "structures", token)) {
            return()
          }
          inflight_clear(inflight, "structures", token)
          progress_text(NULL)
          if (is_task_error(result)) {
            structure_result(list(status = "error", message = result$message, structures = NULL))
            return()
          }
          structure_result(result)
          last_fetch_signature(structure_signature(targets))
          summary <- result$structures$summary
          if (!is.null(summary) && nrow(summary) > 0) {
            current <- selected_target_id()
            if (!has_display_text(current) || !(current %in% summary$project_target_id)) {
              selected_target_id(as.character(summary$project_target_id[[1]]))
            }
          } else {
            selected_target_id(NULL)
          }
          selected_entity_id(NULL)
        },
        error_message = "Structure retrieval could not be completed.",
        inflight = inflight,
        key = "structures",
        token = token
      )
    }

    start_structures <- function(force = FALSE) {
      if (inflight_has(inflight, "structures")) {
        return()
      }
      project <- isolate(project_row())
      targets <- isolate(target_rows())
      if (is.null(project) || is.null(targets)) {
        structure_result(NULL)
        last_fetch_signature(NA_character_)
        selected_target_id(NULL)
        selected_entity_id(NULL)
        return()
      }
      gate <- structure_gate(targets)
      if (!isTRUE(gate$ok)) {
        structure_result(list(status = gate$status, message = gate$message, structures = NULL))
        last_fetch_signature(NA_character_)
        return()
      }
      if (!should_retrieve_structures(TRUE, targets, isolate(last_fetch_signature()), force = force)) {
        return()
      }
      inflight_start(inflight, "structures")
      progress_text("Retrieving experimental structures\u2026")
      token <- inflight_token(inflight, "structures")
      schedule_after_flush(session, function() fetch_structures(project, targets, token))
    }

    observeEvent(
      list(project_id(), identity_revision()),
      {
        if (is.null(project_id())) {
          structure_result(NULL)
          last_fetch_signature(NA_character_)
          return()
        }
        previous <- last_fetch_signature()
        next_sig <- structure_signature(target_rows())
        if (live_signature_stale(previous, next_sig)) {
          structure_result(NULL)
          last_fetch_signature(NA_character_)
        }
        if (!isTRUE(isolate(panel_active()))) {
          return()
        }
        start_structures()
      },
      ignoreNULL = TRUE
    )

    observeEvent(panel_active(), {
      if (isTRUE(panel_active())) {
        start_structures()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$structure_target, {
      summary <- structure_result()$structures$summary
      if (is.null(summary) || nrow(summary) == 0) {
        return()
      }
      chosen <- as.character(input$structure_target %||% "")
      if (chosen %in% as.character(summary$project_target_id)) {
        selected_target_id(chosen)
        selected_entity_id(NULL)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$structure_entity, {
      selected_entity_id(as.character(input$structure_entity %||% ""))
    }, ignoreInit = TRUE)

    selected_item <- reactive({
      items <- structure_result()$structures$targets
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

    selected_record <- reactive({
      item <- selected_item()
      if (is.null(item) || length(item$records) == 0) {
        return(NULL)
      }
      eid <- selected_entity_id()
      for (rec in item$records) {
        if (identical(rec$polymer_entity$entity_identifier, eid)) {
          return(rec)
        }
      }
      NULL
    })

    output$body <- renderUI({
      ns <- session$ns
      if (inflight_has(inflight, "structures")) {
        return(panel_state_ui("retrieving", progress_text() %||% "Retrieving experimental structures\u2026"))
      }
      current <- structure_result()
      if (is.null(current)) {
        return(panel_state_ui(
          "empty",
          "Not retrieved",
          "Open Structures to retrieve experimental PDB coverage for confirmed targets."
        ))
      }
      if (!identical(current$status, "ready")) {
        return(panel_state_ui(
          if (grepl("unavailable|could not|fail", current$message %||% "", ignore.case = TRUE)) "unavailable" else "blocked",
          current$message %||% "Structures are not available."
        ))
      }
      structures_result_ui(ns, current, selected_item(), selected_record(), selected_target_id())
    })

    keep_tab_outputs_visible(output, "body")

    list(current = reactive(structure_result()))
  })
}

structures_result_ui <- function(ns, current, selected, record, selected_id) {
  bag <- current$structures
  summary <- bag$summary
  choices <- if (!is.null(summary) && nrow(summary) > 0) {
    target_selector_choices(summary$project_target_id, summary$symbol)
  } else {
    NULL
  }
  tagList(
    div(
      class = "evidence-pair",
      h2("Structures"),
      p(
        class = "panel-intro",
        "Experimentally determined PDB polymer entities matching the confirmed UniProt accession, with UniProt sequence coverage."
      )
    ),
    p(
      class = "interpretation-note",
      "Experimental PDB structures may represent only part of the canonical protein and may contain engineered mutations or complexes. Structural availability does not by itself indicate biological importance, therapeutic relevance, or target quality."
    ),
    if (length(bag$excluded_targets) > 0) {
      div(class = "form-message", paste(vapply(bag$excluded_targets, function(item) item$message, character(1)), collapse = " "))
    },
    if (length(bag$failures) > 0) {
      div(class = "form-message error", paste(vapply(bag$failures, function(item) item$message, character(1)), collapse = " "))
    },
    if (!is.null(choices)) {
      radioButtons(
        ns("structure_target"),
        "Target",
        choices = choices,
        selected = selected_id %||% as.character(choices[[1]]),
        inline = TRUE
      )
    },
    if (is.null(selected)) {
      panel_state_ui(
        "blocked",
        "No UniProt accession for PDB retrieval",
        "Confirm identity so experimental structure lookup can use a UniProt accession."
      )
    } else {
      tagList(
        div(
          class = "overview-section",
          h3("Experimental structure availability"),
          tags$ul(
            class = "project-status-list",
            tags$li(sprintf("Target: %s", selected$target$symbol)),
            tags$li(sprintf("UniProt: %s", selected$target$uniprot_accession)),
            tags$li(sprintf("Experimental PDB entries: %s", selected$n_pdb_entries)),
            tags$li(sprintf("Matching polymer entities: %s", selected$n_polymer_entities)),
            tags$li(sprintf(
              "Maximum sequence coverage among retrieved experimental structures: %s",
              if (is.na(selected$max_coverage_fraction)) {
                "Not provided"
              } else {
                sprintf("%.1f%%", 100 * selected$max_coverage_fraction)
              }
            ))
          ),
          p(class = "field-help", "Predicted structures are not included in this view. Entry count is unique PDB IDs, not polymer-entity rows.")
        ),
        if (length(selected$unavailable_entities) > 0) {
          p(
            class = "field-help",
            sprintf(
              "Metadata unavailable for %s matching polymer entit%s.",
              length(selected$unavailable_entities),
              if (identical(length(selected$unavailable_entities), 1L)) "y" else "ies"
            )
          )
        },
        if (isTRUE(selected$search_was_empty) || (identical(selected$n_pdb_entries, 0L) && length(selected$unavailable_entities) == 0)) {
          panel_state_ui(
            "empty",
            "No source result",
            "No experimental PDB structures were found for this confirmed UniProt accession. Predicted structures are not included."
          )
        } else if (length(selected$records) == 0) {
          panel_state_ui(
            "unavailable",
            "Metadata unavailable",
            "Experimental PDB identifiers were found, but structure metadata could not be retrieved. Open in RCSB PDB remains available when identifiers are listed."
          )
        } else {
          tagList(
            div(
              class = "overview-section",
              h3("Sequence coverage"),
              p(
                class = "field-help",
                sprintf(
                  "Sorted by sequence coverage, then release date. Showing up to %s polymer entities of %s. Navy marks UniProt residues represented in the mapped experimental polymer entity.",
                  min(STRUCTURE_COVERAGE_ROW_CAP, selected$n_polymer_entities),
                  selected$n_polymer_entities
                )
              ),
              as_live_viz(export_lite_coverage_html(
                selected$records,
                selected$uniprot_length,
                selected$target$symbol
              ))
            ),
            structure_table_ui(ns, selected),
            structure_detail_ui(ns, selected, record),
            div(
              class = "overview-section",
              h3("Experimental PDB entries by target"),
              p(class = "field-help", "Entry counts are availability, not target importance."),
              as_live_viz(export_lite_pdb_counts_html(summary))
            )
          )
        },
        structure_provenance_ui(selected, bag)
      )
    }
  )
}

structure_table_ui <- function(ns, selected) {
  records <- selected$records
  entity_ids <- vapply(records, function(item) item$polymer_entity$entity_identifier, character(1))
  names(entity_ids) <- vapply(records, function(item) {
    sprintf("%s entity %s", item$pdb_id, item$polymer_entity$entity_id)
  }, character(1))
  div(
    class = "overview-section structure-table-wrap",
    h3("Structure table"),
    selectInput(
      ns("structure_entity"),
      "Inspect polymer entity",
      choices = c("Select a structure" = "", entity_ids),
      selected = ""
    ),
    tags$table(
      class = "evidence-table structure-table",
      tags$thead(
        tags$tr(
          tags$th("PDB ID"),
          tags$th("Title"),
          tags$th("Entity"),
          tags$th("Chain(s)"),
          tags$th("Method"),
          tags$th("Resolution"),
          tags$th("Coverage"),
          tags$th("Released")
        )
      ),
      tags$tbody(
        lapply(records, function(item) {
          tags$tr(
            tags$td(item$pdb_id),
            tags$td(item$entry$title),
            tags$td(item$polymer_entity$entity_identifier),
            tags$td(if (length(item$polymer_entity$chains) == 0) "Not provided" else paste(item$polymer_entity$chains, collapse = ", ")),
            tags$td(item$experiment$method),
            tags$td(item$experiment$resolution_angstrom),
            tags$td(item$coverage$coverage_label),
            tags$td(item$entry$release_date)
          )
        })
      )
    )
  )
}

structure_detail_ui <- function(ns, selected, record) {
  div(
    class = "overview-section",
    h3("Selected structure"),
    if (is.null(record)) {
      p(class = "field-help", "Select a polymer entity to inspect metadata and open the 3D view. One experimental structure loads at a time.")
    } else {
      tagList(
        tags$ul(
          class = "project-status-list",
          tags$li(sprintf("PDB ID: %s", record$pdb_id)),
          tags$li(sprintf("Title: %s", record$entry$title)),
          tags$li(sprintf("Experimental method: %s", record$experiment$method)),
          tags$li(sprintf("Resolution: %s", record$experiment$resolution_angstrom)),
          tags$li(sprintf("Target polymer entity: %s", record$polymer_entity$entity_identifier)),
          tags$li(sprintf(
            "Chains: %s",
            if (length(record$polymer_entity$chains) == 0) "Not provided" else paste(record$polymer_entity$chains, collapse = ", ")
          )),
          tags$li(sprintf("Coverage: %s", record$coverage$coverage_label)),
          tags$li(sprintf(
            "Mapped UniProt ranges: %s",
            if (nrow(record$coverage$covered_ranges) == 0) {
              "Not provided"
            } else {
              paste(sprintf("%s\u2013%s", record$coverage$covered_ranges$begin, record$coverage$covered_ranges$end), collapse = "; ")
            }
          )),
          tags$li(sprintf("Release date: %s", record$entry$release_date)),
          if (isTRUE(record$engineered)) tags$li("Modified / engineered construct") else NULL
        ),
        p(
          tags$a(
            class = "btn-text",
            href = record$rcsb_url,
            target = "_blank",
            rel = "noopener noreferrer",
            "Open in RCSB PDB"
          )
        ),
        structure_ligand_ui(record$ligands),
        div(
          class = "structure-viewer-wrap",
          tags$iframe(
            class = "structure-viewer",
            src = record$viewer_url,
            title = sprintf("RCSB Mol* 3D view of %s", record$pdb_id),
            allow = "fullscreen",
            referrerpolicy = "strict-origin-when-cross-origin",
            loading = "lazy"
          )
        ),
        p(
          class = "field-help",
          "3D view is the official RCSB Mol* page for this PDB entry. If the embedded viewer is blank or blocked, the metadata above and Open in RCSB PDB remain the scientific record. Target chains are listed above; the viewer shows the full experimental entry."
        )
      )
    }
  )
}

structure_ligand_ui <- function(ligands) {
  if (is.null(ligands) || nrow(ligands) == 0) {
    return(p(class = "field-help", "No non-water chemical components were listed for this entry."))
  }
  tagList(
    h4("Non-polymer chemical components"),
    p(class = "field-help", "Source-derived chemical component IDs and names. These are not classified here as drugs, inhibitors, or binders."),
    tags$ul(
      lapply(seq_len(nrow(ligands)), function(i) {
        tags$li(sprintf("%s \u2014 %s", ligands$component_id[[i]], ligands$name[[i]]))
      })
    )
  )
}

structure_provenance_ui <- function(selected, bag) {
  prov <- selected$provenance
  cache_label <- switch(
    as.character(prov$cache_status %||% ""),
    live = "Fresh",
    cached = "Cached",
    fresh = "Cached",
    stale = "Stale",
    as.character(prov$cache_status %||% "Unknown")
  )
  div(
    class = "overview-section",
    h3("Source"),
    tags$table(
      class = "evidence-table",
      tags$tbody(
        tags$tr(tags$th("Source"), tags$td("RCSB Protein Data Bank")),
        tags$tr(tags$th("UniProt accession used"), tags$td(prov$identifier_used)),
        tags$tr(tags$th("Query scope"), tags$td("experimental")),
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
          tags$tr(tags$th("Search API"), tags$td(prov$search_api)),
          tags$tr(tags$th("Data API"), tags$td(prov$data_api)),
          tags$tr(tags$th("Sequence Coordinates API"), tags$td(prov$sequence_coordinates_api)),
          tags$tr(tags$th("Query scope"), tags$td("experimental")),
          tags$tr(tags$th("Display sort"), tags$td(prov$sort_rule)),
          tags$tr(tags$th("Cache key family"), tags$td(bag$provenance$cache_key_family)),
          tags$tr(tags$th("3D viewer"), tags$td("Official RCSB Mol* 3D view (iframe)"))
        )
      )
    )
  )
}
