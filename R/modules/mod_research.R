mod_research_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "research-shell",
    uiOutput(ns("body"))
  )
}

mod_research_server <- function(
  id,
  db_pool,
  user,
  project_id,
  identity_revision = reactive(0L),
  disease_revision = reactive(0L),
  panel_active = reactive(TRUE),
  workspace_evidence = reactive(empty_workspace_evidence()),
  on_back_live = function() NULL
) {
  moduleServer(id, function(input, output, session) {
    list_revision <- reactiveVal(0L)
    viewing_id <- reactiveVal(NULL)
    preview <- reactiveVal(NULL)
    form_message <- reactiveVal(NULL)
    viewing <- reactiveVal(NULL)

    project_row <- reactive({
      identity_revision()
      disease_revision()
      req(user(), project_id())
      get_owned_project(db_pool, project_id(), user()$id)
    })

    target_rows <- reactive({
      identity_revision()
      req(user(), project_id())
      list_project_targets(db_pool, project_id(), user()$id)
    })

    snapshots <- reactive({
      list_revision()
      req(user(), project_id())
      list_project_snapshots(db_pool, project_id(), user()$id)
    })

    project_notes <- reactive({
      list_revision()
      req(user(), project_id())
      list_research_notes(db_pool, project_id(), user()$id, scope = "project")
    })

    snapshot_notes <- reactive({
      list_revision()
      sid <- viewing_id()
      if (!has_display_text(sid)) {
        return(NULL)
      }
      list_research_notes(db_pool, project_id(), user()$id, scope = "snapshot", snapshot_id = sid)
    })

    observeEvent(
      list(panel_active(), workspace_evidence(), identity_revision(), disease_revision()),
      {
        if (!isTRUE(panel_active())) {
          return()
        }
        project <- isolate(project_row())
        targets <- isolate(target_rows())
        if (is.null(project) || is.null(targets)) {
          preview(NULL)
          return()
        }
        preview(build_snapshot_payload(project, targets, workspace_evidence()))
      },
      ignoreNULL = TRUE
    )

    save_guard <- new_submit_guard(min_interval_sec = 1)

    observeEvent(viewing_id(), {
      sid <- viewing_id()
      if (!has_display_text(sid)) {
        viewing(NULL)
        return()
      }
      uid <- isolate(user())$id
      viewing(load_snapshot_for_view(db_pool, sid, uid))
    }, ignoreNULL = FALSE)

    output$compare_plot <- renderPlot({
      snapshot_safe_plot(function() {
        snap <- viewing()
        mat <- snap$comparison$model$datatype_matrix
        if (is.null(mat) || !is.data.frame(mat) || nrow(mat) == 0) {
          return(NULL)
        }
        plot_ot_comparison_heatmap(mat, snap$comparison$model$disease$name)
      })
    }, height = 280)

    output$pathway_plot <- renderPlot({
      snapshot_safe_plot(function() {
        snap <- viewing()
        mat <- snap$pathways$model$membership_matrix
        if (is.null(mat) || !is.data.frame(mat) || nrow(mat) == 0) {
          return(NULL)
        }
        capped <- snapshot_pathway_plot_matrix(mat)
        plot_pathway_membership_matrix(
          capped$matrix,
          capped$pathway_order
        )
      })
    }, height = 280)

    output$structure_plot <- renderPlot({
      snapshot_safe_plot(function() {
        snap <- viewing()
        summary <- snap$structures$model$summary
        if (is.null(summary) || !is.data.frame(summary) || nrow(summary) == 0) {
          return(NULL)
        }
        plot_structure_entry_counts(summary)
      })
    }, height = 180)

    output$coverage_plot <- renderPlot({
      snapshot_safe_plot(function() {
        snap <- viewing()
        item <- (snap$structures$model$targets %||% list())[[1]]
        if (is.null(item)) {
          return(NULL)
        }
        plot_structure_coverage(item$records, item$uniprot_length)
      })
    }, height = 240)

    output$body <- renderUI({
      ns <- session$ns
      if (!is.null(viewing())) {
        return(snapshot_view_ui(ns, viewing(), form_message(), snapshot_notes()))
      }
      snapshot_list_ui(ns, snapshots(), preview(), form_message(), project_notes())
    })

    observeEvent(input$save_snapshot, {
      if (!isTRUE(save_guard$try_start())) {
        return()
      }
      on.exit(save_guard$finish(), add = TRUE)
      form_message(NULL)
      result <- create_evidence_snapshot(
        db_pool,
        isolate(user())$id,
        isolate(project_id()),
        workspace = isolate(workspace_evidence()),
        name = isolate(input$snapshot_name)
      )
      if (!isTRUE(result$ok)) {
        form_message(result$error)
        return()
      }
      list_revision(isolate(list_revision()) + 1L)
      form_message("Evidence snapshot saved.")
    }, ignoreInit = TRUE)

    observeEvent(input$view_snapshot, {
      viewing_id(as.character(input$view_snapshot))
    })

    observeEvent(input$back_live, {
      viewing_id(NULL)
      on_back_live()
    })

    observeEvent(input$back_list, {
      viewing_id(NULL)
    })

    observeEvent(input$rename_snapshot, {
      req(viewing_id(), input$rename_name)
      rename_evidence_snapshot(db_pool, viewing_id(), user()$id, input$rename_name)
      list_revision(list_revision() + 1L)
      viewing(hydrate_snapshot_for_view(get_owned_snapshot(db_pool, viewing_id(), user()$id)))
    })

    observeEvent(input$delete_snapshot, {
      req(input$delete_snapshot)
      delete_evidence_snapshot(db_pool, input$delete_snapshot, user()$id)
      if (identical(as.character(viewing_id()), as.character(input$delete_snapshot))) {
        viewing_id(NULL)
      }
      list_revision(list_revision() + 1L)
    })

    observeEvent(input$save_project_note, {
      create_research_note(db_pool, user()$id, project_id(), input$project_note_body, scope = "project")
      list_revision(list_revision() + 1L)
      updateTextAreaInput(session, "project_note_body", value = "")
    })

    observeEvent(input$delete_project_note, {
      req(input$delete_project_note)
      delete_research_note(db_pool, input$delete_project_note, user()$id)
      list_revision(list_revision() + 1L)
    })

    observeEvent(input$save_snapshot_note, {
      req(viewing_id())
      create_research_note(
        db_pool,
        user()$id,
        project_id(),
        input$snapshot_note_body,
        scope = "snapshot",
        snapshot_id = viewing_id()
      )
      list_revision(list_revision() + 1L)
      updateTextAreaInput(session, "snapshot_note_body", value = "")
    })

    observeEvent(input$delete_snapshot_note, {
      req(input$delete_snapshot_note)
      delete_research_note(db_pool, input$delete_snapshot_note, user()$id)
      list_revision(list_revision() + 1L)
    })

    output$download_html <- downloadHandler(
      filename = function() {
        snap <- isolate(viewing())
        paste0(export_safe_stem(snap$name %||% "snapshot", snap$created_at), ".html")
      },
      contentType = "text/html; charset=utf-8",
      content = function(file) {
        result <- export_owned_snapshot(
          db_pool,
          user()$id,
          viewing_id(),
          format = "html",
          include_notes = !isFALSE(input$export_include_notes),
          include_researcher = isTRUE(input$export_include_researcher)
        )
        if (!isTRUE(result$ok)) {
          stop(result$error %||% "Export failed.")
        }
        file.copy(result$path, file, overwrite = TRUE)
        unlink(result$path)
      }
    )

    output$download_zip <- downloadHandler(
      filename = function() {
        snap <- isolate(viewing())
        paste0(export_safe_stem(snap$name %||% "snapshot", snap$created_at), ".zip")
      },
      contentType = "application/zip",
      content = function(file) {
        result <- export_owned_snapshot(
          db_pool,
          user()$id,
          viewing_id(),
          format = "zip",
          include_notes = !isFALSE(input$export_include_notes),
          include_researcher = isTRUE(input$export_include_researcher)
        )
        if (!isTRUE(result$ok)) {
          stop(result$error %||% "Export failed.")
        }
        file.copy(result$path, file, overwrite = TRUE)
        unlink(result$path)
      }
    )

    list(
      viewing = viewing,
      revision = reactive(list_revision())
    )
  })
}

snapshot_list_ui <- function(ns, rows, preview, message, notes = NULL) {
  summary <- preview$capture_summary %||% list(
    will_capture = list("Project context"),
    not_captured = list(),
    stale_at_capture = FALSE
  )
  name_default <- default_snapshot_name()
  tagList(
    div(
      class = "evidence-pair",
      h2("Notes / Snapshots"),
      p(
        class = "panel-intro",
        "Save a point-in-time copy of evidence already retrieved in this workspace. Snapshots do not call scientific APIs and do not update when source databases change."
      )
    ),
    if (!is.null(message)) div(class = "form-message", message),
    div(
      class = "overview-section",
      h3("Save evidence snapshot"),
      if (isTRUE(summary$stale_at_capture)) {
        p(
          class = "form-message warning",
          "Some evidence is currently shown from stale cached data. The snapshot will preserve that state."
        )
      },
      p(strong("Will capture")),
      tags$ul(lapply(summary$will_capture %||% "Project context", tags$li)),
      if (length(summary$not_captured) > 0) {
        tagList(
          p(strong("Not captured")),
          tags$ul(lapply(summary$not_captured, tags$li))
        )
      },
      textInput(ns("snapshot_name"), "Name", value = name_default),
      actionButton(ns("save_snapshot"), "Save evidence snapshot", class = "btn-primary-quiet")
    ),
    div(
      class = "overview-section",
      h3("Snapshots"),
      if (is.null(rows) || nrow(rows) == 0) {
        panel_state_ui(
          "empty",
          "No snapshots yet",
          "Save a snapshot after retrieving evidence you want to keep."
        )
      } else {
        tags$table(
          class = "evidence-table",
          tags$thead(tags$tr(
            tags$th("Name"),
            tags$th("Captured"),
            tags$th("Sources"),
            tags$th("Targets"),
            tags$th("")
          )),
          tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, ]
            summary_row <- decode_snapshot_json(row$capture_summary[[1]])
            tags$tr(
              tags$td(row$name[[1]]),
              tags$td(format_user_timestamp(row$created_at[[1]])),
              tags$td(as.character(summary_row$captured_source_count %||% "0")),
              tags$td(as.character(summary_row$captured_target_count %||% "0")),
              tags$td(
                actionButton(
                  ns(paste0("view_", row$id[[1]])),
                  "View",
                  class = "btn-text",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    ns("view_snapshot"),
                    row$id[[1]]
                  )
                ),
                actionButton(
                  ns(paste0("del_", row$id[[1]])),
                  "Delete",
                  class = "btn-text",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    ns("delete_snapshot"),
                    row$id[[1]]
                  )
                )
              )
            )
          }))
        )
      }
    ),
    notes_panel_ui(ns, notes, "Project notes", save_id = "save_project_note", body_id = "project_note_body", delete_id = "delete_project_note")
  )
}

snapshot_view_ui <- function(ns, snap, message = NULL, notes = NULL) {
  ctx <- snap$project_context$model %||% list()
  identities <- snap$target_identity$model$targets %||% list()
  tagList(
    div(
      class = "snapshot-banner",
      span(class = "snapshot-badge", "Preserved snapshot"),
      h2(snap$name),
      p(strong("This is not the live workspace.")),
      p(sprintf("Captured %s", format_user_timestamp(snap$created_at))),
      p("This snapshot is preserved and does not update when source databases change."),
      p(class = "field-help", "Live evidence may have changed since this snapshot."),
      actionButton(ns("back_live"), "Back to live workspace", class = "btn-primary-quiet"),
      actionButton(ns("back_list"), "All snapshots", class = "btn-text")
    ),
    if (!is.null(message)) div(class = "form-message", message),
    if (has_display_text(snap$view_message)) div(class = "form-message error", snap$view_message),
    textInput(ns("rename_name"), "Rename", value = snap$name),
    actionButton(ns("rename_snapshot"), "Rename", class = "btn-text"),
    div(
      class = "overview-section",
      h3("Project context"),
      tags$ul(
        tags$li(ctx$title),
        tags$li(ctx$research_question),
        tags$li(sprintf("Disease: %s", ctx$disease_name %||% ctx$disease_label)),
        tags$li(identifier_text(ctx$disease_ontology_id))
      )
    ),
    div(
      class = "overview-section",
      h3("Targets / identities"),
      tags$table(
        class = "evidence-table",
        tags$thead(tags$tr(tags$th("Symbol"), tags$th("UniProt"), tags$th("Ensembl"), tags$th("Status"))),
        tags$tbody(lapply(identities, function(row) {
          tags$tr(
            tags$td(row$display_symbol %||% row$input_text),
            tags$td(identifier_text(row$uniprot_accession)),
            tags$td(identifier_text(row$ensembl_gene_id)),
            tags$td(row$resolution_status)
          )
        }))
      )
    ),
    snapshot_section_block("Overview", snap$overview, snapshot_overview_ui),
    snapshot_section_block("Disease evidence", snap$disease_evidence, snapshot_disease_ui),
    snapshot_section_block("Compare evidence", snap$comparison, function(model) {
      if (is.null(model$datatype_matrix)) return(p("No comparison matrix was captured."))
      tagList(
        plotOutput(ns("compare_plot"), height = "280px"),
        p(class = "field-help", sprintf(
          "Open Targets data version %s · API %s",
          model$provenance$data_version %||% "Not provided",
          model$provenance$api_version %||% "Not provided"
        ))
      )
    }),
    snapshot_section_block("Pathways", snap$pathways, function(model) {
      if (is.null(model$membership_matrix)) return(p("No pathway membership was captured."))
      tagList(
        plotOutput(ns("pathway_plot"), height = "280px"),
        p(class = "field-help", sprintf("Reactome release %s", model$provenance$reactome_release %||% "Not provided"))
      )
    }),
    snapshot_section_block("Literature", snap$literature, snapshot_literature_ui),
    snapshot_section_block("Structures", snap$structures, function(model) {
      tagList(
        plotOutput(ns("structure_plot"), height = "180px"),
        plotOutput(ns("coverage_plot"), height = "240px"),
        p(class = "field-help", "PDB coordinates are not stored; open the original RCSB page from captured PDB IDs.")
      )
    }),
    snapshot_manifest_ui(snap$source_manifest),
    div(
      class = "overview-section",
      h3("Export"),
      p("Download a portable research record of this preserved snapshot. Export does not query scientific databases."),
      checkboxInput(ns("export_include_notes"), "Include research notes", value = TRUE),
      checkboxInput(ns("export_include_researcher"), "Include researcher information", value = FALSE),
      p(
        class = "field-help",
        "Researcher information is optional and off by default. Email, passwords, account metadata, and support requests are never exported."
      ),
      div(
        class = "export-actions",
        downloadButton(ns("download_html"), "Research dossier (HTML)", class = "btn-primary-quiet"),
        downloadButton(ns("download_zip"), "Data package (ZIP)", class = "btn-text")
      ),
      p(class = "field-help", "HTML is a readable report. ZIP is a data package of the same snapshot.")
    ),
    notes_panel_ui(ns, notes, "Notes about this snapshot", save_id = "save_snapshot_note", body_id = "snapshot_note_body", delete_id = "delete_snapshot_note")
  )
}

snapshot_empty_plot <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  ggplot2::ggplot() + ggplot2::theme_void()
}

snapshot_safe_plot <- function(fn) {
  tryCatch(
    {
      p <- fn()
      if (is.null(p)) snapshot_empty_plot() else p
    },
    error = function(e) snapshot_empty_plot()
  )
}

snapshot_pathway_plot_matrix <- function(mat, max_rows = PATHWAY_MATRIX_ROW_CAP) {
  order <- unique(as.character(mat$pathway_name))
  if (length(order) > max_rows) {
    order <- order[seq_len(max_rows)]
    mat <- mat[as.character(mat$pathway_name) %in% order, , drop = FALSE]
  }
  list(matrix = mat, pathway_order = order)
}

snapshot_section_block <- function(title, section, render_model) {
  if (!is.list(section)) {
    return(div(
      class = "overview-section",
      h3(title),
      p(NOT_RETRIEVED_BEFORE_CAPTURE)
    ))
  }
  has_body <- as.character(section$capture_status %||% "") %in% c("captured", "partial", "stale_at_capture") &&
    (!is.null(section$model) || !is.null(section$targets))
  div(
    class = "overview-section",
    h3(title),
    p(class = "field-help", section_status_display(section)),
    if (has_body) {
      if (!is.null(section$targets) && is.null(section$model)) {
        render_model(section)
      } else {
        render_model(section$model)
      }
    }
  )
}

snapshot_overview_ui <- function(section) {
  items <- section$targets %||% list()
  if (length(items) == 0) {
    return(p("No overview records were captured."))
  }
  tagList(lapply(names(items), function(tid) {
    env <- items[[tid]]
    model <- env$model
    div(
      class = "note-card",
      p(strong(model$identity$symbol %||% tid)),
      p(sprintf("Capture: %s", section_status_display(env))),
      if (!is.null(model$identity)) {
        tags$ul(
          tags$li(sprintf("UniProt: %s", model$identity$uniprot_accession %||% "Not provided")),
          tags$li(sprintf("Ensembl: %s", model$identity$ensembl_gene_id %||% "Not provided")),
          tags$li(sprintf("Length: %s aa", model$protein$length_aa %||% "Not provided")),
          tags$li(sprintf("Retrieved UniProt: %s (%s)", format_user_timestamp(model$provenance$uniprot$retrieved_at), freshness_label(model$provenance$uniprot$cache_status))),
          tags$li(sprintf("Retrieved Ensembl: %s (%s)", format_user_timestamp(model$provenance$ensembl$retrieved_at), freshness_label(model$provenance$ensembl$cache_status)))
        )
      }
    )
  }))
}

snapshot_disease_ui <- function(section) {
  items <- section$targets %||% list()
  tagList(lapply(names(items), function(tid) {
    env <- items[[tid]]
    model <- env$model
    div(
      class = "note-card",
      p(strong(model$target$symbol %||% tid)),
      p(sprintf("Capture: %s", section_status_display(env))),
      if (!is.null(model$association)) {
        p(sprintf(
          "Direct association: %s",
          if (is.null(model$association$overall_score_direct) || is.na(model$association$overall_score_direct)) {
            "Not provided"
          } else {
            sprintf("%.3f", model$association$overall_score_direct)
          }
        ))
      }
    )
  }))
}

snapshot_literature_ui <- function(model) {
  targets <- model$targets %||% list()
  if (length(targets) == 0) {
    return(p("No literature records were captured."))
  }
  tagList(lapply(targets, function(item) {
    div(
      class = "note-card",
      p(strong(item$target$symbol)),
      p(sprintf("Corpus count: %s", item$corpus$total_count %||% "Not provided")),
      p(sprintf("GeneID: %s", item$target$ncbi_gene_id %||% "Not provided")),
      p(sprintf("Retrieved: %s (%s)", format_user_timestamp(item$provenance$retrieved_at), freshness_label(item$provenance$cache_status)))
    )
  }))
}

snapshot_manifest_ui <- function(manifest) {
  div(
    class = "overview-section",
    h3("Source manifest"),
    if (length(manifest) == 0) {
      p("No source retrievals were captured.")
    } else {
      tags$table(
        class = "evidence-table",
        tags$thead(tags$tr(
          tags$th("Source"),
          tags$th("Identifier"),
          tags$th("Version"),
          tags$th("Retrieved"),
          tags$th("Freshness")
        )),
        tags$tbody(lapply(manifest, function(row) {
          tags$tr(
            tags$td(row$source %||% "Not provided"),
            tags$td(identifier_text(row$record_id)),
            tags$td(row$version %||% row$data_version %||% "Not provided"),
            tags$td(format_user_timestamp(row$retrieved_at)),
            tags$td(freshness_label(row$cache_status))
          )
        }))
      )
    }
  )
}
