mod_project_home_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "content-stack",
    uiOutput(ns("workspace_nav")),
    uiOutput(ns("project_header")),
    div(
      class = "workspace-grid",
      div(
      class = "targets-panel",
      `data-tour` = "target-list",
      h3("Targets"),
        uiOutput(ns("targets"))
      ),
      div(
        class = "workspace-main",
        `data-tour` = "target-workspace",
        uiOutput(ns("project_tabs")),
        uiOutput(ns("target_subnav")),
        div(
          class = "visually-hidden",
          selectInput(
            ns("workspace_panel"),
            label = NULL,
            choices = c("project", "resolver", "overview", "evidence", "compare", "pathways", "literature", "structures", "research"),
            selected = "project"
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'project'",
          ns = ns,
          uiOutput(ns("project_stage"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'resolver'",
          ns = ns,
          mod_target_resolver_ui(ns("resolver"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'overview'",
          ns = ns,
          mod_overview_ui(ns("overview"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'evidence'",
          ns = ns,
          mod_disease_evidence_ui(ns("evidence"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'compare'",
          ns = ns,
          mod_ot_comparison_ui(ns("compare"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'pathways'",
          ns = ns,
          mod_pathways_ui(ns("pathways"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'literature'",
          ns = ns,
          mod_literature_ui(ns("literature"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'structures'",
          ns = ns,
          mod_structures_ui(ns("structures"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'research'",
          ns = ns,
          mod_research_ui(ns("research"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'overview' || input.workspace_panel == 'evidence'",
          ns = ns,
          mod_notes_ui(ns("target_notes"), heading = "Research notes")
        )
      )
    ),
    uiOutput(ns("message"))
  )
}

mod_project_home_server <- function(id, db_pool, user, project_id) {
  moduleServer(id, function(input, output, session) {
    back_requested <- reactiveVal(0L)
    archived <- reactiveVal(0L)
    message <- reactiveVal(NULL)
    refresh_token <- reactiveVal(0L)
    identity_revision <- reactiveVal(0L)
    disease_revision <- reactiveVal(0L)
    selected_target_id <- reactiveVal(NULL)
    workspace_panel <- reactiveVal("project")

    project <- reactive({
      refresh_token()
      disease_revision()
      req(user(), project_id())

      get_owned_project(
        db_pool,
        project_id(),
        user()$id
      )
    })

    targets <- reactive({
      refresh_token()
      identity_revision()
      req(user(), project_id())

      list_project_targets(
        db_pool,
        project_id(),
        user()$id
      )
    })

    selected_target <- reactive({
      identity_revision()
      tid <- selected_target_id()
      if (is.null(tid) || is.null(user())) {
        return(NULL)
      }
      get_owned_target(db_pool, tid, user()$id)
    })

    resolver <- mod_target_resolver_server(
      "resolver",
      db_pool,
      user,
      reactive(selected_target_id())
    )
    retrieving_ids <- resolver$retrieving_ids

    overview <- mod_overview_server(
      "overview",
      db_pool,
      user,
      reactive(selected_target_id()),
      identity_revision = reactive(identity_revision()),
      panel_active = reactive(identical(workspace_panel(), "overview"))
    )

    disease_resolver <- mod_disease_resolver_server(
      "disease_resolver",
      db_pool,
      user,
      project
    )

    evidence <- mod_disease_evidence_server(
      "evidence",
      db_pool,
      user,
      reactive(selected_target_id()),
      reactive(project_id()),
      identity_revision = reactive(identity_revision()),
      disease_revision = reactive(disease_revision()),
      panel_active = reactive(identical(workspace_panel(), "evidence"))
    )

    comparison <- mod_ot_comparison_server(
      "compare",
      db_pool,
      user,
      reactive(project_id()),
      identity_revision = reactive(identity_revision()),
      disease_revision = reactive(disease_revision()),
      panel_active = reactive(identical(workspace_panel(), "compare"))
    )

    pathways <- mod_pathways_server(
      "pathways",
      db_pool,
      user,
      reactive(project_id()),
      identity_revision = reactive(identity_revision()),
      panel_active = reactive(identical(workspace_panel(), "pathways"))
    )

    literature <- mod_literature_server(
      "literature",
      db_pool,
      user,
      reactive(project_id()),
      identity_revision = reactive(identity_revision()),
      disease_revision = reactive(disease_revision()),
      panel_active = reactive(identical(workspace_panel(), "literature"))
    )

    structures <- mod_structures_server(
      "structures",
      db_pool,
      user,
      reactive(project_id()),
      identity_revision = reactive(identity_revision()),
      panel_active = reactive(identical(workspace_panel(), "structures"))
    )

    overview_store <- reactiveVal(list())
    evidence_store <- reactiveVal(list())

    observeEvent(list(project_id(), identity_revision()), {
      overview_store(list())
      evidence_store(list())
    }, ignoreInit = TRUE)

    observeEvent(disease_revision(), {
      evidence_store(list())
    }, ignoreInit = TRUE)

    observe({
      tid <- selected_target_id()
      result <- overview$current()
      if (!has_display_text(tid) || is.null(result)) {
        return()
      }
      store <- overview_store()
      store[[as.character(tid)]] <- result
      overview_store(store)
    })

    observe({
      tid <- selected_target_id()
      result <- evidence$current()
      if (!has_display_text(tid) || is.null(result)) {
        return()
      }
      store <- evidence_store()
      store[[as.character(tid)]] <- result
      evidence_store(store)
    })

    workspace_evidence <- reactive({
      list(
        overviews = overview_store(),
        disease_evidence = evidence_store(),
        comparison = comparison$current(),
        pathways = pathways$current(),
        literature = literature$current(),
        structures = structures$current()
      )
    })

    mod_research_server(
      "research",
      db_pool,
      user,
      reactive(project_id()),
      identity_revision = reactive(identity_revision()),
      disease_revision = reactive(disease_revision()),
      panel_active = reactive(identical(workspace_panel(), "research")),
      workspace_evidence = workspace_evidence,
      on_back_live = function() {
        workspace_panel("project")
      }
    )

    mod_notes_server(
      "target_notes",
      db_pool,
      user,
      reactive(project_id()),
      scope = "target",
      target_id = reactive(selected_target_id()),
      heading_symbol = reactive(target_workspace_label(selected_target()))
    )

    observe({
      updateSelectInput(
        session,
        "workspace_panel",
        selected = workspace_panel()
      )
    })

    observeEvent(resolver$changed(), {
      refresh_token(refresh_token() + 1L)
      identity_revision(identity_revision() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(disease_resolver$changed(), {
      refresh_token(refresh_token() + 1L)
      disease_revision(disease_revision() + 1L)
    }, ignoreInit = TRUE)

    open_target <- function(target_id, preferred_panel = NULL) {
      row <- get_owned_target(db_pool, target_id, user()$id)
      if (is.null(row)) {
        message(list(
          type = "error",
          text = "This target was not found or you do not have access to it."
        ))
        return()
      }

      selected_target_id(as.character(row$id[[1]]))
      if (target_is_confirmed(row) && identical(preferred_panel, "overview")) {
        workspace_panel("overview")
      } else if (target_is_confirmed(row) && identical(preferred_panel, "evidence")) {
        workspace_panel("evidence")
      } else {
        workspace_panel("resolver")
      }
    }

    output$workspace_nav <- renderUI({
      ns <- session$ns
      p <- project()
      row <- selected_target()
      panel <- workspace_panel()
      title <- if (is.null(p)) "Project" else p$title[[1]]
      target_label <- target_workspace_label(row)
      panel_label <- workspace_panel_label(panel)
      back_label <- workspace_back_label(panel)

      div(
        class = "workspace-nav",
        div(
          class = "workspace-breadcrumb",
          actionButton(ns("nav_projects"), "Projects", class = "crumb-link"),
          span(class = "crumb-sep", "/"),
          actionButton(ns("nav_project"), title, class = "crumb-link"),
          if (has_display_text(target_label) && !panel %in% c("project", "compare", "pathways", "literature", "structures", "research")) {
            tagList(
              span(class = "crumb-sep", "/"),
              actionButton(ns("nav_target"), target_label, class = "crumb-link")
            )
          },
          if (has_display_text(panel_label)) {
            tagList(
              span(class = "crumb-sep", "/"),
              span(class = "crumb-current", panel_label)
            )
          }
        ),
        div(
          class = "workspace-nav-actions",
          actionButton(ns("nav_back"), paste("\u2190", back_label), class = "btn-nav-back"),
          tags$details(
            class = "project-actions",
            tags$summary("Project actions"),
            actionButton(ns("archive"), "Archive project", class = "btn-danger-quiet")
          )
        )
      )
    })

    output$project_header <- renderUI({
      p <- project()

      if (is.null(p)) {
        return(
          div(
            class = "form-message error",
            "This project was not found or you do not have access to it."
          )
        )
      }

      div(
        class = "project-hero",
        `data-tour` = "project-header",
        h1(p$title[[1]]),
        p(class = "research-question", p$research_question[[1]]),
        div(
          class = "context-row",
          span(p$organism[[1]]),
          span(
            if (project_disease_is_confirmed(p)) {
              tagList(
                paste(p$disease_name[[1]], "\u00b7"),
                identifier_text(p$disease_ontology_id[[1]])
              )
            } else {
              p$disease_label[[1]]
            }
          )
        )
      )
    })

    output$project_tabs <- renderUI({
      ns <- session$ns
      panel <- workspace_panel()
      div(
        class = "project-tabs",
        role = "navigation",
        `aria-label` = "Project",
        tab_button(ns("project_nav"), "project", "Project", !panel %in% c("compare", "pathways", "literature", "structures", "research")),
        tab_button(
          ns("project_nav"),
          "compare",
          "Compare evidence",
          identical(panel, "compare"),
          data_tour = "compare-nav"
        ),
        tab_button(
          ns("project_nav"),
          "pathways",
          "Pathways",
          identical(panel, "pathways")
        ),
        tab_button(
          ns("project_nav"),
          "literature",
          "Literature",
          identical(panel, "literature")
        ),
        tab_button(
          ns("project_nav"),
          "structures",
          "Structures",
          identical(panel, "structures")
        ),
        tab_button(
          ns("project_nav"),
          "research",
          "Notes / Snapshots",
          identical(panel, "research")
        )
      )
    })

    output$target_subnav <- renderUI({
      ns <- session$ns
      panel <- workspace_panel()
      row <- selected_target()
      if (is.null(row) || !panel %in% c("overview", "evidence", "resolver")) {
        return(NULL)
      }

      symbol <- target_workspace_label(row)
      confirmed <- target_is_confirmed(row)
      div(
        class = "target-subnav",
        `data-tour` = "overview-nav",
        role = "navigation",
        `aria-label` = paste(symbol, "workspace"),
        span(class = "eyebrow", symbol),
        if (isTRUE(confirmed)) {
          tagList(
            tab_button(ns("target_nav"), "overview", "Overview", identical(panel, "overview")),
            tab_button(ns("target_nav"), "evidence", "Disease evidence", identical(panel, "evidence"))
          )
        } else {
          tab_button(ns("target_nav"), "resolver", "Identity", identical(panel, "resolver"))
        }
      )
    })

    output$project_stage <- renderUI({
      ns <- session$ns
      p <- project()
      target_data <- targets()
      n_confirmed <- if (is.null(target_data) || nrow(target_data) == 0) {
        0L
      } else {
        sum(target_data$resolution_status == "confirmed")
      }
      n_need <- if (is.null(target_data)) 0L else nrow(target_data) - n_confirmed
      gate <- comparison_gate(p, target_data)
      path_gate <- pathway_gate(target_data)
      lit_gate <- literature_gate(p, target_data)
      struct_gate <- structure_gate(target_data)

      tagList(
        mod_disease_resolver_ui(ns("disease_resolver")),
        div(
          class = "overview-section",
          h3("Project status"),
          tags$ul(
            class = "project-status-list",
            tags$li(
              if (isTRUE(project_disease_is_confirmed(p))) {
                "Disease identity confirmed"
              } else {
                "Disease identity needs confirmation"
              }
            ),
            tags$li(sprintf("%s confirmed target%s", n_confirmed, if (identical(n_confirmed, 1L)) "" else "s")),
            tags$li(sprintf("%s target%s requiring resolution", n_need, if (identical(n_need, 1L)) "" else "s"))
          ),
          p(
            class = "panel-intro",
            "Select a target to investigate."
          ),
          tags$details(
            class = "project-edit",
            tags$summary("Edit project"),
            p(
              class = "field-help",
              "Title and research question are presentation only. Changing disease wording resets the live disease identity. Adding a target starts unresolved. Removing a confirmed identity does not rewrite snapshots."
            ),
            textInput(ns("edit_title"), "Project title", value = p$title[[1]]),
            textAreaInput(
              ns("edit_question"),
              "Research question",
              value = p$research_question[[1]],
              rows = 3
            ),
            actionButton(ns("save_presentation"), "Save title and question", class = "btn-primary-quiet"),
            textInput(ns("edit_disease"), "Disease context", value = p$disease_label[[1]]),
            p(
              class = "field-help",
              "This clears live Open Targets, Compare, and Literature for the project. Overview, pathways, and structures stay. Snapshots stay."
            ),
            actionButton(ns("save_disease"), "Change disease context", class = "btn-primary-quiet"),
            textInput(ns("add_target_text"), "Add candidate target"),
            actionButton(ns("add_target"), "Add target", class = "btn-primary-quiet"),
            if (!is.null(target_data) && nrow(target_data) > 0) {
              tagList(
                h4("Live targets"),
                tags$ul(
                  class = "project-status-list",
                  lapply(seq_len(nrow(target_data)), function(i) {
                    row <- target_data[i, ]
                    label <- if (identical(as.character(row$resolution_status), "confirmed") &&
                      has_display_text(row$display_symbol)) {
                      row$display_symbol
                    } else {
                      row$input_text
                    }
                    tags$li(
                      span(label),
                      if (identical(as.character(row$resolution_status), "confirmed")) {
                        tags$button(
                          type = "button",
                          class = "btn-text",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                            ns("reset_identity"),
                            row$id
                          ),
                          "Reset identity"
                        )
                      },
                      tags$button(
                        type = "button",
                        class = "btn-text",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                          ns("remove_target"),
                          row$id
                        ),
                        "Remove"
                      )
                    )
                  })
                ),
                checkboxInput(
                  ns("confirm_delete_target_notes"),
                  "I understand that removing a target deletes its live target notes (snapshots stay).",
                  value = FALSE
                )
              )
            }
          ),
          div(
            class = "next-actions",
            if (isTRUE(gate$ok)) {
              actionButton(
                ns("open_compare"),
                "Compare evidence",
                class = "btn-primary-quiet"
              )
            } else {
              span(class = "field-help", gate$message)
            },
            if (isTRUE(path_gate$ok)) {
              actionButton(
                ns("open_pathways"),
                "Pathways",
                class = "btn-primary-quiet"
              )
            } else {
              span(class = "field-help", path_gate$message)
            },
            if (isTRUE(lit_gate$ok)) {
              actionButton(
                ns("open_literature"),
                "Literature",
                class = "btn-primary-quiet"
              )
            } else {
              span(class = "field-help", lit_gate$message)
            },
            if (isTRUE(struct_gate$ok)) {
              actionButton(
                ns("open_structures"),
                "Structures",
                class = "btn-primary-quiet"
              )
            } else {
              span(class = "field-help", struct_gate$message)
            },
            actionButton(
              ns("open_research"),
              "Notes / Snapshots",
              class = "btn-primary-quiet"
            )
          )
        )
      )
    })

    output$targets <- renderUI({
      ns <- session$ns
      target_data <- targets()
      retrieving <- retrieving_ids()

      if (nrow(target_data) == 0) {
        return(p("No candidate targets."))
      }

      div(
        class = "target-list",
        `data-tour` = "identity",
        lapply(seq_len(nrow(target_data)), function(i) {
          target <- target_data[i, ]
          status <- as.character(target$resolution_status)
          selected <- identical(as.character(target$id), as.character(selected_target_id())) &&
            !workspace_panel() %in% c("project", "compare", "pathways", "literature", "structures", "research")
          row_class <- "target-select"
          if (isTRUE(selected)) {
            row_class <- paste(row_class, "is-selected")
          }
          is_retrieving <- as.character(target$id) %in% retrieving
          if (isTRUE(is_retrieving)) {
            row_class <- paste(row_class, "is-retrieving")
          }

          tags$button(
            type = "button",
            class = row_class,
            `aria-pressed` = if (isTRUE(selected)) "true" else "false",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              ns("select_target"),
              target$id
            ),
            strong(if (identical(status, "confirmed") && has_display_text(target$display_symbol)) {
              target$display_symbol
            } else {
              target$input_text
            }),
            if (is_retrieving) {
              span(class = "target-ids", "Retrieving identity\u2026")
            } else if (identical(status, "confirmed")) {
              span(class = "target-ids", identifier_text(target$ensembl_gene_id))
            } else {
              span(class = "target-ids", "Identity not confirmed")
            },
            span(
              class = if (is_retrieving) {
                "status-pill status-pill-info"
              } else {
                resolution_status_class(status)
              },
              if (is_retrieving) "Retrieving" else resolution_status_label(status)
            )
          )
        })
      )
    })

    observeEvent(input$project_nav, {
      req(input$project_nav)
      message(NULL)
      if (identical(input$project_nav, "compare")) {
        workspace_panel("compare")
      } else if (identical(input$project_nav, "pathways")) {
        workspace_panel("pathways")
      } else if (identical(input$project_nav, "literature")) {
        workspace_panel("literature")
      } else if (identical(input$project_nav, "structures")) {
        workspace_panel("structures")
      } else if (identical(input$project_nav, "research")) {
        workspace_panel("research")
      } else {
        selected_target_id(NULL)
        workspace_panel("project")
      }
    })

    observeEvent(input$target_nav, {
      req(input$target_nav, selected_target_id())
      workspace_panel(input$target_nav)
    })

    observeEvent(input$open_compare, {
      message(NULL)
      workspace_panel("compare")
    })

    observeEvent(input$open_pathways, {
      message(NULL)
      workspace_panel("pathways")
    })

    observeEvent(input$open_literature, {
      message(NULL)
      workspace_panel("literature")
    })

    observeEvent(input$open_structures, {
      message(NULL)
      workspace_panel("structures")
    })

    observeEvent(input$open_research, {
      message(NULL)
      workspace_panel("research")
    })

    observeEvent(comparison$inspect(), {
      tid <- comparison$inspect()
      if (has_display_text(tid)) {
        message(NULL)
        open_target(tid, preferred_panel = "evidence")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$select_target, {
      req(user(), input$select_target)
      message(NULL)
      open_target(input$select_target, preferred_panel = "overview")
    })

    observeEvent(input$select_evidence, {
      req(user(), input$select_evidence)
      message(NULL)
      open_target(input$select_evidence, preferred_panel = "evidence")
    })

    observeEvent(input$nav_project, {
      selected_target_id(NULL)
      workspace_panel("project")
    })

    observeEvent(input$nav_target, {
      if (!is.null(selected_target_id())) {
        row <- selected_target()
        if (target_is_confirmed(row)) {
          workspace_panel("overview")
        } else {
          workspace_panel("resolver")
        }
      }
    })

    observeEvent(input$nav_back, {
      dest <- workspace_back_destination(workspace_panel())
      if (identical(dest, "project")) {
        selected_target_id(NULL)
        workspace_panel("project")
      } else {
        selected_target_id(NULL)
        workspace_panel("project")
        back_requested(back_requested() + 1L)
      }
    })

    observeEvent(input$nav_projects, {
      selected_target_id(NULL)
      workspace_panel("project")
      back_requested(back_requested() + 1L)
    })

    output$message <- renderUI({
      current <- message()
      if (is.null(current)) return(NULL)

      div(
        class = paste("form-message", current$type),
        current$text
      )
    })

    observeEvent(workspace_panel(), {
      u <- user()
      req(u, project_id())
      if (isTRUE(user_needs_workspace_tour(u))) {
        return()
      }
      panel <- workspace_panel()
      send_tip <- function(name, step) {
        if (isTRUE(tip_seen(u, name))) {
          return()
        }
        session$sendCustomMessage(
          "twTour",
          tour_client_payload("tip", mode = "tip", tip = name, steps = list(step))
        )
      }
      if (identical(panel, "overview")) {
        send_tip("overview", overview_tip_step())
      } else if (identical(panel, "evidence")) {
        send_tip("evidence", evidence_tip_step())
      } else if (identical(panel, "compare")) {
        send_tip("compare", compare_tip_step())
      } else if (identical(panel, "pathways")) {
        send_tip("pathways", pathways_tip_step())
      } else if (identical(panel, "literature")) {
        send_tip("literature", literature_tip_step())
      } else if (identical(panel, "structures")) {
        send_tip("structures", structures_tip_step())
      } else if (identical(panel, "research")) {
        send_tip("research", research_tip_step())
      }
    }, ignoreInit = TRUE)

    observeEvent(input$save_presentation, {
      req(user(), project_id())
      result <- update_project_presentation(
        db_pool,
        project_id(),
        user()$id,
        input$edit_title,
        input$edit_question
      )
      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        return()
      }
      refresh_token(refresh_token() + 1L)
      message(list(type = "info", text = "Title and research question updated. Evidence was not invalidated."))
    }, ignoreInit = TRUE)

    observeEvent(input$save_disease, {
      req(user(), project_id())
      result <- reset_project_disease(
        db_pool,
        project_id(),
        user()$id,
        input$edit_disease
      )
      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        return()
      }
      refresh_token(refresh_token() + 1L)
      disease_revision(disease_revision() + 1L)
      evidence_store(list())
      message(list(
        type = "info",
        text = "Disease context updated. Live Open Targets, Compare, and Literature were cleared. Snapshots were not changed."
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$add_target, {
      req(user(), project_id())
      result <- add_project_target(
        db_pool,
        project_id(),
        user()$id,
        input$add_target_text
      )
      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        return()
      }
      updateTextInput(session, "add_target_text", value = "")
      refresh_token(refresh_token() + 1L)
      identity_revision(identity_revision() + 1L)
      message(list(type = "info", text = "Target added as unresolved. It is not auto-confirmed."))
    }, ignoreInit = TRUE)

    observeEvent(input$remove_target, {
      req(user(), project_id(), input$remove_target)
      tid <- input$remove_target
      result <- remove_project_target(
        db_pool,
        project_id(),
        user()$id,
        tid,
        confirm_note_deletion = isTRUE(input$confirm_delete_target_notes)
      )
      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        return()
      }
      if (identical(as.character(selected_target_id()), as.character(tid))) {
        selected_target_id(NULL)
        workspace_panel("project")
      }
      refresh_token(refresh_token() + 1L)
      identity_revision(identity_revision() + 1L)
      message(list(type = "info", text = "Target removed from the live project. Snapshots were not changed."))
    }, ignoreInit = TRUE)

    observeEvent(input$reset_identity, {
      req(user(), input$reset_identity)
      result <- reset_confirmed_target(db_pool, input$reset_identity, user()$id)
      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        return()
      }
      refresh_token(refresh_token() + 1L)
      identity_revision(identity_revision() + 1L)
      message(list(
        type = "info",
        text = "Confirmed identity reset. Live evidence for that target was invalidated. Snapshots were not changed."
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$archive, {
      req(user(), project_id())

      ok <- archive_project(
        db_pool,
        project_id(),
        user()$id
      )

      if (!ok) {
        message(list(
          type = "error",
          text = "The project could not be archived."
        ))
        return()
      }

      selected_target_id(NULL)
      workspace_panel("project")
      archived(archived() + 1L)
    })

    list(
      back_requested = reactive(back_requested()),
      archived = reactive(archived()),
      workspace_ready = reactive({
        refresh_token()
        pid <- project_id()
        if (is.null(pid) || !nzchar(as.character(pid))) {
          return(NULL)
        }
        as.character(pid)
      }),
      refresh = function() {
        selected_target_id(NULL)
        workspace_panel("project")
        refresh_token(refresh_token() + 1L)
        identity_revision(identity_revision() + 1L)
      }
    )
  })
}
