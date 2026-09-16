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
          tab_panel_shell(
            loading_label = "Loading project\u2026",
            uiOutput(ns("project_stage"))
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'resolver'",
          ns = ns,
          mod_target_resolver_ui(ns("resolver"))
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'overview'",
          ns = ns,
          tab_panel_shell(
            loading_label = "Loading overview\u2026",
            mod_overview_ui(ns("overview"))
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'evidence'",
          ns = ns,
          tab_panel_shell(
            loading_label = "Loading disease evidence\u2026",
            mod_disease_evidence_ui(ns("evidence"))
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'compare'",
          ns = ns,
          tab_panel_shell(
            loading_label = "Loading compare evidence\u2026",
            mod_ot_comparison_ui(ns("compare"))
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'pathways'",
          ns = ns,
          tab_panel_shell(
            loading_label = "Loading pathways\u2026",
            mod_pathways_ui(ns("pathways"))
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'literature'",
          ns = ns,
          tab_panel_shell(
            loading_label = "Loading literature\u2026",
            mod_literature_ui(ns("literature"))
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'structures'",
          ns = ns,
          tab_panel_shell(
            loading_label = "Loading structures\u2026",
            mod_structures_ui(ns("structures"))
          )
        ),
        conditionalPanel(
          condition = "input.workspace_panel == 'research'",
          ns = ns,
          tab_panel_shell(
            loading_label = "Loading notes and snapshots\u2026",
            mod_research_ui(ns("research"))
          )
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

live_target_display_label <- function(row) {
  if (identical(as.character(row$resolution_status[[1]]), "confirmed") &&
    has_display_text(row$display_symbol[[1]])) {
    return(as.character(row$display_symbol[[1]]))
  }
  as.character(row$input_text[[1]])
}

manage_project_button_ui <- function(ns) {
  actionButton(
    ns("open_manage"),
    "Manage project",
    class = "btn-primary-quiet btn-manage-project"
  )
}

project_confirmed_ids <- function(target_data) {
  if (is.null(target_data) || nrow(target_data) == 0) {
    return(character())
  }
  as.character(target_data$id[target_data$resolution_status == "confirmed"])
}

project_readiness_state <- function(current, gate_ok) {
  if (!isTRUE(gate_ok)) {
    return(list(label = "Unavailable", kind = "unavailable"))
  }
  if (is.null(current)) {
    return(list(label = "Not retrieved", kind = "missing"))
  }
  status <- as.character(current$status %||% "")
  if (isTRUE(current$stale_target_set) || identical(status, "stale")) {
    return(list(label = "Stale", kind = "stale"))
  }
  if (status %in% c("blocked", "blocked_targets", "blocked_disease", "blocked_target")) {
    return(list(label = "Unavailable", kind = "unavailable"))
  }
  if (status %in% c("error", "unavailable")) {
    return(list(label = "Unavailable", kind = "unavailable"))
  }
  if (identical(status, "ready")) {
    return(list(label = "Current", kind = "current"))
  }
  has_payload <- !is.null(current$overview) ||
    !is.null(current$evidence) ||
    !is.null(current$comparison) ||
    !is.null(current$pathways) ||
    !is.null(current$literature) ||
    !is.null(current$structures)
  if (isTRUE(has_payload)) {
    return(list(label = "Current", kind = "current"))
  }
  list(label = "Not retrieved", kind = "missing")
}

project_store_readiness_state <- function(store, confirmed_ids, payload_fn, gate_ok = TRUE) {
  if (!isTRUE(gate_ok) || length(confirmed_ids) == 0L) {
    return(list(label = "Unavailable", kind = "unavailable"))
  }
  if (is.null(store) || length(store) == 0L) {
    return(list(label = "Not retrieved", kind = "missing"))
  }
  any_payload <- FALSE
  any_stale <- FALSE
  for (id in confirmed_ids) {
    item <- store[[as.character(id)]]
    if (is.null(item)) {
      next
    }
    status <- as.character(item$status %||% "")
    if (isTRUE(item$stale_target_set) || identical(status, "stale")) {
      any_stale <- TRUE
    }
    if (isTRUE(payload_fn(item))) {
      any_payload <- TRUE
    }
  }
  if (isTRUE(any_stale) && isTRUE(any_payload)) {
    return(list(label = "Stale", kind = "stale"))
  }
  if (isTRUE(any_payload)) {
    return(list(label = "Current", kind = "current"))
  }
  list(label = "Not retrieved", kind = "missing")
}

project_readiness_row_ui <- function(label, state, action = NULL, hint = NULL) {
  tags$tr(
    class = paste("project-readiness-row", paste0("is-", state$kind)),
    tags$th(label),
    tags$td(
      span(
        class = paste("project-readiness-state", paste0("is-", state$kind)),
        state$label
      )
    ),
    tags$td(
      class = "project-readiness-action",
      action %||% if (has_display_text(hint)) span(class = "field-help", hint) else NULL
    )
  )
}

project_evidence_readiness_ui <- function(ns, project_row, target_data, evidence = NULL) {
  evidence <- evidence %||% list()
  confirmed_ids <- project_confirmed_ids(target_data)
  n_confirmed <- length(confirmed_ids)
  disease_ok <- isTRUE(project_disease_is_confirmed(project_row))
  compare_gate <- comparison_gate(project_row, target_data)
  path_gate <- pathway_gate(target_data)
  lit_gate <- literature_gate(project_row, target_data)
  struct_gate <- structure_gate(target_data)

  overview_state <- project_store_readiness_state(
    evidence$overviews,
    confirmed_ids,
    function(item) !is.null(item$overview),
    gate_ok = n_confirmed >= 1L
  )
  disease_state <- project_store_readiness_state(
    evidence$disease_evidence,
    confirmed_ids,
    function(item) !is.null(item$evidence) || identical(item$status, "ready") || identical(item$status, "empty") || identical(item$status, "stale"),
    gate_ok = isTRUE(disease_ok) && n_confirmed >= 1L
  )

  div(
    class = "project-readiness",
    evidence_section_header(
      "Evidence",
      "Retrieved live evidence for this project. Stale means the confirmed target set or disease identity changed after retrieval."
    ),
    tags$table(
      class = "evidence-table project-readiness-table",
      tags$thead(
        tags$tr(
          tags$th("Workspace"),
          tags$th("State"),
          tags$th("Continue")
        )
      ),
      tags$tbody(
        project_readiness_row_ui(
          "Overview",
          overview_state,
          hint = if (identical(overview_state$kind, "unavailable")) {
            "Confirm a target identity, then open it from the target list."
          } else {
            "Open a confirmed target from the target list."
          }
        ),
        project_readiness_row_ui(
          "Disease evidence",
          disease_state,
          hint = if (identical(disease_state$kind, "unavailable")) {
            if (!isTRUE(disease_ok)) {
              "Confirm the project disease identity first."
            } else {
              "Confirm a target identity, then open Disease evidence."
            }
          } else {
            "Open Disease evidence from a confirmed target."
          }
        ),
        project_readiness_row_ui(
          "Compare evidence",
          project_readiness_state(evidence$comparison, compare_gate$ok),
          action = if (isTRUE(compare_gate$ok)) {
            actionButton(ns("open_compare"), "Compare evidence", class = "btn-text")
          } else {
            span(class = "field-help", compare_gate$message)
          }
        ),
        project_readiness_row_ui(
          "Pathways",
          project_readiness_state(evidence$pathways, path_gate$ok),
          action = if (isTRUE(path_gate$ok)) {
            actionButton(ns("open_pathways"), "Pathways", class = "btn-text")
          } else {
            span(class = "field-help", path_gate$message)
          }
        ),
        project_readiness_row_ui(
          "Literature",
          project_readiness_state(evidence$literature, lit_gate$ok),
          action = if (isTRUE(lit_gate$ok)) {
            actionButton(ns("open_literature"), "Literature", class = "btn-text")
          } else {
            span(class = "field-help", lit_gate$message)
          }
        ),
        project_readiness_row_ui(
          "Structures",
          project_readiness_state(evidence$structures, struct_gate$ok),
          action = if (isTRUE(struct_gate$ok)) {
            actionButton(ns("open_structures"), "Structures", class = "btn-text")
          } else {
            span(class = "field-help", struct_gate$message)
          }
        )
      )
    ),
    p(
      class = "project-secondary-actions",
      actionButton(ns("open_research"), "Notes / Snapshots", class = "btn-text")
    )
  )
}

project_home_stage_ui <- function(ns, project_row, target_data, events = NULL, evidence = NULL) {
  n_confirmed <- if (is.null(target_data) || nrow(target_data) == 0) {
    0L
  } else {
    sum(target_data$resolution_status == "confirmed")
  }
  n_need <- if (is.null(target_data)) 0L else nrow(target_data) - n_confirmed
  tagList(
    div(
      class = "project-target-set",
      evidence_section_header(
        "Target set",
        "Confirmed identities can retrieve evidence. Unresolved identifiers stay in the project until they are confirmed or removed."
      ),
      p(
        class = "project-target-set-summary",
        sprintf(
          "%s confirmed \u00b7 %s unresolved",
          n_confirmed,
          n_need
        )
      ),
      p(
        class = "field-help",
        "Use the target list to open a confirmed target or resolve an unresolved identity. Add, remove, and re-add remain available."
      )
    ),
    project_evidence_readiness_ui(ns, project_row, target_data, evidence),
    evidence_details_disclosure(
      "Project history",
      p(
        class = "field-help",
        "Newest first. Identity confirmation is a scientific record; adding or removing a target is a project edit. Snapshots are never rewritten."
      ),
      project_timeline_ui(events)
    )
  )
}

project_sidebar_target_ui <- function(ns, target, selected = FALSE, retrieving = FALSE) {
  status <- as.character(target$resolution_status)
  row_class <- "target-select"
  if (isTRUE(selected)) {
    row_class <- paste(row_class, "is-selected")
  }
  if (isTRUE(retrieving)) {
    row_class <- paste(row_class, "is-retrieving")
  }
  symbol <- if (identical(status, "confirmed") && has_display_text(target$display_symbol)) {
    target$display_symbol
  } else {
    target$input_text
  }
  div(
    class = "target-item",
    tags$button(
      type = "button",
      class = row_class,
      `aria-pressed` = if (isTRUE(selected)) "true" else "false",
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
        ns("select_target"),
        target$id
      ),
      strong(symbol),
      if (isTRUE(retrieving)) {
        span(class = "target-ids", "Retrieving identity\u2026")
      } else if (identical(status, "confirmed")) {
        span(class = "target-ids", identifier_text(target$ensembl_gene_id))
      } else {
        span(class = "target-ids", "Identity not confirmed")
      },
      span(
        class = if (isTRUE(retrieving)) {
          "status-pill status-pill-info"
        } else {
          resolution_status_class(status)
        },
        if (isTRUE(retrieving)) "Retrieving" else resolution_status_label(status)
      )
    ),
    if (!identical(status, "confirmed") && !isTRUE(retrieving)) {
      tagList(
        p(class = "target-unresolved-note", "Confirm this identifier before retrieving evidence."),
        resolve_identity_button_ui(ns, target$id)
      )
    }
  )
}

add_target_sidebar_ui <- function(ns, n_active, max_targets, form_open = FALSE) {
  at_cap <- as.integer(n_active) >= as.integer(max_targets)
  tagList(
    div(
      class = "targets-heading-row",
      h3("Targets"),
      span(class = "targets-count", sprintf("%s / %s", n_active, max_targets))
    ),
    if (isTRUE(at_cap)) {
      tagList(
        tags$button(
          type = "button",
          class = "btn-add-target",
          disabled = NA,
          "+ Add target"
        ),
        p(class = "field-help", "Maximum 8 active targets")
      )
    } else {
      tagList(
        actionButton(ns("toggle_add_target"), "+ Add target", class = "btn-add-target"),
        if (isTRUE(form_open)) {
          div(
            class = "add-target-form",
            textInput(ns("add_target_text"), NULL, placeholder = "Symbol or identifier"),
            actionButton(ns("add_target"), "Add", class = "btn-primary-quiet")
          )
        }
      )
    }
  )
}

resolve_identity_button_ui <- function(ns, target_id) {
  tags$button(
    type = "button",
    class = "btn-resolve-identity",
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      ns("select_target"),
      target_id
    ),
    "Resolve identity"
  )
}

manage_project_body_ui <- function(ns, project_row, target_data) {
  p <- project_row
  disease_confirmed <- isTRUE(project_disease_is_confirmed(p))
  div(
    class = "manage-project",
    div(
      class = "manage-section",
      h4("Project details"),
      p(class = "field-help", "Title and research question are presentation only. They do not invalidate evidence."),
      textInput(ns("edit_title"), "Project title", value = p$title[[1]]),
      textAreaInput(
        ns("edit_question"),
        "Research question",
        value = p$research_question[[1]],
        rows = 3
      ),
      actionButton(ns("save_presentation"), "Save changes", class = "btn-primary-quiet")
    ),
    div(
      class = "manage-section",
      h4("Disease context"),
      if (isTRUE(disease_confirmed)) {
        tagList(
          p(p$disease_name[[1]]),
          p(class = "identifier", identifier_text(p$disease_ontology_id[[1]]))
        )
      } else {
        p(p$disease_label[[1]])
      },
      textInput(ns("edit_disease"), "Disease context", value = p$disease_label[[1]]),
      p(
        class = "field-help",
        "This clears live Open Targets, Compare, and Literature for the project. Overview, pathways, and structures stay. Snapshots stay."
      ),
      actionButton(ns("save_disease"), "Change disease context", class = "btn-primary-quiet")
    ),
    div(
      class = "manage-section",
      h4("Target management"),
      if (is.null(target_data) || nrow(target_data) == 0) {
        p(class = "field-help", "No active targets.")
      } else {
        tags$ul(
          class = "manage-target-list",
          lapply(seq_len(nrow(target_data)), function(i) {
            row <- target_data[i, ]
            label <- live_target_display_label(row)
            status <- as.character(row$resolution_status[[1]])
            tags$li(
              class = "manage-target-row",
              div(
                class = "manage-target-meta",
                span(class = "manage-target-name", label),
                span(class = resolution_status_class(status), resolution_status_label(status))
              ),
              div(
                class = "target-action-menu",
                span(class = "target-action-menu-label", "Actions"),
                if (identical(status, "confirmed")) {
                  tags$button(
                    type = "button",
                    class = "btn-text",
                    `data-tw-input` = ns("request_reset_identity"),
                    `data-tw-value` = as.character(row$id[[1]]),
                    "Reset identity"
                  )
                },
                tags$button(
                  type = "button",
                  class = "btn-text",
                  `data-tw-input` = ns("request_remove_target"),
                  `data-tw-value` = as.character(row$id[[1]]),
                  "Remove from project"
                )
              )
            )
          })
        )
      }
    )
  )
}

remove_target_confirm_ui <- function(ns, label) {
  shiny::modalDialog(
    title = sprintf("Remove %s from the live project?", label),
    easyClose = FALSE,
    p(sprintf("%s will be removed from the current workspace.", label)),
    p("Existing snapshots, notes, and project history will not be rewritten."),
    footer = tagList(
      actionButton(ns("cancel_remove"), "Cancel", class = "btn-ghost"),
      actionButton(
        ns("confirm_remove_go"),
        sprintf("Remove %s", label),
        class = "btn-danger-quiet"
      )
    )
  )
}

reset_identity_confirm_ui <- function(ns, label) {
  shiny::modalDialog(
    title = sprintf("Reset %s identity?", label),
    easyClose = FALSE,
    tags$ul(
      class = "project-status-list",
      tags$li(sprintf("%s stays in the project.", label)),
      tags$li("Its confirmed scientific identity is cleared."),
      tags$li("Affected live evidence may become stale."),
      tags$li("Snapshots remain unchanged.")
    ),
    footer = tagList(
      actionButton(ns("cancel_reset"), "Cancel", class = "btn-ghost"),
      actionButton(ns("confirm_reset_go"), "Reset identity", class = "btn-primary-quiet")
    )
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
    add_form_open <- reactiveVal(FALSE)
    pending_remove_id <- reactiveVal(NULL)
    pending_reset_id <- reactiveVal(NULL)
    return_to_manage <- reactiveVal(FALSE)

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

    show_manage_modal <- function() {
      p <- isolate(project())
      target_data <- isolate(targets())
      if (is.null(p)) {
        return()
      }
      replace_shiny_modal(
        session,
        shiny::modalDialog(
          title = "Manage project",
          easyClose = TRUE,
          size = "l",
          footer = shiny::modalButton("Close"),
          manage_project_body_ui(session$ns, p, target_data)
        )
      )
    }

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
          manage_project_button_ui(ns),
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
        if (has_display_text(p$research_question[[1]])) {
          p(class = "research-question", p$research_question[[1]])
        },
        div(
          class = "project-context",
          div(
            class = "project-context-item",
            span(class = "project-context-label", "Disease"),
            span(
              class = "project-context-value",
              if (project_disease_is_confirmed(p)) {
                tagList(
                  p$disease_name[[1]],
                  " \u00b7 ",
                  identifier_text(p$disease_ontology_id[[1]])
                )
              } else {
                tagList(
                  p$disease_label[[1]],
                  span(class = "status-pill status-pill-warning", "Needs confirmation")
                )
              }
            )
          ),
          div(
            class = "project-context-item",
            span(class = "project-context-label", "Species"),
            span(class = "project-context-value", p$organism[[1]])
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
      tagList(
        mod_disease_resolver_ui(ns("disease_resolver")),
        project_home_stage_ui(
          ns,
          project(),
          targets(),
          events = list_project_events(db_pool, project_id(), user()$id),
          evidence = workspace_evidence()
        )
      )
    })

    output$targets <- renderUI({
      ns <- session$ns
      target_data <- targets()
      retrieving <- retrieving_ids()
      max_targets <- get_app_config()$max_targets
      n_active <- if (is.null(target_data)) 0L else nrow(target_data)

      tagList(
        add_target_sidebar_ui(ns, n_active, max_targets, form_open = add_form_open()),
        if (n_active == 0) {
          p("No candidate targets.")
        } else {
          div(
            class = "target-list",
            `data-tour` = "identity",
            lapply(seq_len(nrow(target_data)), function(i) {
              target <- target_data[i, ]
              selected <- identical(as.character(target$id), as.character(selected_target_id())) &&
                !workspace_panel() %in% c("project", "compare", "pathways", "literature", "structures", "research")
              project_sidebar_target_ui(
                ns,
                target,
                selected = selected,
                retrieving = as.character(target$id) %in% retrieving
              )
            })
          )
        }
      )
    })

    keep_tab_outputs_visible(
      output,
      c("workspace_nav", "project_header", "project_tabs", "target_subnav", "project_stage", "targets")
    )

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
      show_manage_modal()
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
      show_manage_modal()
    }, ignoreInit = TRUE)

    observeEvent(input$toggle_add_target, {
      add_form_open(!isTRUE(add_form_open()))
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
      add_form_open(FALSE)
      refresh_token(refresh_token() + 1L)
      identity_revision(identity_revision() + 1L)
      message(list(type = "info", text = "Target added as unresolved. It is not auto-confirmed."))
    }, ignoreInit = TRUE)

    observeEvent(input$open_manage, {
      return_to_manage(TRUE)
      show_manage_modal()
    }, ignoreInit = TRUE)

    observeEvent(input$request_remove_target, {
      req(user(), input$request_remove_target)
      tid <- input$request_remove_target
      row <- get_owned_target(db_pool, tid, user()$id)
      if (is.null(row)) {
        message(list(type = "error", text = "Target was not found in this project."))
        return()
      }
      pending_remove_id(as.character(tid))
      replace_shiny_modal(
        session,
        remove_target_confirm_ui(session$ns, live_target_display_label(row))
      )
    }, ignoreInit = TRUE)

    observeEvent(input$cancel_remove, {
      pending_remove_id(NULL)
      if (isTRUE(return_to_manage())) {
        show_manage_modal()
      } else {
        shiny::removeModal()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_remove_go, {
      req(user(), project_id(), pending_remove_id())
      tid <- pending_remove_id()
      result <- remove_project_target(
        db_pool,
        project_id(),
        user()$id,
        tid,
        confirm_removal = TRUE
      )
      pending_remove_id(NULL)
      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        if (isTRUE(return_to_manage())) {
          show_manage_modal()
        } else {
          shiny::removeModal()
        }
        return()
      }
      if (identical(as.character(selected_target_id()), as.character(tid))) {
        selected_target_id(NULL)
        workspace_panel("project")
      }
      refresh_token(refresh_token() + 1L)
      identity_revision(identity_revision() + 1L)
      message(list(type = "info", text = "Target removed from the live project. Snapshots were not changed."))
      if (isTRUE(return_to_manage())) {
        show_manage_modal()
      } else {
        shiny::removeModal()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$request_reset_identity, {
      req(user(), input$request_reset_identity)
      tid <- input$request_reset_identity
      row <- get_owned_target(db_pool, tid, user()$id)
      if (is.null(row)) {
        message(list(type = "error", text = "Target was not found in this project."))
        return()
      }
      pending_reset_id(as.character(tid))
      replace_shiny_modal(
        session,
        reset_identity_confirm_ui(session$ns, live_target_display_label(row))
      )
    }, ignoreInit = TRUE)

    observeEvent(input$cancel_reset, {
      pending_reset_id(NULL)
      if (isTRUE(return_to_manage())) {
        show_manage_modal()
      } else {
        shiny::removeModal()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_reset_go, {
      req(user(), pending_reset_id())
      tid <- pending_reset_id()
      result <- reset_confirmed_target(db_pool, tid, user()$id)
      pending_reset_id(NULL)
      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        if (isTRUE(return_to_manage())) {
          show_manage_modal()
        } else {
          shiny::removeModal()
        }
        return()
      }
      refresh_token(refresh_token() + 1L)
      identity_revision(identity_revision() + 1L)
      message(list(
        type = "info",
        text = "Confirmed identity reset. Live evidence for that target was invalidated. Snapshots were not changed."
      ))
      if (isTRUE(return_to_manage())) {
        show_manage_modal()
      } else {
        shiny::removeModal()
      }
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
