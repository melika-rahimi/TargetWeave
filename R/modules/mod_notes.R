notes_panel_ui <- function(ns, rows, heading, save_id = "save", body_id = "body_text", delete_id = "delete_note") {
  tagList(
    div(
      class = "notes-shell",
      h3(heading),
      p(class = "field-help", "Private to this project. Notes are not sent to scientific APIs."),
      textAreaInput(ns(body_id), "Research note", value = "", rows = 3, width = "100%"),
      actionButton(ns(save_id), "Add note", class = "btn-primary-quiet"),
      if (is.null(rows) || nrow(rows) == 0) {
        panel_state_ui("empty", "No notes yet", "Add a note to keep investigation context with this project.")
      } else {
        div(
          class = "note-list",
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, ]
            div(
              class = "note-card",
              p(row$body[[1]]),
              p(
                class = "note-meta",
                sprintf("Last edited %s", format_user_timestamp(row$updated_at[[1]]))
              ),
              tags$button(
                type = "button",
                class = "btn-text",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                  ns(delete_id),
                  row$id[[1]]
                ),
                "Delete"
              )
            )
          })
        )
      }
    )
  )
}

mod_notes_ui <- function(id, heading = "Research notes") {
  ns <- NS(id)
  div(
    class = "notes-shell",
    h3(heading),
    uiOutput(ns("body"))
  )
}

mod_notes_server <- function(
  id,
  db_pool,
  user,
  project_id,
  scope = "project",
  target_id = reactive(NULL),
  snapshot_id = reactive(NULL),
  heading_symbol = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    revision <- reactiveVal(0L)
    message <- reactiveVal(NULL)

    notes <- reactive({
      revision()
      req(user(), project_id())
      list_research_notes(
        db_pool,
        project_id(),
        user()$id,
        scope = scope,
        project_target_id = if (identical(scope, "target")) target_id() else NULL,
        snapshot_id = if (identical(scope, "snapshot")) snapshot_id() else NULL
      )
    })

    output$body <- renderUI({
      ns <- session$ns
      req(user(), project_id())
      if (identical(scope, "target") && !has_display_text(target_id())) {
        return(NULL)
      }
      if (identical(scope, "snapshot") && !has_display_text(snapshot_id())) {
        return(NULL)
      }
      rows <- notes()
      title <- if (identical(scope, "target") && has_display_text(heading_symbol())) {
        sprintf("Notes for %s", heading_symbol())
      } else if (identical(scope, "snapshot")) {
        "Notes about this snapshot"
      } else {
        "Project notes"
      }
      tagList(
        p(class = "field-help", title),
        if (!is.null(message())) div(class = "form-message", message()),
        textAreaInput(ns("body_text"), "Research note", value = "", rows = 3, width = "100%"),
        actionButton(ns("save"), "Add note", class = "btn-primary-quiet"),
        if (is.null(rows) || nrow(rows) == 0) {
          panel_state_ui("empty", "No notes yet", "Add a note to keep investigation context with this project.")
        } else {
          div(
            class = "note-list",
            lapply(seq_len(nrow(rows)), function(i) {
              row <- rows[i, ]
              div(
                class = "note-card",
                p(row$body[[1]]),
                p(
                  class = "note-meta",
                  sprintf("Last edited %s", format_user_timestamp(row$updated_at[[1]]))
                ),
                actionButton(
                  ns(paste0("delete_", row$id[[1]])),
                  "Delete",
                  class = "btn-text",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    ns("delete_note"),
                    row$id[[1]]
                  )
                )
              )
            })
          )
        }
      )
    })

    observeEvent(input$save, {
      message(NULL)
      result <- create_research_note(
        db_pool,
        user()$id,
        project_id(),
        input$body_text,
        scope = scope,
        project_target_id = target_id(),
        snapshot_id = snapshot_id()
      )
      if (!isTRUE(result$ok)) {
        message(result$error)
        return()
      }
      updateTextAreaInput(session, "body_text", value = "")
      revision(revision() + 1L)
    })

    observeEvent(input$delete_note, {
      req(input$delete_note)
      delete_research_note(db_pool, input$delete_note, user()$id)
      revision(revision() + 1L)
    })

    list(revision = reactive(revision()))
  })
}
