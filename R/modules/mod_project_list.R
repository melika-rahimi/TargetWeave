mod_project_list_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "content-stack",
    div(
      class = "section-heading-row",
      div(
        div(class = "eyebrow", "Research workspace"),
        h2("Projects"),
        p(
          "Each project is one investigation: a research question, one disease context, ",
          "and up to eight candidate targets. New project starts that investigation."
        )
      ),
      actionButton(
        ns("new_project"),
        "New project",
        class = "btn-primary-quiet"
      )
    ),
    uiOutput(ns("project_content")),
    checkboxInput(ns("show_archived"), "Show archived projects", value = FALSE)
  )
}

mod_project_list_server <- function(id, db_pool, user) {
  moduleServer(id, function(input, output, session) {
    selected_project <- reactiveVal(NULL)
    new_project_requested <- reactiveVal(0L)
    refresh_token <- reactiveVal(0L)

    projects <- reactive({
      refresh_token()
      req(user())
      list_projects(
        db_pool,
        user()$id,
        include_archived = isTRUE(input$show_archived)
      )
    })

    output$project_content <- renderUI({
      project_data <- projects()
      ns <- session$ns

      if (nrow(project_data) == 0) {
        return(
          div(
            class = "empty-state",
            h3("Start with a research question"),
            p(
              "New project starts an investigation. Add a research question, disease context, ",
              "and candidate targets, then confirm identities before retrieving evidence."
            ),
            actionButton(
              ns("empty_new_project"),
              "Create first project",
              class = "btn-primary-quiet"
            )
          )
        )
      }

      tagList(
        div(
          class = "project-list",
          lapply(seq_len(nrow(project_data)), function(i) {
            project <- project_data[i, ]
            archived <- identical(as.character(project$status), "archived")
            div(
              class = "project-row",
              div(
                class = "project-row-main",
                h3(project$title),
                p(class = "project-question", project$research_question),
                div(
                  class = "project-context",
                  span(project$organism),
                  span(project$disease_label),
                  span(
                    paste0(
                      project$target_count,
                      if (project$target_count == 1) " target" else " targets"
                    )
                  ),
                  if (isTRUE(archived)) span("Archived")
                )
              ),
              tags$button(
                type = "button",
                class = "btn-text",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                  ns("open_project"),
                  project$id
                ),
                "Open"
              ),
              if (isTRUE(archived)) {
                tags$button(
                  type = "button",
                  class = "btn-text",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    ns("restore_project"),
                    project$id
                  ),
                  "Restore"
                )
              }
            )
          })
        )
      )
    })

    observeEvent(input$new_project, {
      new_project_requested(new_project_requested() + 1L)
    })

    observeEvent(input$empty_new_project, {
      new_project_requested(new_project_requested() + 1L)
    })

    observeEvent(input$open_project, {
      req(input$open_project)
      selected_project(input$open_project)
    })

    observeEvent(input$restore_project, {
      req(user(), input$restore_project)
      restore_project(db_pool, input$restore_project, user()$id)
      refresh_token(refresh_token() + 1L)
    }, ignoreInit = TRUE)

    list(
      selected_project = reactive(selected_project()),
      new_project_requested = reactive(new_project_requested()),
      refresh = function() {
        refresh_token(refresh_token() + 1L)
      },
      clear_selection = function() {
        selected_project(NULL)
      }
    )
  })
}
