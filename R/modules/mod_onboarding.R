mod_onboarding_ui <- function(id) {
  ns <- NS(id)
  roles <- research_role_choices()
  fields <- research_field_choices()

  div(
    class = "content-stack onboarding-shell",
    onboarding_stepper(1L),
    h2("About your research"),
    p(
      class = "panel-intro",
      "Tell us a little about your research context so your workspace feels more personal. ",
      "This is optional and does not change scientific results."
    ),
    div(
      class = "form-block",
      textInput(ns("display_name"), "Display name"),
      selectInput(
        ns("research_role"),
        "Research role",
        choices = c("", roles),
        selected = ""
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'Other'", ns("research_role")),
        textInput(ns("research_role_other"), "Describe your role")
      ),
      selectInput(
        ns("research_field"),
        "Research field",
        choices = c("", fields),
        selected = ""
      ),
      p(
        class = "field-help",
        "These fields describe your context. They are not an exhaustive scientific taxonomy."
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'Other'", ns("research_field")),
        textInput(ns("research_field_other"), "Describe your field")
      ),
      textInput(
        ns("institution"),
        "Institution / organization (optional)"
      ),
      div(
        class = "onboarding-actions",
        actionButton(
          ns("continue"),
          "Continue",
          class = "btn-primary-quiet"
        ),
        actionButton(
          ns("skip"),
          "Skip for now",
          class = "btn-text"
        )
      ),
      uiOutput(ns("message"))
    )
  )
}

resolved_choice <- function(selected, other_text) {
  if (!identical(selected, "Other")) {
    return(blank_to_null(selected))
  }
  blank_to_null(other_text) %||% "Other"
}

mod_onboarding_server <- function(id, db_pool, user) {
  moduleServer(id, function(input, output, session) {
    saved_user <- reactiveVal(NULL)
    outcome <- reactiveVal(NULL)
    message <- reactiveVal(NULL)

    output$message <- renderUI({
      current <- message()
      if (is.null(current)) {
        return(NULL)
      }
      div(class = paste("form-message", current$type), current$text)
    })

    observeEvent(input$continue, {
      req(user())
      message(NULL)
      updated <- tryCatch(
        save_user_profile(
          db_pool,
          user()$id,
          display_name = input$display_name,
          research_role = resolved_choice(input$research_role, input$research_role_other),
          research_field = resolved_choice(input$research_field, input$research_field_other),
          institution = input$institution,
          complete_onboarding_flag = TRUE
        ),
        error = function(e) NULL
      )
      if (is.null(updated)) {
        message(list(type = "error", text = "The profile could not be saved."))
        return()
      }
      saved_user(updated)
      outcome("continue")
    }, ignoreInit = TRUE)

    observeEvent(input$skip, {
      req(user())
      updated <- tryCatch(
        complete_onboarding(db_pool, user()$id),
        error = function(e) NULL
      )
      if (is.null(updated)) {
        message(list(type = "error", text = "Onboarding could not be skipped."))
        return()
      }
      saved_user(updated)
      outcome("skip")
    }, ignoreInit = TRUE)

    list(
      saved_user = reactive(saved_user()),
      outcome = reactive(outcome())
    )
  })
}
