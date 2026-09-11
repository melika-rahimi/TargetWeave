mod_project_setup_ui <- function(id, variant = "standard") {
  ns <- NS(id)
  onboarding <- identical(variant, "onboarding")

  div(
    class = "content-stack",
    div(
      class = "section-heading-row",
      div(
        if (onboarding) onboarding_stepper(2L) else div(class = "eyebrow", "New project"),
        h2(if (onboarding) "Create your first investigation" else "Define the investigation"),
        p(
          if (onboarding) {
            paste(
              "Start with a research question, a disease context, and 2–8 candidate genes or proteins you want to investigate. ",
              "Nothing is pre-filled. Identifiers are assigned only after you confirm them."
            )
          } else {
            paste(
              "Create a project with a research question, disease context, ",
              "and candidate targets. Identifiers are assigned only after you confirm them."
            )
          }
        )
      ),
      actionButton(
        ns("cancel"),
        if (onboarding) "Skip for now" else "Back to projects",
        class = "btn-text"
      )
    ),
    div(
      class = "form-layout",
      div(
        class = "form-block",
        investigation_text_input(
          ns("title"),
          "Project title",
          placeholder = "e.g. Candidate targets in colorectal cancer",
          help = "A short name for this investigation.",
          data_tour = "setup-title"
        ),
        investigation_textarea_input(
          ns("question"),
          "Research question",
          placeholder = "e.g. Which candidates deserve deeper investigation?",
          rows = 5,
          help = "The biological question you want this project to explore.",
          data_tour = "setup-question"
        ),
        investigation_text_input(
          ns("disease"),
          "Disease context",
          placeholder = "e.g. colorectal cancer",
          help = "Use the disease name you normally work with. You will confirm its standardized identity later.",
          data_tour = "setup-disease"
        ),
        div(
          class = "fixed-field",
          span("Organism"),
          strong("Homo sapiens"),
          tags$small("v1 is intentionally limited to human targets.")
        )
      ),
      div(
        class = "form-block",
        investigation_textarea_input(
          ns("targets"),
          "Candidate targets",
          placeholder = "e.g. EGFR\nKRAS",
          rows = 10,
          help = "2–8 gene/protein symbols or identifiers. One per line or comma-separated.",
          data_tour = "setup-targets"
        ),
        actionButton(
          ns("create"),
          "Create project",
          class = "btn-primary-quiet"
        ),
        uiOutput(ns("message"))
      )
    )
  )
}

mod_project_setup_server <- function(id, db_pool, user) {
  moduleServer(id, function(input, output, session) {
    created_project <- reactiveVal(NULL)
    cancelled <- reactiveVal(0L)
    message <- reactiveVal(NULL)

    output$message <- renderUI({
      current <- message()
      if (is.null(current)) return(NULL)

      div(
        class = paste("form-message", current$type),
        current$text
      )
    })

    creating <- reactiveVal(FALSE)

    observeEvent(input$create, {
      req(user())
      if (isTRUE(creating()) || !is.null(created_project())) {
        return()
      }

      creating(TRUE)
      on.exit(creating(FALSE), add = TRUE)
      message(NULL)

      target_inputs <- parse_target_input(input$targets)

      result <- create_project(
        db_pool = db_pool,
        user_id = user()$id,
        title = input$title,
        research_question = input$question,
        disease_label = input$disease,
        target_inputs = target_inputs
      )

      if (!isTRUE(result$ok)) {
        message(list(type = "error", text = result$message))
        return()
      }

      created_project(result$project_id)
    }, ignoreInit = TRUE)

    observeEvent(input$cancel, {
      cancelled(cancelled() + 1L)
    })

    list(
      created_project = reactive(created_project()),
      cancelled = reactive(cancelled()),
      reset = function() {
        updateTextInput(session, "title", value = "")
        updateTextAreaInput(session, "question", value = "")
        updateTextInput(session, "disease", value = "")
        updateTextAreaInput(session, "targets", value = "")
        message(NULL)
        created_project(NULL)
        creating(FALSE)
      }
    )
  })
}
