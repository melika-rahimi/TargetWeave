mod_account_ui <- function(id) {
  ns <- NS(id)
  roles <- research_role_choices()
  fields <- research_field_choices()

  div(
    class = "content-stack account-center",
    div(
      class = "section-heading-row",
      div(
        div(class = "eyebrow", "Account"),
        h2("Account center")
      ),
      actionButton(ns("back"), "Back to workspace", class = "btn-text")
    ),
    uiOutput(ns("notice")),

    div(
      class = "account-section form-block",
      h3("Profile"),
      uiOutput(ns("profile_view")),
      checkboxInput(ns("edit_profile"), "Edit profile", value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("edit_profile")),
        textInput(ns("display_name"), "Display name"),
        selectInput(ns("research_role"), "Research role", choices = c("", roles)),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'Other'", ns("research_role")),
          textInput(ns("research_role_other"), "Describe your role")
        ),
        selectInput(ns("research_field"), "Research field", choices = c("", fields)),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'Other'", ns("research_field")),
          textInput(ns("research_field_other"), "Describe your field")
        ),
        textInput(ns("institution"), "Institution / organization (optional)"),
        actionButton(ns("save_profile"), "Save profile", class = "btn-primary-quiet"),
        uiOutput(ns("profile_message"))
      )
    ),

    div(
      class = "account-section form-block",
      h3("Account & security"),
      uiOutput(ns("security_view")),
      h4("Change password"),
      password_field_ui(ns("current_password"), "Current password", autocomplete = "current-password"),
      password_field_ui(
        ns("new_password"),
        "New password",
        placeholder = "At least 8 characters",
        autocomplete = "new-password"
      ),
      password_field_ui(
        ns("new_password_confirm"),
        "Confirm new password",
        autocomplete = "new-password"
      ),
      actionButton(ns("change_password"), "Update password", class = "btn-primary-quiet"),
      uiOutput(ns("password_message"))
    ),

    div(
      class = "account-section form-block",
      h3("Help & support"),
      p(class = "panel-intro", "Send a request so it can be stored for later handling."),
      selectInput(ns("support_category"), "Category", choices = c("", support_request_categories())),
      textInput(ns("support_subject"), "Subject"),
      textAreaInput(ns("support_message"), "Message", rows = 5),
      actionButton(ns("support_submit"), "Send a support request", class = "btn-primary-quiet"),
      uiOutput(ns("support_message_ui")),
      h4("Recent requests"),
      uiOutput(ns("support_list"))
    ),

    div(
      class = "account-section form-block",
      h3("Product guidance"),
      p("Restart the short coach-mark tour on a project workspace. It does not change scientific results."),
      actionButton(ns("restart_tour"), "Restart product tour", class = "btn-primary-quiet"),
      p(
        class = "panel-intro",
        "Restart the project setup guide on the new-project form. It does not fill fields or create a project."
      ),
      actionButton(ns("restart_setup_tour"), "Restart project setup guide", class = "btn-primary-quiet"),
      h4("About TargetWeave"),
      p("TargetWeave connects protein, genomic, and disease evidence for molecular target investigation."),
      tags$ul(
        tags$li("UniProt — protein identity and annotation"),
        tags$li("Ensembl — gene identity and genomic context"),
        tags$li("Open Targets — target–disease evidence"),
        tags$li("Reactome — pathway membership"),
        tags$li("PubMed — literature landscape"),
        tags$li("RCSB PDB — experimental structures")
      )
    ),

    div(
      class = "account-section form-block",
      h3("Data & privacy"),
      p("Research workspaces are private to the signed-in account."),
      p("Profile fields do not change scientific retrieval or scores."),
      p("Research notes and support messages are not sent to external biomedical APIs."),
      p("Scientific identifiers and query terms are sent to the relevant public sources when you request retrieval.")
    ),

    div(
      class = "account-section form-block",
      h3("Account actions"),
      actionButton("logout", "Sign out", class = "btn-signout")
    )
  )
}

mod_account_server <- function(id, db_pool, user) {
  moduleServer(id, function(input, output, session) {
    saved_user <- reactiveVal(NULL)
    back_requested <- reactiveVal(0L)
    restart_requested <- reactiveVal(0L)
    restart_setup_requested <- reactiveVal(0L)
    notice <- reactiveVal(NULL)
    profile_message <- reactiveVal(NULL)
    password_message <- reactiveVal(NULL)
    support_message <- reactiveVal(NULL)
    support_refresh <- reactiveVal(0L)

    current <- reactive({
      saved_user() %||% user()
    })

    output$notice <- renderUI({
      current_notice <- notice()
      if (is.null(current_notice)) return(NULL)
      div(class = paste("form-message", current_notice$type), current_notice$text)
    })

    output$profile_view <- renderUI({
      u <- current()
      req(u)
      tagList(
        p(strong("Display name"), span(profile_display_value(u$display_name))),
        p(strong("Research role"), span(profile_display_value(u$research_role))),
        p(strong("Research field"), span(profile_display_value(u$research_field))),
        p(strong("Institution / organization"), span(profile_display_value(u$institution)))
      )
    })

    output$security_view <- renderUI({
      u <- current()
      req(u)
      tagList(
        p(strong("Email"), span(u$email)),
        p(strong("Password"), span("\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022"))
      )
    })

    output$profile_message <- renderUI({
      current_msg <- profile_message()
      if (is.null(current_msg)) return(NULL)
      div(class = paste("form-message", current_msg$type), current_msg$text)
    })

    output$password_message <- renderUI({
      current_msg <- password_message()
      if (is.null(current_msg)) return(NULL)
      div(class = paste("form-message", current_msg$type), current_msg$text)
    })

    output$support_message_ui <- renderUI({
      current_msg <- support_message()
      if (is.null(current_msg)) return(NULL)
      div(class = paste("form-message", current_msg$type), current_msg$text)
    })

    output$support_list <- renderUI({
      support_refresh()
      u <- current()
      req(u)
      rows <- list_owned_support_requests(db_pool, u$id)
      if (is.null(rows) || nrow(rows) == 0) {
        return(p(class = "text-muted", "No requests yet."))
      }
      tags$ul(
        class = "support-list",
        lapply(seq_len(nrow(rows)), function(i) {
          tags$li(
            strong(rows$subject[[i]]),
            span(paste(rows$category[[i]], "\u00b7", rows$status[[i]])),
            span(class = "text-muted", as.character(rows$created_at[[i]]))
          )
        })
      )
    })

    observeEvent(current(), {
      u <- current()
      if (is.null(u)) return()
      updateTextInput(session, "display_name", value = u$display_name %||% "")
      role <- u$research_role %||% ""
      field <- u$research_field %||% ""
      if (nzchar(role) && !(role %in% research_role_choices())) {
        updateSelectInput(session, "research_role", selected = "Other")
        updateTextInput(session, "research_role_other", value = role)
      } else {
        updateSelectInput(session, "research_role", selected = role)
      }
      if (nzchar(field) && !(field %in% research_field_choices())) {
        updateSelectInput(session, "research_field", selected = "Other")
        updateTextInput(session, "research_field_other", value = field)
      } else {
        updateSelectInput(session, "research_field", selected = field)
      }
      updateTextInput(session, "institution", value = u$institution %||% "")
    }, ignoreNULL = TRUE)

    observeEvent(input$save_profile, {
      req(user())
      profile_message(NULL)
      updated <- tryCatch(
        save_user_profile(
          db_pool,
          user()$id,
          display_name = input$display_name,
          research_role = resolved_choice(input$research_role, input$research_role_other),
          research_field = resolved_choice(input$research_field, input$research_field_other),
          institution = input$institution,
          complete_onboarding_flag = FALSE
        ),
        error = function(e) NULL
      )
      if (is.null(updated) || (is.list(updated) && isTRUE(identical(updated$ok, FALSE)))) {
        profile_message(list(
          type = "error",
          text = updated$message %||% "The profile could not be saved."
        ))
        return()
      }
      if (is.null(updated$id)) {
        profile_message(list(type = "error", text = "The profile could not be saved."))
        return()
      }
      saved_user(updated)
      profile_message(list(type = "info", text = "Profile updated."))
    }, ignoreInit = TRUE)

    observeEvent(input$change_password, {
      req(user())
      password_message(NULL)
      result <- change_password(
        db_pool,
        user()$id,
        input$current_password,
        input$new_password,
        input$new_password_confirm
      )
      if (!isTRUE(result$ok)) {
        password_message(list(type = "error", text = result$message))
        return()
      }
      updateTextInput(session, "current_password", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "new_password_confirm", value = "")
      password_message(list(type = "info", text = "Password updated."))
    }, ignoreInit = TRUE)

    observeEvent(input$support_submit, {
      req(user())
      support_message(NULL)
      result <- create_support_request(
        db_pool,
        user()$id,
        input$support_category,
        input$support_subject,
        input$support_message
      )
      if (!isTRUE(result$ok)) {
        support_message(list(type = "error", text = result$message))
        return()
      }
      updateTextInput(session, "support_subject", value = "")
      updateTextAreaInput(session, "support_message", value = "")
      support_refresh(support_refresh() + 1L)
      support_message(list(type = "info", text = "Your request has been submitted."))
    }, ignoreInit = TRUE)

    observeEvent(input$restart_tour, {
      restart_requested(restart_requested() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$restart_setup_tour, {
      restart_setup_requested(restart_setup_requested() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$back, {
      back_requested(back_requested() + 1L)
    }, ignoreInit = TRUE)

    list(
      saved_user = reactive(saved_user()),
      back_requested = reactive(back_requested()),
      restart_requested = reactive(restart_requested()),
      restart_setup_requested = reactive(restart_setup_requested())
    )
  })
}
