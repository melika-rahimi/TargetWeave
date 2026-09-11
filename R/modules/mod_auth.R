mod_auth_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("public_body"))
}

mod_auth_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    current_user <- reactiveVal(NULL)
    public_view <- reactiveVal("landing")
    login_message <- reactiveVal(NULL)
    register_message <- reactiveVal(NULL)

    observeEvent(input$nav_login, {
      login_message(NULL)
      public_view("login")
    }, ignoreInit = TRUE)

    observeEvent(input$hero_login, {
      login_message(NULL)
      public_view("login")
    }, ignoreInit = TRUE)

    observeEvent(input$footer_login, {
      login_message(NULL)
      public_view("login")
    }, ignoreInit = TRUE)

    observeEvent(input$nav_register, {
      register_message(NULL)
      public_view("register")
    }, ignoreInit = TRUE)

    observeEvent(input$hero_register, {
      register_message(NULL)
      public_view("register")
    }, ignoreInit = TRUE)

    observeEvent(input$footer_register, {
      register_message(NULL)
      public_view("register")
    }, ignoreInit = TRUE)

    observeEvent(input$go_landing, {
      login_message(NULL)
      register_message(NULL)
      public_view("landing")
    }, ignoreInit = TRUE)

    output$public_body <- renderUI({
      if (!is.null(current_user())) {
        return(NULL)
      }
      public_auth_view_ui(public_view(), session$ns)
    })

    output$login_message <- renderUI({
      current <- login_message()
      if (is.null(current)) return(NULL)
      div(class = paste("form-message", current$type), current$text)
    })

    output$register_message <- renderUI({
      current <- register_message()
      if (is.null(current)) return(NULL)
      div(class = paste("form-message", current$type), current$text)
    })

    observeEvent(input$login_submit, {
      login_message(NULL)

      result <- tryCatch(
        authenticate_user(
          db_pool,
          input$login_email,
          input$login_password
        ),
        error = function(e) {
          list(
            ok = FALSE,
            message = "The database could not be reached."
          )
        }
      )

      if (!isTRUE(result$ok)) {
        login_message(list(type = "error", text = result$message))
        return()
      }

      current_user(result$user)
    }, ignoreInit = TRUE)

    observeEvent(input$register_submit, {
      register_message(NULL)

      check <- validate_registration_passwords(
        input$register_password,
        input$register_password_confirm
      )
      if (!isTRUE(check$ok)) {
        register_message(list(type = "error", text = check$message))
        return()
      }

      result <- tryCatch(
        register_user(
          db_pool,
          input$register_email,
          input$register_password
        ),
        error = function(e) {
          list(
            ok = FALSE,
            message = "The account could not be created."
          )
        }
      )

      if (!isTRUE(result$ok)) {
        register_message(list(type = "error", text = result$message))
        return()
      }

      current_user(result$user)
    }, ignoreInit = TRUE)

    list(
      user = reactive(current_user()),
      replace_user = function(user) {
        current_user(user)
      },
      logout = function() {
        current_user(NULL)
        login_message(NULL)
        register_message(NULL)
        public_view("landing")
      }
    )
  })
}
