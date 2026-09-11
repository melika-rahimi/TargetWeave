app_server <- function(input, output, session, db_pool) {
  auth <- mod_auth_server("auth", db_pool)

  current_view <- reactiveVal("projects")
  current_project_id <- reactiveVal(NULL)
  account_return_view <- reactiveVal("projects")
  tour_launch_token <- reactiveVal(0L)
  setup_tour_launch_token <- reactiveVal(0L)

  onboarding <- mod_onboarding_server("onboarding", db_pool, auth$user)

  project_list <- mod_project_list_server(
    "project_list",
    db_pool,
    auth$user
  )

  project_setup <- mod_project_setup_server(
    "project_setup",
    db_pool,
    auth$user
  )

  project_home <- mod_project_home_server(
    "project_home",
    db_pool,
    auth$user,
    reactive(current_project_id())
  )

  account <- mod_account_server("account", db_pool, auth$user)

  send_tour_stop <- function() {
    session$sendCustomMessage("twTour", tour_client_payload("stop"))
  }

  send_workspace_tour_start <- function() {
    session$sendCustomMessage(
      "twTour",
      tour_client_payload("start", mode = "tour", steps = workspace_tour_steps())
    )
  }

  send_setup_tour_start <- function() {
    session$sendCustomMessage(
      "twTour",
      tour_client_payload("start", mode = "setup", steps = project_setup_tour_steps())
    )
  }

  observeEvent(auth$user(), {
    user <- auth$user()
    if (is.null(user)) {
      current_project_id(NULL)
      current_view("projects")
      send_tour_stop()
      return()
    }
    if (isTRUE(user_needs_onboarding(user)) &&
        !current_view() %in% c("onboarding_profile", "account")) {
      current_view("onboarding_profile")
    }
  }, ignoreNULL = FALSE)

  observeEvent(onboarding$outcome(), {
    req(onboarding$outcome(), onboarding$saved_user())
    auth$replace_user(onboarding$saved_user())
    project_setup$reset()
    current_view("onboarding_project")
  }, ignoreInit = TRUE)

  observeEvent(project_list$new_project_requested(), {
    req(auth$user())
    project_setup$reset()
    current_view("new_project")
  }, ignoreInit = TRUE)

  observeEvent(project_setup$cancelled(), {
    req(auth$user())
    current_view("projects")
  }, ignoreInit = TRUE)

  observeEvent(project_setup$created_project(), {
    req(auth$user(), project_setup$created_project())

    current_project_id(project_setup$created_project())
    project_list$refresh()
    project_home$refresh()
    auth$replace_user(mark_project_setup_tour_seen(db_pool, auth$user()$id))
    current_view("project")
    tour_launch_token(tour_launch_token() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(project_list$selected_project(), {
    req(project_list$selected_project())

    current_project_id(project_list$selected_project())
    project_home$refresh()
    current_view("project")
    tour_launch_token(tour_launch_token() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(project_home$back_requested(), {
    req(auth$user())
    current_project_id(NULL)
    project_list$clear_selection()
    project_list$refresh()
    current_view("projects")
  }, ignoreInit = TRUE)

  observeEvent(project_home$archived(), {
    req(auth$user())
    current_project_id(NULL)
    project_list$clear_selection()
    project_list$refresh()
    current_view("projects")
  }, ignoreInit = TRUE)

  observeEvent(input$open_account, {
    req(auth$user())
    account_return_view(current_view())
    current_view("account")
  }, ignoreInit = TRUE)

  observeEvent(account$back_requested(), {
    req(auth$user())
    dest <- account_return_view()
    if (identical(dest, "account") || is.null(dest)) {
      dest <- if (is.null(current_project_id())) "projects" else "project"
    }
    current_view(dest)
  }, ignoreInit = TRUE)

  observeEvent(account$saved_user(), {
    req(account$saved_user())
    auth$replace_user(account$saved_user())
  }, ignoreInit = TRUE)

  observeEvent(
    list(
      current_view(),
      auth$user(),
      project_home$workspace_ready(),
      tour_launch_token(),
      setup_tour_launch_token()
    ),
    {
      view <- current_view()
      user <- auth$user()
      if (identical(view, "project")) {
        if (!isTRUE(should_start_workspace_tour(
          view,
          user,
          project_home$workspace_ready()
        ))) {
          return()
        }
        send_workspace_tour_start()
        return()
      }
      if (isTRUE(should_start_project_setup_tour(view, user))) {
        send_setup_tour_start()
        return()
      }
      send_tour_stop()
    }
  )

  observeEvent(input$tw_tour_event, {
    req(auth$user(), input$tw_tour_event)
    ev <- input$tw_tour_event
    action <- as.character(ev$action %||% "")
    kind <- as.character(ev$kind %||% ev$tour %||% "")
    if (!action %in% c("skip", "complete")) {
      return()
    }
    if (identical(kind, "setup")) {
      auth$replace_user(mark_project_setup_tour_seen(db_pool, auth$user()$id))
      return()
    }
    if (identical(kind, "tour")) {
      auth$replace_user(mark_workspace_tour_completed(db_pool, auth$user()$id))
      return()
    }
    if (identical(kind, "tip")) {
      auth$replace_user(
        mark_workspace_tip_seen(db_pool, auth$user()$id, ev$tip)
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(account$restart_requested(), {
    req(auth$user())
    auth$replace_user(clear_workspace_tour(db_pool, auth$user()$id))
    plan <- restart_workspace_tour_plan(current_project_id())
    if (!isTRUE(plan$ok)) {
      showNotification(plan$message, type = "message", duration = 8)
      return()
    }
    current_view("project")
    tour_launch_token(tour_launch_token() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(account$restart_setup_requested(), {
    req(auth$user())
    auth$replace_user(clear_project_setup_tour(db_pool, auth$user()$id))
    plan <- restart_project_setup_tour_plan()
    project_setup$reset()
    current_view(plan$view)
    setup_tour_launch_token(setup_tour_launch_token() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$logout, {
    auth$logout()
    current_project_id(NULL)
    project_list$clear_selection()
    current_view("projects")
    session$reload()
  }, ignoreInit = TRUE)

  output$app_body <- renderUI({
    user <- auth$user()

    if (is.null(user)) {
      return(mod_auth_ui("auth"))
    }

    display <- account_display_name(user)
    trigger_label <- if (!is.null(display)) display else "Account"

    div(
      class = "app-frame",
      tags$header(
        class = "topbar",
        tw_brand_lockup("Molecular target investigation", size = 28),
        div(
          class = "user-block",
          actionButton(
            "open_account",
            tagList(
              span(class = "account-name", trigger_label),
              span(class = "account-caret", "\u25BE")
            ),
            class = "account-trigger",
            title = "Account",
            `aria-label` = "Account"
          )
        )
      ),
      tags$main(
        class = "main-content",
        switch(
          current_view(),
          onboarding_profile = mod_onboarding_ui("onboarding"),
          onboarding_project = mod_project_setup_ui("project_setup", variant = "onboarding"),
          projects = mod_project_list_ui("project_list"),
          new_project = mod_project_setup_ui("project_setup", variant = "standard"),
          project = mod_project_home_ui("project_home"),
          account = mod_account_ui("account"),
          mod_project_list_ui("project_list")
        )
      )
    )
  })
}
