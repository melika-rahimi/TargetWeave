current_connected_sources <- function() {
  list(
    list(
      name = "UniProt",
      summary = "Protein identity and annotation"
    ),
    list(
      name = "Ensembl",
      summary = "Gene identity and genomic context"
    ),
    list(
      name = "Open Targets",
      summary = "Target–disease association evidence"
    ),
    list(
      name = "Reactome",
      summary = "Pathway membership and overlap"
    ),
    list(
      name = "PubMed",
      summary = "Literature landscape"
    ),
    list(
      name = "RCSB PDB",
      summary = "Experimental structure availability"
    )
  )
}

landing_feature_items <- function() {
  list(
    list(title = "Resolve identities", body = "Canonical Ensembl + UniProt mapping"),
    list(title = "Explore evidence", body = "Protein, genomic and disease context"),
    list(title = "Compare targets", body = "Open Targets evidence profiles"),
    list(title = "Trace provenance", body = "Sources, versions and retrieval status")
  )
}

landing_workflow_steps <- function() {
  c(
    "Create an investigation",
    "Confirm identities",
    "Explore evidence",
    "Compare candidates"
  )
}

landing_hero_diagram_ui <- function() {
  tags$figure(
    class = "hero-diagram hero-demo",
    `aria-label` = paste(
      "TargetWeave workflow preview showing target identity confirmation, disease",
      "evidence, pathways, literature, structures, comparison, snapshot, and export."
    ),
    p(
      class = "visually-hidden",
      paste(
        "TargetWeave workflow preview showing target identity confirmation, disease",
        "evidence, pathways, literature, structures, comparison, snapshot, and export."
      )
    ),
    landing_product_demo_ui(),
    tags$figcaption(
      class = "demo-captions",
      tags$p(class = "demo-cap is-on", `data-cap` = "enter", "Enter a molecular target"),
      tags$p(class = "demo-cap", `data-cap` = "identity", "Confirm molecular identity"),
      tags$p(class = "demo-cap", `data-cap` = "disease", "Place the investigation in a disease context"),
      tags$p(class = "demo-cap", `data-cap` = "evidence", "Review connected evidence in the workspace"),
      tags$p(class = "demo-cap", `data-cap` = "compare", "Compare evidence across candidates"),
      tags$p(class = "demo-cap", `data-cap` = "preserve", "Preserve the investigation")
    )
  )
}

landing_product_demo_ui <- function() {
  demo_tab <- function(view, label) {
    tags$span(
      class = paste("tw-demo-tab", if (identical(view, "overview")) "is-on" else NULL),
      `data-demo-tab` = view,
      label
    )
  }

  div(
    class = "tw-demo",
    `aria-hidden` = "true",
    `data-step` = "enter",
    `data-view` = "overview",
    div(
      class = "tw-demo-chrome",
      div(
        class = "tw-demo-topbar",
        span(class = "tw-demo-mark", "TW"),
        span("Investigation"),
        span(class = "tw-demo-project", "NSCLC candidates")
      ),
      div(
        class = "tw-demo-nav",
        demo_tab("overview", "Overview"),
        demo_tab("evidence", "Disease evidence"),
        demo_tab("pathways", "Pathways"),
        demo_tab("literature", "Literature"),
        demo_tab("structures", "Structures")
      )
    ),
    div(
      class = "tw-demo-stage",
      div(
        class = "tw-demo-panel",
        `data-panel` = "enter",
        p(class = "tw-demo-label", "Target"),
        div(
          class = "tw-demo-field",
          span(class = "tw-demo-typed"),
          span(class = "tw-demo-caret")
        ),
        span(class = "tw-demo-btn tw-demo-resolve", "Resolve target"),
        div(class = "tw-demo-skel", span(), span())
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "identity",
        div(
          class = "tw-demo-card",
          div(
            class = "tw-demo-card-head",
            strong("EGFR"),
            span(class = "status-pill status-pill-success", "Confirmed")
          ),
          p("UniProt: ", tags$code(class = "identifier", "P00533")),
          p("Ensembl: ", tags$code(class = "identifier", "ENSG00000146648"))
        )
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "disease",
        p(class = "tw-demo-label", "Disease context"),
        div(
          class = "tw-demo-card tw-demo-disease",
          strong("Non-small-cell lung cancer")
        )
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "evidence",
        `data-view-panel` = "evidence",
        p(class = "tw-demo-label", "Disease evidence"),
        p(class = "tw-demo-note", "Open Targets association profile"),
        div(
          class = "tw-demo-bars",
          div(class = "tw-demo-bar", span("Genetic"), span(class = "tw-demo-bar-track", span(class = "is-w72"))),
          div(class = "tw-demo-bar", span("Known drug"), span(class = "tw-demo-bar-track", span(class = "is-w54"))),
          div(class = "tw-demo-bar", span("Literature"), span(class = "tw-demo-bar-track", span(class = "is-w41")))
        )
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "evidence",
        `data-view-panel` = "pathways",
        p(class = "tw-demo-label", "Pathways"),
        p(class = "tw-demo-note", "Reactome membership"),
        tags$table(
          class = "tw-demo-matrix",
          tags$thead(tags$tr(tags$th(""), tags$th("EGFR"), tags$th("KRAS"))),
          tags$tbody(
            tags$tr(tags$td("Signalling by EGFR"), tags$td(class = "is-in"), tags$td("")),
            tags$tr(tags$td("MAPK signalling"), tags$td(class = "is-in"), tags$td(class = "is-in")),
            tags$tr(tags$td("PI3K/AKT"), tags$td(class = "is-in"), tags$td(""))
          )
        )
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "evidence",
        `data-view-panel` = "literature",
        p(class = "tw-demo-label", "Literature"),
        p(class = "tw-demo-note", "PubMed publication trend"),
        htmltools::HTML(paste0(
          '<svg class="tw-demo-spark" viewBox="0 0 220 56" focusable="false">',
          '<polyline fill="none" stroke="#18243D" stroke-width="2" points="4,42 28,38 52,40 76,30 100,33 124,22 148,24 172,16 196,18 216,12"/>',
          '</svg>'
        ))
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "evidence",
        `data-view-panel` = "structures",
        p(class = "tw-demo-label", "Structures"),
        div(
          class = "tw-demo-card",
          strong("Experimental PDB"),
          p("Availability for confirmed UniProt accession"),
          div(class = "tw-demo-coverage", span(class = "is-w62"))
        )
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "compare",
        p(class = "tw-demo-label", "Compare candidates"),
        div(
          class = "tw-demo-chips",
          span(class = "tw-demo-chip is-on", "EGFR"),
          span(class = "tw-demo-chip is-on", "KRAS"),
          span(class = "tw-demo-chip is-on", "MET")
        ),
        tags$table(
          class = "tw-demo-matrix tw-demo-compare",
          tags$thead(tags$tr(tags$th(""), tags$th("EGFR"), tags$th("KRAS"), tags$th("MET"))),
          tags$tbody(
            tags$tr(tags$td("Genetic"), tags$td(class = "is-in"), tags$td(class = "is-in"), tags$td("")),
            tags$tr(tags$td("Known drug"), tags$td(class = "is-in"), tags$td(""), tags$td(class = "is-in")),
            tags$tr(tags$td("Pathways"), tags$td(class = "is-in"), tags$td(class = "is-in"), tags$td(class = "is-in"))
          )
        )
      ),
      div(
        class = "tw-demo-panel",
        `data-panel` = "preserve",
        p(class = "tw-demo-label", "Save evidence snapshot"),
        span(class = "status-pill status-pill-info tw-demo-saved", "Snapshot saved"),
        p(class = "tw-demo-label tw-demo-export-label", "Export research dossier"),
        div(
          class = "tw-demo-export",
          span(class = "tw-demo-btn is-primary", "HTML"),
          span(class = "tw-demo-btn", "ZIP")
        )
      )
    )
  )
}

landing_header_ui <- function(ns, show_product_nav = TRUE) {
  tags$header(
    class = "public-header",
    tags$a(
      href = "#top",
      class = "public-brand",
      tw_logo_svg(size = 28, decorative = TRUE),
      span("TargetWeave")
    ),
    if (isTRUE(show_product_nav)) {
      tags$nav(
        class = "public-nav",
        `aria-label` = "Product",
        tags$a(href = "#how-it-works", "How it works"),
        tags$a(href = "#sources", "Sources")
      )
    },
    div(
      class = "public-header-actions",
      actionButton(ns("nav_login"), "Sign in", class = "btn-ghost"),
      actionButton(ns("nav_register"), "Start an investigation", class = "btn-primary-quiet")
    )
  )
}

landing_features_section_ui <- function() {
  items <- landing_feature_items()
  tags$section(
    class = "landing-section",
    h2("What you can do"),
    div(
      class = "feature-rows",
      lapply(items, function(item) {
        div(
          class = "feature-row",
          strong(item$title),
          span(item$body)
        )
      })
    )
  )
}

landing_workflow_section_ui <- function() {
  steps <- landing_workflow_steps()
  tags$section(
    id = "how-it-works",
    class = "landing-section",
    h2("How it works"),
    div(
      class = "workflow-flow",
      lapply(seq_along(steps), function(i) {
        div(
          class = "workflow-item",
          span(class = "workflow-n", sprintf("%02d", i)),
          strong(steps[[i]])
        )
      })
    )
  )
}

landing_sources_section_ui <- function() {
  sources <- current_connected_sources()
  tags$section(
    id = "sources",
    class = "landing-section",
    h2("Connected sources"),
    div(
      class = "source-rows",
      lapply(sources, function(src) {
        div(
          class = "source-row",
          strong(src$name),
          span(src$summary)
        )
      })
    )
  )
}

landing_page_ui <- function(ns = shiny::NS("auth")) {
  div(
    id = "top",
    class = "landing-page",
    landing_header_ui(ns),
    tags$section(
      class = "landing-hero",
      div(
        class = "hero-copy",
        h1("Connected evidence for molecular target investigation."),
        p(
          "Investigate molecular targets in a disease context by connecting protein, ",
          "genomic, and disease evidence while preserving scientific provenance."
        ),
        div(
          class = "hero-actions",
          actionButton(ns("hero_register"), "Start an investigation", class = "btn-primary-quiet"),
          tags$a(href = "#how-it-works", class = "btn-text", "See how it works")
        ),
        p(
          class = "hero-signin",
          "Already have an account? ",
          actionButton(ns("hero_login"), "Sign in", class = "btn-inline")
        )
      ),
      landing_hero_diagram_ui()
    ),
    landing_features_section_ui(),
    landing_workflow_section_ui(),
    landing_sources_section_ui(),
    tags$section(
      class = "landing-section landing-final-cta",
      h2("Ready to investigate your targets?"),
      div(
        class = "hero-actions",
        actionButton(ns("footer_register"), "Start an investigation", class = "btn-primary-quiet"),
        actionButton(ns("footer_login"), "Sign in", class = "btn-ghost")
      )
    ),
    tags$footer(
      class = "landing-footer",
      p("Public biomedical resources. Not a partnership or endorsement.")
    )
  )
}

public_auth_view_ui <- function(view, ns = shiny::NS("auth")) {
  switch(
    as.character(view),
    login = auth_sign_in_ui(ns),
    register = auth_register_ui(ns),
    landing_page_ui(ns)
  )
}

auth_back_row <- function(ns) {
  div(
    class = "auth-back",
    actionButton(ns("go_landing"), "\u2190 Back to TargetWeave", class = "btn-text")
  )
}

auth_credential_form <- function(ns, submit_id, submit_label, ..., message_ui = NULL) {
  div(
    class = "auth-submit-form",
    `data-tw-submit` = ns(submit_id),
    ...,
    actionButton(ns(submit_id), submit_label, class = "btn-primary-block"),
    message_ui
  )
}

auth_sign_in_ui <- function(ns = shiny::NS("auth")) {
  div(
    class = "auth-screen",
    landing_header_ui(ns, show_product_nav = FALSE),
    div(
      class = "auth-screen-body",
      auth_back_row(ns),
      div(
        class = "auth-panel",
        h1("Sign in"),
        p(class = "panel-intro", "Accounts are private. Projects are never shared."),
        auth_credential_form(
          ns,
          "login_submit",
          "Sign in",
          textInput(ns("login_email"), "Email"),
          password_field_ui(
            ns("login_password"),
            "Password",
            autocomplete = "current-password"
          ),
          message_ui = uiOutput(ns("login_message"))
        )
      )
    )
  )
}

auth_register_ui <- function(ns = shiny::NS("auth")) {
  div(
    class = "auth-screen",
    landing_header_ui(ns, show_product_nav = FALSE),
    div(
      class = "auth-screen-body",
      auth_back_row(ns),
      div(
        class = "auth-panel",
        h1("Create account", id = "create-account"),
        p(class = "panel-intro", "Start an investigation with a private workspace."),
        auth_credential_form(
          ns,
          "register_submit",
          "Create account",
          textInput(ns("register_email"), "Email"),
          password_field_ui(
            ns("register_password"),
            "Password",
            placeholder = "At least 8 characters",
            autocomplete = "new-password"
          ),
          password_field_ui(
            ns("register_password_confirm"),
            "Confirm password",
            placeholder = "Re-enter password",
            autocomplete = "new-password"
          ),
          message_ui = uiOutput(ns("register_message"))
        )
      )
    )
  )
}
