mod_target_resolver_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "resolver-shell",
    uiOutput(ns("body"))
  )
}

mod_target_resolver_server <- function(id, db_pool, user, target_id) {
  moduleServer(id, function(input, output, session) {
    inflight <- reactiveVal(list())
    lookup_error <- reactiveVal(NULL)
    payload <- reactiveVal(NULL)
    confirmed <- reactiveVal(0L)
    local_refresh <- reactiveVal(0L)

    target_row <- reactive({
      local_refresh()
      req(user(), target_id())

      get_owned_target(
        db_pool,
        target_id(),
        user()$id
      )
    })

    observeEvent(target_id(), {
      lookup_error(NULL)
      row <- target_row()
      payload(decode_resolution_payload(row$resolution_payload))
    }, ignoreNULL = TRUE)

    run_lookup <- function(row, token, user_id) {
      tid <- as.character(row$id)
      if (is.null(row)) {
        inflight_clear(inflight, tid, token)
        lookup_error("This target was not found or you do not have access to it.")
        return()
      }

      bind_external_task(
        resolve_target_identity(row$input_text, db_pool = db_pool, perform = http_async_perform),
        session,
        function(result) {
          if (!inflight_matches(inflight, tid, token)) {
            return()
          }
          inflight_clear(inflight, tid, token)
          if (is_task_error(result)) {
            lookup_error(result$message)
            return()
          }

          saved <- save_resolution_lookup(
            db_pool = db_pool,
            target_id = row$id,
            user_id = user_id,
            status = result$status,
            payload = result
          )

          if (!isTRUE(saved)) {
            lookup_error("Lookup finished, but the result could not be saved.")
            return()
          }

          apply <- identity_task_apply(target_id(), tid)
          if (isTRUE(apply$update_visible_panel)) {
            payload(result)
            local_refresh(isolate(local_refresh()) + 1L)
          }
          confirmed(isolate(confirmed()) + 1L)
        },
        error_message = "Identity lookup could not be completed.",
        inflight = inflight,
        key = tid,
        token = token
      )
    }

    start_lookup <- function() {
      row <- isolate(target_row())
      if (is.null(row)) {
        lookup_error("This target was not found or you do not have access to it.")
        return()
      }
      tid <- as.character(row$id)
      if (inflight_has(inflight, tid)) {
        return()
      }
      inflight_start(inflight, tid)
      lookup_error(NULL)
      token <- inflight_token(inflight, tid)
      user_id <- isolate(user())$id
      schedule_after_flush(session, function() run_lookup(row, token, user_id))
    }

    observeEvent(input$lookup, {
      req(user(), target_id())
      start_lookup()
    })

    observeEvent(input$retry_lookup, {
      req(user(), target_id())
      start_lookup()
    })

    observeEvent(input$save_input, {
      req(user(), target_id())

      result <- update_target_input_text(
        db_pool,
        target_id(),
        user()$id,
        input$edit_input
      )

      if (!isTRUE(result$ok)) {
        lookup_error(result$message)
        return()
      }

      payload(NULL)
      lookup_error(NULL)
      local_refresh(local_refresh() + 1L)
      confirmed(confirmed() + 1L)
    })

    observeEvent(input$confirm_candidate, {
      req(user(), target_id(), input$confirm_candidate)

      current <- payload()
      if (is.null(current) || length(current$candidates) == 0) {
        lookup_error("No candidate is available to confirm.")
        return()
      }

      selected <- NULL
      for (candidate in current$candidates) {
        if (identical(candidate$candidate_key, input$confirm_candidate)) {
          selected <- candidate
          break
        }
      }

      if (is.null(selected) || !candidate_is_confirmable(selected)) {
        lookup_error("This candidate is missing an Ensembl gene ID or UniProt accession.")
        return()
      }

      result <- confirm_project_target(
        db_pool = db_pool,
        target_id = target_id(),
        user_id = user()$id,
        display_symbol = selected$display_symbol,
        ensembl_gene_id = selected$ensembl_gene_id,
        uniprot_accession = selected$uniprot_accession,
        hgnc_id = selected$hgnc_id
      )

      if (!isTRUE(result$ok)) {
        lookup_error(result$message)
        return()
      }

      local_refresh(local_refresh() + 1L)
      confirmed(confirmed() + 1L)
    })

    output$body <- renderUI({
      ns <- session$ns

      if (is.null(target_id())) {
        return(
          div(
            class = "resolver-status",
            "Select a target to resolve its identity or view its overview."
          )
        )
      }

      row <- target_row()

      if (is.null(row)) {
        return(
          div(
            class = "form-message error",
            "Select a target you own to resolve its identity."
          )
        )
      }

      if (identical(row$resolution_status, "confirmed")) {
        return(confirmed_identity_ui(row))
      }

      current <- payload()
      busy <- inflight_has(inflight, as.character(row$id))

      tagList(
        resolver_header_ui(row, ns, busy),
        if (!is.null(lookup_error())) {
          div(class = "form-message error", lookup_error())
        },
        if (busy) {
          div(class = "resolver-status", panel_state_ui("retrieving", "Retrieving identity\u2026"))
        },
        if (!is.null(current)) {
          resolver_results_ui(current, ns)
        } else if (!busy) {
          div(
            class = "resolver-status",
            "No lookup yet. Confirm identity before retrieving annotations."
          )
        }
      )
    })

    list(
      changed = reactive(confirmed()),
      retrieving_ids = reactive(inflight_keys(inflight))
    )
  })
}

resolver_header_ui <- function(row, ns, busy) {
  div(
    class = "resolver-header",
    div(
      class = "eyebrow",
      "Target resolver"
    ),
    h3(row$input_text),
    p(
      class = "panel-intro",
      "UniProt and Ensembl are queried only after you request a lookup. ",
      "Nothing is stored as a gene or protein until you confirm a candidate."
    ),
    if (identical(row$resolution_status, "failed")) {
      tagList(
        textInput(
          ns("edit_input"),
          "Edit target string",
          value = row$input_text
        ),
        actionButton(
          ns("save_input"),
          "Save string",
          class = "btn-text"
        )
      )
    },
    actionButton(
      ns("lookup"),
      if (busy) "Retrieving\u2026" else "Resolve target",
      class = "btn-primary-quiet"
    )
  )
}

confirmed_identity_ui <- function(row) {
  div(
    class = "resolver-header",
    div(class = "eyebrow", "Confirmed identity"),
    h3(row$display_symbol),
    p(class = "panel-intro", paste("Original input:", row$input_text)),
    div(
      class = "candidate-meta",
      span(row$ensembl_gene_id),
      span(row$uniprot_accession),
      if (has_display_text(row$hgnc_id)) span(row$hgnc_id)
    ),
    p(
      class = "field-help",
      sprintf(
        "Confirmed at %s.",
        {
          stamped <- format_user_timestamp(row$confirmed_at)
          if (has_display_text(stamped)) stamped else "time not provided"
        }
      )
    )
  )
}

resolver_results_ui <- function(current, ns) {
  reports <- current$source_reports
  candidates <- current$candidates %||% list()
  groups <- split_candidates_by_match_quality(candidates)

  tagList(
    div(
      class = "source-report-row",
      source_badge_ui("UniProt", reports$uniprot),
      source_badge_ui("Ensembl", reports$ensembl)
    ),
    if (identical(current$status, "failed")) {
      div(
        class = "form-message error",
        "No human gene/protein candidate was found. ",
        "You can edit the original string and retry."
      )
    },
    if (identical(current$status, "ambiguous")) {
      div(
        class = "form-message",
        "Several candidates have comparable match quality, so the intended ",
        "identity cannot be prioritized. Choose the intended identity. ",
        "Nothing is confirmed automatically."
      )
    },
    if (isTRUE(groups$unique_primary)) {
      div(
        class = "form-message",
        "A primary match is shown first. Other source-valid matches are listed ",
        "below and are not equally ranked. Confirm the intended identity; ",
        "nothing is selected automatically."
      )
    },
    if (length(candidates) == 0) {
      NULL
    } else if (isTRUE(groups$unique_primary)) {
      tagList(
        div(
          class = "candidate-group",
          div(class = "eyebrow candidate-group-label", "Primary match"),
          candidate_card_ui(groups$primary[[1]], ns, prominence = "primary")
        ),
        div(
          class = "candidate-group",
          div(
            class = "eyebrow candidate-group-label",
            if (length(groups$other) == 1) {
              "Other possible match"
            } else {
              "Other possible matches"
            }
          ),
          lapply(groups$other, function(candidate) {
            candidate_card_ui(candidate, ns, prominence = "secondary")
          })
        )
      )
    } else {
      div(
        class = "candidate-list",
        lapply(candidates, function(candidate) {
          candidate_card_ui(candidate, ns)
        })
      )
    }
  )
}

source_badge_ui <- function(label, report) {
  if (is.null(report)) {
    return(span(class = "source-badge source-missing", paste(label, "not queried")))
  }

  state <- if (!isTRUE(report$ok) && !isTRUE(report$from_cache)) {
    "error"
  } else if (identical(report$cache_status, "stale")) {
    "stale"
  } else if (isTRUE(report$from_cache)) {
    "cache"
  } else {
    "live"
  }

  cache_label <- if (isTRUE(report$from_cache)) {
    if (identical(report$cache_status, "stale")) "cached, stale" else "cached"
  } else {
    "live"
  }

  retrieved_raw <- report$retrieved_at %||% ""
  retrieved <- if (has_display_text(retrieved_raw)) {
    formatted <- format_user_timestamp(retrieved_raw)
    if (has_display_text(formatted)) formatted else retrieved_raw
  } else {
    ""
  }
  error_text <- if (has_display_text(report$error)) {
    paste(" —", report$error)
  } else {
    ""
  }

  span(
    class = paste("source-badge", paste0("source-", state)),
    sprintf("%s · %s · %s%s", label, cache_label, retrieved, error_text)
  )
}

candidate_meta_value <- function(x, missing_label) {
  if (has_display_text(x)) {
    as.character(x)
  } else {
    missing_label
  }
}

decode_resolution_payload <- function(text) {
  if (!has_display_text(text)) {
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.list(parsed) || is.null(parsed$status)) {
    return(NULL)
  }

  if (!is.null(parsed$candidates) && !is.list(parsed$candidates)) {
    return(NULL)
  }

  parsed
}

match_type_label <- function(match_type) {
  switch(
    match_type %||% "",
    exact_current_symbol = "Exact current gene symbol match",
    synonym_or_alias = "Matched through synonym / alias",
    identifier_exact = "Exact identifier match",
    other_valid_match = "Other valid search match",
    "Match quality not provided"
  )
}

split_candidates_by_match_quality <- function(candidates) {
  if (length(candidates) == 0) {
    return(list(
      primary = list(),
      other = list(),
      unique_primary = FALSE
    ))
  }

  ranks <- vapply(candidates, candidate_match_rank, integer(1))
  best <- min(ranks)
  primary <- candidates[ranks == best]
  other <- candidates[ranks > best]

  list(
    primary = unname(primary),
    other = unname(other),
    unique_primary = length(candidates) > 1L &&
      length(primary) == 1L &&
      length(other) > 0L
  )
}

candidate_card_ui <- function(candidate, ns, prominence = NULL) {
  confirmable <- candidate_is_confirmable(candidate)
  cross_check <- candidate$cross_check %||% ""
  match_type <- candidate$match_type %||% ""
  card_class <- "candidate-card"
  if (identical(prominence, "primary")) {
    card_class <- paste(card_class, "candidate-card-primary")
  } else if (identical(prominence, "secondary")) {
    card_class <- paste(card_class, "candidate-card-secondary")
  }

  div(
    class = card_class,
    div(
      class = "candidate-title",
      strong(candidate_meta_value(candidate$display_symbol, candidate$input_text %||% "Unknown symbol")),
      span(
        class = paste("resolution-badge", cross_check),
        switch(
          cross_check,
          matched = "Cross-checked by UniProt + Ensembl",
          mismatch = "Identifier mismatch",
          partial_uniprot = "UniProt only",
          partial_ensembl = "Ensembl only",
          partial = "Partial sources",
          if (has_display_text(cross_check)) cross_check else "Cross-check not provided"
        )
      )
    ),
    p(candidate_meta_value(candidate$name, "Name not provided")),
    p(
      class = "match-reason",
      if (identical(match_type, "synonym_or_alias") && has_display_text(candidate$matched_alias)) {
        tagList(
          span("Matched through synonym / alias:"),
          span(class = "match-alias", candidate$matched_alias)
        )
      } else if (has_display_text(candidate$match_reason)) {
        candidate$match_reason
      } else {
        match_type_label(match_type)
      }
    ),
    div(
      class = "candidate-meta",
      span(candidate_meta_value(candidate$organism, "Organism not provided")),
      span(candidate_meta_value(candidate$biotype, "Biotype not provided")),
      span(candidate_meta_value(candidate$location_label, "Location not provided")),
      span(candidate_meta_value(candidate$ensembl_gene_id, "Ensembl ID not provided")),
      span(candidate_meta_value(candidate$uniprot_accession, "UniProt accession not provided")),
      span(reviewed_status_label(candidate$uniprot_reviewed)),
      span(candidate_meta_value(candidate$hgnc_id, "HGNC ID not provided"))
    ),
    if (has_display_text(candidate$warning)) {
      p(class = "candidate-warning", candidate$warning)
    },
    if (confirmable) {
      tags$button(
        type = "button",
        class = "btn-primary-quiet",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          ns("confirm_candidate"),
          candidate$candidate_key
        ),
        "Confirm"
      )
    } else {
      p(
        class = "field-help",
        "Cannot confirm until both an Ensembl gene ID and a UniProt accession are present."
      )
    }
  )
}
