mod_literature_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "comparison-shell literature-shell",
    `data-tw-literature-runtime` = LITERATURE_RUNTIME_MARKER,
    uiOutput(ns("body"))
  )
}

mod_literature_server <- function(
  id,
  db_pool,
  user,
  project_id,
  identity_revision = reactive(0L),
  disease_revision = reactive(0L),
  panel_active = reactive(TRUE),
  retrieve = retrieve_project_literature
) {
  moduleServer(id, function(input, output, session) {
    literature_result <- reactiveVal(NULL)
    inflight <- reactiveVal(list())
    progress_text <- reactiveVal(NULL)
    last_fetch_signature <- reactiveVal(NA_character_)
    selected_target_id <- reactiveVal(NULL)
    selected_year <- reactiveVal(literature_all_years_value())
    year_packs <- reactiveVal(list())
    year_error <- reactiveVal(NULL)
    year_progress <- reactiveVal(NULL)

    reset_year_state <- function() {
      selected_year(literature_all_years_value())
      year_packs(list())
      year_error(NULL)
      year_progress(NULL)
    }

    project_row <- reactive({
      disease_revision()
      req(user(), project_id())
      get_owned_project(db_pool, project_id(), user()$id)
    })

    target_rows <- reactive({
      identity_revision()
      req(user(), project_id())
      list_project_targets(db_pool, project_id(), user()$id)
    })

    fetch_literature <- function(project, targets, token) {
      bind_external_task(
        invoke_retrieve(
          retrieve,
          project,
          targets,
          db_pool = db_pool,
          on_progress = function(i, n, symbol) {
      progress_text(sprintf("Retrieving PubMed records\u2026 %s of %s", i, n))
          }
        ),
        session,
        function(result) {
          if (!inflight_matches(inflight, "literature", token)) {
            return()
          }
          inflight_clear(inflight, "literature", token)
          progress_text(NULL)
          if (is_task_error(result)) {
            literature_result(list(status = "error", message = result$message, literature = NULL))
            return()
          }
          literature_result(result)
          last_fetch_signature(literature_signature(project, targets))
          counts <- result$literature$target_counts
          if (!is.null(counts) && nrow(counts) > 0) {
            current <- selected_target_id()
            if (!has_display_text(current) || !(current %in% counts$project_target_id)) {
              selected_target_id(as.character(counts$project_target_id[[1]]))
            }
          } else {
            selected_target_id(NULL)
          }
        },
        error_message = "Literature could not be retrieved.",
        inflight = inflight,
        key = "literature",
        token = token
      )
    }

    start_literature <- function(force = FALSE) {
      if (inflight_has(inflight, "literature")) {
        return()
      }
      project <- isolate(project_row())
      targets <- isolate(target_rows())
      if (is.null(project) || is.null(targets)) {
        literature_result(NULL)
        last_fetch_signature(NA_character_)
        selected_target_id(NULL)
        reset_year_state()
        return()
      }

      gate <- literature_gate(project, targets)
      if (!isTRUE(gate$ok)) {
        literature_result(list(status = gate$status, message = gate$message, literature = NULL))
        last_fetch_signature(NA_character_)
        selected_target_id(NULL)
        reset_year_state()
        return()
      }

      if (suppress_auto_retrieve_while_stale(isolate(literature_result()), "literature", force = force)) {
        return()
      }
      if (!should_retrieve_literature(TRUE, project, targets, isolate(last_fetch_signature()), force = force)) {
        return()
      }

      refreshed <- forced_literature_refresh_session(
        force,
        isolate(literature_result()),
        isolate(last_fetch_signature())
      )
      if (isTRUE(refreshed$drop_session_result)) {
        literature_result(NULL)
        last_fetch_signature(NA_character_)
        reset_year_state()
      }

      inflight_start(inflight, "literature")
      progress_text("Retrieving PubMed records\u2026")
      token <- inflight_token(inflight, "literature")
      schedule_after_flush(session, function() fetch_literature(project, targets, token))
    }

    observeEvent(
      list(project_id(), disease_revision()),
      {
        if (is.null(project_id())) {
          literature_result(NULL)
          last_fetch_signature(NA_character_)
          selected_target_id(NULL)
          reset_year_state()
          return()
        }
        literature_result(NULL)
        last_fetch_signature(NA_character_)
        selected_target_id(NULL)
        reset_year_state()
        if (isTRUE(isolate(panel_active()))) {
          start_literature()
        }
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      list(project_id(), identity_revision()),
      {
        if (is.null(project_id())) {
          literature_result(NULL)
          last_fetch_signature(NA_character_)
          selected_target_id(NULL)
          reset_year_state()
          return()
        }
        previous <- last_fetch_signature()
        next_sig <- literature_signature(project_row(), target_rows())
        held <- hold_stale_multi_target_result(
          literature_result(),
          previous,
          next_sig,
          "literature"
        )
        if (isTRUE(held$changed)) {
          literature_result(held$current)
          if (is.null(held$current)) {
            last_fetch_signature(NA_character_)
            selected_target_id(NULL)
            reset_year_state()
          }
        }
        if (!isTRUE(isolate(panel_active()))) {
          return()
        }
        if (isTRUE(held$suppress_auto_retrieve)) {
          return()
        }
        start_literature()
      },
      ignoreNULL = TRUE
    )

    observeEvent(panel_active(), {
      if (isTRUE(panel_active())) {
        start_literature()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$refresh_literature, {
      start_literature(force = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$literature_target, {
      current <- literature_result()
      counts <- current$literature$target_counts
      if (is.null(counts) || nrow(counts) == 0) {
        return()
      }
      allowed <- as.character(counts$project_target_id)
      chosen <- as.character(input$literature_target %||% "")
      if (chosen %in% allowed) {
        selected_target_id(chosen)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$literature_year, {
      item <- selected_item()
      selected_year(normalize_literature_year(input$literature_year, literature_annual_trend_for_ui(item)))
      year_error(NULL)
    }, ignoreInit = TRUE)

    selected_item <- reactive({
      current <- literature_result()
      items <- current$literature$targets
      if (is.null(items) || length(items) == 0) {
        return(NULL)
      }
      sid <- selected_target_id()
      for (item in items) {
        if (identical(item$target$project_target_id, sid)) {
          return(item)
        }
      }
      items[[1]]
    })

    current_year_key <- function(item, year) {
      if (is.null(item) || is_literature_all_years(year)) {
        return(NULL)
      }
      cache_key_pubmed_year_records(
        item$target$ncbi_gene_id,
        literature_query_signature(item$disease$search_terms),
        year
      )
    }

    start_year_records <- function(force = FALSE, already_have = 0L) {
      item <- isolate(selected_item())
      year <- isolate(selected_year())
      if (is.null(item) || is_literature_all_years(year)) {
        year_error(NULL)
        year_progress(NULL)
        return()
      }
      key <- current_year_key(item, year)
      store <- isolate(year_packs())
      existing <- if (is.null(key)) NULL else store[[key]]
      already_have <- max(0L, as.integer(already_have)[[1]])
      if (!isTRUE(force) && already_have == 0L && !is.null(existing) &&
          existing$status %in% c("ok", "empty")) {
        year_error(NULL)
        year_progress(NULL)
        return()
      }
      inflight_entry <- isolate(inflight()[["literature_year"]])
      if (!is.null(inflight_entry) &&
          identical(inflight_entry$key, key) &&
          identical(as.integer(inflight_entry$already_have), already_have) &&
          !isTRUE(force)) {
        return()
      }
      inflight_start(inflight, "literature_year", extra = list(key = key, already_have = already_have))
      year_progress(sprintf("Loading publications from %s\u2026", year))
      year_error(NULL)
      token <- inflight_token(inflight, "literature_year")
      disease <- list(
        disease_query = item$corpus$disease_query,
        search_terms = item$disease$search_terms
      )
      schedule_after_flush(session, function() {
        bind_external_task(
          retrieve_literature_year_records(
            item$target$ncbi_gene_id,
            disease,
            year,
            db_pool = db_pool,
            already_have = already_have
          ),
          session,
          function(result) {
            if (!inflight_matches(inflight, "literature_year", token)) {
              return()
            }
            inflight_clear(inflight, "literature_year", token)
            year_progress(NULL)
            if (is_task_error(result) || !isTRUE(result$ok)) {
              year_error(
                result$message %||% result$error %||%
                  sprintf("Publications from %s could not be retrieved.", year)
              )
              return()
            }
            year_error(NULL)
            pack <- result$literature
            if (already_have > 0L && !is.null(existing) && nrow(existing$records) > 0) {
              combined <- rbind(existing$records, pack$records)
              combined <- combined[!duplicated(combined$pmid), , drop = FALSE]
              pack$records <- combined
              pack$shown <- nrow(combined)
              pack$truncated <- pack$shown < pack$total_count
            }
            cur <- year_packs()
            cur[[key]] <- pack
            year_packs(cur)
          },
          error_message = sprintf("Publications from %s could not be retrieved.", year),
          inflight = inflight,
          key = "literature_year",
          token = token
        )
      })
    }

    observeEvent(
      list(selected_target_id(), selected_year()),
      {
        item <- selected_item()
        year <- normalize_literature_year(selected_year(), literature_annual_trend_for_ui(item))
        if (!identical(year, selected_year())) {
          selected_year(year)
          return()
        }
        start_year_records()
      },
      ignoreInit = TRUE
    )

    observeEvent(input$retry_year_records, {
      start_year_records(force = TRUE, already_have = 0L)
    }, ignoreInit = TRUE)

    observeEvent(input$load_more_year, {
      item <- selected_item()
      year <- selected_year()
      key <- current_year_key(item, year)
      pack <- if (is.null(key)) NULL else year_packs()[[key]]
      already <- if (is.null(pack)) 0L else nrow(pack$records)
      start_year_records(already_have = already)
    }, ignoreInit = TRUE)

    output$body <- renderUI({
      ns <- session$ns
      if (inflight_has(inflight, "literature")) {
        return(div(
          class = "panel-status evidence-loading",
          panel_state_ui("retrieving", progress_text() %||% "Retrieving PubMed records\u2026")
        ))
      }
      current <- literature_result()
      if (is.null(current)) {
        return(panel_state_ui(
          "empty",
          "Not retrieved",
          "Open Literature to retrieve the PubMed landscape for confirmed targets."
        ))
      }
      if (!identical(current$status, "ready")) {
        return(panel_state_ui(
          if (grepl("unavailable|could not|fail", current$message %||% "", ignore.case = TRUE)) "unavailable" else "blocked",
          current$message %||% "Literature is not available."
        ))
      }
      literature_result_ui(
        ns,
        current,
        selected_item(),
        selected_target_id(),
        selected_year = selected_year(),
        year_pack = {
          item <- selected_item()
          year <- selected_year()
          key <- current_year_key(item, year)
          if (is.null(key)) NULL else year_packs()[[key]]
        },
        year_loading = inflight_has(inflight, "literature_year") && !is_literature_all_years(selected_year()),
        year_progress = year_progress(),
        year_error = year_error()
      )
    })

    keep_tab_outputs_visible(output, "body")

    list(current = reactive(literature_result()))
  })
}

literature_ok_trend_rows <- function(trend) {
  if (is.null(trend) || nrow(trend) == 0) {
    return(trend)
  }
  trend[
    trend$status == "ok" & !is.na(trend$record_count),
    ,
    drop = FALSE
  ]
}

literature_current_year_summary <- function(trend) {
  if (is.null(trend) || nrow(trend) == 0) {
    return(list(value = NULL, year = NA_integer_, status = "missing"))
  }
  row <- trend[trend$is_partial_year %in% TRUE, , drop = FALSE]
  if (nrow(row) == 0) {
    return(list(value = NULL, year = NA_integer_, status = "missing"))
  }
  status <- as.character(row$status[[1]] %||% "ok")
  count <- row$record_count[[1]]
  year <- as.integer(row$year[[1]])
  if (!identical(status, "ok") || is.na(count)) {
    return(list(value = NULL, year = year, status = "missing"))
  }
  list(value = as.integer(count), year = year, status = "ok")
}

literature_trend_window_summary <- function(trend) {
  ok <- literature_ok_trend_rows(trend)
  if (is.null(ok) || nrow(ok) == 0) {
    return(list(value = NULL, n_years = 0L, min_year = NA_integer_, max_year = NA_integer_))
  }
  years <- sort(unique(as.integer(ok$year)))
  list(
    value = if (length(years) == 1L) {
      as.character(years[[1]])
    } else {
      sprintf("%s\u2013%s", min(years), max(years))
    },
    n_years = length(years),
    min_year = min(years),
    max_year = max(years)
  )
}

literature_result_ui <- function(
  ns,
  current,
  selected,
  selected_id,
  selected_year = literature_all_years_value(),
  year_pack = NULL,
  year_loading = FALSE,
  year_progress = NULL,
  year_error = NULL
) {
  lit <- current$literature
  counts <- lit$target_counts
  choices <- if (!is.null(counts) && nrow(counts) > 0) {
    target_selector_choices(counts$project_target_id, counts$symbol)
  } else {
    NULL
  }
  disease_label <- if (!is.null(selected)) {
    selected$disease$canonical_name
  } else {
    NULL
  }
  subtitle <- tagList(
    literature_corpus_definition(),
    if (has_display_text(disease_label)) {
      span(class = "evidence-context-mark", paste0(" \u00b7 ", disease_label))
    }
  )

  tagList(
    div(`data-tw-literature-runtime` = LITERATURE_RUNTIME_MARKER, class = "literature-runtime-mark", hidden = NA),
    target_set_stale_banner(ns, current, "refresh_literature", "Refresh literature"),
    evidence_page_header("Literature", subtitle),
    interpretation_guidance_ui(
      "Publication counts describe literature volume within this search definition. They do not measure evidence quality, target importance, or therapeutic value."
    ),
    exclusion_status_ui(lit$excluded_targets, lit$failures),
    if (!is.null(choices)) {
      evidence_target_switcher_ui(
        ns("literature_target"),
        "Target",
        choices,
        selected_id %||% as.character(choices[[1]])
      )
    },
    if (is.null(selected)) {
      panel_state_ui(
        "blocked",
        "No confirmed target mapped to NCBI Gene",
        "Confirm identity so literature retrieval can use a verified NCBI Gene mapping."
      )
    } else {
      series <- literature_annual_ui_model(selected)
      trend_for_ui <- if (isTRUE(series$ok)) series$trend else empty_literature_trend()
      year_metric <- literature_current_year_summary(trend_for_ui)
      window_metric <- literature_trend_window_summary(trend_for_ui)
      year_label <- if (!is.na(year_metric$year)) {
        sprintf("Records in %s", year_metric$year)
      } else {
        "Records in current calendar year"
      }
      year_hint <- "Current calendar year; partial year. Annual count for this year only, not cumulative."
      if (identical(window_metric$n_years, LITERATURE_TREND_YEARS)) {
        activity_label <- "10-year publication activity"
        activity_hint <- "Calendar years with retrieved annual counts. Annual counts, not a cumulative total."
      } else if (isTRUE(window_metric$n_years >= 2L)) {
        activity_label <- "Publication activity"
        activity_hint <- sprintf(
          "%s calendar years with retrieved annual counts. Annual counts, not a cumulative total.",
          window_metric$n_years
        )
      } else {
        activity_label <- "Publication activity"
        activity_hint <- "Historical annual counts were not retrieved. This is not a multi-year trend."
      }
      activity_help <- if (!isTRUE(series$ok)) {
        series$error
      } else if (isTRUE(window_metric$n_years >= 2L)) {
        "Annual PubMed record count. Not cumulative. The current calendar year is a partial year."
      } else {
        "Only one calendar year was retrieved. This is an annual count, not a trend and not a cumulative total."
      }
      tagList(
        evidence_summary_strip(
          aria_label = "Selected target literature counts",
          evidence_metric(
            "Matched PubMed records",
            evidence_count_label(selected$corpus$total_count),
            "All records matching this search definition as of the recorded retrieval."
          ),
          evidence_metric(
            year_label,
            evidence_count_label(year_metric$value),
            year_hint
          ),
          evidence_metric(
            activity_label,
            window_metric$value,
            activity_hint
          )
        ),
        if (selected$corpus$total_count > PUBMED_UID_RETRIEVAL_LIMIT) {
          p(
            class = "field-help",
            sprintf(
              "ESearch can retrieve at most %s UIDs. The matched record count is the corpus size, not that retrieval limit.",
              format(PUBMED_UID_RETRIEVAL_LIMIT, big.mark = ",", scientific = FALSE, trim = TRUE)
            )
          )
        },
        evidence_primary_surface(
          evidence_section_header(
            "Publication activity",
            activity_help
          ),
          if (!isTRUE(series$ok)) {
            panel_state_ui(
              "unavailable",
              series$error
            )
          } else {
            tagList(
              literature_year_control_ui(ns, trend_for_ui, selected_year),
              as_live_viz(export_lite_trend_html(
                trend_for_ui,
                selected$target$symbol,
                year_input_id = ns("literature_year"),
                selected_year = selected_year
              ))
            )
          }
        ),
        literature_records_panel_ui(
          ns,
          selected,
          selected_year,
          year_pack,
          year_loading,
          year_progress,
          year_error
        ),
        div(
          class = "evidence-secondary-block",
          evidence_section_header(
            "PubMed records by target",
            "Literature volume is not target importance. Counts use the same corpus definition for every target."
          ),
          if (!is.null(counts) && nrow(counts) > 0) {
            as_live_viz(export_lite_pubmed_counts_html(counts))
          } else {
            NULL
          }
        ),
        literature_search_definition_ui(selected, lit, selected_year),
        literature_provenance_ui(selected, lit, selected_year)
      )
    }
  )
}

literature_year_control_ui <- function(ns, trend, selected_year) {
  choices <- literature_year_choices(trend)
  selected_year <- normalize_literature_year(selected_year, trend)
  div(
    class = "literature-year-toolbar",
    div(
      class = "literature-year-control",
      `data-literature-year-input` = ns("literature_year"),
      selectInput(
        ns("literature_year"),
        "Publication year",
        choices = choices,
        selected = selected_year,
        selectize = FALSE,
        width = "220px"
      ),
      p(
        class = "field-help",
        "Same defined corpus; only a calendar-year constraint is added. Options are retrieved annual years only. Annual counts are not cumulative."
      )
    )
  )
}

literature_records_panel_ui <- function(
  ns,
  selected,
  selected_year,
  year_pack,
  year_loading,
  year_progress,
  year_error
) {
  if (is_literature_all_years(selected_year)) {
    return(literature_recent_ui(
      selected$recent_records,
      title = "Recent PubMed records",
      intro = "Recent PubMed records from the full defined corpus. Selecting a calendar year inspects that year without changing the scientific definition."
    ))
  }
  year_label <- as.character(as.integer(selected_year))
  body <- if (isTRUE(year_loading)) {
    panel_state_ui(
      "retrieving",
      year_progress %||% sprintf("Loading publications from %s\u2026", year_label)
    )
  } else if (has_display_text(year_error)) {
    div(
      class = "literature-year-error",
      panel_state_ui("unavailable", year_error),
      actionButton(ns("retry_year_records"), "Retry", class = "btn-text")
    )
  } else if (is.null(year_pack)) {
    panel_state_ui(
      "empty",
      sprintf("Publications from %s are not loaded yet.", year_label)
    )
  } else {
    total <- as.integer(year_pack$total_count %||% 0L)
    shown <- as.integer(year_pack$shown %||% nrow(year_pack$records))
    intro <- sprintf(
      "%s records match the current literature definition in %s. This is the annual count for that calendar year, not a cumulative total.",
      evidence_count_label(total) %||% "0",
      year_label
    )
    range_line <- if (shown > 0L && total > 0L) {
      sprintf("Showing 1\u2013%s", format(shown, big.mark = ",", scientific = FALSE, trim = TRUE))
    } else {
      NULL
    }
    footer <- tagList(
      if (has_display_text(range_line)) p(class = "field-help", range_line),
      if (isTRUE(year_pack$truncated)) {
        p(
          class = "field-help",
          sprintf(
            "Displayed rows are a page of the %s matching records, not the complete year corpus.",
            evidence_count_label(total) %||% as.character(total)
          )
        )
      },
      if (isTRUE(year_pack$truncated)) {
        actionButton(ns("load_more_year"), "Load more", class = "btn-primary-quiet")
      }
    )
    literature_recent_ui(
      year_pack$records,
      title = sprintf("Publications in %s", year_label),
      intro = intro,
      empty_title = sprintf("No source result for %s", year_label),
      empty_detail = "No PubMed records matched this defined corpus in the selected calendar year. The search definition is unchanged except for the year constraint.",
      footer = footer
    )
  }
  div(class = "literature-year-records", body)
}

literature_recent_ui <- function(
  records,
  title = "Recent PubMed records",
  intro = NULL,
  empty_title = "No source result",
  empty_detail = "No PubMed records in this defined corpus. The search definition is unchanged.",
  footer = NULL
) {
  div(
    class = "evidence-records-block",
    evidence_section_header(title),
    if (has_display_text(intro)) p(class = "field-help", intro),
    if (is.null(records) || nrow(records) == 0) {
      panel_state_ui(
        "empty",
        empty_title,
        empty_detail
      )
    } else {
      div(
        class = "table-scroll",
        tags$table(
          class = "evidence-table literature-table",
          tags$thead(
            tags$tr(
              tags$th("Title"),
              tags$th("Metadata"),
              tags$th("PMID")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(records)), function(i) {
              row <- records[i, , drop = FALSE]
              tags$tr(
                class = "literature-record-row",
                tags$td(
                  class = "literature-record-title",
                  normalize_pubmed_title(row$title[[1]])
                ),
                tags$td(
                  class = "literature-meta",
                  paste(
                    row$first_author[[1]],
                    row$journal[[1]],
                    row$publication_date[[1]],
                    sep = " \u00b7 "
                  )
                ),
                tags$td(
                  class = "literature-record-pmid",
                  tags$a(
                    href = row$pubmed_url[[1]],
                    target = "_blank",
                    rel = "noopener noreferrer",
                    sprintf("Open in PubMed (%s)", row$pmid[[1]])
                  )
                )
              )
            })
          )
        )
      )
    },
    footer
  )
}

literature_search_definition_ui <- function(selected, lit, selected_year = literature_all_years_value()) {
  evidence_details_disclosure(
    "Search definition",
    tags$table(
      class = "evidence-table",
      tags$tbody(
        tags$tr(tags$th("Confirmed target"), tags$td(selected$target$symbol)),
        tags$tr(tags$th("UniProt accession"), tags$td(selected$target$uniprot_accession)),
        tags$tr(tags$th("NCBI GeneID"), tags$td(selected$target$ncbi_gene_id)),
        tags$tr(tags$th("Confirmed disease"), tags$td(selected$disease$canonical_name)),
        tags$tr(tags$th("Disease terms used"), tags$td(paste(selected$disease$search_terms, collapse = "; "))),
        tags$tr(tags$th("PubMed fields"), tags$td(selected$corpus$query_scope)),
        tags$tr(tags$th("NCBI Gene \u2192 PubMed"), tags$td("gene_pubmed")),
        tags$tr(tags$th("Exact disease query"), tags$td(selected$corpus$disease_query)),
        tags$tr(
          tags$th("Publication year"),
          tags$td(literature_year_filter_label(selected_year))
        ),
        tags$tr(
          tags$th("PubMed records"),
          tags$td(evidence_count_label(selected$corpus$total_count) %||% "Not retrieved")
        )
      )
    )
  )
}

literature_provenance_ui <- function(selected, lit, selected_year = literature_all_years_value()) {
  prov <- selected$provenance
  cache_label <- switch(
    as.character(prov$cache_status %||% ""),
    live = "Fresh",
    cached = "Cached",
    fresh = "Cached",
    stale = "Stale",
    as.character(prov$cache_status %||% "Unknown")
  )
  evidence_details_disclosure(
    "Source and provenance",
    tags$table(
      class = "evidence-table",
      tags$tbody(
        tags$tr(tags$th("Source"), tags$td("NCBI PubMed")),
        tags$tr(tags$th("NCBI GeneID"), tags$td(prov$ncbi_gene_id)),
        tags$tr(tags$th("UniProt accession"), tags$td(prov$identifier_used)),
        tags$tr(tags$th("Confirmed disease"), tags$td(selected$disease$canonical_name)),
        tags$tr(tags$th("Search scope"), tags$td(selected$corpus$query_scope)),
        tags$tr(
          tags$th("Publication year"),
          tags$td(literature_year_filter_label(selected_year))
        ),
        tags$tr(
          tags$th("PubMed database last update"),
          tags$td(prov$database_last_update %||% lit$provenance$database_last_update %||% "not retrieved")
        ),
        tags$tr(tags$th("Retrieved"), tags$td(as.character(prov$retrieved_at))),
        tags$tr(tags$th("Cache"), tags$td(cache_label))
      )
    ),
    evidence_details_disclosure(
      "Technical provenance",
      tags$table(
        class = "evidence-table",
        tags$tbody(
          tags$tr(tags$th("E-utilities"), tags$td(prov$eutilities)),
          tags$tr(tags$th("Link strategy"), tags$td(prov$link_strategy)),
          tags$tr(tags$th("Endpoint family"), tags$td(prov$endpoint_family)),
          tags$tr(tags$th("Rate-limit mode"), tags$td(prov$rate_limit_mode)),
          tags$tr(
            tags$th("Cache key family"),
            tags$td(
              if (is_literature_all_years(selected_year)) {
                lit$provenance$cache_key_family
              } else {
                paste(
                  lit$provenance$cache_key_family,
                  "pubmed:year-records:<geneid>:<query-signature>:<year>",
                  sep = "; "
                )
              }
            )
          )
        )
      )
    )
  )
}
