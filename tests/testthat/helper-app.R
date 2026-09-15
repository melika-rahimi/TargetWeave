app_root <- function() {
  candidates <- c(
    getwd(),
    normalizePath("..", mustWork = FALSE),
    normalizePath("../..", mustWork = FALSE)
  )

  for (path in unique(candidates)) {
    if (file.exists(file.path(path, "app.R"))) {
      return(path)
    }
  }

  stop("Cannot locate TargetWeave app.R")
}

source_app <- function(...) {
  source(file.path(app_root(), ...), local = parent.frame())
}

if (file.exists(file.path(app_root(), ".Renviron"))) {
  readRenviron(file.path(app_root(), ".Renviron"))
}

suppressPackageStartupMessages({
  library(htmltools)
  library(shiny)
})

# testthat helper env does not always inherit globalenv(), so UI closures
# cannot see search-path exports unless they are bound here.
helper_env <- environment()
helper_env$tags <- htmltools::tags
helper_env$div <- htmltools::div
helper_env$span <- htmltools::span
helper_env$p <- htmltools::p
helper_env$h1 <- htmltools::h1
helper_env$h2 <- htmltools::h2
helper_env$h3 <- htmltools::h3
helper_env$h4 <- htmltools::h4
helper_env$strong <- htmltools::strong
helper_env$a <- htmltools::a
helper_env$HTML <- htmltools::HTML
helper_env$tagList <- htmltools::tagList
helper_env$NS <- shiny::NS
helper_env$uiOutput <- shiny::uiOutput
helper_env$actionButton <- shiny::actionButton
helper_env$textInput <- shiny::textInput
helper_env$selectInput <- shiny::selectInput
helper_env$textAreaInput <- shiny::textAreaInput
helper_env$conditionalPanel <- shiny::conditionalPanel
helper_env$radioButtons <- shiny::radioButtons
helper_env$checkboxGroupInput <- shiny::checkboxGroupInput
helper_env$plotOutput <- shiny::plotOutput
helper_env$moduleServer <- shiny::moduleServer
helper_env$observeEvent <- shiny::observeEvent
helper_env$reactiveVal <- shiny::reactiveVal
helper_env$reactive <- shiny::reactive
helper_env$renderUI <- shiny::renderUI
helper_env$req <- shiny::req
helper_env$icon <- shiny::icon
helper_env$hr <- htmltools::hr
helper_env$br <- htmltools::br
helper_env$img <- htmltools::img
helper_env$checkboxInput <- shiny::checkboxInput
helper_env$downloadButton <- shiny::downloadButton
helper_env$downloadHandler <- shiny::downloadHandler
helper_env$renderPlot <- shiny::renderPlot
helper_env$updateTextInput <- shiny::updateTextInput
helper_env$updateTextAreaInput <- shiny::updateTextAreaInput
helper_env$updateSelectInput <- shiny::updateSelectInput


source_app("R/config.R")
source_app("R/ops/log.R")
source_app("R/ops/limits.R")
source_app("R/ops/health.R")
source_app("R/ops/async.R")
source_app("R/db/users.R")
source_app("R/db/profiles.R")
source_app("R/db/support.R")
source_app("R/db/projects.R")
source_app("R/db/targets.R")
source_app("R/db/authz.R")
source_app("R/db/schema.R")
source_app("R/db/migrations.R")
source_app("R/db/pool.R")
source_app("R/db/cache.R")
source_app("R/db/snapshots.R")
source_app("R/db/notes.R")
source_app("R/process/ensembl_ids.R")
source_app("R/api/http_client.R")
source_app("R/api/api_uniprot.R")
source_app("R/api/api_ensembl.R")
source_app("R/api/api_opentargets.R")
source_app("R/api/api_reactome.R")
source_app("R/api/api_ncbi.R")
source_app("R/api/api_rcsb.R")
source_app("R/process/resolve_targets.R")
source_app("R/process/process_overview.R")
source_app("R/process/process_disease.R")
source_app("R/process/process_ot_evidence.R")
source_app("R/process/process_ot_comparison.R")
source_app("R/process/process_reactome.R")
source_app("R/process/process_pathway_overlap.R")
source_app("R/process/process_literature.R")
source_app("R/process/process_structure_coverage.R")
source_app("R/process/process_structures.R")
source_app("R/process/process_snapshots.R")
source_app("R/process/process_invalidation.R")
source_app("R/export/export_tables.R")
source_app("R/export/export_manifest.R")
source_app("R/export/export_dossier.R")
source_app("R/process/workspace_nav.R")
source_app("R/viz/viz_theme.R")
source_app("R/viz/viz_genomic.R")
source_app("R/viz/viz_ot_evidence.R")
source_app("R/viz/viz_ot_comparison.R")
source_app("R/viz/viz_pathways.R")
source_app("R/viz/viz_literature.R")
source_app("R/viz/viz_structures.R")
source_app("R/export/export_lite_viz.R")
source_app("R/ui/ui_components.R")
source_app("R/ui/ui_landing.R")
source_app("R/ui/ui_tour.R")
source_app("R/modules/mod_overview.R")
source_app("R/modules/mod_project_setup.R")
source_app("R/modules/mod_onboarding.R")
source_app("R/modules/mod_auth.R")
source_app("R/modules/mod_account.R")
source_app("R/modules/mod_notes.R")
source_app("R/modules/mod_research.R")

fixture_path <- function(name) {
  file.path(app_root(), "tests", "testdata", name)
}

read_fixture <- function(name) {
  jsonlite::fromJSON(fixture_path(name), simplifyVector = FALSE)
}

reactome_json_perform <- function(payload, status = 200L, url = "https://reactome.org/ContentService/mock") {
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  function(req) {
    httr2::response(
      status_code = status,
      url = url,
      method = "GET",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(as.character(body))
    )
  }
}

reactome_text_perform <- function(text, status = 200L) {
  function(req) {
    httr2::response(
      status_code = status,
      url = "https://reactome.org/ContentService/data/database/version",
      method = "GET",
      headers = list("Content-Type" = "text/plain;charset=UTF-8"),
      body = charToRaw(as.character(text))
    )
  }
}

confirmed_reactome_target <- function(
  id,
  symbol,
  uniprot,
  ensembl = NA_character_,
  status = "confirmed"
) {
  data.frame(
    id = id,
    resolution_status = status,
    display_symbol = symbol,
    ensembl_gene_id = ensembl,
    uniprot_accession = uniprot,
    input_text = symbol,
    stringsAsFactors = FALSE
  )
}

membership_from_ids <- function(target_id, symbol, uniprot, pathway_ids, names = pathway_ids) {
  pathways <- empty_reactome_pathways()
  if (length(pathway_ids) > 0) {
    pathways <- data.frame(
      pathway_id = pathway_ids,
      pathway_name = names,
      species = REACTOME_HUMAN_SPECIES,
      membership_scope = REACTOME_MEMBERSHIP_SCOPE,
      is_in_disease = FALSE,
      pathway_url = vapply(pathway_ids, reactome_pathway_page_url, character(1)),
      stringsAsFactors = FALSE
    )
  }
  list(
    project_target_id = target_id,
    symbol = symbol,
    ensembl_gene_id = NA_character_,
    uniprot_accession = uniprot,
    pathways = pathways
  )
}

uniprot_payload_fetch <- function(payload) {
  function(accession, db_pool = NULL) {
    new_api_result(
      ok = TRUE,
      status = 200L,
      data = payload,
      cache_status = "fresh",
      source = "uniprot",
      url = "https://rest.uniprot.org/uniprotkb/P00533.json"
    )
  }
}

ncbi_url_text <- function(req) {
  as.character(req$url %||% "")
}

ncbi_fixture_perform <- function(
  gene = read_fixture("ncbi_esummary_gene_1956.json"),
  elink = read_fixture("ncbi_elink_gene_pubmed.json"),
  count = read_fixture("ncbi_esearch_count.json"),
  recent = read_fixture("ncbi_esearch_recent.json"),
  summaries = read_fixture("ncbi_esummary_pubmed.json"),
  einfo = read_fixture("ncbi_einfo_pubmed.json"),
  year_counts = list(`2018` = 2L, `2019` = 4L, `2020` = 0L, `2021` = 7L),
  fail_esearch = FALSE
) {
  function(req) {
    url <- ncbi_url_text(req)
    payload <- NULL
    if (grepl("einfo\\.fcgi", url)) {
      payload <- einfo
    } else if (grepl("elink\\.fcgi", url)) {
      payload <- elink
    } else if (grepl("esummary\\.fcgi", url) && grepl("db=gene", url)) {
      payload <- gene
    } else if (grepl("esummary\\.fcgi", url)) {
      payload <- summaries
    } else if (grepl("esearch\\.fcgi", url)) {
      if (isTRUE(fail_esearch)) {
        return(httr2::response(
          status_code = 503L,
          url = url,
          method = "GET",
          headers = list("Content-Type" = "application/json"),
          body = charToRaw("{\"error\":\"unavailable\"}")
        ))
      }
      if (grepl("MET\\[Title/Abstract\\]", url) && !grepl("gene_pubmed|WebEnv|#", url)) {
        stop("raw symbol search is not allowed")
      }
      year <- sub(".*mindate=([0-9]{4}).*", "\\1", url)
      if (grepl("mindate=", url) && grepl("^[0-9]{4}$", year)) {
        n <- year_counts[[year]]
        if (is.null(n)) {
          n <- 3L
        }
        payload <- list(
          header = list(type = "esearch", version = "0.3"),
          esearchresult = list(count = as.character(n))
        )
      } else if (grepl("retmax=15", url) || grepl("sort=pub", url)) {
        payload <- recent
      } else {
        payload <- count
      }
    } else {
      payload <- list(error = "unexpected NCBI request")
    }
    httr2::response(
      status_code = 200L,
      url = url,
      method = "GET",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")))
    )
  }
}

rcsb_url_text <- function(req) {
  as.character(req$url %||% "")
}

rcsb_fixture_perform <- function(
  search = read_fixture("rcsb_search_p00533_excerpt.json"),
  metadata = read_fixture("rcsb_data_entities.json"),
  alignments = read_fixture("rcsb_seqcoords_p00533.json"),
  search_status = 200L,
  metadata_status = 200L,
  drop_entity_ids = character(),
  expected_accession = "P00533"
) {
  empty_search <- read_fixture("rcsb_search_empty.json")
  function(req) {
    url <- rcsb_url_text(req)
    body_text <- tryCatch(
      as.character(jsonlite::toJSON(req$body$data, auto_unbox = TRUE, null = "null")),
      error = function(e) ""
    )
    matches_accession <- !nzchar(expected_accession) || grepl(expected_accession, body_text, fixed = TRUE)
    if (grepl("search\\.rcsb\\.org", url)) {
      if (!identical(as.integer(search_status), 200L)) {
        return(httr2::response(
          status_code = as.integer(search_status),
          url = url,
          method = "POST",
          headers = list("Content-Type" = "application/json"),
          body = charToRaw("{\"error\":\"unavailable\"}")
        ))
      }
      payload <- if (isTRUE(matches_accession)) search else empty_search
    } else if (grepl("sequence-coordinates\\.rcsb\\.org", url)) {
      payload <- if (isTRUE(matches_accession)) alignments else list(data = list(alignments = list(target_alignments = list())))
    } else {
      if (!identical(as.integer(metadata_status), 200L)) {
        return(httr2::response(
          status_code = as.integer(metadata_status),
          url = url,
          method = "POST",
          headers = list("Content-Type" = "application/json"),
          body = charToRaw("{\"error\":\"unavailable\"}")
        ))
      }
      payload <- metadata
      if (length(drop_entity_ids) > 0 && is.list(payload$data$polymer_entities)) {
        keep <- vapply(payload$data$polymer_entities, function(ent) {
          !(as.character(ent$rcsb_id %||% "") %in% drop_entity_ids)
        }, logical(1))
        payload$data$polymer_entities <- payload$data$polymer_entities[keep]
      }
    }
    httr2::response(
      status_code = 200L,
      url = url,
      method = "POST",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")))
    )
  }
}

nsclc_literature_project <- function() {
  payload <- list(
    status = "unresolved",
    query_text = "non-small cell lung cancer",
    candidates = list(list(
      id = "MONDO_0005233",
      name = "non-small cell lung carcinoma",
      match_type = "exact_synonym",
      matched_synonym = "non-small cell lung cancer"
    ))
  )
  data.frame(
    id = "proj-nsclc",
    disease_label = "non-small cell lung cancer",
    disease_name = "non-small cell lung carcinoma",
    disease_ontology_id = "MONDO_0005233",
    disease_resolution_status = "confirmed",
    disease_resolution_payload = as.character(
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
    ),
    research_question = "Which target deserves deeper investigation in NSCLC?",
    stringsAsFactors = FALSE
  )
}

skip_if_no_postgres <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  skip_if_not_installed("pool")
  skip_if_not_installed("sodium")
  skip_if_not_installed("uuid")

  config <- get_app_config()
  if (!nzchar(config$pg_password)) {
    skip("PostgreSQL password is not configured.")
  }

  connect_error <- NULL
  pool <- tryCatch(
    create_db_pool(),
    error = function(e) {
      connect_error <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(pool)) {
    skip(paste("PostgreSQL is not reachable:", connect_error))
  }

  pool
}
