confirmed_ot_target <- function(
  ensembl_gene_id = "ENSG00000146648",
  symbol = "EGFR",
  status = "confirmed",
  id = "target-egfr"
) {
  list(
    id = id,
    resolution_status = status,
    display_symbol = symbol,
    ensembl_gene_id = ensembl_gene_id,
    uniprot_accession = "P00533",
    input_text = symbol
  )
}

confirmed_ot_project <- function(
  disease_id = "MONDO_0005233",
  status = "confirmed"
) {
  list(
    id = "project-1",
    disease_label = "Non-small-cell lung cancer",
    disease_ontology_id = if (identical(status, "confirmed")) disease_id else NA_character_,
    disease_name = if (identical(status, "confirmed")) "non-small cell lung carcinoma" else NA_character_,
    disease_resolution_status = status
  )
}

ot_api_result <- function(payload, cache_status = "live", from_cache = FALSE, ok = TRUE, error = NULL) {
  new_api_result(
    ok = ok,
    status = 200L,
    data = payload,
    error = error,
    retrieved_at = as.POSIXct("2026-09-10 12:00:00", tz = "UTC"),
    from_cache = from_cache,
    cache_status = cache_status,
    source = "opentargets",
    url = OT_GRAPHQL_URL
  )
}

test_that("disease search response parser keeps disease hits and provenance meta", {
  parsed <- parse_ot_disease_search(read_fixture("ot_disease_search_nsclc.json"))
  expect_true(parsed$ok)
  expect_equal(length(parsed$hits), 3)
  expect_equal(parsed$hits[[1]]$id, "MONDO_0005233")
  expect_equal(parsed$meta$data_version, "26-06")
  expect_equal(parsed$meta$api_version, "26.6.3")
})

test_that("search score gap alone is not a unique primary disease match", {
  parsed <- parse_ot_disease_search(read_fixture("ot_disease_search_unique.json"))
  normalized <- normalize_disease_candidates(parsed, "carcinoma")
  expect_equal(normalized$status, "ambiguous")
  expect_false(normalized$unique_primary)
  expect_true(all(vapply(normalized$candidates, function(c) identical(c$match_type, "search_text_match"), logical(1))))
})

test_that("exact documented synonym can mark a unique primary without auto-confirm", {
  parsed <- parse_ot_disease_search(read_fixture("ot_disease_search_nsclc.json"))
  normalized <- normalize_disease_candidates(parsed, "Non small cell lung cancer")
  expect_equal(normalized$status, "unresolved")
  expect_true(normalized$unique_primary)
  expect_equal(normalized$candidates[[1]]$id, "MONDO_0005233")
  expect_equal(normalized$candidates[[1]]$match_type, "exact_synonym")
  expect_equal(normalized$candidates[[1]]$matched_synonym, "non-small cell lung cancer")
  expect_match(normalized$candidates[[1]]$match_reason, "Matched through synonym")
  expect_false(identical(normalized$status, "confirmed"))
})

test_that("NSCLC acronym matches a documented synonym and is not auto-confirmed", {
  parsed <- parse_ot_disease_search(read_fixture("ot_disease_search_unique.json"))
  normalized <- normalize_disease_candidates(parsed, "NSCLC")
  expect_equal(normalized$status, "unresolved")
  expect_true(normalized$unique_primary)
  expect_equal(normalized$candidates[[1]]$id, "MONDO_0005233")
  expect_equal(normalized$candidates[[1]]$match_type, "exact_synonym")
  expect_equal(normalized$candidates[[1]]$matched_synonym, "NSCLC")
  expect_false(identical(normalized$status, "confirmed"))
})

test_that("empty disease search is failed, not an association", {
  parsed <- parse_ot_disease_search(read_fixture("ot_disease_search_empty.json"))
  expect_true(parsed$ok)
  expect_equal(parsed$total, 0)
  normalized <- normalize_disease_candidates(parsed, "zzz")
  expect_equal(normalized$status, "failed")
  expect_equal(length(normalized$candidates), 0)
})

test_that("hyphenated disease labels retry a dehyphenated Open Targets query", {
  queries <- character()
  result <- resolve_project_disease(
    "Non-small-cell lung cancer",
    search_fetch = function(query, db_pool = NULL) {
      queries <<- c(queries, query)
      payload <- if (grepl("-", query, fixed = TRUE)) {
        read_fixture("ot_disease_search_empty.json")
      } else {
        read_fixture("ot_disease_search_nsclc.json")
      }
      out <- ot_api_result(payload)
      out$query_name <- "TargetWeaveDiseaseSearch"
      out
    }
  )
  expect_equal(queries, c("Non-small-cell lung cancer", "Non small cell lung cancer"))
  expect_equal(result$status, "unresolved")
  expect_true(result$unique_primary)
  expect_equal(result$candidates[[1]]$id, "MONDO_0005233")
  expect_equal(result$candidates[[1]]$match_type, "exact_synonym")
  expect_false(identical(result$status, "confirmed"))
})

test_that("GraphQL errors and malformed payloads do not crash parsers", {
  err <- parse_ot_disease_search(read_fixture("ot_graphql_error.json"))
  expect_false(err$ok)
  expect_match(err$error, "Cannot query field")

  malformed <- parse_ot_disease_search(list(foo = 1))
  expect_false(malformed$ok)

  assoc_err <- parse_ot_target_disease_evidence(read_fixture("ot_graphql_error.json"))
  expect_false(assoc_err$ok)
  expect_null(assoc_err$association)

  assoc_bad <- parse_ot_target_disease_evidence(list(hello = TRUE))
  expect_false(assoc_bad$ok)
})

test_that("unresolved disease or target blocks evidence fetch", {
  target <- confirmed_ot_target()
  project <- confirmed_ot_project()
  expect_true(can_fetch_ot_evidence(target, project))

  blocked_disease <- retrieve_target_disease_evidence(
    target,
    confirmed_ot_project(status = "unresolved"),
    fetch = function(...) stop("must not fetch")
  )
  expect_equal(blocked_disease$status, "blocked_disease")
  expect_null(blocked_disease$evidence)

  blocked_target <- retrieve_target_disease_evidence(
    confirmed_ot_target(status = "unresolved"),
    project,
    fetch = function(...) stop("must not fetch")
  )
  expect_equal(blocked_target$status, "blocked_target")
  expect_null(blocked_target$evidence)

  expect_false(
    should_retrieve_ot_evidence(TRUE, confirmed_ot_target(status = "unresolved"), project)
  )
  expect_false(
    should_retrieve_ot_evidence(TRUE, target, confirmed_ot_project(status = "unresolved"))
  )
})

test_that("confirmed target and disease allow evidence fetch and parse scores", {
  payload <- read_fixture("ot_association_egfr_nsclc.json")
  calls <- 0L
  result <- retrieve_target_disease_evidence(
    confirmed_ot_target(),
    confirmed_ot_project(),
    fetch = function(ensembl_gene_id, disease_id, db_pool = NULL) {
      calls <<- calls + 1L
      expect_equal(ensembl_gene_id, "ENSG00000146648")
      expect_equal(disease_id, "MONDO_0005233")
      out <- ot_api_result(payload)
      out$query_name <- "TargetWeaveTargetDiseaseEvidence"
      out
    }
  )

  expect_equal(calls, 1L)
  expect_equal(result$status, "live")
  evidence <- result$evidence
  expect_equal(evidence$association$association_scope, "direct")
  expect_equal(evidence$association$overall_score_direct, 0.8525670184292347, tolerance = 1e-8)
  expect_equal(evidence$association$overall_score_inclusive, 0.9218179690478745, tolerance = 1e-8)
  scores <- evidence$association$datatype_scores
  expect_equal(scores$score[scores$datatype_id == "rna_expression"], 0)
  expect_true("animal_model" %in% scores$datatype_id)
  expect_true(is.na(scores$score[scores$datatype_id == "animal_model"]))
  expect_false("known_drug" %in% scores$datatype_id)
  expect_equal(evidence$provenance$query_name, "TargetWeaveTargetDiseaseEvidence")
  expect_equal(evidence$provenance$target_id, "ENSG00000146648")
  expect_equal(evidence$provenance$disease_id, "MONDO_0005233")
  expect_equal(evidence$provenance$data_version, "26-06")
  expect_equal(length(evidence$therapeutic_evidence$rows), 2)
  expect_equal(evidence$therapeutic_evidence$count, 1490)
  expect_true(all(vapply(evidence$therapeutic_evidence$rows, function(row) {
    identical(row$target_id, "ENSG00000146648") && identical(row$disease_id, "MONDO_0005233")
  }, logical(1))))
  expect_false("datatype_id" %in% names(evidence$association$datasource_scores))
  expect_true("clinical_precedence" %in% evidence$association$datasource_scores$datasource_id)
  expect_equal(
    evidence$association$datasource_scores$datasource_label[
      evidence$association$datasource_scores$datasource_id == "clinical_precedence"
    ],
    "Clinical Precedence"
  )
  expect_true("targetweave_display_label" %in% evidence$association$datasource_scores$label_source)
  expect_match(evidence$association$component_score_note, "does not map data sources")
  expect_equal(evidence$provenance$graphql_scope_direct, "enableIndirect = false")
})

test_that("therapeutic evidence from another target cannot leak into the selected pair", {
  payload <- read_fixture("ot_association_egfr_nsclc.json")
  payload$data$disease$clinicalEvidence$rows <- c(
    payload$data$disease$clinicalEvidence$rows,
    list(list(
      id = "kras-leak",
      score = 1,
      datasourceId = "clinical_precedence",
      datatypeId = "clinical",
      clinicalStage = "APPROVAL",
      drugFromSource = "sotorasib",
      drug = list(id = "CHEMBL4558572", name = "SOTORASIB"),
      target = list(id = "ENSG00000133703", approvedSymbol = "KRAS"),
      disease = list(id = "MONDO_0005233", name = "non-small cell lung carcinoma")
    ))
  )

  result <- retrieve_target_disease_evidence(
    confirmed_ot_target(),
    confirmed_ot_project(),
    fetch = function(...) {
      out <- ot_api_result(payload)
      out$query_name <- "TargetWeaveTargetDiseaseEvidence"
      out
    }
  )

  drugs <- vapply(result$evidence$therapeutic_evidence$rows, function(row) row$drug_id, character(1))
  targets <- vapply(result$evidence$therapeutic_evidence$rows, function(row) row$target_id, character(1))
  expect_false("CHEMBL4558572" %in% drugs)
  expect_false("ENSG00000133703" %in% targets)
  expect_true("CHEMBL3353410" %in% drugs)
  expect_true(all(targets == "ENSG00000146648"))
  expect_match(OT_QUERY_TARGET_DISEASE_EVIDENCE, "ensemblIds: \\[\\$ensemblId\\]")
  expect_match(OT_QUERY_TARGET_DISEASE_EVIDENCE, "disease\\(efoId: \\$diseaseId\\)")
  expect_match(OT_QUERY_TARGET_DISEASE_EVIDENCE, "target \\{ id approvedSymbol \\}")
  expect_match(OT_QUERY_TARGET_DISEASE_EVIDENCE, "disease \\{ id name \\}")
})

test_that("empty target-disease association is distinct from an API error", {
  result <- retrieve_target_disease_evidence(
    confirmed_ot_target(),
    confirmed_ot_project(),
    fetch = function(...) {
      out <- ot_api_result(read_fixture("ot_association_empty.json"))
      out$query_name <- "TargetWeaveTargetDiseaseEvidence"
      out
    }
  )
  expect_equal(result$status, "empty")
  expect_equal(result$evidence$state, "empty")
  expect_match(result$message, "no association")
})

test_that("zero scores remain in plot data while missing scores are omitted", {
  scores <- data.frame(
    datatype_id = c("rna_expression", "animal_model"),
    datatype_label = c("RNA expression", "Animal model"),
    score = c(0, NA_real_),
    score_status = c("present", "missing"),
    stringsAsFactors = FALSE
  )
  plot_data <- build_ot_evidence_plot_data(scores)
  expect_equal(nrow(plot_data), 1)
  expect_equal(plot_data$datatype_id, "rna_expression")
  expect_equal(plot_data$score, 0)
})

test_that("stale cache fallback and fresh-signature skip are preserved", {
  payload <- read_fixture("ot_association_egfr_nsclc.json")
  stale <- retrieve_target_disease_evidence(
    confirmed_ot_target(),
    confirmed_ot_project(),
    fetch = function(...) {
      out <- ot_api_result(payload, cache_status = "stale", from_cache = TRUE, ok = TRUE, error = "HTTP 503.")
      out$query_name <- "TargetWeaveTargetDiseaseEvidence"
      out
    }
  )
  expect_equal(stale$status, "stale")
  expect_equal(stale$evidence$provenance$cache_status, "stale")

  row <- confirmed_ot_target()
  project <- confirmed_ot_project()
  sig <- evidence_fetch_signature(row, project)
  expect_false(should_retrieve_ot_evidence(TRUE, row, project, last_signature = sig))
  expect_true(should_retrieve_ot_evidence(TRUE, row, project, last_signature = sig, force = TRUE))
  expect_false(should_retrieve_ot_evidence(FALSE, row, project))
})

test_that("switching targets does not reuse the previous evidence signature", {
  egfr <- confirmed_ot_target()
  kras <- confirmed_ot_target(
    ensembl_gene_id = "ENSG00000133703",
    symbol = "KRAS",
    id = "target-kras"
  )
  project <- confirmed_ot_project()
  expect_false(identical(
    evidence_fetch_signature(egfr, project),
    evidence_fetch_signature(kras, project)
  ))

  seen <- character()
  fetch <- function(ensembl_gene_id, disease_id, db_pool = NULL) {
    seen <<- c(seen, ensembl_gene_id)
    payload <- read_fixture("ot_association_egfr_nsclc.json")
    payload$data$target$id <- ensembl_gene_id
    payload$data$target$approvedSymbol <- if (identical(ensembl_gene_id, "ENSG00000133703")) "KRAS" else "EGFR"
    out <- ot_api_result(payload)
    out$query_name <- "TargetWeaveTargetDiseaseEvidence"
    out
  }

  first <- retrieve_target_disease_evidence(egfr, project, fetch = fetch)
  second <- retrieve_target_disease_evidence(kras, project, fetch = fetch)
  expect_equal(first$evidence$target$ensembl_gene_id, "ENSG00000146648")
  expect_equal(second$evidence$target$ensembl_gene_id, "ENSG00000133703")
  expect_equal(second$evidence$target$symbol, "KRAS")
  expect_equal(seen, c("ENSG00000146648", "ENSG00000133703"))
})

test_that("disease confirmation persistence is owner-scoped", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("ot-a-%s@example.com", suffix), "correct-horse-battery")
  other <- register_user(db_pool, sprintf("ot-b-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "Potential therapeutic targets in NSCLC",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR")
  )

  payload <- normalize_disease_candidates(
    parse_ot_disease_search(read_fixture("ot_disease_search_unique.json")),
    "NSCLC"
  )
  expect_false(save_disease_lookup(db_pool, created$project_id, other$user$id, payload$status, payload))
  expect_true(save_disease_lookup(db_pool, created$project_id, owner$user$id, payload$status, payload))

  denied <- confirm_project_disease(
    db_pool,
    created$project_id,
    other$user$id,
    "MONDO_0005233",
    "non-small cell lung carcinoma"
  )
  expect_false(denied$ok)

  accepted <- confirm_project_disease(
    db_pool,
    created$project_id,
    owner$user$id,
    "MONDO_0005233",
    "non-small cell lung carcinoma"
  )
  expect_true(accepted$ok)

  owned <- get_owned_project(db_pool, created$project_id, owner$user$id)
  expect_equal(owned$disease_ontology_id, "MONDO_0005233")
  expect_equal(owned$disease_name, "non-small cell lung carcinoma")
  expect_equal(owned$disease_label, "Non-small-cell lung cancer")
  expect_equal(owned$disease_resolution_status, "confirmed")
  expect_true(project_disease_is_confirmed(owned))
  expect_true(is.null(get_owned_project(db_pool, created$project_id, other$user$id)))
})

test_that("legacy disease_efo_id values migrate to disease_ontology_id", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("ot-mig-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "Legacy disease id",
    "Preserve the confirmed ontology identifier.",
    "Non-small-cell lung cancer",
    c("EGFR")
  )

  DBI::dbExecute(
    db_pool,
    "ALTER TABLE projects ADD COLUMN IF NOT EXISTS disease_efo_id TEXT NULL"
  )
  DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET disease_efo_id = $2,
        disease_ontology_id = NULL,
        disease_name = $3,
        disease_resolution_status = 'confirmed'
    WHERE id = $1::uuid
    ",
    params = list(created$project_id, "MONDO_0005233", "non-small cell lung carcinoma")
  )

  migrate_disease_ontology_id(db_pool)
  owned <- get_owned_project(db_pool, created$project_id, owner$user$id)
  expect_equal(owned$disease_ontology_id, "MONDO_0005233")
  expect_equal(owned$disease_label, "Non-small-cell lung cancer")
  expect_equal(owned$disease_name, "non-small cell lung carcinoma")

  leftover <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = current_schema()
      AND table_name = 'projects'
      AND column_name = 'disease_efo_id'
    "
  )
  expect_equal(nrow(leftover), 0)
})

test_that("EFO disease identifiers still persist as ontology ids", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("ot-efo-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "EFO disease id",
    "EFO identifiers remain valid.",
    "Non-small-cell lung cancer",
    c("EGFR")
  )
  accepted <- confirm_project_disease(
    db_pool,
    created$project_id,
    owner$user$id,
    "EFO_0003060",
    "non-small cell lung carcinoma"
  )
  expect_true(accepted$ok)
  owned <- get_owned_project(db_pool, created$project_id, owner$user$id)
  expect_equal(owned$disease_ontology_id, "EFO_0003060")
  expect_true(project_disease_is_confirmed(owned))
})

test_that("Open Targets cache keys are canonical and contain no secrets", {
  expect_equal(
    cache_key_opentargets_disease_search(" Non-Small Cell  "),
    "opentargets:disease-search:non-small cell"
  )
  expect_equal(
    cache_key_opentargets_association("ensg00000146648", "mondo_0005233"),
    "opentargets:association:ENSG00000146648:MONDO_0005233"
  )
})

test_that("http_post_json uses fresh cache and stale fallback", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)
  skip_if_not_installed("httr2")

  key <- paste0("test:ot:post:", uuid::UUIDgenerate())
  calls <- 0L
  perform <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 200,
      url = OT_GRAPHQL_URL,
      method = "POST",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"data":{"ok":true}}')
    )
  }

  first <- http_post_json(
    OT_GRAPHQL_URL,
    body = list(query = "{ meta { name } }", variables = list()),
    db_pool = db_pool,
    cache_source = "opentargets",
    cache_key = key,
    ttl_seconds = 86400,
    perform = perform
  )
  second <- http_post_json(
    OT_GRAPHQL_URL,
    body = list(query = "{ meta { name } }", variables = list()),
    db_pool = db_pool,
    cache_source = "opentargets",
    cache_key = key,
    ttl_seconds = 86400,
    perform = perform
  )
  expect_false(first$from_cache)
  expect_true(second$from_cache)
  expect_equal(second$cache_status, "fresh")
  expect_equal(calls, 1L)

  DBI::dbExecute(
    db_pool,
    "UPDATE api_cache SET expires_at = NOW() - INTERVAL '1 hour' WHERE cache_key = $1",
    params = list(key)
  )
  stale <- http_post_json(
    OT_GRAPHQL_URL,
    body = list(query = "{ meta { name } }", variables = list()),
    db_pool = db_pool,
    cache_source = "opentargets",
    cache_key = key,
    ttl_seconds = 86400,
    perform = function(req) stop("network down")
  )
  expect_true(stale$ok)
  expect_true(stale$from_cache)
  expect_equal(stale$cache_status, "stale")
})

test_that("datasource labels are TargetWeave-maintained names or raw ids", {
  expect_equal(ot_datasource_label("clinical_precedence"), "Clinical Precedence")
  expect_equal(ot_datasource_label("eva_somatic"), "EVA Somatic")
  expect_equal(ot_datasource_label("cancer_gene_census"), "Cancer Gene Census")
  expect_equal(ot_datasource_label("cancer_biomarkers"), "Cancer Biomarkers")
  expect_equal(ot_datasource_label("europepmc"), "Europe PMC")
  expect_equal(ot_datasource_label("not_a_real_source"), "not_a_real_source")
})

test_that("therapeutic presentation uses readable drug names and labelled scores", {
  expect_equal(readable_drug_name("OSIMERTINIB"), "Osimertinib")
  expect_equal(readable_drug_name("ERLOTINIB HYDROCHLORIDE"), "Erlotinib Hydrochloride")
  expect_equal(readable_drug_name("afatinib"), "afatinib")
  expect_equal(format_clinical_stage("PHASE_4"), "Phase 4")
  expect_equal(format_clinical_stage("APPROVAL"), "Approval")
})

test_that("disease evidence UI uses researcher-facing copy", {
  skip_if_not_installed("shiny")
  library(shiny)
  source_app("R/modules/mod_target_resolver.R")
  source_app("R/modules/mod_disease_resolver.R")
  source_app("R/modules/mod_disease_evidence.R")

  packed <- retrieve_target_disease_evidence(
    confirmed_ot_target(),
    confirmed_ot_project(),
    fetch = function(...) {
      out <- ot_api_result(read_fixture("ot_association_egfr_nsclc.json"))
      out$query_name <- "TargetWeaveTargetDiseaseEvidence"
      out
    }
  )
  html <- as.character(evidence_result_ui(packed, NS("evidence")))
  expect_match(html, "Direct association")
  expect_match(html, "Broader ontology-aware association")
  expect_match(html, "Evidence linked directly to the confirmed disease term")
  expect_false(grepl("Chart scope stored in the model", html))
  expect_match(html, "Open Targets evidence score")
  expect_match(html, "Osimertinib")
  expect_false(grepl(">OSIMERTINIB<", html))
  expect_match(html, "not probabilities of causality")
  expect_match(html, "Technical provenance")
  expect_match(html, "enableIndirect = false")

  card <- as.character(disease_candidate_card(
    list(
      name = "non-small cell lung carcinoma",
      id = "MONDO_0005233",
      candidate_key = "MONDO_0005233",
      match_reason = "Matched through synonym: \"non-small cell lung cancer\"",
      search_score = 1457.9
    ),
    NS("disease")
  ))
  expect_match(card, "Matched through synonym")
  expect_false(grepl("search score", card, ignore.case = TRUE))

  resolver <- as.character(div(p("Link this disease to a standardized ontology term before exploring evidence.")))
  expect_match(as.character(resolver), "standardized ontology term")
})

test_that("workspace navigation is not sticky over scientific content", {
  css <- paste(readLines(file.path(app_root(), "www", "styles.css")), collapse = "\n")
  expect_match(css, "\\.topbar[[:space:]]*\\{[^}]*position: sticky")
  expect_match(css, "\\.workspace-nav[[:space:]]*\\{[^}]*position: relative")
  expect_false(grepl("\\.workspace-nav[[:space:]]*\\{[^}]*position: sticky", css))
})
