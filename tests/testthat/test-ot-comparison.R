confirmed_ot_target <- function(
  ensembl_gene_id = "ENSG00000146648",
  symbol = "EGFR",
  status = "confirmed",
  id = "target-egfr"
) {
  data.frame(
    id = id,
    resolution_status = status,
    display_symbol = symbol,
    ensembl_gene_id = ensembl_gene_id,
    uniprot_accession = NA_character_,
    input_text = symbol,
    stringsAsFactors = FALSE
  )
}

confirmed_ot_project <- function(
  disease_id = "MONDO_0005233",
  status = "confirmed",
  id = "project-1"
) {
  data.frame(
    id = id,
    disease_label = "Non-small-cell lung cancer",
    disease_ontology_id = if (identical(status, "confirmed")) disease_id else NA_character_,
    disease_name = if (identical(status, "confirmed")) "non-small cell lung carcinoma" else NA_character_,
    disease_resolution_status = status,
    stringsAsFactors = FALSE
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

bind_targets <- function(...) {
  do.call(rbind, list(...))
}

clone_association_payload <- function(
  ensembl_gene_id,
  symbol,
  overall_direct,
  overall_inclusive,
  datatype_scores
) {
  payload <- read_fixture("ot_association_egfr_nsclc.json")
  payload$data$target$id <- ensembl_gene_id
  payload$data$target$approvedSymbol <- symbol
  payload$data$target$direct$rows[[1]]$score <- overall_direct
  payload$data$target$direct$rows[[1]]$datatypeScores <- datatype_scores
  payload$data$target$inclusive$rows[[1]]$score <- overall_inclusive
  payload$data$disease$clinicalEvidence$count <- 0
  payload$data$disease$clinicalEvidence$rows <- list()
  payload
}

kras_payload <- function() {
  clone_association_payload(
    "ENSG00000133703",
    "KRAS",
    0.72,
    0.80,
    list(
      list(id = "clinical", score = 0.40),
      list(id = "somatic_mutation", score = 0.90),
      list(id = "rna_expression", score = 0),
      list(id = "known_drug", score = 0.55)
    )
  )
}

tp53_payload <- function() {
  clone_association_payload(
    "ENSG00000141510",
    "TP53",
    0.61,
    0.70,
    list(
      list(id = "literature", score = 0.88),
      list(id = "somatic_mutation", score = 0.95),
      list(id = "animal_model", score = NULL)
    )
  )
}

pair_fetch_for <- function(payloads, errors = character(), stale = character()) {
  function(target_row, project_row, db_pool = NULL) {
    ensembl <- canonical_ensembl_gene_id(target_row$ensembl_gene_id[[1]])
    if (ensembl %in% errors) {
      return(list(
        status = "error",
        message = "Open Targets could not be reached.",
        evidence = NULL,
        provenance = list(cache_status = NA_character_)
      ))
    }
    retrieve_target_disease_evidence(
      target_row,
      project_row,
      fetch = function(ensembl_gene_id, disease_id, db_pool = NULL) {
        expect_equal(disease_id, "MONDO_0005233")
        expect_true(grepl("^ENSG", ensembl_gene_id))
        payload <- payloads[[ensembl_gene_id]]
        if (is.null(payload)) {
          stop("unexpected ensembl id ", ensembl_gene_id)
        }
        cache_status <- if (ensembl_gene_id %in% stale) "stale" else "live"
        out <- ot_api_result(
          payload,
          cache_status = cache_status,
          from_cache = ensembl_gene_id %in% stale
        )
        out$query_name <- "TargetWeaveTargetDiseaseEvidence"
        out
      }
    )
  }
}

standard_payloads <- function() {
  list(
    ENSG00000146648 = read_fixture("ot_association_egfr_nsclc.json"),
    ENSG00000133703 = kras_payload(),
    ENSG00000141510 = tp53_payload()
  )
}

nsclc_targets <- function(include_unresolved = FALSE) {
  rows <- bind_targets(
    confirmed_ot_target("ENSG00000146648", "EGFR", id = "t-egfr"),
    confirmed_ot_target("ENSG00000133703", "KRAS", id = "t-kras"),
    confirmed_ot_target("ENSG00000140443", "MET", id = "t-met"),
    confirmed_ot_target("ENSG00000141510", "TP53", id = "t-tp53")
  )
  if (isTRUE(include_unresolved)) {
    rows <- bind_targets(
      rows,
      confirmed_ot_target("ENSG00000000000", "ALK", status = "unresolved", id = "t-alk")
    )
  }
  rows
}

test_that("comparison is blocked without a confirmed disease", {
  targets <- bind_targets(
    confirmed_ot_target(),
    confirmed_ot_target("ENSG00000133703", "KRAS", id = "t-kras")
  )
  result <- retrieve_project_comparison(
    confirmed_ot_project(status = "unresolved"),
    targets,
    pair_retrieve = function(...) stop("must not retrieve")
  )
  expect_equal(result$status, "blocked_disease")
  expect_null(result$comparison)
  expect_false(should_retrieve_comparison(TRUE, confirmed_ot_project(status = "unresolved"), targets))
})

test_that("comparison is blocked with fewer than two confirmed targets", {
  one <- confirmed_ot_target()
  result <- retrieve_project_comparison(
    confirmed_ot_project(),
    one,
    pair_retrieve = function(...) stop("must not retrieve")
  )
  expect_equal(result$status, "blocked_targets")
  expect_null(result$comparison)
})

test_that("unresolved targets are excluded and confirmed targets are included", {
  targets <- nsclc_targets(include_unresolved = TRUE)
  queried <- character()
  result <- retrieve_project_comparison(
    confirmed_ot_project(),
    targets,
    pair_retrieve = function(target_row, project_row, db_pool = NULL) {
      queried <<- c(queried, as.character(target_row$ensembl_gene_id[[1]]))
      pair_fetch_for(standard_payloads(), errors = "ENSG00000140443")(
        target_row,
        project_row,
        db_pool
      )
    }
  )
  expect_equal(result$status, "ready")
  expect_false("ENSG00000000000" %in% queried)
  expect_equal(
    sort(queried),
    sort(c("ENSG00000146648", "ENSG00000133703", "ENSG00000140443", "ENSG00000141510"))
  )
  expect_equal(length(result$comparison$excluded_targets), 1)
  expect_equal(result$comparison$excluded_targets[[1]]$symbol, "ALK")
  expect_true(all(c("EGFR", "KRAS", "MET", "TP53") %in% result$comparison$targets$symbol))
  expect_true(all(grepl("^ENSG", result$comparison$targets$ensembl_gene_id)))
})

test_that("canonical Ensembl identifiers are used for comparison retrieval", {
  seen <- list()
  retrieve_project_comparison(
    confirmed_ot_project(),
    bind_targets(
      confirmed_ot_target("ENSG00000146648.15", "EGFR", id = "t-egfr"),
      confirmed_ot_target("ENSG00000133703.11", "KRAS", id = "t-kras")
    ),
    pair_retrieve = function(target_row, project_row, db_pool = NULL) {
      retrieve_target_disease_evidence(
        target_row,
        project_row,
        fetch = function(ensembl_gene_id, disease_id, db_pool = NULL) {
          seen <<- c(seen, list(ensembl_gene_id))
          payload <- if (ensembl_gene_id == "ENSG00000146648") {
            read_fixture("ot_association_egfr_nsclc.json")
          } else {
            kras_payload()
          }
          out <- ot_api_result(payload)
          out$query_name <- "TargetWeaveTargetDiseaseEvidence"
          out
        }
      )
    }
  )
  expect_equal(unlist(seen), c("ENSG00000146648", "ENSG00000133703"))
})

test_that("direct score matrix keeps zero and missing distinct and unions datatypes", {
  result <- retrieve_project_comparison(
    confirmed_ot_project(),
    bind_targets(
      confirmed_ot_target("ENSG00000146648", "EGFR", id = "t-egfr"),
      confirmed_ot_target("ENSG00000133703", "KRAS", id = "t-kras")
    ),
    pair_retrieve = pair_fetch_for(standard_payloads())
  )
  matrix <- result$comparison$datatype_matrix
  expect_true("known_drug" %in% matrix$datatype_id)
  expect_true("literature" %in% matrix$datatype_id)
  egfr_rna <- matrix[matrix$symbol == "EGFR" & matrix$datatype_id == "rna_expression", ]
  expect_equal(egfr_rna$score, 0)
  expect_false(egfr_rna$is_missing)
  egfr_animal <- matrix[matrix$symbol == "EGFR" & matrix$datatype_id == "animal_model", ]
  expect_true(is.na(egfr_animal$score))
  expect_true(egfr_animal$is_missing)
  kras_lit <- matrix[matrix$symbol == "KRAS" & matrix$datatype_id == "literature", ]
  expect_true(is.na(kras_lit$score))
  expect_true(kras_lit$is_missing)
  kras_rna <- matrix[matrix$symbol == "KRAS" & matrix$datatype_id == "rna_expression", ]
  expect_equal(kras_rna$score, 0)
  expect_false(kras_rna$is_missing)
  kras_known <- matrix[matrix$symbol == "KRAS" & matrix$datatype_id == "known_drug", ]
  expect_equal(kras_known$score, 0.55)
  egfr_known <- matrix[matrix$symbol == "EGFR" & matrix$datatype_id == "known_drug", ]
  expect_true(is.na(egfr_known$score))
  expect_true(egfr_known$is_missing)

  heat <- build_ot_comparison_heatmap_data(matrix)
  expect_equal(as.character(heat$cell_label[heat$symbol == "EGFR" & heat$datatype_id == "rna_expression"]), "0.00")
  expect_equal(as.character(heat$cell_label[heat$symbol == "EGFR" & heat$datatype_id == "animal_model"]), "\u2014")
  expect_equal(
    as.character(heat$fill_kind[heat$symbol == "EGFR" & heat$datatype_id == "animal_model"]),
    "missing"
  )
  expect_equal(tw_plot_colors$heatmap_stops, c("#EEF1F6", "#C7CFDD", "#909DB7", "#5E6F91", "#344563"))
  expect_false(identical(tw_heatmap_fill(1), tw_heatmap_fill(0.4)))
  expect_false(identical(tw_heatmap_fill(0.7), tw_heatmap_fill(0.9)))
  expect_equal(heatmap_label_colour(0, FALSE), tw_plot_colors$text)
  expect_equal(heatmap_label_colour(NA_real_, TRUE), tw_plot_colors$text)

  expect_equal(result$comparison$targets$overall_direct_score[result$comparison$targets$symbol == "EGFR"], 0.8525670184292347, tolerance = 1e-8)
  expect_equal(result$comparison$targets$overall_inclusive_score[result$comparison$targets$symbol == "EGFR"], 0.9218179690478745, tolerance = 1e-8)
  expect_equal(result$comparison$provenance$association_scope, "direct")
})

test_that("association cache keys are pair-specific Ensembl and disease ids", {
  egfr <- cache_key_opentargets_association("ENSG00000146648", "MONDO_0005233")
  kras <- cache_key_opentargets_association("ENSG00000133703", "MONDO_0005233")
  tp53 <- cache_key_opentargets_association("ENSG00000141510", "MONDO_0005233")
  met <- cache_key_opentargets_association("ENSG00000105976", "MONDO_0005233")
  expect_equal(egfr, "opentargets:association:ENSG00000146648:MONDO_0005233")
  expect_equal(kras, "opentargets:association:ENSG00000133703:MONDO_0005233")
  expect_equal(tp53, "opentargets:association:ENSG00000141510:MONDO_0005233")
  expect_equal(met, "opentargets:association:ENSG00000105976:MONDO_0005233")
  expect_equal(length(unique(c(egfr, kras, tp53, met))), 4)
})

test_that("inclusive scores are parsed independently for each target", {
  payloads <- list(
    ENSG00000146648 = clone_association_payload(
      "ENSG00000146648", "EGFR", 0.853, 0.11,
      list(list(id = "clinical", score = 0.9))
    ),
    ENSG00000133703 = clone_association_payload(
      "ENSG00000133703", "KRAS", 0.720, 0.22,
      list(list(id = "clinical", score = 0.4))
    ),
    ENSG00000141510 = clone_association_payload(
      "ENSG00000141510", "TP53", 0.610, 0.33,
      list(list(id = "clinical", score = 0.5))
    ),
    ENSG00000105976 = clone_association_payload(
      "ENSG00000105976", "MET", 0.510, 0.44,
      list(list(id = "clinical", score = 0.3))
    )
  )
  targets <- bind_targets(
    confirmed_ot_target("ENSG00000146648", "EGFR", id = "t-egfr"),
    confirmed_ot_target("ENSG00000133703", "KRAS", id = "t-kras"),
    confirmed_ot_target("ENSG00000141510", "TP53", id = "t-tp53"),
    confirmed_ot_target("ENSG00000105976", "MET", id = "t-met")
  )
  queried <- character()
  result <- retrieve_project_comparison(
    confirmed_ot_project(),
    targets,
    pair_retrieve = function(target_row, project_row, db_pool = NULL) {
      ensembl <- canonical_ensembl_gene_id(target_row$ensembl_gene_id[[1]])
      queried <<- c(queried, ensembl)
      retrieve_target_disease_evidence(
        target_row,
        project_row,
        fetch = function(ensembl_gene_id, disease_id, db_pool = NULL) {
          expect_equal(ensembl_gene_id, ensembl)
          expect_equal(disease_id, "MONDO_0005233")
          payload <- payloads[[ensembl_gene_id]]
          parsed <- parse_ot_target_disease_evidence(payload)
          expect_equal(parsed$target_id, ensembl_gene_id)
          expect_equal(parsed$direct$disease_id, "MONDO_0005233")
          expect_equal(parsed$inclusive$disease_id, "MONDO_0005233")
          out <- ot_api_result(payload)
          out$query_name <- "TargetWeaveTargetDiseaseEvidence"
          out
        }
      )
    }
  )
  expect_equal(queried, c("ENSG00000146648", "ENSG00000133703", "ENSG00000141510", "ENSG00000105976"))
  scores <- result$comparison$targets
  expect_equal(scores$overall_direct_score[scores$symbol == "EGFR"], 0.853)
  expect_equal(scores$overall_direct_score[scores$symbol == "KRAS"], 0.720)
  expect_equal(scores$overall_direct_score[scores$symbol == "TP53"], 0.610)
  expect_equal(scores$overall_direct_score[scores$symbol == "MET"], 0.510)
  expect_equal(scores$overall_inclusive_score[scores$symbol == "EGFR"], 0.11)
  expect_equal(scores$overall_inclusive_score[scores$symbol == "KRAS"], 0.22)
  expect_equal(scores$overall_inclusive_score[scores$symbol == "TP53"], 0.33)
  expect_equal(scores$overall_inclusive_score[scores$symbol == "MET"], 0.44)
  expect_equal(length(unique(scores$overall_inclusive_score)), 4)
})

test_that("identical inclusive scores across targets are kept and not recycled from one pair", {
  same_inclusive <- 0.922
  payloads <- list(
    ENSG00000146648 = clone_association_payload(
      "ENSG00000146648", "EGFR", 0.853, same_inclusive,
      list(list(id = "clinical", score = 0.9))
    ),
    ENSG00000133703 = clone_association_payload(
      "ENSG00000133703", "KRAS", 0.720, same_inclusive,
      list(list(id = "clinical", score = 0.4))
    ),
    ENSG00000141510 = clone_association_payload(
      "ENSG00000141510", "TP53", 0.610, same_inclusive,
      list(list(id = "clinical", score = 0.5))
    ),
    ENSG00000105976 = clone_association_payload(
      "ENSG00000105976", "MET", 0.510, same_inclusive,
      list(list(id = "clinical", score = 0.3))
    )
  )
  targets <- bind_targets(
    confirmed_ot_target("ENSG00000146648", "EGFR", id = "t-egfr"),
    confirmed_ot_target("ENSG00000133703", "KRAS", id = "t-kras"),
    confirmed_ot_target("ENSG00000141510", "TP53", id = "t-tp53"),
    confirmed_ot_target("ENSG00000105976", "MET", id = "t-met")
  )
  result <- retrieve_project_comparison(
    confirmed_ot_project(),
    targets,
    pair_retrieve = pair_fetch_for(payloads)
  )
  scores <- result$comparison$targets
  expect_equal(nrow(scores), 4)
  expect_true(all(abs(scores$overall_inclusive_score - same_inclusive) < 1e-12))
  expect_equal(scores$overall_direct_score[scores$symbol == "EGFR"], 0.853)
  expect_equal(scores$overall_direct_score[scores$symbol == "KRAS"], 0.720)
  expect_equal(scores$overall_direct_score[scores$symbol == "TP53"], 0.610)
  expect_equal(scores$overall_direct_score[scores$symbol == "MET"], 0.510)
  expect_equal(length(unique(scores$ensembl_gene_id)), 4)
})

test_that("overall-score comparison uses Open Targets direct scores and does not invent a ranking label", {
  skip_if_not_installed("ggplot2")
  targets <- data.frame(
    project_target_id = c("a", "b"),
    symbol = c("KRAS", "EGFR"),
    overall_direct_score = c(0.72, 0.85),
    stringsAsFactors = FALSE
  )
  plot <- plot_ot_overall_scores(targets)
  expect_true(inherits(plot, "ggplot"))
  expect_match(plot$labels$x, "Open Targets overall association score")
  expect_true(is.null(plot$labels$title) || !nzchar(as.character(plot$labels$title %||% "")))
  expect_false(grepl("best|recommend|rank", plot$labels$x, ignore.case = TRUE))
})

test_that("partial target failure and stale fallback leave other targets comparable", {
  result <- retrieve_project_comparison(
    confirmed_ot_project(),
    nsclc_targets(),
    pair_retrieve = pair_fetch_for(
      c(standard_payloads(), list(ENSG00000140443 = read_fixture("ot_association_egfr_nsclc.json"))),
      errors = "ENSG00000140443",
      stale = "ENSG00000141510"
    )
  )
  expect_equal(result$status, "ready")
  met <- result$comparison$targets[result$comparison$targets$symbol == "MET", ]
  expect_equal(met$retrieval_status, "error")
  expect_false(isTRUE(met$available))
  expect_true(is.na(met$overall_direct_score))
  expect_false("MET" %in% result$comparison$datatype_matrix$symbol)
  tp53 <- result$comparison$targets[result$comparison$targets$symbol == "TP53", ]
  expect_equal(tp53$retrieval_status, "stale")
  expect_equal(comparison_status_label(tp53$retrieval_status, tp53$cache_status), "Stale evidence")
  expect_true(all(c("EGFR", "KRAS", "TP53") %in% result$comparison$datatype_matrix$symbol))
  expect_equal(comparison_status_label("error"), "Evidence unavailable")
})

test_that("target selection include/exclude requires two confirmed targets", {
  ids <- c("t-egfr", "t-kras", "t-met")
  expect_equal(normalize_comparison_selection(c("t-egfr", "t-kras"), ids), c("t-egfr", "t-kras"))
  expect_equal(
    normalize_comparison_selection("t-egfr", ids, previous_ids = c("t-egfr", "t-kras")),
    c("t-egfr", "t-kras")
  )
  filtered <- filter_comparison_visual(
    list(
      targets = data.frame(project_target_id = ids, symbol = c("EGFR", "KRAS", "MET"), stringsAsFactors = FALSE),
      datatype_matrix = data.frame(
        project_target_id = c("t-egfr", "t-kras", "t-met"),
        datatype_id = c("clinical", "clinical", "clinical"),
        stringsAsFactors = FALSE
      )
    ),
    c("t-egfr", "t-kras")
  )
  expect_equal(filtered$targets_visible$symbol, c("EGFR", "KRAS"))
  expect_equal(nrow(filtered$datatype_matrix_visible), 2)
})

test_that("navigation alone does not retrieve comparison again", {
  targets <- bind_targets(
    confirmed_ot_target(),
    confirmed_ot_target("ENSG00000133703", "KRAS", id = "t-kras")
  )
  project <- confirmed_ot_project()
  sig <- comparison_signature(project, targets$id)
  expect_true(should_retrieve_comparison(TRUE, project, targets, NA_character_))
  expect_false(should_retrieve_comparison(TRUE, project, targets, sig))
  expect_false(should_retrieve_comparison(FALSE, project, targets, NA_character_))
  other <- confirmed_ot_project(id = "project-2")
  expect_true(should_retrieve_comparison(TRUE, other, targets, sig))
})

test_that("fresh M4 pair cache is reused and only missing pairs hit the network", {
  skip_if_not_installed("httr2")
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("cmp-%s@example.com", suffix), "correct-horse-battery")
  other <- register_user(db_pool, sprintf("cmp-b-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "Potential therapeutic targets in NSCLC",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS", "ALK")
  )
  confirm_project_disease(
    db_pool,
    created$project_id,
    owner$user$id,
    sprintf("TWTEST_%s", suffix),
    "non-small cell lung carcinoma"
  )
  listed <- list_project_targets(db_pool, created$project_id, owner$user$id)
  confirm_project_target(db_pool, listed$id[listed$input_text == "EGFR"], owner$user$id, "EGFR", "ENSG00000146648", "P00533", "HGNC:3236")
  confirm_project_target(db_pool, listed$id[listed$input_text == "KRAS"], owner$user$id, "KRAS", "ENSG00000133703", "P01116", "HGNC:6407")
  listed <- list_project_targets(db_pool, created$project_id, owner$user$id)
  project <- get_owned_project(db_pool, created$project_id, owner$user$id)
  disease_id <- project_disease_ontology_id(project)

  cache_put(
    db_pool,
    "opentargets",
    cache_key_opentargets_association("ENSG00000146648", disease_id),
    read_fixture("ot_association_egfr_nsclc.json"),
    200L,
    Sys.time(),
    Sys.time() + 7 * 86400
  )
  expect_equal(
    association_cache_freshness(db_pool, "ENSG00000146648", disease_id),
    "fresh"
  )
  expect_equal(
    association_cache_freshness(db_pool, "ENSG00000133703", disease_id),
    "missing"
  )

  fetched <- character()
  pair_retrieve <- function(target_row, project_row, db_pool = NULL) {
    retrieve_target_disease_evidence(
      target_row,
      project_row,
      db_pool = db_pool,
      fetch = function(ensembl_gene_id, disease_id, db_pool = NULL) {
        ot_target_disease_evidence(
          ensembl_gene_id,
          disease_id,
          db_pool = db_pool,
          perform = function(req) {
            fetched <<- c(fetched, ensembl_gene_id)
            payload <- if (identical(ensembl_gene_id, "ENSG00000133703")) kras_payload() else read_fixture("ot_association_egfr_nsclc.json")
            httr2::response(
              status_code = 200,
              url = OT_GRAPHQL_URL,
              method = "POST",
              headers = list("Content-Type" = "application/json"),
              body = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))
            )
          }
        )
      }
    )
  }

  first <- retrieve_project_comparison(project, listed, db_pool = db_pool, pair_retrieve = pair_retrieve)
  expect_equal(first$status, "ready")
  expect_equal(fetched, "ENSG00000133703")
  expect_equal(first$comparison$targets$retrieval_status[first$comparison$targets$symbol == "EGFR"], "cached")
  expect_equal(length(first$comparison$excluded_targets), 1)
  expect_equal(first$comparison$excluded_targets[[1]]$input_text, "ALK")

  fetched <- character()
  second <- retrieve_project_comparison(project, listed, db_pool = db_pool, pair_retrieve = pair_retrieve)
  expect_equal(fetched, character())
  expect_true(all(second$comparison$targets$retrieval_status[second$comparison$targets$available] %in% c("cached")))

  expect_true(is.null(get_owned_project(db_pool, created$project_id, other$user$id)))
  expect_equal(nrow(list_project_targets(db_pool, created$project_id, other$user$id)), 0)
})

test_that("comparison UI copy distinguishes scores from recommendations", {
  skip_if_not_installed("shiny")
  library(shiny)
  source_app("R/modules/mod_disease_evidence.R")
  source_app("R/modules/mod_ot_comparison.R")

  packed <- retrieve_project_comparison(
    confirmed_ot_project(),
    bind_targets(
      confirmed_ot_target("ENSG00000146648", "EGFR", id = "t-egfr"),
      confirmed_ot_target("ENSG00000133703", "KRAS", id = "t-kras")
    ),
    pair_retrieve = pair_fetch_for(standard_payloads())
  )
  html <- as.character(comparison_result_ui(
    packed,
    NS("compare"),
    packed$comparison$targets$project_target_id
  ))
  expect_match(html, "should not be interpreted as probabilities")
  expect_equal(length(gregexpr("should not be interpreted as probabilities", html)[[1]]), 1)
  expect_match(html, "Open Targets overall association score")
  expect_match(html, "Inspect EGFR")
  expect_false(grepl("TargetWeave score|Recommended target|Best target", html))
  expect_match(html, "Technical provenance")
  expect_false(grepl("View overview", html))
  expect_equal(workspace_panel_label("compare"), "Compare evidence")
  expect_equal(workspace_back_destination("compare"), "project")
  expect_equal(workspace_back_label("compare"), "Back to project")
})
