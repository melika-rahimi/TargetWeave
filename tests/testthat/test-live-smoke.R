test_that("identity assessment separates source outages from semantic mismatch", {
  matched <- list(
    candidates = list(list(
      uniprot_accession = "P00533",
      ensembl_gene_id = "ENSG00000146648",
      cross_check = "matched"
    )),
    source_reports = list(
      uniprot = list(ok = TRUE, status = 200L, error = NULL),
      ensembl = list(ok = TRUE, status = 200L, error = NULL)
    )
  )
  outage <- matched
  outage$source_reports$ensembl$ok <- FALSE
  outage$source_reports$ensembl$status <- 503L
  outage$candidates[[1]]$cross_check <- "partial_uniprot"
  mismatch <- matched
  mismatch$candidates[[1]]$cross_check <- "mismatch"
  recovered <- matched
  recovered$source_reports$ensembl$ok <- FALSE
  recovered$source_reports$ensembl$status <- 503L

  ok <- identity_live_assessment(matched)
  expect_equal(ok$availability, "sources_available")
  expect_equal(ok$semantic, "matched")
  expect_equal(ok$uniprot_accession, "P00533")
  expect_equal(ok$uniprot_ensembl_gene_id, "ENSG00000146648")
  expect_equal(ok$canonical_ensg, "ENSG00000146648")
  expect_equal(ok$ensembl_ensg, "ENSG00000146648")

  down <- identity_live_assessment(outage)
  expect_equal(down$availability, "source_unavailable")
  expect_true("Ensembl" %in% down$unavailable_sources)
  expect_true(is.na(down$semantic))
  expect_equal(down$cross_check, "partial_uniprot")
  expect_true(is.na(down$ensembl_ensg))

  recovered_ok <- identity_live_assessment(recovered)
  expect_equal(recovered_ok$availability, "sources_available")
  expect_equal(recovered_ok$semantic, "matched")

  bad <- identity_live_assessment(mismatch)
  expect_equal(bad$availability, "sources_available")
  expect_equal(bad$semantic, "mismatch")
})

canonical_live_identities <- list(
  EGFR = list(uniprot = "P00533", ensg = "ENSG00000146648"),
  KRAS = list(uniprot = "P01116", ensg = "ENSG00000133703"),
  TP53 = list(uniprot = "P04637", ensg = "ENSG00000141510"),
  MET = list(uniprot = "P08581", ensg = "ENSG00000105976")
)

report_identity_assessment <- function(symbol, assessment) {
  paste(
    symbol,
    "availability=", assessment$availability,
    "semantic=", assessment$semantic %||% "NA",
    "uniprot=", assessment$uniprot_accession %||% "NA",
    "uniprot_ensg=", assessment$uniprot_ensembl_gene_id %||% "NA",
    "canonical_ensg=", assessment$canonical_ensg %||% "NA",
    "ensembl_ensg=", assessment$ensembl_ensg %||% "NA",
    "cross_check=", assessment$cross_check %||% "NA",
    "uniprot_http=", assessment$uniprot_http_ok,
    "ensembl_http=", assessment$ensembl_http_ok,
    "uniprot_status=", assessment$uniprot_status %||% "NA",
    "ensembl_status=", assessment$ensembl_status %||% "NA",
    sep = " "
  )
}

test_that("optional live UniProt+Ensembl lookup for EGFR", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  result <- resolve_target_identity("EGFR", db_pool = NULL)
  assessment <- identity_live_assessment(result)
  message(report_identity_assessment("EGFR", assessment))
  if (identical(assessment$availability, "source_unavailable")) {
    skip(paste("EGFR source unavailable:", paste(assessment$unavailable_sources, collapse = ",")))
  }
  expect_equal(assessment$semantic, "matched")
  expect_true(length(result$candidates) >= 1)
  expect_true(
    any(vapply(result$candidates, function(c) {
      identical(c$uniprot_accession, "P00533") &&
        identical(canonical_ensembl_gene_id(c$ensembl_gene_id), "ENSG00000146648")
    }, logical(1)))
  )
})

test_that("live or cached resolve of EGFR/KRAS/MET/TP53 never errors", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")
  skip_if_not_installed("shiny")
  library(shiny)
  source_app("R/modules/mod_target_resolver.R")

  ns <- NS("resolver")
  for (symbol in c("EGFR", "KRAS", "MET", "TP53")) {
    result <- resolve_target_identity(symbol, db_pool = NULL)
    expect_false(identical(result$status, NULL), info = symbol)
    expect_silent(resolver_results_ui(result, ns))
    stored <- jsonlite::fromJSON(
      jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", na = "null"),
      simplifyVector = FALSE
    )
    expect_silent(resolver_results_ui(stored, ns))
  }
})

test_that("live MET keeps SLTM as a lower-ranked synonym without auto-confirm", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")
  skip_if_not_installed("shiny")
  library(shiny)
  source_app("R/modules/mod_target_resolver.R")

  result <- resolve_target_identity("MET", db_pool = NULL)
  assessment <- identity_live_assessment(result)
  message(report_identity_assessment("MET", assessment))
  if (!isTRUE(result$source_reports$uniprot$ok)) {
    skip(paste("MET UniProt source unavailable:", assessment$uniprot_error %||% "UniProt"))
  }
  expect_equal(result$status, "unresolved")
  expect_false(identical(result$status, "confirmed"))
  expect_true(length(result$candidates) >= 2)
  expect_equal(result$candidates[[1]]$display_symbol, "MET")
  expect_equal(result$candidates[[1]]$match_type, "exact_current_symbol")
  sltm <- Filter(function(c) identical(c$display_symbol, "SLTM"), result$candidates)
  expect_equal(length(sltm), 1)
  expect_equal(sltm[[1]]$match_type, "synonym_or_alias")
  expect_equal(sltm[[1]]$matched_alias, "MET")
  expect_gt(sltm[[1]]$match_rank, result$candidates[[1]]$match_rank)

  html <- as.character(resolver_results_ui(result, NS("resolver")))
  expect_match(html, "Primary match")
  expect_match(html, "Other possible match")
  expect_match(html, "Exact current gene symbol match")
  expect_match(html, "Matched through synonym / alias")
  expect_match(html, "Confirm")
})

test_that("optional live overview for confirmed EGFR P00533 / ENSG00000146648", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  row <- list(
    id = "live-egfr",
    resolution_status = "confirmed",
    display_symbol = "EGFR",
    ensembl_gene_id = "ENSG00000146648",
    uniprot_accession = "P00533",
    hgnc_id = "HGNC:3236",
    input_text = "EGFR"
  )
  result <- retrieve_target_overview(row, db_pool = NULL)
  overview <- result$overview
  uni_ok <- isTRUE(overview$provenance$uniprot$ok)
  ens_ok <- isTRUE(overview$provenance$ensembl$ok)

  expect_true(result$status %in% c("ready", "partial", "unavailable"))
  expect_equal(overview$identity$uniprot_accession, "P00533")
  expect_equal(overview$identity$ensembl_gene_id, "ENSG00000146648")
  if (!uni_ok) {
    skip(paste("EGFR overview UniProt unavailable:", overview$provenance$uniprot$error %||% "UniProt"))
  }
  expect_true(is_present_scalar(overview$protein$length_aa))
  expect_true(has_display_text(overview$protein$function_full))
  if (!ens_ok) {
    skip(paste("EGFR overview Ensembl unavailable:", overview$provenance$ensembl$error %||% "Ensembl"))
  }
  expect_equal(overview$genomic$chromosome, "7")
  expect_true(is_present_scalar(overview$genomic$start))
  expect_true(is_present_scalar(overview$genomic$end))
})

test_that("optional live Open Targets disease resolve and EGFR association", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  resolved <- resolve_project_disease("Non-small-cell lung cancer", db_pool = NULL)
  expect_true(length(resolved$candidates) >= 1)
  nsclc <- Filter(function(c) identical(c$id, "MONDO_0005233"), resolved$candidates)
  expect_equal(length(nsclc), 1, info = "Live Open Targets should still return MONDO_0005233 for NSCLC wording")

  row <- list(
    id = "live-egfr",
    resolution_status = "confirmed",
    display_symbol = "EGFR",
    ensembl_gene_id = "ENSG00000146648",
    uniprot_accession = "P00533",
    input_text = "EGFR"
  )
  project <- list(
    id = "live-project",
    disease_label = "Non-small-cell lung cancer",
    disease_ontology_id = nsclc[[1]]$id,
    disease_name = nsclc[[1]]$name,
    disease_resolution_status = "confirmed"
  )
  result <- retrieve_target_disease_evidence(row, project, db_pool = NULL)
  expect_true(result$status %in% c("live", "cached", "empty"))
  expect_equal(result$evidence$disease$id, nsclc[[1]]$id)
  expect_equal(result$evidence$target$ensembl_gene_id, "ENSG00000146648")
  expect_equal(result$evidence$association$association_scope, "direct")
  expect_true(is.numeric(result$evidence$association$overall_score_direct))
})

test_that("optional live comparison for EGFR and KRAS in NSCLC", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  project <- list(
    id = "live-compare",
    disease_label = "Non-small-cell lung cancer",
    disease_ontology_id = "MONDO_0005233",
    disease_name = "non-small cell lung carcinoma",
    disease_resolution_status = "confirmed"
  )
  targets <- rbind(
    data.frame(
      id = "live-egfr",
      resolution_status = "confirmed",
      display_symbol = "EGFR",
      ensembl_gene_id = "ENSG00000146648",
      uniprot_accession = "P00533",
      input_text = "EGFR",
      stringsAsFactors = FALSE
    ),
    data.frame(
      id = "live-kras",
      resolution_status = "confirmed",
      display_symbol = "KRAS",
      ensembl_gene_id = "ENSG00000133703",
      uniprot_accession = "P01116",
      input_text = "KRAS",
      stringsAsFactors = FALSE
    )
  )
  result <- retrieve_project_comparison(project, targets, db_pool = NULL)
  expect_equal(result$status, "ready")
  expect_true(all(result$comparison$targets$ensembl_gene_id %in% c("ENSG00000146648", "ENSG00000133703")))
  expect_equal(result$comparison$disease$id, "MONDO_0005233")
  expect_equal(result$comparison$provenance$association_scope, "direct")
  expect_true(nrow(result$comparison$datatype_matrix) > 0)
})

test_that("live EGFR, KRAS, and TP53 semantic cross-check when both sources are available", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  availability <- list()
  for (symbol in c("EGFR", "KRAS", "TP53")) {
    result <- resolve_target_identity(symbol, db_pool = NULL)
    assessment <- identity_live_assessment(result)
    message(report_identity_assessment(symbol, assessment))
    availability[[symbol]] <- assessment
    if (identical(assessment$availability, "source_unavailable")) {
      next
    }
    expect_equal(assessment$semantic, "matched", info = report_identity_assessment(symbol, assessment))
    expect_equal(length(result$candidates), 1, info = symbol)
    expect_equal(result$candidates[[1]]$match_type, "exact_current_symbol", info = symbol)
    expect_true(candidate_is_confirmable(result$candidates[[1]]), info = symbol)
    expect_equal(assessment$uniprot_accession, canonical_live_identities[[symbol]]$uniprot, info = symbol)
    expect_equal(assessment$canonical_ensg, canonical_live_identities[[symbol]]$ensg, info = symbol)
    expect_equal(assessment$ensembl_ensg, canonical_live_identities[[symbol]]$ensg, info = symbol)
    expect_equal(assessment$cross_check, "matched", info = symbol)
  }
  if (all(vapply(availability, function(a) identical(a$availability, "source_unavailable"), logical(1)))) {
    skip(paste(
      "Source unavailable for EGFR/KRAS/TP53:",
      paste(unique(unlist(lapply(availability, function(a) a$unavailable_sources))), collapse = ",")
    ))
  }
})

test_that("live identity report for EGFR KRAS TP53 MET", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  for (symbol in names(canonical_live_identities)) {
    result <- resolve_target_identity(symbol, db_pool = NULL)
    assessment <- identity_live_assessment(result)
    message(report_identity_assessment(symbol, assessment))
    expect_true(assessment$availability %in% c("sources_available", "source_unavailable"), info = symbol)
    if (identical(assessment$availability, "sources_available") && symbol != "MET") {
      expect_equal(assessment$semantic, "matched", info = report_identity_assessment(symbol, assessment))
    }
    if (identical(assessment$availability, "sources_available") && identical(symbol, "MET")) {
      expect_true(
        any(vapply(result$candidates, function(c) {
          identical(c$uniprot_accession, canonical_live_identities$MET$uniprot) &&
            identical(canonical_ensembl_gene_id(c$ensembl_gene_id), canonical_live_identities$MET$ensg) &&
            identical(c$cross_check, "matched")
        }, logical(1))),
        info = "MET should include P08581 / ENSG00000105976 when both sources are available"
      )
    }
  }
})

test_that("optional live Reactome UniProt mapping for EGFR P00533", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  version <- fetch_reactome_version(db_pool = NULL)
  expect_true(version$ok)
  expect_match(version$version, "^[0-9]+")

  result <- retrieve_target_reactome_membership(
    confirmed_reactome_target("live-egfr", "EGFR", "P00533", "ENSG00000146648"),
    db_pool = NULL,
    reactome_release = version$version
  )
  expect_true(result$status %in% c("ok", "empty"))
  expect_equal(result$membership$uniprot_accession, "P00533")
  if (identical(result$status, "ok")) {
    expect_true(all(grepl("^R-HSA-", result$membership$pathways$pathway_id)))
    expect_true(all(result$membership$pathways$species == "Homo sapiens"))
    expect_true(all(result$membership$pathways$membership_scope == "lowest_level"))
  }
})

test_that("optional live PubMed literature for EGFR P00533 in NSCLC", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("live-egfr", "EGFR", "P00533", "ENSG00000146648")
  mapped <- resolve_literature_gene_id(target, db_pool = NULL)
  expect_true(mapped$ok)
  expect_equal(mapped$gene_id, "1956")
  expect_equal(mapped$gene$taxid, 9606L)

  result <- retrieve_target_literature(
    target,
    literature_disease_context(project),
    db_pool = NULL
  )
  expect_true(result$status %in% c("ok", "empty"))
  expect_true(is.finite(result$literature$corpus$total_count))
  expect_match(result$literature$corpus$disease_query, "Title/Abstract", fixed = TRUE)
  if (identical(result$status, "ok")) {
    expect_true(nrow(result$literature$recent_records) >= 1)
    expect_true(all(grepl("^[0-9]+$", result$literature$recent_records$pmid)))
  }
  expect_true(any(result$literature$trend$status == "ok"))
  expect_true(any(result$literature$trend$is_partial_year))
})

test_that("optional live RCSB experimental structures for EGFR P00533", {
  skip_if_not(identical(Sys.getenv("TW_LIVE_API"), "1"), "Set TW_LIVE_API=1 for live smoke")
  skip_if_not_installed("httr2")

  search <- fetch_rcsb_uniprot_search("P00533", db_pool = NULL)
  expect_true(search$ok)
  parsed <- parse_rcsb_search_entities(search$data)
  expect_true(parsed$ok)
  expect_true(length(parsed$entities) >= 1)
  ids <- vapply(parsed$entities, `[[`, character(1), "identifier")
  pdb <- vapply(parsed$entities, `[[`, character(1), "pdb_id")
  expect_false(any(grepl("^(AF_|MA_)", ids)))
  expect_true(all(vapply(pdb, is_experimental_pdb_id, logical(1))))

  sample_id <- parsed$entities[[1]]$identifier
  meta <- fetch_rcsb_entity_metadata(sample_id, db_pool = NULL)
  expect_true(meta$ok)
  parsed_meta <- parse_rcsb_entity_metadata(meta$data)
  expect_true(parsed_meta$ok)
  rec <- parsed_meta$records[[sample_id]]
  expect_false(is.null(rec))
  expect_true(has_display_text(rec$experiment$method))
  expect_false(identical(rec$experiment$resolution_angstrom, "NA"))
  expect_false(identical(rec$experiment$resolution_angstrom, "NULL"))

  alignments <- fetch_rcsb_uniprot_alignments("P00533", db_pool = NULL)
  expect_true(alignments$ok)
  parsed_al <- parse_rcsb_alignments(alignments$data)
  expect_true(parsed_al$ok)
  expect_true(is.finite(parsed_al$uniprot_length))
  mapped <- parsed_al$alignments[[sample_id]]
  if (is.null(mapped)) {
    mapped <- parsed_al$alignments[[intersect(names(parsed_al$alignments), ids)[[1]]]]
  }
  expect_false(is.null(mapped))
  coverage <- normalize_entity_coverage(mapped, parsed_al$uniprot_length)
  expect_true(is.finite(coverage$coverage_fraction))
  expect_gte(coverage$coverage_fraction, 0)
  expect_lte(coverage$coverage_fraction, 1)
})

