test_that("a unique UniProt+Ensembl pair becomes one confirmable candidate", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))
  ensembl <- parse_ensembl_gene(read_fixture("ensembl_egfr.json"))

  result <- normalize_resolution_result(
    input_text = "EGFR",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, data = NULL, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, data = NULL, source = "ensembl"),
      genes = list(ensembl)
    )
  )

  expect_equal(result$status, "unresolved")
  expect_equal(length(result$candidates), 1)
  expect_equal(result$candidates[[1]]$uniprot_accession, "P00533")
  expect_equal(result$candidates[[1]]$ensembl_gene_id, "ENSG00000146648")
  expect_equal(result$candidates[[1]]$cross_check, "matched")
  expect_equal(result$candidates[[1]]$match_type, "exact_current_symbol")
  expect_equal(result$candidates[[1]]$match_rank, 2L)
  expect_true(candidate_is_confirmable(result$candidates[[1]]))
  expect_false(identical(result$status, "confirmed"))
})

test_that("MET ranks the current symbol above a UniProt synonym match for SLTM", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_met_sltm.json"))
  expect_equal(uniprot$entries[[2]]$display_symbol, "SLTM")
  expect_equal(uniprot$entries[[2]]$gene_synonyms, "MET")

  result <- normalize_resolution_result(
    input_text = "MET",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(
        parse_ensembl_gene(read_fixture("ensembl_met.json")),
        parse_ensembl_gene(read_fixture("ensembl_sltm.json"))
      )
    )
  )

  expect_equal(result$status, "unresolved")
  expect_false(identical(result$status, "confirmed"))
  expect_false(identical(result$status, "ambiguous"))
  expect_equal(length(result$candidates), 2)
  expect_equal(result$candidates[[1]]$display_symbol, "MET")
  expect_equal(result$candidates[[1]]$uniprot_accession, "P08581")
  expect_equal(result$candidates[[1]]$ensembl_gene_id, "ENSG00000105976")
  expect_equal(result$candidates[[1]]$match_type, "exact_current_symbol")
  expect_equal(result$candidates[[1]]$match_rank, 2L)
  expect_equal(result$candidates[[2]]$display_symbol, "SLTM")
  expect_equal(result$candidates[[2]]$uniprot_accession, "Q9NWH9")
  expect_equal(result$candidates[[2]]$match_type, "synonym_or_alias")
  expect_equal(result$candidates[[2]]$matched_alias, "MET")
  expect_equal(result$candidates[[2]]$match_rank, 3L)
  expect_gt(result$candidates[[2]]$match_rank, result$candidates[[1]]$match_rank)
  expect_true(candidate_is_confirmable(result$candidates[[1]]))
})

test_that("a documented-synonym gap is other_valid_match, not an invented alias", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_ambiguous.json"))
  genes <- list(
    parse_ensembl_gene(read_fixture("ensembl_met.json")),
    parse_ensembl_gene(read_fixture("ensembl_metrnl.json"))
  )

  result <- normalize_resolution_result(
    input_text = "MET",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = genes
    )
  )

  expect_equal(result$status, "unresolved")
  expect_equal(result$candidates[[1]]$display_symbol, "MET")
  expect_equal(result$candidates[[1]]$match_type, "exact_current_symbol")
  metrnl <- result$candidates[[2]]
  expect_equal(metrnl$display_symbol, "METRNL")
  expect_equal(metrnl$match_type, "other_valid_match")
  expect_false(identical(metrnl$match_type, "synonym_or_alias"))
})

test_that("equal-quality current-symbol matches remain ambiguous", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_met_sltm.json"))
  first <- uniprot$entries[[1]]
  second <- first
  second$accession <- "P99999"
  second$ensembl_gene_ids <- "ENSG00000199999"

  extra_gene <- parse_ensembl_gene(read_fixture("ensembl_met.json"))
  extra_gene$ensembl_gene_id <- "ENSG00000199999"

  result <- normalize_resolution_result(
    input_text = "MET",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = list(first, second)
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(
        parse_ensembl_gene(read_fixture("ensembl_met.json")),
        extra_gene
      )
    )
  )

  expect_equal(result$status, "ambiguous")
  expect_equal(length(result$candidates), 2)
  expect_true(all(vapply(result$candidates, function(c) {
    identical(c$match_type, "exact_current_symbol")
  }, logical(1))))
})

test_that("exact Ensembl and UniProt identifier inputs outrank symbol logic", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))
  ensembl <- parse_ensembl_gene(read_fixture("ensembl_egfr.json"))

  by_ensg <- normalize_resolution_result(
    input_text = "ENSG00000146648.22",
    input_class = "ensembl_gene",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(ensembl)
    )
  )
  expect_equal(by_ensg$status, "unresolved")
  expect_equal(by_ensg$candidates[[1]]$match_type, "identifier_exact")
  expect_equal(by_ensg$candidates[[1]]$match_rank, 1L)
  expect_equal(by_ensg$candidates[[1]]$uniprot_accession, "P00533")

  by_acc <- normalize_resolution_result(
    input_text = "P00533",
    input_class = "uniprot_accession",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(ensembl)
    )
  )
  expect_equal(by_acc$status, "unresolved")
  expect_equal(by_acc$candidates[[1]]$match_type, "identifier_exact")
  expect_equal(by_acc$candidates[[1]]$ensembl_gene_id, "ENSG00000146648")
})

test_that("empty sources produce a failed lookup with no fabricated ids", {
  result <- normalize_resolution_result(
    input_text = "NOTAGENE",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = list()
    ),
    ensembl_fetch = list(
      result = new_api_result(FALSE, status = 400, error = "missing", source = "ensembl"),
      genes = list()
    )
  )

  expect_equal(result$status, "failed")
  expect_equal(length(result$candidates), 0)
})

test_that("identifier mismatch is not silently merged", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))
  other <- parse_ensembl_gene(read_fixture("ensembl_met.json"))

  result <- normalize_resolution_result(
    input_text = "EGFR",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(other)
    )
  )

  expect_true(length(result$candidates) >= 1)
  cross <- vapply(result$candidates, function(c) c$cross_check, character(1))
  expect_false(any(cross == "matched"))
  expect_true(any(cross %in% c("mismatch", "partial_ensembl", "partial_uniprot")))
})

test_that("UniProt success with Ensembl failure is partial, not fully cross-checked", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))

  result <- normalize_resolution_result(
    input_text = "EGFR",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(FALSE, status = 503, error = "unavailable", source = "ensembl"),
      genes = list()
    )
  )

  expect_equal(length(result$candidates), 1)
  expect_equal(result$candidates[[1]]$cross_check, "partial_uniprot")
  expect_false(is.null(result$candidates[[1]]$warning))
})

test_that("MET resolver UI labels a primary symbol match and a synonym without auto-confirm", {
  skip_if_not_installed("shiny")
  library(shiny)
  source_app("R/modules/mod_target_resolver.R")

  result <- normalize_resolution_result(
    input_text = "MET",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = parse_uniprot_search(read_fixture("uniprot_met_sltm.json"))$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(
        parse_ensembl_gene(read_fixture("ensembl_met.json")),
        parse_ensembl_gene(read_fixture("ensembl_sltm.json"))
      )
    )
  )

  html <- as.character(resolver_results_ui(result, NS("resolver")))
  expect_match(html, "Primary match")
  expect_match(html, "Other possible match")
  expect_match(html, "Exact current gene symbol match")
  expect_match(html, "Matched through synonym / alias")
  expect_match(html, ">MET<")
  expect_match(html, "SLTM")
  expect_match(html, "Confirm")
  expect_false(grepl("equally likely", html, ignore.case = TRUE))
  expect_equal(result$status, "unresolved")
})
