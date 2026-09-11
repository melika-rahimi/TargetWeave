skip_if_not_installed("shiny")
library(shiny)
source_app("R/modules/mod_target_resolver.R")

json_roundtrip <- function(x) {
  jsonlite::fromJSON(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null"),
    simplifyVector = FALSE
  )
}

sparse_candidate <- function(symbol = "EGFR") {
  candidate <- new_identity_candidate(
    input_text = symbol,
    display_symbol = NA_character_,
    name = NA_character_,
    organism = NA_character_,
    ensembl_gene_id = NA_character_,
    seq_region = NA_character_,
    start = NA_integer_,
    end = NA_integer_,
    strand = NA_integer_,
    biotype = NA_character_,
    uniprot_accession = NA_character_,
    uniprot_reviewed = FALSE,
    hgnc_id = NA_character_,
    cross_check = NA_character_,
    warning = NA_character_
  )
  candidate$uniprot_reviewed <- NA
  candidate
}

test_that("the crashing if() is is.na()+nzchar() on JSON-null candidate fields", {
  # In-memory NA is safe with `!is.na(x) && nzchar(x)` because of short-circuit.
  x_na <- NA_character_
  expect_false(!is.na(x_na) && nzchar(x_na))

  # After JSONB round-trip, NA_character_ becomes JSON null then R NULL.
  roundtripped <- json_roundtrip(list(biotype = NA_character_))
  expect_null(roundtripped$biotype)

  # This is the condition previously used in candidate_card_ui() for
  # biotype, location_label, ensembl_gene_id, uniprot_accession, and hgnc_id.
  # On this R, `if (logical(0))` raises the live Shiny error string.
  expect_error(
    if (!is.na(roundtripped$biotype) && nzchar(roundtripped$biotype)) TRUE,
    "missing value where TRUE/FALSE needed"
  )
})

test_that("has_display_text never returns NA for API/JSON empties", {
  expect_false(has_display_text(NULL))
  expect_false(has_display_text(NA_character_))
  expect_false(has_display_text(""))
  expect_false(has_display_text(character(0)))
  expect_false(has_display_text(NA))
  expect_true(has_display_text("EGFR"))
})

test_that("format_location does not treat NULL/NA coords as TRUE/FALSE", {
  expect_true(is.na(format_location(NA_character_, 1L, 2L, 1L)))
  expect_true(is.na(format_location(NULL, NULL, NULL, NULL)))
  expect_equal(format_location("7", NULL, NULL, NA_integer_), "chr7")
  expect_equal(format_location("7", integer(0), integer(0), integer(0)), "chr7")
})

test_that("candidate_card_ui renders JSON-null fields instead of crashing", {
  stored <- json_roundtrip(sparse_candidate("EGFR"))
  expect_null(stored$biotype)
  expect_null(stored$location_label)
  expect_null(stored$ensembl_gene_id)
  expect_null(stored$warning)

  ui <- candidate_card_ui(stored, NS("resolver"))
  html <- as.character(ui)
  expect_match(html, "Biotype not provided")
  expect_match(html, "Ensembl ID not provided")
  expect_match(html, "UniProt accession not provided")
  expect_match(html, "Review status not provided")
})

test_that("EGFR/KRAS/MET/TP53 payload round-trips never crash resolver UI", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))
  ensembl <- parse_ensembl_gene(read_fixture("ensembl_egfr.json"))
  egfr <- normalize_resolution_result(
    input_text = "EGFR",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(ensembl)
    )
  )

  met <- normalize_resolution_result(
    input_text = "MET",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = parse_uniprot_search(read_fixture("uniprot_ambiguous.json"))$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(
        parse_ensembl_gene(read_fixture("ensembl_met.json")),
        parse_ensembl_gene(read_fixture("ensembl_metrnl.json"))
      )
    )
  )

  ns <- NS("resolver")
  expect_silent(resolver_results_ui(json_roundtrip(egfr), ns))
  expect_silent(resolver_results_ui(json_roundtrip(met), ns))
  expect_match(as.character(resolver_results_ui(json_roundtrip(met), ns)), "Confirm")


  for (symbol in c("EGFR", "KRAS", "MET", "TP53")) {
    stored <- json_roundtrip(sparse_candidate(symbol))
    expect_silent(candidate_card_ui(stored, ns))
    expect_silent(resolver_results_ui(
      list(
        status = "unresolved",
        candidates = list(stored),
        source_reports = list(
          uniprot = json_roundtrip(source_report(new_api_result(TRUE, source = "uniprot"))),
          ensembl = json_roundtrip(source_report(new_api_result(
            TRUE,
            source = "ensembl",
            error = NA_character_
          )))
        )
      ),
      ns
    ))
  }
})

test_that("incompatible stored payload is invalidated instead of crashing", {
  expect_null(decode_resolution_payload(NA_character_))
  expect_null(decode_resolution_payload(NULL))
  expect_null(decode_resolution_payload("{not json"))
  expect_null(decode_resolution_payload('{"candidates":[]}'))
  expect_equal(
    decode_resolution_payload('{"status":"unresolved","candidates":[]}')$status,
    "unresolved"
  )
})

test_that("NULL gene symbols do not crash candidate pairing", {
  entry <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))$entries[[1]]
  entry$display_symbol <- NULL
  gene <- parse_ensembl_gene(read_fixture("ensembl_egfr.json"))
  gene$display_symbol <- NULL

  expect_silent(
    build_candidates(
      input_text = "EGFR",
      uniprot_entries = list(entry),
      ensembl_genes = list(gene),
      uniprot_ok = TRUE,
      ensembl_ok = TRUE
    )
  )
})
