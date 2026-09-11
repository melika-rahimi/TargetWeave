skip_if_not_installed("shiny")
library(shiny)
source_app("R/modules/mod_overview.R")
source_app("R/modules/mod_target_resolver.R")

json_roundtrip <- function(x) {
  jsonlite::fromJSON(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null"),
    simplifyVector = FALSE
  )
}

confirmed_row <- function(
  symbol = "EGFR",
  ensembl_gene_id = "ENSG00000146648",
  uniprot_accession = "P00533",
  hgnc_id = "HGNC:3236",
  status = "confirmed"
) {
  list(
    id = "target-egfr",
    resolution_status = status,
    display_symbol = symbol,
    ensembl_gene_id = ensembl_gene_id,
    uniprot_accession = uniprot_accession,
    hgnc_id = hgnc_id,
    input_text = symbol
  )
}

fixture_result <- function(payload, source, cache_status = "live", from_cache = FALSE) {
  new_api_result(
    ok = TRUE,
    status = 200,
    data = payload,
    error = NULL,
    retrieved_at = as.POSIXct("2026-09-10 08:00:00", tz = "UTC"),
    from_cache = from_cache,
    cache_status = cache_status,
    source = source,
    url = "https://example.test"
  )
}

egfr_uniprot <- function() {
  read_fixture("uniprot_overview_egfr.json")
}

egfr_ensembl <- function() {
  read_fixture("ensembl_overview_egfr.json")
}

kras_ensembl <- function() {
  read_fixture("ensembl_overview_kras.json")
}

retrieve_egfr <- function(
  cache_status = "live",
  from_cache = FALSE,
  ensembl_payload = egfr_ensembl(),
  uniprot_payload = egfr_uniprot(),
  row = confirmed_row()
) {
  uniprot_calls <- 0L
  ensembl_calls <- 0L

  result <- retrieve_target_overview(
    row,
    db_pool = NULL,
    uniprot_fetch = function(...) {
      uniprot_calls <<- uniprot_calls + 1L
      fixture_result(uniprot_payload, "uniprot", cache_status, from_cache)
    },
    ensembl_fetch = function(...) {
      ensembl_calls <<- ensembl_calls + 1L
      fixture_result(ensembl_payload, "ensembl", cache_status, from_cache)
    }
  )

  list(
    result = result,
    uniprot_calls = uniprot_calls,
    ensembl_calls = ensembl_calls
  )
}

test_that("UniProt overview parser extracts length, mass, function, and locations", {
  parsed <- parse_uniprot_search(egfr_uniprot())
  entry <- parsed$entries[[1]]

  expect_true(parsed$ok)
  expect_equal(entry$accession, "P00533")
  expect_equal(entry$display_symbol, "EGFR")
  expect_equal(entry$protein_name, "Epidermal growth factor receptor")
  expect_equal(entry$organism, "Homo sapiens")
  expect_true(entry$reviewed)
  expect_equal(entry$sequence_length, 1210)
  expect_equal(entry$molecular_weight, 134277)
  expect_equal(length(entry$function_comments), 2)
  expect_match(entry$function_comments[[1]]$text, "Receptor tyrosine kinase")
  expect_equal(entry$function_comments[[2]]$text, "Isoform 2 may act as an antagonist of EGF action.")
  expect_equal(entry$subcellular_locations[[1]]$location, "Cell membrane")
  expect_equal(entry$subcellular_locations[[1]]$topology, "Single-pass type I membrane protein")
  expect_match(entry$subcellular_locations[[1]]$evidence, "ECO:0000269")
  expect_match(entry$subcellular_locations[[1]]$evidence, "PubMed:17182860")
  expect_equal(entry$subcellular_locations[[2]]$location, "Nucleus")
})

test_that("UniProt overview parser leaves optional fields empty when absent", {
  entry <- parse_uniprot_search(read_fixture("uniprot_overview_sparse.json"))$entries[[1]]

  expect_equal(entry$sequence_length, 80)
  expect_true(is.na(entry$molecular_weight) || is.null(entry$molecular_weight))
  expect_equal(length(entry$function_comments), 0)
  expect_equal(length(entry$subcellular_locations), 0)
  expect_false(isTRUE(entry$reviewed))
})

test_that("Ensembl genomic parser extracts coordinates, strand, and version", {
  gene <- parse_ensembl_gene(egfr_ensembl())

  expect_equal(gene$ensembl_gene_id, "ENSG00000146648")
  expect_equal(gene$display_symbol, "EGFR")
  expect_equal(gene$biotype, "protein_coding")
  expect_equal(gene$seq_region, "7")
  expect_equal(gene$start, 55019017)
  expect_equal(gene$end, 55211628)
  expect_equal(gene$strand, 1)
  expect_equal(gene$ensembl_gene_version, 22)
})

test_that("missing optional UniProt and Ensembl fields do not crash the overview", {
  overview <- build_target_overview(
    confirmed_row(),
    empty_uniprot_entry(),
    empty_ensembl_gene(),
    fixture_result(list(), "uniprot"),
    fixture_result(list(), "ensembl")
  )

  html <- as.character(overview_body_ui(overview, NS("overview")))
  expect_false(grepl("\\bNA\\b", html))
  expect_false(grepl("NULL", html, fixed = TRUE))
  expect_false(grepl("character\\(0\\)", html))
  expect_match(html, "Not provided")
})

test_that("NULL after JSONB round-trip remains displayable", {
  packed <- retrieve_egfr()$result$overview
  stored <- json_roundtrip(packed)
  stored$protein$length_aa <- NULL
  stored$protein$molecular_weight <- NULL
  stored$protein$function_full <- NULL
  stored$protein$subcellular_locations <- list()
  stored$genomic$chromosome <- NULL
  stored$genomic$start <- NULL
  stored$identity$hgnc_id <- NULL

  html <- as.character(overview_body_ui(stored, NS("overview")))
  expect_match(html, "Not provided")
  expect_false(grepl("character\\(0\\)", html))
})

test_that("positive-strand genomic visualization data points 3-prime-ward", {
  gene <- parse_ensembl_gene(egfr_ensembl())
  data <- build_genomic_plot_data(
    list(
      chromosome = gene$seq_region,
      start = gene$start,
      end = gene$end,
      strand = gene$strand
    ),
    "EGFR"
  )

  expect_equal(data$direction, "positive")
  expect_equal(data$arrow_ends, "last")
  expect_equal(data$chromosome, "7")
  expect_equal(data$span_bp, 55211628L - 55019017L + 1L)
  expect_match(data$start_label, "Mb")
  expect_match(data$end_label, "Mb")
  expect_true(grepl(",", data$start_bp_label) || nchar(data$start_bp_label) >= 7)
})

test_that("negative-strand genomic visualization data reverses the arrow", {
  gene <- parse_ensembl_gene(kras_ensembl())
  data <- build_genomic_plot_data(
    list(
      chromosome = gene$seq_region,
      start = gene$start,
      end = gene$end,
      strand = gene$strand
    ),
    "KRAS"
  )

  expect_equal(data$direction, "negative")
  expect_equal(data$arrow_ends, "first")
  expect_equal(data$strand, -1L)
})

test_that("ggplot genomic context is built for both strands", {
  skip_if_not_installed("ggplot2")

  pos <- plot_genomic_context(
    list(chromosome = "7", start = 55019017, end = 55211628, strand = 1),
    "EGFR"
  )
  neg <- plot_genomic_context(
    list(chromosome = "12", start = 25205246, end = 25326473, strand = -1),
    "KRAS"
  )

  expect_s3_class(pos, "ggplot")
  expect_s3_class(neg, "ggplot")
  expect_match(pos$labels$subtitle, "positive")
  expect_match(neg$labels$subtitle, "negative")
})

test_that("provenance records source, query, cache status, and retrieval time", {
  live <- retrieve_egfr("live", FALSE)$result$overview$provenance
  cached <- retrieve_egfr("fresh", TRUE)$result$overview$provenance
  stale <- retrieve_egfr("stale", TRUE)$result$overview$provenance

  expect_equal(live$uniprot$source, "uniprot")
  expect_equal(live$uniprot$record_id, "P00533")
  expect_equal(live$uniprot$query_id, "P00533")
  expect_equal(live$uniprot$cache_status, "live")
  expect_false(live$uniprot$from_cache)
  expect_match(live$uniprot$url, "uniprot.org")
  expect_equal(live$ensembl$record_id, "ENSG00000146648")
  expect_equal(live$ensembl$query_id, "ENSG00000146648")
  expect_equal(live$ensembl$cache_status, "live")

  expect_equal(cached$uniprot$cache_status, "cached/fresh")
  expect_true(cached$uniprot$from_cache)
  expect_equal(stale$ensembl$cache_status, "stale")
  expect_match(as.character(live$uniprot$retrieved_at), "2026-09-10")
})

test_that("unresolved targets cannot retrieve overview and do not call APIs", {
  uniprot_calls <- 0L
  ensembl_calls <- 0L
  result <- retrieve_target_overview(
    confirmed_row(status = "unresolved"),
    uniprot_fetch = function(...) {
      uniprot_calls <<- uniprot_calls + 1L
      stop("should not fetch UniProt")
    },
    ensembl_fetch = function(...) {
      ensembl_calls <<- ensembl_calls + 1L
      stop("should not fetch Ensembl")
    }
  )

  expect_equal(result$status, "not_confirmed")
  expect_null(result$overview)
  expect_match(result$message, "Confirm this target")
  expect_equal(uniprot_calls, 0)
  expect_equal(ensembl_calls, 0)
})

test_that("confirmed targets retrieve a normalized overview without source JSON", {
  packed <- retrieve_egfr()
  result <- packed$result
  overview <- result$overview

  expect_equal(packed$uniprot_calls, 1)
  expect_equal(packed$ensembl_calls, 1)
  expect_equal(result$status, "ready")
  expect_equal(overview$identity$symbol, "EGFR")
  expect_equal(overview$identity$ensembl_gene_id, "ENSG00000146648")
  expect_equal(overview$identity$uniprot_accession, "P00533")
  expect_equal(overview$protein$length_aa, 1210)
  expect_true(isTRUE(overview$protein$reviewed))
  expect_equal(overview$genomic$chromosome, "7")
  expect_false("primaryAccession" %in% names(overview))
  expect_false("seq_region_name" %in% names(overview$genomic))

  html <- as.character(overview_body_ui(overview, NS("overview")))
  expect_match(html, "Epidermal growth factor receptor")
  expect_match(html, "ENSG00000146648")
  expect_match(html, "P00533")
  expect_match(html, "Show full UniProt annotation", all = FALSE)
  expect_match(html, "Location evidence")
  expect_match(html, "<details")
  expect_match(html, "<summary")
  expect_false(grepl("checkbox", html, ignore.case = TRUE))
  expect_match(html, "Cell membrane")
  if (length(overview$protein$function_pubmed_ids) > 0) {
    expect_match(html, "reference")
  }
  expect_match(html, "cached/fresh|live")
})

test_that("source disagreement is visible when UniProt and Ensembl identities differ", {
  packed <- retrieve_egfr(ensembl_payload = kras_ensembl())
  warnings <- packed$result$overview$disagreements
  html <- as.character(overview_body_ui(packed$result$overview, NS("overview")))

  expect_true(length(warnings) >= 1)
  expect_true(any(grepl("differ", unlist(warnings), ignore.case = TRUE)))
  expect_match(html, "Source disagreement")
})

test_that("function preview truncates without rewriting UniProt wording", {
  long <- paste(rep("Receptor tyrosine kinase binding ligands of the EGF family.", 8), collapse = " ")
  preview <- function_preview_text(long, max_chars = 120)
  expect_true(nchar(preview) < nchar(long))
  expect_match(preview, "Receptor tyrosine kinase")
  expect_false(grepl("This protein", preview))
})

test_that("function references and location evidence are collapsed by default", {
  skip_if_not_installed("shiny")
  library(shiny)
  source_app("R/modules/mod_overview.R")

  protein <- list(
    function_full = "Receptor tyrosine kinase (PubMed:10805725).",
    function_readable = "Receptor tyrosine kinase.",
    function_preview = "Receptor tyrosine kinase.",
    function_pubmed_ids = c("10805725", "2"),
    subcellular_locations = list(
      list(location = "Cell membrane", topology = NA_character_, molecule = NA_character_, evidence = "ECO:0000269|PubMed:17182860")
    )
  )
  html <- as.character(overview_function_ui(protein, NS("ov")))
  expect_match(html, "<details")
  expect_match(html, "<summary>2 references</summary>")
  expect_false(grepl("<details[^>]*\\sopen", html))
  expect_false(grepl("checkbox", html, ignore.case = TRUE))
  expect_match(html, "PubMed 10805725")

  loc_html <- as.character(overview_location_ui(protein$subcellular_locations, NS("ov")))
  expect_match(loc_html, "Cell membrane")
  expect_match(loc_html, "<summary>Location evidence</summary>")
  expect_false(grepl("<details[^>]*\\sopen", loc_html))
  expect_match(loc_html, "ECO:0000269")
})

test_that("PubMed identifiers are separated from readable UniProt function text", {
  text <- "Receptor tyrosine kinase (PubMed:10805725). Binds EGF (PubMed:1, PubMed:2)."
  readable <- readable_function_text(text)
  ids <- extract_pubmed_ids(text)
  expect_equal(ids, c("10805725", "1", "2"))
  expect_false(grepl("PubMed", readable))
  expect_match(readable, "Receptor tyrosine kinase")
  expect_match(readable, "Binds EGF")
  expect_equal(readable_function_text(text), readable_function_text(text))
})

test_that("user-facing timestamps drop seconds and convert from stored UTC", {
  stamp <- as.POSIXct("2026-09-10 18:32:29.06081", tz = "UTC")
  expect_equal(format_user_timestamp(stamp, tz = "UTC"), "10 Sep 2026, 18:32 UTC")
  expect_equal(format_user_timestamp("2026-09-10 18:32:29.06081", tz = "UTC"), "10 Sep 2026, 18:32 UTC")
  expect_equal(format_user_timestamp(stamp, tz = "Europe/Amsterdam"), "10 Sep 2026, 20:32 CEST")
  expect_true(is.na(format_user_timestamp(NULL)))
})

test_that("overview display never prints raw NA/NULL/character(0)", {
  expect_equal(overview_display(NULL), "Not provided")
  expect_equal(overview_display(NA_character_), "Not provided")
  expect_equal(overview_display(character(0)), "Not provided")
  expect_equal(overview_display(""), "Not provided")
  expect_equal(overview_display("EGFR"), "EGFR")
})

test_that("duplicate UniProt subcellular locations are collapsed", {
  payload <- list(
    entryType = "UniProtKB reviewed (Swiss-Prot)",
    primaryAccession = "P00000",
    comments = list(
      list(
        commentType = "SUBCELLULAR LOCATION",
        subcellularLocations = list(
          list(location = list(value = "Nucleus")),
          list(location = list(value = "Nucleus"))
        )
      )
    )
  )
  entry <- parse_uniprot_entry(payload)
  expect_equal(length(entry$subcellular_locations), 1)
  expect_equal(entry$subcellular_locations[[1]]$location, "Nucleus")
})

test_that("authorization still hides another user's confirmed target", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  user_a <- register_user(db_pool, sprintf("ov-a-%s@example.com", suffix), "correct-horse-battery")
  user_b <- register_user(db_pool, sprintf("ov-b-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    user_a$user$id,
    "NSCLC overview gate",
    "Which candidates deserve overview?",
    "Non-small-cell lung cancer",
    c("EGFR")
  )
  target_id <- list_project_targets(db_pool, created$project_id, user_a$user$id)$id[[1]]
  confirm_project_target(
    db_pool,
    target_id,
    user_a$user$id,
    "EGFR",
    "ENSG00000146648",
    "P00533",
    "HGNC:3236"
  )

  expect_equal(get_owned_target(db_pool, target_id, user_a$user$id)$resolution_status, "confirmed")
  expect_true(is.null(get_owned_target(db_pool, target_id, user_b$user$id)))
})

test_that("overview disclosure markup does not change retrieval gating", {
  row <- confirmed_row()
  expect_false(should_retrieve_overview(FALSE, row, NA_character_))
  expect_true(should_retrieve_overview(TRUE, row, NA_character_))
  expect_false(should_retrieve_overview(TRUE, row, overview_fetch_signature(row)))

  packed <- retrieve_egfr()
  html <- as.character(overview_body_ui(packed$result$overview, NS("overview")))
  expect_match(html, "<details")
  expect_match(html, "Show full UniProt annotation")
  expect_false(grepl("type=\"checkbox\"", html, ignore.case = TRUE))
})
