egfr_structure_target <- function() {
  confirmed_reactome_target("egfr", "EGFR", "P00533", "ENSG00000146648")
}

test_that("structure gate requires a confirmed target and ignores disease", {
  none <- confirmed_reactome_target("t1", "EGFR", "P00533", status = "ambiguous")
  expect_false(structure_gate(none)$ok)

  one <- egfr_structure_target()
  expect_true(structure_gate(one)$ok)
})

test_that("Search API query is polymer-entity UniProt exact match and experimental-only", {
  body <- rcsb_uniprot_search_body("P00533")
  expect_equal(body$return_type, "polymer_entity")
  expect_equal(as.character(body$request_options$results_content_type), "experimental")
  encoded <- jsonlite::toJSON(body, auto_unbox = TRUE)
  expect_match(encoded, '"results_content_type":\\["experimental"\\]')
  expect_match(encoded, "database_accession")
  expect_match(encoded, "exact_match")
  expect_match(encoded, "UniProt")
  expect_false(grepl("AF_", encoded))
})

test_that("computed structure identifiers are rejected from experimental search results", {
  parsed <- parse_rcsb_search_entities(read_fixture("rcsb_search_with_csm.json"))
  expect_true(parsed$ok)
  ids <- vapply(parsed$entities, `[[`, character(1), "identifier")
  expect_equal(ids, "1M17_1")
  expect_true(all(grepl("^(AF_|MA_)", parsed$rejected)))
  expect_false(any(grepl("^(AF_|MA_)", ids)))
})

test_that("inclusive coverage ranges union without double-counting", {
  disjoint <- data.frame(begin = c(100L, 300L), end = c(200L, 400L))
  expect_equal(covered_residue_count(disjoint), 202L)

  overlap <- data.frame(begin = c(100L, 150L), end = c(200L, 250L))
  merged <- merge_inclusive_ranges(overlap)
  expect_equal(merged$begin, 100L)
  expect_equal(merged$end, 250L)
  expect_equal(covered_residue_count(overlap), 151L)
  expect_equal(coverage_fraction(151L, 1000L), 0.151)
})

test_that("NMR resolution is Not provided and waters are excluded from ligands", {
  parsed <- parse_rcsb_entity_metadata(read_fixture("rcsb_data_entities.json"))
  expect_true(parsed$ok)
  nmr <- parsed$records[["1Z9I_1"]]
  expect_equal(nmr$experiment$method, "SOLUTION NMR")
  expect_equal(nmr$experiment$resolution_angstrom, "Not provided")
  xray <- parsed$records[["1NQL_1"]]
  expect_equal(xray$experiment$method, "X-RAY DIFFRACTION")
  expect_match(xray$experiment$resolution_angstrom, "2.80")
  expect_false("HOH" %in% xray$ligands$component_id)
  expect_true("NAG" %in% xray$ligands$component_id)
})

test_that("one PDB entry with two matching polymer entities counts as one entry", {
  skip_if_not_installed("httr2")
  target <- egfr_structure_target()
  result <- retrieve_target_structures(
    target,
    db_pool = NULL,
    perform = rcsb_fixture_perform(
      search = read_fixture("rcsb_search_same_entry.json"),
      metadata = read_fixture("rcsb_data_same_entry.json"),
      alignments = list(data = list(alignments = list(target_alignments = list())))
    )
  )
  expect_equal(result$status, "ok")
  expect_equal(result$structures$n_pdb_entries, 1L)
  expect_equal(result$structures$n_polymer_entities, 2L)
  rec <- result$structures$records[[1]]
  expect_equal(rec$experiment$method, "ELECTRON MICROSCOPY")
  expect_match(rec$experiment$resolution_angstrom, "3.10")
  expect_true(isTRUE(result$structures$records[[1]]$engineered) || isTRUE(result$structures$records[[2]]$engineered))
  chains <- result$structures$records[[1]]$polymer_entity$chains
  if (identical(result$structures$records[[1]]$polymer_entity$entity_id, "1")) {
    expect_equal(chains, c("A", "B"))
  }
})

test_that("EGFR fixture retrieval excludes CSMs, sorts by coverage, and keeps NMR", {
  skip_if_not_installed("httr2")
  result <- retrieve_target_structures(
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform()
  )
  expect_equal(result$status, "ok")
  ids <- vapply(result$structures$records, function(item) item$polymer_entity$entity_identifier, character(1))
  expect_false(any(grepl("^(AF_|MA_)", ids)))
  expect_false(any(grepl("^(AF_|MA_)", result$structures$rejected_computed)))
  expect_equal(ids[[1]], "1NQL_1")
  expect_equal(result$structures$n_pdb_entries, 4L)
  expect_equal(result$structures$uniprot_length, 1210L)
  expect_equal(result$structures$records[[1]]$coverage$covered_residue_count, 618L)
  nmr <- Filter(function(item) identical(item$pdb_id, "1Z9I"), result$structures$records)[[1]]
  expect_equal(nmr$experiment$resolution_angstrom, "Not provided")
  expect_equal(result$structures$provenance$query_scope, "experimental")
})

test_that("CSM-like search rows never enter the normalized experimental model", {
  skip_if_not_installed("httr2")
  result <- retrieve_target_structures(
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform(search = read_fixture("rcsb_search_with_csm.json"))
  )
  ids <- vapply(result$structures$records, function(item) item$polymer_entity$entity_identifier, character(1))
  pdb <- vapply(result$structures$records, `[[`, character(1), "pdb_id")
  expect_true(all(ids == "1M17_1"))
  expect_true(all(grepl("^[0-9][A-Z0-9]{3}$", pdb)))
  expect_false(any(grepl("^(AF_|MA_)", ids)))
  expect_true(any(grepl("^(AF_|MA_)", result$structures$rejected_computed)))
})

test_that("zero experimental hits, search failure, missing UniProt, and unconfirmed are distinct", {
  skip_if_not_installed("httr2")
  empty <- retrieve_target_structures(
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform(search = read_fixture("rcsb_search_empty.json"))
  )
  expect_equal(empty$status, "empty")
  expect_true(isTRUE(empty$structures$search_was_empty))

  failed <- retrieve_target_structures(
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform(search_status = 503L)
  )
  expect_equal(failed$status, "error")

  missing <- retrieve_target_structures(
    confirmed_reactome_target("t", "EGFR", NA_character_),
    db_pool = NULL,
    perform = rcsb_fixture_perform()
  )
  expect_equal(missing$status, "unavailable")
  expect_equal(missing$message, "Structure mapping unavailable.")

  project <- retrieve_project_structures(
    list(id = "p1", disease_resolution_status = "unresolved"),
    rbind(
      egfr_structure_target(),
      confirmed_reactome_target("raw", "RAW", "P01116", status = "ambiguous"),
      confirmed_reactome_target("kras", "KRAS", "P01116")
    ),
    db_pool = NULL,
    perform = rcsb_fixture_perform()
  )
  expect_equal(project$status, "ready")
  expect_equal(project$structures$summary$symbol, c("EGFR", "KRAS"))
  expect_equal(project$structures$summary$n_pdb_entries[[2]], 0L)
  reasons <- vapply(project$structures$excluded_targets, `[[`, character(1), "reason")
  expect_true("unconfirmed" %in% reasons)
})

test_that("partial metadata failure keeps remaining experimental structures", {
  skip_if_not_installed("httr2")
  result <- retrieve_target_structures(
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform(drop_entity_ids = "2GS6_1")
  )
  expect_equal(result$status, "ok")
  ids <- vapply(result$structures$records, function(item) item$polymer_entity$entity_identifier, character(1))
  expect_false("2GS6_1" %in% ids)
  expect_true("1NQL_1" %in% ids)
  expect_true("2GS6_1" %in% result$structures$unavailable_entities)
})

test_that("malformed Sequence Coordinates and Data API payloads do not crash parse", {
  expect_false(parse_rcsb_entity_metadata(read_fixture("rcsb_error.json"))$ok)
  expect_false(parse_rcsb_alignments(read_fixture("rcsb_error.json"))$ok)
})

test_that("computational methodology metadata is skipped", {
  payload <- read_fixture("rcsb_data_entities.json")
  payload$data$polymer_entities[[1]]$entry$rcsb_entry_info$structure_determination_methodology <- "computational"
  parsed <- parse_rcsb_entity_metadata(payload)
  expect_false("1NQL_1" %in% names(parsed$records))
})

test_that("cache keys and TTL are centralized", {
  expect_equal(cache_key_rcsb_search("p00533"), "rcsb:search:P00533:experimental")
  expect_equal(cache_key_rcsb_coverage("P00533"), "rcsb:coverage:P00533:experimental")
  expect_equal(get_app_config()$rcsb_search_ttl_days, 7L)
  expect_equal(get_app_config()$rcsb_metadata_ttl_days, 14L)
  expect_equal(rcsb_search_ttl_seconds(), 7L * 86400L)
  expect_equal(rcsb_metadata_ttl_seconds(), 14L * 86400L)
})

test_that("Structures UI states avoid NA/NULL and load one Mol* viewer after selection", {
  skip_if_not_installed("httr2")
  source_app("R/modules/mod_structures.R")
  result <- retrieve_project_structures(
    list(id = "p1"),
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform()
  )
  ns <- NS("structures")
  selected <- result$structures$targets[[1]]
  html <- as.character(structures_result_ui(ns, result, selected, NULL, selected$target$project_target_id))
  expect_match(html, "Experimental PDB structures may represent only part of the canonical protein")
  expect_equal(length(gregexpr("Experimental PDB structures may represent only part", html, fixed = TRUE)[[1]]), 1)
  expect_match(html, "Sorted by sequence coverage")
  expect_match(html, "SOLUTION NMR|Not provided")
  expect_false(grepl(">NA<", html))
  expect_false(grepl(">NULL<", html))
  expect_false(grepl("3d-view", html))

  rec <- selected$records[[1]]
  detail <- as.character(structure_detail_ui(ns, selected, rec))
  expect_match(detail, "Open in RCSB PDB")
  expect_match(detail, "rcsb.org/structure/")
  expect_match(detail, "rcsb.org/3d-view/")
  expect_match(detail, "iframe")
  expect_match(detail, rec$pdb_id)
})

test_that("Structures navigation exists and the module does not call httr2", {
  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  mod <- paste(readLines(file.path(app_root(), "R/modules/mod_structures.R")), collapse = "\n")
  expect_match(home, "Structures")
  expect_match(home, "mod_structures_ui")
  expect_false(grepl("httr2", mod))
  expect_equal(workspace_panel_label("structures"), "Structures")
  expect_equal(workspace_back_destination("structures"), "project")
})

test_that("stale RCSB search is reused when the live request fails", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  live <- retrieve_target_structures(
    egfr_structure_target(),
    db_pool = db_pool,
    perform = rcsb_fixture_perform()
  )
  expect_equal(live$status, "ok")

  DBI::dbExecute(
    db_pool,
    "
    UPDATE api_cache
    SET expires_at = NOW() - INTERVAL '1 hour'
    WHERE cache_key LIKE '%rcsb:%'
    "
  )

  stale <- retrieve_target_structures(
    egfr_structure_target(),
    db_pool = db_pool,
    perform = rcsb_fixture_perform(search_status = 503L)
  )
  expect_equal(stale$status, "ok")
  expect_equal(stale$structures$provenance$cache_status, "stale")
  expect_equal(stale$structures$n_pdb_entries, live$structures$n_pdb_entries)
})
