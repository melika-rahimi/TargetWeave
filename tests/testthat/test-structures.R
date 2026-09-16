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
  expect_match(html, "Experimentally determined PDB structures associated with confirmed targets")
  expect_match(html, "Sorted by sequence coverage")
  expect_match(html, "SOLUTION NMR|Not provided")
  expect_match(html, "experimental-structures-surface")
  expect_match(html, "evidence-target-switcher")
  expect_match(html, 'role="radiogroup"')
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
  expect_false(grepl("radioButtons", mod))
  expect_equal(workspace_panel_label("structures"), "Structures")
  expect_equal(workspace_back_destination("structures"), "project")
})

structures_ui_html <- function(current, selected, record = NULL, selected_id = NULL, query = "", visible_n = STRUCTURE_PAGE_SIZE) {
  source_app("R/modules/mod_structures.R")
  ns <- shiny::NS("structures")
  if (is.null(selected_id) && !is.null(selected)) {
    selected_id <- selected$target$project_target_id
  }
  paste(
    as.character(structures_result_ui(
      ns,
      current,
      selected,
      record,
      selected_id,
      query = query,
      visible_n = visible_n
    )),
    collapse = "\n"
  )
}

test_that("Structures evidence UI preserves experimental records, zero, failure, stale, and switcher a11y", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("shiny")

  multi <- retrieve_project_structures(
    list(id = "p1"),
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform()
  )
  selected <- multi$structures$targets[[1]]
  html <- structures_ui_html(multi, selected)
  expect_match(html, "Confirmed targets included")
  expect_match(html, "Experimental PDB entries retrieved")
  expect_match(html, "Targets with at least one retrieved structure")
  expect_match(html, "How to interpret these results")
  expect_false(grepl("interpretation-guidance[^>]*open", html))
  expect_match(html, "Predicted structures are not included")
  expect_match(html, "PDB entry count is the number of retrieved matching experimental entries")
  expect_match(html, "structure-table")
  expect_match(html, "structure-pdb-id")
  expect_match(html, "structure-title")
  expect_match(html, "X-RAY DIFFRACTION")
  expect_match(html, "SOLUTION NMR")
  expect_match(html, "Not provided")
  expect_match(html, "Open in RCSB")
  expect_match(html, "Technical provenance")
  expect_match(html, "RCSB Protein Data Bank")
  expect_match(html, "Entry counts are availability, not target importance")
  expect_false(grepl("AlphaFold", html, ignore.case = TRUE))
  expect_false(grepl(
    "druggability|structural support|structural strength|best structure|most structured",
    html,
    ignore.case = TRUE
  ))
  expect_gt(length(selected$records), 1L)
  expect_equal(selected$n_pdb_entries, 4L)

  long_title <- paste(rep("Long experimental structure title fragment", 10), collapse = " ")
  selected$records[[1]]$entry$title <- long_title
  long_html <- structures_ui_html(multi, selected)
  expect_match(long_html, "structure-title")
  expect_match(long_html, "Long experimental structure title fragment")

  rec <- selected$records[[1]]
  selected_html <- structures_ui_html(multi, selected, rec)
  expect_match(selected_html, "is-selected")
  expect_match(selected_html, "structure-inspect-pdb")
  expect_match(selected_html, "3d-view")
  expect_match(selected_html, rec$pdb_id)
  expect_false(grepl("structure-shell is-stale", html))

  one <- retrieve_project_structures(
    list(id = "p1"),
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform(
      search = read_fixture("rcsb_search_same_entry.json"),
      metadata = read_fixture("rcsb_data_same_entry.json"),
      alignments = list(data = list(alignments = list(target_alignments = list())))
    )
  )
  one_sel <- one$structures$targets[[1]]
  one_html <- structures_ui_html(one, one_sel)
  expect_equal(one_sel$n_pdb_entries, 1L)
  expect_match(one_html, "1 experimental PDB entry")
  expect_match(one_html, "ELECTRON MICROSCOPY")
  expect_match(one_html, "3.10")

  empty <- retrieve_project_structures(
    list(id = "p1"),
    egfr_structure_target(),
    db_pool = NULL,
    perform = rcsb_fixture_perform(search = read_fixture("rcsb_search_empty.json"))
  )
  empty_sel <- empty$structures$targets[[1]]
  empty_html <- structures_ui_html(empty, empty_sel)
  expect_true(isTRUE(empty_sel$search_was_empty))
  expect_match(empty_html, "No source result")
  expect_match(empty_html, "Predicted structures are not included")
  expect_false(grepl("3d-view", empty_html))
  expect_false(grepl("panel-state-unavailable", empty_html))

  missing_meta <- selected
  missing_meta$records <- list()
  missing_meta$unavailable_entities <- c("1NQL_1")
  missing_meta$search_was_empty <- FALSE
  missing_current <- multi
  missing_current$structures$targets[[1]] <- missing_meta
  missing_html <- structures_ui_html(missing_current, missing_meta)
  expect_match(missing_html, "Metadata unavailable")
  expect_false(grepl("No source result", missing_html, fixed = TRUE))

  failed <- multi
  failed$structures$failures <- list(list(
    project_target_id = "kras",
    symbol = "KRAS",
    message = "KRAS experimental structure retrieval failed."
  ))
  failed_html <- structures_ui_html(failed, selected)
  expect_match(failed_html, "KRAS experimental structure retrieval failed")
  expect_match(failed_html, "1NQL")
  expect_match(failed_html, "structure-table")
  expect_false(grepl("No source result", failed_html, fixed = TRUE))

  stale <- multi
  stale$stale_target_set <- TRUE
  stale$stale_message <- "Confirmed target set changed. Refresh structures to retrieve current evidence."
  stale_html <- structures_ui_html(stale, selected)
  expect_match(stale_html, "target-set-stale")
  expect_match(stale_html, "Refresh structures")
  expect_match(stale_html, "structure-shell is-stale")
  expect_match(stale_html, "1NQL")
  expect_false(grepl("structure-shell is-stale", html))

  css <- paste(readLines(file.path(app_root(), "www", "styles.css")), collapse = "\n")
  expect_match(css, "structure-record-row.is-selected")
  expect_match(css, "structure-title")
  expect_match(
    css,
    "\\.evidence-target-switcher input\\.target-switch-input\\[type=\"radio\"\\] \\{[^}]*appearance: none"
  )
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

fake_structure_record <- function(i, pdb_id = NULL, title = NULL, method = "X-RAY DIFFRACTION") {
  pdb_id <- pdb_id %||% sprintf("1%03d", as.integer(i))
  entity <- paste0(pdb_id, "_1")
  list(
    pdb_id = pdb_id,
    entry = list(
      title = title %||% sprintf("Experimental structure %s of a receptor", pdb_id),
      release_date = "2020-01-01"
    ),
    experiment = list(method = method, resolution_angstrom = "2.10 Å"),
    polymer_entity = list(
      entity_identifier = entity,
      entity_id = "1",
      chains = "A"
    ),
    coverage = list(
      coverage_label = "12.0%",
      covered_ranges = data.frame(begin = integer(), end = integer())
    ),
    rcsb_url = sprintf("https://www.rcsb.org/structure/%s", pdb_id),
    viewer_url = sprintf("https://www.rcsb.org/3d-view/%s", pdb_id)
  )
}

test_that("Structures list search and show-more are presentation-only over the full retrieved set", {
  skip_if_not_installed("shiny")
  source_app("R/modules/mod_structures.R")
  records <- lapply(seq_len(392L), fake_structure_record)
  records[[50]]$pdb_id <- "7SYD"
  records[[50]]$polymer_entity$entity_identifier <- "7SYD_1"
  records[[50]]$entry$title <- "EGFR kinase domain bound to osimertinib"
  records[[50]]$rcsb_url <- "https://www.rcsb.org/structure/7SYD"
  records[[50]]$viewer_url <- "https://www.rcsb.org/3d-view/7SYD"
  records[[300]]$entry$title <- "cryo-EM EGFR dimer"
  records[[300]]$experiment$method <- "ELECTRON MICROSCOPY"

  expect_equal(length(records), 392L)
  expect_equal(STRUCTURE_PAGE_SIZE, 25L)
  expect_equal(length(structure_visible_records(records, STRUCTURE_PAGE_SIZE)), 25L)
  more <- structure_visible_records(records, STRUCTURE_PAGE_SIZE * 2L)
  expect_equal(length(more), 50L)
  expect_equal(length(records), 392L)

  kinase <- filter_structure_records(records, "kinase")
  expect_equal(length(kinase), 1L)
  expect_equal(kinase[[1]]$pdb_id, "7SYD")
  expect_equal(length(filter_structure_records(records, "7syd")), 1L)
  expect_equal(filter_structure_records(records, "7syd")[[1]]$pdb_id, "7SYD")
  expect_gt(length(filter_structure_records(records, "EGFR")), 1L)
  expect_equal(length(filter_structure_records(records, "CRYO")), 1L)
  expect_equal(length(filter_structure_records(records, "")), 392L)
  expect_equal(length(filter_structure_records(records, "zzz-no-structure")), 0L)

  selected <- list(
    target = list(symbol = "EGFR", uniprot_accession = "P00533", project_target_id = "egfr"),
    n_pdb_entries = 392L,
    n_polymer_entities = 392L,
    max_coverage_fraction = 0.5,
    unavailable_entities = character(),
    search_was_empty = FALSE,
    records = records,
    provenance = list(
      identifier_used = "P00533",
      cache_status = "live",
      retrieved_at = "2026-09-16",
      search_api = "search.rcsb.org",
      data_api = "data.rcsb.org",
      sequence_coordinates_api = "sequence-coordinates.rcsb.org",
      sort_rule = "coverage"
    )
  )
  current <- list(
    status = "ready",
    stale_target_set = FALSE,
    structures = list(
      summary = data.frame(
        project_target_id = "egfr",
        symbol = "EGFR",
        n_pdb_entries = 392L,
        stringsAsFactors = FALSE
      ),
      targets = list(selected),
      excluded_targets = list(),
      failures = list(),
      provenance = list(cache_key_family = "rcsb:search")
    )
  )

  html25 <- structures_ui_html(current, selected, visible_n = 25L)
  expect_equal(selected$n_pdb_entries, 392L)
  expect_match(html25, "392 experimental PDB")
  expect_match(html25, "Showing 1\u201325 of 392 experimental structures")
  expect_equal(length(gregexpr("structure-record-row", html25, fixed = TRUE)[[1]]), 25L)
  expect_match(html25, "Show 25 more")
  expect_match(html25, "Search experimental structures")
  expect_false(grepl("3d-view", html25))

  html50 <- structures_ui_html(current, selected, visible_n = 50L)
  expect_equal(length(gregexpr("structure-record-row", html50, fixed = TRUE)[[1]]), 50L)
  expect_match(html50, "Showing 1\u201350 of 392 experimental structures")
  expect_match(html50, "392 experimental PDB")

  html_id <- structures_ui_html(current, selected, query = "7syd")
  expect_match(html_id, "7SYD")
  expect_match(html_id, "Showing 1\u20131 of 1 matching experimental structures")
  expect_false(grepl("Show 25 more", html_id, fixed = TRUE))
  expect_equal(selected$n_pdb_entries, 392L)

  html_title <- structures_ui_html(current, selected, query = "KINASE")
  expect_match(html_title, "EGFR kinase domain bound to osimertinib")
  expect_match(html_title, "7SYD")

  html_none <- structures_ui_html(current, selected, query = "zzz-no-structure")
  expect_match(html_none, "No experimental structures match this search")
  expect_false(grepl("No experimental PDB structures were found for this confirmed UniProt accession", html_none, fixed = TRUE))
  expect_false(grepl("No source result", html_none, fixed = TRUE))

  html_reset <- structures_ui_html(current, selected, query = "")
  expect_match(html_reset, "Showing 1\u201325 of 392 experimental structures")

  other <- selected
  other$target$symbol <- "KRAS"
  other$target$uniprot_accession <- "P01116"
  other$target$project_target_id <- "kras"
  other$n_pdb_entries <- 1L
  other$n_polymer_entities <- 1L
  other$records <- records[1]
  other_current <- current
  other_current$structures$summary$project_target_id <- "kras"
  other_current$structures$summary$symbol <- "KRAS"
  other_current$structures$summary$n_pdb_entries <- 1L
  other_current$structures$targets <- list(other)
  rec_a <- records[[50]]
  html_b <- structures_ui_html(other_current, other, record = NULL, selected_id = "kras")
  expect_false(grepl("structure-inspect-pdb", html_b))
  expect_false(grepl("7SYD", html_b))
  expect_match(html_b, other$records[[1]]$pdb_id)
  expect_match(html_b, "Showing 1\u20131 of 1 experimental structures")

  detail <- as.character(structure_detail_ui(shiny::NS("structures"), selected, rec_a))
  expect_match(detail, "Open in RCSB PDB")
  expect_match(detail, "rcsb.org/3d-view/7SYD")
  expect_match(detail, "iframe")

  css <- paste(readLines(file.path(app_root(), "www", "styles.css")), collapse = "\n")
  expect_match(css, "\\.structure-table-wrap \\{[^}]*overflow-x:\\s*auto")
  expect_match(css, "\\.structure-shell \\{[^}]*overflow-x:\\s*hidden")
})
