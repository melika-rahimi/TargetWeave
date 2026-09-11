test_that("owner can create, view, rename, and delete a snapshot", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)

  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Baseline review"
  )
  expect_true(created$ok)
  snap <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(snap$name, "Baseline review")
  expect_equal(snap$schema_version, 1L)
  expect_equal(snap$project_context$model$title, "Potential therapeutic targets in NSCLC")

  expect_true(rename_evidence_snapshot(db_pool, created$snapshot_id, seed$owner$id, "Baseline review v1")$ok)
  expect_true(delete_evidence_snapshot(db_pool, created$snapshot_id, seed$owner$id))
  expect_null(get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id))
})

test_that("auto snapshot names use the captured display timestamp", {
  db_pool <- skip_if_no_postgres()
  on.exit(
    {
      if (exists("created", inherits = FALSE) && isTRUE(created$ok)) {
        try(delete_evidence_snapshot(db_pool, created$snapshot_id, seed$owner$id), silent = TRUE)
      }
      try(pool::poolClose(db_pool), silent = TRUE)
    },
    add = TRUE
  )
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Snapshot \u00b7 12 Sep 2026, 00:16 CEST"
  )
  expect_true(created$ok)
  shown <- format_user_timestamp(created$snapshot$created_at)
  expect_equal(created$snapshot$name, paste("Snapshot \u00b7", shown))
  listed <- list_project_snapshots(db_pool, seed$project_id, seed$owner$id)
  expect_equal(listed$name[[1]], created$snapshot$name)
  expect_equal(format_user_timestamp(listed$created_at[[1]]), shown)
})

test_that("non-owner cannot view or delete a snapshot", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Private"
  )
  expect_null(get_owned_snapshot(db_pool, created$snapshot_id, seed$other$id))
  expect_false(delete_evidence_snapshot(db_pool, created$snapshot_id, seed$other$id))
  expect_equal(nrow(list_project_snapshots(db_pool, seed$project_id, seed$other$id)), 0)
})

test_that("snapshot values stay frozen after live cache and project edits", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Frozen"
  )
  original <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)

  cache_put(
    db_pool,
    "uniprot",
    cache_key_uniprot_accession("P00533"),
    list(changed = TRUE),
    200L,
    Sys.time(),
    Sys.time() + 3600
  )
  DBI::dbExecute(
    db_pool,
    "UPDATE projects SET title = 'Edited live title' WHERE id = $1::uuid",
    params = list(seed$project_id)
  )

  frozen <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(frozen$project_context$model$title, original$project_context$model$title)
  expect_equal(frozen$overview$targets[[seed$egfr_id]]$model$protein$length_aa, 1210)
  expect_equal(frozen$source_manifest[[1]]$retrieved_at, original$source_manifest[[1]]$retrieved_at)
})

test_that("source manifest keeps independent versions and retrieval times", {
  payload <- build_snapshot_payload(
    data.frame(
      id = "proj",
      title = "NSCLC",
      research_question = "q",
      organism = "Homo sapiens",
      disease_label = "NSCLC",
      disease_name = "non-small cell lung carcinoma",
      disease_ontology_id = "MONDO_0005233",
      disease_resolution_status = "confirmed",
      stringsAsFactors = FALSE
    ),
    data.frame(
      id = "egfr",
      input_text = "EGFR",
      display_symbol = "EGFR",
      resolution_status = "confirmed",
      ensembl_gene_id = "ENSG00000146648",
      uniprot_accession = "P00533",
      hgnc_id = "HGNC:3236",
      stringsAsFactors = FALSE
    ),
    local_workspace(list(egfr_id = "egfr"))
  )
  sources <- vapply(payload$source_manifest, `[[`, character(1), "source")
  expect_true("UniProt" %in% sources)
  expect_true("Ensembl" %in% sources)
  expect_true("Open Targets" %in% sources)
  expect_true("Reactome" %in% sources)
  expect_true("NCBI PubMed" %in% sources)
  expect_true("RCSB PDB" %in% sources)
  times <- vapply(payload$source_manifest, function(row) as.character(row$retrieved_at %||% ""), character(1))
  expect_true(length(unique(times[nzchar(times) & times != "NA"])) >= 4)
  ot <- Filter(function(row) identical(row$source, "Open Targets") && identical(row$scope, "disease_evidence"), payload$source_manifest)[[1]]
  reactome <- Filter(function(row) identical(row$source, "Reactome"), payload$source_manifest)[[1]]
  expect_equal(ot$data_version, "24.09")
  expect_equal(reactome$version, "90")
  expect_false(identical(ot$retrieved_at, reactome$retrieved_at))
})

test_that("stale evidence is allowed and marked stale_at_capture", {
  payload <- build_snapshot_payload(
    data.frame(
      id = "proj", title = "t", research_question = "q", organism = "Homo sapiens",
      disease_label = "d", disease_name = "d", disease_ontology_id = "MONDO_0005233",
      disease_resolution_status = "confirmed", stringsAsFactors = FALSE
    ),
    data.frame(
      id = "egfr", input_text = "EGFR", display_symbol = "EGFR", resolution_status = "confirmed",
      ensembl_gene_id = "ENSG00000146648", uniprot_accession = "P00533", hgnc_id = NA_character_,
      stringsAsFactors = FALSE
    ),
    local_workspace(list(egfr_id = "egfr"), stale_overview = TRUE)
  )
  expect_true(isTRUE(payload$capture_summary$stale_at_capture))
  expect_true(isTRUE(payload$overview$stale_at_capture))
  snap <- hydrate_snapshot_for_view(payload)
  snap$name <- "Baseline review"
  snap$created_at <- Sys.time()
  html <- as.character(snapshot_view_ui(NS("snap"), snap))
  expect_match(html, "Stale at capture")
  expect_match(html, "Preserved snapshot")
})

test_that("partial snapshots preserve not_retrieved and unavailable sections", {
  payload <- build_snapshot_payload(
    data.frame(
      id = "proj", title = "t", research_question = "q", organism = "Homo sapiens",
      disease_label = "d", disease_name = "d", disease_ontology_id = "MONDO_0005233",
      disease_resolution_status = "confirmed", stringsAsFactors = FALSE
    ),
    data.frame(
      id = "egfr", input_text = "EGFR", display_symbol = "EGFR", resolution_status = "confirmed",
      ensembl_gene_id = "ENSG00000146648", uniprot_accession = "P00533", hgnc_id = NA_character_,
      stringsAsFactors = FALSE
    ),
    local_workspace(list(egfr_id = "egfr"), include_literature = FALSE, structures_status = "error")
  )
  expect_equal(payload$literature$capture_status, "not_retrieved")
  expect_equal(payload$structures$capture_status, "unavailable")
  expect_false(is.null(payload$literature$capture_status))
  expect_equal(payload$comparison$capture_status, "captured")
  expect_equal(payload$pathways$capture_status, "captured")
  snap <- hydrate_snapshot_for_view(payload)
  snap$name <- "Partial"
  snap$created_at <- as.POSIXct("2026-09-11 21:50:00", tz = "UTC")
  view_html <- as.character(snapshot_view_ui(NS("snap"), snap))
  expect_false(grepl("Not captured / not retrieved", view_html, fixed = TRUE))
  lit_view <- gregexpr("Not retrieved before snapshot capture.", view_html, fixed = TRUE)[[1]]
  expect_true(all(lit_view > 0))
  dossier <- render_dossier_html(snap, build_export_manifest(snap))
  expect_false(grepl("Not captured / not retrieved", dossier, fixed = TRUE))
  lit <- sub(".*<section id=\"literature\">", "<section id=\"literature\">", dossier)
  lit <- sub("</section>.*", "</section>", lit)
  expect_equal(length(gregexpr("Not retrieved before snapshot capture.", lit, fixed = TRUE)[[1]]), 1L)
})

test_that("snapshot view renders without calling scientific APIs", {
  skip_if_not_installed("httr2")
  payload <- build_snapshot_payload(
    data.frame(
      id = "proj", title = "NSCLC", research_question = "q", organism = "Homo sapiens",
      disease_label = "NSCLC", disease_name = "non-small cell lung carcinoma",
      disease_ontology_id = "MONDO_0005233", disease_resolution_status = "confirmed",
      stringsAsFactors = FALSE
    ),
    data.frame(
      id = "egfr", input_text = "EGFR", display_symbol = "EGFR", resolution_status = "confirmed",
      ensembl_gene_id = "ENSG00000146648", uniprot_accession = "P00533", hgnc_id = "HGNC:3236",
      stringsAsFactors = FALSE
    ),
    local_workspace(list(egfr_id = "egfr"))
  )
  snap <- hydrate_snapshot_for_view(payload)
  snap$name <- "Baseline review"
  snap$created_at <- Sys.time()
  perform <- function(req) stop("scientific API must not be called when opening a snapshot")
  html <- as.character(snapshot_view_ui(NS("snap"), snap))
  expect_match(html, "Preserved snapshot")
  expect_match(html, "Source manifest")
  expect_match(html, "Reactome")
  expect_false(grepl("Refresh", html))
  expect_error(perform(NULL), "scientific API must not be called")
})
