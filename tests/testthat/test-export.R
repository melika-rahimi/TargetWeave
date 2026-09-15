m10_fixture_snapshot <- function(workspace = NULL, name = "Baseline review") {
  payload <- build_snapshot_payload(
    data.frame(
      id = "proj",
      title = "Potential therapeutic targets in NSCLC",
      research_question = "Which candidates deserve deeper investigation?",
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
    workspace %||% local_workspace(list(egfr_id = "egfr"))
  )
  snap <- export_hydrate_snapshot(payload)
  snap$id <- "00000000-0000-0000-0000-000000000010"
  snap$project_id <- "00000000-0000-0000-0000-000000000011"
  snap$name <- name
  snap$created_at <- as.POSIXct("2026-09-11 14:30:00", tz = "UTC")
  snap
}

m10_zero_missing_workspace <- function() {
  ws <- local_workspace(list(egfr_id = "egfr"))
  ws$comparison$comparison$targets <- data.frame(
    symbol = c("EGFR", "KRAS"),
    overall_direct_score = c(0, NA_real_),
    overall_inclusive_score = c(0, NA_real_),
    retrieval_status = c("ok", "unavailable"),
    cache_status = c("live", NA_character_),
    stringsAsFactors = FALSE
  )
  ws$comparison$comparison$datatype_matrix <- data.frame(
    symbol = c("EGFR", "KRAS"),
    datatype_id = c("genetic_association", "genetic_association"),
    datatype_label = c("Genetic associations", "Genetic associations"),
    score = c(0, NA_real_),
    is_missing = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  ws$literature$literature$targets[[1]]$corpus$total_count <- 0L
  ws$literature$literature$targets[[1]]$recent_records <- data.frame(
    pmid = character(),
    title = character(),
    stringsAsFactors = FALSE
  )
  ws
}

read_zip_text <- function(zip_path) {
  dir <- tempfile("tw-unzip-")
  dir.create(dir)
  utils::unzip(zip_path, exdir = dir)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  texts <- lapply(files, function(path) {
    raw <- readBin(path, what = "raw", n = file.info(path)$size)
    list(path = path, rel = substring(path, nchar(dir) + 2L), text = rawToChar(raw, multiple = FALSE))
  })
  list(dir = dir, files = vapply(texts, `[[`, character(1), "rel"), contents = texts)
}

expect_no_export_secrets <- function(text) {
  expect_false(grepl("WebEnv", text, ignore.case = TRUE))
  expect_false(grepl("query_key", text, ignore.case = TRUE))
  expect_false(grepl("api_key", text, ignore.case = TRUE))
  expect_false(grepl("password", text, ignore.case = TRUE))
  expect_false(grepl("NCBI_API_KEY", text, ignore.case = TRUE))
}

test_that("export filenames are filesystem-safe", {
  expect_equal(
    export_safe_stem("Baseline review", as.POSIXct("2026-09-11 14:30:00", tz = "UTC")),
    "TargetWeave_Baseline-review_2026-09-11"
  )
  stem <- export_safe_stem("../../etc/passwd", as.POSIXct("2026-09-11", tz = "UTC"))
  expect_false(grepl("\\.\\.", stem))
  expect_false(grepl("/", stem))
})

test_that("HTML and ZIP export succeed when HTTP helpers throw", {
  snap <- m10_fixture_snapshot()
  blocked <- function(...) stop("external HTTP must not run during export")
  env <- environment(http_get_json)
  originals <- list(
    http_get_json = http_get_json,
    http_post_json = http_post_json
  )
  for (name in names(originals)) {
    if (bindingIsLocked(name, env)) {
      unlockBinding(name, env)
    }
    assign(name, blocked, envir = env)
  }
  on.exit({
    for (name in names(originals)) {
      assign(name, originals[[name]], envir = env)
    }
  }, add = TRUE)

  html <- export_snapshot_artifact(snap, format = "html")
  expect_true(html$ok)
  expect_true(file.exists(html$path))
  zip <- export_snapshot_artifact(snap, format = "zip")
  expect_true(zip$ok)
  expect_true(file.exists(zip$path))
  unlink(c(html$path, zip$path))
})

test_that("HTML dossier is self-contained and preserves captured values", {
  snap <- m10_fixture_snapshot()
  result <- export_snapshot_artifact(snap, format = "html")
  on.exit(unlink(result$path), add = TRUE)
  html <- paste(readLines(result$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "TargetWeave")
  expect_match(html, "Baseline review")
  expect_match(html, "Potential therapeutic targets in NSCLC")
  expect_match(html, "EGFR")
  expect_match(html, "P00533")
  expect_match(html, "ENSG00000146648")
  expect_match(html, "1210")
  expect_match(html, "Open Targets")
  expect_match(html, "Reactome")
  expect_match(html, "not probabilities of causality")
  expect_match(html, "Publication counts describe literature volume")
  expect_match(html, "data:image/png;base64,")
  expect_false(grepl("127\\.0\\.0\\.1", html))
  expect_false(grepl("localhost", html, ignore.case = TRUE))
  expect_false(grepl("shiny", html, ignore.case = TRUE))
  expect_false(grepl("websocket", html, ignore.case = TRUE))
  expect_true(startsWith(normalizePath(result$path), normalizePath(tempdir())))
  expect_false(grepl("cdn\\.", html, ignore.case = TRUE))
  expect_false(grepl("@example.com", html, ignore.case = TRUE))
  expect_no_export_secrets(html)
  expect_equal(result$manifest$dossier_format_version, 1L)
})

test_that("partial snapshots export without missing-section CSVs", {
  snap <- m10_fixture_snapshot(
    local_workspace(list(egfr_id = "egfr"), include_literature = FALSE, structures_status = "error")
  )
  result <- export_snapshot_artifact(snap, format = "zip")
  on.exit(unlink(result$path), add = TRUE)
  packed <- read_zip_text(result$path)
  on.exit(unlink(packed$dir, recursive = TRUE), add = TRUE)
  expect_true("README.txt" %in% packed$files)
  expect_true("manifest.txt" %in% packed$files)
  expect_true("manifest.json" %in% packed$files)
  expect_true("report.html" %in% packed$files)
  expect_true("tables/targets.csv" %in% packed$files)
  expect_true("tables/open_targets_comparison.csv" %in% packed$files)
  expect_true("tables/reactome_pathways.csv" %in% packed$files)
  expect_false("tables/literature_records.csv" %in% packed$files)
  expect_false("tables/structures.csv" %in% packed$files)
  html <- packed$contents[[match("report.html", packed$files)]]$text
  expect_match(html, "Not retrieved before snapshot capture.", fixed = TRUE)
  expect_match(html, "unavailable", ignore.case = TRUE)
  expect_false(grepl("Not captured / not retrieved", html, fixed = TRUE))
  lit <- sub(".*<section id=\"literature\">", "<section id=\"literature\">", html)
  lit <- sub("</section>.*", "</section>", lit)
  lit_hits <- gregexpr("Not retrieved before snapshot capture.", lit, fixed = TRUE)[[1]]
  expect_equal(length(lit_hits[lit_hits > 0]), 1L)
  expect_equal(length(gregexpr("<p class=\"status-line\">", lit, fixed = TRUE)[[1]]), 1L)
  expect_match(lit, "<h2>Literature landscape</h2>")
  stru <- sub(".*<section id=\"structures\">", "<section id=\"structures\">", html)
  stru <- sub("</section>.*", "</section>", stru)
  expect_equal(length(gregexpr("<p class=\"status-line\">", stru, fixed = TRUE)[[1]]), 1L)
  expect_match(stru, "unavailable", ignore.case = TRUE)
  expect_false(grepl("Not retrieved before snapshot capture.", stru, fixed = TRUE))
  expect_false(grepl("password", paste(vapply(packed$contents, `[[`, character(1), "text"), collapse = "\n"), ignore.case = TRUE))
})

test_that("full ZIP contains expected tables and no raw payloads", {
  snap <- m10_fixture_snapshot()
  result <- export_snapshot_artifact(snap, format = "zip")
  on.exit(unlink(result$path), add = TRUE)
  packed <- read_zip_text(result$path)
  on.exit(unlink(packed$dir, recursive = TRUE), add = TRUE)
  expect_true(all(c(
    "README.txt",
    "manifest.txt",
    "report.html",
    "tables/targets.csv",
    "tables/open_targets_comparison.csv",
    "tables/reactome_pathways.csv",
    "tables/literature_records.csv",
    "tables/structures.csv"
  ) %in% packed$files))
  blob <- paste(vapply(packed$contents, `[[`, character(1), "text"), collapse = "\n")
  expect_no_export_secrets(blob)
  expect_false(grepl("function_full", blob))
  expect_false(grepl("raw uniprot dump", blob, ignore.case = TRUE))
  expect_match(blob, "immutable evidence snapshot")
  manifest <- packed$contents[[match("manifest.txt", packed$files)]]$text
  expect_match(manifest, "Snapshot captured at")
  expect_match(manifest, "Export generated at")
  expect_match(manifest, "UniProt")
  expect_match(manifest, "24.09")
  expect_match(manifest, "90")
})

test_that("exported tables keep zero distinct from missing", {
  snap <- m10_fixture_snapshot(m10_zero_missing_workspace())
  dest <- tempfile("tw-tables-")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  write_export_tables(export_hydrate_snapshot(snap), file.path(dest, "tables"))
  cmp <- utils::read.csv(file.path(dest, "tables", "open_targets_comparison.csv"), stringsAsFactors = FALSE, na.strings = "")
  egfr <- cmp[cmp$target == "EGFR", ]
  kras <- cmp[cmp$target == "KRAS", ]
  expect_equal(egfr$direct_score, 0)
  expect_true(is.na(kras$direct_score))
  html <- render_dossier_html(
    export_hydrate_snapshot(snap),
    build_export_manifest(snap, generated_at = as.POSIXct("2026-09-11 15:02:00", tz = "UTC"))
  )
  expect_match(html, "0.000")
  expect_match(html, "Unavailable")
  expect_match(html, "Defined PubMed corpus count: 0")
})

test_that("source manifest keeps independent retrieval times and versions", {
  snap <- m10_fixture_snapshot()
  manifest <- build_export_manifest(snap, generated_at = as.POSIXct("2026-09-11 15:02:00", tz = "UTC"))
  expect_false(identical(manifest$snapshot_captured_at, manifest$export_generated_at))
  sources <- manifest$sources
  names <- vapply(sources, function(row) as.character(row$source), character(1))
  expect_true(all(c("UniProt", "Ensembl", "Open Targets", "Reactome", "NCBI PubMed", "RCSB PDB") %in% names))
  uni <- Filter(function(row) identical(row$source, "UniProt"), sources)[[1]]
  ens <- Filter(function(row) identical(row$source, "Ensembl"), sources)[[1]]
  ot <- Filter(function(row) identical(row$source, "Open Targets") && identical(row$scope, "disease_evidence"), sources)[[1]]
  rea <- Filter(function(row) identical(row$source, "Reactome"), sources)[[1]]
  expect_equal(uni$retrieved_at, "2026-01-15T10:00:00Z")
  expect_equal(ens$retrieved_at, "2026-01-15T10:01:00Z")
  expect_equal(ot$version %||% ot$data_version %||% NA_character_, "24.09")
  expect_equal(rea$version, "90")
  html <- render_dossier_html(snap, manifest)
  expect_match(html, format_user_timestamp("2026-01-15T10:00:00Z"), fixed = TRUE)
  expect_match(html, format_user_timestamp("2026-01-15T10:01:00Z"), fixed = TRUE)
  expect_no_export_secrets(html)
})

test_that("dossier captured and generated times use the configured display timezone", {
  snap <- m10_fixture_snapshot()
  snap$created_at <- as.POSIXct("2026-09-11 21:50:00", tz = "UTC")
  generated <- as.POSIXct("2026-09-11 21:51:00", tz = "UTC")
  old_tz <- Sys.getenv("TW_DISPLAY_TZ", unset = NA_character_)
  on.exit(
    {
      if (is.na(old_tz) || !nzchar(old_tz)) {
        Sys.unsetenv("TW_DISPLAY_TZ")
      } else {
        Sys.setenv(TW_DISPLAY_TZ = old_tz)
      }
    },
    add = TRUE
  )
  Sys.unsetenv("TW_DISPLAY_TZ")
  snap$name <- default_snapshot_name(snap$created_at)
  manifest <- build_export_manifest(snap, generated_at = generated)
  expect_equal(manifest$snapshot_captured_at, "2026-09-11T21:50:00Z")
  expect_equal(manifest$snapshot_captured_at_display, "11 Sep 2026, 21:50 UTC")
  expect_equal(manifest$export_generated_at_display, "11 Sep 2026, 21:51 UTC")
  expect_equal(manifest$display_timezone, "UTC")
  html <- render_dossier_html(snap, manifest)
  expect_match(html, "Captured: 11 Sep 2026, 21:50 UTC", fixed = TRUE)
  expect_match(html, "Generated: 11 Sep 2026, 21:51 UTC", fixed = TRUE)
  expect_true(grepl("11 Sep 2026, 21:50 UTC", snap$name, fixed = TRUE))

  Sys.setenv(TW_DISPLAY_TZ = "Europe/Amsterdam")
  snap$name <- default_snapshot_name(snap$created_at)
  manifest_ams <- build_export_manifest(snap, generated_at = generated)
  expect_equal(manifest_ams$snapshot_captured_at, "2026-09-11T21:50:00Z")
  expect_equal(manifest_ams$snapshot_captured_at_display, "11 Sep 2026, 23:50 CEST")
  expect_equal(manifest_ams$export_generated_at_display, "11 Sep 2026, 23:51 CEST")
  expect_equal(manifest_ams$display_timezone, "Europe/Amsterdam")
  html_ams <- render_dossier_html(snap, manifest_ams)
  expect_match(html_ams, "Captured: 11 Sep 2026, 23:50 CEST", fixed = TRUE)
  expect_match(html_ams, "Generated: 11 Sep 2026, 23:51 CEST", fixed = TRUE)
  expect_true(grepl("11 Sep 2026, 23:50 CEST", snap$name, fixed = TRUE))

  local_generated <- as.POSIXct("2026-09-11 23:51:00", tz = "Europe/Amsterdam")
  manifest_local <- build_export_manifest(snap, generated_at = local_generated)
  html_local <- render_dossier_html(snap, manifest_local)
  expect_equal(manifest_local$export_generated_at, "2026-09-11T21:51:00Z")
  expect_match(html_local, "Generated: 11 Sep 2026, 23:51 CEST", fixed = TRUE)
})

test_that("stale-at-capture evidence is labelled in the dossier", {
  snap <- m10_fixture_snapshot(local_workspace(list(egfr_id = "egfr"), stale_overview = TRUE))
  html <- render_dossier_html(snap, build_export_manifest(snap))
  expect_match(html, "Stale at capture")
})

test_that("owner can export and another user cannot", {
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
  owner_html <- export_owned_snapshot(db_pool, seed$owner$id, created$snapshot_id, format = "html")
  expect_true(owner_html$ok)
  on.exit(unlink(owner_html$path), add = TRUE)
  owner_zip <- export_owned_snapshot(db_pool, seed$owner$id, created$snapshot_id, format = "zip")
  expect_true(owner_zip$ok)
  on.exit(unlink(owner_zip$path), add = TRUE)
  other_html <- export_owned_snapshot(db_pool, seed$other$id, created$snapshot_id, format = "html")
  expect_false(isTRUE(other_html$ok))
  other_zip <- export_owned_snapshot(db_pool, seed$other$id, created$snapshot_id, format = "zip")
  expect_false(isTRUE(other_zip$ok))
})

test_that("exports from the same snapshot stay scientifically equivalent after live edits", {
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
  first <- export_owned_snapshot(
    db_pool,
    seed$owner$id,
    created$snapshot_id,
    format = "html",
    generated_at = as.POSIXct("2026-09-11 15:02:00", tz = "UTC")
  )
  expect_true(first$ok)
  html_a <- paste(readLines(first$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(first$path)

  cache_put(
    db_pool,
    "uniprot",
    cache_key_uniprot_accession("P00533"),
    list(changed = TRUE, length_aa = 12L),
    200L,
    Sys.time(),
    Sys.time() + 3600
  )
  DBI::dbExecute(
    db_pool,
    "UPDATE projects SET title = 'Edited live title' WHERE id = $1::uuid",
    params = list(seed$project_id)
  )

  second <- export_owned_snapshot(
    db_pool,
    seed$owner$id,
    created$snapshot_id,
    format = "html",
    generated_at = as.POSIXct("2026-09-11 16:10:00", tz = "UTC")
  )
  expect_true(second$ok)
  html_b <- paste(readLines(second$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(second$path)

  strip_generated <- function(html) {
    html <- gsub("Generated: [^<]+", "Generated: STAMP", html)
    gsub("Export generated at: [^<]+", "Export generated at: STAMP", html)
  }
  expect_equal(strip_generated(html_a), strip_generated(html_b))
  expect_match(html_a, "Potential therapeutic targets in NSCLC")
  expect_false(grepl("Edited live title", html_b, fixed = TRUE))
  expect_match(html_b, "1210")
})

test_that("notes can be included and researcher email is never exported", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  save_user_profile(
    db_pool,
    seed$owner$id,
    display_name = "Ada Investigator",
    research_role = "Researcher",
    research_field = "Cancer biology",
    institution = "Example Institute"
  )
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Baseline review"
  )
  create_research_note(
    db_pool,
    seed$owner$id,
    seed$project_id,
    "Follow up coverage before the lab meeting.",
    scope = "snapshot",
    snapshot_id = created$snapshot_id
  )
  with_notes <- export_owned_snapshot(
    db_pool,
    seed$owner$id,
    created$snapshot_id,
    format = "html",
    include_notes = TRUE,
    include_researcher = TRUE
  )
  expect_true(with_notes$ok)
  html <- paste(readLines(with_notes$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(with_notes$path)
  expect_match(html, "Researcher note")
  expect_match(html, "Follow up coverage before the lab meeting")
  expect_match(html, "Ada Investigator")
  expect_false(grepl(seed$owner$email, html, fixed = TRUE))
  expect_false(grepl("@example.com", html, ignore.case = TRUE))

  without <- export_owned_snapshot(
    db_pool,
    seed$owner$id,
    created$snapshot_id,
    format = "html",
    include_notes = FALSE,
    include_researcher = FALSE
  )
  html2 <- paste(readLines(without$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(without$path)
  expect_false(grepl("Follow up coverage before the lab meeting", html2, fixed = TRUE))
  expect_false(grepl("Ada Investigator", html2, fixed = TRUE))
})

test_that("snapshot view has export actions and live research list does not", {
  snap <- m10_fixture_snapshot()
  view_html <- as.character(snapshot_view_ui(NS("snap"), snap))
  expect_match(view_html, "Research dossier \\(HTML\\)")
  expect_match(view_html, "Data package \\(ZIP\\)")
  expect_match(view_html, "Include research notes")
  expect_match(view_html, "Include researcher information")
  expect_false(grepl("Research dossier \\(PDF\\)", view_html))
  list_html <- as.character(snapshot_list_ui(NS("snap"), NULL, NULL, NULL))
  expect_false(grepl("Research dossier \\(HTML\\)", list_html))
  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  expect_false(grepl("download_html", home))
  research <- paste(readLines(file.path(app_root(), "R/modules/mod_research.R")), collapse = "\n")
  expect_match(research, "export_owned_snapshot")
  expect_false(grepl("httr2", research))
})

test_that("export numeric helpers keep missing values without coercion warnings", {
  expect_identical(export_scalar_num(NULL), NA_real_)
  expect_identical(export_scalar_num(list()), NA_real_)
  expect_identical(export_scalar_num(list(NULL)), NA_real_)
  expect_identical(export_scalar_num(NA), NA_real_)
  expect_identical(export_scalar_num(NA_real_), NA_real_)
  expect_identical(export_scalar_num("NA"), NA_real_)
  expect_identical(export_scalar_num("null"), NA_real_)
  expect_identical(export_scalar_num(""), NA_real_)
  expect_identical(export_scalar_num("Not provided"), NA_real_)
  expect_identical(export_scalar_num(0), 0)
  expect_identical(export_scalar_num("0.81"), 0.81)
  expect_identical(export_scalar_num(list(0.21)), 0.21)
  expect_no_warning({
    expect_identical(export_scalar_num("NA"), NA_real_)
    expect_identical(export_scalar_num("Not provided"), NA_real_)
  })
  expect_identical(csv_score(c(0, NA_real_, "NA", "0.5")), c(0, NA_real_, NA_real_, 0.5))
})

test_that("json_rows_to_df keeps omitted and JSON-null numeric cells as NA, not 0", {
  omitted <- jsonlite::fromJSON(
    '[{"symbol":"EGFR","score":0.8},{"symbol":"KRAS"}]',
    simplifyVector = FALSE
  )
  df_omitted <- json_rows_to_df(omitted)
  expect_equal(df_omitted$symbol, c("EGFR", "KRAS"))
  expect_equal(df_omitted$score[[1]], 0.8)
  expect_true(is.na(df_omitted$score[[2]]))
  expect_false(identical(df_omitted$score[[2]], 0))

  with_null <- jsonlite::fromJSON(
    '[{"symbol":"EGFR","score":0.8},{"symbol":"KRAS","score":null}]',
    simplifyVector = FALSE
  )
  df_null <- json_rows_to_df(with_null)
  expect_true(is.na(df_null$score[[2]]))
  expect_false(any(df_null$score == 0, na.rm = TRUE))
})

test_that("HTML export uses frozen snapshot data only", {
  files <- c(
    "R/export/export_dossier.R",
    "R/export/export_tables.R",
    "R/export/export_manifest.R",
    "R/modules/mod_research.R"
  )
  for (rel in files) {
    src <- paste(readLines(file.path(app_root(), rel), warn = FALSE), collapse = "\n")
    expect_false(grepl("httr2::", src), info = rel)
    expect_false(grepl("req_perform", src), info = rel)
    expect_false(grepl("resolve_target_identity", src), info = rel)
    expect_false(grepl("ot_search_diseases", src), info = rel)
    expect_false(grepl("cache_put\\(", src), info = rel)
  }
})

test_that("production-shaped four-target dossier renders without coercion warnings", {
  snap <- production_like_export_snapshot(json_na = "string")
  expect_equal(snap$overview$capture_status, "not_retrieved")
  expect_equal(snap$disease_evidence$capture_status, "not_retrieved")
  expect_equal(snap$comparison$capture_status, "captured")
  expect_equal(snap$pathways$capture_status, "captured")
  expect_equal(snap$literature$capture_status, "captured")
  expect_equal(snap$structures$capture_status, "captured")
  identity <- vapply(snap$target_identity$model$targets, function(row) {
    as.character(row$display_symbol %||% row$input_text)
  }, character(1))
  expect_equal(sort(identity), c("EGFR", "KRAS", "MET", "TP53"))

  expect_equal(nrow(snap$pathways$model$pathways), PATHWAY_MATRIX_ROW_CAP)
  expect_equal(
    length(unique(as.character(snap$pathways$model$membership_matrix$pathway_name))),
    PATHWAY_MATRIX_ROW_CAP
  )
  expect_equal(length(snap$literature$model$targets), 4L)
  expect_equal(nrow(snap$literature$model$targets[[1]]$recent_records), LITERATURE_RECENT_N)
  expect_equal(length(snap$structures$model$targets), 4L)
  expect_equal(length(snap$structures$model$targets[[1]]$records), STRUCTURE_COVERAGE_ROW_CAP)

  expect_no_warning({
    csv <- export_structures_csv(snap)
  })
  expect_equal(nrow(csv), 4L * STRUCTURE_COVERAGE_ROW_CAP)
  expect_true(sum(is.na(csv$coverage_fraction)) >= 4L * 8L)
  expect_false(any(csv$coverage_fraction == 0, na.rm = TRUE))
  expect_true(any(!is.na(csv$coverage_fraction)))

  cmp <- export_comparison_csv(snap)
  expect_true(any(is.na(cmp$broader_score)))
  expect_false(any(cmp$broader_score == 0, na.rm = TRUE))
  expect_false(any(cmp$direct_score == 0, na.rm = TRUE))

  result <- NULL
  expect_no_warning({
    result <- export_snapshot_artifact(snap, format = "html", include_notes = FALSE)
  })
  expect_true(result$ok)
  expect_true(file.exists(result$path))
  html <- paste(readLines(result$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(result$path)
  expect_match(html, "<html", ignore.case = TRUE)
  expect_match(html, "EGFR")
  expect_match(html, "KRAS")
  expect_match(html, "MET")
  expect_match(html, "TP53")
  expect_match(html, "Not retrieved before snapshot capture.")
  expect_match(html, "data:image/png;base64,")
  expect_false(grepl("NAs introduced by coercion", html, fixed = TRUE))
  expect_true(isTRUE(result$file_bytes > 0))
  expect_equal(result$metrics$plot_n, 12L)
})

test_that("legacy string NA and JSON-null snapshots both export missing coverage as missing", {
  for (mode in c("string", "null")) {
    snap <- production_like_export_snapshot(json_na = mode)
    csv <- NULL
    result <- NULL
    expect_no_warning({
      csv <- export_structures_csv(snap)
      result <- export_snapshot_artifact(snap, format = "html", include_notes = FALSE)
    })
    expect_true(result$ok, info = mode)
    unlink(result$path)
    expect_true(any(is.na(csv$coverage_fraction)), info = mode)
    expect_false(any(csv$coverage_fraction == 0, na.rm = TRUE), info = mode)
    inclusive <- export_comparison_csv(snap)$broader_score
    expect_true(any(is.na(inclusive)), info = mode)
    expect_false(any(inclusive == 0, na.rm = TRUE), info = mode)
  }
})
