test_that("literature gate requires confirmed disease and one confirmed target", {
  project <- nsclc_literature_project()
  none <- confirmed_reactome_target("t1", "EGFR", "P00533", status = "ambiguous")
  expect_false(literature_gate(project, none)$ok)

  unresolved <- project
  unresolved$disease_resolution_status <- "unresolved"
  one <- confirmed_reactome_target("t1", "EGFR", "P00533")
  expect_false(literature_gate(unresolved, one)$ok)
  expect_true(literature_gate(project, one)$ok)
  expect_true(pathway_gate(one)$ok)
})

test_that("UniProt GeneID extraction requires exactly one human GeneID xref", {
  ids <- uniprot_ncbi_gene_ids(read_fixture("uniprot_p00533_geneid.json"))
  expect_equal(ids, "1956")
  expect_equal(uniprot_ncbi_gene_ids(read_fixture("uniprot_no_geneid.json")), character())
  expect_equal(length(uniprot_ncbi_gene_ids(read_fixture("uniprot_ambiguous_geneid.json"))), 2)
})

test_that("NCBI Gene verification accepts EGFR and rejects mouse or mismatched identity", {
  human <- parse_ncbi_gene_summary(read_fixture("ncbi_esummary_gene_1956.json"), "1956")
  expect_true(human$ok)
  expect_equal(human$symbol, "EGFR")
  expect_equal(human$taxid, 9606L)
  expect_true(verify_ncbi_gene(human, "EGFR")$ok)
  expect_false(verify_ncbi_gene(human, "KRAS")$ok)

  mouse <- parse_ncbi_gene_summary(read_fixture("ncbi_esummary_gene_mouse_egfr.json"), "13649")
  expect_false(verify_ncbi_gene(mouse, "EGFR")$ok)
})

test_that("NSCLC disease query uses confirmed terms and Title/Abstract, not all synonyms", {
  project <- nsclc_literature_project()
  terms <- build_literature_disease_terms(project)
  expect_true("non-small cell lung carcinoma" %in% terms)
  expect_true("non-small cell lung cancer" %in% terms)
  expect_false("NSCLC" %in% terms)
  query <- build_disease_pubmed_query(terms)
  expect_match(query, "\\[Title/Abstract\\]")
  expect_match(query, '"non-small cell lung carcinoma"')
  expect_match(query, '"non-small cell lung cancer"')
  expect_false(grepl("research question", query, ignore.case = TRUE))
  expect_false(grepl("Which target", query))
})

test_that("ELink history, ESearch count, and PubMed summaries parse live-shaped fixtures", {
  history <- parse_elink_history(read_fixture("ncbi_elink_gene_pubmed.json"))
  expect_true(history$ok)
  expect_false(history$empty)
  expect_equal(history$linkname, "gene_pubmed")

  counted <- parse_esearch_count(read_fixture("ncbi_esearch_count.json"))
  expect_equal(counted$count, 6029L)
  expect_true(counted$count < PUBMED_UID_RETRIEVAL_LIMIT || counted$count > PUBMED_UID_RETRIEVAL_LIMIT)
  expect_equal(PUBMED_UID_RETRIEVAL_LIMIT, 10000L)
  expect_true(LITERATURE_RECENT_N < PUBMED_UID_RETRIEVAL_LIMIT)

  empty <- parse_esearch_count(read_fixture("ncbi_esearch_empty.json"))
  expect_equal(empty$count, 0L)

  recent <- parse_pubmed_summaries(read_fixture("ncbi_esummary_pubmed.json"))
  expect_true(recent$ok)
  expect_true(nrow(recent$records) >= 1)
  expect_true(all(nzchar(recent$records$title)))
  expect_true(all(grepl("pubmed.ncbi.nlm.nih.gov", recent$records$pubmed_url)))
  expect_false(any(is.na(recent$records$first_author)))
  expect_false(any(is.na(recent$records$doi)))
  sparse <- recent$records[recent$records$doi == "DOI not provided", ]
  expect_true(nrow(sparse) >= 1)
})

test_that("EGFR literature is retrieved through verified GeneID, not a symbol query", {
  skip_if_not_installed("httr2")
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533", "ENSG00000146648")
  result <- retrieve_project_literature(
    project,
    target,
    db_pool = NULL,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  expect_equal(result$status, "ready")
  item <- result$literature$targets[[1]]
  expect_equal(item$target$ncbi_gene_id, "1956")
  expect_equal(item$corpus$total_count, 6029L)
  expect_false(grepl("EGFR\\[Title/Abstract\\]", item$corpus$disease_query))
  expect_false(grepl("MET\\[Title/Abstract\\]", item$corpus$disease_query))
  expect_match(item$provenance$link_strategy, "gene_pubmed")
  expect_equal(item$corpus$uid_retrieval_limit, 10000L)
  expect_true(nrow(item$recent_records) >= 1)
  expect_false(grepl("webenv", tolower(item$provenance$retrieval_method)))
})

test_that("missing GeneID does not fall back to a symbol PubMed query", {
  skip_if_not_installed("httr2")
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533")
  result <- retrieve_project_literature(
    project,
    target,
    db_pool = NULL,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_no_geneid.json"))
  )
  expect_equal(result$literature$excluded_targets[[1]]$message, "Literature mapping unavailable for this target.")
  expect_equal(nrow(result$literature$target_counts), 0)
})

test_that("trend keeps valid zeros and does not convert failure to zero", {
  trend <- data.frame(
    year = c(2018L, 2019L, 2020L, 2021L),
    record_count = c(2L, 4L, 0L, 7L),
    is_partial_year = c(FALSE, FALSE, FALSE, TRUE),
    status = "ok",
    stringsAsFactors = FALSE
  )
  expect_equal(trend$record_count[trend$year == 2020L], 0L)
  failed <- trend
  failed$record_count[failed$year == 2020L] <- NA_integer_
  failed$status[failed$year == 2020L] <- "error"
  expect_true(is.na(failed$record_count[failed$year == 2020L]))
  expect_false(identical(failed$record_count[failed$year == 2020L], 0L))

  skip_if_not_installed("httr2")
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533")
  result <- retrieve_target_literature(
    target,
    literature_disease_context(project),
    db_pool = NULL,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  expect_equal(result$literature$trend$record_count[result$literature$trend$year == 2020L], 0L)
  expect_true(any(result$literature$trend$is_partial_year))
  years <- sort(unique(as.integer(result$literature$trend$year)))
  expect_equal(length(years), LITERATURE_TREND_YEARS)
  expect_equal(min(years), max(years) - LITERATURE_TREND_YEARS + 1L)
  expect_equal(length(years), length(result$literature$trend$year))
})

test_that("project comparison keeps real zeros and does not rank failures", {
  counts <- data.frame(
    project_target_id = c("a", "b", "d"),
    symbol = c("A", "B", "D"),
    ncbi_gene_id = c("1", "2", "4"),
    pubmed_record_count = c(40L, 5L, 0L),
    status = c("ok", "ok", "empty"),
    stringsAsFactors = FALSE
  )
  expect_equal(counts$pubmed_record_count, c(40L, 5L, 0L))
  expect_false("rank" %in% names(counts))
  failures <- list(list(project_target_id = "c", symbol = "C", message = "Literature retrieval unavailable for C."))
  expect_false("C" %in% counts$symbol)
  expect_equal(failures[[1]]$symbol, "C")
})

test_that("NCBI API keys are not used in cache keys or sanitized errors", {
  expect_equal(cache_key_ncbi_gene_verify("1956"), "ncbi:gene-verify:1956")
  expect_false(grepl("api_key", cache_key_pubmed_corpus("1956", "tiab:nsclc")))
  redacted <- sanitize_ncbi_error("failed api_key=SECRETKEY extra", api_key = "SECRETKEY")
  expect_false(grepl("SECRETKEY", redacted))
})

test_that("Literature navigation exists and the module does not call httr2", {
  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  mod <- paste(readLines(file.path(app_root(), "R/modules/mod_literature.R")), collapse = "\n")
  expect_match(home, "Literature")
  expect_match(home, "mod_literature_ui")
  expect_false(grepl("httr2", mod))
  expect_equal(workspace_panel_label("literature"), "Literature")
  expect_equal(workspace_back_destination("literature"), "project")
})

test_that("stale PubMed corpus is reused when NCBI fails", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533")
  live <- retrieve_target_literature(
    target,
    literature_disease_context(project),
    db_pool = db_pool,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  expect_true(live$status %in% c("ok", "empty"))

  DBI::dbExecute(
    db_pool,
    "
    UPDATE api_cache
    SET expires_at = NOW() - INTERVAL '1 hour'
    WHERE cache_key LIKE 'test:pubmed:%'
    "
  )

  stale <- retrieve_target_literature(
    target,
    literature_disease_context(project),
    db_pool = db_pool,
    perform = ncbi_fixture_perform(fail_esearch = TRUE),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  expect_equal(stale$literature$provenance$cache_status, "stale")
  expect_equal(stale$literature$corpus$total_count, live$literature$corpus$total_count)
})

test_that("literature summary metrics use existing counts and keep missing distinct from zero", {
  source_app("R/modules/mod_literature.R")
  expect_equal(evidence_count_label(0L), "0")
  expect_null(evidence_count_label(NA_integer_))
  expect_null(evidence_count_label(NULL))

  missing_html <- paste(as.character(evidence_metric("Records in current calendar year", NULL)), collapse = "\n")
  expect_match(missing_html, "Not retrieved")
  expect_match(missing_html, "is-missing")

  zero_html <- paste(as.character(evidence_metric("Records in current calendar year", "0")), collapse = "\n")
  expect_match(zero_html, ">0<")
  expect_false(grepl("is-missing", zero_html))

  trend <- data.frame(
    year = c(2024L, 2025L),
    record_count = c(4L, 0L),
    is_partial_year = c(FALSE, TRUE),
    status = "ok",
    stringsAsFactors = FALSE
  )
  year_ok <- literature_current_year_summary(trend)
  expect_equal(year_ok$value, 0L)
  expect_equal(year_ok$year, 2025L)
  failed <- trend
  failed$record_count[2] <- NA_integer_
  failed$status[2] <- "error"
  year_fail <- literature_current_year_summary(failed)
  expect_null(year_fail$value)
  expect_equal(year_fail$status, "missing")
  window <- literature_trend_window_summary(trend)
  expect_match(window$value, "2024")
  expect_match(window$value, "2025")
  expect_equal(window$n_years, 2L)
  one <- literature_trend_window_summary(trend[trend$year == 2025L, , drop = FALSE])
  expect_equal(one$n_years, 1L)
  expect_equal(one$value, "2025")
  expect_false(identical(one$n_years, LITERATURE_TREND_YEARS))
})

test_that("literature workspace preserves counts, caveats, selector, and disclosures", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("shiny")
  source_app("R/modules/mod_literature.R")
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533", "ENSG00000146648")
  result <- retrieve_project_literature(
    project,
    target,
    db_pool = NULL,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  item <- result$literature$targets[[1]]
  html <- paste(as.character(literature_result_ui(
    shiny::NS("literature"),
    result,
    item,
    item$target$project_target_id
  )), collapse = "\n")

  expect_match(html, "Matched PubMed records")
  expect_match(html, evidence_count_label(item$corpus$total_count), fixed = TRUE)
  expect_match(html, "All records matching this search definition as of the recorded retrieval")
  expect_match(html, sprintf("Records in %s", max(as.integer(item$trend$year))))
  expect_match(html, "Current calendar year; partial year")
  expect_false(grepl("Trend window", html))
  expect_match(html, "not a cumulative")
  expect_false(grepl("is cumulative", html, ignore.case = TRUE))
  expect_match(html, "partial year")
  expect_match(html, "How to interpret these results")
  expect_match(html, "do not measure evidence quality")
  expect_false(grepl("interpretation-guidance[^>]*open", html))
  expect_match(html, "literature_target")
  expect_match(html, "evidence-target-switcher")
  expect_match(html, 'type="radio"')
  expect_match(html, "Search definition")
  expect_match(html, "P00533")
  expect_match(html, "1956")
  expect_match(html, "non-small cell lung carcinoma")
  expect_match(html, "gene_pubmed")
  expect_match(html, "Title/Abstract")
  expect_match(html, "NCBI PubMed")
  expect_match(html, "Technical provenance")
  expect_match(html, "Publication activity")
  expect_match(html, "Annual PubMed record count")
  expect_match(html, "Recent PubMed records")
  expect_match(html, "literature_year")
  expect_match(html, "All years")
  expect_match(html, "No additional year filter")
  expect_match(html, "data-literature-year")
  expect_match(html, "matched records")
  expect_match(html, "Open in PubMed")
  expect_match(html, "pubmed.ncbi.nlm.nih.gov")
  expect_match(html, "Literature volume is not target importance")
  expect_match(html, "tw-trend-svg")
  expect_match(html, "tw-trend-table")
  expect_match(html, "Calendar year")
  n_ok <- sum(item$trend$status == "ok")
  expect_equal(n_ok, LITERATURE_TREND_YEARS)
  expect_match(html, "10-year publication activity")
  for (year in item$trend$year) {
    expect_match(html, as.character(year))
  }
  expect_false(grepl("recommend|best target|TargetWeave score", html, ignore.case = TRUE))
  expect_false(grepl("Defined literature corpus", html))
  expect_match(html, "<summary")
  expect_false(grepl("evidence-details[^>]*\\sopen", html))
  expect_match(html, "evidence-primary-surface")
  expect_false(grepl("ggplot", html, ignore.case = TRUE))
})

test_that("single-year publication activity is not labeled as a 10-year trend", {
  source_app("R/modules/mod_literature.R")
  trend <- data.frame(
    year = 2026L,
    record_count = 415L,
    is_partial_year = TRUE,
    status = "ok",
    stringsAsFactors = FALSE
  )
  html <- export_lite_trend_html(trend, "EGFR")
  expect_match(html, "415")
  expect_match(html, "2026")
  expect_match(html, "Historical annual counts were not retrieved")
  expect_match(html, "not a cumulative")
  expect_false(grepl("10-year", html))
  expect_false(grepl("Ten-year", html))
  expect_false(grepl("tw-trend-svg", html))

  mixed <- data.frame(
    year = c(2024L, 2025L, 2026L),
    record_count = c(NA_integer_, 0L, 8L),
    is_partial_year = c(FALSE, FALSE, TRUE),
    status = c("error", "ok", "ok"),
    stringsAsFactors = FALSE
  )
  mixed_html <- export_lite_trend_html(mixed, "EGFR")
  expect_match(mixed_html, "Not retrieved")
  expect_match(mixed_html, ">0<")
  expect_false(grepl("10-year", mixed_html))
  window <- literature_trend_window_summary(mixed)
  expect_equal(window$n_years, 2L)
})

test_that("year options come from retrieved annual evidence and default to all years", {
  source_app("R/modules/mod_literature.R")
  requested <- annual_literature_requested_years(2026L)
  expect_equal(as.integer(requested), 2017:2026)
  trend <- data.frame(
    year = requested,
    record_count = ifelse(requested == 2020L, NA_integer_, as.integer(requested - 2000L)),
    is_partial_year = requested == 2026L,
    status = ifelse(requested == 2020L, "error", "ok"),
    stringsAsFactors = FALSE
  )
  trend$record_count[trend$year == 2018L] <- 0L
  choices <- literature_year_choices(trend, current_year = 2026L)
  expect_equal(unname(choices)[[1]], literature_all_years_value())
  expect_equal(names(choices)[[1]], "All years")
  expect_false("2016" %in% unname(choices))
  expect_false("2020" %in% unname(choices))
  expect_equal(length(unname(choices)[unname(choices) == "2026"]), 1L)
  expect_true("2017" %in% unname(choices))
  expect_true("2018" %in% unname(choices))
  expect_match(names(choices)[unname(choices) == "2026"], "partial")
  expect_equal(normalize_literature_year(NULL, trend), "all")
  expect_equal(normalize_literature_year("2020", trend), "all")
  expect_equal(normalize_literature_year("2017", trend), "2017")
  expect_equal(literature_year_filter_label("all"), "No additional year filter")
  expect_equal(literature_year_filter_label("2017"), "2017")
})

test_that("selected-year retrieval adds only a pdat year constraint to the same corpus", {
  skip_if_not_installed("httr2")
  project <- nsclc_literature_project()
  disease <- literature_disease_context(project)
  captured <- character()
  inner <- ncbi_fixture_perform()
  perform <- function(req) {
    captured <<- c(captured, ncbi_url_text(req))
    inner(req)
  }
  query_sig <- literature_query_signature(disease$search_terms)
  result <- retrieve_literature_year_records(
    "1956",
    disease,
    2017,
    db_pool = NULL,
    perform = perform
  )
  expect_true(result$ok)
  pack <- result$literature
  expect_equal(pack$year, 2017L)
  expect_equal(pack$mindate, "2017")
  expect_equal(pack$maxdate, "2017")
  expect_equal(pack$datetype, "pdat")
  expect_equal(pack$disease_query, disease$disease_query)
  expect_equal(literature_query_signature(disease$search_terms), query_sig)
  expect_equal(pack$total_count, 3L)
  expect_false(identical(pack$total_count, 6029L))
  year_urls <- captured[grepl("esearch", captured) & grepl("mindate=", captured)]
  expect_true(length(year_urls) >= 1)
  expect_true(all(grepl("mindate=2017", year_urls)))
  expect_true(all(grepl("maxdate=2017", year_urls)))
  expect_true(all(grepl("datetype=pdat", year_urls)))
  expect_false(any(grepl("EGFR\\[Title/Abstract\\]", captured)))
  expect_true(any(grepl("elink", captured)))
})

test_that("selected-year count is annual, missing years stay missing, and current year stays partial", {
  skip_if_not_installed("httr2")
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533")
  disease <- literature_disease_context(project)
  overview <- retrieve_target_literature(
    target,
    disease,
    db_pool = NULL,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  trend <- overview$literature$trend
  expect_equal(trend$record_count[trend$year == 2020L], 0L)
  expect_true(any(trend$is_partial_year))
  year_2018 <- retrieve_literature_year_records(
    "1956",
    disease,
    2018,
    perform = ncbi_fixture_perform()
  )
  expect_equal(year_2018$literature$total_count, 2L)
  expect_equal(year_2018$literature$total_count, trend$record_count[trend$year == 2018L])
  year_2020 <- retrieve_literature_year_records("1956", disease, 2020, perform = ncbi_fixture_perform())
  expect_equal(year_2020$literature$total_count, 0L)
  expect_equal(year_2020$literature$status, "empty")
})

test_that("all-years UI keeps recent records and selected year shows year-filtered pages", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("shiny")
  source_app("R/modules/mod_literature.R")
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533", "ENSG00000146648")
  result <- retrieve_project_literature(
    project,
    target,
    db_pool = NULL,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  item <- result$literature$targets[[1]]
  ns <- shiny::NS("literature")
  all_html <- paste(as.character(literature_result_ui(ns, result, item, item$target$project_target_id)), collapse = "\n")
  expect_match(all_html, "Recent PubMed records from the full defined corpus")
  expect_match(all_html, "No additional year filter")
  expect_false(grepl("Publications in 2017", all_html))

  year_pack <- literature_year_pack(
    year = 2017,
    records = item$recent_records[seq_len(min(2L, nrow(item$recent_records))), , drop = FALSE],
    total_count = 128L,
    disease_query = item$corpus$disease_query,
    gene_id = item$target$ncbi_gene_id
  )
  year_html <- paste(as.character(literature_result_ui(
    ns,
    result,
    item,
    item$target$project_target_id,
    selected_year = "2017",
    year_pack = year_pack
  )), collapse = "\n")
  expect_match(year_html, "Publications in 2017")
  expect_match(year_html, "128 records match the current literature definition")
  expect_match(year_html, "Showing 1")
  expect_match(year_html, "not the complete year corpus")
  expect_match(year_html, "Load more")
  expect_match(year_html, ">2017<")
  expect_false(grepl("No additional year filter", year_html))
  expect_false(grepl("hot target|growth score|trend score", year_html, ignore.case = TRUE))
  expect_match(year_html, "do not measure evidence quality")
  loading_html <- paste(as.character(literature_result_ui(
    ns,
    result,
    item,
    item$target$project_target_id,
    selected_year = "2017",
    year_loading = TRUE,
    year_progress = "Loading publications from 2017\u2026"
  )), collapse = "\n")
  expect_match(loading_html, "Loading publications from 2017")
  expect_match(loading_html, "Publication activity")
  expect_match(loading_html, "Matched PubMed records")
})

test_that("selected-year records cache per identity and changing year keeps the corpus cache", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533")
  disease <- literature_disease_context(project)
  live <- retrieve_target_literature(
    target,
    disease,
    db_pool = db_pool,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  expect_true(live$status %in% c("ok", "empty"))
  first <- retrieve_literature_year_records("1956", disease, 2018, db_pool = db_pool, perform = ncbi_fixture_perform())
  expect_false(isTRUE(first$from_cache))
  hits <- 0L
  cached <- retrieve_literature_year_records(
    "1956",
    disease,
    2018,
    db_pool = db_pool,
    perform = function(req) {
      hits <<- hits + 1L
      stop("year cache should not call NCBI")
    }
  )
  expect_true(cached$from_cache)
  expect_equal(hits, 0L)
  expect_equal(cached$literature$total_count, first$literature$total_count)
  other <- retrieve_literature_year_records("1956", disease, 2019, db_pool = db_pool, perform = ncbi_fixture_perform())
  expect_equal(other$literature$year, 2019L)
  keys <- DBI::dbGetQuery(db_pool, "SELECT cache_key FROM api_cache")$cache_key
  expect_true(any(grepl("pubmed:target-disease:", keys)))
  expect_true(any(grepl("pubmed:year-records:1956:.+:2018$", keys)))
  expect_true(any(grepl("pubmed:year-records:1956:.+:2019$", keys)))
})

test_that("year pagination uses retstart and does not imply completeness", {
  skip_if_not_installed("httr2")
  project <- nsclc_literature_project()
  disease <- literature_disease_context(project)
  captured <- character()
  inner <- ncbi_fixture_perform()
  perform <- function(req) {
    captured <<- c(captured, ncbi_url_text(req))
    inner(req)
  }
  first <- retrieve_literature_year_records("1956", disease, 2021, perform = perform, page_size = 2L)
  expect_true(first$literature$truncated)
  expect_equal(first$literature$total_count, 7L)
  expect_true(first$literature$shown < first$literature$total_count)
  expect_false(any(grepl("retstart=", captured)))
  captured <- character()
  retrieve_literature_year_records(
    "1956",
    disease,
    2021,
    perform = perform,
    already_have = 2L,
    page_size = 2L
  )
  expect_true(any(grepl("retstart=2", captured)))
})

pubmed_record_frame <- function(title) {
  data.frame(
    pmid = "41593906",
    title = title,
    first_author = "Liu Y",
    journal = "Ann Med",
    publication_date = "2026 Dec",
    publication_year = "2026",
    doi = "DOI not provided",
    pubmed_url = "https://pubmed.ncbi.nlm.nih.gov/41593906/",
    stringsAsFactors = FALSE
  )
}

test_that("PubMed titles are normalized to plain text without changing identifiers", {
  skip_if_not_installed("shiny")
  source_app("R/modules/mod_literature.R")
  expect_equal(
    normalize_pubmed_title("<p>Immunotherapy after EGFR-TKI treatment</p>"),
    "Immunotherapy after EGFR-TKI treatment"
  )
  expect_equal(normalize_pubmed_title("AT&amp;T-style example"), "AT&T-style example")
  expect_equal(normalize_pubmed_title("<i>EGFR</i> and nested <b>markup</b>"), "EGFR and nested markup")
  expect_equal(normalize_pubmed_title("a &lt; b &gt; c"), "a < b > c")
  expect_equal(normalize_pubmed_title("say &#39;ok&#39; and &quot;go&quot;"), "say 'ok' and \"go\"")
  expect_equal(
    normalize_pubmed_title("<script>alert('x')</script>Target study"),
    "Target study"
  )

  payload <- list(
    result = list(
      uids = list("1"),
      `1` = list(
        uid = "1",
        title = "<p>Immunotherapy after EGFR-TKI treatment</p>",
        source = "Ann Med",
        pubdate = "2026 Dec",
        sortfirstauthor = "Liu Y"
      )
    )
  )
  parsed <- parse_pubmed_summaries(payload)
  expect_true(parsed$ok)
  expect_equal(parsed$records$pmid[[1]], "1")
  expect_equal(parsed$records$journal[[1]], "Ann Med")
  expect_equal(parsed$records$publication_date[[1]], "2026 Dec")
  expect_equal(parsed$records$title[[1]], "Immunotherapy after EGFR-TKI treatment")

  dirty <- pubmed_record_frame("<p>Immunotherapy after EGFR-TKI treatment</p>")
  recent <- paste(as.character(literature_recent_ui(dirty)), collapse = "\n")
  year <- paste(
    as.character(literature_recent_ui(
      dirty,
      title = "Publications in 2021",
      empty_title = "No source result for 2021"
    )),
    collapse = "\n"
  )
  expect_match(recent, "Immunotherapy after EGFR-TKI treatment")
  expect_match(year, "Immunotherapy after EGFR-TKI treatment")
  expect_false(grepl("<p>", recent, fixed = TRUE))
  expect_false(grepl("</p>", recent, fixed = TRUE))
  expect_false(grepl("<p>", year, fixed = TRUE))
  expect_match(recent, "41593906")
  expect_match(recent, "Ann Med")
  expect_match(recent, "2026 Dec")

  scripted <- paste(
    as.character(literature_recent_ui(pubmed_record_frame("<script>alert('x')</script>Target study"))),
    collapse = "\n"
  )
  expect_false(grepl("<script>", scripted, fixed = TRUE))
  expect_match(scripted, "Target study")
})
