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
