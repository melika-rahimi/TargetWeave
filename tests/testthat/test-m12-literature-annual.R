test_that("ESearch annual count uses total Count, never retmax or ID length", {
  payload <- list(
    header = list(type = "esearch", version = "0.3"),
    esearchresult = list(
      count = "417",
      retmax = "50",
      retstart = "0",
      idlist = as.character(seq_len(50))
    )
  )
  counted <- parse_esearch_total_count(payload)
  expect_equal(counted$count, 417L)
  expect_false(identical(counted$count, 50L))
  ids <- parse_esearch_ids(payload)
  expect_equal(length(ids$ids), 50L)
  expect_equal(ids$count, 417L)
})

test_that("annual series validation rejects duplicates and preserves zero vs missing", {
  requested <- annual_literature_requested_years(2026L)
  expect_equal(requested, 2017:2026)
  results <- lapply(requested, function(year) {
    status <- if (identical(year, 2020L)) "error" else "ok"
    count <- if (identical(year, 2020L)) {
      NA_integer_
    } else if (identical(year, 2018L)) {
      0L
    } else if (identical(year, 2024L)) {
      1500L
    } else {
      as.integer(year)
    }
    annual_literature_result(year, 2026L, status, count = count)
  })
  ok <- validate_annual_literature_series(results, requested, 2026L)
  expect_true(ok$ok)
  expect_equal(ok$trend$year, requested)
  expect_equal(ok$trend$record_count[ok$trend$year == 2018L], 0L)
  expect_true(is.na(ok$trend$record_count[ok$trend$year == 2020L]))
  expect_equal(ok$trend$record_count[ok$trend$year == 2024L], 1500L)
  expect_true(ok$trend$is_partial_year[ok$trend$year == 2026L])
  expect_false(any(ok$trend$is_partial_year[ok$trend$year != 2026L]))

  dup_requested <- c(2017L, 2017L)
  expect_false(validate_annual_literature_series(results[1:2], dup_requested, 2026L)$ok)

  dup_results <- results
  dup_results[[2]]$year <- 2017L
  expect_false(validate_annual_literature_series(dup_results, requested, 2026L)$ok)

  zero_as_error <- results
  zero_as_error[[which(requested == 2020L)]]$count <- 0L
  expect_false(validate_annual_literature_series(zero_as_error, requested, 2026L)$ok)

  historical_partial <- results
  historical_partial[[1]]$partial_year <- TRUE
  expect_false(validate_annual_literature_series(historical_partial, requested, 2026L)$ok)
})

test_that("out-of-order async annual completion keeps count attached to year", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  requested <- 2017:2026
  counts <- c(
    `2017` = 117L, `2018` = 218L, `2019` = 319L, `2020` = 420L, `2021` = 521L,
    `2022` = 622L, `2023` = 723L, `2024` = 824L, `2025` = 925L, `2026` = 1026L
  )
  completion <- c(2021L, 2018L, 2026L, 2017L, 2024L, 2019L, 2025L, 2022L, 2020L, 2023L)
  delays <- stats::setNames(seq_along(completion) * 0.01, as.character(completion))
  tasks <- lapply(requested, function(year_value) {
    year_value <- as.integer(year_value)[[1]]
    promises::promise(function(resolve, reject) {
      later::later(function() {
        resolve(annual_literature_result(year_value, 2026L, "ok", count = counts[[as.character(year_value)]]))
      }, delays[[as.character(year_value)]])
    })
  })
  assembled <- NULL
  promises::then(tw_list_all(tasks), function(results) {
    assembled <<- validate_annual_literature_series(results, requested, 2026L)
  })
  drain_tw_later(3)
  expect_true(isTRUE(assembled$ok))
  expect_equal(assembled$trend$year, requested)
  expect_equal(assembled$trend$record_count, unname(counts[as.character(requested)]))
  expect_equal(unique(assembled$trend$year), requested)
})

test_that("invalid annual series refuses a misleading visualization and duplicate year options", {
  source_app("R/modules/mod_literature.R")
  skip_if_not_installed("shiny")
  corrupt <- data.frame(
    year = rep(2026L, 10),
    record_count = c(415L, rep(50L, 9)),
    is_partial_year = TRUE,
    status = "ok",
    stringsAsFactors = FALSE
  )
  expect_false(validate_annual_literature_series(
    annual_results_from_trend(corrupt),
    annual_literature_requested_years(2026L),
    2026L
  )$ok)
  choices <- literature_year_choices(corrupt, current_year = 2026L)
  expect_equal(unname(choices), literature_all_years_value())
  expect_equal(length(choices), 1L)

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
  item$annual_series <- NULL
  item$trend <- corrupt
  item$trend_ok <- FALSE
  item$trend_error <- "Annual publication activity could not be assembled correctly."
  html <- paste(as.character(literature_result_ui(
    shiny::NS("literature"),
    result,
    item,
    item$target$project_target_id
  )), collapse = "\n")
  expect_match(html, "Annual publication activity could not be assembled correctly")
  expect_false(grepl("tw-trend-svg", html))
  expect_false(grepl("2026 \\(partial\\).*2026 \\(partial\\)", html))
})

test_that("annual cache namespace is v3 and ignores legacy and v2 trend rows", {
  expect_equal(
    cache_key_pubmed_trend("1956", "tiab:nsclc", 2017L, 2026L),
    "pubmed:trend:v3:1956:tiab:nsclc:2017:2026"
  )
  expect_equal(
    cache_key_pubmed_trend_v2("1956", "tiab:nsclc", 2017L, 2026L),
    "pubmed:trend:v2:1956:tiab:nsclc:2017:2026"
  )
  expect_false(grepl("pubmed:trend:v3:", cache_key_pubmed_trend_legacy("1956", "tiab:nsclc", 2017L, 2026L)))
  expect_false(grepl("pubmed:trend:v3:", cache_key_pubmed_trend_v2("1956", "tiab:nsclc", 2017L, 2026L)))
  expect_equal(cache_key_pubmed_corpus("1956", "tiab:nsclc"), "pubmed:target-disease:1956:tiab:nsclc")
  expect_match(cache_key_pubmed_year_records("1956", "tiab:nsclc", 2021L), "^pubmed:year-records:1956:tiab:nsclc:2021$")
  expect_equal(cache_key_pubmed_info(), "ncbi:pubmed-info")

  src <- paste(readLines(file.path(app_root(), "R/process/process_literature.R")), collapse = "\n")
  expect_match(src, "trend_key <- cache_key_pubmed_trend")
  expect_false(grepl("cache_key_pubmed_trend_legacy\\(", src))
  expect_false(grepl("cache_key_pubmed_trend_v2\\(", src))
  expect_false(grepl("pubmed:trend:v2:", src))

  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533")
  disease <- literature_disease_context(project)
  query_sig <- literature_query_signature(disease$search_terms)
  broken <- data.frame(
    year = rep(2026L, 10),
    record_count = c(415L, rep(50L, 9)),
    is_partial_year = TRUE,
    status = "ok",
    stringsAsFactors = FALSE
  )
  zeros <- data.frame(
    year = 2017:2026,
    record_count = 0L,
    is_partial_year = 2017:2026 == 2026L,
    status = "ok",
    stringsAsFactors = FALSE
  )
  put_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    cache_key_pubmed_trend_legacy("1956", query_sig, 2017L, 2026L),
    list(trend = broken, start_year = 2017L, end_year = 2026L),
    3600L
  )
  put_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    cache_key_pubmed_trend_v2("1956", query_sig, 2017L, 2026L),
    list(trend = zeros, start_year = 2017L, end_year = 2026L, cache_namespace = "pubmed:trend:v2"),
    3600L
  )
  live <- retrieve_target_literature(
    target,
    disease,
    db_pool = db_pool,
    perform = ncbi_fixture_perform(),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  years <- as.integer(live$literature$trend$year)
  expect_equal(length(unique(years)), length(years))
  expect_false(identical(unique(years), 2026L))
  expect_false(all(live$literature$trend$record_count == 0L))
  expect_true(isTRUE(live$literature$trend_ok))
  written <- read_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    cache_key_pubmed_trend("1956", query_sig, 2017L, 2026L)
  )
  expect_false(is.null(written))
  expect_equal(written$data$cache_namespace, "pubmed:trend:v3")
  leftover_v2 <- read_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    cache_key_pubmed_trend_v2("1956", query_sig, 2017L, 2026L)
  )
  expect_false(is.null(leftover_v2))
})

test_that("literature target switcher hides native radios and centers visible labels", {
  source_app("R/modules/mod_literature.R")
  choices <- target_selector_choices(
    c("t1", "t2", "t3", "t4", "t5"),
    c("EGFR", "KRAS", "MET", "TP53", "ALK")
  )
  html <- paste(as.character(evidence_target_switcher_ui("literature-literature_target", "Target", choices, "t1")), collapse = "\n")
  expect_match(html, "target-switch-item")
  expect_match(html, "target-switch-label")
  expect_match(html, "target-switch-input")
  expect_match(html, 'type="radio"')
  expect_match(html, 'role="radiogroup"')
  expect_equal(length(gregexpr('value="t1"', html)[[1]]), 1L)
  expect_equal(length(gregexpr("EGFR", html)[[1]]), 1L)
  css <- paste(readLines(file.path(app_root(), "www/styles.css")), collapse = "\n")
  expect_match(css, "input\\.target-switch-input\\[type=\"radio\"\\]")
  expect_match(css, "appearance: none !important")
  expect_match(css, "\\.target-switch-label \\{[^}]*justify-content: center")
  expect_match(css, "\\.target-switch-label \\{[^}]*align-items: center")
  expect_match(css, ":checked\\) \\.target-switch-label")
  expect_false(grepl("margin-left:\\s*-", css))
  expect_false(grepl("translateX", css))
})

test_that("canonical annual UI model is the only series literature_result_ui renders", {
  skip_if_not_installed("shiny")
  source_app("R/modules/mod_literature.R")
  requested <- 2017:2026
  counts <- c(117L, 218L, 319L, 420L, 521L, 622L, 723L, 824L, 925L, 1026L)
  names(counts) <- as.character(requested)
  completion <- c(2021L, 2018L, 2026L, 2017L, 2024L, 2019L, 2025L, 2022L, 2020L, 2023L)
  results <- lapply(completion, function(year) {
    annual_literature_result(year, 2026L, "ok", count = unname(counts[[as.character(year)]]))
  })
  series <- validate_annual_literature_series(results, requested, 2026L)
  expect_true(series$ok)
  expect_equal(series$trend$year, requested)
  expect_equal(series$trend$record_count, unname(counts))

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
  item$trend <- data.frame(
    year = rep(2026L, 10),
    record_count = c(415L, rep(50L, 9)),
    is_partial_year = TRUE,
    status = "ok",
    stringsAsFactors = FALSE
  )
  item$annual_series <- series
  ui_series <- literature_annual_ui_model(item, current_year = 2026L)
  expect_equal(ui_series$trend$year, requested)
  expect_equal(ui_series$trend$record_count, unname(counts))
  html <- paste(as.character(literature_result_ui(
    shiny::NS("literature"),
    result,
    item,
    item$target$project_target_id
  )), collapse = "\n")
  expect_match(html, LITERATURE_RUNTIME_MARKER)
  expect_match(html, "2017\u20132026")
  expect_equal(length(gregexpr("2026 \\(partial\\)", html)[[1]]), 1L)
  expect_true(grepl("value=\"2017\"", html))
  expect_true(grepl(">2017<", html))
  expect_false(grepl("Only one calendar year was retrieved", html))
  expect_match(html, "tw-trend-svg")
  expect_match(html, "View annual counts")
  expect_match(html, "target-switch-item")
  expect_false(grepl("radioButtons", paste(readLines(file.path(app_root(), "R/modules/mod_literature.R")), collapse = "\n")))
})

test_that("annual retrieve uses ESearch Count per bound year even when ID page is 50", {
  skip_if_not_installed("httr2")
  requested <- 2017:2026
  counts <- as.integer(requested - 1900L)
  names(counts) <- as.character(requested)
  captured <- list()
  perform <- function(req) {
    url <- ncbi_url_text(req)
    year <- sub(".*mindate=([0-9]{4}).*", "\\1", url)
    captured[[length(captured) + 1L]] <<- list(url = url, year = year)
    n <- unname(counts[[year]])
    payload <- list(
      header = list(type = "esearch", version = "0.3"),
      esearchresult = list(
        count = as.character(n),
        retmax = "50",
        retstart = "0",
        idlist = as.character(seq_len(50))
      )
    )
    httr2::response(
      status_code = 200L,
      url = url,
      method = "GET",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")))
    )
  }
  assembled <- retrieve_annual_literature_counts(
    term = "#1 AND (test[Title/Abstract])",
    webenv = "webenv-test",
    requested_years = requested,
    current_year = 2026L,
    perform = perform
  )
  expect_true(assembled$ok)
  expect_equal(assembled$trend$year, requested)
  expect_equal(assembled$trend$record_count, unname(counts))
  expect_false(any(assembled$trend$record_count == 50L))
  expect_true(length(captured) >= 10L)
  year_urls <- vapply(captured, function(x) x$url, character(1))
  expect_true(all(grepl("rettype=count", year_urls)))
  expect_true(all(grepl("retmax=0", year_urls)))
  expect_true(all(grepl("usehistory=y", year_urls)))
  expect_true(all(grepl("WebEnv=", year_urls)))
  for (year in requested) {
    hit <- Filter(function(x) identical(x$year, as.character(year)), captured)
    expect_true(length(hit) >= 1L)
    expect_match(hit[[1]]$url, sprintf("mindate=%s", year))
    expect_match(hit[[1]]$url, sprintf("maxdate=%s", year))
  }
})

test_that("annual cache read and write keys are the v3 namespace", {
  key <- cache_key_pubmed_trend("1956", "tiab:nsclc", 2017L, 2026L)
  expect_equal(key, "pubmed:trend:v3:1956:tiab:nsclc:2017:2026")
  expect_equal(key, cache_key_pubmed_trend("1956", "tiab:nsclc", 2017L, 2026L))
  expect_false(identical(key, cache_key_pubmed_trend_legacy("1956", "tiab:nsclc", 2017L, 2026L)))
  expect_false(identical(key, cache_key_pubmed_trend_v2("1956", "tiab:nsclc", 2017L, 2026L)))
  src <- paste(readLines(file.path(app_root(), "R/process/process_literature.R")), collapse = "\n")
  expect_match(src, "trend_key <- cache_key_pubmed_trend")
  expect_match(src, "read_api_cache_payload\\(db_pool, PUBMED_SOURCE, trend_key\\)")
  expect_match(src, "put_api_cache_payload\\([\\s\\S]*trend_key", perl = TRUE)
  expect_false(grepl("cache_key_pubmed_trend_legacy\\(", src))
  expect_false(grepl("cache_key_pubmed_trend_v2\\(", src))
  expect_match(src, "PUBMED_TREND_CACHE_NAMESPACE")
})

esearch_query_from_url <- function(url) {
  parsed <- httr2::url_parse(url)
  as.list(parsed$query %||% list())
}

literature_esearch_compare_fields <- function(query) {
  query[c(
    "db", "term", "WebEnv", "query_key", "usehistory", "datetype",
    "mindate", "maxdate", "retmode", "rettype", "retmax", "sort"
  )]
}

browser_annual_year_counts <- function() {
  list(
    `2017` = 301L,
    `2018` = 344L,
    `2019` = 377L,
    `2020` = 401L,
    `2021` = 455L,
    `2022` = 489L,
    `2023` = 510L,
    `2024` = 536L,
    `2025` = 562L,
    `2026` = 415L
  )
}

test_that("ESearch Count parser never treats missing or invalid Count as zero", {
  expect_equal(parse_esearch_total_count(list(esearchresult = list(count = "417")))$count, 417L)
  expect_equal(parse_esearch_total_count(list(esearchresult = list(count = "0")))$count, 0L)

  missing <- parse_esearch_total_count(list(esearchresult = list(retmax = "50", idlist = c("1"))))
  expect_false(missing$ok)
  expect_true(is.na(missing$count))
  expect_false(identical(missing$count, 0L))

  null_count <- parse_esearch_total_count(list(esearchresult = list(count = NULL)))
  expect_false(null_count$ok)
  expect_true(is.na(null_count$count))

  invalid <- parse_esearch_total_count(list(esearchresult = list(count = "not-a-count")))
  expect_false(invalid$ok)
  expect_true(is.na(invalid$count))
  expect_false(identical(invalid$count, 0L))
})

test_that("annual Count and selected-year Count send the same NCBI history request", {
  skip_if_not_installed("httr2")
  project <- nsclc_literature_project()
  disease <- literature_disease_context(project)
  history <- parse_elink_history(read_fixture("ncbi_elink_gene_pubmed.json"))
  term <- history_intersection_term(history$query_key, disease$disease_query)
  count_query <- literature_year_esearch_query(
    term,
    history$webenv,
    2021L,
    rettype = "count",
    retmax = 0L
  )
  expect_equal(count_query$db, "pubmed")
  expect_equal(count_query$term, term)
  expect_equal(count_query$WebEnv, history$webenv)
  expect_null(count_query$query_key)
  expect_equal(count_query$usehistory, "y")
  expect_equal(count_query$datetype, "pdat")
  expect_equal(count_query$mindate, "2021")
  expect_equal(count_query$maxdate, "2021")
  expect_equal(count_query$rettype, "count")
  expect_equal(count_query$retmax, "0")

  year_counts <- browser_annual_year_counts()
  captured <- list()
  inner <- ncbi_fixture_perform(year_counts = year_counts)
  perform <- function(req) {
    url <- ncbi_url_text(req)
    if (grepl("esearch\\.fcgi", url) && grepl("mindate=2021", url) && grepl("rettype=count", url)) {
      captured[[length(captured) + 1L]] <<- esearch_query_from_url(url)
    }
    inner(req)
  }

  annual <- retrieve_annual_literature_counts(
    term = term,
    webenv = history$webenv,
    requested_years = 2021L,
    current_year = 2026L,
    perform = perform
  )
  year_pack <- retrieve_literature_year_records("1956", disease, 2021L, perform = perform)
  expect_true(annual$ok)
  expect_true(year_pack$ok)
  expect_equal(length(captured), 2L)
  annual_params <- literature_esearch_compare_fields(captured[[1]])
  year_params <- literature_esearch_compare_fields(captured[[2]])
  expect_equal(annual_params, year_params)
  expect_equal(annual_params$usehistory, "y")
  expect_equal(annual_params$WebEnv, history$webenv)
  expect_equal(annual_params$term, term)
  expect_true(grepl("^#", annual_params$term))
  expect_true(is.null(annual_params$query_key) || !nzchar(annual_params$query_key %||% ""))
  expect_equal(annual$trend$record_count[[1]], 455L)
  expect_equal(year_pack$literature$total_count, 455L)
  expect_equal(year_pack$literature$total_count, annual$trend$record_count[[1]])
  expect_true(nrow(year_pack$literature$records) <= LITERATURE_YEAR_PAGE_SIZE)
  expect_false(identical(nrow(year_pack$literature$records), year_pack$literature$total_count))
})

test_that("browser-shaped annual series keeps non-zero counts and missing Count is not zero", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("shiny")
  source_app("R/modules/mod_literature.R")
  requested <- 2017:2026
  year_counts <- browser_annual_year_counts()
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533", "ENSG00000146648")
  result <- retrieve_project_literature(
    project,
    target,
    db_pool = NULL,
    perform = ncbi_fixture_perform(year_counts = year_counts),
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  item <- result$literature$targets[[1]]
  expect_equal(item$corpus$total_count, 6029L)
  series <- literature_annual_ui_model(item, 2026L)
  expect_true(series$ok)
  expect_equal(series$trend$year, requested)
  expect_equal(series$trend$record_count, unname(unlist(year_counts[as.character(requested)])))
  expect_equal(series$trend$record_count[series$trend$year == 2026L], 415L)
  expect_false(any(series$trend$record_count == 0L))
  choices <- literature_year_choices(series$trend, current_year = 2026L)
  expect_equal(unname(choices), c(literature_all_years_value(), as.character(requested)))
  expect_equal(length(unique(unname(choices))), length(choices))
  html <- paste(as.character(literature_result_ui(
    shiny::NS("literature"),
    result,
    item,
    item$target$project_target_id
  )), collapse = "\n")
  expect_match(html, "415")
  expect_match(html, "301")
  expect_match(html, "tw-trend-svg")
  expect_match(html, ">301<")
  expect_equal(length(unique(series$trend$year)), 10L)

  missing_perform <- function(req) {
    url <- ncbi_url_text(req)
    if (grepl("esearch\\.fcgi", url) && grepl("mindate=", url) && grepl("rettype=count", url)) {
      payload <- list(
        header = list(type = "esearch", version = "0.3"),
        esearchresult = list(retmax = "0", idlist = list())
      )
      return(httr2::response(
        status_code = 200L,
        url = url,
        method = "GET",
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")))
      ))
    }
    ncbi_fixture_perform()(req)
  }
  missing_series <- retrieve_annual_literature_counts(
    term = "#1 AND (test[Title/Abstract])",
    webenv = "MCID_TESTENV",
    requested_years = 2021L,
    current_year = 2026L,
    perform = missing_perform
  )
  expect_true(missing_series$ok)
  expect_true(is.na(missing_series$trend$record_count[[1]]))
  expect_false(identical(missing_series$trend$record_count[[1]], 0L))
  expect_equal(missing_series$trend$status[[1]], "error")
})

test_that("literature switcher CSS keeps compact gene segments on one row when width allows", {
  css <- paste(readLines(file.path(app_root(), "www/styles.css")), collapse = "\n")
  expect_match(css, "\\.evidence-target-switcher\\.shiny-input-container \\{[^}]*width: auto")
  expect_match(css, "\\.target-switch \\{[^}]*width: max-content")
  expect_match(css, "\\.target-switch-label \\{[^}]*white-space: nowrap")
  expect_false(grepl("EGFR.*width", css))
})

test_that("v3 annual cache is read and Refresh drops in-memory v2 results", {
  skip_if_not_installed("httr2")
  requested <- 2017:2026
  year_counts <- browser_annual_year_counts()
  v3_trend <- data.frame(
    year = requested,
    record_count = unname(unlist(year_counts[as.character(requested)])),
    is_partial_year = requested == 2026L,
    status = "ok",
    stringsAsFactors = FALSE
  )
  zeros <- data.frame(
    year = requested,
    record_count = 0L,
    is_partial_year = requested == 2026L,
    status = "ok",
    stringsAsFactors = FALSE
  )
  logged <- capture.output(
    log_annual_trend_source("cache-v3", list(ok = TRUE, trend = v3_trend)),
    type = "message"
  )
  expect_true(any(grepl("ANNUAL TREND SOURCE: cache-v3", logged)))
  expect_true(any(grepl("2017=301", logged)))
  expect_true(any(grepl("2026=415", logged)))

  old <- list(literature = list(trend = zeros, annual_series = list(ok = TRUE, trend = zeros)))
  dropped <- forced_literature_refresh_session(TRUE, old, "v2-session")
  expect_true(dropped$drop_session_result)
  expect_null(dropped$result)
  expect_true(is.na(dropped$signature))
  kept <- forced_literature_refresh_session(FALSE, old, "v2-session")
  expect_false(kept$drop_session_result)
  expect_identical(kept$result, old)
  mod_src <- paste(readLines(file.path(app_root(), "R/modules/mod_literature.R")), collapse = "\n")
  expect_match(mod_src, "forced_literature_refresh_session")
  expect_match(mod_src, "start_literature\\(force = TRUE\\)")

  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  project <- nsclc_literature_project()
  target <- confirmed_reactome_target("egfr", "EGFR", "P00533")
  disease <- literature_disease_context(project)
  query_sig <- literature_query_signature(disease$search_terms)
  put_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    cache_key_pubmed_corpus("1956", query_sig),
    list(
      total_count = 6029L,
      recent_records = empty_recent_records(),
      disease_query = disease$disease_query
    ),
    3600L
  )
  put_api_cache_payload(
    db_pool,
    PUBMED_SOURCE,
    cache_key_pubmed_trend("1956", query_sig, 2017L, 2026L),
    list(
      trend = v3_trend,
      start_year = 2017L,
      end_year = 2026L,
      trend_ok = TRUE,
      cache_namespace = "pubmed:trend:v3"
    ),
    3600L
  )
  year_hits <- 0L
  perform <- function(req) {
    url <- ncbi_url_text(req)
    if (grepl("esearch\\.fcgi", url) && grepl("mindate=", url)) {
      year_hits <<- year_hits + 1L
    }
    ncbi_fixture_perform()(req)
  }
  cached <- retrieve_target_literature(
    target,
    disease,
    db_pool = db_pool,
    perform = perform,
    uniprot_fetch = uniprot_payload_fetch(read_fixture("uniprot_p00533_geneid.json"))
  )
  expect_equal(cached$literature$trend$record_count, v3_trend$record_count)
  expect_equal(cached$literature$trend$record_count[cached$literature$trend$year == 2026L], 415L)
  expect_equal(year_hits, 0L)
})
