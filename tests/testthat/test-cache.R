test_that("identity cache keys are canonical and contain no secrets", {
  expect_equal(
    cache_key_uniprot_symbol("egfr"),
    "uniprot:symbol:EGFR:9606:reviewed"
  )
  expect_equal(
    cache_key_uniprot_accession("p00533"),
    "uniprot:accession:P00533"
  )
  expect_equal(
    cache_key_ensembl_symbol("Egfr"),
    "ensembl:symbol:homo_sapiens:EGFR"
  )
  expect_equal(
    cache_key_ensembl_id("ensg00000146648"),
    "ensembl:id:ENSG00000146648"
  )
})

test_that("cache freshness distinguishes reusable and stale rows", {
  now <- as.POSIXct("2026-01-10 12:00:00", tz = "UTC")

  expect_equal(
    cache_freshness("2026-01-11 12:00:00", now),
    "fresh"
  )
  expect_equal(
    cache_freshness("2026-01-09 12:00:00", now),
    "stale"
  )
})

test_that("http_get_json returns a stable result object and survives failures", {
  skip_if_not_installed("httr2")

  ok_perform <- function(req) {
    httr2::response(
      status_code = 200,
      url = "https://example.invalid/ok",
      method = "GET",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"hello":"world"}')
    )
  }

  ok <- http_get_json(
    "https://example.invalid/ok",
    perform = ok_perform,
    db_pool = NULL
  )

  expect_true(ok$ok)
  expect_equal(ok$data$hello, "world")
  expect_false(ok$from_cache)
  expect_equal(ok$cache_status, "live")

  bad_json <- function(req) {
    httr2::response(
      status_code = 200,
      url = "https://example.invalid/bad",
      method = "GET",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw("{")
    )
  }

  malformed <- http_get_json(
    "https://example.invalid/bad",
    perform = bad_json,
    db_pool = NULL
  )

  expect_false(malformed$ok)
  expect_match(malformed$error, "Malformed")

  boom <- function(req) {
    stop("network down")
  }

  failed <- http_get_json(
    "https://example.invalid/down",
    perform = boom,
    db_pool = NULL
  )

  expect_false(failed$ok)
  expect_match(failed$error, "network down")
})

test_that("fresh cache rows prevent a second HTTP call", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)
  skip_if_not_installed("httr2")

  key <- paste0("test:cache:hit:", uuid::UUIDgenerate())
  calls <- 0L

  perform <- function(req) {
    calls <<- calls + 1L
    httr2::response(
      status_code = 200,
      url = "https://example.invalid/cache",
      method = "GET",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"n":1}')
    )
  }

  first <- http_get_json(
    "https://example.invalid/cache",
    db_pool = db_pool,
    cache_source = "test",
    cache_key = key,
    ttl_seconds = 86400,
    perform = perform
  )
  second <- http_get_json(
    "https://example.invalid/cache",
    db_pool = db_pool,
    cache_source = "test",
    cache_key = key,
    ttl_seconds = 86400,
    perform = perform
  )

  expect_true(first$ok)
  expect_false(first$from_cache)
  expect_true(second$ok)
  expect_true(second$from_cache)
  expect_equal(second$cache_status, "fresh")
  expect_equal(calls, 1L)
})

test_that("testthat cache writes are namespaced away from development keys", {
  expect_equal(cache_key_namespace(), "test:")
  expect_equal(
    namespaced_cache_key("opentargets:association:ENSG00000146648:MONDO_0005233"),
    "test:opentargets:association:ENSG00000146648:MONDO_0005233"
  )
  expect_equal(
    namespaced_cache_key("test:ot:post:already-isolated"),
    "test:ot:post:already-isolated"
  )
  expect_equal(
    cache_key_opentargets_association("ENSG00000146648", "MONDO_0005233"),
    "opentargets:association:ENSG00000146648:MONDO_0005233"
  )
})
