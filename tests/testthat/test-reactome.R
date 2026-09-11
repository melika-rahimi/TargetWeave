reactome_json_perform <- function(payload, status = 200L, url = "https://reactome.org/ContentService/mock") {
  body <- if (is.character(payload) && length(payload) == 1 && grepl("[{[]", payload)) {
    payload
  } else {
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  }
  function(req) {
    httr2::response(
      status_code = status,
      url = url,
      method = "GET",
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(as.character(body))
    )
  }
}

reactome_text_perform <- function(text, status = 200L) {
  function(req) {
    httr2::response(
      status_code = status,
      url = reactome_version_url(),
      method = "GET",
      headers = list("Content-Type" = "text/plain;charset=UTF-8"),
      body = charToRaw(as.character(text))
    )
  }
}

confirmed_reactome_target <- function(
  id,
  symbol,
  uniprot,
  ensembl = NA_character_,
  status = "confirmed"
) {
  data.frame(
    id = id,
    resolution_status = status,
    display_symbol = symbol,
    ensembl_gene_id = ensembl,
    uniprot_accession = uniprot,
    input_text = symbol,
    stringsAsFactors = FALSE
  )
}

membership_from_ids <- function(target_id, symbol, uniprot, pathway_ids, names = pathway_ids) {
  pathways <- empty_reactome_pathways()
  if (length(pathway_ids) > 0) {
    pathways <- data.frame(
      pathway_id = pathway_ids,
      pathway_name = names,
      species = REACTOME_HUMAN_SPECIES,
      membership_scope = REACTOME_MEMBERSHIP_SCOPE,
      is_in_disease = FALSE,
      pathway_url = vapply(pathway_ids, reactome_pathway_page_url, character(1)),
      stringsAsFactors = FALSE
    )
  }
  list(
    project_target_id = target_id,
    symbol = symbol,
    ensembl_gene_id = NA_character_,
    uniprot_accession = uniprot,
    pathways = pathways
  )
}

test_that("Reactome cache keys include release, UniProt, and membership scope", {
  expect_equal(
    cache_key_reactome_membership("p00533", "97", "lowest_level"),
    "reactome:membership:97:P00533:lowest_level"
  )
  expect_equal(cache_key_reactome_version(), "reactome:version")
})

test_that("official UniProt mapping parses human lowest-level pathways from a live-shaped fixture", {
  payload <- read_fixture("reactome_pathways_p00533_excerpt.json")
  parsed <- parse_reactome_pathway_membership(payload)
  expect_true(parsed$ok)
  expect_true(all(grepl("^R-HSA-", parsed$pathways$pathway_id)))
  expect_true(all(parsed$pathways$species == "Homo sapiens"))
  expect_true(all(parsed$pathways$membership_scope == "lowest_level"))
  expect_true("R-HSA-177929" %in% parsed$pathways$pathway_id)
  expect_equal(
    parsed$pathways$pathway_name[parsed$pathways$pathway_id == "R-HSA-177929"],
    "Signaling by EGFR"
  )
  expect_true(parsed$pathways$is_in_disease[parsed$pathways$pathway_id == "R-HSA-1236382"])
  expect_false(anyDuplicated(parsed$pathways$pathway_id) > 0)
})

test_that("duplicate pathway IDs collapse and non-human inferred rows are excluded", {
  payload <- list(
    list(
      stId = "R-HSA-177929",
      displayName = "Signaling by EGFR",
      speciesName = "Homo sapiens",
      schemaClass = "Pathway",
      isInDisease = FALSE,
      isInferred = FALSE
    ),
    list(
      stId = "R-HSA-177929",
      displayName = "Signaling by EGFR duplicate",
      speciesName = "Homo sapiens",
      schemaClass = "Pathway",
      isInDisease = FALSE,
      isInferred = FALSE
    ),
    list(
      stId = "R-MMU-177929",
      displayName = "Mouse EGFR",
      speciesName = "Mus musculus",
      schemaClass = "Pathway",
      isInDisease = FALSE,
      isInferred = FALSE
    ),
    list(
      stId = "R-HSA-9999999",
      displayName = "Inferred",
      speciesName = "Homo sapiens",
      schemaClass = "Pathway",
      isInDisease = FALSE,
      isInferred = TRUE
    ),
    list(
      stId = "R-HSA-9927432",
      displayName = "Lineage",
      speciesName = "Homo sapiens",
      schemaClass = "CellLineagePath",
      isInDisease = FALSE,
      isInferred = FALSE
    )
  )
  parsed <- parse_reactome_pathway_membership(payload)
  expect_equal(parsed$pathways$pathway_id, "R-HSA-177929")
  expect_equal(parsed$pathways$pathway_name, "Signaling by EGFR")
})

test_that("empty valid mapping, 404 no-pathways, malformed payload, and timeout are distinct", {
  skip_if_not_installed("httr2")

  empty_ok <- parse_reactome_pathway_membership(list())
  expect_true(empty_ok$ok)
  expect_equal(nrow(empty_ok$pathways), 0)

  not_found <- read_fixture("reactome_pathways_not_found.json")
  api_404 <- new_api_result(
    ok = FALSE,
    status = 404L,
    data = not_found,
    error = "HTTP 404.",
    cache_status = "live",
    url = reactome_mapping_url("NOTAREALACC99")
  )
  expect_true(reactome_mapping_is_empty(api_404))

  malformed <- parse_reactome_pathway_membership("nope")
  expect_false(malformed$ok)

  failed <- fetch_reactome_uniprot_pathways(
    "P00533",
    db_pool = NULL,
    reactome_release = "97",
    perform = function(req) stop("Timeout was reached")
  )
  expect_false(failed$ok)
  expect_match(failed$error, "Timeout")
})

test_that("Reactome version is parsed from the official text endpoint", {
  skip_if_not_installed("httr2")
  result <- fetch_reactome_version(db_pool = NULL, perform = reactome_text_perform("97"))
  expect_true(result$ok)
  expect_equal(result$version, "97")
})
