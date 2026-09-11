cache_key_uniprot_symbol <- function(symbol, organism_id = "9606", reviewed = TRUE) {
  sprintf(
    "uniprot:symbol:%s:%s:%s",
    toupper(trimws(symbol)),
    organism_id,
    if (isTRUE(reviewed)) "reviewed" else "any"
  )
}

cache_key_uniprot_accession <- function(accession) {
  sprintf("uniprot:accession:%s", toupper(trimws(accession)))
}

cache_key_ensembl_symbol <- function(symbol, species = "homo_sapiens") {
  sprintf(
    "ensembl:symbol:%s:%s",
    tolower(species),
    toupper(trimws(symbol))
  )
}

cache_key_ensembl_id <- function(ensembl_id) {
  sprintf("ensembl:id:%s", toupper(trimws(ensembl_id)))
}

cache_key_opentargets_disease_search <- function(query) {
  sprintf(
    "opentargets:disease-search:%s",
    gsub("\\s+", " ", tolower(trimws(query)))
  )
}

cache_key_opentargets_association <- function(ensembl_gene_id, disease_id) {
  sprintf(
    "opentargets:association:%s:%s",
    canonical_ensembl_gene_id(ensembl_gene_id) %||% toupper(trimws(ensembl_gene_id)),
    toupper(trimws(disease_id))
  )
}

cache_key_ensembl_xrefs <- function(symbol, species = "homo_sapiens") {
  sprintf(
    "ensembl:xrefs:%s:%s",
    tolower(species),
    toupper(trimws(symbol))
  )
}

cache_key_reactome_version <- function() {
  "reactome:version"
}

cache_key_rcsb_search <- function(uniprot_accession) {
  sprintf("rcsb:search:%s:experimental", toupper(trimws(uniprot_accession)))
}

cache_key_rcsb_metadata <- function(entity_ids) {
  ids <- sort(unique(toupper(trimws(as.character(entity_ids)))))
  sprintf("rcsb:metadata:%s", paste(ids, collapse = ","))
}

cache_key_rcsb_coverage <- function(uniprot_accession) {
  sprintf("rcsb:coverage:%s:experimental", toupper(trimws(uniprot_accession)))
}

cache_key_pubmed_info <- function() {
  "ncbi:pubmed-info"
}

cache_key_ncbi_gene_verify <- function(gene_id) {
  sprintf("ncbi:gene-verify:%s", trimws(as.character(gene_id)))
}

cache_key_pubmed_corpus <- function(gene_id, query_signature) {
  sprintf(
    "pubmed:target-disease:%s:%s",
    trimws(as.character(gene_id)),
    query_signature
  )
}

cache_key_pubmed_trend <- function(gene_id, query_signature, start_year, end_year) {
  sprintf(
    "pubmed:trend:%s:%s:%s:%s",
    trimws(as.character(gene_id)),
    query_signature,
    as.integer(start_year),
    as.integer(end_year)
  )
}

cache_key_reactome_membership <- function(
  uniprot_accession,
  reactome_release,
  membership_scope = "lowest_level"
) {
  release <- trimws(as.character(reactome_release %||% ""))
  if (!nzchar(release) || identical(release, "NA")) {
    release <- "unknown"
  }
  sprintf(
    "reactome:membership:%s:%s:%s",
    release,
    toupper(trimws(uniprot_accession)),
    membership_scope
  )
}

association_cache_freshness <- function(db_pool, ensembl_gene_id, disease_id) {
  if (is.null(db_pool)) {
    return("missing")
  }

  cached <- tryCatch(
    cache_get(
      db_pool,
      "opentargets",
      cache_key_opentargets_association(ensembl_gene_id, disease_id)
    ),
    error = function(e) NULL
  )

  if (is.null(cached)) {
    return("missing")
  }

  cached$freshness[[1]] %||% "missing"
}

cache_freshness <- function(expires_at, now = Sys.time()) {
  expires <- as.POSIXct(expires_at, tz = "UTC")
  current <- as.POSIXct(now, tz = "UTC")

  if (is.na(expires)) {
    return("unknown")
  }

  if (current <= expires) {
    "fresh"
  } else {
    "stale"
  }
}

cache_key_namespace <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    "test:"
  } else {
    ""
  }
}

namespaced_cache_key <- function(cache_key) {
  ns <- cache_key_namespace()
  if (!nzchar(ns) || !nzchar(cache_key) || startsWith(as.character(cache_key), ns)) {
    return(as.character(cache_key))
  }
  paste0(ns, cache_key)
}

cache_get <- function(db_pool, source, cache_key) {
  if (is.null(db_pool) || !nzchar(source) || !nzchar(cache_key)) {
    return(NULL)
  }

  cache_key <- namespaced_cache_key(cache_key)

  result <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      id::text AS id,
      source,
      cache_key,
      response::text AS response,
      http_status,
      retrieved_at,
      expires_at,
      status
    FROM api_cache
    WHERE source = $1
      AND cache_key = $2
    ",
    params = list(source, cache_key)
  )

  if (nrow(result) != 1) {
    return(NULL)
  }

  result$freshness <- cache_freshness(result$expires_at[[1]])
  result[1, , drop = FALSE]
}

cache_put <- function(
  db_pool,
  source,
  cache_key,
  response,
  http_status,
  retrieved_at,
  expires_at,
  status = "ok"
) {
  if (is.null(db_pool)) {
    return(invisible(FALSE))
  }

  cache_key <- namespaced_cache_key(cache_key)

  payload <- if (is.character(response)) {
    response
  } else {
    as.character(jsonlite::toJSON(response, auto_unbox = TRUE, null = "null"))
  }

  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO api_cache (
      id,
      source,
      cache_key,
      response,
      http_status,
      retrieved_at,
      expires_at,
      status
    )
    VALUES ($1::uuid, $2, $3, $4::jsonb, $5, $6, $7, $8)
    ON CONFLICT (source, cache_key)
    DO UPDATE SET
      response = EXCLUDED.response,
      http_status = EXCLUDED.http_status,
      retrieved_at = EXCLUDED.retrieved_at,
      expires_at = EXCLUDED.expires_at,
      status = EXCLUDED.status
    ",
    params = list(
      uuid::UUIDgenerate(),
      source,
      cache_key,
      payload,
      http_status,
      retrieved_at,
      expires_at,
      status
    )
  )

  invisible(TRUE)
}
