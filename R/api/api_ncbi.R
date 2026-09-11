NCBI_EUTILS <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
NCBI_SOURCE <- "ncbi"
PUBMED_SOURCE <- "pubmed"
NCBI_HUMAN_TAXON <- 9606L
NCBI_HUMAN_SPECIES <- "Homo sapiens"
NCBI_GENE_PUBMED_LINK <- "gene_pubmed"
PUBMED_UID_RETRIEVAL_LIMIT <- 10000L
LITERATURE_RECENT_N <- 15L
LITERATURE_TREND_YEARS <- 10L
PUBMED_TITLE_ABSTRACT_FIELD <- "Title/Abstract"

.ncbi_rate_env <- new.env(parent = emptyenv())

ncbi_app_identity <- function(config = get_app_config()) {
  email <- trimws(config$ncbi_email %||% "")
  if (!nzchar(email)) {
    email <- trimws(config$contact_email %||% "")
  }
  list(
    tool = trimws(config$ncbi_tool %||% "TargetWeave"),
    email = email,
    api_key = trimws(config$ncbi_api_key %||% ""),
    has_api_key = nzchar(trimws(config$ncbi_api_key %||% "")),
    rate_limit_mode = if (nzchar(trimws(config$ncbi_api_key %||% ""))) {
      "api_key_10_rps"
    } else {
      "standard_3_rps"
    }
  )
}

ncbi_min_interval_seconds <- function() {
  identity <- ncbi_app_identity()
  if (isTRUE(identity$has_api_key)) 0.12 else 0.40
}

ncbi_throttle_wait <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(0)
  }
  last <- .ncbi_rate_env$last
  gap <- ncbi_min_interval_seconds()
  wait <- 0
  if (!is.null(last)) {
    wait <- gap - as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (!is.finite(wait) || wait < 0) {
      wait <- 0
    }
  }
  .ncbi_rate_env$last <- Sys.time() + wait
  wait
}

ncbi_throttle <- function() {
  wait <- ncbi_throttle_wait()
  if (wait > 0) {
    Sys.sleep(wait)
  }
  invisible(TRUE)
}

eutils_get <- function(
  utility,
  query = list(),
  db_pool = NULL,
  cache_source = NULL,
  cache_key = NULL,
  ttl_seconds = NULL,
  perform = httr2::req_perform
) {
  wrapped <- function(req) {
    wait <- ncbi_throttle_wait()
    tw_then(tw_delay(wait), function(...) perform(req))
  }
  tw_then(
    http_get_json(
      url = eutils_url(utility),
      query = ncbi_common_query(query),
      db_pool = db_pool,
      cache_source = cache_source,
      cache_key = cache_key,
      ttl_seconds = ttl_seconds,
      perform = wrapped
    ),
    function(result) {
      if (!is.null(result$error)) {
        result$error <- sanitize_ncbi_error(result$error)
      }
      result
    }
  )
}

sanitize_ncbi_error <- function(message, api_key = ncbi_app_identity()$api_key) {
  text <- as.character(message %||% "")
  if (nzchar(api_key)) {
    text <- gsub(api_key, "[redacted]", text, fixed = TRUE)
  }
  text <- gsub("api_key=[^&\\s]+", "api_key=[redacted]", text, ignore.case = TRUE)
  text
}

ncbi_common_query <- function(extra = list()) {
  identity <- ncbi_app_identity()
  query <- c(
    list(
      tool = identity$tool,
      email = identity$email,
      retmode = "json"
    ),
    extra
  )
  if (isTRUE(identity$has_api_key)) {
    query$api_key <- identity$api_key
  }
  query[!vapply(query, function(x) is.null(x) || (length(x) == 1 && !nzchar(as.character(x))), logical(1))]
}

eutils_url <- function(utility) {
  sprintf("%s/%s", NCBI_EUTILS, utility)
}

fetch_ncbi_gene_summary <- function(gene_id, db_pool = NULL, perform = httr2::req_perform) {
  gene_id <- trimws(as.character(gene_id %||% ""))
  ttl_seconds <- get_app_config()$identity_cache_ttl_days * 86400L
  eutils_get(
    "esummary.fcgi",
    query = list(db = "gene", id = gene_id),
    db_pool = db_pool,
    cache_source = NCBI_SOURCE,
    cache_key = cache_key_ncbi_gene_verify(gene_id),
    ttl_seconds = ttl_seconds,
    perform = perform
  )
}

fetch_gene_pubmed_history <- function(gene_id, perform = httr2::req_perform) {
  eutils_get(
    "elink.fcgi",
    query = list(
      dbfrom = "gene",
      db = "pubmed",
      id = trimws(as.character(gene_id)),
      linkname = NCBI_GENE_PUBMED_LINK,
      cmd = "neighbor_history"
    ),
    db_pool = NULL,
    perform = perform
  )
}

fetch_pubmed_esearch <- function(
  term,
  webenv = NULL,
  query_key = NULL,
  rettype = NULL,
  retmax = 0L,
  sort = NULL,
  mindate = NULL,
  maxdate = NULL,
  datetype = NULL,
  usehistory = FALSE,
  perform = httr2::req_perform
) {
  query <- list(
    db = "pubmed",
    term = term,
    retmax = as.integer(retmax)
  )
  if (isTRUE(usehistory)) {
    query$usehistory <- "y"
  }
  if (has_display_text(webenv)) {
    query$WebEnv <- webenv
  }
  if (has_display_text(query_key)) {
    query$query_key <- query_key
  }
  if (has_display_text(rettype)) {
    query$rettype <- rettype
  }
  if (has_display_text(sort)) {
    query$sort <- sort
  }
  if (has_display_text(datetype)) {
    query$datetype <- datetype
  }
  if (has_display_text(mindate)) {
    query$mindate <- mindate
  }
  if (has_display_text(maxdate)) {
    query$maxdate <- maxdate
  }
  eutils_get("esearch.fcgi", query = query, db_pool = NULL, perform = perform)
}

fetch_pubmed_esummary <- function(pmids, perform = httr2::req_perform) {
  ids <- paste(pmids, collapse = ",")
  eutils_get(
    "esummary.fcgi",
    query = list(db = "pubmed", id = ids),
    db_pool = NULL,
    perform = perform
  )
}

fetch_pubmed_einfo <- function(db_pool = NULL, perform = httr2::req_perform) {
  ttl_seconds <- get_app_config()$pubmed_ttl_hours * 3600L
  eutils_get(
    "einfo.fcgi",
    query = list(db = "pubmed"),
    db_pool = db_pool,
    cache_source = NCBI_SOURCE,
    cache_key = cache_key_pubmed_info(),
    ttl_seconds = ttl_seconds,
    perform = perform
  )
}

esearch_result_node <- function(payload) {
  payload$esearchresult %||% payload$eSearchResult %||% payload
}

parse_esearch_count <- function(payload) {
  node <- esearch_result_node(payload)
  if (!is.list(node) || is.null(node$count)) {
    return(list(ok = FALSE, count = NA_integer_, error = "Malformed ESearch count payload."))
  }
  count <- suppressWarnings(as.integer(node$count[[1]]))
  if (is.na(count) || count < 0L) {
    return(list(ok = FALSE, count = NA_integer_, error = "ESearch count was not an integer."))
  }
  list(ok = TRUE, count = count, error = NULL)
}

parse_esearch_ids <- function(payload) {
  counted <- parse_esearch_count(payload)
  if (!isTRUE(counted$ok)) {
    return(counted)
  }
  node <- esearch_result_node(payload)
  ids <- unlist(node$idlist %||% node$IdList %||% list())
  ids <- as.character(ids)
  ids <- ids[nzchar(ids)]
  list(ok = TRUE, count = counted$count, ids = ids, error = NULL)
}

parse_elink_history <- function(payload) {
  if (!is.list(payload) || is.null(payload$linksets)) {
    return(list(ok = FALSE, empty = FALSE, error = "Malformed ELink payload."))
  }
  sets <- payload$linksets
  if (length(sets) == 0) {
    return(list(ok = TRUE, empty = TRUE, webenv = NA_character_, query_key = NA_character_, error = NULL))
  }
  item <- sets[[1]]
  histories <- item$linksetdbhistories %||% list()
  pubmed_hist <- NULL
  for (hist in histories) {
    if (identical(as.character(hist$linkname %||% ""), NCBI_GENE_PUBMED_LINK) ||
        identical(as.character(hist$dbto %||% ""), "pubmed")) {
      pubmed_hist <- hist
      break
    }
  }
  if (is.null(pubmed_hist)) {
    return(list(ok = TRUE, empty = TRUE, webenv = NA_character_, query_key = NA_character_, error = NULL))
  }
  webenv <- as.character(item$webenv %||% "")
  query_key <- as.character(pubmed_hist$querykey %||% pubmed_hist$query_key %||% "")
  if (!nzchar(webenv) || !nzchar(query_key)) {
    return(list(ok = FALSE, empty = FALSE, error = "ELink History tokens were missing."))
  }
  list(
    ok = TRUE,
    empty = FALSE,
    webenv = webenv,
    query_key = query_key,
    linkname = as.character(pubmed_hist$linkname %||% NCBI_GENE_PUBMED_LINK),
    error = NULL
  )
}

parse_ncbi_gene_summary <- function(payload, gene_id) {
  gene_id <- trimws(as.character(gene_id))
  result <- payload$result %||% payload
  if (!is.list(result)) {
    return(list(ok = FALSE, error = "Malformed NCBI Gene ESummary payload."))
  }
  record <- result[[gene_id]]
  if (is.null(record) && length(result$uids) > 0) {
    record <- result[[as.character(result$uids[[1]])]]
  }
  if (!is.list(record)) {
    return(list(ok = FALSE, error = "NCBI Gene ESummary did not contain the requested GeneID."))
  }
  organism <- record$organism %||% list()
  taxid <- suppressWarnings(as.integer(organism$taxid %||% organism$taxId %||% NA_integer_))
  aliases <- unlist(strsplit(as.character(record$otheraliases %||% ""), ",\\s*"))
  aliases <- trimws(aliases)
  aliases <- aliases[nzchar(aliases)]
  list(
    ok = TRUE,
    gene_id = as.character(record$uid %||% gene_id),
    symbol = as.character(record$name %||% record$nomenclaturesymbol %||% ""),
    description = as.character(record$description %||% ""),
    taxid = taxid,
    species = as.character(organism$scientificname %||% organism$scientificName %||% ""),
    aliases = aliases,
    current_id = as.character(record$currentid %||% ""),
    status = as.character(record$status %||% ""),
    error = NULL
  )
}

verify_ncbi_gene <- function(gene, expected_symbol) {
  if (!isTRUE(gene$ok)) {
    return(list(ok = FALSE, reason = gene$error %||% "NCBI Gene verification failed."))
  }
  if (has_display_text(gene$current_id)) {
    return(list(ok = FALSE, reason = "NCBI GeneID is discontinued or replaced."))
  }
  human <- isTRUE(as.integer(gene$taxid) == NCBI_HUMAN_TAXON) ||
    identical(tolower(gene$species), tolower(NCBI_HUMAN_SPECIES))
  if (!isTRUE(human)) {
    return(list(ok = FALSE, reason = "NCBI GeneID is not Homo sapiens."))
  }
  expected <- toupper(trimws(expected_symbol %||% ""))
  symbols <- toupper(c(gene$symbol, gene$aliases))
  symbols <- symbols[nzchar(symbols)]
  if (!nzchar(expected) || !(expected %in% symbols)) {
    return(list(ok = FALSE, reason = "NCBI Gene identity does not match the confirmed target symbol."))
  }
  list(ok = TRUE, reason = NULL)
}

parse_pubmed_einfo <- function(payload) {
  info <- payload$einforesult$dbinfo %||% payload$eInfoResult$dbInfo %||% payload
  if (is.list(info) && !is.null(info[[1]]) && is.list(info[[1]])) {
    info <- info[[1]]
  }
  if (!is.list(info)) {
    return(list(ok = FALSE, last_update = NA_character_, error = "Malformed EInfo payload."))
  }
  last_update <- as.character(info$lastupdate %||% info$lastUpdate %||% "")
  if (!nzchar(last_update)) {
    last_update <- NA_character_
  }
  list(
    ok = TRUE,
    last_update = last_update,
    dbname = as.character(info$dbname %||% "pubmed"),
    error = NULL
  )
}

pubmed_article_id <- function(record, idtype) {
  ids <- record$articleids %||% list()
  for (item in ids) {
    if (identical(tolower(as.character(item$idtype %||% "")), idtype)) {
      value <- as.character(item$value %||% "")
      if (nzchar(value)) {
        return(value)
      }
    }
  }
  NA_character_
}

pubmed_first_author <- function(record) {
  authors <- record$authors %||% list()
  if (length(authors) > 0 && is.list(authors[[1]])) {
    name <- as.character(authors[[1]]$name %||% "")
    if (nzchar(name)) {
      return(name)
    }
  }
  sort_author <- as.character(record$sortfirstauthor %||% "")
  if (nzchar(sort_author)) {
    return(sort_author)
  }
  "Author not provided"
}

pubmed_year_from_date <- function(text) {
  text <- as.character(text %||% "")
  year <- sub("^([0-9]{4}).*$", "\\1", text)
  if (grepl("^[0-9]{4}$", year)) {
    return(year)
  }
  NA_character_
}

parse_pubmed_summaries <- function(payload) {
  rows <- empty_recent_records()
  result <- payload$result %||% payload
  if (!is.list(result)) {
    return(list(ok = FALSE, records = rows, error = "Malformed PubMed ESummary payload."))
  }
  uids <- as.character(unlist(result$uids %||% list()))
  parsed <- list()
  for (uid in uids) {
    record <- result[[uid]]
    if (!is.list(record)) {
      next
    }
    pmid <- as.character(record$uid %||% uid)
    title <- as.character(record$title %||% "")
    if (!nzchar(title)) {
      title <- "Title not provided"
    }
    journal <- as.character(record$source %||% record$fulljournalname %||% "")
    if (!nzchar(journal)) {
      journal <- "Journal not provided"
    }
    pubdate <- as.character(record$pubdate %||% "")
    if (!nzchar(pubdate)) {
      pubdate <- as.character(record$epubdate %||% record$sortpubdate %||% "")
    }
    if (!nzchar(pubdate)) {
      pubdate <- "Date not provided"
    }
    year <- pubmed_year_from_date(record$pubdate) %||%
      pubmed_year_from_date(record$epubdate) %||%
      pubmed_year_from_date(record$sortpubdate)
    if (!has_display_text(year)) {
      year <- "Year not provided"
    }
    doi <- pubmed_article_id(record, "doi")
    parsed[[length(parsed) + 1L]] <- data.frame(
      pmid = pmid,
      title = title,
      first_author = pubmed_first_author(record),
      journal = journal,
      publication_date = pubdate,
      publication_year = year,
      doi = if (has_display_text(doi)) doi else "DOI not provided",
      pubmed_url = sprintf("https://pubmed.ncbi.nlm.nih.gov/%s/", pmid),
      stringsAsFactors = FALSE
    )
  }
  records <- if (length(parsed) == 0) rows else do.call(rbind, parsed)
  list(ok = TRUE, records = records, error = NULL)
}

empty_recent_records <- function() {
  data.frame(
    pmid = character(),
    title = character(),
    first_author = character(),
    journal = character(),
    publication_date = character(),
    publication_year = character(),
    doi = character(),
    pubmed_url = character(),
    stringsAsFactors = FALSE
  )
}

empty_literature_trend <- function() {
  data.frame(
    year = integer(),
    record_count = integer(),
    is_partial_year = logical(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

uniprot_ncbi_gene_ids <- function(payload) {
  entry <- payload
  if (is.list(payload) && !is.null(payload$results) && length(payload$results) > 0) {
    entry <- payload$results[[1]]
  }
  ids <- uniprot_xref_ids(entry, "GeneID")
  unique(ids[grepl("^[0-9]+$", ids)])
}
