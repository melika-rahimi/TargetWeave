REACTOME_CONTENT_SERVICE <- "https://reactome.org/ContentService"
REACTOME_MEMBERSHIP_SCOPE <- "lowest_level"
REACTOME_HUMAN_TAXON <- "9606"
REACTOME_HUMAN_SPECIES <- "Homo sapiens"
REACTOME_HUMAN_STID_PREFIX <- "R-HSA-"
REACTOME_SOURCE <- "reactome"

reactome_mapping_url <- function(uniprot_accession) {
  sprintf(
    "%s/data/mapping/UniProt/%s/pathways",
    REACTOME_CONTENT_SERVICE,
    toupper(trimws(uniprot_accession))
  )
}

reactome_version_url <- function() {
  sprintf("%s/data/database/version", REACTOME_CONTENT_SERVICE)
}

reactome_pathway_page_url <- function(pathway_id) {
  sprintf("https://reactome.org/content/detail/%s", pathway_id)
}

reactome_is_human_stid <- function(pathway_id) {
  grepl(sprintf("^%s", REACTOME_HUMAN_STID_PREFIX), as.character(pathway_id %||% ""), ignore.case = TRUE)
}

fetch_reactome_version <- function(
  db_pool = NULL,
  perform = httr2::req_perform
) {
  config <- get_app_config()
  url <- reactome_version_url()
  cache_key <- cache_key_reactome_version()
  ttl_seconds <- config$disease_search_ttl_hours * 3600L

  cached <- NULL
  if (!is.null(db_pool)) {
    cached <- tryCatch(
      cache_get(db_pool, REACTOME_SOURCE, cache_key),
      error = function(e) NULL
    )
  }

  read_version <- function(payload) {
    if (is.null(payload)) {
      return(NA_character_)
    }
    if (is.list(payload) && !is.null(payload$version)) {
      return(as.character(payload$version[[1]]))
    }
    as.character(payload[[1]] %||% payload)
  }

  if (!is.null(cached) && identical(cached$freshness, "fresh")) {
    decoded <- tryCatch(
      decode_cached_json(cached$response[[1]]),
      error = function(e) NULL
    )
    version <- read_version(decoded)
    if (nzchar(version) && !identical(version, "NA")) {
      return(list(
        ok = TRUE,
        version = version,
        cache_status = "fresh",
        from_cache = TRUE,
        retrieved_at = cached$retrieved_at[[1]],
        url = url
      ))
    }
  }

  live <- tryCatch(
    {
      req <- httr2::request(url)
      req <- httr2::req_headers(
        req,
        `User-Agent` = http_user_agent(),
        Accept = "text/plain"
      )
      req <- httr2::req_timeout(req, config$http_timeout_seconds)
      req <- httr2::req_retry(
        req,
        max_tries = 3,
        backoff = ~ pmin(2^(.x - 1), 8),
        is_transient = function(resp) {
          httr2::resp_status(resp) %in% c(429, 503)
        }
      )
      req <- httr2::req_error(req, is_error = function(resp) FALSE)
      tw_then(perform(req), function(resp) {
        status <- httr2::resp_status(resp)
        body <- trimws(httr2::resp_body_string(resp))
        if (status >= 400 || !nzchar(body) || grepl("[^0-9.]", body)) {
          list(ok = FALSE, status = status, version = NA_character_, error = sprintf("HTTP %s.", status))
        } else {
          list(ok = TRUE, status = status, version = body, error = NULL)
        }
      }, function(e) {
        list(ok = FALSE, status = NA_integer_, version = NA_character_, error = conditionMessage(e))
      })
    },
    error = function(e) {
      list(ok = FALSE, status = NA_integer_, version = NA_character_, error = conditionMessage(e))
    }
  )

  tw_then(live, function(live) {
    if (isTRUE(live$ok)) {
      if (!is.null(db_pool)) {
        tryCatch(
          cache_put(
            db_pool = db_pool,
            source = REACTOME_SOURCE,
            cache_key = cache_key,
            response = list(version = live$version),
            http_status = live$status,
            retrieved_at = Sys.time(),
            expires_at = Sys.time() + ttl_seconds,
            status = "ok"
          ),
          error = function(e) NULL
        )
      }
      return(list(
        ok = TRUE,
        version = live$version,
        cache_status = "live",
        from_cache = FALSE,
        retrieved_at = Sys.time(),
        url = url
      ))
    }

    if (!is.null(cached)) {
      decoded <- tryCatch(
        decode_cached_json(cached$response[[1]]),
        error = function(e) NULL
      )
      version <- read_version(decoded)
      if (nzchar(version) && !identical(version, "NA")) {
        return(list(
          ok = TRUE,
          version = version,
          cache_status = "stale",
          from_cache = TRUE,
          retrieved_at = cached$retrieved_at[[1]],
          url = url,
          error = live$error
        ))
      }
    }

    list(
      ok = FALSE,
      version = NA_character_,
      cache_status = "error",
      from_cache = FALSE,
      retrieved_at = Sys.time(),
      url = url,
      error = live$error %||% "Reactome version could not be retrieved."
    )
  })
}

fetch_reactome_uniprot_pathways <- function(
  uniprot_accession,
  db_pool = NULL,
  reactome_release = NA_character_,
  perform = httr2::req_perform
) {
  accession <- toupper(trimws(uniprot_accession %||% ""))
  url <- reactome_mapping_url(accession)
  ttl_seconds <- get_app_config()$identity_cache_ttl_days * 86400L
  cache_key <- cache_key_reactome_membership(
    accession,
    reactome_release,
    REACTOME_MEMBERSHIP_SCOPE
  )

  http_get_json(
    url,
    query = list(species = REACTOME_HUMAN_TAXON),
    db_pool = db_pool,
    cache_source = REACTOME_SOURCE,
    cache_key = cache_key,
    ttl_seconds = ttl_seconds,
    perform = perform
  )
}

empty_reactome_pathways <- function() {
  data.frame(
    pathway_id = character(),
    pathway_name = character(),
    species = character(),
    membership_scope = character(),
    is_in_disease = logical(),
    pathway_url = character(),
    stringsAsFactors = FALSE
  )
}

reactome_mapping_is_empty <- function(api_result) {
  if (isTRUE(api_result$ok) && is.list(api_result$data) && length(api_result$data) == 0) {
    return(TRUE)
  }
  status <- as.integer(api_result$status %||% NA_integer_)
  if (!identical(status, 404L)) {
    return(FALSE)
  }
  messages <- api_result$data$messages
  if (is.null(messages)) {
    return(TRUE)
  }
  grepl("No pathways found", paste(unlist(messages), collapse = " "), ignore.case = TRUE)
}

parse_reactome_pathway_membership <- function(payload, membership_scope = REACTOME_MEMBERSHIP_SCOPE) {
  if (is.null(payload)) {
    return(list(
      ok = FALSE,
      pathways = empty_reactome_pathways(),
      error = "Missing Reactome payload."
    ))
  }

  if (is.list(payload) && !is.null(payload$code)) {
    return(list(
      ok = FALSE,
      pathways = empty_reactome_pathways(),
      error = paste(unlist(payload$messages %||% payload$reason %||% "Reactome error."), collapse = " ")
    ))
  }

  rows <- payload
  if (is.data.frame(rows)) {
    rows <- lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, , drop = FALSE]))
  }
  if (!is.list(rows)) {
    return(list(
      ok = FALSE,
      pathways = empty_reactome_pathways(),
      error = "Malformed Reactome pathway payload."
    ))
  }

  parsed <- list()
  seen <- character()

  for (item in rows) {
    if (!is.list(item)) {
      next
    }
    pathway_id <- as.character(item$stId %||% "")
    if (!nzchar(pathway_id) || pathway_id %in% seen) {
      next
    }
    schema_class <- as.character(item$schemaClass %||% item$className %||% "")
    species <- as.character(item$speciesName %||% "")
    inferred <- isTRUE(item$isInferred)

    if (!identical(schema_class, "Pathway")) {
      next
    }
    if (isTRUE(inferred)) {
      next
    }
    if (!identical(species, REACTOME_HUMAN_SPECIES) || !reactome_is_human_stid(pathway_id)) {
      next
    }

    name <- item$displayName %||% item$name
    if (is.list(name) || length(name) > 1) {
      name <- name[[1]]
    }
    name <- as.character(name %||% "")
    if (!nzchar(name)) {
      next
    }

    seen <- c(seen, pathway_id)
    parsed[[length(parsed) + 1L]] <- data.frame(
      pathway_id = pathway_id,
      pathway_name = name,
      species = species,
      membership_scope = membership_scope,
      is_in_disease = isTRUE(item$isInDisease),
      pathway_url = reactome_pathway_page_url(pathway_id),
      stringsAsFactors = FALSE
    )
  }

  pathways <- if (length(parsed) == 0) {
    empty_reactome_pathways()
  } else {
    do.call(rbind, parsed)
  }

  list(ok = TRUE, pathways = pathways, error = NULL)
}
