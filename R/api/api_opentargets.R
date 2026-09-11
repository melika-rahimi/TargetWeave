OT_GRAPHQL_URL <- "https://api.platform.opentargets.org/api/v4/graphql"

OT_QUERY_DISEASE_SEARCH <- "
query TargetWeaveDiseaseSearch($q: String!) {
  search(queryString: $q, page: {index: 0, size: 20}) {
    total
    hits {
      id
      name
      entity
      score
      description
      object {
        ... on Disease {
          id
          name
          synonyms { relation terms }
        }
      }
    }
  }
  meta {
    name
    dataVersion { year month }
    apiVersion { x y z }
  }
}
"

# Therapeutic rows are restricted to the confirmed pair by:
#   disease(efoId: $diseaseId) — Open Targets still names this argument efoId
#   evidences(ensemblIds: [$ensemblId], enableIndirect: false,
#             datasourceIds: [\"clinical_precedence\"])
# Parsers also require each row's target.id and disease.id to match.
OT_QUERY_TARGET_DISEASE_EVIDENCE <- "
query TargetWeaveTargetDiseaseEvidence(
  $ensemblId: String!
  $diseaseId: String!
  $diseaseIds: [String!]!
) {
  meta {
    name
    dataVersion { year month }
    apiVersion { x y z }
  }
  target(ensemblId: $ensemblId) {
    id
    approvedSymbol
    direct: associatedDiseases(
      Bs: $diseaseIds
      enableIndirect: false
      page: {index: 0, size: 5}
    ) {
      count
      rows {
        score
        datatypeScores { id score }
        datasourceScores { id score }
        disease { id name }
      }
    }
    inclusive: associatedDiseases(
      Bs: $diseaseIds
      enableIndirect: true
      page: {index: 0, size: 5}
    ) {
      count
      rows {
        score
        datatypeScores { id score }
        datasourceScores { id score }
        disease { id name }
      }
    }
  }
  disease(efoId: $diseaseId) {
    id
    name
    description
    therapeuticAreas { id name }
    clinicalEvidence: evidences(
      ensemblIds: [$ensemblId]
      enableIndirect: false
      datasourceIds: [\"clinical_precedence\"]
      size: 12
    ) {
      count
      rows {
        id
        score
        datasourceId
        datatypeId
        clinicalStage
        drugFromSource
        drug { id name }
        target { id approvedSymbol }
        disease { id name }
      }
    }
  }
}
"

ot_graphql <- function(
  query,
  variables,
  cache_key,
  ttl_seconds,
  db_pool = NULL,
  perform = httr2::req_perform
) {
  tw_then(
    http_post_json(
      url = OT_GRAPHQL_URL,
      body = list(query = query, variables = variables),
      db_pool = db_pool,
      cache_source = "opentargets",
      cache_key = cache_key,
      ttl_seconds = ttl_seconds,
      perform = perform
    ),
    function(result) {
      if (!isTRUE(result$ok)) {
        return(result)
      }

      payload <- result$data
      if (!is.null(payload$errors) && length(payload$errors) > 0) {
        result$ok <- FALSE
        result$error <- graphql_error_message(payload$errors)
        if (is.null(payload$data)) {
          result$data <- payload
        }
      }

      result$query_name <- NA_character_
      result
    }
  )
}

ot_search_diseases <- function(query, db_pool = NULL, perform = httr2::req_perform) {
  config <- get_app_config()
  result <- ot_graphql(
    query = OT_QUERY_DISEASE_SEARCH,
    variables = list(q = query),
    cache_key = cache_key_opentargets_disease_search(query),
    ttl_seconds = config$disease_search_ttl_hours * 3600L,
    db_pool = db_pool,
    perform = perform
  )
  tw_then(result, function(result) {
    result$query_name <- "TargetWeaveDiseaseSearch"
    result
  })
}

ot_target_disease_evidence <- function(
  ensembl_gene_id,
  disease_id,
  db_pool = NULL,
  perform = httr2::req_perform
) {
  config <- get_app_config()
  ensembl_gene_id <- canonical_ensembl_gene_id(ensembl_gene_id)
  disease_id <- trimws(disease_id)
  result <- ot_graphql(
    query = OT_QUERY_TARGET_DISEASE_EVIDENCE,
    variables = list(
      ensemblId = ensembl_gene_id,
      diseaseId = disease_id,
      diseaseIds = list(disease_id)
    ),
    cache_key = cache_key_opentargets_association(ensembl_gene_id, disease_id),
    ttl_seconds = config$ot_association_ttl_days * 86400L,
    db_pool = db_pool,
    perform = perform
  )
  tw_then(result, function(result) {
    result$query_name <- "TargetWeaveTargetDiseaseEvidence"
    result
  })
}

parse_ot_meta <- function(payload) {
  meta <- payload$meta
  if (is.null(meta)) {
    return(list(
      name = NA_character_,
      data_version = NA_character_,
      api_version = NA_character_
    ))
  }

  dv <- meta$dataVersion
  av <- meta$apiVersion
  list(
    name = meta$name %||% NA_character_,
    data_version = if (is.null(dv)) {
      NA_character_
    } else {
      paste(c(dv$year, dv$month)[!vapply(list(dv$year, dv$month), is.null, logical(1))], collapse = "-")
    },
    api_version = if (is.null(av)) {
      NA_character_
    } else {
      paste(c(av$x, av$y, av$z), collapse = ".")
    }
  )
}

parse_disease_synonyms <- function(object) {
  if (is.null(object) || !is.list(object)) {
    return(list())
  }

  groups <- object$synonyms %||% list()
  out <- list()
  for (group in groups) {
    relation <- group$relation %||% NA_character_
    terms <- group$terms %||% list()
    for (term in terms) {
      if (has_display_text(term)) {
        out <- c(out, list(list(
          relation = as.character(relation),
          term = as.character(term)
        )))
      }
    }
  }
  out
}

parse_ot_disease_search <- function(payload) {
  if (is.null(payload) || !is.list(payload)) {
    return(list(ok = FALSE, error = "Malformed Open Targets payload.", hits = list(), total = 0L, meta = parse_ot_meta(NULL)))
  }

  if (!is.null(payload$errors) && is.null(payload$data)) {
    return(list(
      ok = FALSE,
      error = graphql_error_message(payload$errors),
      hits = list(),
      total = 0L,
      meta = parse_ot_meta(NULL)
    ))
  }

  data <- payload$data %||% payload
  search <- data$search
  if (is.null(search)) {
    return(list(ok = FALSE, error = "Malformed Open Targets search payload.", hits = list(), total = 0L, meta = parse_ot_meta(data)))
  }

  hits <- lapply(search$hits %||% list(), function(hit) {
    if (!is.list(hit) || !has_display_text(hit$id)) {
      return(NULL)
    }
    list(
      id = as.character(hit$id),
      name = hit$name %||% NA_character_,
      entity = hit$entity %||% NA_character_,
      score = suppressWarnings(as.numeric(hit$score %||% NA_real_)),
      description = hit$description %||% NA_character_,
      synonyms = parse_disease_synonyms(hit$object)
    )
  })
  hits <- Filter(Negate(is.null), hits)

  list(
    ok = TRUE,
    error = NULL,
    hits = hits,
    total = as.integer(search$total %||% length(hits)),
    meta = parse_ot_meta(data)
  )
}

parse_scored_components <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(data.frame(
      id = character(),
      score = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  ids <- vapply(items, function(x) as.character(x$id %||% NA_character_), character(1))
  scores <- vapply(items, function(x) {
    value <- x$score
    if (is.null(value)) {
      return(NA_real_)
    }
    as.numeric(value)
  }, numeric(1))

  data.frame(id = ids, score = scores, stringsAsFactors = FALSE)
}

parse_association_row <- function(row) {
  if (is.null(row) || !is.list(row)) {
    return(NULL)
  }

  disease <- row$disease %||% list()
  list(
    overall_score = if (is.null(row$score)) NA_real_ else as.numeric(row$score),
    disease_id = disease$id %||% NA_character_,
    disease_name = disease$name %||% NA_character_,
    datatype_scores = parse_scored_components(row$datatypeScores),
    datasource_scores = parse_scored_components(row$datasourceScores)
  )
}

parse_ot_target_disease_evidence <- function(payload) {
  if (is.null(payload) || !is.list(payload)) {
    return(list(ok = FALSE, error = "Malformed Open Targets payload.", association = NULL))
  }

  if (!is.null(payload$errors) && is.null(payload$data)) {
    return(list(
      ok = FALSE,
      error = graphql_error_message(payload$errors),
      association = NULL
    ))
  }

  data <- payload$data %||% payload
  target <- data$target
  disease <- data$disease
  if (is.null(target) && is.null(disease)) {
    return(list(ok = FALSE, error = "Malformed Open Targets association payload.", association = NULL))
  }

  direct_rows <- target$direct$rows %||% list()
  inclusive_rows <- target$inclusive$rows %||% list()

  list(
    ok = TRUE,
    error = NULL,
    meta = parse_ot_meta(data),
    target_id = target$id %||% NA_character_,
    target_symbol = target$approvedSymbol %||% NA_character_,
    direct = if (length(direct_rows) == 0) NULL else parse_association_row(direct_rows[[1]]),
    inclusive = if (length(inclusive_rows) == 0) NULL else parse_association_row(inclusive_rows[[1]]),
    disease_id = disease$id %||% NA_character_,
    disease_name = disease$name %||% NA_character_,
    disease_description = disease$description %||% NA_character_,
    clinical_count = as.integer(disease$clinicalEvidence$count %||% 0L),
    clinical_rows = disease$clinicalEvidence$rows %||% list()
  )
}
