# Canonical Ensembl stable identifiers.
# Comparisons and joins use the unversioned ID; the identifier type is retained.

parse_ensembl_stable_id <- function(id) {
  if (is.null(id) || length(id) == 0) {
    return(NULL)
  }

  first <- id[[1]]
  if (is.null(first) || length(first) == 0 || (length(first) == 1 && is.atomic(first) && is.na(first))) {
    return(NULL)
  }

  original <- toupper(trimws(as.character(id[[1]])))
  if (!nzchar(original)) {
    return(NULL)
  }

  match <- regexec(
    "^(ENSG|ENST|ENSP)([0-9]{11})(?:\\.([0-9]+))?$",
    original,
    perl = TRUE
  )
  parts <- regmatches(original, match)[[1]]

  if (length(parts) < 3) {
    return(NULL)
  }

  prefix <- parts[[2]]
  digits <- parts[[3]]
  version <- if (length(parts) >= 4 && nzchar(parts[[4]])) {
    parts[[4]]
  } else {
    NA_character_
  }

  type <- switch(
    prefix,
    ENSG = "gene",
    ENST = "transcript",
    ENSP = "protein",
    NA_character_
  )

  list(
    type = type,
    prefix = prefix,
    stable_id = paste0(prefix, digits),
    version = version,
    original = original
  )
}

normalize_ensembl_id <- function(id) {
  parsed <- parse_ensembl_stable_id(id)
  if (is.null(parsed)) {
    return(NA_character_)
  }

  parsed$stable_id
}

ensembl_id_type <- function(id) {
  parsed <- parse_ensembl_stable_id(id)
  if (is.null(parsed)) {
    return(NA_character_)
  }

  parsed$type
}

canonical_ensembl_gene_id <- function(id) {
  parsed <- parse_ensembl_stable_id(id)
  if (is.null(parsed) || !identical(parsed$type, "gene")) {
    return(NA_character_)
  }

  parsed$stable_id
}

canonical_ensembl_gene_ids <- function(ids) {
  if (is.null(ids) || length(ids) == 0) {
    return(character())
  }

  out <- vapply(
    as.character(ids),
    canonical_ensembl_gene_id,
    character(1),
    USE.NAMES = FALSE
  )

  unique(out[!is.na(out) & nzchar(out)])
}
