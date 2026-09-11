merge_inclusive_ranges <- function(ranges) {
  empty <- data.frame(begin = integer(), end = integer(), stringsAsFactors = FALSE)
  if (is.null(ranges) || nrow(ranges) == 0) {
    return(empty)
  }
  ordered <- ranges[order(ranges$begin, ranges$end), , drop = FALSE]
  merged <- list(ordered[1, , drop = FALSE])
  if (nrow(ordered) == 1L) {
    return(ordered)
  }
  for (i in 2:nrow(ordered)) {
    current <- merged[[length(merged)]]
    nxt <- ordered[i, , drop = FALSE]
    if (nxt$begin[[1]] <= current$end[[1]] + 1L) {
      current$end[[1]] <- max(current$end[[1]], nxt$end[[1]])
      merged[[length(merged)]] <- current
    } else {
      merged[[length(merged) + 1L]] <- nxt
    }
  }
  do.call(rbind, merged)
}

covered_residue_count <- function(ranges) {
  merged <- merge_inclusive_ranges(ranges)
  if (nrow(merged) == 0) {
    return(0L)
  }
  as.integer(sum(merged$end - merged$begin + 1L))
}

coverage_fraction <- function(covered, uniprot_length) {
  length <- as.integer(uniprot_length)
  covered <- as.integer(covered)
  if (is.na(length) || length <= 0L || is.na(covered) || covered < 0L) {
    return(NA_real_)
  }
  max(0, min(1, covered / length))
}

format_coverage_label <- function(covered, uniprot_length, fraction) {
  if (is.na(as.integer(uniprot_length)) || is.na(as.integer(covered))) {
    return("Not provided")
  }
  if (is.na(fraction)) {
    return(sprintf("%s / %s residues", covered, uniprot_length))
  }
  sprintf("%s / %s residues (%.1f%% sequence coverage)", covered, uniprot_length, 100 * fraction)
}

normalize_entity_coverage <- function(alignment, uniprot_length) {
  ranges <- merge_inclusive_ranges(alignment$ranges)
  covered <- covered_residue_count(ranges)
  length <- as.integer(alignment$uniprot_length %||% uniprot_length)
  fraction <- coverage_fraction(covered, length)
  list(
    uniprot_length = length,
    covered_ranges = ranges,
    covered_residue_count = covered,
    coverage_fraction = fraction,
    coverage_label = format_coverage_label(covered, length, fraction)
  )
}
