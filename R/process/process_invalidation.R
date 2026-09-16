# Live evidence invalidation dependency model (M11).
#
# PROJECT TITLE / RESEARCH QUESTION
#   Presentation only. Does not invalidate identities, evidence, cache, snapshots, or notes.
#
# TARGET IDENTITY (confirm / reset / remove)
#   Invalidates LIVE: Overview, Open Targets (target side), Compare, Reactome,
#   Literature target mapping, Structures.
#
# DISEASE IDENTITY (label / confirmation reset)
#   Invalidates LIVE: Open Targets, Compare, Literature.
#   Does NOT invalidate: target molecular identities, Overview, Reactome, Structures.
#
# Reactome: target-dependent only.
# Structures: target-dependent only.
# Snapshot JSONB: never rewritten. Cache rows keyed by public identifiers are not deleted.
#
# MULTI-TARGET STALE RULE (M12)
# Live Compare / Pathways / Literature / Structures are current iff they were
# retrieved for the same confirmed scientific identity set.
#   Compare, Literature: disease ontology + sorted canonical Ensembl IDs
#   Pathways, Structures: sorted canonical Ensembl IDs (target-only)
# Unresolved targets are ignored. target_set_revision is an audit counter and
# is not part of the scientific signature. Removing then re-confirming the same
# Ensembl/UniProt identity restores currency; the old result is not kept stale.

INVALIDATION_SCOPES <- list(
  presentation = character(),
  disease = c("opentargets", "compare", "literature"),
  target = c(
    "overview",
    "opentargets",
    "compare",
    "reactome",
    "literature",
    "structures"
  )
)

live_invalidation_event <- function(kind, details = list()) {
  list(
    kind = kind,
    scopes = INVALIDATION_SCOPES[[kind]] %||% character(),
    details = details,
    at = Sys.time()
  )
}

confirmed_scientific_target_set <- function(target_rows) {
  if (is.null(target_rows) || !is.data.frame(target_rows) || nrow(target_rows) == 0) {
    return("")
  }
  keep <- vapply(seq_len(nrow(target_rows)), function(i) {
    target_is_confirmed(target_rows[i, , drop = FALSE])
  }, logical(1))
  rows <- target_rows[keep, , drop = FALSE]
  if (nrow(rows) == 0) {
    return("")
  }
  tokens <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    ens <- canonical_ensembl_gene_id(row$ensembl_gene_id[[1]])
    if (has_display_text(ens)) {
      return(as.character(ens))
    }
    acc <- toupper(trimws(as.character(row$uniprot_accession[[1]] %||% "")))
    if (has_display_text(acc)) {
      return(paste0("uniprot:", acc))
    }
    as.character(row$id[[1]])
  }, character(1))
  paste(sort(unique(tokens)), collapse = ",")
}

live_signature_stale <- function(previous, next_sig) {
  has_display_text(previous) &&
    has_display_text(next_sig) &&
    !identical(as.character(previous), as.character(next_sig))
}

TARGET_SET_STALE_MESSAGE <- "Project targets changed since this evidence was retrieved."

hold_stale_multi_target_result <- function(current, previous_sig, next_sig, payload_key) {
  if (!live_signature_stale(previous_sig, next_sig)) {
    return(list(changed = FALSE, current = current, suppress_auto_retrieve = FALSE))
  }
  if (is.list(current) && !is.null(current[[payload_key]])) {
    current$stale_target_set <- TRUE
    current$stale_message <- TARGET_SET_STALE_MESSAGE
    return(list(changed = TRUE, current = current, suppress_auto_retrieve = TRUE))
  }
  list(changed = TRUE, current = NULL, suppress_auto_retrieve = FALSE)
}

suppress_auto_retrieve_while_stale <- function(current, payload_key, force = FALSE) {
  if (isTRUE(force)) {
    return(FALSE)
  }
  is.list(current) &&
    isTRUE(current$stale_target_set) &&
    !is.null(current[[payload_key]])
}

target_set_stale_banner <- function(ns, current, refresh_id, refresh_label) {
  if (!isTRUE(current$stale_target_set)) {
    return(NULL)
  }
  div(
    class = "form-message warning target-set-stale",
    p(current$stale_message %||% TARGET_SET_STALE_MESSAGE),
    actionButton(ns(refresh_id), refresh_label, class = "btn-primary-quiet")
  )
}
