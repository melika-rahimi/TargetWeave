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

live_signature_stale <- function(previous, next_sig) {
  has_display_text(previous) &&
    has_display_text(next_sig) &&
    !identical(as.character(previous), as.character(next_sig))
}
