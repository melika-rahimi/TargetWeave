workspace_tour_steps <- function() {
  list(
    list(
      target = "[data-tour='project-header']",
      fallback = ".project-hero",
      title = "Project context",
      body = "Your investigation keeps one research question, disease context, and candidate target set together."
    ),
    list(
      target = "[data-tour='target-list']",
      fallback = ".targets-panel",
      title = "Targets",
      body = "Your candidate targets live here. Select a target to confirm its molecular identity and investigate its evidence."
    ),
    list(
      target = "[data-tour='identity']",
      fallback = "[data-tour='target-list']",
      title = "Identity first",
      body = "TargetWeave confirms molecular identity before retrieving downstream evidence."
    ),
    list(
      target = "[data-tour='target-workspace']",
      fallback = ".workspace-main",
      title = "Target workspace",
      body = "Once a target is confirmed, use Overview and Disease evidence to explore molecular annotations and disease-linked evidence. Those views unlock after confirmation."
    ),
    list(
      target = "[data-tour='compare-nav']",
      fallback = ".project-tabs",
      title = "Compare evidence",
      body = "After the disease and at least two targets are confirmed, compare their Open Targets evidence profiles here. You are ready to start investigating."
    )
  )
}

project_setup_tour_steps <- function() {
  list(
    list(
      target = "[data-tour='setup-title']",
      placement = "below",
      title = "Name your investigation",
      body = "Give the project a short title that helps you recognize it later.",
      example = "Potential therapeutic targets in NSCLC"
    ),
    list(
      target = "[data-tour='setup-question']",
      placement = "below",
      title = "Define your research question",
      body = "Write the biological question this investigation is meant to explore. This field expects a question, not a gene symbol or disease name.",
      example = "Which of EGFR, KRAS, MET and TP53 deserves deeper investigation in NSCLC?"
    ),
    list(
      target = "[data-tour='setup-disease']",
      placement = "below",
      title = "Add the disease context",
      body = "Enter the disease in ordinary scientific language. TargetWeave will ask you to confirm its standardized disease identity later.",
      example = "non-small cell lung cancer"
    ),
    list(
      target = "[data-tour='setup-targets']",
      placement = "below",
      title = "Add candidate targets",
      body = "Enter 2–8 gene or protein symbols or identifiers. Use one per line or separate them with commas.",
      example = "EGFR\nKRAS\nMET\nTP53"
    )
  )
}

overview_tip_step <- function() {
  list(
    target = "[data-tour='overview-nav']",
    fallback = "[data-tour='target-workspace']",
    title = "Target overview",
    body = "Overview summarizes identity, protein annotation, genomic context, and source provenance."
  )
}

evidence_tip_step <- function() {
  list(
    target = "[data-tour='evidence-chart']",
    fallback = ".evidence-hero",
    title = "Disease evidence",
    body = "This profile shows Open Targets association scores by evidence type for the confirmed target–disease pair."
  )
}

pathways_tip_step <- function() {
  list(
    target = ".pathway-shell",
    fallback = ".workspace-main",
    title = "Pathways",
    body = "Pathways shows Reactome pathway membership and overlap across your confirmed targets. Membership is not an activity or enrichment score."
  )
}

literature_tip_step <- function() {
  list(
    target = ".literature-shell",
    fallback = ".workspace-main",
    title = "Literature",
    body = "Literature shows PubMed records linked by NCBI Gene to a confirmed target and matching the confirmed disease wording in title or abstract. Counts are volume, not evidence quality."
  )
}

structures_tip_step <- function() {
  list(
    target = ".structure-shell",
    fallback = ".workspace-main",
    title = "Structures",
    body = "Structures shows experimentally determined PDB polymer entities for a confirmed UniProt accession, with sequence coverage. Predicted models are not included."
  )
}

research_tip_step <- function() {
  list(
    target = ".research-shell",
    fallback = ".workspace-main",
    title = "Notes / Snapshots",
    body = "Save private research notes and immutable evidence snapshots of what you have already retrieved. Snapshots are not the API cache and do not refresh."
  )
}

compare_tip_step <- function() {
  list(
    target = "[data-tour='compare-heatmap']",
    fallback = ".comparison-shell",
    title = "Compare evidence",
    body = "Compare evidence patterns across your confirmed targets. A dash means no score was returned; zero is a returned score of 0."
  )
}

unbox_chr <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  jsonlite::unbox(as.character(value[[1]]))
}

encode_tour_step <- function(step) {
  encoded <- list(
    target = unbox_chr(step$target),
    title = unbox_chr(step$title),
    body = unbox_chr(step$body)
  )
  if (!is.null(step$fallback)) {
    encoded$fallback <- unbox_chr(step$fallback)
  }
  if (!is.null(step$example)) {
    encoded$example <- unbox_chr(step$example)
  }
  if (!is.null(step$placement)) {
    encoded$placement <- unbox_chr(step$placement)
  }
  encoded
}

# Shiny jsonlite uses dataframe="columns" + auto_unbox. Identical-key step
# lists otherwise serialize as one column object, and tour.js bails on
# `!msg.steps.length`. Keep an array of scalar-unboxed objects.
tour_client_payload <- function(action, mode = "tour", tip = NULL, steps = NULL) {
  payload <- list(
    action = unbox_chr(action),
    mode = unbox_chr(mode)
  )
  if (!is.null(tip) && nzchar(as.character(tip))) {
    payload$tip <- unbox_chr(tip)
  }
  if (!is.null(steps)) {
    payload$steps <- I(lapply(steps, encode_tour_step))
  }
  payload
}

# Mimic shiny:::toJSON options used by sendCustomMessage.
tour_payload_json <- function(payload) {
  jsonlite::toJSON(
    payload,
    dataframe = "columns",
    null = "null",
    na = "string",
    auto_unbox = TRUE,
    digits = 16,
    use_signif = TRUE,
    force = TRUE,
    POSIXt = "ISO8601",
    UTC = TRUE,
    rownames = FALSE,
    keep_vec_names = TRUE,
    json_verbatim = TRUE
  )
}

should_start_workspace_tour <- function(view, user, workspace_ready) {
  identical(as.character(view), "project") &&
    isTRUE(user_needs_workspace_tour(user)) &&
    !is.null(workspace_ready)
}

restart_workspace_tour_plan <- function(project_id) {
  if (is.null(project_id) || !nzchar(as.character(project_id))) {
    return(list(
      ok = FALSE,
      message = "Open a project to start the workspace tour."
    ))
  }
  list(ok = TRUE, view = "project")
}

should_start_project_setup_tour <- function(view, user) {
  (identical(as.character(view), "onboarding_project") ||
    identical(as.character(view), "new_project")) &&
    isTRUE(user_needs_project_setup_tour(user))
}

restart_project_setup_tour_plan <- function() {
  list(ok = TRUE, view = "new_project")
}

tip_seen <- function(user, tip) {
  seen <- user$workspace_tips_seen
  if (is.null(seen) || length(seen) == 0) {
    return(FALSE)
  }
  if (is.character(seen) && length(seen) == 1) {
    parsed <- tryCatch(jsonlite::fromJSON(seen, simplifyVector = FALSE), error = function(e) list())
    seen <- parsed
  }
  if (is.data.frame(seen)) {
    return(isTRUE(seen[[tip]][[1]]))
  }
  if (is.list(seen)) {
    return(isTRUE(seen[[tip]]) || has_display_text(seen[[tip]]))
  }
  FALSE
}
