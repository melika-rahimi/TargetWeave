source_app("R/modules/mod_project_home.R")

test_that("project dashboard exposes Manage project and sidebar add/resolve actions", {
  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  expect_match(home, "Manage project")
  expect_match(home, "btn-manage-project")
  expect_match(home, "\\+ Add target")
  expect_match(home, "Maximum 8 active targets")
  expect_match(home, "Resolve identity")
  expect_false(grepl("Edit project", home, fixed = TRUE))
  expect_false(grepl("confirm_remove_target", home, fixed = TRUE))
  expect_match(home, "Project history")
  expect_match(home, "open_manage")
})

test_that("project home presents identity, target states, evidence readiness, and history", {
  ns <- shiny::NS("home")
  project <- data.frame(
    title = "NSCLC investigation",
    research_question = "Which candidates deserve deeper investigation?",
    organism = "Homo sapiens",
    disease_label = "Non-small-cell lung cancer",
    disease_name = "non-small cell lung carcinoma",
    disease_ontology_id = "MONDO_0005233",
    disease_resolution_status = "confirmed",
    stringsAsFactors = FALSE
  )
  targets <- data.frame(
    id = c("egfr", "kras", "brca"),
    input_text = c("EGFR", "KRAS", "BRCA2"),
    display_symbol = c("EGFR", "KRAS", NA_character_),
    ensembl_gene_id = c("ENSG00000146648", "ENSG00000133703", NA_character_),
    resolution_status = c("confirmed", "confirmed", "unresolved"),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    event_type = c("TARGET_IDENTITY_CONFIRMED", "PROJECT_CREATED"),
    actor_label = c("Melika", "Melika"),
    created_at = as.POSIXct(c("2026-09-17 09:44:00", "2026-09-15 14:10:00"), tz = "UTC"),
    metadata = c(
      '{"label":"EGFR","ensembl_gene_id":"ENSG00000146648","uniprot_accession":"P00533"}',
      "{}"
    ),
    stringsAsFactors = FALSE
  )
  html <- paste(
    as.character(project_home_stage_ui(
      ns,
      project,
      targets,
      events = events,
      evidence = list(
        overviews = list(egfr = list(status = "ready", overview = list(ok = TRUE))),
        disease_evidence = list(),
        comparison = list(status = "ready", stale_target_set = TRUE, comparison = list()),
        pathways = NULL,
        literature = list(status = "ready", literature = list()),
        structures = list(status = "error", message = "RCSB unavailable")
      )
    )),
    collapse = "\n"
  )
  expect_match(html, "Target set")
  expect_match(html, "2 confirmed")
  expect_match(html, "1 unresolved")
  expect_match(html, "Evidence")
  expect_match(html, "project-readiness-table")
  expect_match(html, "Overview")
  expect_match(html, "Current")
  expect_match(html, "Disease evidence")
  expect_match(html, "Not retrieved")
  expect_match(html, "Compare evidence")
  expect_match(html, "Stale")
  expect_match(html, "Pathways")
  expect_match(html, "Literature")
  expect_match(html, "Structures")
  expect_match(html, "Unavailable")
  expect_match(html, "Notes / Snapshots")
  expect_match(html, "Project history")
  expect_match(html, "EGFR identity confirmed")
  expect_match(html, "ENSG00000146648")
  expect_false(grepl("completeness|readiness percentage|project health|research quality", html, ignore.case = TRUE))
  expect_match(html, "open_compare")
  expect_match(html, "open_research")

  confirmed <- paste(as.character(project_sidebar_target_ui(ns, targets[1, ])), collapse = "\n")
  expect_match(confirmed, "EGFR")
  expect_match(confirmed, "ENSG00000146648")
  expect_match(confirmed, "status-pill-success")
  expect_match(confirmed, "Confirmed")
  expect_false(grepl("Resolve identity", confirmed))

  unresolved <- paste(as.character(project_sidebar_target_ui(ns, targets[3, ])), collapse = "\n")
  expect_match(unresolved, "BRCA2")
  expect_match(unresolved, "Identity not confirmed")
  expect_match(unresolved, "Confirm this identifier before retrieving evidence")
  expect_match(unresolved, "Resolve identity")
  expect_match(unresolved, "Needs confirmation")

  blocked <- paste(
    as.character(project_home_stage_ui(
      ns,
      transform(project, disease_resolution_status = "unresolved", disease_name = NA_character_, disease_ontology_id = NA_character_),
      targets[3, , drop = FALSE],
      events = NULL,
      evidence = list()
    )),
    collapse = "\n"
  )
  expect_match(blocked, "Unavailable")
  expect_match(blocked, "Confirm the project disease identity")
  expect_match(blocked, "0 confirmed")
  expect_match(blocked, "1 unresolved")
})

test_that("manage modals are replaced after close rather than stacked", {
  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  helper <- paste(readLines(file.path(app_root(), "R/ui/ui_components.R")), collapse = "\n")
  js <- paste(readLines(file.path(app_root(), "www/modals.js")), collapse = "\n")
  ui <- paste(readLines(file.path(app_root(), "R/ui.R")), collapse = "\n")
  expect_match(helper, "replace_shiny_modal")
  expect_match(helper, "onFlushed")
  expect_match(helper, "removeModal")
  expect_match(home, "replace_shiny_modal")
  expect_false(grepl("shiny::showModal\\(", home))
  expect_match(js, "hidden.bs.modal")
  expect_match(js, "modal-open")
  expect_match(js, "modal-backdrop")
  expect_false(grepl("overflow:\\s*auto\\s*!important", js))
  expect_match(ui, "modals.js")
  src <- paste(readLines(file.path(app_root(), "R/ui/ui_components.R")), collapse = "\n")
  expect_match(src, "body.modal-open")
})

test_that("add target is visible beside Targets and disables at the cap", {
  ns <- shiny::NS("home")
  open <- as.character(add_target_sidebar_ui(ns, 5L, 8L, form_open = FALSE))
  expect_match(open, "Targets")
  expect_match(open, "5 / 8")
  expect_match(open, "\\+ Add target")
  expect_false(grepl("disabled", open))
  capped <- as.character(add_target_sidebar_ui(ns, 8L, 8L))
  expect_match(capped, "8 / 8")
  expect_match(capped, "disabled")
  expect_match(capped, "Maximum 8 active targets")
})

test_that("unresolved targets expose Resolve identity", {
  html <- as.character(resolve_identity_button_ui(shiny::NS("home"), "target-1"))
  expect_match(html, "Resolve identity")
  expect_match(html, "select_target")
})

test_that("remove confirmation is target-specific and cancel is a no-mutation control", {
  ns <- shiny::NS("home")
  html <- paste(as.character(remove_target_confirm_ui(ns, "MET")), collapse = "\n")
  expect_match(html, "Remove MET from the live project")
  expect_match(html, "current workspace")
  expect_match(html, "snapshots, notes, and project history will not be rewritten")
  expect_match(html, ">Cancel<")
  expect_match(html, "Remove MET")
  expect_match(html, "cancel_remove")
  expect_match(html, "confirm_remove_go")

  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  cancel <- sub(".*observeEvent\\(input\\$cancel_remove.*?\\{", "", home)
  cancel <- sub("observeEvent\\(input\\$confirm_remove_go.*", "", cancel)
  expect_false(grepl("remove_project_target", cancel))
  expect_match(home, "confirm_removal = TRUE")
  expect_equal(length(gregexpr("remove_project_target\\(", home)[[1]]), 1L)
})

test_that("reset identity confirmation explains live vs snapshot effects", {
  html <- paste(as.character(reset_identity_confirm_ui(shiny::NS("home"), "EGFR")), collapse = "\n")
  expect_match(html, "Reset EGFR identity")
  expect_match(html, "stays in the project")
  expect_match(html, "confirmed scientific identity is cleared")
  expect_match(html, "may become stale")
  expect_match(html, "Snapshots remain unchanged")
  expect_match(html, "cancel_reset")
})

test_that("unresolved targets expose Remove from project without a details menu", {
  ns <- shiny::NS("home")
  project <- data.frame(
    title = "NSCLC investigation",
    research_question = "Which candidates deserve deeper investigation?",
    disease_label = "Non-small-cell lung cancer",
    disease_name = NA_character_,
    disease_ontology_id = NA_character_,
    disease_resolution_status = "unresolved",
    stringsAsFactors = FALSE
  )
  targets <- data.frame(
    id = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
    input_text = "BRCA2",
    display_symbol = NA_character_,
    resolution_status = "unresolved",
    stringsAsFactors = FALSE
  )
  html <- paste(as.character(manage_project_body_ui(ns, project, targets)), collapse = "\n")
  expect_match(html, "Actions")
  expect_match(html, "Remove from project")
  expect_match(html, "data-tw-input")
  expect_match(html, "request_remove_target")
  expect_match(html, "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
  expect_false(grepl("<details", html))
  expect_false(grepl("Reset identity", html))
})

test_that("unresolved BRCA2 can be removed without confirming identity", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("ux-unres-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "UX unresolved remove",
    "Can an unresolved identifier be removed?",
    "Non-small-cell lung cancer",
    "EGFR"
  )
  pid <- created$project_id
  uid <- owner$user$id
  egfr_id <- list_project_targets(db_pool, pid, uid)$id[[1]]
  expect_true(confirm_project_target(
    db_pool,
    egfr_id,
    uid,
    "EGFR",
    "ENSG00000146648",
    "P00533",
    "HGNC:3236"
  )$ok)
  expect_true(add_project_target(db_pool, pid, uid, "BRCA2")$ok)
  rows <- list_project_targets(db_pool, pid, uid)
  brca <- rows$id[rows$input_text == "BRCA2"][[1]]
  expect_equal(as.character(rows$resolution_status[rows$input_text == "BRCA2"][[1]]), "unresolved")
  before_sig <- list(
    scientific = confirmed_scientific_target_set(rows),
    pathways = pathway_signature(rows),
    structures = structure_signature(rows)
  )
  before <- list_project_events(db_pool, pid, uid)
  n_removed <- sum(before$event_type == "TARGET_REMOVED")
  n_confirmed <- sum(before$event_type == "TARGET_IDENTITY_CONFIRMED")

  removed <- remove_project_target(db_pool, pid, uid, brca, confirm_removal = TRUE)
  expect_true(removed$ok)
  after_rows <- list_project_targets(db_pool, pid, uid)
  expect_false("BRCA2" %in% after_rows$input_text)
  after <- list_project_events(db_pool, pid, uid)
  expect_equal(sum(after$event_type == "TARGET_REMOVED"), n_removed + 1L)
  expect_equal(sum(after$event_type == "TARGET_IDENTITY_CONFIRMED"), n_confirmed)
  expect_equal(
    list(
      scientific = confirmed_scientific_target_set(after_rows),
      pathways = pathway_signature(after_rows),
      structures = structure_signature(after_rows)
    ),
    before_sig
  )
})

test_that("unconfirmed evidence notices are compact and keep source failures as errors", {
  one <- list(list(reason = "unconfirmed", input_text = "BRCA2", symbol = "brca2"))
  html <- paste(as.character(unresolved_exclusion_notice_ui(one)), collapse = "\n")
  expect_match(html, "unresolved-exclusion-note")
  expect_match(html, "BRCA2 excluded until its identity is confirmed")
  expect_false(grepl("form-message", html))
  expect_false(grepl("Reactome", html))
  expect_false(grepl("brca2 excluded", html))

  two <- list(
    list(reason = "unconfirmed", input_text = "BRCA2"),
    list(reason = "Target is not confirmed", input_text = "ALK")
  )
  html2 <- paste(as.character(unresolved_exclusion_notice_ui(two)), collapse = "\n")
  expect_match(html2, "2 unresolved targets are excluded until identity is confirmed")
  expect_match(html2, "BRCA2")
  expect_match(html2, "ALK")

  mixed <- exclusion_status_ui(
    list(
      list(reason = "unconfirmed", input_text = "BRCA2"),
      list(reason = "missing_uniprot", message = "Structure mapping unavailable.")
    ),
    list(list(message = "Reactome could not be reached."))
  )
  mixed_html <- paste(as.character(mixed), collapse = "\n")
  expect_match(mixed_html, "BRCA2 excluded until its identity is confirmed")
  expect_match(mixed_html, "Structure mapping unavailable")
  expect_match(mixed_html, "form-message error")
  expect_match(mixed_html, "Reactome could not be reached")
})

test_that("Manage project fields still save title, question, and disease", {
  ns <- shiny::NS("home")
  project <- data.frame(
    title = "NSCLC investigation",
    research_question = "Which candidates deserve deeper investigation?",
    disease_label = "Non-small-cell lung cancer",
    disease_name = "non-small cell lung carcinoma",
    disease_ontology_id = "MONDO_0005233",
    disease_resolution_status = "confirmed",
    stringsAsFactors = FALSE
  )
  targets <- data.frame(
    id = "t1",
    input_text = "EGFR",
    display_symbol = "EGFR",
    resolution_status = "confirmed",
    stringsAsFactors = FALSE
  )
  html <- paste(as.character(manage_project_body_ui(ns, project, targets)), collapse = "\n")
  expect_match(html, "Project details")
  expect_match(html, "Save changes")
  expect_match(html, "Disease context")
  expect_match(html, "Change disease context")
  expect_match(html, "Target management")
  expect_match(html, "Reset identity")
  expect_match(html, "Remove from project")
  expect_match(html, "edit_title")
  expect_match(html, "edit_question")
  expect_match(html, "edit_disease")
})

test_that("confirmed remove still writes one TARGET_REMOVED and cancel does not", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("ux-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "UX project",
    "Can the dashboard remove a target safely?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  pid <- created$project_id
  uid <- owner$user$id
  rows <- list_project_targets(db_pool, pid, uid)
  kras <- rows$id[rows$input_text == "KRAS"][[1]]
  before <- list_project_events(db_pool, pid, uid)
  n_removed <- sum(before$event_type == "TARGET_REMOVED")

  blocked <- remove_project_target(db_pool, pid, uid, kras, confirm_removal = FALSE)
  expect_false(blocked$ok)
  expect_equal(nrow(list_project_targets(db_pool, pid, uid)), 2)
  expect_equal(
    sum(list_project_events(db_pool, pid, uid)$event_type == "TARGET_REMOVED"),
    n_removed
  )

  removed <- remove_project_target(db_pool, pid, uid, kras, confirm_removal = TRUE)
  expect_true(removed$ok)
  after <- list_project_events(db_pool, pid, uid)
  expect_equal(sum(after$event_type == "TARGET_REMOVED"), n_removed + 1L)
  expect_equal(nrow(list_project_targets(db_pool, pid, uid)), 1)

  last <- list_project_targets(db_pool, pid, uid)$id[[1]]
  last_block <- remove_project_target(db_pool, pid, uid, last, confirm_removal = TRUE)
  expect_false(last_block$ok)
  expect_match(last_block$message, "at least one")
  expect_equal(nrow(list_project_targets(db_pool, pid, uid)), 1)
  expect_equal(
    sum(list_project_events(db_pool, pid, uid)$event_type == "TARGET_REMOVED"),
    n_removed + 1L
  )
})

test_that("presentation save from Manage project fields still does not rewrite snapshots", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "UX freeze"
  )
  original <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  updated <- update_project_presentation(
    db_pool,
    seed$project_id,
    seed$owner$id,
    "Dashboard title",
    "Dashboard question"
  )
  expect_true(updated$ok)
  live <- get_owned_project(db_pool, seed$project_id, seed$owner$id)
  expect_equal(live$title[[1]], "Dashboard title")
  frozen <- get_owned_snapshot(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(frozen$project_context$model$title, original$project_context$model$title)
})
