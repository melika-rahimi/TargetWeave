m12_confirm <- function(db_pool, project_id, user_id, symbol, ensembl, uniprot, hgnc) {
  rows <- list_project_targets(db_pool, project_id, user_id)
  tid <- rows$id[rows$input_text == symbol][[1]]
  confirm_project_target(db_pool, tid, user_id, symbol, ensembl, uniprot, hgnc)
}

m12_snapshot_inputs <- function(snapshot) {
  targets <- snapshot$target_identity$model$targets
  vapply(targets, function(row) as.character(row$input_text), character(1))
}

m12_event_n <- function(events, type) {
  sum(events$event_type == type)
}

m12_section_signatures <- function(project, targets) {
  list(
    comparison = comparison_signature(project, targets),
    pathways = pathway_signature(targets),
    literature = literature_signature(project, targets),
    structures = structure_signature(targets)
  )
}

m12_seed_four <- function(db_pool) {
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("m12s-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "NSCLC investigation",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS", "MET", "TP53")
  )
  pid <- created$project_id
  uid <- owner$user$id
  expect_true(m12_confirm(db_pool, pid, uid, "EGFR", "ENSG00000146648", "P00533", "HGNC:3236")$ok)
  expect_true(m12_confirm(db_pool, pid, uid, "KRAS", "ENSG00000133703", "P01116", "HGNC:6407")$ok)
  expect_true(m12_confirm(db_pool, pid, uid, "MET", "ENSG00000105976", "P08581", "HGNC:7029")$ok)
  expect_true(m12_confirm(db_pool, pid, uid, "TP53", "ENSG00000141510", "P04637", "HGNC:11998")$ok)
  expect_true(confirm_project_disease(
    db_pool,
    pid,
    uid,
    "MONDO_0005233",
    "non-small cell lung carcinoma"
  )$ok)
  list(pid = pid, uid = uid, owner = owner$user)
}

test_that("project timeline UI is compact and distinguishes scientific events", {
  html <- as.character(project_timeline_ui(NULL))
  expect_match(html, "No project history recorded yet")
  events <- data.frame(
    event_type = c("TARGET_IDENTITY_CONFIRMED", "TARGET_ADDED", "PROJECT_CREATED"),
    actor_label = c("Melika", "Melika", "Melika"),
    created_at = as.POSIXct(c("2026-09-17 09:44:00", "2026-09-17 09:42:00", "2026-09-15 14:10:00"), tz = "UTC"),
    metadata = c(
      '{"label":"ALK","ensembl_gene_id":"ENSG00000171094","uniprot_accession":"Q9UM73"}',
      '{"label":"ALK"}',
      "{}"
    ),
    stringsAsFactors = FALSE
  )
  html <- as.character(project_timeline_ui(events))
  expect_match(html, "ALK identity confirmed")
  expect_match(html, "ENSG00000171094")
  expect_match(html, "ALK added")
  expect_match(html, "Project created")
  expect_match(html, "timeline-scientific")
  expect_match(html, "timeline-edit")
})

test_that("mutable project targets, audit trail, stale multi-target evidence, and frozen snapshots", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("m12-%s@example.com", suffix), "correct-horse-battery")
  other <- register_user(db_pool, sprintf("m12-b-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "NSCLC investigation",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS", "MET", "TP53")
  )
  expect_true(created$ok)
  pid <- created$project_id
  uid <- owner$user$id

  expect_true(m12_confirm(db_pool, pid, uid, "EGFR", "ENSG00000146648", "P00533", "HGNC:3236")$ok)
  expect_true(m12_confirm(db_pool, pid, uid, "KRAS", "ENSG00000133703", "P01116", "HGNC:6407")$ok)
  expect_true(m12_confirm(db_pool, pid, uid, "MET", "ENSG00000105976", "P08581", "HGNC:7029")$ok)
  expect_true(m12_confirm(db_pool, pid, uid, "TP53", "ENSG00000141510", "P04637", "HGNC:11998")$ok)
  expect_true(confirm_project_disease(
    db_pool,
    pid,
    uid,
    "MONDO_0005233",
    "non-small cell lung carcinoma"
  )$ok)

  live <- list_project_targets(db_pool, pid, uid)
  expect_equal(sort(live$input_text), c("EGFR", "KRAS", "MET", "TP53"))
  project <- get_owned_project(db_pool, pid, uid)
  confirmed <- live[live$resolution_status == "confirmed", , drop = FALSE]
  egfr <- live[live$input_text == "EGFR", , drop = FALSE]
  met <- live[live$input_text == "MET", , drop = FALSE]
  cmp_sig <- comparison_signature(project, live)
  path_sig <- pathway_signature(live)
  egfr_ot_sig <- evidence_fetch_signature(egfr, project)
  revision_before <- as.integer(project$target_set_revision[[1]])

  cache_put(
    db_pool,
    "opentargets",
    cache_key_opentargets_association("ENSG00000146648", "MONDO_0005233"),
    list(kept = TRUE),
    200L,
    Sys.time(),
    Sys.time() + 3600
  )
  expect_equal(
    association_cache_freshness(db_pool, "ENSG00000146648", "MONDO_0005233"),
    "fresh"
  )

  seed <- list(egfr_id = egfr$id[[1]], kras_id = live$id[live$input_text == "KRAS"][[1]])
  snap <- create_evidence_snapshot(
    db_pool,
    uid,
    pid,
    workspace = local_workspace(seed),
    name = "Four-target freeze"
  )
  expect_true(snap$ok)
  original <- get_owned_snapshot(db_pool, snap$snapshot_id, uid)
  expect_equal(sort(m12_snapshot_inputs(original)), c("EGFR", "KRAS", "MET", "TP53"))
  expect_equal(original$target_identity$model$n_targets, 4)

  events <- list_project_events(db_pool, pid, uid)
  expect_equal(m12_event_n(events, "PROJECT_CREATED"), 1)
  expect_equal(m12_event_n(events, "TARGET_ADDED"), 4)
  expect_equal(m12_event_n(events, "TARGET_IDENTITY_CONFIRMED"), 4)
  expect_equal(m12_event_n(events, "DISEASE_IDENTITY_CONFIRMED"), 1)
  expect_equal(m12_event_n(events, "SNAPSHOT_CREATED"), 1)
  expect_true(all(!is.na(events$created_at)))
  expect_true(all(events$actor_user_id == uid))
  expect_equal(events$event_type[[1]], "SNAPSHOT_CREATED")

  fifth <- add_project_target(db_pool, pid, uid, "ALK")
  expect_true(fifth$ok)
  live_unresolved <- list_project_targets(db_pool, pid, uid)
  expect_equal(nrow(live_unresolved), 5)
  expect_equal(
    live_unresolved$resolution_status[live_unresolved$input_text == "ALK"],
    "unresolved"
  )
  project_unresolved <- get_owned_project(db_pool, pid, uid)
  confirmed_unresolved <- live_unresolved[live_unresolved$resolution_status == "confirmed", , drop = FALSE]
  expect_equal(
    comparison_signature(project_unresolved, live_unresolved),
    cmp_sig
  )
  expect_equal(as.integer(project_unresolved$target_set_revision[[1]]), revision_before)

  dup <- add_project_target(db_pool, pid, uid, "alk")
  expect_false(dup$ok)
  expect_match(dup$message, "already has that target")

  expect_true(m12_confirm(db_pool, pid, uid, "ALK", "ENSG00000171094", "Q9UM73", "HGNC:427")$ok)
  live_five <- list_project_targets(db_pool, pid, uid)
  project_five <- get_owned_project(db_pool, pid, uid)
  confirmed_five <- live_five[live_five$resolution_status == "confirmed", , drop = FALSE]
  expect_equal(nrow(confirmed_five), 5)
  expect_gt(as.integer(project_five$target_set_revision[[1]]), revision_before)
  next_cmp <- comparison_signature(project_five, live_five)
  next_path <- pathway_signature(live_five)
  expect_true(live_signature_stale(cmp_sig, next_cmp))
  expect_true(live_signature_stale(path_sig, next_path))
  expect_true(should_retrieve_comparison(TRUE, project_five, live_five, cmp_sig))
  expect_true(should_retrieve_pathways(TRUE, live_five, path_sig))

  held_cmp <- hold_stale_multi_target_result(
    list(comparison = list(ok = TRUE, disease = "MONDO_0005233")),
    cmp_sig,
    next_cmp,
    "comparison"
  )
  expect_true(held_cmp$suppress_auto_retrieve)
  expect_true(held_cmp$current$stale_target_set)
  expect_equal(held_cmp$current$stale_message, TARGET_SET_STALE_MESSAGE)
  expect_equal(held_cmp$current$comparison$disease, "MONDO_0005233")
  expect_true(suppress_auto_retrieve_while_stale(held_cmp$current, "comparison"))
  expect_false(suppress_auto_retrieve_while_stale(held_cmp$current, "comparison", force = TRUE))

  held_path <- hold_stale_multi_target_result(
    list(pathways = list(membership = TRUE)),
    path_sig,
    next_path,
    "pathways"
  )
  expect_true(held_path$current$stale_target_set)

  egfr_after <- live_five[live_five$input_text == "EGFR", , drop = FALSE]
  expect_false(should_retrieve_ot_evidence(TRUE, egfr_after, project_five, egfr_ot_sig))
  expect_equal(egfr_after$ensembl_gene_id[[1]], "ENSG00000146648")
  expect_equal(egfr_after$uniprot_accession[[1]], "P00533")
  expect_equal(
    association_cache_freshness(db_pool, "ENSG00000146648", "MONDO_0005233"),
    "fresh"
  )

  frozen <- get_owned_snapshot(db_pool, snap$snapshot_id, uid)
  expect_equal(sort(m12_snapshot_inputs(frozen)), c("EGFR", "KRAS", "MET", "TP53"))
  expect_equal(frozen$target_identity$model$n_targets, original$target_identity$model$n_targets)
  expect_false("ALK" %in% m12_snapshot_inputs(frozen))

  later <- create_evidence_snapshot(
    db_pool,
    uid,
    pid,
    workspace = local_workspace(seed),
    name = "Five-target capture"
  )
  expect_true(later$ok)
  later_snap <- get_owned_snapshot(db_pool, later$snapshot_id, uid)
  expect_true("ALK" %in% m12_snapshot_inputs(later_snap))
  expect_equal(later_snap$target_identity$model$n_targets, 5)

  exported <- export_owned_snapshot(db_pool, uid, snap$snapshot_id, format = "html")
  expect_true(exported$ok)
  on.exit(unlink(exported$path), add = TRUE)
  html <- paste(readLines(exported$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "EGFR")
  expect_match(html, "KRAS")
  expect_match(html, "MET")
  expect_match(html, "TP53")
  expect_false(grepl("\\bALK\\b", html))

  blocked_remove <- remove_project_target(
    db_pool,
    pid,
    uid,
    met$id[[1]],
    confirm_removal = FALSE
  )
  expect_false(blocked_remove$ok)
  expect_true(isTRUE(blocked_remove$needs_confirmation))

  removed <- remove_project_target(
    db_pool,
    pid,
    uid,
    met$id[[1]],
    confirm_removal = TRUE
  )
  expect_true(removed$ok)
  after_remove <- list_project_targets(db_pool, pid, uid)
  expect_false(met$id[[1]] %in% after_remove$id)
  expect_false("MET" %in% after_remove$input_text)
  still_frozen <- get_owned_snapshot(db_pool, snap$snapshot_id, uid)
  expect_true("MET" %in% m12_snapshot_inputs(still_frozen))

  readd <- add_project_target(db_pool, pid, uid, "MET")
  expect_true(readd$ok)
  expect_true(isTRUE(readd$revived))
  expect_equal(readd$target_id, met$id[[1]])
  revived <- list_project_targets(db_pool, pid, uid)
  met_row <- revived[revived$id == met$id[[1]], , drop = FALSE]
  expect_equal(nrow(met_row), 1)
  expect_equal(met_row$resolution_status[[1]], "unresolved")
  expect_true(is.na(met_row$ensembl_gene_id[[1]]) || !nzchar(as.character(met_row$ensembl_gene_id[[1]])))

  events_after <- list_project_events(db_pool, pid, uid)
  expect_equal(m12_event_n(events_after, "PROJECT_CREATED"), 1)
  expect_equal(m12_event_n(events_after, "TARGET_ADDED"), 6)
  expect_equal(m12_event_n(events_after, "TARGET_REMOVED"), 1)
  expect_equal(m12_event_n(events_after, "TARGET_IDENTITY_CONFIRMED"), 5)
  expect_equal(m12_event_n(events_after, "SNAPSHOT_CREATED"), 2)
  expect_equal(m12_event_n(events_after, "DISEASE_IDENTITY_CONFIRMED"), 1)
  met_added <- events_after[
    events_after$event_type == "TARGET_ADDED" & events_after$project_target_id == met$id[[1]],
    ,
    drop = FALSE
  ]
  expect_equal(nrow(met_added), 2)

  expect_false(add_project_target(db_pool, pid, other$user$id, "BRAF")$ok)
  expect_false(remove_project_target(
    db_pool,
    pid,
    other$user$id,
    egfr$id[[1]],
    confirm_removal = TRUE
  )$ok)
  expect_equal(nrow(list_project_events(db_pool, pid, other$user$id)), 0)
  expect_equal(nrow(list_project_targets(db_pool, pid, uid)), 5)
})

test_that("max eight active targets is preserved", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("m12max-%s@example.com", suffix), "correct-horse-battery")
  names <- paste0("GENE", seq_len(8))
  created <- create_project(
    db_pool,
    owner$user$id,
    "Eight target cap",
    "Does the live workspace enforce the cap?",
    "Non-small-cell lung cancer",
    names
  )
  expect_true(created$ok)
  ninth <- add_project_target(db_pool, created$project_id, owner$user$id, "GENE9")
  expect_false(ninth$ok)
  expect_match(ninth$message, "at most 8")
  expect_equal(nrow(list_project_targets(db_pool, created$project_id, owner$user$id)), 8)
})

test_that("a project cannot remove its last live target", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("m12min-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "Single target project",
    "Must keep at least one live target.",
    "Non-small-cell lung cancer",
    "EGFR"
  )
  rows <- list_project_targets(db_pool, created$project_id, owner$user$id)
  blocked <- remove_project_target(
    db_pool,
    created$project_id,
    owner$user$id,
    rows$id[[1]],
    confirm_removal = TRUE
  )
  expect_false(blocked$ok)
  expect_match(blocked$message, "at least one")
  expect_equal(nrow(list_project_targets(db_pool, created$project_id, owner$user$id)), 1)
})

test_that("an unrelated schema_migrations version is never relabeled as M12", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  version <- 50L
  DBI::dbExecute(
    db_pool,
    "DELETE FROM schema_migrations WHERE version = $1",
    params = list(version)
  )
  on.exit(
    try(
      DBI::dbExecute(
        db_pool,
        "DELETE FROM schema_migrations WHERE version = $1",
        params = list(version)
      ),
      silent = TRUE
    ),
    add = TRUE
  )
  DBI::dbExecute(
    db_pool,
    "INSERT INTO schema_migrations (version, name) VALUES ($1, $2)",
    params = list(version, "historical_unrelated")
  )
  ran_existing_m12 <- FALSE
  apply_named_migration(
    db_pool,
    version,
    "m12_target_lifecycle",
    function(db) {
      ran_existing_m12 <<- TRUE
    }
  )
  expect_false(ran_existing_m12)
  ran <- FALSE
  expect_error(
    apply_named_migration(
      db_pool,
      version,
      "m12_target_lifecycle_probe",
      function(db) {
        ran <<- TRUE
      }
    ),
    "already applied as 'historical_unrelated'"
  )
  expect_false(ran)
  occupant <- DBI::dbGetQuery(
    db_pool,
    "SELECT name FROM schema_migrations WHERE version = $1",
    params = list(version)
  )
  expect_equal(occupant$name[[1]], "historical_unrelated")
  named <- DBI::dbGetQuery(
    db_pool,
    "SELECT version FROM schema_migrations WHERE name = $1",
    params = list("m12_target_lifecycle")
  )
  expect_equal(as.integer(named$version[[1]]), 3L)
  ensure_schema(db_pool)
  occupant_after <- DBI::dbGetQuery(
    db_pool,
    "SELECT name FROM schema_migrations WHERE version = $1",
    params = list(version)
  )
  expect_equal(occupant_after$name[[1]], "historical_unrelated")
})

test_that("multi-target signatures follow the confirmed scientific identity set", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- m12_seed_four(db_pool)
  pid <- seed$pid
  uid <- seed$uid

  four <- list_project_targets(db_pool, pid, uid)
  project <- get_owned_project(db_pool, pid, uid)
  sig_four <- m12_section_signatures(project, four)
  expect_match(sig_four$comparison, "ENSG00000146648")
  expect_false(grepl("target_set_revision", sig_four$comparison, fixed = TRUE))
  expect_equal(sig_four$pathways, confirmed_scientific_target_set(four))
  expect_equal(sig_four$structures, sig_four$pathways)
  expect_match(sig_four$literature, "MONDO_0005233")

  expect_true(add_project_target(db_pool, pid, uid, "ALK")$ok)
  after_unresolved <- list_project_targets(db_pool, pid, uid)
  project_unresolved <- get_owned_project(db_pool, pid, uid)
  sig_unresolved <- m12_section_signatures(project_unresolved, after_unresolved)
  expect_identical(sig_unresolved, sig_four)
  dummy <- list(comparison = list(ok = TRUE), pathways = list(ok = TRUE), literature = list(ok = TRUE), structures = list(ok = TRUE))
  expect_false(hold_stale_multi_target_result(dummy, sig_four$comparison, sig_unresolved$comparison, "comparison")$changed)
  expect_false(hold_stale_multi_target_result(dummy, sig_four$pathways, sig_unresolved$pathways, "pathways")$changed)
  expect_false(hold_stale_multi_target_result(dummy, sig_four$literature, sig_unresolved$literature, "literature")$changed)
  expect_false(hold_stale_multi_target_result(dummy, sig_four$structures, sig_unresolved$structures, "structures")$changed)

  expect_true(m12_confirm(db_pool, pid, uid, "ALK", "ENSG00000171094", "Q9UM73", "HGNC:427")$ok)
  after_alk <- list_project_targets(db_pool, pid, uid)
  project_alk <- get_owned_project(db_pool, pid, uid)
  sig_alk <- m12_section_signatures(project_alk, after_alk)
  expect_true(live_signature_stale(sig_four$comparison, sig_alk$comparison))
  expect_true(live_signature_stale(sig_four$pathways, sig_alk$pathways))
  expect_true(live_signature_stale(sig_four$literature, sig_alk$literature))
  expect_true(live_signature_stale(sig_four$structures, sig_alk$structures))
  expect_true(hold_stale_multi_target_result(dummy, sig_four$comparison, sig_alk$comparison, "comparison")$suppress_auto_retrieve)
  expect_true(hold_stale_multi_target_result(dummy, sig_four$pathways, sig_alk$pathways, "pathways")$suppress_auto_retrieve)

  met_id <- after_alk$id[after_alk$input_text == "MET"][[1]]
  expect_true(remove_project_target(db_pool, pid, uid, met_id, confirm_removal = TRUE)$ok)
  after_remove <- list_project_targets(db_pool, pid, uid)
  project_remove <- get_owned_project(db_pool, pid, uid)
  sig_remove <- m12_section_signatures(project_remove, after_remove)
  expect_true(live_signature_stale(sig_alk$comparison, sig_remove$comparison))
  expect_true(live_signature_stale(sig_alk$pathways, sig_remove$pathways))
  expect_true(live_signature_stale(sig_alk$literature, sig_remove$literature))
  expect_true(live_signature_stale(sig_alk$structures, sig_remove$structures))

  expect_true(add_project_target(db_pool, pid, uid, "MET")$ok)
  expect_true(m12_confirm(db_pool, pid, uid, "MET", "ENSG00000105976", "P08581", "HGNC:7029")$ok)
  restored <- list_project_targets(db_pool, pid, uid)
  project_restored <- get_owned_project(db_pool, pid, uid)
  sig_restored <- m12_section_signatures(project_restored, restored)
  expect_gt(as.integer(project_restored$target_set_revision[[1]]), as.integer(project_alk$target_set_revision[[1]]))
  expect_identical(sig_restored, sig_alk)
  expect_false(live_signature_stale(sig_alk$comparison, sig_restored$comparison))
  expect_false(hold_stale_multi_target_result(dummy, sig_alk$comparison, sig_restored$comparison, "comparison")$changed)
  expect_false(hold_stale_multi_target_result(dummy, sig_alk$pathways, sig_restored$pathways, "pathways")$changed)
  expect_false(hold_stale_multi_target_result(dummy, sig_alk$literature, sig_restored$literature, "literature")$changed)
  expect_false(hold_stale_multi_target_result(dummy, sig_alk$structures, sig_restored$structures, "structures")$changed)
})

test_that("failed target mutations do not write audit events", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- m12_seed_four(db_pool)
  pid <- seed$pid
  uid <- seed$uid
  before <- list_project_events(db_pool, pid, uid)
  n_added <- m12_event_n(before, "TARGET_ADDED")
  n_removed <- m12_event_n(before, "TARGET_REMOVED")
  n_confirmed <- m12_event_n(before, "TARGET_IDENTITY_CONFIRMED")

  expect_false(add_project_target(db_pool, pid, uid, "EGFR")$ok)
  expect_false(add_project_target(db_pool, pid, uid, "egfr")$ok)
  after_dup <- list_project_events(db_pool, pid, uid)
  expect_equal(m12_event_n(after_dup, "TARGET_ADDED"), n_added)

  solo <- create_project(
    db_pool,
    uid,
    "Solo",
    "Keep one target.",
    "Non-small-cell lung cancer",
    "BRAF"
  )
  solo_rows <- list_project_targets(db_pool, solo$project_id, uid)
  expect_false(remove_project_target(
    db_pool,
    solo$project_id,
    uid,
    solo_rows$id[[1]],
    confirm_removal = TRUE
  )$ok)
  solo_events <- list_project_events(db_pool, solo$project_id, uid)
  expect_equal(m12_event_n(solo_events, "TARGET_REMOVED"), 0)
  expect_equal(nrow(list_project_targets(db_pool, solo$project_id, uid)), 1)

  live <- list_project_targets(db_pool, pid, uid)
  last_id <- live$id[live$input_text == "EGFR"][[1]]
  already <- m12_confirm(db_pool, pid, uid, "EGFR", "ENSG00000146648", "P00533", "HGNC:3236")
  expect_false(already$ok)
  missing <- confirm_project_target(db_pool, last_id, uid, "EGFR", "ENSG00000146648", "", "HGNC:3236")
  expect_false(missing$ok)
  after_confirm <- list_project_events(db_pool, pid, uid)
  expect_equal(m12_event_n(after_confirm, "TARGET_IDENTITY_CONFIRMED"), n_confirmed)
  expect_equal(m12_event_n(after_confirm, "TARGET_REMOVED"), n_removed)
  expect_equal(m12_event_n(after_confirm, "TARGET_ADDED"), n_added)
})

test_that("event backfill uses only timestamps already on project rows", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("m12bf-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "Backfill source",
    "Derive events only from stored rows.",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  pid <- created$project_id
  uid <- owner$user$id
  rows <- list_project_targets(db_pool, pid, uid)
  egfr <- rows$id[rows$input_text == "EGFR"][[1]]
  confirmed_at <- as.POSIXct("2026-09-10 08:00:00", tz = "UTC")
  DBI::dbExecute(
    db_pool,
    "
    UPDATE project_targets
    SET resolution_status = 'confirmed',
        display_symbol = 'EGFR',
        ensembl_gene_id = 'ENSG00000146648',
        uniprot_accession = 'P00533',
        confirmed_at = $2
    WHERE id = $1::uuid
    ",
    params = list(egfr, confirmed_at)
  )
  DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET disease_resolution_status = 'confirmed',
        disease_ontology_id = 'MONDO_0005233',
        disease_name = 'non-small cell lung carcinoma',
        disease_confirmed_at = $2
    WHERE id = $1::uuid
    ",
    params = list(pid, as.POSIXct("2026-09-10 09:00:00", tz = "UTC"))
  )
  DBI::dbExecute(db_pool, "DELETE FROM project_events WHERE project_id = $1::uuid", params = list(pid))
  backfill_project_events_from_existing_rows(db_pool)
  events <- list_project_events(db_pool, pid, uid)
  expect_equal(m12_event_n(events, "PROJECT_CREATED"), 1)
  expect_equal(m12_event_n(events, "TARGET_ADDED"), 2)
  expect_equal(m12_event_n(events, "TARGET_IDENTITY_CONFIRMED"), 1)
  expect_equal(m12_event_n(events, "DISEASE_IDENTITY_CONFIRMED"), 1)
  expect_equal(m12_event_n(events, "TARGET_REMOVED"), 0)
  expect_equal(m12_event_n(events, "SNAPSHOT_CREATED"), 0)
  confirm_ev <- events[events$event_type == "TARGET_IDENTITY_CONFIRMED", , drop = FALSE]
  expect_equal(
    format(as.POSIXct(confirm_ev$created_at[[1]], tz = "UTC"), tz = "UTC"),
    format(confirmed_at, tz = "UTC")
  )
  expect_true(all(events$actor_user_id == uid))
  backfill_project_events_from_existing_rows(db_pool)
  expect_equal(nrow(list_project_events(db_pool, pid, uid)), nrow(events))
})
