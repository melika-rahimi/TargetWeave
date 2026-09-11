delay_all_http <- function(inner, delay = 0.05) {
  function(req) {
    promises::promise(function(resolve, reject) {
      later::later(function() {
        tryCatch(resolve(inner(req)), error = function(e) reject(e))
      }, delay)
    })
  }
}

collect_async <- function(pending, timeout = 6) {
  box <- new.env(parent = emptyenv())
  box$value <- NULL
  bind_external_task(
    pending,
    list(isClosed = function() FALSE),
    function(result) {
      box$value <- result
    }
  )
  drain_tw_later(timeout)
  box$value
}

four_confirmed_targets <- function() {
  rbind(
    confirmed_reactome_target("id-egfr", "EGFR", "P00533", "ENSG00000146648"),
    confirmed_reactome_target("id-kras", "KRAS", "P01116", "ENSG00000133703"),
    confirmed_reactome_target("id-tp53", "TP53", "P04637", "ENSG00000141510"),
    confirmed_reactome_target("id-met", "MET", "P08581", "ENSG00000105976")
  )
}

test_that("submit guard allows one start and blocks rapid repeats", {
  guard <- new_submit_guard(min_interval_sec = 1)
  expect_true(guard$try_start())
  expect_false(guard$try_start())
  guard$finish()
  expect_false(guard$try_start())
})

test_that("target selector labels stay paired with immutable target IDs", {
  ids <- c("id-egfr", "id-kras", "id-met", "id-tp53")
  labels <- c("EGFR", "KRAS", "MET", "TP53")
  choices <- target_selector_choices(ids, labels)
  expect_equal(unname(as.character(choices)), ids)
  expect_equal(names(choices), labels)
  expect_equal(length(unique(names(choices))), 4L)
  rerendered <- target_selector_choices(ids, labels)
  expect_identical(rerendered, choices)
  expect_identical(rerendered[["KRAS"]], "id-kras")
})

test_that("async pathway retrieval keeps EGFR/KRAS/MET/TP53 selector labels", {
  skip_if_not_installed("later")
  payload <- read_fixture("reactome_pathways_p00533_excerpt.json")
  pending <- retrieve_project_pathways(
    list(id = "p1"),
    four_confirmed_targets(),
    db_pool = NULL,
    perform = delay_all_http(reactome_json_perform(payload)),
    version_perform = delay_all_http(reactome_text_perform("97"))
  )
  result <- collect_async(pending)
  expect_equal(result$status, "ready")
  symbols <- as.character(result$pathways$targets$symbol)
  ids <- as.character(result$pathways$targets$project_target_id)
  expect_equal(sort(symbols), c("EGFR", "KRAS", "MET", "TP53"))
  expect_equal(sort(ids), c("id-egfr", "id-kras", "id-met", "id-tp53"))
  choices <- target_selector_choices(ids, symbols)
  expect_equal(unname(choices[names(choices) == "EGFR"]), "id-egfr")
  expect_equal(unname(choices[names(choices) == "MET"]), "id-met")
})

test_that("async structure retrieval keeps distinct target IDs and symbols", {
  skip_if_not_installed("later")
  pending <- retrieve_project_structures(
    list(id = "p1"),
    four_confirmed_targets(),
    db_pool = NULL,
    perform = delay_all_http(rcsb_fixture_perform(expected_accession = ""))
  )
  result <- collect_async(pending, timeout = 8)
  expect_equal(result$status, "ready")
  summary <- result$structures$summary
  expect_equal(sort(as.character(summary$symbol)), c("EGFR", "KRAS", "MET", "TP53"))
  expect_equal(sort(as.character(summary$project_target_id)), c("id-egfr", "id-kras", "id-met", "id-tp53"))
})

test_that("one save action inserts one snapshot; rapid resubmit does not duplicate", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  workspace <- local_workspace(seed)

  save_once <- function() {
    create_evidence_snapshot(
      db_pool,
      seed$owner$id,
      seed$project_id,
      workspace = workspace,
      name = "Snapshot · guarded"
    )
  }

  guard <- new_submit_guard(min_interval_sec = 1)
  created <- list()
  for (i in 1:3) {
    if (!isTRUE(guard$try_start())) {
      next
    }
    created[[length(created) + 1L]] <- save_once()
    guard$finish()
  }
  expect_equal(length(created), 1L)
  expect_true(created[[1]]$ok)
  rows <- list_project_snapshots(db_pool, seed$project_id, seed$owner$id)
  expect_equal(nrow(rows), 1L)
})

test_that("View snapshot loads frozen data without scientific API calls", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  created <- create_evidence_snapshot(
    db_pool,
    seed$owner$id,
    seed$project_id,
    workspace = local_workspace(seed),
    name = "Frozen view"
  )
  expect_true(created$ok)

  viewed <- load_snapshot_for_view(db_pool, created$snapshot_id, seed$owner$id)
  expect_equal(viewed$name, "Frozen view")
  expect_equal(viewed$project_context$model$title, "Potential therapeutic targets in NSCLC")

  ui <- snapshot_view_ui(shiny::NS("research"), viewed)
  html <- as.character(ui)
  expect_match(html, "Preserved snapshot")
  expect_match(html, "This is not the live workspace")
  expect_match(html, "Back to live workspace")

  plot <- snapshot_safe_plot(function() {
    plot_ot_comparison_heatmap(
      viewed$comparison$model$datatype_matrix,
      viewed$comparison$model$disease$name
    )
  })
  expect_false(is.null(plot))
})

test_that("research module one click saves once and View settles", {
  skip_if_not_installed("shiny")
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)

  shiny::testServer(
    mod_research_server,
    args = list(
      db_pool = db_pool,
      user = shiny::reactive(list(id = seed$owner$id)),
      project_id = shiny::reactive(seed$project_id),
      panel_active = shiny::reactive(TRUE),
      workspace_evidence = shiny::reactive(local_workspace(seed))
    ),
    {
      session$setInputs(snapshot_name = "Module save")
      session$setInputs(save_snapshot = 1L)
      session$flushReact()
      rows <- list_project_snapshots(db_pool, seed$project_id, seed$owner$id)
      expect_equal(nrow(rows), 1L)

      session$setInputs(save_snapshot = 2L)
      session$flushReact()
      rows_again <- list_project_snapshots(db_pool, seed$project_id, seed$owner$id)
      expect_equal(nrow(rows_again), 1L)

      sid <- rows$id[[1]]
      session$setInputs(view_snapshot = sid)
      session$flushReact()
      snap <- viewing()
      expect_false(is.null(snap))
      expect_equal(snap$name, "Module save")
      expect_false(is.null(snap$project_context$model$title))
      plot <- snapshot_safe_plot(function() NULL)
      expect_false(is.null(plot))
    }
  )
})
