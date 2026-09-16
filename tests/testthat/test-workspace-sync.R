seed_nsclc <- function(db_pool) {
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(
    db_pool,
    sprintf("ws-%s@example.com", suffix),
    "correct-horse-battery"
  )
  other <- register_user(
    db_pool,
    sprintf("ws-b-%s@example.com", suffix),
    "correct-horse-battery"
  )
  created <- create_project(
    db_pool,
    owner$user$id,
    "Potential therapeutic targets in NSCLC",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  rows <- list_project_targets(db_pool, created$project_id, owner$user$id)
  list(
    owner = owner$user,
    other = other$user,
    project_id = created$project_id,
    egfr_id = rows$id[rows$input_text == "EGFR"][[1]],
    kras_id = rows$id[rows$input_text == "KRAS"][[1]]
  )
}

test_that("unresolved EGFR cannot retrieve overview", {
  expect_false(
    should_retrieve_overview(
      TRUE,
      list(resolution_status = "unresolved", id = "x"),
      NA_character_
    )
  )
  result <- retrieve_target_overview(list(resolution_status = "unresolved"))
  expect_equal(result$status, "not_confirmed")
  expect_null(result$overview)
})

test_that("navigation helpers return one workspace level", {
  expect_equal(workspace_back_destination("overview"), "project")
  expect_equal(workspace_back_destination("resolver"), "project")
  expect_equal(workspace_back_destination("project"), "projects")
  expect_equal(workspace_back_label("overview"), "Back to project")
  expect_equal(workspace_back_label("project"), "Back to projects")
  expect_equal(workspace_panel_label("overview"), "Overview")
  expect_equal(workspace_panel_label("evidence"), "Disease evidence")
  expect_equal(workspace_panel_label("compare"), "Compare evidence")
  expect_equal(workspace_back_destination("evidence"), "project")
  expect_equal(workspace_back_destination("compare"), "project")
  expect_equal(workspace_back_label("evidence"), "Back to project")
  expect_equal(workspace_back_label("compare"), "Back to project")
})

test_that("target status labels remain readable without relying on color", {
  expect_equal(resolution_status_label("confirmed"), "Confirmed")
  expect_equal(resolution_status_label("unresolved"), "Needs confirmation")
  expect_equal(resolution_status_label("ambiguous"), "Ambiguous")
  expect_match(resolution_status_class("confirmed"), "status-pill")
  css <- paste(readLines(file.path(app_root(), "www", "styles.css")), collapse = "\n")
  expect_match(css, "\\.target-select \\.status-pill[[:space:]]*\\{[^}]*align-self:\\s*center")
})

test_that("workspace canvas is a cool light neutral and breadcrumbs are not sticky", {
  css <- paste(readLines(file.path(app_root(), "www", "styles.css")), collapse = "\n")
  expect_match(css, "--tw-bg:\\s*#f5f6fa")
  expect_match(css, "--tw-primary:\\s*#18243d")
  expect_match(css, "--tw-accent:\\s*#b83280")
  expect_match(css, "width:\\s*fit-content")
  expect_false(grepl("--tw-bg:\\s*#f4f1ea", css))
  expect_false(grepl("Iowan Old Style|Palatino", css))
  expect_match(css, "\\.topbar[[:space:]]*\\{[^}]*position: sticky")
  expect_match(css, "\\.workspace-nav[[:space:]]*\\{[^}]*position: relative")
  expect_false(grepl("\\.workspace-nav[[:space:]]*\\{[^}]*position: sticky", css))
})

test_that("confirm EGFR is visible in the same session without reload", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)
  fixture <- seed_nsclc(db_pool)

  selected_id <- fixture$egfr_id
  stale_row <- get_owned_target(db_pool, selected_id, fixture$owner$id)
  expect_equal(stale_row$resolution_status, "unresolved")
  expect_false(should_retrieve_overview(TRUE, stale_row, NA_character_))

  calls <- 0L
  retrieve_if_needed <- function(panel_active, row, last_sig, force = FALSE) {
    if (!should_retrieve_overview(panel_active, row, last_sig, force = force)) {
      return(last_sig)
    }
    calls <<- calls + 1L
    retrieve_target_overview(
      row,
      uniprot_fetch = function(...) stop("unresolved/nav must not hit APIs"),
      ensembl_fetch = function(...) stop("unresolved/nav must not hit APIs")
    )
    overview_fetch_signature(row)
  }

  expect_equal(
    retrieve_if_needed(TRUE, stale_row, NA_character_),
    NA_character_
  )
  expect_equal(calls, 0L)

  accepted <- confirm_project_target(
    db_pool,
    selected_id,
    fixture$owner$id,
    "EGFR",
    "ENSG00000146648",
    "P00533",
    "HGNC:3236"
  )
  expect_true(accepted$ok)

  fresh_row <- get_owned_target(db_pool, selected_id, fixture$owner$id)
  expect_equal(fresh_row$resolution_status, "confirmed")
  expect_equal(as.character(fresh_row$id), as.character(selected_id))
  expect_equal(fresh_row$ensembl_gene_id, "ENSG00000146648")
  expect_equal(fresh_row$uniprot_accession, "P00533")
  expect_true(should_retrieve_overview(TRUE, fresh_row, NA_character_))

  mock_sig <- NA_character_
  mock_retrieve <- function(panel_active, row, force = FALSE) {
    if (!should_retrieve_overview(panel_active, row, mock_sig, force = force)) {
      return(invisible(mock_sig))
    }
    calls <<- calls + 1L
    mock_sig <<- overview_fetch_signature(row)
    mock_sig
  }

  mock_retrieve(TRUE, fresh_row)
  expect_equal(calls, 1L)

  mock_retrieve(FALSE, fresh_row)
  expect_equal(calls, 1L)

  mock_retrieve(TRUE, fresh_row)
  expect_equal(calls, 1L)

  listed <- list_project_targets(db_pool, fixture$project_id, fixture$owner$id)
  egfr <- listed[listed$input_text == "EGFR", ]
  expect_equal(egfr$resolution_status, "confirmed")

  confirm_project_target(
    db_pool,
    fixture$kras_id,
    fixture$owner$id,
    "KRAS",
    "ENSG00000133703",
    "P01116",
    "HGNC:6407"
  )
  kras_row <- get_owned_target(db_pool, fixture$kras_id, fixture$owner$id)
  expect_true(should_retrieve_overview(TRUE, kras_row, mock_sig))
  expect_false(
    identical(overview_fetch_signature(fresh_row), overview_fetch_signature(kras_row))
  )

  mock_retrieve(TRUE, kras_row)
  expect_equal(calls, 2L)
  expect_equal(mock_sig, overview_fetch_signature(kras_row))

  expect_true(is.null(get_owned_target(db_pool, selected_id, fixture$other$id)))
  expect_equal(
    get_owned_target(db_pool, selected_id, fixture$owner$id)$resolution_status,
    "confirmed"
  )
})
