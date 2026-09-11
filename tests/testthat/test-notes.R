test_that("owner can create, edit, and delete project and target notes", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)

  created <- create_research_note(
    db_pool,
    seed$owner$id,
    seed$project_id,
    "Follow up EGFR coverage before the lab meeting."
  )
  expect_true(created$ok)
  expect_equal(created$note$scope, "project")

  edited <- update_research_note(
    db_pool,
    created$note_id,
    seed$owner$id,
    "Updated project note."
  )
  expect_true(edited$ok)
  expect_equal(edited$note$body, "Updated project note.")

  target_note <- create_research_note(
    db_pool,
    seed$owner$id,
    seed$project_id,
    "Notes for EGFR kinase coverage.",
    scope = "target",
    project_target_id = seed$egfr_id
  )
  expect_true(target_note$ok)

  listed <- list_research_notes(db_pool, seed$project_id, seed$owner$id)
  expect_true(nrow(listed) >= 2)

  expect_true(delete_research_note(db_pool, created$note_id, seed$owner$id))
  expect_null(get_owned_note(db_pool, created$note_id, seed$owner$id))
})

test_that("another user cannot read or edit notes and notes stay out of api_cache", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)

  created <- create_research_note(
    db_pool,
    seed$owner$id,
    seed$project_id,
    "SECRETNOTE-EGFR-ONLY"
  )
  expect_true(created$ok)
  expect_null(get_owned_note(db_pool, created$note_id, seed$other$id))
  expect_equal(nrow(list_research_notes(db_pool, seed$project_id, seed$other$id)), 0)
  expect_false(update_research_note(db_pool, created$note_id, seed$other$id, "hijack")$ok)
  expect_false(delete_research_note(db_pool, created$note_id, seed$other$id))

  leaked <- DBI::dbGetQuery(
    db_pool,
    "SELECT 1 FROM api_cache WHERE response::text LIKE '%SECRETNOTE-EGFR-ONLY%'"
  )
  expect_equal(nrow(leaked), 0)
  expect_equal(cache_key_uniprot_symbol("EGFR"), cache_key_uniprot_symbol("EGFR"))
})

test_that("research modules do not call httr2", {
  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  research <- paste(readLines(file.path(app_root(), "R/modules/mod_research.R")), collapse = "\n")
  notes <- paste(readLines(file.path(app_root(), "R/modules/mod_notes.R")), collapse = "\n")
  expect_match(home, "Notes / Snapshots")
  expect_false(grepl("httr2", research))
  expect_false(grepl("httr2", notes))
  expect_equal(workspace_panel_label("research"), "Notes / Snapshots")
})
