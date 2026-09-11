test_that("ensure_schema does not delete project_targets rows", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  user <- register_user(
    db_pool,
    sprintf("schema-%s@example.com", suffix),
    "correct-horse-battery"
  )
  created <- create_project(
    db_pool,
    user$user$id,
    "Startup must not delete",
    "Duplicates are repaired only on request.",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  expect_true(created$ok)

  on.exit(
    {
      try(repair_duplicate_project_targets(db_pool, apply = TRUE), silent = TRUE)
      try(ensure_schema(db_pool), silent = TRUE)
    },
    add = TRUE
  )

  DBI::dbExecute(db_pool, "DROP INDEX IF EXISTS idx_project_targets_input_normalized")

  extra_id <- uuid::UUIDgenerate()
  extra_id <- as.character(extra_id)
  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO project_targets (id, project_id, input_text, resolution_status)
    VALUES ($1::uuid, $2::uuid, $3, 'unresolved')
    ",
    params = list(extra_id, created$project_id, "egfr")
  )

  before <- list_project_targets(db_pool, created$project_id, user$user$id)
  expect_equal(nrow(before), 3)

  ensure_schema(db_pool)

  after_startup <- list_project_targets(db_pool, created$project_id, user$user$id)
  expect_equal(nrow(after_startup), 3)
  expect_true(extra_id %in% after_startup$id)

  dry <- repair_duplicate_project_targets(db_pool, apply = FALSE)
  expect_false(isTRUE(dry$apply))
  expect_equal(dry$removed, 0L)
  expect_equal(nrow(dry$rows), 1)
  expect_equal(dry$rows$id, extra_id)
  expect_equal(dry$rows$reason, "duplicate_normalized_input")
  expect_equal(
    nrow(list_project_targets(db_pool, created$project_id, user$user$id)),
    3
  )

  applied <- repair_duplicate_project_targets(db_pool, apply = TRUE)
  expect_true(isTRUE(applied$apply))
  expect_equal(applied$removed, 1L)

  remaining <- list_project_targets(db_pool, created$project_id, user$user$id)
  expect_equal(nrow(remaining), 2)
  expect_false(extra_id %in% remaining$id)
  expect_true(isTRUE(applied$index_results$idx_project_targets_input_normalized$ok))
})
