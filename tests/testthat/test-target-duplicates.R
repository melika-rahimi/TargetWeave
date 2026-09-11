test_that("a project cannot store duplicate unresolved target strings", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  user <- register_user(
    db_pool,
    sprintf("dup-%s@example.com", suffix),
    "correct-horse-battery"
  )

  created <- create_project(
    db_pool,
    user$user$id,
    "Duplicate prevention",
    "The same string must not appear twice.",
    "Non-small-cell lung cancer",
    c("EGFR", "egfr", "EGFR", "KRAS")
  )

  expect_true(created$ok)
  targets <- list_project_targets(db_pool, created$project_id, user$user$id)
  expect_equal(nrow(targets), 2)
  expect_equal(targets$input_text, c("EGFR", "KRAS"))

  insert_error <- db_execute_guarded(
    db_pool,
    "
    INSERT INTO project_targets (id, project_id, input_text, resolution_status)
    VALUES ($1::uuid, $2::uuid, $3, 'unresolved')
    ",
    list(uuid::UUIDgenerate(), created$project_id, "egfr")
  )
  expect_true(inherits(insert_error, "error"))
  expect_match(conditionMessage(insert_error), "idx_project_targets_input_normalized")
})

test_that("two inputs cannot confirm the same Ensembl gene in one project", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  user <- register_user(
    db_pool,
    sprintf("ensg-%s@example.com", suffix),
    "correct-horse-battery"
  )

  created <- create_project(
    db_pool,
    user$user$id,
    "Shared identity",
    "Different strings, one gene.",
    "Non-small-cell lung cancer",
    c("EGFR", "ENSG00000146648")
  )
  expect_true(created$ok)

  targets <- list_project_targets(db_pool, created$project_id, user$user$id)
  first_id <- targets$id[targets$input_text == "EGFR"][[1]]
  second_id <- targets$id[targets$input_text == "ENSG00000146648"][[1]]

  first <- confirm_project_target(
    db_pool,
    first_id,
    user$user$id,
    "EGFR",
    "ENSG00000146648.22",
    "P00533",
    "HGNC:3236"
  )
  expect_true(first$ok)

  stored <- get_owned_target(db_pool, first_id, user$user$id)
  expect_equal(stored$ensembl_gene_id, "ENSG00000146648")
  expect_equal(stored$resolution_status, "confirmed")

  second <- confirm_project_target(
    db_pool,
    second_id,
    user$user$id,
    "EGFR",
    "ENSG00000146648",
    "P00533",
    "HGNC:3236"
  )
  expect_false(second$ok)
  expect_match(second$message, "already confirmed")

  statuses <- list_project_targets(db_pool, created$project_id, user$user$id)
  expect_equal(sum(statuses$resolution_status == "confirmed"), 1)
})

test_that("editing a failed string cannot collide with an existing target", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  user <- register_user(
    db_pool,
    sprintf("edit-%s@example.com", suffix),
    "correct-horse-battery"
  )

  created <- create_project(
    db_pool,
    user$user$id,
    "Edit collision",
    "Renaming must respect uniqueness.",
    "Non-small-cell lung cancer",
    c("EGFR", "NOTAGENE")
  )
  expect_true(created$ok)

  targets <- list_project_targets(db_pool, created$project_id, user$user$id)
  other_id <- targets$id[targets$input_text == "NOTAGENE"][[1]]

  blocked <- update_target_input_text(
    db_pool,
    other_id,
    user$user$id,
    "egfr"
  )
  expect_false(blocked$ok)
  expect_match(blocked$message, "already has that target string")
})
