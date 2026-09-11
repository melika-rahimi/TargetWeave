test_that("confirmation persists identifiers and stays owner-scoped", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  user_a <- register_user(db_pool, sprintf("a-%s@example.com", suffix), "correct-horse-battery")
  user_b <- register_user(db_pool, sprintf("b-%s@example.com", suffix), "correct-horse-battery")

  created <- create_project(
    db_pool,
    user_a$user$id,
    "Potential therapeutic targets in NSCLC",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR")
  )

  expect_true(created$ok)
  targets <- list_project_targets(db_pool, created$project_id, user_a$user$id)
  target_id <- targets$id[[1]]

  expect_true(is.na(targets$ensembl_gene_id[[1]]) || !nzchar(targets$ensembl_gene_id[[1]]))

  payload <- list(
    input_text = "EGFR",
    status = "unresolved",
    candidates = list(
      list(
        candidate_key = "P00533|ENSG00000146648",
        display_symbol = "EGFR",
        ensembl_gene_id = "ENSG00000146648",
        uniprot_accession = "P00533"
      )
    )
  )

  expect_true(
    save_resolution_lookup(
      db_pool,
      target_id,
      user_a$user$id,
      "unresolved",
      payload
    )
  )

  expect_false(
    save_resolution_lookup(
      db_pool,
      target_id,
      user_b$user$id,
      "unresolved",
      payload
    )
  )

  denied <- confirm_project_target(
    db_pool,
    target_id,
    user_b$user$id,
    "EGFR",
    "ENSG00000146648",
    "P00533",
    "HGNC:3236"
  )
  expect_false(denied$ok)

  accepted <- confirm_project_target(
    db_pool,
    target_id,
    user_a$user$id,
    "EGFR",
    "ENSG00000146648",
    "P00533",
    "HGNC:3236"
  )
  expect_true(accepted$ok)

  owned <- get_owned_target(db_pool, target_id, user_a$user$id)
  foreign <- get_owned_target(db_pool, target_id, user_b$user$id)

  expect_equal(owned$resolution_status, "confirmed")
  expect_equal(owned$ensembl_gene_id, "ENSG00000146648")
  expect_equal(owned$uniprot_accession, "P00533")
  expect_false(is.na(owned$confirmed_at))
  expect_true(is.null(foreign))
})

test_that("new targets still start unresolved without fabricated identifiers", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)
  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  user <- register_user(
    db_pool,
    sprintf("owner-%s@example.com", suffix),
    "correct-horse-battery"
  )

  created <- create_project(
    db_pool,
    user$user$id,
    "Identity gate",
    "Keep targets unresolved until confirmation.",
    "Non-small-cell lung cancer",
    c("EGFR")
  )

  row <- list_project_targets(db_pool, created$project_id, user$user$id)

  expect_equal(row$resolution_status, "unresolved")
  expect_true(is.na(row$ensembl_gene_id) || identical(row$ensembl_gene_id, NA_character_))
  expect_true(is.na(row$uniprot_accession) || identical(row$uniprot_accession, NA_character_))
  expect_true(is.na(row$hgnc_id) || identical(row$hgnc_id, NA_character_))
})
