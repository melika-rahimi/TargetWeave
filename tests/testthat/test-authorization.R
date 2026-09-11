test_that("user B cannot read or archive user A's project", {
  db_pool <- skip_if_no_postgres()
  on.exit(pool::poolClose(db_pool), add = TRUE)

  ensure_schema(db_pool)

  suffix <- gsub("-", "", uuid::UUIDgenerate())
  email_a <- sprintf("user-a-%s@example.com", suffix)
  email_b <- sprintf("user-b-%s@example.com", suffix)
  password <- "correct-horse-battery"

  user_a <- register_user(db_pool, email_a, password)
  user_b <- register_user(db_pool, email_b, password)

  expect_true(user_a$ok)
  expect_true(user_b$ok)

  created <- create_project(
    db_pool = db_pool,
    user_id = user_a$user$id,
    title = "Potential therapeutic targets in NSCLC",
    research_question = "Which candidates deserve deeper investigation?",
    disease_label = "Non-small-cell lung cancer",
    target_inputs = c("EGFR", "KRAS", "MET", "TP53")
  )

  expect_true(created$ok)

  owned_by_a <- get_owned_project(
    db_pool,
    created$project_id,
    user_a$user$id
  )
  owned_by_b <- get_owned_project(
    db_pool,
    created$project_id,
    user_b$user$id
  )

  expect_false(is.null(owned_by_a))
  expect_true(is.null(owned_by_b))

  targets_a <- list_project_targets(
    db_pool,
    created$project_id,
    user_a$user$id
  )
  targets_b <- list_project_targets(
    db_pool,
    created$project_id,
    user_b$user$id
  )

  expect_equal(nrow(targets_a), 4)
  expect_true(all(targets_a$resolution_status == "unresolved"))
  expect_equal(nrow(targets_b), 0)

  expect_false(
    archive_project(db_pool, created$project_id, user_b$user$id)
  )
  expect_true(
    archive_project(db_pool, created$project_id, user_a$user$id)
  )

  listed_b <- list_projects(db_pool, user_b$user$id, include_archived = TRUE)
  expect_false(created$project_id %in% listed_b$id)
})

test_that("created targets store only input text and unresolved status", {
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
    db_pool = db_pool,
    user_id = user$user$id,
    title = "Identity gate",
    research_question = "Keep targets unresolved until confirmation.",
    disease_label = "Non-small-cell lung cancer",
    target_inputs = c("EGFR")
  )

  expect_true(created$ok)

  row <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT input_text, resolution_status, ensembl_gene_id, uniprot_accession, hgnc_id
    FROM project_targets
    WHERE project_id = $1::uuid
    ",
    params = list(created$project_id)
  )

  expect_equal(row$input_text, "EGFR")
  expect_equal(row$resolution_status, "unresolved")
  expect_true(is.na(row$ensembl_gene_id))
  expect_true(is.na(row$uniprot_accession))
  expect_true(is.na(row$hgnc_id))
})
