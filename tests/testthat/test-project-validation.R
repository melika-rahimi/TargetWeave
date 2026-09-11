test_that("project input requires title, question, disease, and targets", {
  expect_false(
    validate_project_input("", "q", "NSCLC", "EGFR")$ok
  )
  expect_false(
    validate_project_input("title", "", "NSCLC", "EGFR")$ok
  )
  expect_false(
    validate_project_input("title", "q", "", "EGFR")$ok
  )
  expect_false(
    validate_project_input("title", "q", "NSCLC", character())$ok
  )
})

test_that("project input rejects more than eight targets", {
  too_many <- paste0("GENE", seq_len(9))

  result <- validate_project_input(
    "title",
    "question",
    "NSCLC",
    too_many,
    max_targets = 8L
  )

  expect_false(result$ok)
})

test_that("valid NSCLC example is accepted", {
  result <- validate_project_input(
    "Potential therapeutic targets in NSCLC",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS", "MET", "TP53")
  )

  expect_true(result$ok)
  expect_equal(result$target_inputs, c("EGFR", "KRAS", "MET", "TP53"))
})

test_that("EGFR and egfr count as one project target", {
  result <- validate_project_input(
    "title",
    "question",
    "NSCLC",
    c("EGFR", "egfr", "KRAS")
  )

  expect_true(result$ok)
  expect_equal(result$target_inputs, c("EGFR", "KRAS"))
})
