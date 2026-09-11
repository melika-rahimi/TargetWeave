test_that("candidate target input is split and deduplicated", {
  result <- parse_target_input(
    "EGFR\nKRAS, MET;TP53\nEGFR"
  )

  expect_equal(
    result,
    c("EGFR", "KRAS", "MET", "TP53")
  )
})

test_that("case variants of the same target string are one unresolved input", {
  expect_equal(
    parse_target_input("EGFR\negfr\nEgfr\n EGFR "),
    "EGFR"
  )
  expect_equal(
    dedupe_target_inputs(c("EGFR", "egfr", "KRAS")),
    c("EGFR", "KRAS")
  )
  expect_equal(normalize_target_input_key(" EGFR "), "egfr")
})

test_that("empty target input returns no strings", {
  expect_equal(parse_target_input("   "), character())
  expect_equal(parse_target_input(NULL), character())
})
