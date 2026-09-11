test_that("user-authored strings are escaped in UI and HTML export", {
  skip_if_not_installed("shiny")
  library(shiny)
  xss <- "<script>alert(1)</script>"
  html <- as.character(h1(xss))
  expect_false(grepl("<script>alert", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;alert")
  expect_equal(html_esc(xss), "&lt;script&gt;alert(1)&lt;/script&gt;")
  expect_match(html_esc_attr("x\"y"), "&quot;")
})

test_that("support sanitizer strips tags before storage", {
  expect_equal(sanitize_plain_text("Hello <b>there</b>"), "Hello there")
  expect_false(grepl("<script>", sanitize_plain_text("<script>alert(1)</script>")))
})
