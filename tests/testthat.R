library(testthat)

app_root <- if (file.exists("app.R")) {
  getwd()
} else if (file.exists("../../app.R")) {
  normalizePath("../..")
} else {
  stop("Cannot locate TargetWeave app.R")
}

old <- setwd(app_root)
on.exit(setwd(old), add = TRUE)

test_dir("tests/testthat")
