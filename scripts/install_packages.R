# Ubuntu/Debian system libraries required to compile or load:
#   sudo apt install libpq-dev libsodium-dev
# PostgreSQL itself:
#   docker compose up -d db
#   or a local PostgreSQL 14+ instance

packages <- c(
  "shiny",
  "bslib",
  "DBI",
  "RPostgres",
  "pool",
  "sodium",
  "uuid",
  "httr2",
  "jsonlite",
  "ggplot2",
  "testthat",
  "renv"
)

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  options(
    HTTPUserAgent = sprintf(
      "R/%s R (%s)",
      getRversion(),
      paste(getRversion(), R.version$platform, R.version$arch, R.version$os)
    )
  )

  repos <- c(
    CRAN = sprintf(
      "https://packagemanager.posit.co/cran/__linux__/%s/latest",
      "noble"
    )
  )

  install.packages(missing, repos = repos)
}

message("TargetWeave packages are installed.")
