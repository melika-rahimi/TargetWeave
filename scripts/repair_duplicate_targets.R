# One-time repair for legacy duplicate project_targets rows.
# Default is a dry run. Nothing is deleted unless --apply is passed.
#
#   Rscript scripts/repair_duplicate_targets.R
#   Rscript scripts/repair_duplicate_targets.R --apply

args <- commandArgs(trailingOnly = TRUE)
apply_repair <- "--apply" %in% args

app_root <- if (file.exists("app.R")) {
  getwd()
} else if (file.exists(file.path("..", "app.R"))) {
  normalizePath("..")
} else {
  stop("Run this script from the TargetWeave repository root.")
}

setwd(app_root)

source("R/config.R")
source("R/db/pool.R")
source("R/db/schema.R")

db_pool <- create_db_pool()
on.exit(pool::poolClose(db_pool), add = TRUE)

ensure_schema(db_pool)
report <- repair_duplicate_project_targets(db_pool, apply = apply_repair)

if (nrow(report$rows) == 0) {
  cat("No duplicate project_targets rows found.\n")
  quit(status = 0)
}

cat(
  if (isTRUE(apply_repair)) {
    sprintf("Removed %d duplicate project_targets row(s):\n", report$removed)
  } else {
    sprintf(
      "Dry run: %d duplicate project_targets row(s) would be removed.\n",
      nrow(report$rows)
    )
  }
)
print(report$rows[, c("id", "project_id", "input_text", "resolution_status", "ensembl_gene_id", "keeper_id", "reason"), drop = FALSE])

if (!isTRUE(apply_repair)) {
  cat("\nNo rows were deleted. Re-run with --apply to perform cleanup:\n")
  cat("  Rscript scripts/repair_duplicate_targets.R --apply\n")
}
