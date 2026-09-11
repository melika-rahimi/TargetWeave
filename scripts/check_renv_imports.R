# Fail the image build if DESCRIPTION Imports are missing after renv::restore().
# Do not install undeclared packages from latest CRAN.

desc <- read.dcf("DESCRIPTION", fields = "Imports")[1, 1]
imports <- trimws(unlist(strsplit(gsub("\n", "", desc), ",")))
imports <- sub("\\s*\\(.*$", "", imports)
missing <- setdiff(imports, rownames(installed.packages()))
if (length(missing)) {
  stop("renv restore missing: ", paste(missing, collapse = ", "))
}
cat("renv restore imports present:", paste(imports, collapse = ", "), "\n")
