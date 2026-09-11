# Live UniProt/Ensembl availability vs semantic cross-check.
# Run from repo root. Does not write fixtures.

source("tests/testthat/helper-app.R")

canonical <- list(
  EGFR = list(uniprot = "P00533", ensg = "ENSG00000146648"),
  KRAS = list(uniprot = "P01116", ensg = "ENSG00000133703"),
  TP53 = list(uniprot = "P04637", ensg = "ENSG00000141510"),
  MET = list(uniprot = "P08581", ensg = "ENSG00000105976")
)

summarize_symbol <- function(symbol) {
  cat("\n==== ", symbol, " ====\n", sep = "")
  t0 <- proc.time()[["elapsed"]]
  result <- resolve_target_identity(symbol, db_pool = NULL)
  assessment <- identity_live_assessment(result)
  elapsed <- round(proc.time()[["elapsed"]] - t0, 1)
  cat("availability=", assessment$availability, "\n", sep = "")
  cat("semantic=", assessment$semantic %||% "NA", "\n", sep = "")
  cat(
    "UniProt accession=", assessment$uniprot_accession %||% "NA",
    " http_ok=", assessment$uniprot_http_ok,
    " status=", assessment$uniprot_status %||% "NA",
    "\n",
    sep = ""
  )
  cat(
    "UniProt Ensembl GeneId=", assessment$uniprot_ensembl_gene_id %||% "NA",
    "\n",
    sep = ""
  )
  cat("canonical normalized ENSG=", assessment$canonical_ensg %||% "NA", "\n", sep = "")
  cat(
    "Ensembl returned ENSG=", assessment$ensembl_ensg %||% "NA",
    " http_ok=", assessment$ensembl_http_ok,
    " status=", assessment$ensembl_status %||% "NA",
    " error=", assessment$ensembl_error %||% "",
    "\n",
    sep = ""
  )
  cat(
    "cross_check=", assessment$cross_check %||% "NA",
    " n_candidates=", assessment$n_candidates,
    " elapsed_s=", elapsed,
    "\n",
    sep = ""
  )
  expected <- canonical[[symbol]]
  if (!is.null(expected) && identical(assessment$availability, "sources_available")) {
    cat(
      "expected uniprot=", expected$uniprot,
      " expected ENSG=", expected$ensg,
      "\n",
      sep = ""
    )
  }
  invisible(list(result = result, assessment = assessment))
}

for (symbol in names(canonical)) {
  summarize_symbol(symbol)
}
