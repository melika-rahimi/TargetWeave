test_that("version suffixes are stripped without changing identifier type", {
  gene <- parse_ensembl_stable_id("ENSG00000146648.22")
  expect_equal(gene$type, "gene")
  expect_equal(gene$stable_id, "ENSG00000146648")
  expect_equal(gene$version, "22")
  expect_equal(normalize_ensembl_id("ENSG00000146648.22"), "ENSG00000146648")
  expect_equal(canonical_ensembl_gene_id("ENSG00000146648.22"), "ENSG00000146648")
  expect_equal(canonical_ensembl_gene_id("ENSG00000146648"), "ENSG00000146648")

  tx <- parse_ensembl_stable_id("ENST00000275493.7")
  expect_equal(tx$type, "transcript")
  expect_equal(tx$stable_id, "ENST00000275493")

  prot <- parse_ensembl_stable_id("ENSP00000275493.2")
  expect_equal(prot$type, "protein")
  expect_equal(prot$stable_id, "ENSP00000275493")
})

test_that("ENST and ENSP are not treated as Ensembl gene IDs", {
  expect_true(is.na(canonical_ensembl_gene_id("ENST00000275493.7")))
  expect_true(is.na(canonical_ensembl_gene_id("ENSP00000275493.2")))
  expect_equal(ensembl_id_type("ENST00000275493"), "transcript")
  expect_equal(ensembl_id_type("ENSP00000275493"), "protein")
  expect_equal(ensembl_id_type("ENSG00000146648.22"), "gene")
  expect_equal(
    canonical_ensembl_gene_ids(c(
      "ENSG00000146648.22",
      "ENST00000275493.7",
      "ENSP00000275493.2"
    )),
    "ENSG00000146648"
  )
})

test_that("versioned UniProt GeneId plus unversioned Ensembl lookup is one EGFR candidate", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))
  expect_equal(uniprot$entries[[1]]$ensembl_gene_ids, "ENSG00000146648")
  expect_equal(uniprot$entries[[1]]$accession, "P00533")
  expect_true(uniprot$entries[[1]]$reviewed)

  ensembl <- parse_ensembl_gene(read_fixture("ensembl_egfr.json"))
  expect_equal(ensembl$ensembl_gene_id, "ENSG00000146648")

  result <- normalize_resolution_result(
    input_text = "EGFR",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(ensembl)
    )
  )

  expect_equal(result$status, "unresolved")
  expect_equal(length(result$candidates), 1)
  expect_equal(result$candidates[[1]]$display_symbol, "EGFR")
  expect_equal(result$candidates[[1]]$uniprot_accession, "P00533")
  expect_equal(result$candidates[[1]]$ensembl_gene_id, "ENSG00000146648")
  expect_equal(result$candidates[[1]]$hgnc_id, "HGNC:3236")
  expect_true(result$candidates[[1]]$uniprot_reviewed)
  expect_equal(result$candidates[[1]]$cross_check, "matched")
  expect_equal(result$candidates[[1]]$match_type, "exact_current_symbol")
  expect_true(candidate_is_confirmable(result$candidates[[1]]))
  expect_false(identical(result$status, "confirmed"))
  expect_false(identical(result$status, "ambiguous"))
})

test_that("truly different ENSG IDs remain a mismatch", {
  uniprot <- parse_uniprot_search(read_fixture("uniprot_egfr.json"))
  other <- parse_ensembl_gene(read_fixture("ensembl_met.json"))

  result <- normalize_resolution_result(
    input_text = "EGFR",
    input_class = "gene_symbol",
    uniprot_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "uniprot"),
      entries = uniprot$entries
    ),
    ensembl_fetch = list(
      result = new_api_result(TRUE, status = 200, source = "ensembl"),
      genes = list(other)
    )
  )

  expect_true(length(result$candidates) >= 1)
  cross <- vapply(result$candidates, function(c) c$cross_check, character(1))
  expect_false(any(cross == "matched"))
})
