test_that("UniProt EGFR fixture parses accession, reviewed flag, and xrefs", {
  payload <- read_fixture("uniprot_egfr.json")
  parsed <- parse_uniprot_search(payload)

  expect_true(parsed$ok)
  expect_equal(length(parsed$entries), 1)
  expect_equal(parsed$entries[[1]]$accession, "P00533")
  expect_equal(parsed$entries[[1]]$display_symbol, "EGFR")
  expect_true(parsed$entries[[1]]$reviewed)
  expect_equal(parsed$entries[[1]]$ensembl_gene_ids, "ENSG00000146648")
  expect_equal(parsed$entries[[1]]$hgnc_id, "HGNC:3236")
  expect_equal(parsed$entries[[1]]$gene_synonyms, character())
})

test_that("UniProt empty search is a valid empty result, not an error", {
  parsed <- parse_uniprot_search(read_fixture("uniprot_empty.json"))

  expect_true(parsed$ok)
  expect_equal(length(parsed$entries), 0)
})

test_that("malformed UniProt payload does not crash", {
  parsed <- parse_uniprot_search("not-a-list")

  expect_false(parsed$ok)
  expect_equal(length(parsed$entries), 0)
})

test_that("Ensembl EGFR fixture parses location and biotype", {
  gene <- parse_ensembl_gene(read_fixture("ensembl_egfr.json"))

  expect_equal(gene$ensembl_gene_id, "ENSG00000146648")
  expect_equal(gene$display_symbol, "EGFR")
  expect_equal(gene$biotype, "protein_coding")
  expect_equal(gene$seq_region, "7")
  expect_equal(gene$hgnc_id, "HGNC:3236")
})

test_that("Ensembl error payload is not treated as a gene", {
  expect_null(parse_ensembl_gene(read_fixture("ensembl_error.json")))
})

test_that("UniProt SLTM fixture exposes MET as a documented gene synonym", {
  parsed <- parse_uniprot_search(read_fixture("uniprot_met_sltm.json"))
  sltm <- parsed$entries[[2]]
  expect_equal(sltm$accession, "Q9NWH9")
  expect_equal(sltm$display_symbol, "SLTM")
  expect_equal(sltm$gene_synonyms, "MET")
  expect_equal(parsed$entries[[1]]$gene_synonyms, character())
})
