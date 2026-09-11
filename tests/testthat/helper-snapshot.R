m9_overview_result <- function(
  target_id,
  symbol = "EGFR",
  uniprot = "P00533",
  ensembl = "ENSG00000146648",
  cache_status = "live",
  retrieved_at = "2026-01-15T10:00:00Z"
) {
  list(
    ok = TRUE,
    status = "ready",
    overview = list(
      project_target_id = target_id,
      identity = list(
        symbol = symbol,
        protein_name = "Epidermal growth factor receptor",
        ensembl_gene_id = ensembl,
        uniprot_accession = uniprot,
        hgnc_id = "HGNC:3236",
        organism = "Homo sapiens",
        input_text = symbol
      ),
      protein = list(
        length_aa = 1210L,
        molecular_weight = 134277L,
        reviewed = TRUE,
        function_full = "raw uniprot dump should not be required",
        function_readable = "Receptor tyrosine kinase.",
        function_preview = "Receptor tyrosine kinase.",
        subcellular_locations = list("Cell membrane")
      ),
      genomic = list(
        chromosome = "7",
        start = 55019017L,
        end = 55211628L,
        strand = 1L,
        biotype = "protein_coding",
        location_label = "7:55019017-55211628"
      ),
      provenance = list(
        uniprot = list(
          source = "UniProt",
          record_id = uniprot,
          retrieved_at = retrieved_at,
          cache_status = cache_status
        ),
        ensembl = list(
          source = "Ensembl",
          record_id = ensembl,
          retrieved_at = "2026-01-15T10:01:00Z",
          cache_status = "cached"
        )
      )
    )
  )
}

m9_comparison_result <- function() {
  list(
    status = "ready",
    comparison = list(
      disease = list(id = "MONDO_0005233", name = "non-small cell lung carcinoma"),
      targets = data.frame(
        project_target_id = "egfr",
        symbol = "EGFR",
        overall_direct_score = 0.8,
        cache_status = "live",
        retrieved_at = "2026-02-01T12:00:00Z",
        stringsAsFactors = FALSE
      ),
      datatype_matrix = data.frame(
        symbol = "EGFR",
        datatype_id = "genetic_association",
        datatype_label = "Genetic associations",
        score = 0.5,
        is_missing = FALSE,
        stringsAsFactors = FALSE
      ),
      provenance = list(
        source = "Open Targets",
        disease_id = "MONDO_0005233",
        data_version = "24.09",
        api_version = "4"
      )
    )
  )
}

m9_pathways_result <- function() {
  list(
    status = "ready",
    pathways = list(
      membership_matrix = data.frame(
        symbol = "EGFR",
        pathway_id = "R-HSA-1",
        pathway_name = "Signaling by EGFR",
        is_member = TRUE,
        stringsAsFactors = FALSE
      ),
      pathways = data.frame(
        pathway_id = "R-HSA-1",
        pathway_name = "Signaling by EGFR",
        matched_target_count = 1L,
        stringsAsFactors = FALSE
      ),
      failures = list(),
      provenance = list(
        source = "Reactome",
        reactome_release = "90",
        membership_scope = "lowest_level",
        retrieved_at = "2026-03-01T09:00:00Z",
        cache_status = "live"
      )
    )
  )
}

m9_literature_result <- function() {
  list(
    status = "ready",
    literature = list(
      disease = list(disease_query = "non-small cell lung cancer[Title/Abstract]"),
      targets = list(list(
        target = list(symbol = "EGFR", ncbi_gene_id = "1956", uniprot_accession = "P00533"),
        corpus = list(total_count = 12L, disease_query = "non-small cell lung cancer[Title/Abstract]"),
        trend = data.frame(year = 2024L, record_count = 3L, is_partial_year = TRUE, status = "ok"),
        recent_records = data.frame(pmid = "1", title = "Example", stringsAsFactors = FALSE),
        provenance = list(
          source = "NCBI PubMed",
          ncbi_gene_id = "1956",
          retrieved_at = "2026-04-01T11:00:00Z",
          cache_status = "cached",
          database_last_update = "2026-04-01"
        )
      )),
      failures = list(),
      provenance = list(source = "NCBI PubMed", retrieved_at = "2026-04-01T11:00:00Z")
    )
  )
}

m9_structures_result <- function(status = "ready") {
  list(
    status = status,
    message = if (!identical(status, "ready")) "Structure retrieval unavailable" else NULL,
    structures = if (identical(status, "ready")) {
      list(
        summary = data.frame(symbol = "EGFR", n_pdb_entries = 2L, stringsAsFactors = FALSE),
        targets = list(list(
          target = list(symbol = "EGFR", uniprot_accession = "P00533"),
          records = list(list(
            pdb_id = "1M17",
            polymer_entity = list(entity_id = "1", entity_identifier = "1M17_1", chains = "A"),
            coverage = list(
              uniprot_length = 1210L,
              covered_ranges = data.frame(begin = 695L, end = 1022L),
              coverage_fraction = 0.27
            ),
            rcsb_url = "https://www.rcsb.org/structure/1M17"
          )),
          provenance = list(
            source = "RCSB Protein Data Bank",
            identifier_used = "P00533",
            query_scope = "experimental",
            retrieved_at = "2026-05-01T08:00:00Z",
            cache_status = "live"
          )
        )),
        provenance = list(query_scope = "experimental")
      )
    } else {
      NULL
    }
  )
}

local_workspace <- function(seed, stale_overview = FALSE, include_literature = TRUE, structures_status = "ready") {
  egfr <- as.character(seed$egfr_id)
  list(
    overviews = stats::setNames(
      list(m9_overview_result(
        egfr,
        cache_status = if (isTRUE(stale_overview)) "stale" else "live",
        retrieved_at = "2026-01-15T10:00:00Z"
      )),
      egfr
    ),
    disease_evidence = stats::setNames(
      list(list(
        status = "live",
        evidence = list(
          target = list(symbol = "EGFR", ensembl_gene_id = "ENSG00000146648"),
          association = list(overall_score_direct = 0.71),
          provenance = list(
            source = "Open Targets",
            target_id = "ENSG00000146648",
            disease_id = "MONDO_0005233",
            retrieved_at = "2026-02-01T12:00:00Z",
            cache_status = "live",
            data_version = "24.09",
            api_version = "4"
          )
        )
      )),
      egfr
    ),
    comparison = m9_comparison_result(),
    pathways = m9_pathways_result(),
    literature = if (isTRUE(include_literature)) m9_literature_result() else NULL,
    structures = m9_structures_result(structures_status)
  )
}

seed_snapshot_project <- function(db_pool) {
  suffix <- gsub("-", "", uuid::UUIDgenerate())
  owner <- register_user(db_pool, sprintf("snap-%s@example.com", suffix), "correct-horse-battery")
  other <- register_user(db_pool, sprintf("snap-b-%s@example.com", suffix), "correct-horse-battery")
  created <- create_project(
    db_pool,
    owner$user$id,
    "Potential therapeutic targets in NSCLC",
    "Which candidates deserve deeper investigation?",
    "Non-small-cell lung cancer",
    c("EGFR", "KRAS")
  )
  rows <- list_project_targets(db_pool, created$project_id, owner$user$id)
  egfr <- rows$id[rows$input_text == "EGFR"][[1]]
  DBI::dbExecute(
    db_pool,
    "
    UPDATE project_targets
    SET resolution_status = 'confirmed',
        display_symbol = 'EGFR',
        ensembl_gene_id = 'ENSG00000146648',
        uniprot_accession = 'P00533'
    WHERE id = $1::uuid
    ",
    params = list(egfr)
  )
  DBI::dbExecute(
    db_pool,
    "
    UPDATE projects
    SET disease_resolution_status = 'confirmed',
        disease_ontology_id = 'MONDO_0005233',
        disease_name = 'non-small cell lung carcinoma'
    WHERE id = $1::uuid
    ",
    params = list(created$project_id)
  )
  list(
    owner = owner$user,
    other = other$user,
    project_id = created$project_id,
    egfr_id = egfr,
    kras_id = rows$id[rows$input_text == "KRAS"][[1]]
  )
}
