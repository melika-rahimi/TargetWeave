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

decode_legacy_snapshot_json <- function(value) {
  decode_snapshot_json(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    POSIXt = "ISO8601"
  ))
}

production_like_export_snapshot <- function(json_na = c("string", "null")) {
  json_na <- match.arg(json_na)
  roundtrip <- if (identical(json_na, "string")) {
    decode_legacy_snapshot_json
  } else {
    function(value) decode_snapshot_json(snapshot_json(value))
  }
  ids <- c("egfr", "kras", "met", "tp53")
  symbols <- c("EGFR", "KRAS", "MET", "TP53")
  accessions <- c("P00533", "P01116", "P08581", "P04637")
  ensembl <- c("ENSG00000146648", "ENSG00000133703", "ENSG00000105976", "ENSG00000141510")
  gene_ids <- c("1956", "3845", "4233", "7157")
  ot_types <- data.frame(
    datatype_id = c(
      "genetic_association", "somatic_mutation", "affected_pathway",
      "literature", "rna_expression", "animal_model", "known_drug"
    ),
    datatype_label = c(
      "Genetic associations", "Somatic mutations", "Affected pathways",
      "Literature", "RNA expression", "Animal models", "Known drugs"
    ),
    stringsAsFactors = FALSE
  )
  pathway_names <- sprintf("Reactome pathway %02d", seq_len(PATHWAY_MATRIX_ROW_CAP))
  targets_df <- data.frame(
    id = ids,
    input_text = symbols,
    display_symbol = symbols,
    resolution_status = "confirmed",
    ensembl_gene_id = ensembl,
    uniprot_accession = accessions,
    hgnc_id = NA_character_,
    stringsAsFactors = FALSE
  )
  project <- data.frame(
    id = "proj",
    title = "Potential therapeutic targets in NSCLC",
    research_question = "Which candidates deserve deeper investigation?",
    organism = "Homo sapiens",
    disease_label = "NSCLC",
    disease_name = "non-small cell lung carcinoma",
    disease_ontology_id = "MONDO_0005233",
    disease_resolution_status = "confirmed",
    stringsAsFactors = FALSE
  )
  comparison_targets <- data.frame(
    project_target_id = ids,
    symbol = symbols,
    overall_direct_score = c(0.81, 0.64, 0.55, 0.42),
    overall_inclusive_score = c(0.83, NA_real_, 0.57, 0.40),
    retrieval_status = "ok",
    cache_status = "live",
    stringsAsFactors = FALSE
  )
  mat <- do.call(rbind, lapply(seq_along(symbols), function(i) {
    data.frame(
      symbol = symbols[[i]],
      datatype_id = ot_types$datatype_id,
      datatype_label = ot_types$datatype_label,
      score = ifelse(seq_len(nrow(ot_types)) %% 3L == 0L, NA_real_, max(0, 0.9 - 0.08 * i)),
      is_missing = seq_len(nrow(ot_types)) %% 3L == 0L,
      stringsAsFactors = FALSE
    )
  }))
  membership <- do.call(rbind, lapply(seq_along(symbols), function(i) {
    data.frame(
      symbol = symbols[[i]],
      pathway_id = sprintf("R-HSA-%s", seq_along(pathway_names)),
      pathway_name = pathway_names,
      is_member = (seq_along(pathway_names) %% 4L) == (i %% 4L),
      stringsAsFactors = FALSE
    )
  }))
  years <- 2015:2024
  literature_targets <- lapply(seq_along(symbols), function(i) {
    rec_n <- LITERATURE_RECENT_N
    list(
      target = list(symbol = symbols[[i]], ncbi_gene_id = gene_ids[[i]], uniprot_accession = accessions[[i]]),
      corpus = list(total_count = 800L + 40L * i, disease_query = "non-small cell lung cancer[Title/Abstract]"),
      trend = data.frame(
        year = years,
        record_count = as.integer(20L + i * 3L + (years - 2015L) * 2L),
        is_partial_year = years == max(years),
        status = "ok",
        stringsAsFactors = FALSE
      ),
      recent_records = data.frame(
        pmid = as.character(10000L * i + seq_len(rec_n)),
        title = paste(symbols[[i]], "literature", seq_len(rec_n)),
        first_author = ifelse(seq_len(rec_n) %% 5L == 0L, NA_character_, paste0("Author", seq_len(rec_n))),
        journal = ifelse(seq_len(rec_n) %% 7L == 0L, NA_character_, "Example Journal"),
        publication_date = ifelse(seq_len(rec_n) %% 6L == 0L, NA_character_, "2024-06-01"),
        doi = ifelse(seq_len(rec_n) %% 4L == 0L, NA_character_, paste0("10.0000/tw.", i, ".", seq_len(rec_n))),
        stringsAsFactors = FALSE
      ),
      provenance = list(
        source = "NCBI PubMed",
        ncbi_gene_id = gene_ids[[i]],
        retrieved_at = "2026-04-01T11:00:00Z",
        cache_status = "cached"
      )
    )
  })
  structure_targets <- lapply(seq_along(symbols), function(i) {
    n_rec <- STRUCTURE_COVERAGE_ROW_CAP
    records <- lapply(seq_len(n_rec), function(j) {
      missing_cov <- j > 12L
      list(
        pdb_id = sprintf("%s%02d", c("1A", "2B", "3C", "4D")[[i]], j),
        polymer_entity = list(
          entity_id = "1",
          entity_identifier = sprintf("%s%02d_1", c("1A", "2B", "3C", "4D")[[i]], j),
          chains = "A"
        ),
        coverage = list(
          uniprot_length = 1000L + 50L * i,
          covered_ranges = if (missing_cov) {
            data.frame(begin = integer(), end = integer())
          } else {
            data.frame(begin = 10L + j, end = 80L + 8L * j)
          },
          coverage_fraction = if (missing_cov) NA_real_ else (0.08 + 0.01 * j)
        ),
        experiment = list(
          method = if (missing_cov) NA_character_ else "X-RAY DIFFRACTION",
          resolution_angstrom = if (missing_cov) NA_real_ else 1.4 + 0.05 * j
        ),
        entry = list(release_date = if (missing_cov) NA_character_ else "2020-01-01"),
        rcsb_url = sprintf("https://www.rcsb.org/structure/%s%02d", c("1A", "2B", "3C", "4D")[[i]], j)
      )
    })
    list(
      target = list(symbol = symbols[[i]], uniprot_accession = accessions[[i]]),
      uniprot_length = 1000L + 50L * i,
      records = records,
      provenance = list(
        source = "RCSB Protein Data Bank",
        identifier_used = accessions[[i]],
        query_scope = "experimental",
        retrieved_at = "2026-05-01T08:00:00Z",
        cache_status = "live"
      )
    )
  })
  workspace <- list(
    overviews = list(),
    disease_evidence = list(),
    comparison = list(
      status = "ready",
      comparison = list(
        disease = list(id = "MONDO_0005233", name = "non-small cell lung carcinoma"),
        targets = comparison_targets,
        datatype_matrix = mat,
        provenance = list(
          source = "Open Targets",
          disease_id = "MONDO_0005233",
          data_version = "24.09",
          api_version = "4"
        )
      )
    ),
    pathways = list(
      status = "ready",
      pathways = list(
        membership_matrix = membership,
        pathways = data.frame(
          pathway_id = sprintf("R-HSA-%s", seq_along(pathway_names)),
          pathway_name = pathway_names,
          matched_target_count = as.integer(1L + (seq_along(pathway_names) %% 4L)),
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
    ),
    literature = list(
      status = "ready",
      literature = list(
        disease = list(disease_query = "non-small cell lung cancer[Title/Abstract]"),
        targets = literature_targets,
        failures = list(),
        provenance = list(source = "NCBI PubMed", retrieved_at = "2026-04-01T11:00:00Z")
      )
    ),
    structures = list(
      status = "ready",
      structures = list(
        summary = data.frame(
          symbol = symbols,
          n_pdb_entries = as.integer(c(47L, 33L, 21L, 18L)),
          stringsAsFactors = FALSE
        ),
        targets = structure_targets,
        provenance = list(query_scope = "experimental")
      )
    )
  )
  payload <- build_snapshot_payload(project, targets_df, workspace)
  for (key in c(
    "project_context", "target_identity", "overview", "disease_evidence",
    "comparison", "pathways", "literature", "structures", "source_manifest", "capture_summary"
  )) {
    payload[[key]] <- roundtrip(payload[[key]])
  }
  snap <- export_hydrate_snapshot(payload)
  snap$id <- "00000000-0000-0000-0000-00000000prod"
  snap$project_id <- "00000000-0000-0000-0000-00000000pr01"
  snap$name <- "Production-shaped NSCLC snapshot"
  snap$created_at <- as.POSIXct("2026-09-11 14:30:00", tz = "UTC")
  snap
}
