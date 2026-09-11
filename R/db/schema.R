ensure_schema <- function(db_pool) {
  statements <- c(
    "
    CREATE TABLE IF NOT EXISTS users (
      id UUID PRIMARY KEY,
      email TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
    )
    ",
    "
    CREATE TABLE IF NOT EXISTS user_profiles (
      user_id UUID PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE,
      display_name TEXT NULL,
      research_role TEXT NULL,
      research_field TEXT NULL,
      institution TEXT NULL,
      profile_updated_at TIMESTAMPTZ NULL
    )
    ",
    "
    CREATE TABLE IF NOT EXISTS projects (
      id UUID PRIMARY KEY,
      user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      title TEXT NOT NULL,
      research_question TEXT NOT NULL,
      organism TEXT NOT NULL DEFAULT 'Homo sapiens',
      disease_label TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'active',
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      CONSTRAINT projects_status_check
        CHECK (status IN ('active', 'archived'))
    )
    ",
    "
    CREATE TABLE IF NOT EXISTS project_targets (
      id UUID PRIMARY KEY,
      project_id UUID NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
      input_text TEXT NOT NULL,
      resolution_status TEXT NOT NULL DEFAULT 'unresolved',
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      CONSTRAINT target_resolution_status_check
        CHECK (resolution_status IN ('unresolved', 'ambiguous', 'failed', 'confirmed'))
    )
    ",
    "
    ALTER TABLE project_targets
      ADD COLUMN IF NOT EXISTS display_symbol TEXT NULL
    ",
    "
    ALTER TABLE project_targets
      ADD COLUMN IF NOT EXISTS ensembl_gene_id TEXT NULL
    ",
    "
    ALTER TABLE project_targets
      ADD COLUMN IF NOT EXISTS uniprot_accession TEXT NULL
    ",
    "
    ALTER TABLE project_targets
      ADD COLUMN IF NOT EXISTS hgnc_id TEXT NULL
    ",
    "
    ALTER TABLE project_targets
      ADD COLUMN IF NOT EXISTS resolution_payload JSONB NULL
    ",
    "
    ALTER TABLE project_targets
      ADD COLUMN IF NOT EXISTS confirmed_at TIMESTAMPTZ NULL
    ",
    "
    ALTER TABLE projects
      ADD COLUMN IF NOT EXISTS disease_ontology_id TEXT NULL
    ",
    "
    ALTER TABLE projects
      ADD COLUMN IF NOT EXISTS disease_name TEXT NULL
    ",
    "
    ALTER TABLE projects
      ADD COLUMN IF NOT EXISTS disease_resolution_status TEXT NOT NULL DEFAULT 'unresolved'
    ",
    "
    ALTER TABLE projects
      ADD COLUMN IF NOT EXISTS disease_resolution_payload JSONB NULL
    ",
    "
    ALTER TABLE projects
      ADD COLUMN IF NOT EXISTS disease_confirmed_at TIMESTAMPTZ NULL
    ",
    "
    CREATE TABLE IF NOT EXISTS api_cache (
      id UUID PRIMARY KEY,
      source TEXT NOT NULL,
      cache_key TEXT NOT NULL,
      response JSONB NOT NULL,
      http_status INTEGER NULL,
      retrieved_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      expires_at TIMESTAMPTZ NOT NULL,
      status TEXT NOT NULL DEFAULT 'ok',
      CONSTRAINT api_cache_source_key UNIQUE (source, cache_key)
    )
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_projects_user_id
      ON projects(user_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_project_targets_project_id
      ON project_targets(project_id)
    "
  )

  for (statement in statements) {
    DBI::dbExecute(db_pool, statement)
  }

  migrate_disease_ontology_id(db_pool)
  ensure_user_onboarding_column(db_pool)
  ensure_workspace_tour_column(db_pool)
  ensure_project_setup_tour_column(db_pool)
  ensure_support_requests_table(db_pool)
  ensure_research_record_tables(db_pool)
  ensure_unique_target_indexes(db_pool)
  run_named_migrations(db_pool)
  invisible(TRUE)
}

column_exists_in_current_schema <- function(db_pool, table_name, column_name) {
  result <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = current_schema()
      AND table_name = $1
      AND column_name = $2
    ",
    params = list(table_name, column_name)
  )
  nrow(result) >= 1
}

# Existing accounts keep working. Only users present when the column is first
# added are treated as already past onboarding.
ensure_user_onboarding_column <- function(db_pool) {
  existed <- column_exists_in_current_schema(
    db_pool,
    "users",
    "onboarding_completed_at"
  )

  DBI::dbExecute(
    db_pool,
    "ALTER TABLE users ADD COLUMN IF NOT EXISTS onboarding_completed_at TIMESTAMPTZ NULL"
  )

  if (!isTRUE(existed)) {
    DBI::dbExecute(
      db_pool,
      "
      UPDATE users
      SET onboarding_completed_at = created_at
      WHERE onboarding_completed_at IS NULL
      "
    )
  }

  invisible(TRUE)
}

# Canonical tour state is workspace_tour_completed_at + workspace_tips_seen.
# product_intro_seen_at is legacy-only: copied when present, never added, never dropped at startup.
ensure_workspace_tour_column <- function(db_pool) {
  existed <- column_exists_in_current_schema(
    db_pool,
    "users",
    "workspace_tour_completed_at"
  )

  DBI::dbExecute(
    db_pool,
    "ALTER TABLE users ADD COLUMN IF NOT EXISTS workspace_tour_completed_at TIMESTAMPTZ NULL"
  )
  DBI::dbExecute(
    db_pool,
    "ALTER TABLE users ADD COLUMN IF NOT EXISTS workspace_tips_seen JSONB NOT NULL DEFAULT '{}'::jsonb"
  )

  if (column_exists_in_current_schema(db_pool, "users", "product_intro_seen_at")) {
    DBI::dbExecute(
      db_pool,
      "
      UPDATE users
      SET workspace_tour_completed_at = product_intro_seen_at
      WHERE workspace_tour_completed_at IS NULL
        AND product_intro_seen_at IS NOT NULL
      "
    )
  }

  if (!isTRUE(existed)) {
    DBI::dbExecute(
      db_pool,
      "
      UPDATE users
      SET workspace_tour_completed_at = COALESCE(
        workspace_tour_completed_at,
        onboarding_completed_at,
        created_at
      )
      WHERE workspace_tour_completed_at IS NULL
        AND onboarding_completed_at IS NOT NULL
      "
    )
  }

  invisible(TRUE)
}

ensure_project_setup_tour_column <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "ALTER TABLE users ADD COLUMN IF NOT EXISTS project_setup_tour_seen_at TIMESTAMPTZ NULL"
  )
  invisible(TRUE)
}

# Research notes and evidence snapshots are project-owned records, not api_cache.
# Live target edits do not rewrite snapshot JSONB. Project deletion (if added)
# cascades snapshots/notes with the project; archive leaves them intact.
ensure_research_record_tables <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "
    CREATE TABLE IF NOT EXISTS evidence_snapshots (
      id UUID PRIMARY KEY,
      project_id UUID NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
      user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      name TEXT NOT NULL,
      description TEXT NULL,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      schema_version INTEGER NOT NULL DEFAULT 1,
      project_context JSONB NOT NULL,
      target_identity JSONB NOT NULL,
      overview JSONB NOT NULL,
      disease_evidence JSONB NOT NULL,
      comparison JSONB NOT NULL,
      pathways JSONB NOT NULL,
      literature JSONB NOT NULL,
      structures JSONB NOT NULL,
      source_manifest JSONB NOT NULL,
      capture_summary JSONB NOT NULL
    )
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_evidence_snapshots_project
      ON evidence_snapshots(project_id, created_at DESC)
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE TABLE IF NOT EXISTS research_notes (
      id UUID PRIMARY KEY,
      project_id UUID NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
      user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      project_target_id UUID NULL REFERENCES project_targets(id) ON DELETE CASCADE,
      snapshot_id UUID NULL REFERENCES evidence_snapshots(id) ON DELETE CASCADE,
      scope TEXT NOT NULL,
      title TEXT NULL,
      body TEXT NOT NULL,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      CONSTRAINT research_notes_scope_check
        CHECK (scope IN ('project', 'target', 'snapshot')),
      CONSTRAINT research_notes_scope_refs_check
        CHECK (
          (scope = 'project' AND project_target_id IS NULL AND snapshot_id IS NULL) OR
          (scope = 'target' AND project_target_id IS NOT NULL AND snapshot_id IS NULL) OR
          (scope = 'snapshot' AND snapshot_id IS NOT NULL AND project_target_id IS NULL)
        )
    )
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_research_notes_project
      ON research_notes(project_id, updated_at DESC)
    "
  )
  invisible(TRUE)
}

ensure_support_requests_table <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "
    CREATE TABLE IF NOT EXISTS support_requests (
      id UUID PRIMARY KEY,
      user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      category TEXT NOT NULL,
      subject TEXT NOT NULL,
      message TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'submitted',
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      CONSTRAINT support_requests_status_check
        CHECK (status IN ('submitted'))
    )
    "
  )
  DBI::dbExecute(
    db_pool,
    "CREATE INDEX IF NOT EXISTS idx_support_requests_user_id ON support_requests(user_id)"
  )
  invisible(TRUE)
}

# Copy confirmed Open Targets disease IDs (MONDO, EFO, HP, ...) off the
# misleading disease_efo_id column, then drop it.
migrate_disease_ontology_id <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "ALTER TABLE projects ADD COLUMN IF NOT EXISTS disease_ontology_id TEXT NULL"
  )

  legacy <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = current_schema()
      AND table_name = 'projects'
      AND column_name = 'disease_efo_id'
    "
  )

  if (nrow(legacy) >= 1) {
    DBI::dbExecute(
      db_pool,
      "
      UPDATE projects
      SET disease_ontology_id = disease_efo_id
      WHERE disease_ontology_id IS NULL
        AND disease_efo_id IS NOT NULL
      "
    )
    DBI::dbExecute(
      db_pool,
      "ALTER TABLE projects DROP COLUMN IF EXISTS disease_efo_id"
    )
  }

  invisible(TRUE)
}

unique_target_index_statements <- function() {
  list(
    idx_project_targets_confirmed_ensembl = "
      CREATE UNIQUE INDEX IF NOT EXISTS idx_project_targets_confirmed_ensembl
        ON project_targets(project_id, ensembl_gene_id)
        WHERE resolution_status = 'confirmed'
          AND ensembl_gene_id IS NOT NULL
    ",
    idx_project_targets_input_normalized = "
      CREATE UNIQUE INDEX IF NOT EXISTS idx_project_targets_input_normalized
        ON project_targets (project_id, (lower(btrim(input_text))))
    ",
    idx_project_targets_confirmed_ensembl_canonical = "
      CREATE UNIQUE INDEX IF NOT EXISTS idx_project_targets_confirmed_ensembl_canonical
        ON project_targets (
          project_id,
          (regexp_replace(upper(ensembl_gene_id), '\\.[0-9]+$', ''))
        )
        WHERE resolution_status = 'confirmed'
          AND ensembl_gene_id IS NOT NULL
    "
  )
}

ensure_unique_target_indexes <- function(db_pool) {
  results <- list()
  statements <- unique_target_index_statements()

  for (name in names(statements)) {
    con <- pool::poolCheckout(db_pool)
    created <- tryCatch(
      {
        DBI::dbExecute(con, statements[[name]])
        TRUE
      },
      error = function(e) {
        suppressWarnings(try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE))
        e
      }
    )
    pool::poolReturn(con)

    if (isTRUE(created)) {
      results[[name]] <- list(ok = TRUE, message = NULL)
    } else {
      results[[name]] <- list(
        ok = FALSE,
        message = conditionMessage(created)
      )
      message(
        "Could not create ", name, " because existing rows collide. ",
        "No project data was deleted. Inspect with a dry run, then apply:\n",
        "  Rscript scripts/repair_duplicate_targets.R\n",
        "  Rscript scripts/repair_duplicate_targets.R --apply"
      )
    }
  }

  results
}

list_duplicate_project_targets <- function(db_pool) {
  input_dups <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      ranked.id::text AS id,
      ranked.project_id::text AS project_id,
      ranked.input_text,
      ranked.resolution_status,
      ranked.ensembl_gene_id,
      ranked.created_at,
      ranked.confirmed_at,
      ranked.keeper_id::text AS keeper_id,
      'duplicate_normalized_input' AS reason
    FROM (
      SELECT
        id,
        project_id,
        input_text,
        resolution_status,
        ensembl_gene_id,
        created_at,
        confirmed_at,
        first_value(id) OVER (
          PARTITION BY project_id, lower(btrim(input_text))
          ORDER BY
            CASE WHEN resolution_status = 'confirmed' THEN 0 ELSE 1 END,
            created_at ASC,
            id ASC
        ) AS keeper_id,
        row_number() OVER (
          PARTITION BY project_id, lower(btrim(input_text))
          ORDER BY
            CASE WHEN resolution_status = 'confirmed' THEN 0 ELSE 1 END,
            created_at ASC,
            id ASC
        ) AS rn
      FROM project_targets
    ) ranked
    WHERE ranked.rn > 1
    "
  )

  ensembl_dups <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      ranked.id::text AS id,
      ranked.project_id::text AS project_id,
      ranked.input_text,
      ranked.resolution_status,
      ranked.ensembl_gene_id,
      ranked.created_at,
      ranked.confirmed_at,
      ranked.keeper_id::text AS keeper_id,
      'duplicate_confirmed_ensembl' AS reason
    FROM (
      SELECT
        id,
        project_id,
        input_text,
        resolution_status,
        ensembl_gene_id,
        created_at,
        confirmed_at,
        first_value(id) OVER (
          PARTITION BY
            project_id,
            regexp_replace(upper(ensembl_gene_id), '\\.[0-9]+$', '')
          ORDER BY confirmed_at ASC NULLS LAST, created_at ASC, id ASC
        ) AS keeper_id,
        row_number() OVER (
          PARTITION BY
            project_id,
            regexp_replace(upper(ensembl_gene_id), '\\.[0-9]+$', '')
          ORDER BY confirmed_at ASC NULLS LAST, created_at ASC, id ASC
        ) AS rn
      FROM project_targets
      WHERE resolution_status = 'confirmed'
        AND ensembl_gene_id IS NOT NULL
    ) ranked
    WHERE ranked.rn > 1
    "
  )

  rows <- rbind(input_dups, ensembl_dups)
  if (nrow(rows) == 0) {
    return(rows)
  }

  rows[!duplicated(rows$id), , drop = FALSE]
}

repair_duplicate_project_targets <- function(db_pool, apply = FALSE) {
  to_remove <- list_duplicate_project_targets(db_pool)

  result <- list(
    apply = isTRUE(apply),
    removed = 0L,
    rows = to_remove,
    index_results = NULL
  )

  if (nrow(to_remove) == 0) {
    result$index_results <- ensure_unique_target_indexes(db_pool)
    return(result)
  }

  if (!isTRUE(apply)) {
    return(result)
  }

  ids <- unique(to_remove$id)
  placeholders <- paste(sprintf("$%d::uuid", seq_along(ids)), collapse = ", ")
  deleted <- DBI::dbExecute(
    db_pool,
    paste0("DELETE FROM project_targets WHERE id IN (", placeholders, ")"),
    params = as.list(ids)
  )

  result$removed <- as.integer(deleted)
  result$index_results <- ensure_unique_target_indexes(db_pool)
  result
}
