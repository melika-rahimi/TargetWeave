# Named, idempotent migrations. Startup never DELETEs user scientific rows.

ensure_schema_migrations_table <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "
    CREATE TABLE IF NOT EXISTS schema_migrations (
      version INTEGER PRIMARY KEY,
      name TEXT NOT NULL UNIQUE,
      applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
    )
    "
  )
  invisible(TRUE)
}

migration_applied <- function(db_pool, name) {
  rows <- DBI::dbGetQuery(
    db_pool,
    "SELECT 1 FROM schema_migrations WHERE name = $1",
    params = list(name)
  )
  nrow(rows) >= 1
}

migration_name_for_version <- function(db_pool, version) {
  rows <- DBI::dbGetQuery(
    db_pool,
    "SELECT name FROM schema_migrations WHERE version = $1",
    params = list(as.integer(version))
  )
  if (nrow(rows) != 1) {
    return(NULL)
  }
  as.character(rows$name[[1]])
}

record_migration <- function(db_pool, version, name) {
  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO schema_migrations (version, name)
    VALUES ($1, $2)
    ",
    params = list(as.integer(version), name)
  )
  invisible(TRUE)
}

apply_named_migration <- function(db_pool, version, name, fn) {
  version <- as.integer(version[[1]])
  name <- as.character(name[[1]])
  if (isTRUE(migration_applied(db_pool, name))) {
    return(invisible(TRUE))
  }
  occupant <- migration_name_for_version(db_pool, version)
  if (!is.null(occupant)) {
    stop(
      sprintf(
        paste(
          "schema_migrations version %s is already applied as '%s'.",
          "Refusing to apply '%s' or rewrite migration history."
        ),
        version,
        occupant,
        name
      ),
      call. = FALSE
    )
  }
  fn(db_pool)
  if (!isTRUE(migration_applied(db_pool, name))) {
    record_migration(db_pool, version, name)
  }
  tw_log("migration_applied", version = version, name = name)
  invisible(TRUE)
}

migration_m11_indexes <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_projects_user_status
      ON projects(user_id, status, updated_at DESC)
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_research_notes_snapshot
      ON research_notes(snapshot_id)
      WHERE snapshot_id IS NOT NULL
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_research_notes_target
      ON research_notes(project_target_id)
      WHERE project_target_id IS NOT NULL
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_api_cache_expires
      ON api_cache(expires_at)
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_project_targets_confirmed_uniprot
      ON project_targets(project_id, uniprot_accession)
      WHERE resolution_status = 'confirmed'
        AND uniprot_accession IS NOT NULL
    "
  )
  invisible(TRUE)
}

migration_m11_checks <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "
    DO $$
    BEGIN
      IF NOT EXISTS (
        SELECT 1 FROM pg_constraint WHERE conname = 'projects_disease_status_check'
      ) THEN
        ALTER TABLE projects
          ADD CONSTRAINT projects_disease_status_check
          CHECK (
            disease_resolution_status IN ('unresolved', 'ambiguous', 'failed', 'confirmed')
          );
      END IF;
    END
    $$
    "
  )
  invisible(TRUE)
}

migration_m12_target_lifecycle <- function(db_pool) {
  DBI::dbExecute(
    db_pool,
    "
    CREATE TABLE IF NOT EXISTS project_events (
      id UUID PRIMARY KEY,
      project_id UUID NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
      event_type TEXT NOT NULL,
      actor_user_id UUID NOT NULL REFERENCES users(id),
      project_target_id UUID NULL REFERENCES project_targets(id) ON DELETE SET NULL,
      metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      CONSTRAINT project_events_type_check
        CHECK (
          event_type IN (
            'PROJECT_CREATED',
            'TARGET_ADDED',
            'TARGET_REMOVED',
            'TARGET_IDENTITY_CONFIRMED',
            'DISEASE_IDENTITY_CONFIRMED',
            'SNAPSHOT_CREATED'
          )
        )
    )
    "
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_project_events_project
      ON project_events(project_id, created_at DESC)
    "
  )
  DBI::dbExecute(
    db_pool,
    "DROP INDEX IF EXISTS idx_project_targets_input_normalized"
  )
  DBI::dbExecute(
    db_pool,
    "DROP INDEX IF EXISTS idx_project_targets_confirmed_ensembl"
  )
  DBI::dbExecute(
    db_pool,
    "DROP INDEX IF EXISTS idx_project_targets_confirmed_ensembl_canonical"
  )
  statements <- unique_target_index_statements()
  for (sql in statements) {
    DBI::dbExecute(db_pool, sql)
  }
  DBI::dbExecute(
    db_pool,
    "DROP INDEX IF EXISTS idx_project_targets_confirmed_uniprot"
  )
  DBI::dbExecute(
    db_pool,
    "
    CREATE INDEX IF NOT EXISTS idx_project_targets_confirmed_uniprot
      ON project_targets(project_id, uniprot_accession)
      WHERE resolution_status = 'confirmed'
        AND uniprot_accession IS NOT NULL
        AND removed_at IS NULL
    "
  )
  backfill_project_events_from_existing_rows(db_pool)
  invisible(TRUE)
}

backfill_project_events_from_existing_rows <- function(db_pool) {
  projects <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT p.id::text AS id, p.user_id::text AS user_id, p.created_at
    FROM projects p
    WHERE NOT EXISTS (
      SELECT 1 FROM project_events e WHERE e.project_id = p.id
    )
    "
  )
  if (nrow(projects) == 0) {
    return(invisible(TRUE))
  }
  for (i in seq_len(nrow(projects))) {
    pid <- projects$id[[i]]
    uid <- projects$user_id[[i]]
    insert_project_event(
      db_pool,
      pid,
      "PROJECT_CREATED",
      uid,
      created_at = projects$created_at[[i]]
    )
    targets <- DBI::dbGetQuery(
      db_pool,
      "
      SELECT id::text AS id, input_text, display_symbol, ensembl_gene_id,
             uniprot_accession, resolution_status, created_at, confirmed_at, removed_at
      FROM project_targets
      WHERE project_id = $1::uuid
      ORDER BY created_at ASC
      ",
      params = list(pid)
    )
    for (j in seq_len(nrow(targets))) {
      row <- targets[j, ]
      label <- if (has_display_text(row$display_symbol)) row$display_symbol else row$input_text
      insert_project_event(
        db_pool,
        pid,
        "TARGET_ADDED",
        uid,
        project_target_id = row$id,
        metadata = list(label = label, input_text = row$input_text),
        created_at = row$created_at
      )
      if (identical(as.character(row$resolution_status), "confirmed") &&
          !is.na(row$confirmed_at)) {
        insert_project_event(
          db_pool,
          pid,
          "TARGET_IDENTITY_CONFIRMED",
          uid,
          project_target_id = row$id,
          metadata = list(
            label = label,
            ensembl_gene_id = row$ensembl_gene_id,
            uniprot_accession = row$uniprot_accession
          ),
          created_at = row$confirmed_at
        )
      }
      if (!is.na(row$removed_at)) {
        insert_project_event(
          db_pool,
          pid,
          "TARGET_REMOVED",
          uid,
          project_target_id = row$id,
          metadata = list(label = label),
          created_at = row$removed_at
        )
      }
    }
    disease <- DBI::dbGetQuery(
      db_pool,
      "
      SELECT disease_resolution_status, disease_name, disease_ontology_id, disease_confirmed_at
      FROM projects
      WHERE id = $1::uuid
      ",
      params = list(pid)
    )
    if (nrow(disease) == 1 &&
        identical(as.character(disease$disease_resolution_status[[1]]), "confirmed") &&
        !is.na(disease$disease_confirmed_at[[1]])) {
      insert_project_event(
        db_pool,
        pid,
        "DISEASE_IDENTITY_CONFIRMED",
        uid,
        metadata = list(
          disease_name = disease$disease_name[[1]],
          disease_ontology_id = disease$disease_ontology_id[[1]]
        ),
        created_at = disease$disease_confirmed_at[[1]]
      )
    }
    snaps <- DBI::dbGetQuery(
      db_pool,
      "
      SELECT id::text AS id, name, created_at
      FROM evidence_snapshots
      WHERE project_id = $1::uuid
      ORDER BY created_at ASC
      ",
      params = list(pid)
    )
    for (k in seq_len(nrow(snaps))) {
      insert_project_event(
        db_pool,
        pid,
        "SNAPSHOT_CREATED",
        uid,
        metadata = list(name = snaps$name[[k]], snapshot_id = snaps$id[[k]]),
        created_at = snaps$created_at[[k]]
      )
    }
  }
  invisible(TRUE)
}

# Immutable version map. Never reuse or rename a version applied under another name.
#   1  m11_indexes
#   2  m11_disease_status_check
#   3  m12_target_lifecycle
run_named_migrations <- function(db_pool) {
  ensure_schema_migrations_table(db_pool)
  apply_named_migration(db_pool, 1L, "m11_indexes", migration_m11_indexes)
  apply_named_migration(db_pool, 2L, "m11_disease_status_check", migration_m11_checks)
  apply_named_migration(db_pool, 3L, "m12_target_lifecycle", migration_m12_target_lifecycle)
  invisible(TRUE)
}
