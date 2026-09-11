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

record_migration <- function(db_pool, version, name) {
  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO schema_migrations (version, name)
    VALUES ($1, $2)
    ON CONFLICT (name) DO NOTHING
    ",
    params = list(as.integer(version), name)
  )
  invisible(TRUE)
}

apply_named_migration <- function(db_pool, version, name, fn) {
  if (isTRUE(migration_applied(db_pool, name))) {
    return(invisible(TRUE))
  }
  fn(db_pool)
  record_migration(db_pool, version, name)
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

run_named_migrations <- function(db_pool) {
  ensure_schema_migrations_table(db_pool)
  apply_named_migration(db_pool, 1L, "m11_indexes", migration_m11_indexes)
  apply_named_migration(db_pool, 2L, "m11_disease_status_check", migration_m11_checks)
  invisible(TRUE)
}
