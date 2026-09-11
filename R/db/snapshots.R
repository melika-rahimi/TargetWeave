SNAPSHOT_SCHEMA_VERSION <- 1L
SNAPSHOT_MAX_BYTES <- 4L * 1024L * 1024L

snapshot_json <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    POSIXt = "ISO8601"
  ))
}

decode_snapshot_json <- function(text) {
  if (is.null(text) || !nzchar(as.character(text))) {
    return(NULL)
  }
  jsonlite::fromJSON(as.character(text), simplifyVector = FALSE)
}

snapshot_from_row <- function(row) {
  if (is.null(row) || nrow(row) != 1L) {
    return(NULL)
  }
  list(
    id = as.character(row$id[[1]]),
    project_id = as.character(row$project_id[[1]]),
    user_id = as.character(row$user_id[[1]]),
    name = as.character(row$name[[1]]),
    description = blank_to_null(row$description[[1]]),
    created_at = row$created_at[[1]],
    schema_version = as.integer(row$schema_version[[1]]),
    project_context = decode_snapshot_json(row$project_context[[1]]),
    target_identity = decode_snapshot_json(row$target_identity[[1]]),
    overview = decode_snapshot_json(row$overview[[1]]),
    disease_evidence = decode_snapshot_json(row$disease_evidence[[1]]),
    comparison = decode_snapshot_json(row$comparison[[1]]),
    pathways = decode_snapshot_json(row$pathways[[1]]),
    literature = decode_snapshot_json(row$literature[[1]]),
    structures = decode_snapshot_json(row$structures[[1]]),
    source_manifest = decode_snapshot_json(row$source_manifest[[1]]),
    capture_summary = decode_snapshot_json(row$capture_summary[[1]])
  )
}

list_project_snapshots <- function(db_pool, project_id, user_id) {
  DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      s.id::text AS id,
      s.project_id::text AS project_id,
      s.user_id::text AS user_id,
      s.name,
      s.description,
      s.created_at,
      s.schema_version,
      s.capture_summary::text AS capture_summary
    FROM evidence_snapshots s
    INNER JOIN projects p
      ON p.id = s.project_id
     AND p.user_id = $2::uuid
    WHERE s.project_id = $1::uuid
    ORDER BY s.created_at DESC
    ",
    params = list(project_id, user_id)
  )
}

get_owned_snapshot <- function(db_pool, snapshot_id, user_id) {
  result <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      s.id::text AS id,
      s.project_id::text AS project_id,
      s.user_id::text AS user_id,
      s.name,
      s.description,
      s.created_at,
      s.schema_version,
      s.project_context::text AS project_context,
      s.target_identity::text AS target_identity,
      s.overview::text AS overview,
      s.disease_evidence::text AS disease_evidence,
      s.comparison::text AS comparison,
      s.pathways::text AS pathways,
      s.literature::text AS literature,
      s.structures::text AS structures,
      s.source_manifest::text AS source_manifest,
      s.capture_summary::text AS capture_summary
    FROM evidence_snapshots s
    INNER JOIN projects p
      ON p.id = s.project_id
     AND p.user_id = $2::uuid
    WHERE s.id = $1::uuid
    ",
    params = list(snapshot_id, user_id)
  )
  if (nrow(result) != 1) {
    return(NULL)
  }
  snapshot_from_row(result)
}

insert_evidence_snapshot_row <- function(db_pool, payload) {
  owner <- get_owned_project(db_pool, payload$project_id, payload$user_id)
  if (is.null(owner)) {
    return(list(ok = FALSE, error = "Project was not found or you do not have access to it."))
  }
  name_len <- enforce_length(payload$name %||% "", INPUT_LIMITS$snapshot_name, "Snapshot name")
  if (!isTRUE(name_len$ok)) {
    return(list(ok = FALSE, error = name_len$message))
  }
  encoded <- list(
    project_context = snapshot_json(payload$project_context),
    target_identity = snapshot_json(payload$target_identity),
    overview = snapshot_json(payload$overview),
    disease_evidence = snapshot_json(payload$disease_evidence),
    comparison = snapshot_json(payload$comparison),
    pathways = snapshot_json(payload$pathways),
    literature = snapshot_json(payload$literature),
    structures = snapshot_json(payload$structures),
    source_manifest = snapshot_json(payload$source_manifest),
    capture_summary = snapshot_json(payload$capture_summary)
  )
  total_bytes <- sum(nchar(unlist(encoded), type = "bytes"))
  if (total_bytes > SNAPSHOT_MAX_BYTES) {
    return(list(ok = FALSE, error = "Snapshot is too large to store. Capture fewer retrieved sections."))
  }
  snapshot_id <- uuid::UUIDgenerate()
  pool::poolWithTransaction(db_pool, function(conn) {
    DBI::dbExecute(
      conn,
      "
      INSERT INTO evidence_snapshots (
        id, project_id, user_id, name, description, schema_version,
        project_context, target_identity, overview, disease_evidence,
        comparison, pathways, literature, structures, source_manifest, capture_summary
      )
      VALUES (
        $1::uuid, $2::uuid, $3::uuid, $4, $5, $6,
        $7::jsonb, $8::jsonb, $9::jsonb, $10::jsonb,
        $11::jsonb, $12::jsonb, $13::jsonb, $14::jsonb, $15::jsonb, $16::jsonb
      )
      ",
      params = list(
        snapshot_id,
        payload$project_id,
        payload$user_id,
        payload$name,
        null_to_na(payload$description),
        as.integer(payload$schema_version),
        encoded$project_context,
        encoded$target_identity,
        encoded$overview,
        encoded$disease_evidence,
        encoded$comparison,
        encoded$pathways,
        encoded$literature,
        encoded$structures,
        encoded$source_manifest,
        encoded$capture_summary
      )
    )
  })
  list(ok = TRUE, snapshot_id = snapshot_id)
}

rename_evidence_snapshot <- function(db_pool, snapshot_id, user_id, name) {
  label <- trimws(as.character(name %||% ""))
  if (!nzchar(label)) {
    return(list(ok = FALSE, error = "Provide a snapshot name."))
  }
  name_len <- enforce_length(label, INPUT_LIMITS$snapshot_name, "Snapshot name")
  if (!isTRUE(name_len$ok)) {
    return(list(ok = FALSE, error = name_len$message))
  }
  n <- DBI::dbExecute(
    db_pool,
    "
    UPDATE evidence_snapshots s
    SET name = $3
    FROM projects p
    WHERE s.id = $1::uuid
      AND s.project_id = p.id
      AND p.user_id = $2::uuid
    ",
    params = list(snapshot_id, user_id, label)
  )
  list(ok = isTRUE(n == 1L))
}

delete_evidence_snapshot <- function(db_pool, snapshot_id, user_id) {
  n <- DBI::dbExecute(
    db_pool,
    "
    DELETE FROM evidence_snapshots s
    USING projects p
    WHERE s.id = $1::uuid
      AND s.project_id = p.id
      AND p.user_id = $2::uuid
    ",
    params = list(snapshot_id, user_id)
  )
  isTRUE(n == 1L)
}
