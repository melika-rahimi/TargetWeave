# Authorization helpers.
#
# Authentication answers: who is this user?
# Authorization answers: may this user access this project?
#
# Every project-scoped query includes the owner predicate
# (project_id AND user_id). Hiding UI is not security.

get_owned_project <- function(db_pool, project_id, user_id) {
  result <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      id::text AS id,
      user_id::text AS user_id,
      title,
      research_question,
      organism,
      disease_label,
      disease_ontology_id,
      disease_name,
      disease_resolution_status,
      disease_resolution_payload,
      disease_confirmed_at,
      status,
      created_at,
      updated_at,
      target_set_revision
    FROM projects
    WHERE id = $1::uuid
      AND user_id = $2::uuid
    ",
    params = list(project_id, user_id)
  )

  if (nrow(result) != 1) {
    return(NULL)
  }

  result[1, , drop = FALSE]
}
