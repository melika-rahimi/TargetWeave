research_role_choices <- function() {
  c(
    "Researcher",
    "PhD student",
    "Master's student",
    "Undergraduate student",
    "Bioinformatician",
    "Clinician / translational researcher",
    "Industry scientist",
    "Other"
  )
}

research_field_choices <- function() {
  c(
    "Cancer biology",
    "Molecular biology",
    "Genetics / genomics",
    "Bioinformatics",
    "Drug discovery",
    "Immunology",
    "Cell biology",
    "Other"
  )
}

blank_to_null <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  if (length(value) == 1 && is.atomic(value) && is.na(value)) {
    return(NULL)
  }
  text <- trimws(as.character(value))
  if (!nzchar(text)) {
    NULL
  } else {
    text
  }
}

null_to_na <- function(value) {
  if (is.null(value)) {
    NA_character_
  } else {
    value
  }
}

user_needs_onboarding <- function(user) {
  if (is.null(user)) {
    return(FALSE)
  }
  completed <- user$onboarding_completed_at
  is.null(completed) || (length(completed) == 1 && is.na(completed))
}

user_needs_workspace_tour <- function(user) {
  if (is.null(user)) {
    return(FALSE)
  }
  seen <- user$workspace_tour_completed_at
  is.null(seen) || (length(seen) == 1 && is.na(seen))
}

user_needs_project_setup_tour <- function(user) {
  if (is.null(user)) {
    return(FALSE)
  }
  seen <- user$project_setup_tour_seen_at
  is.null(seen) || (length(seen) == 1 && is.na(seen))
}

load_user_by_id <- function(db_pool, user_id) {
  row <- DBI::dbGetQuery(
    db_pool,
    "
    SELECT
      u.id::text AS id,
      u.email,
      u.onboarding_completed_at,
      u.workspace_tour_completed_at,
      u.project_setup_tour_seen_at,
      u.workspace_tips_seen,
      p.display_name,
      p.research_role,
      p.research_field,
      p.institution,
      p.profile_updated_at
    FROM users u
    LEFT JOIN user_profiles p ON p.user_id = u.id
    WHERE u.id = $1::uuid
    ",
    params = list(user_id)
  )

  if (nrow(row) != 1) {
    return(NULL)
  }

  list(
    id = row$id[[1]],
    email = row$email[[1]],
    display_name = blank_to_null(row$display_name[[1]]),
    research_role = blank_to_null(row$research_role[[1]]),
    research_field = blank_to_null(row$research_field[[1]]),
    institution = blank_to_null(row$institution[[1]]),
    onboarding_completed_at = row$onboarding_completed_at[[1]],
    workspace_tour_completed_at = row$workspace_tour_completed_at[[1]],
    project_setup_tour_seen_at = row$project_setup_tour_seen_at[[1]],
    workspace_tips_seen = parse_tips_seen(row$workspace_tips_seen[[1]]),
    profile_updated_at = row$profile_updated_at[[1]]
  )
}

parse_tips_seen <- function(value) {
  if (is.null(value) || (length(value) == 1 && is.na(value))) {
    return(list())
  }
  if (is.list(value) && !is.data.frame(value)) {
    return(value)
  }
  if (is.character(value) && length(value) == 1) {
    parsed <- tryCatch(
      jsonlite::fromJSON(value, simplifyVector = FALSE),
      error = function(e) list()
    )
    if (is.list(parsed)) {
      return(parsed)
    }
  }
  list()
}

complete_onboarding <- function(db_pool, user_id) {
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET onboarding_completed_at = COALESCE(onboarding_completed_at, NOW())
    WHERE id = $1::uuid
    ",
    params = list(user_id)
  )
  load_user_by_id(db_pool, user_id)
}

mark_workspace_tour_completed <- function(db_pool, user_id) {
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET workspace_tour_completed_at = COALESCE(workspace_tour_completed_at, NOW())
    WHERE id = $1::uuid
    ",
    params = list(user_id)
  )
  load_user_by_id(db_pool, user_id)
}

clear_workspace_tour <- function(db_pool, user_id) {
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET workspace_tour_completed_at = NULL
    WHERE id = $1::uuid
    ",
    params = list(user_id)
  )
  load_user_by_id(db_pool, user_id)
}

mark_project_setup_tour_seen <- function(db_pool, user_id) {
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET project_setup_tour_seen_at = COALESCE(project_setup_tour_seen_at, NOW())
    WHERE id = $1::uuid
    ",
    params = list(user_id)
  )
  load_user_by_id(db_pool, user_id)
}

clear_project_setup_tour <- function(db_pool, user_id) {
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET project_setup_tour_seen_at = NULL
    WHERE id = $1::uuid
    ",
    params = list(user_id)
  )
  load_user_by_id(db_pool, user_id)
}

mark_workspace_tip_seen <- function(db_pool, user_id, tip) {
  if (!nzchar(as.character(tip %||% ""))) {
    return(load_user_by_id(db_pool, user_id))
  }
  DBI::dbExecute(
    db_pool,
    "
    UPDATE users
    SET workspace_tips_seen = COALESCE(workspace_tips_seen, '{}'::jsonb) ||
      jsonb_build_object($2::text, to_jsonb(NOW()::text))
    WHERE id = $1::uuid
    ",
    params = list(user_id, as.character(tip))
  )
  load_user_by_id(db_pool, user_id)
}

save_user_profile <- function(
  db_pool,
  user_id,
  display_name = NULL,
  research_role = NULL,
  research_field = NULL,
  institution = NULL,
  complete_onboarding_flag = TRUE
) {
  display_name <- blank_to_null(display_name)
  research_role <- blank_to_null(research_role)
  research_field <- blank_to_null(research_field)
  institution <- blank_to_null(institution)
  for (pair in list(
    list(display_name, INPUT_LIMITS$display_name, "Display name"),
    list(institution, INPUT_LIMITS$institution, "Institution")
  )) {
    if (!is.null(pair[[1]])) {
      checked <- enforce_length(pair[[1]], pair[[2]], pair[[3]])
      if (!isTRUE(checked$ok)) {
        return(checked)
      }
    }
  }

  DBI::dbExecute(
    db_pool,
    "
    INSERT INTO user_profiles (
      user_id,
      display_name,
      research_role,
      research_field,
      institution,
      profile_updated_at
    )
    VALUES ($1::uuid, $2, $3, $4, $5, NOW())
    ON CONFLICT (user_id) DO UPDATE
    SET
      display_name = EXCLUDED.display_name,
      research_role = EXCLUDED.research_role,
      research_field = EXCLUDED.research_field,
      institution = EXCLUDED.institution,
      profile_updated_at = NOW()
    ",
    params = list(
      user_id,
      null_to_na(display_name),
      null_to_na(research_role),
      null_to_na(research_field),
      null_to_na(institution)
    )
  )

  if (isTRUE(complete_onboarding_flag)) {
    complete_onboarding(db_pool, user_id)
  } else {
    load_user_by_id(db_pool, user_id)
  }
}

account_display_name <- function(user) {
  if (is.null(user)) {
    return(NULL)
  }
  blank_to_null(user$display_name)
}
