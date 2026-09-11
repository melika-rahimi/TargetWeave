INPUT_LIMITS <- list(
  email = 254L,
  password = 256L,
  display_name = 120L,
  institution = 200L,
  research_role_other = 120L,
  research_field_other = 120L,
  project_title = 200L,
  research_question = 2000L,
  disease_label = 300L,
  target_input = 80L,
  note_title = 200L,
  note_body = 8000L,
  support_subject = 200L,
  support_message = 4000L,
  snapshot_name = 120L
)

enforce_length <- function(value, limit, label) {
  text <- if (is.null(value)) "" else as.character(value)
  if (nchar(text) > as.integer(limit)) {
    return(list(
      ok = FALSE,
      message = sprintf("%s must be at most %s characters.", label, limit)
    ))
  }
  list(ok = TRUE, value = text)
}
