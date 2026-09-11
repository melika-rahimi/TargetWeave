target_workspace_label <- function(row) {
  if (is.null(row) || (is.data.frame(row) && nrow(row) != 1)) {
    return(NA_character_)
  }

  symbol <- row$display_symbol[[1]]
  if (has_display_text(symbol)) {
    return(as.character(symbol))
  }

  input_text <- row$input_text[[1]]
  if (has_display_text(input_text)) {
    return(as.character(input_text))
  }

  NA_character_
}

workspace_panel_label <- function(panel) {
  if (identical(panel, "overview")) {
    return("Overview")
  }
  if (identical(panel, "resolver")) {
    return("Identity")
  }
  if (identical(panel, "evidence")) {
    return("Disease evidence")
  }
  if (identical(panel, "compare")) {
    return("Compare evidence")
  }
  if (identical(panel, "pathways")) {
    return("Pathways")
  }
  if (identical(panel, "literature")) {
    return("Literature")
  }
  if (identical(panel, "structures")) {
    return("Structures")
  }
  if (identical(panel, "research")) {
    return("Notes / Snapshots")
  }
  NA_character_
}

workspace_back_destination <- function(panel) {
  if (identical(panel, "overview") || identical(panel, "resolver") || identical(panel, "evidence") || identical(panel, "compare") || identical(panel, "pathways") || identical(panel, "literature") || identical(panel, "structures") || identical(panel, "research")) {
    "project"
  } else {
    "projects"
  }
}

workspace_back_label <- function(panel) {
  if (identical(workspace_back_destination(panel), "project")) {
    "Back to project"
  } else {
    "Back to projects"
  }
}
