export_researcher_fields <- function(user) {
  if (is.null(user)) {
    return(NULL)
  }
  fields <- list(
    display_name = user$display_name,
    research_role = user$research_role,
    research_field = user$research_field,
    institution = user$institution
  )
  fields <- fields[vapply(fields, has_display_text, logical(1))]
  if (length(fields) == 0) {
    return(NULL)
  }
  fields
}

collect_export_notes <- function(db_pool, snapshot, user_id) {
  rows <- list_research_notes(db_pool, snapshot$project_id, user_id)
  if (is.null(rows) || nrow(rows) == 0) {
    return(list())
  }
  identity <- snapshot$target_identity$model$targets %||% list()
  symbol_for <- function(tid) {
    for (row in identity) {
      if (identical(as.character(row$project_target_id), as.character(tid))) {
        return(as.character(row$display_symbol %||% row$input_text %||% "target"))
      }
    }
    "target"
  }
  keep <- vapply(seq_len(nrow(rows)), function(i) {
    scope <- as.character(rows$scope[[i]])
    if (identical(scope, "project")) {
      return(TRUE)
    }
    if (identical(scope, "target")) {
      return(TRUE)
    }
    identical(scope, "snapshot") &&
      identical(as.character(rows$snapshot_id[[i]]), as.character(snapshot$id))
  }, logical(1))
  rows <- rows[keep, , drop = FALSE]
  lapply(seq_len(nrow(rows)), function(i) {
    scope <- as.character(rows$scope[[i]])
    list(
      scope = scope,
      body = as.character(rows$body[[i]]),
      title = blank_to_null(rows$title[[i]]),
      created_at = rows$created_at[[i]],
      target_symbol = if (identical(scope, "target")) symbol_for(rows$project_target_id[[i]]) else NULL
    )
  })
}

export_hydrate_snapshot <- function(snapshot) {
  snap <- hydrate_snapshot_for_view(snapshot)
  for (tid in names(snap$disease_evidence$targets %||% list())) {
    model <- snap$disease_evidence$targets[[tid]]$model
    if (is.null(model)) {
      next
    }
    if (!is.null(model$association$datatype_scores)) {
      model$association$datatype_scores <- json_rows_to_df(model$association$datatype_scores)
    }
    if (!is.null(model$association$datasource_scores)) {
      model$association$datasource_scores <- json_rows_to_df(model$association$datasource_scores)
    }
    if (!is.null(model$therapeutic_evidence$rows)) {
      model$therapeutic_evidence$rows <- json_rows_to_df(model$therapeutic_evidence$rows)
    }
    snap$disease_evidence$targets[[tid]]$model <- model
  }
  snap
}

html_esc <- function(value) {
  htmltools::htmlEscape(as.character(value %||% ""), attribute = FALSE)
}

html_esc_attr <- function(value) {
  htmltools::htmlEscape(as.character(value %||% ""), attribute = TRUE)
}

export_format_when <- function(value) {
  text <- format_user_timestamp(value)
  if (!has_display_text(text)) {
    return("Not provided")
  }
  text
}

export_format_score <- function(value) {
  num <- suppressWarnings(as.numeric(value))
  if (length(num) != 1L || is.na(num)) {
    return("Unavailable")
  }
  sprintf("%.3f", num)
}

export_embed_plot <- function(plot, width = 7.2, height = 3.2, alt = "", caption = NULL) {
  if (is.null(plot)) {
    return("")
  }
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  ok <- tryCatch({
    ggplot2::ggsave(
      filename = path,
      plot = plot,
      width = width,
      height = height,
      dpi = 120,
      device = "png",
      bg = "white"
    )
    file.exists(path) && isTRUE(file.info(path)$size > 0)
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    return("")
  }
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  uri <- sprintf(
    "data:image/png;base64,%s",
    gsub("\\s+", "", jsonlite::base64_enc(raw))
  )
  cap <- if (has_display_text(caption)) {
    sprintf("<figcaption>%s</figcaption>", html_esc(caption))
  } else {
    ""
  }
  sprintf(
    "<figure><img src=\"%s\" alt=\"%s\" />%s</figure>",
    uri,
    html_esc_attr(alt),
    cap
  )
}

export_html_table <- function(headers, rows, na_label = "Unavailable") {
  head <- paste(sprintf("<th>%s</th>", vapply(headers, html_esc, character(1))), collapse = "")
  body <- paste(vapply(rows, function(row) {
    cells <- vapply(row, function(cell) {
      if (is.null(cell) || (length(cell) == 1 && is.na(cell))) {
        sprintf("<td>%s</td>", html_esc(na_label))
      } else if (isTRUE(attr(cell, "html"))) {
        sprintf("<td>%s</td>", as.character(cell))
      } else {
        sprintf("<td>%s</td>", html_esc(cell))
      }
    }, character(1))
    sprintf("<tr>%s</tr>", paste(cells, collapse = ""))
  }, character(1)), collapse = "\n")
  sprintf(
    "<table><thead><tr>%s</tr></thead><tbody>%s</tbody></table>",
    head,
    body
  )
}

export_source_link <- function(label, href) {
  if (!has_display_text(href)) {
    return(html_esc(label))
  }
  sprintf(
    "<a href=\"%s\">%s</a>",
    html_esc_attr(href),
    html_esc(label)
  )
}

as_html <- function(text) {
  structure(text, html = TRUE)
}

dossier_status_chip <- function(section) {
  sprintf("<p class=\"status-line\">%s</p>", html_esc(section_status_display(section)))
}

dossier_empty_section_html <- function(id, heading, section) {
  sprintf(
    "<section id=\"%s\"><h2>%s</h2>%s</section>",
    html_esc_attr(id),
    html_esc(heading),
    dossier_status_chip(section)
  )
}

dossier_display_time <- function(display_value, fallback_value) {
  if (has_display_text(display_value)) {
    return(as.character(display_value[[1]]))
  }
  export_format_when(fallback_value)
}

dossier_css <- function() {
  paste(
    c(
      ":root { --navy:#18243D; --magenta:#B83280; --ink:#171B28; --muted:#5F6B7A; --paper:#ffffff; --page:#EEF1F6; --line:#D5DCE6; }",
      "html { font-size: 16px; }",
      "body { margin:0; background:var(--page); color:var(--ink); font-family: 'Source Sans 3', 'Segoe UI', Helvetica, Arial, sans-serif; line-height:1.5; }",
      "main { max-width: 920px; margin: 1.5rem auto 3rem; background: var(--paper); padding: 2.2rem 2.4rem; box-shadow: 0 8px 30px rgba(24,36,61,.08); }",
      "h1,h2,h3 { font-family: Georgia, 'Times New Roman', serif; color: var(--navy); page-break-after: avoid; }",
      "h1 { font-size: 2rem; margin: 0 0 .4rem; }",
      "h2 { font-size: 1.35rem; margin: 2rem 0 .6rem; border-bottom: 2px solid var(--magenta); padding-bottom: .25rem; }",
      "h3 { font-size: 1.08rem; margin: 1.2rem 0 .4rem; }",
      "p, li { color: var(--ink); }",
      ".kicker { letter-spacing: .12em; text-transform: uppercase; color: var(--magenta); font-size: .78rem; font-weight: 700; font-family: 'Segoe UI', Helvetica, Arial, sans-serif; }",
      ".meta { color: var(--muted); }",
      "nav.toc { background: #F7F8FB; border: 1px solid var(--line); padding: 1rem 1.2rem; margin: 1.4rem 0; }",
      "nav.toc a { color: var(--navy); }",
      "table { width: 100%; border-collapse: collapse; margin: .6rem 0 1rem; font-size: .95rem; }",
      "th, td { border: 1px solid var(--line); padding: .4rem .55rem; text-align: left; vertical-align: top; }",
      "th { background: #F3F5F9; color: var(--navy); }",
      "figure { margin: 1rem 0 1.4rem; page-break-inside: avoid; }",
      "img { max-width: 100%; height: auto; }",
      "figcaption { color: var(--muted); font-size: .9rem; margin-top: .35rem; }",
      "a { color: var(--magenta); }",
      ".badge-stale { display:inline-block; background:#F7E6EE; color:#8A245C; padding:.1rem .45rem; font-size:.8rem; font-weight:700; }",
      ".researcher-note { border-left: 4px solid var(--magenta); background: #FBF7FA; padding: .7rem 1rem; margin: .8rem 0; }",
      ".note-kicker { margin:0 0 .3rem; font-weight:700; color: var(--magenta); font-family: 'Segoe UI', Helvetica, Arial, sans-serif; }",
      ".source-label { color: var(--muted); font-size: .9rem; }",
      "section { page-break-inside: avoid; }",
      "details.appendix { margin-top: 1rem; color: var(--muted); }",
      "@media print { body { background:#fff; } main { margin:0; box-shadow:none; padding:0; max-width:none; } nav.toc, .no-print { display:none !important; } a { color: inherit; text-decoration: none; } h2, h3 { page-break-after: avoid; } img { break-inside: avoid; } }"
    ),
    collapse = "\n"
  )
}

dossier_limitations_html <- function() {
  paste(
    c(
      "<section id=\"limitations\">",
      "<h2>Method and interpretation limitations</h2>",
      "<p>This dossier is a research record of source data captured in a TargetWeave evidence snapshot. It is not a clinical recommendation, diagnosis, or treatment suggestion.</p>",
      "<ul>",
      "<li>Open Targets association scores summarize evidence within the Platform scoring framework. They are not probabilities of causality, therapeutic success, or clinical validity. TargetWeave does not generate its own score.</li>",
      "<li>Reactome pathway membership is categorical annotation membership, not pathway activity, enrichment, or a statistical over-representation result.</li>",
      "<li>Publication counts describe literature volume within this search definition and do not measure evidence quality or target importance. The PubMed corpus is not claimed to be exhaustive.</li>",
      "<li>Experimental PDB availability and sequence coverage do not measure target quality or therapeutic potential. Coordinate files and molecular viewers are not embedded.</li>",
      "<li>The snapshot reflects source data as retrieved at the recorded source times. Export generation time is separate from snapshot capture time and from each source retrieval time.</li>",
      "</ul>",
      "</section>"
    ),
    collapse = "\n"
  )
}

render_cover_html <- function(snapshot, manifest, researcher) {
  ctx <- snapshot$project_context$model
  researcher_block <- ""
  if (!is.null(researcher)) {
    bits <- c(
      researcher$display_name,
      researcher$research_role,
      researcher$research_field,
      researcher$institution
    )
    bits <- bits[vapply(bits, has_display_text, logical(1))]
    researcher_block <- sprintf("<p><strong>Researcher</strong> %s</p>", html_esc(paste(bits, collapse = " · ")))
  }
  paste(
    c(
      "<header>",
      "<p class=\"kicker\">TargetWeave</p>",
      sprintf("<h1>%s</h1>", html_esc(snapshot$name)),
      sprintf("<p>%s</p>", html_esc(ctx$title)),
      sprintf("<p>%s</p>", html_esc(ctx$research_question)),
      sprintf("<p class=\"meta\">Captured: %s</p>", html_esc(dossier_display_time(manifest$snapshot_captured_at_display, snapshot$created_at))),
      sprintf("<p class=\"meta\">Generated: %s</p>", html_esc(dossier_display_time(manifest$export_generated_at_display, manifest$export_generated_at))),
      sprintf("<p class=\"meta\">Dossier format version %s · snapshot schema version %s</p>", html_esc(manifest$dossier_format_version), html_esc(manifest$snapshot_schema_version)),
      researcher_block,
      "</header>"
    ),
    collapse = "\n"
  )
}

render_identity_html <- function(snapshot) {
  rows <- snapshot_target_rows(snapshot)
  if (nrow(rows) == 0) {
    return("")
  }
  table_rows <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    list(
      row$symbol,
      as_html(export_source_link(row$ensembl_gene_id, if (has_display_text(row$ensembl_gene_id)) paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", row$ensembl_gene_id) else NA)),
      as_html(export_source_link(row$uniprot_accession, if (has_display_text(row$uniprot_accession)) paste0("https://www.uniprot.org/uniprotkb/", row$uniprot_accession) else NA)),
      row$hgnc_id,
      row$ncbi_gene_id,
      row$organism
    )
  })
  paste(
    c(
      "<section id=\"identities\">",
      "<h2>Confirmed target identities</h2>",
      export_html_table(
        c("Symbol", "Ensembl gene ID", "UniProt accession", "HGNC ID", "NCBI GeneID", "Organism"),
        table_rows,
        na_label = "Not provided"
      ),
      "</section>"
    ),
    collapse = "\n"
  )
}

render_overview_html <- function(snapshot) {
  section <- snapshot$overview
  if (is.null(section)) {
    return("")
  }
  if (!export_section_has_content(section)) {
    return(dossier_empty_section_html("overview", "Target overview", section))
  }
  blocks <- lapply(section$targets %||% list(), function(env) {
    if (!export_section_has_content(env) || is.null(env$model)) {
      return(sprintf("<div>%s</div>", dossier_status_chip(env)))
    }
    model <- env$model
    idn <- model$identity
    protein <- model$protein
    genomic <- model$genomic
    loc <- paste(as.character(protein$subcellular_locations %||% character()), collapse = "; ")
    plot <- export_embed_plot(
      plot_genomic_context(genomic, idn$symbol),
      alt = sprintf("Genomic locus for %s", idn$symbol %||% "target"),
      caption = "Genomic context from captured Ensembl coordinates. Source: Ensembl"
    )
    fn <- protein$function_readable %||% protein$function_preview %||% "Not provided"
    stale <- if (isTRUE(env$stale_at_capture)) "<p class=\"badge-stale\">Stale at capture</p>" else ""
    paste(
      c(
        sprintf("<h3>%s</h3>", html_esc(idn$symbol %||% "Target")),
        stale,
        dossier_status_chip(env),
        sprintf("<p class=\"source-label\">Source: UniProt · Source: Ensembl</p>"),
        "<ul>",
        sprintf("<li>Protein name: %s</li>", html_esc(idn$protein_name %||% "Not provided")),
        sprintf("<li>Function: %s</li>", html_esc(fn)),
        sprintf("<li>Protein length: %s aa</li>", html_esc(protein$length_aa %||% "Not provided")),
        sprintf("<li>Molecular weight: %s</li>", html_esc(protein$molecular_weight %||% "Not provided")),
        sprintf("<li>Subcellular location: %s</li>", html_esc(if (nzchar(loc)) loc else "Not provided")),
        sprintf("<li>Genomic context: %s</li>", html_esc(genomic$location_label %||% "Not provided")),
        "</ul>",
        plot
      ),
      collapse = "\n"
    )
  })
  paste(c("<section id=\"overview\"><h2>Target overview</h2>", dossier_status_chip(section), unlist(blocks), "</section>"), collapse = "\n")
}

render_disease_html <- function(snapshot) {
  section <- snapshot$disease_evidence
  ctx <- snapshot$project_context$model
  if (is.null(section)) {
    return("")
  }
  if (!export_section_has_content(section)) {
    return(dossier_empty_section_html("disease", "Target–disease evidence", section))
  }
  disease_name <- ctx$disease_name %||% ctx$disease_label
  disease_id <- ctx$disease_ontology_id
  blocks <- lapply(section$targets %||% list(), function(env) {
    model <- env$model
    if (is.null(model)) {
      return(sprintf("<p>%s</p>", html_esc(section_status_display(env))))
    }
    assoc <- model$association
    scores <- assoc$datatype_scores
    plot <- if (!is.null(scores) && is.data.frame(scores) && nrow(scores) > 0) {
      export_embed_plot(
        plot_ot_evidence_profile(scores, model$target$symbol, model$disease$name %||% disease_name),
        alt = sprintf("Open Targets data-type association scores for %s", model$target$symbol %||% "target"),
        caption = "Open Targets data-type association scores. Numeric labels are the captured scores."
      )
    } else {
      ""
    }
    therapeutic <- ""
    rows <- model$therapeutic_evidence$rows
    if (!is.null(rows) && is.data.frame(rows) && nrow(rows) > 0) {
      show <- rows[seq_len(min(nrow(rows), 8L)), , drop = FALSE]
      names_use <- intersect(c("drug", "phase", "mechanism", "status"), names(show))
      if (length(names_use) > 0) {
        therapeutic <- export_html_table(
          names_use,
          lapply(seq_len(nrow(show)), function(i) as.list(show[i, names_use, drop = FALSE]))
        )
      }
    }
    paste(
      c(
        sprintf("<h3>%s</h3>", html_esc(model$target$symbol %||% "Target")),
        if (isTRUE(env$stale_at_capture)) "<p class=\"badge-stale\">Stale at capture</p>" else "",
        sprintf("<p>Disease: %s (%s)</p>", html_esc(model$disease$name %||% disease_name), html_esc(model$disease$id %||% disease_id %||% "Not provided")),
        sprintf("<p>Direct association score: %s</p>", html_esc(export_format_score(assoc$overall_score_direct))),
        sprintf("<p>Broader ontology-aware score: %s</p>", html_esc(export_format_score(assoc$overall_score_inclusive))),
        sprintf("<p>%s</p>", html_esc(assoc$association_scope_note %||% "")),
        "<p class=\"source-label\">Source: Open Targets</p>",
        plot,
        if (nzchar(therapeutic)) "<p>Known therapeutic evidence sample (captured)</p>" else "",
        therapeutic
      ),
      collapse = "\n"
    )
  })
  paste(
    c(
      "<section id=\"disease\"><h2>Target–disease evidence</h2>",
      dossier_status_chip(section),
      sprintf("<p>%s</p>", html_esc("Open Targets scores are not probabilities of causality, therapeutic success, or clinical validity.")),
      unlist(blocks),
      "</section>"
    ),
    collapse = "\n"
  )
}

render_comparison_html <- function(snapshot) {
  section <- snapshot$comparison
  if (is.null(section)) {
    return("")
  }
  if (!export_section_has_content(section)) {
    return(dossier_empty_section_html("comparison", "Candidate comparison", section))
  }
  model <- section$model
  targets <- model$targets
  matrix <- model$datatype_matrix
  score_rows <- list()
  if (!is.null(targets) && nrow(targets) > 0) {
    score_rows <- lapply(seq_len(nrow(targets)), function(i) {
      list(
        targets$symbol[[i]],
        export_format_score(targets$overall_direct_score[[i]]),
        if ("overall_inclusive_score" %in% names(targets)) export_format_score(targets$overall_inclusive_score[[i]]) else "Unavailable",
        targets$retrieval_status[[i]] %||% targets$cache_status[[i]]
      )
    })
  }
  heat <- export_embed_plot(
    plot_ot_comparison_heatmap(matrix, model$disease$name),
    width = 7.4,
    height = 4.2,
    alt = "Open Targets comparison heatmap of data-type association scores",
    caption = "Missing values are shown as a dash, distinct from a returned score of zero. Source: Open Targets"
  )
  bars <- export_embed_plot(
    plot_ot_overall_scores(targets),
    alt = "Open Targets overall direct association scores by target",
    caption = "Bars are sorted by Open Targets direct association score for display. This is not a recommendation or ranking."
  )
  paste(
    c(
      "<section id=\"comparison\"><h2>Candidate comparison</h2>",
      dossier_status_chip(section),
      sprintf("<p>Disease: %s (%s)</p>", html_esc(model$disease$name), html_esc(model$disease$id)),
      "<p class=\"source-label\">Source: Open Targets</p>",
      sprintf("<p>Open Targets data version %s · API %s</p>", html_esc(model$provenance$data_version %||% "Not provided"), html_esc(model$provenance$api_version %||% "Not provided")),
      "<p>Open Targets scores are not probabilities of causality, therapeutic success, or clinical validity.</p>",
      export_html_table(c("Target", "Direct score", "Broader score", "Retrieval status"), score_rows),
      bars,
      heat,
      "</section>"
    ),
    collapse = "\n"
  )
}

render_pathways_html <- function(snapshot) {
  section <- snapshot$pathways
  if (is.null(section)) {
    return("")
  }
  if (!export_section_has_content(section)) {
    return(dossier_empty_section_html("pathways", "Reactome pathway membership", section))
  }
  model <- section$model
  mat <- model$membership_matrix
  order <- if (!is.null(mat) && nrow(mat) > 0) unique(as.character(mat$pathway_name)) else character()
  plot <- export_embed_plot(
    plot_pathway_membership_matrix(mat, order),
    width = 7.4,
    height = max(3.2, min(8, 0.35 * length(order) + 1.6)),
    alt = "Reactome pathway membership matrix",
    caption = "Filled marker = membership present; dash = not present in the retrieved membership set. This is not enrichment."
  )
  csv <- export_pathways_csv(snapshot)
  table <- ""
  if (!is.null(csv) && nrow(csv) > 0) {
    table <- export_html_table(
      c("Pathway ID", "Pathway name", "Matched targets", "Count", "Membership scope"),
      lapply(seq_len(nrow(csv)), function(i) {
        pid <- csv$pathway_id[[i]]
        list(
          as_html(export_source_link(pid, paste0("https://reactome.org/content/detail/", pid))),
          csv$pathway_name[[i]],
          csv$matched_targets[[i]],
          csv$matched_target_count[[i]],
          csv$membership_scope[[i]]
        )
      })
    )
  }
  paste(
    c(
      "<section id=\"pathways\"><h2>Reactome pathway membership</h2>",
      dossier_status_chip(section),
      sprintf("<p>Reactome release %s · membership scope %s</p>", html_esc(model$provenance$reactome_release %||% "Not provided"), html_esc(model$provenance$membership_scope %||% "lowest_level")),
      "<p class=\"source-label\">Source: Reactome</p>",
      "<p>Reactome membership indicates annotation membership. Shared membership is not pathway activity, enrichment, or a ranking.</p>",
      plot,
      table,
      "</section>"
    ),
    collapse = "\n"
  )
}

render_literature_html <- function(snapshot) {
  section <- snapshot$literature
  if (is.null(section)) {
    return("")
  }
  if (!export_section_has_content(section)) {
    return(dossier_empty_section_html("literature", "Literature landscape", section))
  }
  model <- section$model
  blocks <- lapply(model$targets %||% list(), function(item) {
    recs <- item$recent_records
    rec_table <- ""
    if (!is.null(recs) && nrow(recs) > 0) {
      rec_table <- export_html_table(
        c("PMID", "Title", "First author", "Journal", "Date", "DOI"),
        lapply(seq_len(nrow(recs)), function(i) {
          pmid <- as.character(recs$pmid[[i]])
          list(
            as_html(export_source_link(pmid, paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"))),
            recs$title[[i]],
            if ("first_author" %in% names(recs)) recs$first_author[[i]] else NA,
            if ("journal" %in% names(recs)) recs$journal[[i]] else NA,
            if ("publication_date" %in% names(recs)) recs$publication_date[[i]] else NA,
            if ("doi" %in% names(recs)) recs$doi[[i]] else NA
          )
        })
      )
    }
    paste(
      c(
        sprintf("<h3>%s</h3>", html_esc(item$target$symbol)),
        sprintf("<p>Defined PubMed corpus count: %s</p>", html_esc(item$corpus$total_count %||% "Not provided")),
        sprintf("<p>Search definition: %s</p>", html_esc(item$corpus$disease_query %||% model$disease$disease_query %||% "Not provided")),
        sprintf("<p>NCBI GeneID: %s</p>", html_esc(item$target$ncbi_gene_id %||% "Not provided")),
        export_embed_plot(
          plot_publication_trend(item$trend),
          alt = sprintf("Ten-year publication trend for %s", item$target$symbol),
          caption = "Publication counts within this search definition. Source: NCBI PubMed"
        ),
        rec_table
      ),
      collapse = "\n"
    )
  })
  paste(
    c(
      "<section id=\"literature\"><h2>Literature landscape</h2>",
      dossier_status_chip(section),
      "<p class=\"source-label\">Source: NCBI PubMed</p>",
      "<p>Publication counts describe literature volume within this search definition and do not measure evidence quality or target importance.</p>",
      "<p>Abstracts are not included. This corpus is not claimed to be exhaustive.</p>",
      unlist(blocks),
      "</section>"
    ),
    collapse = "\n"
  )
}

render_structures_html <- function(snapshot) {
  section <- snapshot$structures
  if (is.null(section)) {
    return("")
  }
  if (!export_section_has_content(section)) {
    return(dossier_empty_section_html("structures", "Experimental structures", section))
  }
  model <- section$model
  counts <- export_embed_plot(
    plot_structure_entry_counts(model$summary),
    alt = "Experimental PDB entry counts by target",
    caption = "Experimental PDB entry counts from captured RCSB metadata. Source: RCSB PDB"
  )
  coverage_plots <- lapply(model$targets %||% list(), function(item) {
    length_aa <- item$uniprot_length %||% item$records[[1]]$coverage$uniprot_length
    export_embed_plot(
      plot_structure_coverage(item$records, length_aa),
      height = 3.6,
      alt = sprintf("Sequence coverage of experimental structures for %s", item$target$symbol %||% "target"),
      caption = sprintf("Sequence coverage for %s. Coordinate files are not embedded.", item$target$symbol %||% "target")
    )
  })
  csv <- export_structures_csv(snapshot)
  table <- ""
  if (!is.null(csv) && nrow(csv) > 0) {
    table <- export_html_table(
      c("Target", "PDB ID", "Polymer entity", "Chains", "Method", "Resolution", "Coverage", "Release date"),
      lapply(seq_len(nrow(csv)), function(i) {
        pdb <- csv$pdb_id[[i]]
        list(
          csv$target[[i]],
          as_html(export_source_link(pdb, paste0("https://www.rcsb.org/structure/", pdb))),
          csv$polymer_entity[[i]],
          csv$chains[[i]],
          csv$method[[i]],
          csv$resolution_angstrom[[i]],
          csv$coverage_fraction[[i]],
          csv$release_date[[i]]
        )
      })
    )
  }
  paste(
    c(
      "<section id=\"structures\"><h2>Experimental structures</h2>",
      dossier_status_chip(section),
      "<p class=\"source-label\">Source: RCSB PDB</p>",
      "<p>PDB IDs and metadata are preserved. Full coordinate files and molecular viewers are not embedded. External RCSB links require a network connection.</p>",
      counts,
      unlist(coverage_plots),
      table,
      "</section>"
    ),
    collapse = "\n"
  )
}

render_notes_html <- function(notes) {
  if (is.null(notes) || length(notes) == 0) {
    return("")
  }
  cards <- vapply(notes, function(note) {
    scope <- switch(
      as.character(note$scope %||% ""),
      project = "Project",
      snapshot = "Snapshot",
      target = sprintf("Target · %s", note$target_symbol %||% "target"),
      "Research"
    )
    sprintf(
      "<aside class=\"researcher-note\"><p class=\"note-kicker\">Researcher note</p><p class=\"meta\">%s · %s</p><p>%s</p></aside>",
      html_esc(scope),
      html_esc(export_format_when(note$created_at)),
      html_esc(note$body)
    )
  }, character(1))
  paste(c("<section id=\"notes\"><h2>Research notes</h2>", cards, "</section>"), collapse = "\n")
}

render_provenance_html <- function(snapshot, manifest) {
  rows <- lapply(snapshot$source_manifest %||% list(), function(row) {
    list(
      row$source %||% "Not provided",
      row$record_id,
      row$version %||% row$data_version %||% "Not provided",
      row$api_version %||% "",
      export_format_when(row$retrieved_at),
      freshness_label(row$cache_status),
      row$scope %||% row$query_scope %||% row$membership_scope %||% row$disease_query %||% ""
    )
  })
  appendix <- sprintf(
    "<details class=\"appendix\"><summary>Export timestamps</summary><ul><li>Snapshot captured at: %s</li><li>Export generated at: %s</li><li>Dossier format version: %s</li><li>Snapshot schema version: %s</li></ul></details>",
    html_esc(manifest$snapshot_captured_at),
    html_esc(manifest$export_generated_at),
    html_esc(manifest$dossier_format_version),
    html_esc(manifest$snapshot_schema_version)
  )
  paste(
    c(
      "<section id=\"provenance\"><h2>Source manifest / provenance</h2>",
      "<p>Source retrieval time is distinct from snapshot capture time and from export generation time.</p>",
      export_html_table(
        c("Source", "Identifier", "Version / release", "API version", "Retrieved at", "Freshness at capture", "Scope / definition"),
        rows,
        na_label = "Not provided"
      ),
      appendix,
      "</section>"
    ),
    collapse = "\n"
  )
}

render_context_html <- function(snapshot) {
  ctx <- snapshot$project_context$model
  paste(
    c(
      "<section id=\"context\"><h2>Research context</h2>",
      "<ul>",
      sprintf("<li>Project title: %s</li>", html_esc(ctx$title)),
      sprintf("<li>Research question: %s</li>", html_esc(ctx$research_question)),
      sprintf("<li>Organism: %s</li>", html_esc(ctx$organism)),
      sprintf("<li>Disease: %s</li>", html_esc(ctx$disease_name %||% ctx$disease_label)),
      sprintf("<li>Disease ontology ID: %s</li>", html_esc(ctx$disease_ontology_id %||% "Not provided")),
      "</ul>",
      "</section>"
    ),
    collapse = "\n"
  )
}

render_dossier_html <- function(
  snapshot,
  manifest,
  notes = list(),
  researcher = NULL
) {
  parts <- list(
    cover = render_cover_html(snapshot, manifest, researcher),
    context = render_context_html(snapshot),
    identities = render_identity_html(snapshot),
    overview = render_overview_html(snapshot),
    disease = render_disease_html(snapshot),
    comparison = render_comparison_html(snapshot),
    pathways = render_pathways_html(snapshot),
    literature = render_literature_html(snapshot),
    structures = render_structures_html(snapshot),
    notes = if (isTRUE(manifest$include_notes)) render_notes_html(notes) else "",
    provenance = render_provenance_html(snapshot, manifest),
    limitations = dossier_limitations_html()
  )
  toc_items <- c(
    "<li><a href=\"#context\">Research context</a></li>",
    "<li><a href=\"#identities\">Confirmed target identities</a></li>",
    "<li><a href=\"#overview\">Target overview</a></li>",
    "<li><a href=\"#disease\">Target–disease evidence</a></li>",
    "<li><a href=\"#comparison\">Candidate comparison</a></li>",
    "<li><a href=\"#pathways\">Reactome pathway membership</a></li>",
    "<li><a href=\"#literature\">Literature landscape</a></li>",
    "<li><a href=\"#structures\">Experimental structures</a></li>"
  )
  if (nzchar(parts$notes)) {
    toc_items <- c(toc_items, "<li><a href=\"#notes\">Research notes</a></li>")
  }
  toc_items <- c(
    toc_items,
    "<li><a href=\"#provenance\">Source manifest / provenance</a></li>",
    "<li><a href=\"#limitations\">Method / interpretation limitations</a></li>"
  )
  paste(
    c(
      "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"utf-8\" />",
      sprintf("<title>%s — TargetWeave research dossier</title>", html_esc(snapshot$name)),
      "<style>",
      dossier_css(),
      "</style>",
      "</head>",
      "<body>",
      "<main>",
      parts$cover,
      sprintf("<nav class=\"toc\" aria-label=\"Contents\"><h2>Contents</h2><ol>%s</ol></nav>", paste(toc_items, collapse = "")),
      parts$context,
      parts$identities,
      parts$overview,
      parts$disease,
      parts$comparison,
      parts$pathways,
      parts$literature,
      parts$structures,
      parts$notes,
      parts$provenance,
      parts$limitations,
      "</main>",
      "</body>",
      "</html>"
    ),
    collapse = "\n"
  )
}

write_export_zip <- function(source_dir, zip_path) {
  zip_path <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  files <- list.files(source_dir, recursive = TRUE)
  if (length(files) == 0) {
    stop("Export package was empty.")
  }
  if (nzchar(Sys.which("zip"))) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(source_dir)
    utils::zip(zip_path, files, flags = "-r9Xq")
    if (file.exists(zip_path)) {
      return(zip_path)
    }
  }
  py <- Sys.which("python3")
  if (!nzchar(py)) {
    py <- Sys.which("python")
  }
  if (!nzchar(py)) {
    stop("ZIP export requires the system zip command or Python.")
  }
  script <- tempfile(fileext = ".py")
  on.exit(unlink(script), add = TRUE)
  writeLines(
    c(
      "import os, zipfile, sys",
      sprintf("root = %s", jsonlite::toJSON(normalizePath(source_dir), auto_unbox = TRUE)),
      sprintf("out = %s", jsonlite::toJSON(zip_path, auto_unbox = TRUE)),
      "with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:",
      "    for dirpath, _, filenames in os.walk(root):",
      "        for name in filenames:",
      "            path = os.path.join(dirpath, name)",
      "            z.write(path, os.path.relpath(path, root))"
    ),
    script
  )
  status <- system2(py, script)
  if (!file.exists(zip_path)) {
    stop(sprintf("ZIP export failed (status %s).", status))
  }
  zip_path
}

build_snapshot_export_package <- function(
  snapshot,
  dest_dir,
  include_notes = TRUE,
  notes = list(),
  researcher = NULL,
  generated_at = Sys.time()
) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  snap <- export_hydrate_snapshot(snapshot)
  manifest <- build_export_manifest(
    snap,
    generated_at = generated_at,
    include_notes = include_notes,
    include_researcher = !is.null(researcher)
  )
  html <- render_dossier_html(
    snap,
    manifest,
    notes = if (isTRUE(include_notes)) notes else list(),
    researcher = researcher
  )
  writeLines(html, file.path(dest_dir, "report.html"), useBytes = TRUE)
  write_export_manifest_files(dest_dir, manifest)
  write_export_tables(snap, file.path(dest_dir, "tables"))
  invisible(list(dir = dest_dir, manifest = manifest, html = html))
}

export_snapshot_artifact <- function(
  snapshot,
  format = c("html", "zip"),
  include_notes = TRUE,
  notes = list(),
  researcher = NULL,
  generated_at = Sys.time()
) {
  format <- match.arg(format)
  work <- tempfile("tw-export-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  built <- build_snapshot_export_package(
    snapshot,
    work,
    include_notes = include_notes,
    notes = notes,
    researcher = researcher,
    generated_at = generated_at
  )
  stem <- export_safe_stem(snapshot$name, snapshot$created_at)
  if (identical(format, "html")) {
    path <- tempfile(pattern = paste0(stem, "_"), fileext = ".html")
    file.copy(file.path(work, "report.html"), path, overwrite = TRUE)
    return(list(ok = TRUE, path = path, filename = paste0(stem, ".html"), manifest = built$manifest))
  }
  path <- tempfile(pattern = paste0(stem, "_"), fileext = ".zip")
  write_export_zip(work, path)
  list(ok = TRUE, path = path, filename = paste0(stem, ".zip"), manifest = built$manifest)
}

export_owned_snapshot <- function(
  db_pool,
  user_id,
  snapshot_id,
  format = c("html", "zip"),
  include_notes = TRUE,
  include_researcher = FALSE,
  generated_at = Sys.time()
) {
  format <- match.arg(format)
  snapshot <- get_owned_snapshot(db_pool, snapshot_id, user_id)
  if (is.null(snapshot)) {
    return(list(ok = FALSE, error = "Snapshot was not found or you do not have access to it."))
  }
  snapshot <- export_hydrate_snapshot(snapshot)
  notes <- list()
  if (isTRUE(include_notes)) {
    notes <- collect_export_notes(db_pool, snapshot, user_id)
  }
  researcher <- NULL
  if (isTRUE(include_researcher)) {
    researcher <- export_researcher_fields(load_user_by_id(db_pool, user_id))
  }
  result <- tryCatch(
    export_snapshot_artifact(
      snapshot,
      format = format,
      include_notes = include_notes,
      notes = notes,
      researcher = researcher,
      generated_at = generated_at
    ),
    error = function(e) {
      if (exists("tw_log", mode = "function", inherits = TRUE)) {
        tw_log("export_failed", level = "error")
      }
      list(ok = FALSE, error = "The export could not be generated.")
    }
  )
  if (!isTRUE(result$ok) && exists("tw_log", mode = "function", inherits = TRUE)) {
    tw_log("export_failed", level = "error")
  }
  result
}
