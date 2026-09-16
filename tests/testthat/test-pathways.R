test_that("pathway gate requires one confirmed target and ignores disease", {
  none <- confirmed_reactome_target("t1", "EGFR", "P00533", status = "ambiguous")
  expect_false(pathway_gate(none)$ok)

  one <- confirmed_reactome_target("t1", "EGFR", "P00533")
  expect_true(pathway_gate(one)$ok)
})

test_that("unconfirmed targets are excluded and missing UniProt is unavailable", {
  skip_if_not_installed("httr2")
  targets <- rbind(
    confirmed_reactome_target("t1", "EGFR", "P00533", "ENSG00000146648"),
    confirmed_reactome_target("t2", "UNK", NA_character_),
    confirmed_reactome_target("t3", "RAW", "P01116", status = "ambiguous")
  )
  payload <- read_fixture("reactome_pathways_p00533_excerpt.json")
  result <- retrieve_project_pathways(
    list(id = "p1"),
    targets,
    db_pool = NULL,
    perform = reactome_json_perform(payload),
    version_perform = reactome_text_perform("97")
  )
  expect_equal(result$status, "ready")
  expect_equal(result$pathways$targets$symbol, "EGFR")
  reasons <- vapply(result$pathways$excluded_targets, `[[`, character(1), "reason")
  expect_true("missing_uniprot" %in% reasons)
  expect_true("unconfirmed" %in% reasons)
})

test_that("overlap matrix matches the EGFR/KRAS/TP53 membership example", {
  memberships <- list(
    membership_from_ids("egfr", "EGFR", "P00533", c("A", "B", "C"), c("Path A", "Path B", "Path C")),
    membership_from_ids("kras", "KRAS", "P01116", c("B", "C", "D"), c("Path B", "Path C", "Path D")),
    membership_from_ids("tp53", "TP53", "P04637", c("C", "E"), c("Path C", "Path E"))
  )
  overlap <- build_pathway_overlap(memberships, reactome_release = "97")

  count_for <- function(id) {
    overlap$pathways$matched_target_count[overlap$pathways$pathway_id == id]
  }
  members_for <- function(id) {
    overlap$pathways$matched_targets[overlap$pathways$pathway_id == id]
  }

  expect_equal(count_for("A"), 1L)
  expect_equal(members_for("A"), "EGFR")
  expect_equal(count_for("B"), 2L)
  expect_match(members_for("B"), "EGFR")
  expect_match(members_for("B"), "KRAS")
  expect_equal(count_for("C"), 3L)
  expect_equal(count_for("D"), 1L)
  expect_equal(members_for("D"), "KRAS")
  expect_equal(count_for("E"), 1L)
  expect_equal(members_for("E"), "TP53")

  c_row <- overlap$membership_matrix[overlap$membership_matrix$pathway_id == "C", ]
  expect_true(all(c_row$is_member))

  a_row <- overlap$membership_matrix[overlap$membership_matrix$pathway_id == "A", ]
  expect_equal(a_row$is_member[a_row$symbol == "EGFR"], TRUE)
  expect_equal(a_row$is_member[a_row$symbol == "KRAS"], FALSE)

  shared <- overlap$pathways[is_shared_pathway(overlap$pathways$matched_target_count), ]
  expect_equal(sort(shared$pathway_id), c("B", "C"))

  expect_equal(overlap$pathways$pathway_id[[1]], "C")
  expect_false(any(c("p-value", "fdr", "enrichment") %in% names(overlap$pathways)))
})

test_that("failed retrieval is not treated as absence and empty membership is not an error", {
  memberships <- list(
    membership_from_ids("egfr", "EGFR", "P00533", "A", "Path A"),
    membership_from_ids("kras", "KRAS", "P01116", character())
  )
  overlap <- build_pathway_overlap(
    memberships,
    failures = list(list(project_target_id = "met", symbol = "MET", message = "Pathway data unavailable for MET.")),
    reactome_release = "97"
  )
  expect_equal(sort(overlap$targets$symbol), c("EGFR", "KRAS"))
  expect_false("MET" %in% overlap$membership_matrix$symbol)
  expect_equal(overlap$targets$status[overlap$targets$symbol == "KRAS"], "empty")
  expect_equal(overlap$failures[[1]]$symbol, "MET")

  visible <- filter_pathway_visual(overlap, overlap$targets$project_target_id, shared_only = TRUE)
  expect_equal(nrow(visible$pathways_visible), 0)
})

test_that("selection filter is display-only and does not invent identifiers", {
  memberships <- list(
    membership_from_ids("egfr", "EGFR", "P00533", c("A", "B"), c("Path A", "Path B")),
    membership_from_ids("kras", "KRAS", "P01116", "B", "Path B")
  )
  overlap <- build_pathway_overlap(memberships)
  selected <- normalize_pathway_selection("egfr", overlap$targets$project_target_id)
  expect_equal(selected, "egfr")
  expect_equal(normalize_pathway_selection("outsider", overlap$targets$project_target_id), overlap$targets$project_target_id)

  visible <- filter_pathway_visual(overlap, "egfr", shared_only = FALSE, max_rows = 20)
  expect_equal(unique(as.character(visible$targets_visible$symbol)), "EGFR")
})

test_that("Pathways navigation exists and the module does not call httr2", {
  home <- paste(readLines(file.path(app_root(), "R/modules/mod_project_home.R")), collapse = "\n")
  mod <- paste(readLines(file.path(app_root(), "R/modules/mod_pathways.R")), collapse = "\n")
  expect_match(home, "Pathways")
  expect_match(home, "mod_pathways_ui")
  expect_false(grepl("httr2", mod))
  expect_equal(workspace_panel_label("pathways"), "Pathways")
  expect_equal(workspace_back_destination("pathways"), "project")
})

test_that("Reactome stale fallback uses cached membership without zero-filling failures", {
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)

  payload <- read_fixture("reactome_pathways_p00533_excerpt.json")
  live <- fetch_reactome_uniprot_pathways(
    "P00533",
    db_pool = db_pool,
    reactome_release = "97",
    perform = reactome_json_perform(payload)
  )
  expect_true(live$ok)

  DBI::dbExecute(
    db_pool,
    "
    UPDATE api_cache
    SET expires_at = NOW() - INTERVAL '1 hour'
    WHERE source = 'reactome'
      AND cache_key LIKE 'test:reactome:membership:97:P00533:%'
    "
  )

  stale <- fetch_reactome_uniprot_pathways(
    "P00533",
    db_pool = db_pool,
    reactome_release = "97",
    perform = function(req) stop("Reactome unavailable")
  )
  expect_true(stale$ok)
  expect_equal(stale$cache_status, "stale")
  parsed <- parse_reactome_pathway_membership(stale$data)
  expect_true(nrow(parsed$pathways) > 0)
})

pathways_ui_html <- function(packed, selected = NULL, inspect_id = NULL, filter_choice = NULL, query = NULL) {
  source_app("R/modules/mod_pathways.R")
  if (is.null(selected)) {
    selected <- packed$pathways$targets$project_target_id
  }
  paste(
    as.character(pathways_result_ui(
      packed,
      shiny::NS("pathways"),
      as.character(selected),
      inspect_id,
      filter_choice,
      query
    )),
    collapse = "\n"
  )
}

test_that("pathways overlap UI preserves membership, shared counts, zero, failed, and stale", {
  skip_if_not_installed("shiny")
  memberships <- list(
    membership_from_ids("egfr", "EGFR", "P00533", c("A", "B", "C"), c("Path A", "Path B", "Path C")),
    membership_from_ids("kras", "KRAS", "P01116", c("B", "C", "D"), c("Path B", "Path C", "Path D")),
    membership_from_ids("tp53", "TP53", "P04637", c("C", "E"), c("Path C", "Path E")),
    membership_from_ids("met", "MET", "P08581", character())
  )
  overlap <- build_pathway_overlap(
    memberships,
    failures = list(list(project_target_id = "alk", symbol = "ALK", message = "Pathway data unavailable for ALK.")),
    reactome_release = "97"
  )
  packed <- list(status = "ready", pathways = overlap)
  html <- pathways_ui_html(packed, filter_choice = "all")
  expect_match(html, "pathway-overlap-matrix")
  expect_match(html, "pathway-matrix-scroll")
  expect_match(html, "Confirmed targets included")
  expect_match(html, "Unique Reactome pathways")
  expect_match(html, "Shared pathways across 2\\+ targets")
  expect_equal(overlap$pathways$pathway_id[[1]], "C")
  expect_match(html, "Shared by 3 targets")
  expect_match(html, "Shared by 2 targets")
  expect_match(html, ">1 target<")
  expect_match(html, "data-pathway-member=\"present\"")
  expect_match(html, "data-pathway-member=\"absent\"")
  expect_true(grepl("EGFR", html))
  expect_true(grepl("MET", html))
  expect_false(grepl("pathway-overlap-target[^>]*>ALK<", html))
  expect_match(html, "Pathway data unavailable for ALK")
  expect_false(grepl("enrichment|pathway importance|best target|highly shared", html, ignore.case = TRUE))
  expect_match(html, "How to interpret these results")
  expect_false(grepl("interpretation-guidance[^>]*open", html))
  expect_match(html, "View pathway list")
  expect_match(html, "Technical provenance")

  shared_html <- pathways_ui_html(packed, filter_choice = "shared")
  expect_match(shared_html, "pathway-overlap-id\">C<")
  expect_match(shared_html, "pathway-overlap-id\">B<")
  expect_false(grepl("pathway-overlap-id\">A<", shared_html))
  expect_match(shared_html, "Path A")

  stale <- packed
  stale$stale_target_set <- TRUE
  stale$stale_message <- "Confirmed target set changed. Refresh pathways to retrieve current evidence."
  stale_html <- pathways_ui_html(stale, filter_choice = "all")
  expect_match(stale_html, "target-set-stale")
  expect_match(stale_html, "Refresh pathways")
  expect_match(stale_html, "pathway-shell is-stale")
  expect_match(stale_html, "Path C")
  expect_false(grepl("pathway-shell is-stale", html))
})

test_that("pathways matrix keeps target columns for 3, 5, and 8 confirmed targets", {
  skip_if_not_installed("shiny")
  symbols <- c("EGFR", "KRAS", "MET", "TP53", "ALK", "BRAF", "ROS1", "RET")
  uniprot <- c("P00533", "P01116", "P08581", "P04637", "Q9UM73", "P15056", "P08922", "P07949")
  ids <- paste0("t-", tolower(symbols))
  long_name <- "Signaling by the B Cell Receptor (BCR) in the immune system long Reactome name"

  render_n <- function(n) {
    slice <- seq_len(n)
    memberships <- lapply(slice, function(i) {
      pids <- c("SHARED", paste0("U", i))
      names <- c(long_name, sprintf("Unique pathway %s", symbols[[i]]))
      membership_from_ids(ids[[i]], symbols[[i]], uniprot[[i]], pids, names)
    })
    overlap <- build_pathway_overlap(memberships, reactome_release = "97")
    packed <- list(status = "ready", pathways = overlap)
    html <- pathways_ui_html(packed, filter_choice = "all")
    list(overlap = overlap, html = html, symbols = symbols[slice])
  }

  for (n in c(3L, 5L, 8L)) {
    rendered <- render_n(n)
    expect_equal(nrow(rendered$overlap$targets), n)
    expect_equal(
      rendered$overlap$pathways$matched_target_count[rendered$overlap$pathways$pathway_id == "SHARED"],
      n
    )
    expect_match(rendered$html, "pathway-matrix-scroll")
    expect_match(rendered$html, long_name, fixed = TRUE)
    expect_match(rendered$html, sprintf("Shared by %s targets", n))
    for (symbol in rendered$symbols) {
      expect_true(grepl(symbol, rendered$html, fixed = TRUE), info = paste(n, symbol))
    }
    expect_equal(
      length(gregexpr("pathway-overlap-target", rendered$html)[[1]]),
      n
    )
  }
})
