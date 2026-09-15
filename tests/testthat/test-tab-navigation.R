source_app("R/modules/mod_pathways.R")
source_app("R/modules/mod_ot_comparison.R")
source_app("R/modules/mod_literature.R")
source_app("R/modules/mod_structures.R")
source_app("R/modules/mod_disease_evidence.R")

read_module <- function(name) {
  paste(readLines(file.path(app_root(), "R/modules", name)), collapse = "\n")
}

test_that("live workspace modules do not rasterize ggplot on the server", {
  files <- c(
    "mod_pathways.R",
    "mod_ot_comparison.R",
    "mod_literature.R",
    "mod_structures.R",
    "mod_overview.R",
    "mod_disease_evidence.R",
    "mod_research.R"
  )
  for (name in files) {
    src <- read_module(name)
    expect_false(grepl("renderPlot\\(|plotOutput\\(", src), info = name)
  }
})

test_that("tab shells announce a section loading state", {
  home <- read_module("mod_project_home.R")
  expect_match(home, "Loading pathways")
  expect_match(home, "Loading literature")
  expect_match(home, "Loading structures")
  expect_match(home, "Loading compare evidence")
  expect_match(home, "Loading notes and snapshots")
  expect_match(home, "Loading project")
  css <- paste(readLines(file.path(app_root(), "www", "styles.css")), collapse = "\n")
  expect_match(css, "tab-loading-msg")
  expect_match(css, "recalculating")
})

test_that("generic tab loading and scientific retrieval banners are mutually exclusive", {
  css <- paste(readLines(file.path(app_root(), "www", "styles.css")), collapse = "\n")
  expect_match(
    css,
    "\\.tab-panel-shell:has\\(\\.panel-state-retrieving\\)\\s*>\\s*\\.tab-loading-msg\\s*\\{[[:space:]]*display:\\s*none"
  )
  expect_match(css, "panel-state-empty")
  expect_false(grepl(
    "\\.tab-panel-shell:has\\(\\.shiny-html-output\\.recalculating\\)\\s*>\\s*\\.tab-loading-msg",
    css
  ))

  retrieving <- as.character(panel_state_ui("retrieving", "Retrieving Reactome pathways\u2026"))
  expect_match(retrieving, "panel-state-retrieving")
  expect_false(grepl("Loading pathways", retrieving, fixed = TRUE))

  shell <- as.character(tab_panel_shell(
    loading_label = "Loading pathways\u2026",
    div(
      class = "shiny-html-output recalculating",
      panel_state_ui("retrieving", "Retrieving Reactome pathways\u2026")
    )
  ))
  expect_match(shell, "Loading pathways")
  expect_match(shell, "Retrieving Reactome pathways")
  expect_match(shell, "panel-state-retrieving")
  expect_match(shell, "data-tab-loading")

  compare <- read_module("mod_ot_comparison.R")
  pathways <- read_module("mod_pathways.R")
  literature <- read_module("mod_literature.R")
  structures <- read_module("mod_structures.R")
  overview <- read_module("mod_overview.R")
  evidence <- read_module("mod_disease_evidence.R")
  expect_match(compare, "Retrieving Open Targets evidence")
  expect_match(pathways, "Retrieving Reactome pathways")
  expect_match(literature, "Retrieving PubMed records")
  expect_match(structures, "Retrieving experimental structures")
  expect_match(overview, "Retrieving identity")
  expect_match(evidence, "Retrieving Open Targets evidence")
})

test_that("live comparison and pathway UIs use HTML visuals, not plot placeholders", {
  snap <- production_like_export_snapshot()
  ns <- shiny::NS("x")
  cmp <- as.character(comparison_result_ui(
    list(status = "ready", comparison = snap$comparison$model),
    ns,
    as.character(snap$comparison$model$targets$project_target_id)
  ))
  expect_match(cmp, "tw-viz")
  expect_false(grepl("plot-container|shiny-plot-output", cmp))

  overlap <- build_pathway_overlap(list(
    membership_from_ids("egfr", "EGFR", "P00533", c("A", "B"), c("Path A", "Path B")),
    membership_from_ids("kras", "KRAS", "P01116", "B", "Path B")
  ))
  path <- as.character(pathways_result_ui(
    list(status = "ready", pathways = overlap),
    ns,
    as.character(overlap$targets$project_target_id),
    NULL,
    "shared",
    NULL
  ))
  expect_match(path, "tw-matrix")
  expect_false(grepl("shiny-plot-output", path))
})

test_that("revisit with the same signature does not retrieve again", {
  targets <- rbind(
    confirmed_reactome_target("t1", "EGFR", "P00533"),
    confirmed_reactome_target("t2", "KRAS", "P01116")
  )
  sig <- pathway_signature(targets)
  expect_false(should_retrieve_pathways(TRUE, targets, sig))
  expect_true(should_retrieve_pathways(TRUE, targets, NA_character_))
  expect_false(should_retrieve_pathways(FALSE, targets, NA_character_))

  project <- data.frame(
    id = "p1",
    disease_label = "NSCLC",
    disease_name = "non-small cell lung carcinoma",
    disease_ontology_id = "MONDO_0005233",
    disease_resolution_status = "confirmed",
    stringsAsFactors = FALSE
  )
  lit_sig <- literature_signature(project, targets)
  expect_false(should_retrieve_literature(TRUE, project, targets, lit_sig))
  expect_false(should_retrieve_literature(FALSE, project, targets, NA_character_))

  expect_false(should_retrieve_structures(TRUE, targets, structure_signature(targets)))
  expect_false(should_retrieve_comparison(
    TRUE,
    project,
    targets,
    comparison_signature(project, targets$id)
  ))
})

test_that("pathways retrieve is not repeated when the panel is hidden and shown again", {
  skip_if_not_installed("shiny")
  db_pool <- skip_if_no_postgres()
  on.exit(try(pool::poolClose(db_pool), silent = TRUE), add = TRUE)
  ensure_schema(db_pool)
  seed <- seed_snapshot_project(db_pool)
  overlap <- production_like_export_snapshot()$pathways$model
  calls <- 0L
  active <- shiny::reactiveVal(FALSE)

  shiny::testServer(
    mod_pathways_server,
    args = list(
      db_pool = db_pool,
      user = shiny::reactive(list(id = seed$owner$id)),
      project_id = shiny::reactive(seed$project_id),
      panel_active = shiny::reactive(active()),
      retrieve = function(...) {
        calls <<- calls + 1L
        list(status = "ready", pathways = overlap)
      }
    ),
    {
      session$flushReact()
      drain_tw_later(1)
      expect_equal(calls, 0L)
      active(TRUE)
      session$flushReact()
      drain_tw_later(1)
      first <- calls
      expect_gte(first, 1L)
      active(FALSE)
      session$flushReact()
      drain_tw_later(1)
      active(TRUE)
      session$flushReact()
      drain_tw_later(1)
      expect_equal(calls, first)
    }
  )
})

test_that("structure coverage plot data treats missing length as missing", {
  expect_null(structure_coverage_plot_data(list(list()), NULL))
  expect_null(structure_coverage_plot_data(list(list()), NA_integer_))
  expect_null(structure_coverage_plot_data(list(), 100L))
})
