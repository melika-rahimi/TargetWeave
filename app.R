# TargetWeave — authenticated workspace through notes, snapshots, and production hardening (Milestone 11).

library(shiny)
library(bslib)

source("R/config.R")
source("R/ops/log.R")
source("R/ops/limits.R")
source("R/ops/health.R")
source("R/ops/async.R")
source("R/db/pool.R")
source("R/db/schema.R")
source("R/db/migrations.R")
source("R/db/users.R")
source("R/db/profiles.R")
source("R/db/support.R")
source("R/db/events.R")
source("R/db/projects.R")
source("R/db/targets.R")
source("R/db/authz.R")
source("R/db/cache.R")
source("R/db/snapshots.R")
source("R/db/notes.R")
source("R/process/ensembl_ids.R")
source("R/api/http_client.R")
source("R/api/api_uniprot.R")
source("R/api/api_ensembl.R")
source("R/api/api_opentargets.R")
source("R/api/api_reactome.R")
source("R/api/api_ncbi.R")
source("R/api/api_rcsb.R")
source("R/process/resolve_targets.R")
source("R/process/process_overview.R")
source("R/process/process_disease.R")
source("R/process/process_ot_evidence.R")
source("R/process/process_invalidation.R")
source("R/process/process_ot_comparison.R")
source("R/process/process_reactome.R")
source("R/process/process_pathway_overlap.R")
source("R/process/process_literature.R")
source("R/process/process_structure_coverage.R")
source("R/process/process_structures.R")
source("R/process/process_snapshots.R")
source("R/export/export_tables.R")
source("R/export/export_manifest.R")
source("R/export/export_dossier.R")
source("R/process/workspace_nav.R")
source("R/viz/viz_theme.R")
source("R/viz/viz_genomic.R")
source("R/viz/viz_ot_evidence.R")
source("R/viz/viz_ot_comparison.R")
source("R/viz/viz_pathways.R")
source("R/viz/viz_literature.R")
source("R/viz/viz_structures.R")
source("R/export/export_lite_viz.R")
source("R/ui/ui_components.R")
source("R/ui/ui_evidence.R")
source("R/ui/ui_landing.R")
source("R/ui/ui_tour.R")
source("R/modules/mod_auth.R")
source("R/modules/mod_onboarding.R")
source("R/modules/mod_project_list.R")
source("R/modules/mod_project_setup.R")
source("R/modules/mod_target_resolver.R")
source("R/modules/mod_overview.R")
source("R/modules/mod_disease_resolver.R")
source("R/modules/mod_disease_evidence.R")
source("R/modules/mod_ot_comparison.R")
source("R/modules/mod_pathways.R")
source("R/modules/mod_literature.R")
source("R/modules/mod_structures.R")
source("R/modules/mod_notes.R")
source("R/modules/mod_research.R")
source("R/modules/mod_project_home.R")
source("R/modules/mod_account.R")
source("R/ui.R")
source("R/server.R")

config <- get_app_config()
if (identical(config$env, "production")) {
  options(shiny.sanitize.errors = TRUE)
}

tw_log("startup", env = config$env)

db_pool <- tryCatch(
  create_db_pool(),
  error = function(e) {
    tw_log("db_connect_failed", level = "error", error = conditionMessage(e))
    stop(
      paste(
        "PostgreSQL is unavailable. TargetWeave will not start in a half-working state.",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  }
)

tryCatch(
  ensure_schema(db_pool),
  error = function(e) {
    tw_log("schema_failed", level = "error", error = conditionMessage(e))
    stop(e)
  }
)

onStop(function() {
  tw_log("shutdown")
  try(pool::poolClose(db_pool), silent = TRUE)
})

shinyApp(
  ui = function(req) {
    if (is_healthz_request(req)) {
      return(app_health_http(req, db_pool))
    }
    build_app_ui()
  },
  server = function(input, output, session) {
    app_server(input, output, session, db_pool)
  },
  uiPattern = HEALTHZ_UI_PATTERN
)
