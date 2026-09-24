## =============================================================================
## run_all.R -- reproduce every table and figure from the raw data
## -----------------------------------------------------------------------------
## Open navigating-success-under-occupation.Rproj first (so the working directory
## is the project root), then either source this file or run it line by line.
## Scripts 03-06 fit WLSMV SEMs and can take a few minutes in total.
##
##   source("run_all.R")
##
## Outputs land in output/ (tables in output/tables, figures in output/figures,
## the compiled Word file at output/Results_Compiled.docx).
## =============================================================================
t0 <- Sys.time()
steps <- c(
  "R/00_setup.R",            # packages, paths, helpers, contrast set
  "R/01_clean.R",            # raw -> analysis_data.rds, flow, CHERRIES
  "R/02_descriptives.R",     # Table 1, S1, S3, S6
  "R/02b_supp_descriptives.R",# Table S8 a-f (Palestinian descriptive supplement)
  "R/03_measurement.R",      # Table 2, S2  (four-factor CFA incl. family support)
  "R/04_primary_models.R",   # Table 3, S4  (core)
  "R/04b_family_support.R",  # Table 5, S7  (family support: main + moderation)
  "R/05_within_palestinian.R",# Table 4
  "R/06_robustness.R",       # Table S5
  "R/07_figures.R",          # Figures 1-3
  "R/08_export_word.R"       # Results_Compiled.docx
)

for (s in steps) {
  message("\n=====================================================")
  message("RUNNING: ", s)
  message("=====================================================")
  source(s, echo = FALSE)
}

message("\nAll steps complete in ",
        round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min.")
message("See output/ for tables, figures, and Results_Compiled.docx.")
