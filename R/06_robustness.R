## =============================================================================
## 06_robustness.R -- leave one country out (Table S5)
## -----------------------------------------------------------------------------
## Exposure is assigned by childhood country, so the single robustness question
## that matters for this design is whether a comparison baseline is the product
## of one country. A country random effect or country-clustered SE is NOT
## estimable for the Palestinian arms (one country each), so the estimable
## response is to drop each country in the two MULTI-country strata (reference
## and FCS) in turn and refit.
##
## Two estimands are tracked -- the two on which the pre-specified primary claim
## rests, and the two a reader is most likely to interrogate:
##   E1  educational disruption, oPt vs FCS   (outcome SD, from Model 1)
##   E2  structural attribution, oPt vs FCS   (OR, from Model 3)
##
## Produces: Table S5.
## =============================================================================

suppressWarnings(suppressMessages(library(lavaan)))

d    <- readRDS(file.path(PATH$out, "analysis_data.rds"))
covs <- COV_FORMULA

## Countries with n >= 15 in the two multi-country strata (ref, fcs).
elig <- d[d$exposure %in% c("ref", "fcs"), ]
cc   <- as.data.frame(table(elig$country))
drop_countries <- as.character(cc$Var1[cc$Freq >= 15])

## --- refit helpers -----------------------------------------------------------
edis_optfcs <- function(dat) {
  MODEL <- paste(c(
    "ptss =~ ptss1 + ptss2 + ptss3 + ptss4",
    "anx  =~ anx1  + anx2  + anx3",
    "edis =~ edu_closures + edu_missed + edu_distress",
    paste("ptss ~ b_ptss_fcs*exp_fcs + b_ptss_p48*exp_p48 + b_ptss_opt*exp_opt +", covs),
    paste("anx  ~ b_anx_fcs*exp_fcs + b_anx_p48*exp_p48 + b_anx_opt*exp_opt +", covs),
    paste("edis ~ b_edis_fcs*exp_fcs + b_edis_p48*exp_p48 + b_edis_opt*exp_opt +", covs),
    "ptss ~~ anx", "ptss ~~ edis", "anx ~~ edis",
    "B_edis_optfcs := b_edis_opt - b_edis_fcs"), collapse = "\n")
  f <- lavaan::sem(MODEL, data = dat,
                   ordered = c(paste0("ptss", 1:4), paste0("anx", 1:3),
                               "edu_closures", "edu_missed", "edu_distress"),
                   estimator = "WLSMV", std.lv = FALSE)
  pe  <- lavaan::parameterEstimates(f)
  pes <- lavaan::standardizedSolution(f, type = "std.nox")
  k   <- pes$est.std[pes$lhs == "edis" & pes$op == "~" & pes$rhs == "exp_opt"] /
         pe$est[pe$lhs == "edis" & pe$op == "~" & pe$rhs == "exp_opt"]
  r <- pe[pe$lhs == "B_edis_optfcs" & pe$op == ":=", ]
  fmt_ci(r$est * k, r$ci.lower * k, r$ci.upper * k)
}
struc_optfcs <- function(dat) {
  m <- stats::glm(stats::as.formula(paste("barrier_structural ~ exp_fcs + exp_p48 + exp_opt +", covs)),
                  data = dat[!is.na(dat$barrier_structural), ], family = stats::binomial())
  r <- lin_or(stats::coef(m), stats::vcov(m), c(exp_fcs = -1, exp_opt = 1))
  fmt_or(r$or, r$lo, r$hi)
}

## --- primary row + one row per dropped country -------------------------------
rows <- list(data.frame(
  Specification = "Primary specification (all countries)", n = nrow(d),
  `Educational disruption, oPt vs FCS (outcome SD)` = edis_optfcs(d),
  `Structural attribution, oPt vs FCS (OR)` = struc_optfcs(d), check.names = FALSE))

for (ctry in drop_countries) {
  dd <- d[d$country != ctry, ]
  stratum <- ifelse(ctry %in% c("Yemen", "Lebanon", "Syria", "Libya", "Sudan", "Iraq"),
                    "FCS stratum", "reference stratum")
  rows[[length(rows) + 1]] <- data.frame(
    Specification = sprintf("Exclude %s (n = %d, %s)", ctry, sum(d$country == ctry), stratum),
    n = nrow(dd),
    `Educational disruption, oPt vs FCS (outcome SD)` = edis_optfcs(dd),
    `Structural attribution, oPt vs FCS (OR)` = struc_optfcs(dd), check.names = FALSE)
}
TableS5 <- do.call(rbind, rows)
write.csv(TableS5, file.path(PATH$tables, "TableS5_leave_one_country_out.csv"), row.names = FALSE)

message("06_robustness.R done: Table S5 written (", length(drop_countries), " countries dropped in turn).")
