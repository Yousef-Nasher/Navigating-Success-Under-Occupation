## =============================================================================
## 04_primary_models.R -- childhood exposure and all outcome domains
## -----------------------------------------------------------------------------
## The paper's central analysis. Four models, one contrast set, one decomposition.
##
##   Model 1  three latent outcomes (ptss, anx, edis) ~ exposure + covariates,
##            WLSMV ordinal, correlated residuals. Reported in std.nox units
##            (latent outcome standardised, 0/1 dummies not), so each coefficient
##            is a difference in outcome SDs and every contrast is a genuine one.
##   Model 2  dream-major attainment           ~ exposure + covariates  (logistic)
##   Model 3  structural barrier attribution   ~ exposure + covariates  (logistic)
##   Model 4  political situation as a barrier ~ exposure + covariates  (logistic)
##
## THE CONTRAST SET (00_setup.R):
##   A  fcs/p48/oPt vs the non-FCS reference   (total association)
##   B  '48/oPt vs the FCS stratum             (net of a high-adversity benchmark)
##   C  oPt vs Palestinian '48                 (internal to the Palestinian population)
## A and B together decompose the Palestinian association; C compares the two
## Palestinian contexts. None is labelled primary at the level of the table --
## the pre-specified primary estimand (educational disruption and structural/
## political attribution against the FCS stratum) is a subset, interpreted in
## the manuscript.
##
## Produces:
##   Table 3   the full decomposition (tidy long form; 08 pivots to wide)
##   Table S4  full covariate coefficients for Models 1 and 2
##   output/primary_models.rds  (estimates reused by 07_figures.R)
## =============================================================================

suppressWarnings(suppressMessages(library(lavaan)))

d   <- readRDS(file.path(PATH$out, "analysis_data.rds"))
ORD <- attr(d, "ORD_IND")
covs <- COV_FORMULA

## ---- Model 1: latent SEM ----------------------------------------------------
lat_out <- c(ptss = "Post-traumatic stress", anx = "Anxiety",
             edis = "Educational disruption")

## Build the regression + labelled-coefficient + contrast syntax programmatically.
reg_line <- function(y)
  sprintf("%s ~ b_%s_fcs*exp_fcs + b_%s_p48*exp_p48 + b_%s_opt*exp_opt + %s",
          y, y, y, y, covs)
con_lines <- function(y) c(
  sprintf("B_%s_p48fcs := b_%s_p48 - b_%s_fcs", y, y, y),
  sprintf("B_%s_optfcs := b_%s_opt - b_%s_fcs", y, y, y),
  sprintf("C_%s_optp48 := b_%s_opt - b_%s_p48", y, y, y))

MODEL1 <- paste(c(
  "ptss =~ ptss1 + ptss2 + ptss3 + ptss4",
  "anx  =~ anx1  + anx2  + anx3",
  "edis =~ edu_closures + edu_missed + edu_distress",
  reg_line("ptss"), reg_line("anx"), reg_line("edis"),
  "ptss ~~ anx", "ptss ~~ edis", "anx ~~ edis",
  con_lines("ptss"), con_lines("anx"), con_lines("edis")
), collapse = "\n")

fit1 <- lavaan::sem(MODEL1, data = d,
                    ordered = c(paste0("ptss", 1:4), paste0("anx", 1:3),
                                "edu_closures", "edu_missed", "edu_distress"),
                    estimator = "WLSMV", std.lv = FALSE)

pe  <- lavaan::parameterEstimates(fit1)
pes <- lavaan::standardizedSolution(fit1, type = "std.nox")

## k_y: the constant that converts a coefficient for outcome y to std.nox units
## (= 1 / model-implied SD of the latent outcome). Recovered as the exact ratio
## of the std.nox to the unstandardised main effect (all share the same k_y).
k_of <- function(y) {
  u <- pe$est[pe$lhs == y & pe$op == "~" & pe$rhs == "exp_opt"]
  s <- pes$est.std[pes$lhs == y & pes$op == "~" & pes$rhs == "exp_opt"]
  s / u
}

## Assemble the six std.nox contrasts for one latent outcome.
latent_contrasts <- function(y) {
  k <- k_of(y)
  ## Main effect (block A): unstandardised row for a predictor, scaled by k.
  main <- function(rhs) {
    r <- pe[pe$lhs == y & pe$op == "~" & pe$rhs == rhs, ]
    data.frame(est = r$est * k, lo = r$ci.lower * k, hi = r$ci.upper * k, p = r$pvalue)
  }
  ## Defined contrast (blocks B, C): ":=" row matched by its label, scaled by k.
  defd <- function(label) {
    r <- pe[pe$lhs == label & pe$op == ":=", ]
    data.frame(est = r$est * k, lo = r$ci.lower * k, hi = r$ci.upper * k, p = r$pvalue)
  }
  rows <- list(
    fcs_ref = main("exp_fcs"),
    p48_ref = main("exp_p48"),
    opt_ref = main("exp_opt"),
    p48_fcs = defd(paste0("B_", y, "_p48fcs")),
    opt_fcs = defd(paste0("B_", y, "_optfcs")),
    opt_p48 = defd(paste0("C_", y, "_optp48")))
  do.call(rbind, lapply(names(rows), function(k2) data.frame(
    outcome = lat_out[[y]], metric = "b (outcome SD)",
    block = CONTRASTS[[k2]]$block, contrast = k2,
    value = paste0(fmt_ci(rows[[k2]]$est, rows[[k2]]$lo, rows[[k2]]$hi),
                   stars(rows[[k2]]$p)),
    p = rows[[k2]]$p, est = rows[[k2]]$est, lo = rows[[k2]]$lo, hi = rows[[k2]]$hi)))
}
latent_tbl <- do.call(rbind, lapply(names(lat_out), latent_contrasts))

## ---- Models 2-4: logistic outcomes -----------------------------------------
logit_contrasts <- function(outcome_var, data, outcome_label) {
  f <- stats::as.formula(paste(outcome_var, "~ exp_fcs + exp_p48 + exp_opt +", covs))
  m <- stats::glm(f, data = data, family = stats::binomial())
  b <- stats::coef(m); V <- stats::vcov(m)
  do.call(rbind, lapply(names(CONTRASTS), function(k) {
    w <- CONTRASTS[[k]]$w
    r <- lin_or(b, V, c(exp_fcs = unname(w["fcs"]),
                        exp_p48 = unname(w["p48"]),
                        exp_opt = unname(w["opt"])))
    data.frame(outcome = outcome_label, metric = "OR",
               block = CONTRASTS[[k]]$block, contrast = k,
               value = paste0(fmt_or(r$or, r$lo, r$hi), stars(r$p)),
               p = r$p, est = r$or, lo = r$lo, hi = r$hi)
  }))
}
dream_tbl <- logit_contrasts("dream_major",        d[!is.na(d$dream_major), ],        "Dream-major attainment")
struc_tbl <- logit_contrasts("barrier_structural", d[!is.na(d$barrier_structural), ], "Structural (vs internal/no) barrier attribution")
polit_tbl <- logit_contrasts("barrier_political",  d[!is.na(d$barrier_political), ],  "Names the political situation as a barrier")

## ---- Table 3 (tidy long form) ----------------------------------------------
Table3 <- rbind(latent_tbl, dream_tbl, struc_tbl, polit_tbl)
Table3$contrast_label <- vapply(Table3$contrast, function(k) CONTRASTS[[k]]$label, character(1))
write.csv(Table3, file.path(PATH$tables, "Table3_decomposition.csv"), row.names = FALSE)

## ---- Table S4: full covariate coefficients ---------------------------------
## Panel A: Model 1 latent outcomes, all predictors, std.nox.
s4a <- pes[pes$op == "~" & pes$lhs %in% names(lat_out), ]
pmap <- c(exp_fcs = "FCS MENA vs non-FCS MENA", exp_p48 = "Palestinian '48 vs non-FCS MENA",
          exp_opt = "oPt vs non-FCS MENA", age_c = "Age (years, centred)",
          sex = "Female (vs male)", ses_low = "Low childhood SES (vs middle)",
          ses_high = "High childhood SES (vs middle)", area_rural = "Rural upbringing (vs urban)",
          area_camp = "Refugee-camp upbringing (vs urban)", migrated = "Moved country since childhood")
TableS4a <- data.frame(
  Outcome  = unname(lat_out[s4a$lhs]),
  Predictor = unname(pmap[s4a$rhs]),
  `b (outcome SD) [95% CI]` = sprintf("%.3f [%.3f, %.3f]", s4a$est.std, s4a$ci.lower, s4a$ci.upper),
  p = fmt_p(s4a$pvalue), check.names = FALSE, row.names = NULL)
write.csv(TableS4a, file.path(PATH$tables, "TableS4a_covariates_model1.csv"), row.names = FALSE)

## Panel B: Model 2 dream-major, all predictors, OR.
m2 <- stats::glm(stats::as.formula(paste("dream_major ~ exp_fcs + exp_p48 + exp_opt +", covs)),
                 data = d[!is.na(d$dream_major), ], family = stats::binomial())
ci2  <- suppressMessages(stats::confint.default(m2))
cf   <- stats::coef(m2)
keep <- names(cf) != "(Intercept)" & names(cf) %in% names(pmap)   # drop intercept
TableS4b <- data.frame(
  Predictor = unname(pmap[names(cf)[keep]]),
  `OR [95% CI]` = sprintf("%.2f [%.2f, %.2f]", exp(cf[keep]),
                          exp(ci2[keep, 1]), exp(ci2[keep, 2])),
  p = fmt_p(summary(m2)$coefficients[keep, 4]),
  check.names = FALSE, row.names = NULL)
write.csv(TableS4b, file.path(PATH$tables, "TableS4b_covariates_model2.csv"), row.names = FALSE)

saveRDS(list(Table3 = Table3, fit1 = fit1), file.path(PATH$out, "primary_models.rds"))

message("04_primary_models.R done: Table 3 and Table S4 (A/B) written.")
