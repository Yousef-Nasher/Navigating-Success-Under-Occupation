## =============================================================================
## 03_measurement.R -- measurement model for the analysed outcome constructs
## -----------------------------------------------------------------------------
## Four correlated latent factors, all indicators ordinal, WLSMV:
##   ptss  post-traumatic stress   -- 4 items adapted from the PCL-5 (of 20)
##   anx   anxiety                 -- 3 items adapted from the GAD-7 (of 7)
##   edis  educational disruption  -- 3 author-developed items
##   fsup  family support          -- 3 items (1-5 agreement)
## These are exactly the constructs that enter the models in scripts 04-06
## (family support is the protective resource analysed in 04b), so the
## measurement model validates precisely what is analysed.
##
## WHY family support IS modelled but coping is NOT:
##   - The coding scheme pre-specifies family support (F36-F38) as a reliable,
##     unidimensional scale analysed as a protective resource; its three items are
##     interchangeable indicators of one construct (being listened to, helped,
##     and cared for), so a reflective factor is well specified. It now enters
##     the analysis (04b), so validating it here is exactly on-target.
##   - The three coping items are drawn from THREE DIFFERENT Brief COPE facets
##     (support-seeking, self-distraction, relaxation). They are distinct
##     strategies, not interchangeable indicators of one trait, so a reflective
##     latent "coping" factor is mis-specified. Modelling it would invite a
##     legitimate psychometric objection for no analytic gain, so coping is reported
##     descriptively only (not adjusted for) and is deliberately excluded here.
##
## The PTSS and anxiety item sets are ABBREVIATED, RESCALED adaptations of the
## PCL-5 and GAD-7 -- not the full instruments and not diagnostic. The CFA,
## reliability, and invariance here validate the item sets AS USED IN THIS
## SAMPLE; they do not license clinical cut-offs or caseness, which are never
## computed anywhere in the pipeline.
##
## Produces:
##   Table 2   Panel A CFA fit; Panel B reliability, validity + item provenance
##   Table S2  Panel A loadings; Panel B discriminant validity (Fornell-Larcker);
##             Panel C measurement invariance across the four exposure groups
##   output/measurement_fit.rds  (objects reused by later scripts if needed)
## =============================================================================

suppressWarnings(suppressMessages(library(lavaan)))

d <- readRDS(file.path(PATH$out, "analysis_data.rds"))
## Ordered indicators = the four modelled factors (family support included;
## coping deliberately excluded -- see header).
ORD <- c(paste0("ptss", 1:4), paste0("anx", 1:3),
         "edu_closures", "edu_missed", "edu_distress",
         paste0("fam", 1:3))

MODEL_CFA <- '
  ptss =~ ptss1 + ptss2 + ptss3 + ptss4
  anx  =~ anx1  + anx2  + anx3
  edis =~ edu_closures + edu_missed + edu_distress
  fsup =~ fam1 + fam2 + fam3
'

fit <- lavaan::cfa(MODEL_CFA, data = d, ordered = ORD, estimator = "WLSMV",
                   std.lv = FALSE)

## ---- Table 2 Panel A: CFA fit (scaled WLSMV indices) ------------------------
fm <- lavaan::fitMeasures(fit, c("cfi.scaled", "tli.scaled", "rmsea.scaled",
                                 "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
                                 "srmr"))
Table2a <- data.frame(
  Sample = "Full sample", n = nrow(d),
  CFI  = sprintf("%.3f", fm["cfi.scaled"]),
  TLI  = sprintf("%.3f", fm["tli.scaled"]),
  `RMSEA [90% CI]` = sprintf("%.3f [%.3f, %.3f]", fm["rmsea.scaled"],
                             fm["rmsea.ci.lower.scaled"], fm["rmsea.ci.upper.scaled"]),
  SRMR = sprintf("%.3f", fm["srmr"]),
  check.names = FALSE)
write.csv(Table2a, file.path(PATH$tables, "Table2a_cfa_fit.csv"), row.names = FALSE)

## ---- Reliability and convergent validity -----------------------------------
std <- lavaan::standardizedSolution(fit)
load_std <- std[std$op == "=~", c("lhs", "rhs", "est.std")]
factors  <- c(ptss = "Post-traumatic stress", anx = "Anxiety",
              edis = "Educational disruption", fsup = "Family support")
## Item provenance -- stated in-table so the abbreviation is never hidden.
source_of <- c(
  ptss = "Adapted from PCL-5 (4 of 20 items; 4-pt frequency, childhood-anchored)",
  anx  = "Adapted from GAD-7 (3 of 7 items; 3-pt scale)",
  edis = "Author-developed (3 items)",
  fsup = "Family support scale (3 items; 1-5 agreement)")

rel_row <- function(f) {
  lam <- load_std$est.std[load_std$lhs == f]
  omega <- sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2))
  ave   <- mean(lam^2)
  ## Cronbach's alpha on the raw ordinal items (standardised alpha).
  items <- load_std$rhs[load_std$lhs == f]
  a <- suppressWarnings(psych::alpha(d[, items], warnings = FALSE, check.keys = FALSE))
  data.frame(
    Factor = factors[[f]],
    `Source (items used)` = source_of[[f]],
    Items = length(lam),
    `Loading range` = sprintf("%.2f to %.2f", min(lam), max(lam)),
    omega = sprintf("%.2f", omega),
    alpha = sprintf("%.2f", a$total$std.alpha),
    AVE   = sprintf("%.2f", ave), check.names = FALSE)
}
Table2b <- do.call(rbind, lapply(names(factors), rel_row))
write.csv(Table2b, file.path(PATH$tables, "Table2b_reliability.csv"), row.names = FALSE)

## ---- Table S2 Panel A: standardised loadings --------------------------------
loadCI <- std[std$op == "=~", c("lhs", "rhs", "est.std", "se", "ci.lower", "ci.upper")]
TableS2a <- data.frame(
  Factor = loadCI$lhs, Indicator = loadCI$rhs,
  `Loading [95% CI]` = sprintf("%.3f [%.3f, %.3f]", loadCI$est.std,
                               loadCI$ci.lower, loadCI$ci.upper),
  SE = sprintf("%.3f", loadCI$se), check.names = FALSE)
write.csv(TableS2a, file.path(PATH$tables, "TableS2a_loadings.csv"), row.names = FALSE)

## ---- Table S2 Panel B: discriminant validity (Fornell-Larcker) --------------
cor_lv <- lavaan::lavInspect(fit, "cor.lv")
ave_v  <- vapply(names(factors), function(f)
  mean(load_std$est.std[load_std$lhs == f]^2), numeric(1))
FL <- cor_lv
diag(FL) <- sqrt(ave_v[rownames(FL)])
TableS2b <- data.frame(factor = rownames(FL), round(FL, 3), check.names = FALSE)
write.csv(TableS2b, file.path(PATH$tables, "TableS2b_discriminant.csv"), row.names = FALSE)

## ---- Table S2 Panel C: measurement invariance across exposure groups --------
## Judged by change in fit (Chen, 2007): dCFI <= .010 and dRMSEA <= .015.
inv_fit <- function(equal = NULL)
  lavaan::cfa(MODEL_CFA, data = d, ordered = ORD, estimator = "WLSMV",
              group = "exposure", group.equal = equal)

f_conf   <- inv_fit(NULL)
f_metric <- inv_fit("loadings")
f_scalar <- inv_fit(c("loadings", "thresholds"))

grab <- function(f) lavaan::fitMeasures(f, c("cfi.scaled", "tli.scaled",
                                             "rmsea.scaled", "srmr"))
M <- rbind(configural = grab(f_conf), metric = grab(f_metric), scalar = grab(f_scalar))
TableS2c <- data.frame(
  model = rownames(M),
  CFI   = sprintf("%.3f", M[, "cfi.scaled"]),
  TLI   = sprintf("%.3f", M[, "tli.scaled"]),
  RMSEA = sprintf("%.3f", M[, "rmsea.scaled"]),
  SRMR  = sprintf("%.3f", M[, "srmr"]),
  dCFI  = c("", sprintf("%.3f", diff(M[, "cfi.scaled"]))),
  dRMSEA = c("", sprintf("%.3f", diff(M[, "rmsea.scaled"]))),
  passes = c("", ifelse(abs(diff(M[, "cfi.scaled"])) <= .010 &
                          abs(diff(M[, "rmsea.scaled"])) <= .015, "yes", "no")),
  check.names = FALSE)
write.csv(TableS2c, file.path(PATH$tables, "TableS2c_invariance.csv"), row.names = FALSE)

saveRDS(list(fit = fit, fit_measures = fm, invariance = M),
        file.path(PATH$out, "measurement_fit.rds"))

message("03_measurement.R done: Table 2 (A/B) and Table S2 (A/B/C) written.")
message(sprintf("  CFA scaled fit: CFI=%.3f TLI=%.3f RMSEA=%.3f SRMR=%.3f",
                fm["cfi.scaled"], fm["tli.scaled"], fm["rmsea.scaled"], fm["srmr"]))
