## =============================================================================
## 05_within_palestinian.R -- individual conflict exposure within Palestinians
## -----------------------------------------------------------------------------
## Restricted to the n = 809 Palestinian participants who did not select "I do
## not live under occupation" (624 oPt, 185 '48).
##
## This is the study's SECOND, INDEPENDENT line of evidence. The between-group
## models in 04 assign exposure by country, so every country-level characteristic
## is confounded with the exposure. This section varies conflict experience
## BETWEEN INDIVIDUALS reached through the same channels within the same contexts,
## so no country-level or recruitment-level factor can generate the associations.
## Its own limitation is the mirror image: it cannot speak to what varies between
## contexts rather than between individuals.
##
##   Panel A  conflict-experience index -> all six outcomes
##            (three latent outcomes per 1 SD via SEM; three occupation-specific
##             outlook outcomes per 1 point via ordinal logistic)
##   Panel B  the three conflict items, MUTUALLY ADJUSTED, -> the three outlook
##            outcomes (proportional-odds). This is where the specific driver
##            (being prevented from attending school/work) is identified.
##
## The index is FORMATIVE (its items are causes, not interchangeable
## indicators), so inference rests on the mutually adjusted model in Panel B,
## not on the internal consistency of the summed score (Bollen & Bauldry, 2011).
##
## Produces:
##   Table 4  Panels A and B
##   output/within_palestinian.rds  (Panel B estimates reused by 07_figures.R)
## =============================================================================

suppressWarnings(suppressMessages({library(lavaan); library(MASS)}))

d   <- readRDS(file.path(PATH$out, "analysis_data.rds"))
dp <- d[d$under_occupation == 1, ]
assert(nrow(dp) == 809, "within-Palestinian subsample is not 809")

## Standardise the conflict index within the analysed subsample (per 1 SD).
dp$conflict_index_z <- as.numeric(scale(dp$conflict_index))
covs <- COV_FORMULA

CONF_ITEMS  <- c("conf_displace", "conf_prevented", "conf_safeexpr")
CONF_LABELS <- c(conf_displace  = "Displacement, raids or arrests",
                 conf_prevented = "Prevented from attending school or work",
                 conf_safeexpr  = "Does not feel safe expressing political views")
OUT_ORD <- c(lim_career_o = "Perceived career/educational limitation",
             mig_intent_o = "Migration intention",
             hope_o       = "Hope for the future")

## ---- Panel A (i): index -> three latent outcomes (SEM, per 1 SD) ------------
MODEL5 <- paste(c(
  "ptss =~ ptss1 + ptss2 + ptss3 + ptss4",
  "anx  =~ anx1  + anx2  + anx3",
  "edis =~ edu_closures + edu_missed + edu_distress",
  paste("ptss ~ conflict_index_z +", covs),
  paste("anx  ~ conflict_index_z +", covs),
  paste("edis ~ conflict_index_z +", covs),
  "ptss ~~ anx", "ptss ~~ edis", "anx ~~ edis"), collapse = "\n")

fit5 <- lavaan::sem(MODEL5, data = dp,
                    ordered = c(paste0("ptss", 1:4), paste0("anx", 1:3),
                                "edu_closures", "edu_missed", "edu_distress"),
                    estimator = "WLSMV", std.lv = FALSE)
s5 <- lavaan::standardizedSolution(fit5, type = "std.nox")
s5 <- s5[s5$op == "~" & s5$rhs == "conflict_index_z", ]
lat_lab <- c(ptss = "Post-traumatic stress", anx = "Anxiety", edis = "Educational disruption")
panelA_latent <- data.frame(
  Outcome = unname(lat_lab[s5$lhs]), Predictor = "Conflict index, per 1 SD",
  Estimate = "b (outcome SD)", n = 809,
  Value = fmt_ci(s5$est.std, s5$ci.lower, s5$ci.upper),
  p = fmt_p(s5$pvalue), check.names = FALSE)

## ---- Panel A (ii): index -> three outlook outcomes (ordinal, per 1 point) ---
polr_or <- function(term, m) {
  b  <- stats::coef(m)[term]
  se <- sqrt(diag(stats::vcov(m))[term])
  data.frame(or = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
             p = 2 * stats::pnorm(-abs(b / se)))
}
panelA_ord <- do.call(rbind, lapply(names(OUT_ORD), function(y) {
  raw_col <- sub("_o$", "", y)      # ordered-factor name -> raw column for n
  m <- MASS::polr(stats::as.formula(paste(y, "~ conflict_index +", covs)),
                  data = dp, Hess = TRUE)
  r <- polr_or("conflict_index", m)
  data.frame(Outcome = OUT_ORD[[y]], Predictor = "Conflict index, per 1 point (0-6)",
             Estimate = "OR", n = sum(!is.na(dp[[raw_col]])),
             Value = fmt_or(r$or, r$lo, r$hi), p = fmt_p(r$p), check.names = FALSE)
}))

Table4A <- rbind(panelA_latent, panelA_ord)
write.csv(Table4A, file.path(PATH$tables, "Table4A_index.csv"), row.names = FALSE)

## ---- Panel B: three items mutually adjusted -> three outlook outcomes -------
panelB <- do.call(rbind, lapply(names(OUT_ORD), function(y) {
  m <- MASS::polr(stats::as.formula(paste(y, "~", paste(CONF_ITEMS, collapse = " + "),
                                          "+", covs)), data = dp, Hess = TRUE)
  n_y <- sum(!is.na(dp[[sub("_o$", "", y)]]))
  do.call(rbind, lapply(CONF_ITEMS, function(it) {
    r <- polr_or(it, m)
    data.frame(Outcome = OUT_ORD[[y]], n = n_y,
               `Conflict experience` = CONF_LABELS[[it]],
               OR = fmt_or(r$or, r$lo, r$hi), p = fmt_p(r$p),
               est = r$or, lo = r$lo, hi = r$hi, item = it,
               check.names = FALSE)
  }))
}))
Table4B <- panelB[, c("Outcome", "n", "Conflict experience", "OR", "p")]
write.csv(Table4B, file.path(PATH$tables, "Table4B_items_mutually_adjusted.csv"), row.names = FALSE)

saveRDS(list(panelB = panelB), file.path(PATH$out, "within_palestinian.rds"))

message("05_within_palestinian.R done: Table 4 (A and B) written (n = 809).")
