## =============================================================================
## 04b_family_support.R -- family support as a protective resource
## -----------------------------------------------------------------------------
## Family support (F36-F38) is pre-specified in the coding scheme as a reliable,
## unidimensional scale analysed as a protective resource. It is validated as a
## latent factor in 03_measurement.R (four-factor CFA). This script puts it to
## analytic use in the way that strengthens the paper WITHOUT scattering it:
##
##   Panel A (MAIN, Table 5): family support (latent) -> the three latent
##     outcomes, adjusted for childhood exposure and the standard covariate
##     block, full sample, WLSMV SEM. A robust ADJUSTED MAIN EFFECT: is family
##     support associated with better mental-health / educational outcomes over
##     and above adversity? This is well powered and complements the adversity
##     story with a resource/resilience angle.
##
##   Panel B (EXPLORATORY, Table S7): the exposure x family-support interaction
##     on the same three outcomes -- does the protective association differ by
##     context? Reported in the SUPPLEMENT with an explicit power caveat: a
##     convenience university sample is not designed to detect interactions, so
##     these terms are hypothesis-generating, not confirmatory. The family-
##     support score is entered as an observed, standardised composite here so
##     the group-by-support product terms are estimable.
##
## Produces:
##   Table 5    output/tables/Table5_family_support.csv        (main)
##   Table S7   output/tables/TableS7_family_support_moderation.csv (supplement)
##   output/family_support.rds
## =============================================================================

suppressWarnings(suppressMessages(library(lavaan)))

d    <- readRDS(file.path(PATH$out, "analysis_data.rds"))
covs <- COV_FORMULA
IND_ORD <- c(paste0("ptss", 1:4), paste0("anx", 1:3),
             "edu_closures", "edu_missed", "edu_distress")
lat_lab <- c(ptss = "Post-traumatic stress", anx = "Anxiety",
             edis = "Educational disruption")

## ---- Panel A (Table 5): adjusted main effect, family support -> outcomes -----
## Family support enters as a latent factor (fam1-3 ordinal), so its three items
## are added to the ordered set.
MODEL_FS <- paste(c(
  "ptss =~ ptss1 + ptss2 + ptss3 + ptss4",
  "anx  =~ anx1  + anx2  + anx3",
  "edis =~ edu_closures + edu_missed + edu_distress",
  "fsup =~ fam1 + fam2 + fam3",
  paste("ptss ~ fsup + exp_fcs + exp_p48 + exp_opt +", covs),
  paste("anx  ~ fsup + exp_fcs + exp_p48 + exp_opt +", covs),
  paste("edis ~ fsup + exp_fcs + exp_p48 + exp_opt +", covs),
  "ptss ~~ anx", "ptss ~~ edis", "anx ~~ edis"), collapse = "\n")

fit_fs <- lavaan::sem(MODEL_FS, data = d,
                      ordered = c(IND_ORD, paste0("fam", 1:3)),
                      estimator = "WLSMV", std.lv = FALSE)

sfs <- lavaan::standardizedSolution(fit_fs)                 # fully standardised
sfs <- sfs[sfs$op == "~" & sfs$rhs == "fsup", ]
Table5 <- data.frame(
  Outcome   = unname(lat_lab[sfs$lhs]),
  Predictor = "Family support (latent), per 1 SD",
  Estimate  = "b (outcome SD)", n = nrow(d),
  Value     = paste0(fmt_ci(sfs$est.std, sfs$ci.lower, sfs$ci.upper), stars(sfs$pvalue)),
  p         = fmt_p(sfs$pvalue), check.names = FALSE)
write.csv(Table5, file.path(PATH$tables, "Table5_family_support.csv"), row.names = FALSE)

## ---- Panel B (Table S7): exposure x family-support interaction (exploratory) -
## Observed, standardised family-support composite so the product terms are
## estimable; latent outcomes as before.
d$fsup_z  <- as.numeric(scale(d$fam_support))
d$fsz_fcs <- d$exp_fcs * d$fsup_z
d$fsz_p48 <- d$exp_p48 * d$fsup_z
d$fsz_opt <- d$exp_opt * d$fsup_z
INT <- c("fsz_fcs", "fsz_p48", "fsz_opt")
int_lab <- c(fsz_fcs = "FCS x family support",
             fsz_p48 = "Palestinian '48 x family support",
             fsz_opt = "oPt x family support")

reg_mod <- function(y)
  paste(y, "~ fsup_z + exp_fcs + exp_p48 + exp_opt +",
        paste(INT, collapse = " + "), "+", covs)
MODEL_MOD <- paste(c(
  "ptss =~ ptss1 + ptss2 + ptss3 + ptss4",
  "anx  =~ anx1  + anx2  + anx3",
  "edis =~ edu_closures + edu_missed + edu_distress",
  reg_mod("ptss"), reg_mod("anx"), reg_mod("edis"),
  "ptss ~~ anx", "ptss ~~ edis", "anx ~~ edis"), collapse = "\n")

fit_mod <- lavaan::sem(MODEL_MOD, data = d, ordered = IND_ORD,
                       estimator = "WLSMV", std.lv = FALSE)
smod <- lavaan::standardizedSolution(fit_mod)
smod <- smod[smod$op == "~" & smod$rhs %in% INT, ]
TableS7 <- data.frame(
  Outcome     = unname(lat_lab[smod$lhs]),
  Interaction = unname(int_lab[smod$rhs]),
  `b (outcome SD) [95% CI]` = paste0(
    fmt_ci(smod$est.std, smod$ci.lower, smod$ci.upper), stars(smod$pvalue)),
  p = fmt_p(smod$pvalue), check.names = FALSE)
write.csv(TableS7, file.path(PATH$tables, "TableS7_family_support_moderation.csv"),
          row.names = FALSE)

saveRDS(list(fit_fs = fit_fs, fit_mod = fit_mod, Table5 = Table5, TableS7 = TableS7),
        file.path(PATH$out, "family_support.rds"))

message("04b_family_support.R done: Table 5 (main) and Table S7 (exploratory) written.")
