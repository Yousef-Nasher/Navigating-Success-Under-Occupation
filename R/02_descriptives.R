## =============================================================================
## 02_descriptives.R -- sample description
## -----------------------------------------------------------------------------
## Produces:
##   Table 1  Participant characteristics by exposure group
##   Table S1 Between-group differences as effect sizes (not significance tests)
##   Table S3 Country -> FCS classification (Panel A) and composition (Panel B)
##   Table S6 Descriptive occupation outlook among Palestinians
##
## Because a sample this large makes trivial differences significant, groups are
## compared through effect sizes (Hedges' g; risk differences), not p-values.
## =============================================================================

d <- readRDS(file.path(PATH$out, "analysis_data.rds"))

## ---- Effect-size helpers ----------------------------------------------------
hedges_g <- function(x1, x0) {
  x1 <- x1[!is.na(x1)]; x0 <- x0[!is.na(x0)]
  n1 <- length(x1); n0 <- length(x0)
  sp <- sqrt(((n1 - 1) * var(x1) + (n0 - 1) * var(x0)) / (n1 + n0 - 2))
  d  <- (mean(x1) - mean(x0)) / sp
  J  <- 1 - 3 / (4 * (n1 + n0) - 9)               # small-sample correction
  g  <- d * J
  se <- sqrt((n1 + n0) / (n1 * n0) + g^2 / (2 * (n1 + n0)))
  c(g = g, lo = g - 1.96 * se, hi = g + 1.96 * se)
}
risk_diff_pp <- function(b1, b0) {                # b* are 0/1 vectors
  b1 <- b1[!is.na(b1)]; b0 <- b0[!is.na(b0)]
  p1 <- mean(b1); p0 <- mean(b0); n1 <- length(b1); n0 <- length(b0)
  se <- sqrt(p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0)
  100 * c(diff = p1 - p0, lo = (p1 - p0) - 1.96 * se, hi = (p1 - p0) + 1.96 * se)
}

## ---- Table 1 ----------------------------------------------------------------
grp <- function(g) d[d$exposure == g, ]
pct <- function(x) sprintf("%d (%.1f)", sum(x, na.rm = TRUE), 100 * mean(x, na.rm = TRUE))
msd <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
msd2 <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))

t1_rows <- function(g) {
  s <- grp(g)
  c(
    n            = nrow(s),
    `Age M (SD)` = msd(s$age),
    `Women n (%)`         = pct(s$sex == 1),
    `Low SES n (%)`       = pct(s$ses_low == 1),
    `High SES n (%)`      = pct(s$ses_high == 1),
    `Refugee camp n (%)`  = pct(s$area_camp == 1),
    `University n (%)`     = pct(s$edu_level == "university"),
    `Migrated n (%)`      = pct(s$migrated == 1),
    `Family support M (SD)` = msd2(s$fam_support)
  )
}
Table1 <- data.frame(Characteristic = names(t1_rows("ref")),
                     ref = t1_rows("ref"), fcs = t1_rows("fcs"),
                     p48 = t1_rows("p48"), opt = t1_rows("opt"),
                     check.names = FALSE, row.names = NULL)
names(Table1)[-1] <- EXP_LABELS[c("ref", "fcs", "p48", "opt")]
write.csv(Table1, file.path(PATH$tables, "Table1_characteristics.csv"), row.names = FALSE)

## ---- Table S1 (effect sizes vs reference) -----------------------------------
ref <- grp("ref")
s1 <- list()
for (g in c("fcs", "p48", "opt")) {
  s <- grp(g); lab <- EXP_LABELS[[g]]
  add <- function(char, metric, v, d = 2)
    data.frame(Comparison = paste(lab, "vs reference"), Characteristic = char,
               Metric = metric, Value = fmt_ci(v[1], v[2], v[3], d))
  s1[[g]] <- rbind(
    add("Age (years)",       "Hedges' g",     hedges_g(s$age, ref$age)),
    add("Family support (1-5)", "Hedges' g",  hedges_g(s$fam_support, ref$fam_support)),
    add("Women",             "Difference (pp)", risk_diff_pp(s$sex == 1, ref$sex == 1), 1),
    add("Low childhood SES", "Difference (pp)", risk_diff_pp(s$ses_low, ref$ses_low), 1),
    add("High childhood SES","Difference (pp)", risk_diff_pp(s$ses_high, ref$ses_high), 1),
    add("Refugee-camp upbringing", "Difference (pp)", risk_diff_pp(s$area_camp, ref$area_camp), 1),
    add("Moved country since childhood", "Difference (pp)", risk_diff_pp(s$migrated, ref$migrated), 1)
  )
}
TableS1 <- do.call(rbind, s1); rownames(TableS1) <- NULL
write.csv(TableS1, file.path(PATH$tables, "TableS1_effect_sizes.csv"), row.names = FALSE)

## ---- Table S3 Panel A: country -> FCS classification ------------------------
## FCS FY26 category (World Bank FY26 list), matched by keyword so the cleaned
## country labels resolve correctly.
cat_of <- function(cn, exp) {
  if (grepl("West Bank|Gaza", cn))                 return("Conflict (West Bank and Gaza)")
  if (grepl("Yemen|Syria|Sudan|Iraq|Lebanon", cn)) return("Conflict")
  if (grepl("Libya", cn))                          return("Institutional and social fragility")
  if (grepl("Occupied Palestine", cn))             return("Not FCS-listed (occupation context)")
  if (exp == "ref")                                return("Not listed")
  "n/a"
}
level_of <- c(ref = "0 Not listed (reference)", fcs = "1 FCS, not under occupation",
              p48 = "2 Palestinian citizens of Israel", opt = "3 Occupied Palestinian territory")
comp <- aggregate(row_id ~ country + exposure, d, length)
names(comp)[3] <- "n"
comp <- comp[order(comp$exposure, -comp$n), ]
comp$exposure_level <- level_of[as.character(comp$exposure)]
comp$FCS_category <- mapply(cat_of, comp$country, as.character(comp$exposure))
TableS3a <- comp[, c("country", "exposure_level", "FCS_category", "n")]
write.csv(TableS3a, file.path(PATH$tables, "TableS3a_country_classification.csv"), row.names = FALSE)

## ---- Table S3 Panel B: composition of each exposure level -------------------
comp$level_total <- ave(comp$n, comp$exposure, FUN = sum)
comp$pct_of_level <- round(100 * comp$n / comp$level_total, 1)
TableS3b <- comp[, c("exposure_level", "country", "n", "pct_of_level")]
write.csv(TableS3b, file.path(PATH$tables, "TableS3b_composition.csv"), row.names = FALSE)

## ---- Table S6: descriptive occupation outlook (Palestinian subsample) -------
sub <- d[d$under_occupation == 1, ]
sub$grp <- factor(sub$exposure, levels = c("p48", "opt"),
                  labels = c(EXP_LABELS[["p48"]], EXP_LABELS[["opt"]]))
outlook <- as.data.frame(table(sub$grp, sub$occ_outlook))
names(outlook) <- c("Palestinian group", "Response", "n")
outlook$pct <- round(100 * outlook$n /
                       ave(outlook$n, outlook$`Palestinian group`, FUN = sum), 1)
write.csv(outlook, file.path(PATH$tables, "TableS6_occupation_outlook.csv"), row.names = FALSE)

## Note: F44 ("what could help ...") is a select-all item; it is tabulated
## descriptively in Table S8 (Panel F) by 02b_supp_descriptives.R.

message("02_descriptives.R done: Table 1, S1, S3 (A/B) and S6 written.")
