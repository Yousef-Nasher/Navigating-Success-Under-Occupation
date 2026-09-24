## =============================================================================
## 02b_supp_descriptives.R -- descriptive supplement, Palestinian sample only
## -----------------------------------------------------------------------------
## Purely descriptive tabulation of occupation-context items within the
## Palestinian sample (oPt + Palestinian '48). NO regression, NO causal claim --
## distributions and endorsement rates only, by Palestinian group and overall.
## Occupation-gated items (F17, F42) are naturally confined to participants who
## live under occupation (others are non-applicable / missing).
##
## Items:
##   F17  occupation salience / influence on thinking (Yes/No)      -> Table S8a
##   F42  perceived resilience following occupation (Yes/No)        -> Table S8a
##   F25  education discrimination/limitation (0-4)                 -> Table S8b
##   F26  access to electricity/internet/resources (0-4, rev: hi=less) -> Table S8b
##   F30  current employment/student status                        -> Table S8c
##   F28  reasons the dream major was not achieved (select-all)     -> Table S8d
##   F35  main barriers to future goals (select-all)                -> Table S8e
##   F44  perceived remedies / support needed (select-all)          -> Table S8f
##
## Produces output/tables/TableS8a..f_*.csv
## =============================================================================

d   <- readRDS(file.path(PATH$out, "analysis_data.rds"))
pal <- d[d$exposure %in% c("p48", "opt"), ]
pal$grp <- factor(pal$exposure, levels = c("p48", "opt"),
                  labels = c(EXP_LABELS[["p48"]], EXP_LABELS[["opt"]]))
GRPS <- c(levels(pal$grp), "All Palestinians")

## n (%) endorsing a 0/1 indicator among non-missing rows.
pct_yes <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("--")
  sprintf("%d/%d (%.1f%%)", sum(x == 1), length(x), 100 * mean(x == 1))
}
## mean (SD) [n] for a numeric item.
msd_n <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("--")
  sprintf("%.2f (%.2f) [n=%d]", mean(x), sd(x), length(x))
}
## Apply a per-group summariser across the two groups + all Palestinians.
by_grp <- function(fun, col) {
  vapply(GRPS, function(g) {
    v <- if (g == "All Palestinians") pal[[col]] else pal[[col]][pal$grp == g]
    fun(v)
  }, character(1))
}
row_bin <- function(label, col) c(Item = label, by_grp(pct_yes, col))
row_num <- function(label, col) c(Item = label, by_grp(msd_n, col))
as_tab  <- function(rows) {
  m <- as.data.frame(do.call(rbind, rows), check.names = FALSE, row.names = NULL)
  m
}

## ---- Table S8a: occupation appraisal (F17, F42) -----------------------------
S8a <- as_tab(list(
  row_bin("Occupation has influenced how I think/decide (F17: Yes)", "occ_influence"),
  row_bin("Occupation has made me more resilient (F42: Yes)",        "occ_resilient")))
write.csv(S8a, file.path(PATH$tables, "TableS8a_appraisal.csv"), row.names = FALSE)

## ---- Table S8b: education context (F25, F26), mean(SD) -----------------------
S8b <- as_tab(list(
  row_num("Education discrimination/limitation (F25, 0-4; higher = more)", "edu_discrim"),
  row_num("Less access to electricity/internet/resources (F26, 0-4; higher = less access)", "edu_resources")))
write.csv(S8b, file.path(PATH$tables, "TableS8b_education_context.csv"), row.names = FALSE)

## ---- Table S8c: employment status (F30), distribution ------------------------
emp_lab <- c(employed = "Employed", student = "Student",
             unemployed = "Unemployed", other = "Other")
s8c_col <- function(v) {
  v <- v[!is.na(v)]; n <- length(v)
  vapply(names(emp_lab), function(k)
    if (n) sprintf("%d (%.1f%%)", sum(v == k), 100 * mean(v == k)) else "--",
    character(1))
}
S8c <- data.frame(Status = unname(emp_lab), check.names = FALSE)
for (g in GRPS) {
  v <- if (g == "All Palestinians") pal$employment else pal$employment[pal$grp == g]
  S8c[[g]] <- s8c_col(v)
}
write.csv(S8c, file.path(PATH$tables, "TableS8c_employment.csv"), row.names = FALSE)

## ---- Table S8d: reasons dream major not achieved (F28, select-all) ----------
## Denominator = university students not studying their dream major (why_* != NA).
S8d <- as_tab(list(
  row_bin("Could not afford it",                         "why_afford"),
  row_bin("Specialty unavailable in country",            "why_unavailable"),
  row_bin("Parents refused",                             "why_parents"),
  row_bin("Not seen as prestigious",                     "why_prestige"),
  row_bin("Not employable afterwards",                   "why_employ")))
write.csv(S8d, file.path(PATH$tables, "TableS8d_dreammajor_reasons.csv"), row.names = FALSE)

## ---- Table S8e: main barriers to future goals (F35, select-all) -------------
S8e <- as_tab(list(
  row_bin("Financial capabilities",  "bar_financial"),
  row_bin("Political situation",     "barrier_political"),
  row_bin("Lack of opportunities",   "bar_opportunities"),
  row_bin("Nepotism",                "bar_nepotism"),
  row_bin("Does not feel limited",   "bar_notlimited")))
write.csv(S8e, file.path(PATH$tables, "TableS8e_goal_barriers.csv"), row.names = FALSE)

## ---- Table S8f: perceived remedies / support needed (F44, select-all) -------
S8f <- as_tab(list(
  row_bin("Better access to education", "rem_education"),
  row_bin("Mental-health support",      "rem_mentalhealth"),
  row_bin("Community programmes",       "rem_community")))
write.csv(S8f, file.path(PATH$tables, "TableS8f_remedies.csv"), row.names = FALSE)

message(sprintf("02b_supp_descriptives.R done: Table S8 a-f written (Palestinian n = %d).",
                nrow(pal)))
