## =============================================================================
## 00_setup.R -- packages, paths, options, constants, shared helpers
## Project: Navigating Success Under Occupation
## -----------------------------------------------------------------------------
## Sourced by every other script. Produces no analytic output of its own.
## Loading order is fixed by run_all.R: 00 -> 01 -> ... -> 08.
## =============================================================================

## ---- 1. Packages ------------------------------------------------------------
## Versions are the ones the pipeline was written against. A mismatch does not
## stop the run; it prints a note. lavaan in particular changes standardisation
## and fit-index defaults between versions, so record the version you used.
PKG_VERSIONS <- c(
  readxl   = "1.4.3",  dplyr   = "1.1.4",  tidyr    = "1.3.1",
  stringr  = "1.5.1",  purrr   = "1.0.2",  tibble   = "3.2.1",
  lavaan   = "0.6-17", MASS    = "7.3-60", psych    = "2.4.1",
  ggplot2  = "3.5.1",  scales  = "1.3.0",  officer  = "0.6.5",
  flextable = "0.9.5"
)

.need <- names(PKG_VERSIONS)
.missing <- .need[!vapply(.need, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  message("Installing missing packages: ", paste(.missing, collapse = ", "))
  install.packages(.missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(.need, function(p)
  suppressPackageStartupMessages(requireNamespace(p, quietly = TRUE))))

## Report version drift (informational only).
for (p in .need) {
  have <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA)
  want <- PKG_VERSIONS[[p]]
  if (!is.na(have) && have != want)
    message(sprintf("  note: %s %s installed (pipeline written for %s)", p, have, want))
}

## ---- 2. Global options ------------------------------------------------------
options(stringsAsFactors = FALSE)
set.seed(2025)                 # only matters for any resampling; kept for reproducibility

## ---- 3. Paths ---------------------------------------------------------------
## All paths are relative to the project root. Run scripts with the project
## root as the working directory (open the .Rproj, or setwd() to the root).
PATH <- list(
  root    = ".",
  data    = file.path(".", "data"),
  out     = file.path(".", "output"),
  tables  = file.path(".", "output", "tables"),
  figures = file.path(".", "output", "figures")
)
for (d in PATH[c("out", "tables", "figures")])
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

## ---- 4. Constants -----------------------------------------------------------
CONST <- list(
  N_EXPECTED = 2580,           # rows in the raw export
  N_COLS     = 45,             # columns F0-F44
  RAW_FILE   = file.path(PATH$data, "raw_responses.xlsx")
)

## Exposure groups, in the reference-first order used everywhere.
EXP_LEVELS <- c("ref", "fcs", "p48", "opt")
EXP_LABELS <- c(
  ref = "MENA, not FCS-listed (reference)",
  fcs = "MENA, fragile/conflict-affected (FCS)",
  p48 = "Palestinian '48",
  opt = "Occupied Palestinian territory"
)

## World Bank FY26 FCS MENA countries NOT under occupation (English keys used
## for robust matching against the bilingual "English / Arabic" export values).
FCS_KEYS <- c("Yemen", "Lebanon", "Syria", "Sudan", "Libya", "Iraq")
OPT_KEYS <- c("West Bank", "Gaza")            # occupied Palestinian territory
P48_KEY  <- "Occupied Palestine (48)"         # Palestinian citizens of Israel

## The covariate block, adjusted for in every model in scripts 03-06.
## age centred; sex; childhood SES (ref = middle); childhood area (ref = urban);
## migration since childhood.
COVARS <- c("age_c", "sex", "ses_low", "ses_high",
            "area_rural", "area_camp", "migrated")
COV_FORMULA <- paste(COVARS, collapse = " + ")

## ---- 5. The contrast set (used in scripts 04 and 07) ------------------------
## One decomposition, three comparison blocks. b_g is the model coefficient for
## exposure group g (dummies: exp_fcs, exp_p48, exp_opt; ref is the baseline).
##   A  each exposed group vs the non-FCS reference   -> the dummy itself
##   B  each Palestinian group vs the FCS stratum     -> difference of dummies
##   C  oPt vs Palestinian '48                         -> difference of dummies
## Each entry: label, and the linear combination over (fcs, p48, opt).
CONTRASTS <- list(
  fcs_ref = list(block = "A", label = "FCS vs ref",  w = c(fcs =  1, p48 = 0, opt = 0)),
  p48_ref = list(block = "A", label = "'48 vs ref",  w = c(fcs =  0, p48 = 1, opt = 0)),
  opt_ref = list(block = "A", label = "oPt vs ref",  w = c(fcs =  0, p48 = 0, opt = 1)),
  p48_fcs = list(block = "B", label = "'48 vs FCS",  w = c(fcs = -1, p48 = 1, opt = 0)),
  opt_fcs = list(block = "B", label = "oPt vs FCS",  w = c(fcs = -1, p48 = 0, opt = 1)),
  opt_p48 = list(block = "C", label = "oPt vs '48",  w = c(fcs =  0, p48 = -1, opt = 1))
)

## ---- 6. Helper functions ----------------------------------------------------

## Stop with a clear message if a data assumption is violated.
assert <- function(cond, msg) {
  if (!isTRUE(cond)) stop("ASSERTION FAILED: ", msg, call. = FALSE)
  invisible(TRUE)
}

## Recode a bilingual character vector by matching English keywords.
## `map` is a named character vector: names are regex patterns (matched with
## fixed = FALSE, ignore.case = TRUE), values are the recoded results. The first
## matching pattern wins. Unmatched -> NA (with a warning listing the values).
recode_key <- function(x, map, to_numeric = FALSE, varname = "variable") {
  x <- as.character(x)
  out     <- rep(NA_character_, length(x))
  matched <- rep(FALSE, length(x))            # tracks handled rows (incl. -> NA)
  for (pat in names(map)) {
    hit <- !matched & !is.na(x) & grepl(pat, x, ignore.case = TRUE)
    out[hit]     <- map[[pat]]                # may be NA on purpose
    matched[hit] <- TRUE
  }
  miss <- unique(x[!matched & !is.na(x)])
  if (length(miss))
    warning(sprintf("recode_key(%s): unmatched values -> NA: %s",
                    varname, paste(head(miss, 10), collapse = " | ")), call. = FALSE)
  if (to_numeric) out <- as.numeric(out)
  out
}

## Extract the leading integer from a Google-Forms linear-scale export such as
## "5 (Strongly agree / ...)", "4.0", "1 (Strongly disagree / ...)".
lead_int <- function(x) {
  x <- as.character(x)
  suppressWarnings(as.integer(sub("^\\s*([0-9]+).*$", "\\1", x)))
}

## Linear contrasts from a fitted glm (log-odds scale) -> OR table.
## `coefs`, `V` are the coefficient vector and covariance matrix; `w_named` is a
## named vector of weights over model terms. Returns est(OR), lo, hi, p.
lin_or <- function(coefs, V, w_named) {
  w <- rep(0, length(coefs)); names(w) <- names(coefs)
  w[names(w_named)] <- w_named
  est <- sum(w * coefs)
  se  <- sqrt(as.numeric(t(w) %*% V %*% w))
  z   <- est / se
  data.frame(
    or = exp(est),
    lo = exp(est - 1.96 * se),
    hi = exp(est + 1.96 * se),
    p  = 2 * stats::pnorm(-abs(z))
  )
}

## Format an estimate with a 95% CI, e.g. "0.564 [0.443, 0.685]".
fmt_ci <- function(est, lo, hi, d = 3) {
  sprintf(paste0("%.", d, "f [%.", d, "f, %.", d, "f]"), est, lo, hi)
}
fmt_or <- function(or, lo, hi, d = 2) {
  sprintf(paste0("%.", d, "f [%.", d, "f, %.", d, "f]"), or, lo, hi)
}
## p-value to APA string.
fmt_p <- function(p) ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))

## Significance stars for compact tables.
stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))

message("00_setup.R loaded: helpers, paths, constants and the contrast set are ready.")
