## =============================================================================
## 01_clean.R -- raw export -> frozen analysis dataset
## -----------------------------------------------------------------------------
## Reads data/raw_responses.xlsx (45 columns, F0-F44 in the coding scheme) and
## writes:
##   output/analysis_data.rds   the single dataset every model uses
##   output/flow_counts.csv     counts for the STROBE flow diagram (Figure 1)
##   output/cherries_rates.csv  the CHERRIES rates that are computable here
##
## The raw values are bilingual ("English / Arabic"); every recode matches on
## an English keyword and is followed by a check. Derivation rules were fixed
## against the coding scheme and reproduce the reported group sizes exactly.
## =============================================================================

suppressWarnings(suppressMessages(library(readxl)))

raw <- readxl::read_excel(CONST$RAW_FILE, col_names = FALSE, skip = 1,
                          .name_repair = "minimal")
assert(nrow(raw) == CONST$N_EXPECTED,
       sprintf("expected %d rows, found %d", CONST$N_EXPECTED, nrow(raw)))
assert(ncol(raw) == CONST$N_COLS,
       sprintf("expected %d columns, found %d", CONST$N_COLS, ncol(raw)))

## Name the 45 columns F0..F44 (F-labels match the coding scheme).
names(raw) <- paste0("f", 0:44)

flow <- data.frame(step = "Raw responses received", n = nrow(raw))

## ---- 1. Demographics and covariates -----------------------------------------
d <- data.frame(row_id = seq_len(nrow(raw)))

d$age <- suppressWarnings(as.numeric(sub("\\..*$", "", as.character(raw$f3))))
d$sex <- recode_key(raw$f4, c("Female" = "1", "Male" = "0"),
                    to_numeric = TRUE, varname = "sex")   # 1 = female

d$area <- recode_key(raw$f6,
  c("Urban" = "urban", "Rural" = "rural", "Refugee" = "camp"), varname = "area")
d$ses  <- recode_key(raw$f9,
  c("Low" = "low", "Middle" = "middle", "High" = "high"), varname = "ses")
d$edu_level <- recode_key(raw$f22,
  c("University" = "university", "Secondary" = "secondary", "Primary" = "primary"),
  varname = "edu_level")

## ---- 2. Exposure group (childhood country, F5) -------------------------------
country <- as.character(raw$f5)
exposure <- rep(NA_character_, length(country))
exposure[grepl(P48_KEY, country, fixed = TRUE)]                 <- "p48"
exposure[Reduce(`|`, lapply(OPT_KEYS, grepl, country, fixed = TRUE))] <- "opt"
is_fcs <- Reduce(`|`, lapply(FCS_KEYS, grepl, country, fixed = TRUE))
exposure[is_fcs & is.na(exposure)]                             <- "fcs"
exposure[is.na(exposure)]                                      <- "ref"
d$exposure <- factor(exposure, levels = EXP_LEVELS)
d$country_raw <- country

## Short, clean English country label (for S3/S4 composition tables).
d$country <- trimws(sub("/.*$", "", country))

assert(all(table(d$exposure)[EXP_LEVELS] == c(1338, 390, 213, 639)),
       "exposure group sizes do not match the expected 1338/390/213/639")

## Exposure dummies (reference = 'ref').
d$exp_fcs <- as.integer(d$exposure == "fcs")
d$exp_p48 <- as.integer(d$exposure == "p48")
d$exp_opt <- as.integer(d$exposure == "opt")

## ---- 3. Migration since childhood ------------------------------------------
## Childhood country (F5) differs from current country (F7).
d$migrated <- as.integer(trimws(as.character(raw$f5)) != trimws(as.character(raw$f7)))

## ---- 4. Latent-model indicators (all ordinal) -------------------------------
freq4 <- c("Never" = "0", "Sometimes" = "1", "Often" = "2", "Always" = "3")
anx3  <- c("Not at all" = "0", "Sometimes" = "1", "Nearly every day" = "2")
freq5 <- c("Never" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Always" = "4")

## Post-traumatic stress (F10-F13)
d$ptss1 <- recode_key(raw$f10, freq4, TRUE, "ptss1")
d$ptss2 <- recode_key(raw$f11, freq4, TRUE, "ptss2")
d$ptss3 <- recode_key(raw$f12, freq4, TRUE, "ptss3")
d$ptss4 <- recode_key(raw$f13, freq4, TRUE, "ptss4")
## Anxiety (F14-F16)
d$anx1 <- recode_key(raw$f14, anx3, TRUE, "anx1")
d$anx2 <- recode_key(raw$f15, anx3, TRUE, "anx2")
d$anx3 <- recode_key(raw$f16, anx3, TRUE, "anx3")
## Educational disruption (F23, F24, F27) -- the retained three items
d$edu_closures <- recode_key(raw$f23, freq5, TRUE, "edu_closures")
d$edu_missed   <- recode_key(raw$f24, freq5, TRUE, "edu_missed")
d$edu_distress <- recode_key(raw$f27, freq5, TRUE, "edu_distress")
## Family support (F36-F38) -- 1-5 agreement, leading integer of the export
d$fam1 <- lead_int(raw$f36)
d$fam2 <- lead_int(raw$f37)
d$fam3 <- lead_int(raw$f38)
## Coping (F39-F41) -- 0-4 frequency
d$cope1 <- recode_key(raw$f39, freq5, TRUE, "cope1")
d$cope2 <- recode_key(raw$f40, freq5, TRUE, "cope2")
d$cope3 <- recode_key(raw$f41, freq5, TRUE, "cope3")

## Excluded education items, retained for the S2 rationale panel (descriptive).
d$edu_discrim   <- recode_key(raw$f25, freq5, TRUE, "edu_discrim")   # exposure-like
rev5 <- c("Always" = "0", "Often" = "1", "Sometimes" = "2", "Rarely" = "3", "Never" = "4")
d$edu_resources <- recode_key(raw$f26, rev5, TRUE, "edu_resources")  # reverse: higher = less access

## Continuous family-support score (Table 1, Table S1 and the exploratory
## interaction in Table S7).
d$fam_support <- rowMeans(d[, c("fam1", "fam2", "fam3")], na.rm = TRUE)
d$fam_support_c <- d$fam_support - mean(d$fam_support, na.rm = TRUE)

## ---- 5. Occupation and conflict items (Palestinian subsample) ---------------
## F17 defines the "under occupation" subsample; NA = not applicable.
d$occ_influence <- recode_key(raw$f17,
  c("do not live under occupation" = NA, "Yes" = "1", "No" = "0"),
  to_numeric = TRUE, varname = "occ_influence")

## Occupation outlook (F18), descriptive (S6).
d$occ_outlook <- recode_key(raw$f18,
  c("do not liver under occupation" = NA, "do not live under occupation" = NA,
    "more cautious" = "cautious", "more resilient" = "resilient",
    "feel limited" = "limited", "Other" = "other"), varname = "occ_outlook")

## Made you more resilient (F42), descriptive (S6).
d$occ_resilient <- recode_key(raw$f42,
  c("do not live under occupation" = NA, "Yes" = "1", "No" = "0"),
  to_numeric = TRUE, varname = "occ_resilient")

## Conflict-experience items (F19, F20, F21). 0/1/2; F21 reverse-coded so that
## a high value = greater insecurity/exposure.
d$conf_displace  <- recode_key(raw$f19, c("No" = "0", "Unsure" = "1", "Yes" = "2"),
                               TRUE, "conf_displace")
d$conf_prevented <- recode_key(raw$f20, c("No" = "0", "Unsure" = "1", "Yes" = "2"),
                               TRUE, "conf_prevented")
d$conf_safeexpr  <- recode_key(raw$f21, c("Yes" = "0", "Unsure" = "1", "No" = "2"),
                               TRUE, "conf_safeexpr")     # reverse-coded
d$conflict_index <- d$conf_displace + d$conf_prevented + d$conf_safeexpr   # 0-6

## Occupation-specific outcome items (F32 limit, F33 migration, F34 hope).
## 0-4; "I do not live under occupation" -> NA. Higher hope = more hopeful.
occ5 <- c("do not live under occupation" = NA,
          "Never" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Always" = "4")
d$lim_career <- recode_key(raw$f32, occ5, TRUE, "lim_career")
d$mig_intent <- recode_key(raw$f33, occ5, TRUE, "mig_intent")
d$hope       <- recode_key(raw$f34, occ5, TRUE, "hope")

## ---- 6. Aspiration and attribution outcomes --------------------------------
## Dream-major attainment (F28). NA if not at university OR "Other" selected
## alone (ambiguous); 1 if "Yes" present; else 0. Reproduces n = 2234.
f28 <- trimws(as.character(raw$f28))
not_uni    <- grepl("do not study at university", f28)
other_only <- startsWith(f28, "Other")
d$dream_major <- ifelse(not_uni | other_only, NA_integer_,
                        as.integer(grepl("Yes", f28)))

## F28 reasons a dream major was NOT achieved (select-all; 0/1 indicators),
## descriptive only (Table S8d). Defined among university students who are NOT
## studying their dream major; NA otherwise. Keywords match the coding-scheme
## English option text.
uni_nondream <- !not_uni & !grepl("Yes", f28)
r28 <- function(pat) ifelse(uni_nondream, as.integer(grepl(pat, f28, ignore.case = TRUE)), NA_integer_)
d$why_afford      <- r28("afford")
d$why_unavailable <- r28("specliaty|special|not available|unavailable")
d$why_parents     <- r28("parents")
d$why_prestige    <- r28("prestig")
d$why_employ      <- r28("employ")

## F30 current employment/student status (single choice), descriptive (Table S8c).
## Order matters: match "Unemployed" before "Employed" (substring).
d$employment <- recode_key(raw$f30,
  c("Unemployed" = "unemployed", "Employed" = "employed",
    "Student" = "student", "Other" = "other"), varname = "employment")

## Structural (vs internal/no) barrier attribution (F31). Structural = lack of
## resources or limited opportunities; internal/none = fear of failure or no
## barriers; "Other" excluded from the binary contrast. Reproduces n = 2342.
d$barrier_structural <- recode_key(raw$f31,
  c("Lack of resources" = "1", "Limited opportunities" = "1",
    "Fear of failure" = "0", "do not have any barriers" = "0",
    "Other" = NA), to_numeric = TRUE, varname = "barrier_structural")

## Political situation named as a barrier (F35, select-all). 1 if named; all N.
f35 <- as.character(raw$f35)
d$barrier_political <- as.integer(grepl("political situation", f35, ignore.case = TRUE))

## F35 full barrier categories (select-all; 0/1 indicators), descriptive (Table S8e).
d$bar_financial     <- as.integer(grepl("financial", f35, ignore.case = TRUE))
d$bar_opportunities <- as.integer(grepl("opportunit", f35, ignore.case = TRUE))
d$bar_nepotism      <- as.integer(grepl("nepotism", f35, ignore.case = TRUE))
d$bar_notlimited    <- as.integer(grepl("not feel limited", f35, ignore.case = TRUE))

## F44 perceived remedies / support needed (select-all; 0/1 indicators),
## descriptive (Table S8f). The coding scheme records F44 as a select-all item.
f44 <- as.character(raw$f44)
d$rem_education     <- as.integer(grepl("education", f44, ignore.case = TRUE))
d$rem_mentalhealth  <- as.integer(grepl("mental",    f44, ignore.case = TRUE))
d$rem_community     <- as.integer(grepl("community", f44, ignore.case = TRUE))
if (sum(d$rem_education + d$rem_mentalhealth + d$rem_community, na.rm = TRUE) == 0)
  message("  note: F44 matched no fixed categories -- verify it is select-all, not free text.")

## Biggest-barrier type (F31, full categories) for the multinomial in S-note /
## Table S4 companion (kept as a labelled factor; modelled only if requested).
d$barrier_type <- recode_key(raw$f31,
  c("Lack of resources" = "resources", "Limited opportunities" = "opportunities",
    "Fear of failure" = "fear", "do not have any barriers" = "none",
    "Other" = "other"), varname = "barrier_type")

## ---- 7. Covariate transforms ------------------------------------------------
d$age_c      <- d$age - mean(d$age, na.rm = TRUE)
d$ses_low    <- as.integer(d$ses == "low")
d$ses_high   <- as.integer(d$ses == "high")
d$area_rural <- as.integer(d$area == "rural")
d$area_camp  <- as.integer(d$area == "camp")

## ---- 8. Subsample flag ------------------------------------------------------
## Palestinian participants who did NOT select "I do not live under occupation"
## on F17. Reproduces n = 809 (624 oPt + 185 '48).
d$under_occupation <- as.integer(d$exposure %in% c("p48", "opt") & !is.na(d$occ_influence))
assert(sum(d$under_occupation) == 809, "under-occupation subsample is not 809")
assert(all(table(d$exposure[d$under_occupation == 1])[c("opt", "p48")] == c(624, 185)),
       "under-occupation subsample composition is not 624 oPt / 185 '48")

## ---- 9. Ordered factors for lavaan / polr -----------------------------------
ORD_IND <- c(paste0("ptss", 1:4), paste0("anx", 1:3),
             "edu_closures", "edu_missed", "edu_distress",
             paste0("fam", 1:3), paste0("cope", 1:3))
## (Indicators are passed to lavaan via `ordered = ORD_IND`; kept numeric here.)

## Ordinal outcomes for proportional-odds models (Table 4).
d$lim_career_o <- factor(d$lim_career, levels = 0:4, ordered = TRUE)
d$mig_intent_o <- factor(d$mig_intent, levels = 0:4, ordered = TRUE)
d$hope_o       <- factor(d$hope,       levels = 0:4, ordered = TRUE)

## ---- 10. Flow and analytic-sample counts ------------------------------------
flow <- rbind(flow,
  data.frame(step = "Eligible (18-25, MENA childhood, consented)", n = nrow(d)),
  data.frame(step = "Analytic sample (full)",                      n = nrow(d)),
  data.frame(step = "  Model 1 latent outcomes",                   n = nrow(d)),
  data.frame(step = "  Dream-major attainment",         n = sum(!is.na(d$dream_major))),
  data.frame(step = "  Structural attribution",         n = sum(!is.na(d$barrier_structural))),
  data.frame(step = "  Political attribution",          n = sum(!is.na(d$barrier_political))),
  data.frame(step = "  Palestinian under-occupation subsample", n = sum(d$under_occupation)),
  data.frame(step = "    Perceived career limitation",  n = sum(d$under_occupation == 1 & !is.na(d$lim_career))),
  data.frame(step = "    Migration intention",          n = sum(d$under_occupation == 1 & !is.na(d$mig_intent))),
  data.frame(step = "    Hope for the future",          n = sum(d$under_occupation == 1 & !is.na(d$hope)))
)
write.csv(flow, file.path(PATH$out, "flow_counts.csv"), row.names = FALSE)

## ---- 11. CHERRIES rates (what is computable for a social-media survey) -------
## View rate and unique-visitor denominators are not knowable for open
## distribution through social-media groups, so they are reported as NA with a
## note. Substantive items were forced-response, so item completion is complete
## by design; occupation-specific items carry an explicit not-applicable option
## that is treated as missing, not as non-completion.
cherries <- data.frame(
  metric = c("View rate", "Participation rate", "Completion rate",
             "Item non-response (substantive items)"),
  value  = c("Not determinable (open social-media distribution)",
             "Not determinable (denominator of those who opened the link unknown)",
             "100% (forced-response items; no partial submissions retained)",
             "0% (forced-response; not-applicable options treated as missing)"),
  stringsAsFactors = FALSE
)
write.csv(cherries, file.path(PATH$out, "cherries_rates.csv"), row.names = FALSE)

## ---- 12. Freeze the dataset -------------------------------------------------
attr(d, "ORD_IND")   <- ORD_IND
saveRDS(d, file.path(PATH$out, "analysis_data.rds"))

message(sprintf("01_clean.R done: analysis_data.rds written (N = %d).", nrow(d)))
message("  Group sizes: ", paste(sprintf("%s=%d", EXP_LEVELS, table(d$exposure)[EXP_LEVELS]), collapse = ", "))
