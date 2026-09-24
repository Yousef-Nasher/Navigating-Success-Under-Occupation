## =============================================================================
## 08_export_word.R -- assemble tables + figures into one Word document
## -----------------------------------------------------------------------------
## Reads every CSV in output/tables/ and the PNGs in output/figures/ and writes
##   output/Results_Compiled.docx
## with the main tables and figures first, then the supplementary tables.
##
## Layout:
##   * every table is set to 100% of the text width;
##   * every table carries a "Note." row (definitions, units, method) merged
##     across the full width;
##   * figures are capped to the text width.
## The note text lives in this script, so re-running the pipeline reproduces it.
## =============================================================================

suppressWarnings(suppressMessages({library(officer); library(flextable); library(dplyr)}))

## ---- Layout constants -------------------------------------------------------
BODY_SIZE <- 10     # table body / header font (pt)
NOTE_SIZE <- 9      # "Note." footer font (pt)
MAX_IMG_W <- 6.2    # max image width (in) for A4 portrait with the default margins

tbl <- function(path) read.csv(file.path(PATH$tables, path), check.names = FALSE)

## Build a flextable that (a) fills the text width and (b) carries a merged,
## full-width, italic "Note." footer row.
make_ft <- function(df, note = NULL) {
  x <- flextable::flextable(df)
  x <- flextable::theme_booktabs(x)
  x <- flextable::fontsize(x, size = BODY_SIZE, part = "all")
  x <- flextable::padding(x, padding = 3, part = "all")
  x <- flextable::valign(x, valign = "top", part = "body")
  if (!is.null(note) && nzchar(note)) {
    ## One footer cell spanning every column (colwidths = ncol -> a single span).
    x <- flextable::add_footer_row(x, top = FALSE, values = "", colwidths = ncol(df))
    x <- flextable::compose(
      x, i = 1, j = 1, part = "footer",
      value = flextable::as_paragraph(
        flextable::as_i(flextable::as_b("Note. ")),
        flextable::as_i(note)))
    x <- flextable::fontsize(x, size = NOTE_SIZE, part = "footer")
    x <- flextable::align(x, align = "left", part = "footer")
  }
  ## Fit the whole table to 100% of the available (text) width.
  x <- flextable::set_table_properties(x, layout = "autofit", width = 1)
  x
}

doc <- officer::read_docx()
## officer inserts at an internal cursor that body_add_flextable and body_add_par
## do not advance in lockstep; mixing them detaches the paragraphs (headings pile
## up at the end in reverse). Forcing the cursor to the document end before every
## insertion keeps headings, tables and figures in call order, version-agnostic.
at_end <- function() doc <<- officer::cursor_end(doc)
h1  <- function(txt) { at_end(); doc <<- officer::body_add_par(doc, txt, style = "heading 1") }
h2  <- function(txt) { at_end(); doc <<- officer::body_add_par(doc, txt, style = "heading 2") }
note <- function(txt) { at_end(); doc <<- officer::body_add_par(doc, txt, style = "Normal") }
add_tbl <- function(df, note = NULL) {
  at_end(); doc <<- flextable::body_add_flextable(doc, make_ft(df, note))
  at_end(); doc <<- officer::body_add_par(doc, "", style = "Normal")
}
add_img <- function(file, w = MAX_IMG_W, h = 4) {
  if (w > MAX_IMG_W) { h <- h * MAX_IMG_W / w; w <- MAX_IMG_W }   # keep aspect ratio, cap width
  p <- file.path(PATH$figures, file)
  if (file.exists(p)) { at_end(); doc <<- officer::body_add_img(doc, p, width = w, height = h)
    at_end(); doc <<- officer::body_add_par(doc, "", style = "Normal") }
}

## Heading: number/label on one line, descriptive title beneath.
lab <- function(label, title) { h2(label); note(title) }
tt  <- function(label, title, df, notetxt = NULL) { lab(label, title); add_tbl(df, notetxt) }

## ---- Table notes ------------------------------------------------------------
## All table notes, kept together in one place.
NOTES <- list(
  t1 = paste(
    "FCS = World Bank Fragile and Conflict-affected Situations list (FY26);",
    "SES = socioeconomic status; oPt = occupied Palestinian territory;",
    "'48 = Palestinian citizens of Israel. Because the large sample renders",
    "trivial differences statistically significant, groups are compared through",
    "effect sizes (Table S1) rather than significance tests."),
  t2a = paste(
    "Four-factor measurement model (post-traumatic stress, anxiety, educational",
    "disruption, family support); ordinal indicators estimated with WLSMV;",
    "scaled fit indices are reported. CFI = comparative fit index;",
    "TLI = Tucker-Lewis index; RMSEA = root mean square error of approximation",
    "(with 90% CI); SRMR = standardised root mean square residual. Categorical",
    "estimation yields a larger RMSEA and smaller CFI/TLI than maximum likelihood",
    "for the same misspecification (Xia & Yang, 2019), and RMSEA is upwardly",
    "biased and unstable when degrees of freedom are small (Kenny, Kaniskan &",
    "McCoach, 2015), so it is interpreted alongside CFI and SRMR rather than",
    "against a single cut-off."),
  t2b = paste(
    "omega = McDonald's omega; alpha = Cronbach's alpha;",
    "AVE = average variance extracted. The square root of the AVE exceeded every",
    "off-diagonal latent correlation for each factor. PCL-5 = PTSD Checklist for",
    "DSM-5; GAD-7 = Generalized Anxiety Disorder 7-item scale. Full loadings,",
    "discriminant validity and invariance are in Table S2."),
  t3 = paste(
    "Values are point estimates with 95% confidence intervals;",
    "* p < .05, ** p < .01, *** p < .001. Block A = each exposed group vs the",
    "non-FCS reference; Block B = each Palestinian group vs the FCS stratum;",
    "Block C = oPt vs '48. ref = MENA countries not on the World Bank FY26 FCS",
    "list; FCS = fragile and conflict-affected MENA countries;",
    "oPt = occupied Palestinian territory; '48 = Palestinian citizens of Israel.",
    "For the three latent outcomes the outcome is standardised and the 0/1",
    "exposure dummies are not, so b is a difference in outcome standard",
    "deviations; OR = adjusted odds ratio. All models adjust for age, sex,",
    "childhood socioeconomic status, childhood area type and migration since",
    "childhood."),
  t4a = paste(
    "Analytic subsample n = 809 Palestinian participants (624 occupied",
    "Palestinian territory, 185 Palestinian '48). The conflict-experience index",
    "is standardised to 1 SD within the subsample; the three latent outcomes are",
    "regressed on it by WLSMV SEM (b in outcome SD units) and the three",
    "occupation-outlook outcomes by ordinal logistic regression (OR per 1 point",
    "of the raw 0-6 index).",
    "All models adjust for age, sex, childhood SES, childhood area type and",
    "migration. The index is formative, so inference rests on the mutually",
    "adjusted item model in Panel B rather than on its internal consistency",
    "(Bollen & Bauldry, 2011)."),
  t4b = paste(
    "Adjusted odds ratios from ordinal (proportional-odds) logistic regression",
    "with all three conflict experiences entered simultaneously and the standard",
    "covariate block; n varies by outcome and is shown in the n column. Values",
    "are OR [95% CI]. Higher outcome values denote greater limitation, stronger",
    "migration intention and more hope. The three items form a formative",
    "checklist, so internal consistency is not an appropriate criterion",
    "(Bollen & Bauldry, 2011; Streiner, 2003)."),
  s1 = paste(
    "Continuous characteristics as Hedges' g against the non-FCS reference",
    "group; binary characteristics as differences in percentage points. Values",
    "are estimate [95% CI]. Reported instead of significance tests, which are",
    "uninformative at this sample size."),
  s2a = paste(
    "Standardised factor loadings from the WLSMV measurement model on the full",
    "sample (N = 2,580), with 95% confidence intervals and standard errors.",
    "All indicators are ordinal."),
  s2b = paste(
    "Square root of the average variance extracted on the diagonal; latent",
    "correlations off the diagonal. ptss = post-traumatic stress;",
    "anx = anxiety; edis = educational disruption."),
  s2c = paste(
    "Multi-group WLSMV measurement invariance across the four exposure groups.",
    "Judged by change in fit rather than the chi-square difference test, which",
    "is oversensitive at this sample size. Criteria: dCFI <= .010 and",
    "dRMSEA <= .015 (Chen, 2007)."),
  s3a = paste(
    "World Bank FY26 Classification of Fragile and Conflict-affected Situations.",
    "Exposure level: 0 = not listed (reference); FCS = listed, not under",
    "occupation; Palestinian '48 = Palestinian citizens of Israel;",
    "oPt = occupied Palestinian territory. n = participants from each childhood",
    "country."),
  s3b = paste(
    "Exposure level is a deterministic function of childhood country;",
    "pct_of_level = percentage of participants at that exposure level from each",
    "country. Countries contributing fewer than 15 participants are retained in",
    "the analysis and shown here for completeness."),
  s4a = paste(
    "Full coefficient block for Model 1 (three latent outcomes, WLSMV).",
    "b is in outcome SD units (std.nox); values are b [95% CI]. CIs come from the",
    "standardised solution and can differ in the third decimal from Table 3,",
    "which rescales the unstandardised interval. Covariate block:",
    "age (centred), sex, childhood socioeconomic status, childhood area type and",
    "migration since childhood."),
  s4b = paste(
    "Full coefficient block for Model 2 (dream-major attainment, logistic",
    "regression). Values are OR [95% CI]. Same covariate block as Model 1."),
  s5 = paste(
    "Each childhood country contributing at least 15 participants to one of the",
    "two multi-country strata (reference, FCS) is dropped in turn and the two",
    "tracked estimands are refitted: educational disruption (oPt vs FCS, outcome",
    "SD) and structural attribution (oPt vs FCS, OR). A country random effect or",
    "country-clustered SE is not estimable for the Palestinian arms, which",
    "contain one country each, so leave-one-country-out is the estimable check",
    "that a comparison baseline is not the product of a single country."),
  s6 = paste(
    "Descriptive distribution of occupation-specific outlook among the",
    "Palestinian subsample; percentages (pct) are within each Palestinian group.",
    "Descriptive only; not modelled."),
  t5 = paste(
    "Adjusted association of family support with each latent outcome, full sample",
    "(N = 2,580), from a WLSMV structural equation model. Family support is a",
    "latent factor (three ordinal indicators); b is the fully standardised",
    "coefficient, i.e. the change in outcome SD per 1 SD of family support, net",
    "of childhood exposure group and the standard covariate block (age, sex,",
    "childhood SES, childhood area type, migration). * p<.05, ** p<.01,",
    "*** p<.001. This is an adjusted main effect, not a causal or buffering",
    "claim."),
  s7 = paste(
    "EXPLORATORY. Exposure x family-support interaction on each latent outcome",
    "(WLSMV SEM); family support entered as an observed standardised composite.",
    "Coefficients are the difference in the family-support slope for each group",
    "relative to the non-FCS reference, in outcome SD units. A convenience",
    "university sample is not designed to detect interactions, so these terms are",
    "hypothesis-generating and are not interpreted as confirmatory evidence of",
    "differential buffering."),
  s8a = paste(
    "Descriptive endorsement within the Palestinian sample (occupied Palestinian",
    "territory + Palestinian '48). Items F17 and F42 are answered only by those",
    "living under occupation; cells are n endorsing / n answering (%)."),
  s8b = paste(
    "Mean (SD) [n] within the Palestinian sample. F25: higher = more perceived",
    "discrimination/limitation (0-4). F26 is reverse-coded so higher = less",
    "access to electricity, internet and educational resources (0-4). Descriptive."),
  s8c = paste(
    "Current employment/student status (F30) within the Palestinian sample;",
    "column percentages within each group. Descriptive."),
  s8d = paste(
    "Reasons a dream major was not achieved (F28, select-all), among Palestinian",
    "university students not studying their dream major; n endorsing / n (%).",
    "Descriptive."),
  s8e = paste(
    "Main barriers to future goals (F35, select-all) within the Palestinian",
    "sample; n endorsing / n answering (%). Categories are not mutually",
    "exclusive. Descriptive."),
  s8f = paste(
    "Perceived remedies / support needed (F44, select-all) within the Palestinian",
    "sample; n endorsing / n answering (%). Categories are not mutually",
    "exclusive. Descriptive.")
)

## ---- Pivot Table 3 (tidy -> wide) ------------------------------------------
t3 <- tbl("Table3_decomposition.csv")
ord_contrast <- names(CONTRASTS)                       # A(3), B(2), C(1)
ord_outcome  <- c("Post-traumatic stress", "Anxiety", "Educational disruption",
                  "Dream-major attainment",
                  "Structural (vs internal/no) barrier attribution",
                  "Names the political situation as a barrier")
wide <- data.frame(Outcome = ord_outcome, Metric = NA, check.names = FALSE)
for (k in ord_contrast) wide[[CONTRASTS[[k]]$label]] <- NA
for (i in seq_along(ord_outcome)) {
  sub <- t3[t3$outcome == ord_outcome[i], ]
  wide$Metric[i] <- sub$metric[1]
  for (k in ord_contrast) {
    v <- sub$value[sub$contrast == k]
    if (length(v)) wide[i, CONTRASTS[[k]]$label] <- v
  }
}

## ---- Main document ----------------------------------------------------------
h1("Navigating Success Under Occupation - Compiled Results")
note("Generated by the analysis pipeline. Main tables and figures, then supplement.")
note(paste("Significance stars: * p<.05, ** p<.01, *** p<.001. Latent outcomes in",
           "std.nox units (outcome SD). Binary outcomes as odds ratios."))

tt("Table 1", "Participant Characteristics by Childhood Group", tbl("Table1_characteristics.csv"), NOTES$t1)
tt("Table 2, Panel A", "Measurement Model Fit", tbl("Table2a_cfa_fit.csv"), NOTES$t2a)
tt("Table 2, Panel B", "Reliability and Convergent Validity", tbl("Table2b_reliability.csv"), NOTES$t2b)
tt("Table 3", "Childhood Group and All Outcomes, Decomposed", wide, NOTES$t3)
tt("Table 4, Panel A", "Individual Conflict Experience and Outcomes Among Palestinian Participants: Conflict-Experience Index", tbl("Table4A_index.csv"), NOTES$t4a)
tt("Table 4, Panel B", "Individual Conflict Experience and Outcomes Among Palestinian Participants: Three Experiences Entered Simultaneously", tbl("Table4B_items_mutually_adjusted.csv"), NOTES$t4b)
tt("Table 5", "Family Support as a Protective Resource (Full-Sample SEM)", tbl("Table5_family_support.csv"), NOTES$t5)

lab("Figure 1", "Participant Flow"); add_img("Figure1_flow.png", 5.5, 6.8)
lab("Figure 2", "Decomposition of Childhood Exposure Effects"); add_img("Figure2_decomposition.png", 6.5, 3.25)
lab("Figure 3", "Within-Palestinian Conflict Experience and Outlook"); add_img("Figure3_within_palestinian.png", 6.5, 2.4)

## ---- Supplement -------------------------------------------------------------
doc <- officer::cursor_end(doc); doc <- officer::body_add_break(doc)
h1("Supplementary Material")
tt("Table S1", "Between-Group Differences as Effect Sizes", tbl("TableS1_effect_sizes.csv"), NOTES$s1)
tt("Table S2, Panel A", "Standardised Factor Loadings", tbl("TableS2a_loadings.csv"), NOTES$s2a)
tt("Table S2, Panel B", "Discriminant Validity (Fornell-Larcker; sqrt(AVE) on Diagonal)", tbl("TableS2b_discriminant.csv"), NOTES$s2b)
tt("Table S2, Panel C", "Measurement Invariance Across Exposure Groups", tbl("TableS2c_invariance.csv"), NOTES$s2c)
tt("Table S3, Panel A", "Childhood Country Classification", tbl("TableS3a_country_classification.csv"), NOTES$s3a)
tt("Table S3, Panel B", "Composition of Each Exposure Level", tbl("TableS3b_composition.csv"), NOTES$s3b)
tt("Table S4, Panel A", "Full Covariate Coefficients, Model 1 (Latent Outcomes)", tbl("TableS4a_covariates_model1.csv"), NOTES$s4a)
tt("Table S4, Panel B", "Full Covariate Coefficients, Model 2 (Dream-Major Attainment)", tbl("TableS4b_covariates_model2.csv"), NOTES$s4b)
tt("Table S5", "Leave-One-Country-Out Robustness", tbl("TableS5_leave_one_country_out.csv"), NOTES$s5)
tt("Table S6", "Descriptive Occupation Outlook (Palestinians)", tbl("TableS6_occupation_outlook.csv"), NOTES$s6)
tt("Table S7", "Exposure x Family-Support Interaction (Exploratory)", tbl("TableS7_family_support_moderation.csv"), NOTES$s7)
tt("Table S8, Panel A", "Occupation Appraisal (Palestinian Sample)", tbl("TableS8a_appraisal.csv"), NOTES$s8a)
tt("Table S8, Panel B", "Education Context (Palestinian Sample)", tbl("TableS8b_education_context.csv"), NOTES$s8b)
tt("Table S8, Panel C", "Employment Status (Palestinian Sample)", tbl("TableS8c_employment.csv"), NOTES$s8c)
tt("Table S8, Panel D", "Reasons Dream Major Not Achieved (Palestinian Sample)", tbl("TableS8d_dreammajor_reasons.csv"), NOTES$s8d)
tt("Table S8, Panel E", "Main Barriers to Future Goals (Palestinian Sample)", tbl("TableS8e_goal_barriers.csv"), NOTES$s8e)
tt("Table S8, Panel F", "Perceived Remedies / Support Needed (Palestinian Sample)", tbl("TableS8f_remedies.csv"), NOTES$s8f)

print(doc, target = file.path(PATH$out, "Results_Compiled.docx"))
message("08_export_word.R done: output/Results_Compiled.docx written.")
