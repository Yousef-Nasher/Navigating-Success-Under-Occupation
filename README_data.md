# Data documentation

`raw_responses.xlsx` is the survey export, sheet **Form Responses 1**: 2,580 rows
× 45 columns. Values are bilingual, formatted `English / Arabic`. Script
`R/01_clean.R` reads the sheet with the header row skipped and names the 45
columns `f0`–`f44`, matching the coding scheme in `docs/`. F1–F44 are the item
numbers of the bilingual questionnaire (Supplementary File 1); F0 is the
submission timestamp.

## Re-identification caveat (read before sharing)

Column **F2** (`f2`, "collaborator number") identifies which of the 49
distributors recruited each respondent and is potentially re-identifying in
combination with country and demographics. **Hash or drop F2 before any public
release**, and confirm the study's ethics approval permits public data deposit.
The analysis never uses F2, so removing it does not change any result.

## Column map (F-label → variable → coding)

| F | Column | Derived variable | Coding |
|---|---|---|---|
| F0 | Timestamp | — | not used |
| F1 | Consent text | — | not used |
| F2 | Collaborator number | — | **re-identifying; drop/hash before sharing** |
| F3 | Age | `age`, `age_c` | numeric; `age_c` = mean-centred |
| F4 | Gender | `sex` | Female = 1, Male = 0 |
| F5 | Childhood country | `exposure`, `country` | ref / fcs / p48 / opt (see below) |
| F6 | Childhood area | `area`, `area_rural`, `area_camp` | urban (ref) / rural / camp |
| F7 | Current country | `migrated` | 1 if F5 ≠ F7 |
| F8 | Current area | — | not used |
| F9 | Childhood SES | `ses`, `ses_low`, `ses_high` | low / middle (ref) / high |
| F10–F13 | PTSD symptoms | `ptss1`–`ptss4` | Never 0 … Always 3 |
| F14–F16 | Anxiety | `anx1`–`anx3` | Not at all 0, Sometimes 1, Nearly every day 2 |
| F17 | Occupation influences thinking | `occ_influence` | Yes 1 / No 0; "do not live under occupation" → NA |
| F18 | Occupation outlook | `occ_outlook` | cautious / limited / resilient / other; n/a → NA |
| F19 | Displacement/raids/arrests | `conf_displace` | No 0, Unsure 1, Yes 2 |
| F20 | Prevented from school/work | `conf_prevented` | No 0, Unsure 1, Yes 2 |
| F21 | Feels safe expressing views | `conf_safeexpr` | **reverse:** Yes 0, Unsure 1, No 2 |
| F22 | Education level | `edu_level` | primary / secondary / university |
| F23 | School closures/strikes/violence | `edu_closures` | Never 0 … Always 4 |
| F24 | Missed exams/classes | `edu_missed` | Never 0 … Always 4 |
| F25 | Discrimination in access | `edu_discrim` | Never 0 … Always 4 (**excluded** from factor) |
| F26 | Access to resources | `edu_resources` | **reverse:** Always 0 … Never 4 (**excluded**) |
| F27 | Academic distress | `edu_distress` | Never 0 … Always 4 |
| F28 | Studying dream major (select-all) | `dream_major`; reason indicators `why_afford`, `why_unavailable`, `why_parents`, `why_prestige`, `why_employ` | `dream_major` see rule below (n = 2,234); reason 0/1 indicators defined among university students not studying their dream major (descriptive, Table S8d) |
| F29 | "Other" free text | — | not used |
| F30 | Employment status | `employment` | employed / student / unemployed / other (descriptive, Table S8c) |
| F31 | Biggest barrier | `barrier_structural`, `barrier_type` | see rule below (n = 2,342) |
| F32 | Occupation limits career | `lim_career`, `lim_career_o` | Never 0 … Always 4; n/a → NA |
| F33 | Considered leaving | `mig_intent`, `mig_intent_o` | Never 0 … Always 4; n/a → NA |
| F34 | Hopeful about future | `hope`, `hope_o` | Never 0 … Always 4; n/a → NA |
| F35 | Main barriers (select all) | `barrier_political`; `bar_financial`, `bar_opportunities`, `bar_nepotism`, `bar_notlimited` | each 0/1 if that option selected (descriptive, Table S8e) |
| F36–F38 | Family support | `fam1`–`fam3`, `fam_support`, `fam_support_c` | 1–5 (leading integer of export); latent factor `fsup` in the CFA (script 03) and protective-resource predictor (script 04b) |
| F39–F41 | Coping | `cope1`–`cope3` | Never 0 … Always 4 |
| F42 | Occupation made you resilient | `occ_resilient` | Yes 1 / No 0; n/a → NA |
| F43 | Approach to problems | — | not used |
| F44 | What could help (select-all) | `rem_education`, `rem_mentalhealth`, `rem_community` | each 0/1 if that option selected (descriptive, Table S8f) |

## Exposure groups (from F5), verified sizes

- **opt** — childhood country contains "West Bank" or "Gaza" → n = 639
- **p48** — "Occupied Palestine (48)" → n = 213
- **fcs** — Yemen, Lebanon, Syria, Sudan, Libya, or Iraq → n = 390
- **ref** — any other MENA country → n = 1,338

## Verified derivation rules (reproduce the reported Ns exactly)

- **dream_major** (F28): `NA` if the response contains "do not study at
  university" (189) **or** begins with "Other" (157); otherwise 1 if it contains
  "Yes", else 0. Included n = 2,234 (1,503 yes / 731 no).
- **barrier_structural** (F31): "Lack of resources" or "Limited opportunities"
  → 1; "Fear of failure" or "do not have any barriers" → 0; "Other" → NA.
  n = 2,342.
- **barrier_political** (F35, select-all): 1 if the response contains "political
  situation", else 0. n = 2,580.
- **under_occupation subsample**: `exposure ∈ {p48, opt}` **and** F17 ≠ "I do not
  live under occupation" → n = 809 (624 oPt + 185 '48).
- **conflict_index**: `conf_displace + conf_prevented + conf_safeexpr`, range 0–6.

The exposure-group sizes and the 809-participant subsample are checked by
assertions in `R/01_clean.R`, and the pipeline stops if either differs. The
analytic sample for each outcome is written to `output/flow_counts.csv`.
