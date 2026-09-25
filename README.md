# Navigating Success Under Occupation

Analysis code for *Navigating Success Under Occupation: The Long-Term Effects of
Childhood Experiences in Palestine on Mental Health, Education, and Career
Prospects.*

The pipeline reproduces every table and figure from the raw survey export. It is
written to be run top to bottom with no manual steps.

## The study

A cross-sectional, non-probability (convenience and snowball) online survey of
2,580 young adults aged 18–25 who grew up in the Middle East and North Africa,
recruited June 2025 – February 2026 through 49 volunteer distributors on social
media. Participants are grouped by childhood country into four groups: MENA
countries not on the World Bank FY26 list of Fragile and Conflict-affected
Situations (FCS) (reference, n = 1,338); FCS-listed MENA countries not under
occupation (n = 390); Palestinian citizens of Israel, "Occupied Palestine (48)"
(n = 213); and the occupied Palestinian territory, West Bank and Gaza (n = 639).
The four groups are treated as distinct contexts, not an ordinal dose ladder.

## Analysis

**Primary estimand:** the association of occupation exposure with (a)
educational disruption and (b) structural and political barrier attribution,
estimated against the FCS stratum as a high-adversity benchmark, and examined
within Palestinians using individually measured conflict experience.

The analysis has two parts:

1. **Between contexts** (script 04): each group compared with the non-FCS
   reference (Comparison A), each Palestinian group compared with the FCS
   stratum (Comparison B), and the occupied Palestinian territory compared with
   Palestinian citizens of Israel (Comparison C). All six contrasts come from a
   single model per outcome.
2. **Within Palestinians** (script 05): individually measured conflict
   experience related to the same outcomes among the 809 Palestinian
   participants who did not select "I do not live under occupation".

Family support is analysed as a protective resource: an adjusted main effect on
the outcomes (Table 5) and an exploratory exposure × family-support interaction
(Table S7). Leave-one-country-out refits check that no single country drives the
oPt vs FCS contrasts (Table S5). Estimates are interpreted through effect sizes
and 95% confidence intervals.

## Data availability

The individual-level survey data are **not included** in this repository. They
record childhood country, refugee-camp residence, arrest and displacement history
and political-expression items for identifiable minority populations, so they are
available from the corresponding author on reasonable request, subject to the
study's ethics approval (see `DATA_LICENSE`). The aggregate outputs (every table
as CSV and Figures 1–3) are included in `output/tables/` and `output/figures/`.

To reproduce the analysis, place the survey export at `data/raw_responses.xlsx`
(sheet *Form Responses 1*, 2,580 rows × 45 columns; column map in
`data/README_data.md`) and run the pipeline below.

## How to cite

Al Jabry LK, Nasher YM, Alghzawi HM, Amin R, Ibrahim FM, Whdan I, Abuznaid OF,
Al-Manasrah AK, El-Sayed N, Hamad S, Alghazal A, Issa Y, Manasra J, Moftah AW.
*Navigating Success Under Occupation* — analysis code (version 1.0.0)
[Software].(https://doi.org/10.5281/zenodo.22949171). Citation metadata are in
`CITATION.cff`.

## How to reproduce

1. Install [R](https://cran.r-project.org/) (≥ 4.2) and, ideally, RStudio.
2. Open `navigating-success-under-occupation.Rproj` (this sets the working
   directory to the project root).
3. Run the whole pipeline:

   ```r
   source("run_all.R")
   ```

   The first run installs any missing packages. Scripts 03–06 fit WLSMV
   structural-equation models and take a few minutes in total.

Outputs are written to `output/`:

- `output/tables/` — one CSV per table
- `output/figures/` — Figures 1–3 as 300-dpi PNGs
- `output/flow_counts.csv`, `output/cherries_rates.csv` — participant flow and
  CHERRIES rates
- `output/Results_Compiled.docx` — all tables and figures in one Word file
- `output/analysis_data.rds` — the analysis dataset (built by script 01; not
  included in the repository)

## What each script produces

| Script | Produces |
|---|---|
| `R/00_setup.R` | packages, paths, constants, helpers, the A/B/C contrast set |
| `R/01_clean.R` | `analysis_data.rds`, `flow_counts.csv`, `cherries_rates.csv` |
| `R/02_descriptives.R` | **Table 1**, **Table S1**, **Table S3** (A/B), **Table S6** |
| `R/02b_supp_descriptives.R` | **Table S8** (A–F): Palestinian-sample descriptive items |
| `R/03_measurement.R` | **Table 2** (A/B), **Table S2** (A/B/C): four-factor CFA |
| `R/04_primary_models.R` | **Table 3** (A/B/C contrasts), **Table S4** (A/B) |
| `R/04b_family_support.R` | **Table 5**, **Table S7** (exploratory interaction) |
| `R/05_within_palestinian.R` | **Table 4** (A/B) |
| `R/06_robustness.R` | **Table S5** (leave-one-country-out) |
| `R/07_figures.R` | **Figure 1**, **Figure 2**, **Figure 3** |
| `R/08_export_word.R` | `Results_Compiled.docx` |

## Measurement

The measurement model (script 03) contains the four latent constructs used in
the analysis, all indicators ordinal, estimated by WLSMV: post-traumatic stress
(4 items), anxiety (3), educational disruption (3) and family support (3). RMSEA
is read alongside CFI and SRMR rather than against a single cut-off, because
categorical WLSMV estimation inflates it (Xia & Yang, 2019) and it is upwardly
biased at small degrees of freedom (Kenny et al., 2015).

The post-traumatic stress and anxiety item sets are abbreviated, rescaled
adaptations of the PCL-5 (4 of 20 items; 4-point frequency scale;
childhood-anchored) and the GAD-7 (3 of 7 items; 3-point scale). They are not
the full instruments and are not diagnostic: no clinical cut-off, prevalence or
caseness is computed. The source and number of items used are listed in Table 2
(Panel B). Educational disruption is author-developed.

Family support (F36–F38) is a unidimensional reflective scale and is validated in
the measurement model. Coping (F39–F41) is not modelled as a latent factor,
because its three items reflect different strategies (support-seeking,
self-distraction, relaxation) rather than interchangeable indicators of one
trait; it is reported descriptively and is not adjusted for.

Two education items are not loaded on the disruption factor and are reported
descriptively in Table S8 (Panel B): F25 (discrimination), whose content
overlaps the exposure, and F26 (access to electricity, internet and resources),
a reverse-worded resource item.

## Repository layout

```
navigating-success-under-occupation/
├── navigating-success-under-occupation.Rproj
├── run_all.R
├── README.md
├── CITATION.cff            citation metadata
├── LICENSE                 code licence (MIT)
├── DATA_LICENSE            data terms
├── .gitignore
├── R/                      00–08, the pipeline
├── data/
│   └── README_data.md      column map F0–F44 and derivation rules
│                           (raw_responses.xlsx not included; available on request)
├── docs/
│   ├── analysis_plan.md    analysis plan
│   └── Questionnaire_and_Coding_Scheme.docx  bilingual questionnaire with coding
└── output/
    ├── tables/             every table as CSV (included)
    └── figures/            Figures 1–3 (included)
```
