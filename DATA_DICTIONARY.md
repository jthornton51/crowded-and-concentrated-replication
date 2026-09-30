# Data used by the replication workflow

The deposited files retain their original column names. This dictionary describes fields consumed by the current workflow. Extra source columns are preserved for provenance; their labels should not be interpreted as additional variables in the fitted models.

## Organization-level input

`data/processed/pz_constituency_measures.csv` contains the original saved analytic rows. Repeated EIN/year rows are deliberately retained so that the filing-selection procedure is reproduced rather than assumed.

| Field | Meaning in this workflow |
|---|---|
| EIN | Organization identifier; pair with TAXYEAR for organization-year selection. |
| TAXYEAR | Tax year; analysis window 2012–2021. |
| FTYPE | Saved analytic form classification: full 990 or 990-EZ. |
| CBSA | Core-based statistical area identifier. |
| ntee_broad_clean | Broad sector classification used to define local markets. |
| private_contributions | Historical gross contribution measure; the name predates the correction. Do not interpret it as net private contributions in this input. |
| public_grants | Historical nonnegative grant measure with blanks coded zero. Selected-policy grant values are reconstructed from GOVT_GRANTS and the filing lookup. |
| client_services | Program service revenue measure. |
| GOVT_GRANTS | Grant amount before blank/negative handling; missingness identifies numeric reporting. |
| TOTREV | Total organizational revenue; used in log(1 + max(TOTREV, 0)) partial correlations. |

`data/external/government_grants.csv` provides source-presence keys EIN/TAXYEAR. Presence indicates a match in the archived extract, not complete reporting or exact-return linkage.

## Filing selection and coverage

`inputs/filing_policy/duplicate_grant_lookup.csv` has one row per affected EIN/TAXYEAR. `selected_raw_grant` records the selected amount. `policy` distinguishes period-matched selection, common amounts with unverified period linkage, and unresolved conflicts. The last category remains missing rather than zero. Other fields document candidate counts, matching granularity, source identifiers, and the preceding selection audit.

`inputs/filing_policy/selected_market_measures.csv` is an independent, earlier gross-contribution market reconstruction used to validate organization counts and filing selection. It is not the corrected R2 outcome dataset.

`inputs/source_coverage/source_determinability_keys.csv` stores EIN, TAXYEAR, `cls`, and `grant`. Classes beginning `determinable` identify reported amounts or blanks resolved by the contribution accounting identity. The coverage sensitivities weight these classifications by gross contributions. These classifications do not establish that other missing grants are positive.

## Controls and historical validation

`output/revisions/NVSQ_R1_2026-07/tables/cbsa_acs5_controls_2012_2021.csv` supplies CBSA/year population, median household income, unemployment and poverty rates, the recorded 2022-dollar income conversion, and logarithms. `log_pop`, `log_mhi_real_2022`, `poverty_rate`, and `unemployment_rate` enter growth models; `pop_total` supplies the density denominator.

The same directory contains `sample_flags.csv`, `coupling_validation_appendix.csv`, and `entry_rerun_coefficients.csv`. These preserve historical eligibility and validation targets. Historical HQ flags are used for R1 validation and historical comparisons; they do not define the R2 higher-reporting sample.

`data/processed/market_level_summaries.csv` supplies historical market counts, HHIs, and revenue totals for reconstruction checks. The current market measures are rebuilt from organization-level data.

## Generated variables

A market-year is CBSA × broad NTEE sector × TAXYEAR. `scenario` identifies original rows, the selected organization-year policy, or reporting organizations only. Revenue shares divide each organization's revenue by that stream's market total. HHI is the sum of squared shares; CR4 is the sum of the four largest shares. Inactive streams are excluded from active-side HHI summaries. Unresolved values remain distinct from inactivity.

Concordance compares revenue shares within a market. `pp`, `pc`, and `qc` designate private–public, private–client, and public–client pairs. Paired gaps are private–public minus private–client on common eligible markets within the stated measure. Bootstrap intervals use 999 draws of CBSAs with a recorded seed.

The growth outcome is (N_t − N_(t−1))/N_(t−1), requiring consecutive years. The model identifier encodes filing scenario, sample restriction, inactivity treatment, and regressor set. `eligible_N` precedes estimator exclusions; `estimated_N` is the fitted sample. Coefficients are in proportional-growth units. Multiplying an HHI coefficient by 10 gives the percentage-point association for a 0.1 HHI increase.
