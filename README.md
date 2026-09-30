# Crowded and Concentrated? — R2 replication package

This package reproduces the empirical analyses for *Crowded and Concentrated? A Density Paradox in Nonprofit Markets*, R2 manuscript version dated 30 September 2026, by Jeremy Thornton.

**Version r2-2026-09-30.** Complete data and code archive: [Zenodo, DOI 10.5281/zenodo.23067000](https://doi.org/10.5281/zenodo.23067000). Code and documentation: [GitHub](https://github.com/jthornton51/crowded-and-concentrated-replication). Download the complete Zenodo archive to run the analysis; a GitHub source checkout alone omits the large analysis inputs.

## Reproduce the analysis

Install Python 3 and R. The tested R version is 4.6.0; direct package versions are data.table 1.18.4, fixest 0.14.2, diptest 0.77-2, and ggplot2 4.0.3. The root `renv.lock` records the exact 29-package dependency set captured from the successful R2 run. The separate historical lock under `provenance/` describes an earlier project environment and must not be used as the R2 installation lock.

To install those versions into a local library, first install `renv` in R with `install.packages("renv", repos="https://cloud.r-project.org")`, then run `Rscript setup_environment.R` from the package root. Add `--r-library r-library` to the Python commands below. Package installation requires internet access and may require compilation tools. The deposited-input reproduction was tested with the installed packages listed in the lock; restoration from a fresh internet download has not yet been certified.

Extract the full replication archive into a writable folder. From that folder, run:

```text
python run_replication.py --preflight
python run_replication.py
```

If Rscript is not on your system path, add `--rscript` followed by the path to its executable, quoted if it contains spaces. No network access is required during the analysis once the software and archive are installed.

The driver verifies input checksums and required packages, reconstructs concentration and concordance from the deposited organization-level records, estimates the growth models and sensitivity analyses, and generates the exhibit data and five figures. Results are compared with the certified reference CSVs using a tolerance of 1e-10 (absolute or relative). Original reference outputs remain unchanged.

Each run creates a timestamped directory under `verification/` containing progress, logs, comparisons, and the locations of generated outputs. Successful comparison creates `SUCCESS.txt`. Figure byte comparisons are reported separately because rendering may differ across software environments. A failed analysis or numerical comparison exits with an error.

## What is reproduced

`EXHIBIT_MAP.csv` maps all 22 table panels and five figures in the current manuscript package to the generating scripts, output files, and selection rules. Additional machine-readable results support the main-text estimates and sensitivity discussion. There is no Appendix Table B2 in the current paper; Appendix Figure B2 is retained.

The run sequence is:

1. `r2_12_corrected_coupling.R`: filing-selection policy, net-private revenue, market concentration, paired correlations, and bootstrap intervals.
2. `r2_13_corrected_net_growth.R`: lagged net-growth specifications and fitted sample sizes.
3. `r2_14_coupling_sensitivities.R`: common-sample, floor, and period-verification comparisons.
4. `r2_15_combined_descriptives_and_tables.R`: headline density/concentration, distributional diagnostics, and model display data.
5. `r2_16_submission_exhibits.R`: reporting accounting, sector summaries, concentration ratios, health comparisons, and figures.

The first four scripts preserve the verified repository code. The fifth preserves the independently verified exhibit calculations and replaces machine-specific paths with command-line arguments. It also reproduces the count/density reconciliation. The package does not regenerate the manually edited Word manuscript or response letters.

## Data and scope

All inputs required by the five-stage analysis are included in the full archive. These include the original organization-level panel before the R2 correction, grant extract, filing-policy lookup, source-coverage classifications, historical validation inputs, and ACS controls. See `DATA_DICTIONARY.md`, `DATA_PROVENANCE.md`, and `FILES.csv`.

This is replication **from deposited analysis inputs**. It does not yet claim a tested rebuild of those inputs from newly downloaded raw files. Historical upstream scripts are supplied under `provenance/upstream_code/` with their status explained in `DATA_PROVENANCE.md`. Their paths and source vintages differ from the portable analysis workflow. Run the top-level driver rather than executing those historical files in place.

The original panel's `private_contributions` column contains gross contributions. The R2 code subtracts government grants and floors negative differences at zero. Blanks follow the manuscript's blank-as-zero convention; explicitly unresolved filing conflicts remain unknown. The higher-reporting sensitivity uses numeric grant-field reporting in 2020–2021, rather than the inherited positive-grant participation flags used in historical validation.

## Archive and citation

Thornton, Jeremy (2026). *Crowded and Concentrated? A Density Paradox in Nonprofit Markets — R2 replication package* (r2-2026-09-30). Zenodo. https://doi.org/10.5281/zenodo.23067000

The GitHub release tag is `r2-2026-09-30`. `CITATION.cff` provides machine-readable citation metadata. The full archive's `FILES.csv` and `SHA256SUMS.txt` identify its contents independently of repository history. Author-owned code and documentation use the MIT license; third-party data retain their provider terms, as described in `LICENSES.md`.
