# Data provenance and release scope

The package deposits the exact intermediate inputs needed to reproduce the paper's empirical results. SHA-256 hashes in `FILES.csv` identify their versions. Original data-provider downloads may have changed since the research files were acquired; the workflow therefore does not silently replace them with current downloads.

## Sources

- NCCS/Urban Institute PZ nonprofit records supply organization revenues, form classifications, sectors, and geography. The saved geography is already linked; the current analysis does not read HUD crosswalk files separately. The historical `03_link_geography_sector.R` explicitly uses prelinked CBSA values.
- NCCS IRS electronic-filing Part VIII revenue files, named `F9-P08-T00-REVENUE-YYYY.csv`, supply the grant extract and source-record accounting used in filing and coverage classifications. The provider maintains a [catalog and data dictionaries](https://nccs.urban.org/nccs/catalogs/catalog-efile.html).
- Census ACS five-year DP03/DP05 profiles supply contextual controls. The historical controls-construction script records the variables, year-specific input filenames, and inflation factors used.

## Construction history

The organization panel follows stacking, cleaning, sector/geography preparation, and revenue construction. The saved panel intentionally precedes the R2 correction: its historical private-contribution field includes government grants. The portable workflow applies the correction and filing policy during reconstruction.

Filing-policy construction followed a candidate filing audit, matching against PZ tax periods, then grant lookup and market reconstruction. The lookup was stored in the authoring project's output folder rather than the analysis repository. It is included in this archive with an exact source checksum. Source-coverage classifications come from the repository's September 16 decomposability run. Neither component is inferred from the published results.

The certified reference runs are September 30 coupling and combined tables; September 16 growth and coupling sensitivities; and the current manuscript's verified exhibit exports. Their small outputs are deposited under `expected/`. They are comparison targets, not substitutes for re-estimation.

The historical source-construction files under `provenance/upstream_code/` document the transformations but are not a portable raw-download driver. For example, the stacking script expects an earlier raw-file layout, and the ACS mirror contains an old machine path. This release has not tested their complete raw-to-analysis sequence. A raw-data reconstruction extension must identify exact download vintages and adapt/test that sequence separately.

## Rights review

The author reported no known special data agreements on 30 September 2026. NCCS's [current terms, section 3.5](https://nccs.urban.org/nccs/terms/) generally make its cleaned datasets available under ODC-BY 1.0 unless a dataset entry specifies otherwise. Attribute the National Center for Charitable Statistics at the Urban Institute and retain applicable dataset notices. This general statement does not itself establish the historical vintage of each deposited input.

Author-owned code and accompanying documentation use the MIT license. This does not relicense third-party data. Retain source attribution and applicable provider notices when reusing or redistributing the deposited inputs; see `LICENSES.md`.
