# R2 replication verification — 30 September 2026

The complete five-stage analysis was run in a separate candidate directory using only its deposited inputs and the recorded R environment. It did not read analysis data from the original project or overwrite certified repository outputs.

- All five analysis stages completed successfully in approximately 472 seconds.
- Forty reference CSV files agreed, with zero cell discrepancies under absolute/relative tolerance 1e-10. The comparison covered 665,619 cells, including filing-lookup metadata as well as numerical results; this is not a count of independent statistical tests.
- All five regenerated figures were byte-for-byte identical to the figures used in the current manuscript package.
- R1 market, correlation, and regression validation checks embedded in the R2 scripts passed.
- The exhibit map covers 22 table panels and five figures. The manually formatted Word documents were not regenerated.

Machine-readable results are in `verification/certified/comparison_rechecked.json`; stage timing is in `verification/certified/status.json`. The initial comparison incorrectly expected a historical alignment-report file to be produced by the analysis driver. That administrative report is excluded from analysis-output comparisons, with the original comparison retained for transparency. No numerical targets or analysis calculations changed in resolving this check.

The environment lock captures the exact installed dependency set. This verifies reproduction in that environment and from the deposited analysis inputs. A fresh download/restore of the software environment and a complete rebuild of the deposited inputs from original raw files have not been certified. Source-vintage reconstruction and publication checks are documented separately.

The manuscript consistency audit completed immediately before packaging passed 3,599 checks with zero failures or unresolved numerical occurrences. That is a different check: it links document claims to the certified outputs. The replication reported here independently regenerates the outputs.
