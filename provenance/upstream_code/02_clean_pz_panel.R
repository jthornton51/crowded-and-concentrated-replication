# Script: 02_clean_pz_panel.R
# Title: Nonprofit Density Analysis - PZ Data Loading and Cleaning
# Author: Jeremy Thornton
# Date: Created January 2025
# Purpose: Load PZ panel, clean data issues, create analysis-ready dataset

# Load required packages
library(dplyr)
library(readr)
library(here)

# Start logging
sink(here("output", "01_pz_data_loading_cleaning_log.txt"), append = TRUE, split = TRUE)
cat("=== PZ Data Loading and Cleaning Started ===\n")
cat("Date:", as.character(Sys.time()), "\n")
cat("Project root:", here(), "\n\n")

# =============================================================================
# LOAD PANEL DATA
# =============================================================================

cat("=== LOADING PZ PANEL ===\n")
pz_panel_full <- read_csv(here("data", "processed", "pz_panel_2011_2022.csv"), 
                          show_col_types = FALSE)

cat("Full panel dimensions:", nrow(pz_panel_full), "rows x", ncol(pz_panel_full), "columns\n")
cat("Years covered:", min(pz_panel_full$TAXYEAR), "-", max(pz_panel_full$TAXYEAR), "\n\n")

# =============================================================================
# STEP 1: REMOVE DUPLICATE COLUMNS
# =============================================================================

cat("=== STEP 1: REMOVING DUPLICATE COLUMNS ===\n")

# Based on diagnostic analysis, all duplicate columns are identical
# Keep the first occurrence, drop the second

pz_clean <- pz_panel_full %>%
  select(-c(
    `F9_08_REV_PROG_TOT_TOT...139`,  # Keep ...76
    `F9_10_NAFB_TOT_EOY...145`,       # Keep ...79
    `F9_08_REV_TOT_TOT...141`,        # Keep ...140
    `F9_09_EXP_TOT_TOT...143`         # Keep ...142
  ))

# Rename the kept columns to remove ...suffix
pz_clean <- pz_clean %>%
  rename(
    F9_08_REV_PROG_TOT_TOT = `F9_08_REV_PROG_TOT_TOT...76`,
    F9_10_NAFB_TOT_EOY = `F9_10_NAFB_TOT_EOY...79`,
    F9_08_REV_TOT_TOT = `F9_08_REV_TOT_TOT...140`,
    F9_09_EXP_TOT_TOT = `F9_09_EXP_TOT_TOT...142`
  )

cat("Duplicate columns removed\n")
cat("New column count:", ncol(pz_clean), "\n\n")

# =============================================================================
# STEP 2: DEDUPLICATE EINs WITHIN YEARS
# =============================================================================

cat("=== STEP 2: DEDUPLICATING EINs ===\n")

# Count duplicates before
duplicates_before <- pz_clean %>%
  group_by(TAXYEAR, F9_00_ORG_EIN) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  summarise(total_duplicates = sum(n - 1))

cat("Duplicate observations before deduplication:", duplicates_before$total_duplicates, "\n")

# Deduplication strategy:
# 1. Prioritize form type: 990-PC > 990-EZ > 990-PF
# 2. Within same form type, keep most recent tax period end date
# 3. If still tied, keep first observation

pz_clean <- pz_clean %>%
  mutate(
    # Create priority score for form type
    form_priority = case_when(
      FTYPE == "990-PC" ~ 1,
      FTYPE == "990-EZ" ~ 2,
      FTYPE == "990-PF" ~ 3,
      TRUE ~ 4  # Any other form type
    ),
    # Convert tax period end date to numeric for sorting
    tax_period_end_numeric = as.numeric(F9_00_TAX_PERIOD_END_DATE)
  ) %>%
  # Sort by priority
  arrange(TAXYEAR, F9_00_ORG_EIN, form_priority, desc(tax_period_end_numeric)) %>%
  # Keep first observation per EIN-YEAR
  group_by(TAXYEAR, F9_00_ORG_EIN) %>%
  slice(1) %>%
  ungroup() %>%
  # Remove temporary variables
  select(-form_priority, -tax_period_end_numeric)

# Count after
cat("Observations after deduplication:", nrow(pz_clean), "\n")
cat("Observations removed:", nrow(pz_panel_full) - nrow(pz_clean), "\n\n")

# =============================================================================
# STEP 3: CREATE COMPOSITE NTEE CODE
# =============================================================================

cat("=== STEP 3: CREATING COMPOSITE NTEE CODE ===\n")

# Based on diagnostic: NTEE_IRS has 99.66% coverage (best)
# Create composite using coalesce for maximum coverage

pz_clean <- pz_clean %>%
  mutate(
    NTEE_COMPOSITE = coalesce(NTEE_IRS, NTEE_NCCS, NTEEV2)
  )

# Check coverage
ntee_coverage <- pz_clean %>%
  summarise(
    total = n(),
    has_ntee_irs = sum(!is.na(NTEE_IRS)),
    has_ntee_composite = sum(!is.na(NTEE_COMPOSITE)),
    pct_composite = round(has_ntee_composite / total * 100, 2)
  )

cat("NTEE coverage:\n")
cat("  NTEE_IRS alone:", ntee_coverage$has_ntee_irs, 
    "(", round(ntee_coverage$has_ntee_irs/ntee_coverage$total*100, 2), "%)\n")
cat("  NTEE_COMPOSITE:", ntee_coverage$has_ntee_composite,
    "(", ntee_coverage$pct_composite, "%)\n\n")

# =============================================================================
# STEP 4: FILTER TO CORE ANALYSIS YEARS (2012-2021)
# =============================================================================

cat("=== STEP 4: FILTERING TO CORE YEARS (2012-2021) ===\n")

pz_core <- pz_clean %>%
  filter(TAXYEAR >= 2012 & TAXYEAR <= 2021)

cat("Observations before filtering:", nrow(pz_clean), "\n")
cat("Observations after filtering:", nrow(pz_core), "\n")
cat("Years coverage:\n")
print(table(pz_core$TAXYEAR))
cat("\n")

# =============================================================================
# STEP 5: VARIABLE SELECTION AND RENAMING
# =============================================================================

cat("=== STEP 5: SELECTING AND RENAMING VARIABLES ===\n")

# Keep only variables needed for analysis
# Based on your original GTDC project needs

pz_core_final <- pz_core %>%
  select(
    # Identifiers
    EIN = F9_00_ORG_EIN,
    TAXYEAR,
    FTYPE,
    
    # Geographic variables (already in PZ!)
    CITY = F990_ORG_ADDR_CITY,
    STATE = F990_ORG_ADDR_STATE,
    ZIP = F990_ORG_ADDR_ZIP,
    CBSA = CENSUS_CBSA_FIPS,
    CBSA_NAME = CENSUS_CBSA_NAME,
    COUNTY = CENSUS_COUNTY_NAME,
    
    # NTEE codes
    NTEE = NTEE_COMPOSITE,
    NTEE_IRS,
    NTEE_NCCS,
    
    # Financial variables (using calculated XX_ versions where available)
    TOTREV = XX_REV_TOT,
    TOTEXP = XX_EXP_TOT,
    TOTASSETS = F9_10_ASSET_TOT_EOY,
    TOTLIAB = F9_10_LIAB_TOT_EOY,
    NETASSETS = XX_NET_ASS,
    
    # Revenue detail (for constituency analysis)
    # Note: Some XX_ variables may not exist - using F9_ equivalents
    REV_CONTRIBUTIONS = XX_REV_CONTR,
    REV_PROGRAM = F9_08_REV_PROG_TOT_TOT,  # XX_REV_PROG not in data
    REV_INVESTMENT = XX_REV_INVEST,
    
    # Additional useful variables
    TAX_PERIOD_END = F9_00_TAX_PERIOD_END_DATE,
    EFILE = EFILE_X,
    
    # BMF variables
    BMF_AFFILIATION_CODE,
    BMF_ASSET_CODE,
    BMF_CLASSIFICATION_CODE,
    BMF_FOUNDATION_CODE,
    BMF_ORGANIZATION_CODE,
    ORG_NAME = ORG_NAME_CURRENT,
    ORG_RULING_DATE,
    ORG_RULING_YEAR
  )

cat("Final variable count:", ncol(pz_core_final), "\n")
cat("Variables selected:\n")
print(names(pz_core_final))
cat("\n")

# =============================================================================
# STEP 6: DATA QUALITY CHECKS
# =============================================================================

cat("=== STEP 6: DATA QUALITY CHECKS ===\n")

# Check for basic data quality issues
quality_checks <- pz_core_final %>%
  summarise(
    total_obs = n(),
    missing_ein = sum(is.na(EIN)),
    missing_year = sum(is.na(TAXYEAR)),
    missing_cbsa = sum(is.na(CBSA)),
    missing_ntee = sum(is.na(NTEE)),
    missing_totrev = sum(is.na(TOTREV)),
    missing_totexp = sum(is.na(TOTEXP)),
    negative_revenue = sum(TOTREV < 0, na.rm = TRUE),
    negative_expenses = sum(TOTEXP < 0, na.rm = TRUE)
  )

cat("Data quality summary:\n")
print(t(quality_checks))
cat("\n")

# Panel balance
cat("Panel balance (observations per EIN):\n")
ein_balance <- pz_core_final %>%
  count(EIN) %>%
  count(n, name = "n_orgs") %>%
  arrange(n)

print(head(ein_balance, 10))
if (nrow(ein_balance) > 10) {
  cat("... showing first 10 rows only\n")
}
cat("\n")

# Geographic coverage
cat("Geographic coverage:\n")
cat("  Unique CBSAs:", n_distinct(pz_core_final$CBSA, na.rm = TRUE), "\n")
cat("  Unique states:", n_distinct(pz_core_final$STATE, na.rm = TRUE), "\n")
cat("  Organizations with CBSA:", sum(!is.na(pz_core_final$CBSA)), 
    "(", round(sum(!is.na(pz_core_final$CBSA))/nrow(pz_core_final)*100, 2), "%)\n\n")

# NTEE coverage
cat("NTEE coverage by year:\n")
ntee_by_year <- pz_core_final %>%
  group_by(TAXYEAR) %>%
  summarise(
    total = n(),
    has_ntee = sum(!is.na(NTEE)),
    pct_has_ntee = round(has_ntee/total*100, 2)
  )
print(ntee_by_year)
cat("\n")

# =============================================================================
# STEP 7: SAVE CLEANED DATASET
# =============================================================================

cat("=== STEP 7: SAVING CLEANED DATASET ===\n")

output_file <- here("data", "processed", "pz_core_2012_2021_clean.csv")
write_csv(pz_core_final, output_file)

cat("Cleaned dataset saved to:", output_file, "\n")
cat("File size:", round(file.size(output_file) / 1024^2, 2), "MB\n\n")

# =============================================================================
# SUMMARY STATISTICS FOR DOCUMENTATION
# =============================================================================

cat("=== FINAL SUMMARY ===\n")
cat("Data source: PZ Panel (2011-2022)\n")
cat("Analysis period: 2012-2021\n")
cat("Final observations:", format(nrow(pz_core_final), big.mark = ","), "\n")
cat("Unique organizations:", format(n_distinct(pz_core_final$EIN), big.mark = ","), "\n")
cat("Variables retained:", ncol(pz_core_final), "\n")
cat("Geographic coverage:", sum(!is.na(pz_core_final$CBSA)), "observations with CBSA\n")
cat("NTEE coverage:", sum(!is.na(pz_core_final$NTEE)), "observations with NTEE\n")
cat("\nData cleaning steps applied:\n")
cat("  1. Removed 4 duplicate columns (100% identical)\n")
cat("  2. Deduplicated EINs within years (kept most recent 990-PC filing)\n")
cat("  3. Created composite NTEE code (99.7% coverage)\n")
cat("  4. Filtered to 2012-2021 analysis period\n")
cat("  5. Selected and renamed variables for analysis compatibility\n")
cat("\nOutput: Clean, analysis-ready dataset\n")
cat("Next step: Run script 02 for geographic and sector processing\n")
cat("\nProcessing completed:", as.character(Sys.time()), "\n")

# Stop logging
sink()

# Display completion message
cat("\n✓ Data Loading and Cleaning Complete!\n")
cat("Clean dataset saved:", nrow(pz_core_final), "observations\n")
cat("File:", "data/processed/pz_core_2012_2021_clean.csv\n")
cat("Check log for details: output/01_pz_data_loading_cleaning_log.txt\n")
