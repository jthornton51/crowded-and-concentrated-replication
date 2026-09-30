# Script: 00_stack_pz_panel.R
# Title: PZ Data Panel Construction - Stack Annual Files
# Author: Jeremy Thornton
# Date: Created January 2025
# Purpose: Stack individual PZ annual files (2011-2022) into single panel dataset

# Load required packages
library(dplyr)
library(readr)
library(here)
library(purrr)

# Start logging
sink(here("output", "00_pz_data_stacking_log.txt"), append = TRUE, split = TRUE)
cat("=== PZ Data Panel Construction Started ===\n")
cat("Date:", as.character(Sys.time()), "\n")
cat("Project root:", here(), "\n\n")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Define year range
years <- 2011:2022
cat("Years to process:", paste(years, collapse = ", "), "\n")

# Define file path pattern
data_raw_dir <- here("data", "raw")
cat("Raw data directory:", data_raw_dir, "\n")

# Check if directory exists
if (!dir.exists(data_raw_dir)) {
  stop("Raw data directory not found: ", data_raw_dir)
}

# =============================================================================
# FUNCTION TO READ AND VALIDATE INDIVIDUAL FILES
# =============================================================================

read_pz_file <- function(year) {
  
  filename <- paste0("pz_taxyear_", year, ".csv")
  filepath <- file.path(data_raw_dir, filename)
  
  cat("\n--- Processing year:", year, "---\n")
  cat("File:", filename, "\n")
  
  # Check if file exists
  if (!file.exists(filepath)) {
    cat("WARNING: File not found, skipping\n")
    return(NULL)
  }
  
  # Read the file
  tryCatch({
    data <- read_csv(filepath, show_col_types = FALSE)
    
    # Basic validation
    cat("Rows:", nrow(data), "\n")
    cat("Columns:", ncol(data), "\n")
    
    # Check for key variables
    key_vars <- c("F9_00_ORG_EIN", "TAXYEAR")
    missing_vars <- key_vars[!key_vars %in% names(data)]
    
    if (length(missing_vars) > 0) {
      cat("WARNING: Missing key variables:", paste(missing_vars, collapse = ", "), "\n")
    }
    
    # Verify TAXYEAR matches filename
    if ("TAXYEAR" %in% names(data)) {
      taxyears_in_file <- unique(data$TAXYEAR)
      cat("TAXYEAR values in file:", paste(taxyears_in_file, collapse = ", "), "\n")
      
      if (!year %in% taxyears_in_file) {
        cat("WARNING: Expected year", year, "not found in TAXYEAR variable\n")
      }
    }
    
    # Check for duplicates
    if ("F9_00_ORG_EIN" %in% names(data)) {
      n_unique_eins <- n_distinct(data$F9_00_ORG_EIN)
      n_total <- nrow(data)
      if (n_unique_eins < n_total) {
        cat("Note: Duplicate EINs detected -", n_total - n_unique_eins, "duplicates\n")
      }
    }
    
    cat("Successfully loaded\n")
    return(data)
    
  }, error = function(e) {
    cat("ERROR reading file:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# =============================================================================
# READ ALL FILES
# =============================================================================

cat("\n=== READING ALL PZ FILES ===\n")

# Read all files into a list
pz_list <- map(years, read_pz_file)

# Remove NULL entries (failed/missing files)
pz_list <- pz_list[!sapply(pz_list, is.null)]

cat("\nSuccessfully loaded", length(pz_list), "files out of", length(years), "attempted\n")

if (length(pz_list) == 0) {
  stop("ERROR: No files were successfully loaded. Check file paths and names.")
}

# =============================================================================
# CHECK COLUMN CONSISTENCY ACROSS YEARS
# =============================================================================

cat("\n=== CHECKING COLUMN CONSISTENCY ===\n")

# Get column names from each file
all_colnames <- map(pz_list, names)

# Check if all files have the same columns
first_cols <- all_colnames[[1]]
cat("First file has", length(first_cols), "columns\n")

inconsistent_files <- FALSE
for (i in seq_along(all_colnames)) {
  if (!identical(sort(all_colnames[[i]]), sort(first_cols))) {
    inconsistent_files <- TRUE
    cat("\nWARNING: File", i, "has different columns\n")
    
    missing_in_file <- setdiff(first_cols, all_colnames[[i]])
    extra_in_file <- setdiff(all_colnames[[i]], first_cols)
    
    if (length(missing_in_file) > 0) {
      cat("  Missing columns:", paste(head(missing_in_file, 10), collapse = ", "), "\n")
      if (length(missing_in_file) > 10) cat("  ... and", length(missing_in_file) - 10, "more\n")
    }
    
    if (length(extra_in_file) > 0) {
      cat("  Extra columns:", paste(head(extra_in_file, 10), collapse = ", "), "\n")
      if (length(extra_in_file) > 10) cat("  ... and", length(extra_in_file) - 10, "more\n")
    }
  }
}

if (!inconsistent_files) {
  cat("✓ All files have consistent column structure\n")
}

# =============================================================================
# STANDARDIZE COLUMN TYPES BEFORE STACKING
# =============================================================================

cat("\n=== STANDARDIZING COLUMN TYPES ===\n")

# Function to standardize column types
standardize_types <- function(df) {
  
  # Identify numeric columns that should be numeric (financial variables)
  # These include XX_ variables and F9_ variables that contain amounts
  numeric_pattern <- "^(XX_|F9_.*_(TOT|AMT|EOY|CY|NUM|PY))"
  
  numeric_cols <- names(df)[grepl(numeric_pattern, names(df), ignore.case = TRUE)]
  
  # Convert to numeric, handling any character values
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
    }
  }
  
  # Ensure key character columns remain character
  char_cols <- c("F9_00_ORG_EIN", "EIN2", "FTYPE", "SOURCE", "NON_PF_REASON",
                 "F990_ORG_ADDR_STREET", "F990_ORG_ADDR_CITY", "F990_ORG_ADDR_STATE",
                 "F990_ORG_ADDR_ZIP", "CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME",
                 "CENSUS_CBSA_NAME", "ORG_NAME_CURRENT", "ORG_NAME_SEC",
                 "NTEE_NCCS", "NTEEV2", "NTEE_IRS", "CENSUS_CBSA_FIPS")
  
  for (col in char_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  # Ensure TAXYEAR is integer
  if ("TAXYEAR" %in% names(df)) {
    df$TAXYEAR <- as.integer(df$TAXYEAR)
  }
  
  return(df)
}

# Apply standardization to all dataframes
cat("Standardizing", length(pz_list), "dataframes...\n")
pz_list_standardized <- map(pz_list, standardize_types)
cat("✓ Column types standardized\n")

# =============================================================================
# STACK THE DATA
# =============================================================================

cat("\n=== STACKING DATA ===\n")

# Stack all dataframes using bind_rows (handles missing columns gracefully)
pz_panel <- bind_rows(pz_list_standardized)

cat("Panel dataset created\n")
cat("Total rows:", nrow(pz_panel), "\n")
cat("Total columns:", ncol(pz_panel), "\n")

# =============================================================================
# VALIDATE PANEL DATASET
# =============================================================================

cat("\n=== VALIDATING PANEL DATASET ===\n")

# Check year coverage
if ("TAXYEAR" %in% names(pz_panel)) {
  year_coverage <- pz_panel %>%
    count(TAXYEAR) %>%
    arrange(TAXYEAR)
  
  cat("\nYear coverage:\n")
  print(year_coverage)
  
  # Check for gaps
  expected_years <- min(year_coverage$TAXYEAR):max(year_coverage$TAXYEAR)
  missing_years <- setdiff(expected_years, year_coverage$TAXYEAR)
  
  if (length(missing_years) > 0) {
    cat("\nWARNING: Missing years:", paste(missing_years, collapse = ", "), "\n")
  } else {
    cat("\n✓ No gaps in year coverage\n")
  }
}

# Check EIN coverage
if ("F9_00_ORG_EIN" %in% names(pz_panel)) {
  cat("\nUnique organizations (EINs):", n_distinct(pz_panel$F9_00_ORG_EIN), "\n")
  
  # Panel balance
  ein_counts <- pz_panel %>%
    count(F9_00_ORG_EIN) %>%
    count(n, name = "n_orgs") %>%
    arrange(n)
  
  cat("\nPanel balance (observations per EIN):\n")
  print(head(ein_counts, 10))
  
  if (nrow(ein_counts) > 10) {
    cat("... showing first 10 rows only\n")
  }
}

# Check for missing values in key variables
cat("\nMissing values in key variables:\n")
key_vars_check <- c("F9_00_ORG_EIN", "TAXYEAR", "XX_REV_TOT", "XX_EXP_TOT", 
                    "XX_NET_ASS", "CENSUS_CBSA_FIPS", "NTEE_NCCS")

existing_key_vars <- key_vars_check[key_vars_check %in% names(pz_panel)]

missing_summary <- pz_panel %>%
  summarise(across(all_of(existing_key_vars), 
                   ~sum(is.na(.)), 
                   .names = "missing_{.col}"))

print(t(missing_summary))

# =============================================================================
# SAVE PANEL DATASET
# =============================================================================

cat("\n=== SAVING PANEL DATASET ===\n")

# Create processed directory if it doesn't exist
processed_dir <- here("data", "processed")
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
  cat("Created processed directory:", processed_dir, "\n")
}

# Save the panel
output_file <- here(processed_dir, "pz_panel_2011_2022.csv")
write_csv(pz_panel, output_file)

cat("Panel dataset saved to:", output_file, "\n")
cat("File size:", round(file.size(output_file) / 1024^2, 2), "MB\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== PROCESSING SUMMARY ===\n")
cat("Files processed:", length(pz_list), "out of", length(years), "attempted\n")
cat("Total observations:", format(nrow(pz_panel), big.mark = ","), "\n")
cat("Total variables:", ncol(pz_panel), "\n")
cat("Unique organizations:", format(n_distinct(pz_panel$F9_00_ORG_EIN), big.mark = ","), "\n")
cat("Year range:", min(pz_panel$TAXYEAR, na.rm = TRUE), "-", 
    max(pz_panel$TAXYEAR, na.rm = TRUE), "\n")
cat("\nOutput file:", output_file, "\n")
cat("\nPanel construction completed:", as.character(Sys.time()), "\n")

# Stop logging
sink()

cat("\n✓ PZ Panel Construction Complete!\n")
cat("Check the log file for details: output/00_pz_data_stacking_log.txt\n")
