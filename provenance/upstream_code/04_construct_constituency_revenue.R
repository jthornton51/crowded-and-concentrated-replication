# Script: 04_construct_constituency_revenue.R
# Title: Nonprofit Density Analysis - Constituency Revenue Measures (PZ Data - CORRECTED)
# Author: Jeremy Thornton  
# Date: Created January 2025
# Purpose: Build constituency participation measures following Form 990 logic

# Load required packages
library(dplyr)
library(readr)
library(here)

# Start logging
sink(here("output", "03_constituency_measures_log.txt"), append = FALSE, split = TRUE)
cat("=== CONSTITUENCY REVENUE MEASURES STARTED ===\n")
cat("Date:", as.character(Sys.time()), "\n")
cat("Project root:", here(), "\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("=== LOADING DATA ===\n")
pz_data <- read_csv(here("data", "processed", "pz_core_with_geo_sector.csv"), 
                    show_col_types = FALSE)

cat("PZ dataset dimensions:", nrow(pz_data), "rows x", ncol(pz_data), "columns\n\n")

# =============================================================================
# LOAD GOVERNMENT GRANTS DATA (IF AVAILABLE)
# =============================================================================

cat("=== CHECKING FOR GOVERNMENT GRANTS DATA ===\n")

govt_grants_file <- here("data", "external", "government_grants.csv")

if(file.exists(govt_grants_file)) {
  cat("✓ Government grants file found - will use Approach 1 (three constituencies)\n\n")
  
  govt_grants <- read_csv(govt_grants_file, show_col_types = FALSE)
  cat("Government grants dimensions:", nrow(govt_grants), "rows x", ncol(govt_grants), "columns\n\n")
  
  # Merge with PZ data - ensure EIN types match
  pz_data <- pz_data %>%
    mutate(EIN = as.numeric(EIN)) %>%
    left_join(
      govt_grants %>% mutate(EIN = as.numeric(EIN)),
      by = c("EIN", "TAXYEAR")
    )
  
  cat("After merge dimensions:", nrow(pz_data), "rows x", ncol(pz_data), "columns\n")
  
  govt_merge_summary <- pz_data %>%
    summarise(
      total_obs = n(),
      has_govt_grants = sum(!is.na(GOVT_GRANTS)),
      positive_govt_grants = sum(!is.na(GOVT_GRANTS) & GOVT_GRANTS > 0),
      pct_with_data = round(has_govt_grants / total_obs * 100, 1),
      pct_positive = round(positive_govt_grants / total_obs * 100, 1)
    )
  
  cat("Government grants merge summary:\n")
  print(govt_merge_summary)
  cat("\n")
  
  use_approach <- "three_sided"
  
} else {
  cat("✗ Government grants file NOT found\n")
  cat("  Will use Approach 2 (two constituencies following 990 logic)\n\n")
  
  use_approach <- "two_sided"
}

# =============================================================================
# CREATE NTEE CLEAN CATEGORIES
# =============================================================================

cat("=== CREATING CLEAN NTEE CATEGORIES ===\n")

pz_data <- pz_data %>%
  mutate(
    # Create human-readable broad categories from ntee_broad
    ntee_broad_clean = case_when(
      ntee_broad == "A" ~ "A_Arts",
      ntee_broad == "B" ~ "B_Education", 
      ntee_broad == "C" ~ "CD_Environment_Animals",
      ntee_broad == "D" ~ "CD_Environment_Animals",
      ntee_broad == "E" ~ "EFGH_Health",
      ntee_broad == "F" ~ "EFGH_Health",
      ntee_broad == "G" ~ "EFGH_Health", 
      ntee_broad == "H" ~ "EFGH_Health",
      ntee_broad == "I" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "J" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "K" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "L" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "M" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "N" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "O" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "P" ~ "IJKLMNOP_Human_Services",
      ntee_broad == "Q" ~ "Q_International",
      ntee_broad == "R" ~ "RSTUVW_Public_Benefit",
      ntee_broad == "S" ~ "RSTUVW_Public_Benefit",
      ntee_broad == "T" ~ "RSTUVW_Public_Benefit",
      ntee_broad == "U" ~ "RSTUVW_Public_Benefit", 
      ntee_broad == "V" ~ "RSTUVW_Public_Benefit",
      ntee_broad == "W" ~ "RSTUVW_Public_Benefit",
      ntee_broad == "X" ~ "X_Religion",
      ntee_broad == "Y" ~ "Y_Mutual_Benefit",
      ntee_broad == "Z" ~ "Z_Unknown",
      is.na(ntee_broad) ~ "Z_Unknown",
      TRUE ~ "Z_Unknown"
    )
  )

# Check NTEE distribution
ntee_check <- pz_data %>%
  count(ntee_broad_clean, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat("NTEE broad category distribution:\n")
print(ntee_check)
cat("\n")

# =============================================================================
# REVENUE CATEGORIZATION APPROACH
# =============================================================================

cat("=== REVENUE CATEGORIZATION APPROACH ===\n")

if(use_approach == "three_sided") {
  cat("APPROACH 1: Three-Constituency Model\n")
  cat("When government grants are separately available:\n\n")
  
  cat("Constituencies:\n")
  cat("  1. Private contributions = REV_CONTRIBUTIONS\n")
  cat("     - PZ data: Already net of government grants per data dictionary\n")
  cat("  2. Public grants = GOVERNGRANTS (from external merge)\n")
  cat("     - Government grants from all sources\n")
  cat("  3. Client services = REV_PROGRAM\n")
  cat("     - Program service revenue (fee-for-service)\n\n")
  
  cat("Three-sided markets can be identified:\n")
  cat("  - Donors (private contributions)\n")
  cat("  - Government (public grants)\n")
  cat("  - Clients (earned revenue)\n\n")
  
} else {
  cat("APPROACH 2: Two-Constituency Model (Following Form 990 Structure)\n")
  cat("When government grants are NOT separately available:\n\n")
  
  cat("Form 990 Part VIII Revenue Structure:\n")
  cat("  Line 1: Total Contributions (includes govt grants as subset)\n")
  cat("  Line 2: Program Service Revenue (earned, not contributed)\n\n")
  
  cat("CORRECTED Constituencies:\n")
  cat("  1. Contributed Revenue = REV_CONTRIBUTIONS\n")
  cat("     - Includes: Private donations + Government grants (lumped)\n")
  cat("     - This is the 990's 'Total Contributions' concept\n")
  cat("  2. Earned Revenue = REV_PROGRAM\n")
  cat("     - Program service revenue (fee-for-service)\n")
  cat("     - This is the 990's 'Program Service Revenue' concept\n\n")
  
  cat("CRITICAL: Following IRS Form 990 logic, government grants are a SUBSET of\n")
  cat("          contributions (Part VIII Line 1e), NOT grouped with program revenue.\n")
  cat("          Therefore, when govt grants aren't separately available, we lump\n")
  cat("          private + public TOGETHER as 'contributed revenue'.\n\n")
  
  cat("Two-sided markets can be identified:\n")
  cat("  - Contributors (donors + government lumped)\n")
  cat("  - Clients (earned revenue)\n\n")
  
  cat("Limitation: Cannot distinguish private from public funders\n\n")
}

# =============================================================================
# BUILD CONSTITUENCY REVENUE MEASURES
# =============================================================================

cat("=== BUILDING CONSTITUENCY REVENUE MEASURES ===\n")

# Filter to analysis-ready organizations
analysis_data <- pz_data %>%
  filter(
    !is.na(CBSA),
    !is.na(ntee_broad_clean),
    ntee_broad_clean != "Z_Unknown",
    !is.na(TAXYEAR),
    !is.na(TOTREV),
    TOTREV > 0
  )

cat("Organizations after filtering:", nrow(analysis_data), "\n")
cat("Years covered:", min(analysis_data$TAXYEAR), "-", max(analysis_data$TAXYEAR), "\n")
cat("Unique organizations:", n_distinct(analysis_data$EIN), "\n\n")

# Create constituency measures based on approach
if(use_approach == "three_sided") {
  
  cat("Creating three-constituency measures...\n\n")
  
  constituency_data <- analysis_data %>%
    mutate(
      # Constituency 1: Private contributions
      # REV_CONTRIBUTIONS already excludes government per PZ data dictionary
      private_contributions = pmax(0, coalesce(REV_CONTRIBUTIONS, 0)),
      
      # Constituency 2: Public grants (from external merge)
      public_grants = pmax(0, coalesce(GOVT_GRANTS, 0)),
      
      # Constituency 3: Client services (earned revenue)
      client_services = pmax(0, coalesce(REV_PROGRAM, 0))
    )
  
  # Validation
  validation_summary <- constituency_data %>%
    summarise(
      n_obs = n(),
      n_orgs = n_distinct(EIN),
      
      # Participation rates
      n_private = sum(private_contributions > 0),
      n_public = sum(public_grants > 0),
      n_client = sum(client_services > 0),
      
      # Mean values (conditional on >0)
      mean_private = mean(private_contributions[private_contributions > 0], na.rm = TRUE),
      mean_public = mean(public_grants[public_grants > 0], na.rm = TRUE),
      mean_client = mean(client_services[client_services > 0], na.rm = TRUE),
      
      # Percentage with activity
      pct_private = round(n_private / n_obs * 100, 1),
      pct_public = round(n_public / n_obs * 100, 1),
      pct_client = round(n_client / n_obs * 100, 1)
    )
  
  cat("Three-constituency validation:\n")
  print(validation_summary)
  
} else {
  
  cat("Creating two-constituency measures (following 990 logic)...\n\n")
  
  constituency_data <- analysis_data %>%
    mutate(
      # Constituency 1: Contributed revenue (private + public lumped)
      # This follows Form 990 Part VIII Line 1 (Total Contributions)
      # Government grants are a SUBSET of contributions, not separate
      contributed_revenue = pmax(0, coalesce(REV_CONTRIBUTIONS, 0)),
      
      # Constituency 2: Earned revenue (client services)  
      # This follows Form 990 Part VIII Line 2 (Program Service Revenue)
      earned_revenue = pmax(0, coalesce(REV_PROGRAM, 0))
    )
  
  # Validation
  validation_summary <- constituency_data %>%
    summarise(
      n_obs = n(),
      n_orgs = n_distinct(EIN),
      
      # Participation rates
      n_contributed = sum(contributed_revenue > 0),
      n_earned = sum(earned_revenue > 0),
      
      # Mean values (conditional on >0)
      mean_contributed = mean(contributed_revenue[contributed_revenue > 0], na.rm = TRUE),
      mean_earned = mean(earned_revenue[earned_revenue > 0], na.rm = TRUE),
      
      # Percentage with activity
      pct_contributed = round(n_contributed / n_obs * 100, 1),
      pct_earned = round(n_earned / n_obs * 100, 1)
    )
  
  cat("Two-constituency validation (following 990 structure):\n")
  print(validation_summary)
}

cat("\n")

# =============================================================================
# NTEE CATEGORY DISTRIBUTION
# =============================================================================

cat("=== NTEE CATEGORY DISTRIBUTION ===\n")

ntee_dist <- constituency_data %>%
  count(ntee_broad_clean, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat("NTEE broad category distribution:\n")
print(ntee_dist)
cat("\n")

# =============================================================================
# SECTOR-LEVEL PATTERNS
# =============================================================================

cat("=== SECTOR-LEVEL REVENUE PATTERNS ===\n")

if(use_approach == "three_sided") {
  sector_patterns <- constituency_data %>%
    group_by(ntee_broad_clean) %>%
    summarise(
      n_orgs = n(),
      
      # Participation rates
      pct_private = round(mean(private_contributions > 0) * 100, 1),
      pct_public = round(mean(public_grants > 0) * 100, 1),
      pct_client = round(mean(client_services > 0) * 100, 1),
      
      # Mean revenues (conditional on >0)
      mean_private = round(mean(private_contributions[private_contributions > 0], na.rm = TRUE)),
      mean_public = round(mean(public_grants[public_grants > 0], na.rm = TRUE)),
      mean_client = round(mean(client_services[client_services > 0], na.rm = TRUE)),
      
      # Revenue shares
      total_revenue = sum(private_contributions + public_grants + client_services, na.rm = TRUE),
      share_private = round(sum(private_contributions, na.rm = TRUE) / total_revenue * 100, 1),
      share_public = round(sum(public_grants, na.rm = TRUE) / total_revenue * 100, 1),
      share_client = round(sum(client_services, na.rm = TRUE) / total_revenue * 100, 1),
      
      .groups = "drop"
    ) %>%
    arrange(desc(n_orgs))
  
} else {
  sector_patterns <- constituency_data %>%
    group_by(ntee_broad_clean) %>%
    summarise(
      n_orgs = n(),
      
      # Participation rates
      pct_contributed = round(mean(contributed_revenue > 0) * 100, 1),
      pct_earned = round(mean(earned_revenue > 0) * 100, 1),
      
      # Mean revenues (conditional on >0)
      mean_contributed = round(mean(contributed_revenue[contributed_revenue > 0], na.rm = TRUE)),
      mean_earned = round(mean(earned_revenue[earned_revenue > 0], na.rm = TRUE)),
      
      # Revenue shares (following 990 structure)
      total_revenue = sum(contributed_revenue + earned_revenue, na.rm = TRUE),
      share_contributed = round(sum(contributed_revenue, na.rm = TRUE) / total_revenue * 100, 1),
      share_earned = round(sum(earned_revenue, na.rm = TRUE) / total_revenue * 100, 1),
      
      .groups = "drop"
    ) %>%
    arrange(desc(n_orgs))
}

cat("Sector revenue patterns:\n")
print(sector_patterns)

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

write_csv(constituency_data, 
          here("data", "processed", "pz_constituency_measures.csv"))
write_csv(validation_summary, 
          here("output", "constituency_validation_summary.csv"))
write_csv(sector_patterns, 
          here("output", "sector_revenue_patterns.csv"))

cat("\nFiles saved:\n")
cat("  - pz_constituency_measures.csv (", nrow(constituency_data), "organizations)\n")
cat("  - constituency_validation_summary.csv\n")
cat("  - sector_revenue_patterns.csv\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== CONSTITUENCY MEASURES SUMMARY ===\n")
cat("Approach:", use_approach, "\n")
cat("Organizations in analysis:", nrow(constituency_data), "\n")
cat("Unique EINs:", n_distinct(constituency_data$EIN), "\n")
cat("Year range:", min(constituency_data$TAXYEAR), "-", max(constituency_data$TAXYEAR), "\n")

if(use_approach == "three_sided") {
  cat("\nThree constituencies identified:\n")
  cat("  1. Private contributions (donors)\n")
  cat("  2. Public grants (government)\n")
  cat("  3. Client services (earned revenue)\n")
} else {
  cat("\nTwo constituencies identified (following Form 990 structure):\n")
  cat("  1. Contributed revenue (private + public lumped per 990 logic)\n")
  cat("  2. Earned revenue (client services)\n")
  cat("\nNote: This follows IRS Form 990 Part VIII categorization where\n")
  cat("      government grants are a subset of total contributions.\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Completed:", as.character(Sys.time()), "\n")

sink()
