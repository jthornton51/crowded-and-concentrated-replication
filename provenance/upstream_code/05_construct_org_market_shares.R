# ==============================================================================
# Script: 05_construct_org_market_shares.R
# Title: Organization-Level Market Shares
# Purpose: Calculate each organization's share of market revenue by constituency
#
# Position in Data Construction Pipeline:
#   Script 01: Deduplication & year filter
#   Script 02: Geographic & sector linkage
#   Script 03: THIS SCRIPT - Org-level shares (NEW)
#   Script 04: Market-level constituency measures & HHI
#   Script 05: Market aggregation
#   Script 06: Analysis
#
# Key Output: org_level_market_shares.csv
#   - Each org's % of market revenue on each constituency
#   - Used by Script 04 to calculate HHI
#   - Used by Script 06 to measure platform coupling
# ==============================================================================

library(tidyverse)
library(here)

# ==============================================================================
# 0. SETUP AND LOGGING
# ==============================================================================

log_file <- here("logs", paste0("04_org_market_shares_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
sink(log_file, split = TRUE)

cat(strrep("=", 80), "\n")
cat("SCRIPT 04: ORGANIZATION-LEVEL MARKET SHARES\n")
cat(strrep("=", 80), "\n\n")

cat("PURPOSE: Calculate org-level market shares by constituency\n\n")

cat("WORKFLOW CONTEXT:\n")
cat("  INPUT:  pz_constituency_measures.csv (from Script 04 - to be renamed)\n")
cat("  OUTPUT: org_level_market_shares.csv (for Script 04 HHI calculation)\n\n")

# ==============================================================================
# 1. LOAD ORG-LEVEL DATA
# ==============================================================================

cat("SECTION 1: LOADING DATA\n")
cat(strrep("-", 80), "\n\n")

# NOTE: This currently comes from what's called Script 04
# In a proper workflow, this would be the output of Script 02
org_data <- read_csv(
  here("data", "processed", "pz_constituency_measures.csv"),
  show_col_types = FALSE
)

cat("✓ Loaded:", format(nrow(org_data), big.mark = ","), "org-years\n")
cat("  Unique EINs:", format(n_distinct(org_data$EIN), big.mark = ","), "\n")
cat("  Unique CBSAs:", n_distinct(org_data$CBSA), "\n")
cat("  Unique sectors:", n_distinct(org_data$ntee_broad_clean), "\n")
cat("  Years:", paste(range(org_data$TAXYEAR), collapse = "-"), "\n")
cat("  Unique markets:", format(n_distinct(org_data$CBSA, org_data$ntee_broad_clean), big.mark = ","), "\n\n")

# ==============================================================================
# 2. CALCULATE MARKET TOTALS
# ==============================================================================

cat("SECTION 2: CALCULATING MARKET TOTALS\n")
cat(strrep("-", 80), "\n\n")

cat("Aggregating total revenue by market and constituency...\n\n")

market_totals <- org_data %>%
  group_by(CBSA, ntee_broad_clean, TAXYEAR) %>%
  summarise(
    # Total revenue by constituency in this market
    market_total_private = sum(private_contributions, na.rm = TRUE),
    market_total_public = sum(public_grants, na.rm = TRUE),
    market_total_client = sum(client_services, na.rm = TRUE),
    
    # Market characteristics
    n_orgs = n(),
    n_orgs_with_private = sum(private_contributions > 0, na.rm = TRUE),
    n_orgs_with_public = sum(public_grants > 0, na.rm = TRUE),
    n_orgs_with_client = sum(client_services > 0, na.rm = TRUE),
    
    .groups = "drop"
  )

cat("✓ Market totals calculated for", format(nrow(market_totals), big.mark = ","), "market-years\n\n")

# Quick diagnostic
cat("Market-level diagnostics:\n")
market_diag <- market_totals %>%
  summarise(
    mean_orgs = mean(n_orgs),
    mean_private_rev = mean(market_total_private),
    mean_public_rev = mean(market_total_public),
    mean_client_rev = mean(market_total_client),
    pct_markets_with_public = 100 * mean(market_total_public > 0)
  )
print(market_diag)
cat("\n")

# ==============================================================================
# 3. CALCULATE ORG-LEVEL MARKET SHARES
# ==============================================================================

cat("SECTION 3: CALCULATING ORGANIZATION MARKET SHARES\n")
cat(strrep("-", 80), "\n\n")

cat("Computing each org's share of market revenue by constituency...\n\n")

org_shares <- org_data %>%
  left_join(
    market_totals %>% select(CBSA, ntee_broad_clean, TAXYEAR, 
                              market_total_private, market_total_public, 
                              market_total_client, n_orgs),
    by = c("CBSA", "ntee_broad_clean", "TAXYEAR")
  ) %>%
  mutate(
    # Calculate market shares (0-1 scale)
    # Only calculate share if market has positive total revenue on that side
    private_share = if_else(
      market_total_private > 0,
      private_contributions / market_total_private,
      0
    ),
    public_share = if_else(
      market_total_public > 0,
      public_grants / market_total_public,
      0
    ),
    client_share = if_else(
      market_total_client > 0,
      client_services / market_total_client,
      0
    ),
    
    # Flag which sides org is active on
    active_private = private_contributions > 0,
    active_public = public_grants > 0,
    active_client = client_services > 0,
    
    # Count number of sides org operates on
    n_sides_active = as.integer(active_private) + 
                     as.integer(active_public) + 
                     as.integer(active_client),
    
    # Flag multi-homing organizations
    is_multihoming = n_sides_active >= 2,
    is_platform = n_sides_active == 3
  )

cat("✓ Market shares calculated for all organizations\n\n")

# ==============================================================================
# 4. QUALITY CHECKS
# ==============================================================================

cat("SECTION 4: DATA QUALITY CHECKS\n")
cat(strrep("-", 80), "\n\n")

# Check 1: Shares sum to 1 within each market/constituency
cat("CHECK 1: Do shares sum to 1 within markets?\n")
share_sums <- org_shares %>%
  group_by(CBSA, ntee_broad_clean, TAXYEAR) %>%
  summarise(
    sum_private = sum(private_share, na.rm = TRUE),
    sum_public = sum(public_share, na.rm = TRUE),
    sum_client = sum(client_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(sum_private > 0 | sum_public > 0 | sum_client > 0) %>%
  summarise(
    private_mean = mean(sum_private),
    public_mean = mean(sum_public),
    client_mean = mean(sum_client),
    private_sd = sd(sum_private),
    public_sd = sd(sum_public),
    client_sd = sd(sum_client)
  )

print(share_sums)
cat("  → Shares should sum to ~1.0 (allowing for rounding)\n\n")

# Check 2: Share distribution
cat("CHECK 2: Distribution of market shares\n")
share_dist <- org_shares %>%
  summarise(
    across(
      c(private_share, public_share, client_share),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        p90 = ~quantile(., 0.90, na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

print(t(share_dist))
cat("\n")

# Check 3: Multi-homing prevalence
cat("CHECK 3: Multi-homing prevalence\n")
multihoming <- org_shares %>%
  summarise(
    total_orgs = n(),
    single_sided = sum(n_sides_active == 1),
    two_sided = sum(n_sides_active == 2),
    three_sided = sum(n_sides_active == 3),
    pct_single = 100 * single_sided / total_orgs,
    pct_two = 100 * two_sided / total_orgs,
    pct_three = 100 * three_sided / total_orgs
  )

print(multihoming)
cat("\n")

cat("INTERPRETATION:\n")
cat("  ", round(multihoming$pct_three, 1), 
    "% of organizations operate as three-sided platforms\n")
cat("  ", round(multihoming$pct_two + multihoming$pct_three, 1), 
    "% operate on 2+ sides (multi-homing)\n\n")

# ==============================================================================
# 5. SAVE PRIMARY OUTPUT
# ==============================================================================

cat("SECTION 5: SAVING PRIMARY OUTPUT\n")
cat(strrep("-", 80), "\n\n")

# Select final variables for output
org_shares_output <- org_shares %>%
  select(
    # Core identifiers
    EIN, CBSA, CBSA_NAME, ntee_broad_clean, TAXYEAR,
    
    # Organization info
    ORG_NAME, CITY, STATE, COUNTY,
    
    # Revenue amounts
    private_contributions, public_grants, client_services,
    TOTREV, TOTEXP, TOTASSETS,
    
    # Market context
    market_total_private, market_total_public, market_total_client, n_orgs,
    
    # Market shares (PRIMARY OUTPUT)
    private_share, public_share, client_share,
    
    # Activity indicators
    active_private, active_public, active_client,
    n_sides_active, is_multihoming, is_platform
  )

# Save
write_csv(
  org_shares_output,
  here("data", "processed", "org_level_market_shares.csv")
)

cat("✓ PRIMARY OUTPUT SAVED:\n")
cat("  File: data/processed/org_level_market_shares.csv\n")
cat("  Variables:", ncol(org_shares_output), "\n")
cat("  Observations:", format(nrow(org_shares_output), big.mark = ","), "\n\n")

cat("KEY VARIABLES:\n")
cat("  - private_share, public_share, client_share (0-1 scale)\n")
cat("  - n_sides_active (1, 2, or 3)\n")
cat("  - is_platform (TRUE if active on all 3 sides)\n\n")

cat("USES:\n")
cat("  1. Script 04 will use shares to calculate market-level HHI\n")
cat("  2. Script 06 will use shares to measure platform coupling\n\n")

# ==============================================================================
# 6. CALCULATE MARKET-LEVEL SUMMARIES (FOR SCRIPT 04)
# ==============================================================================

cat("SECTION 6: MARKET-LEVEL SUMMARIES\n")
cat(strrep("-", 80), "\n\n")

cat("Pre-calculating market summaries for Script 04 efficiency...\n\n")

market_summaries <- org_shares %>%
  group_by(CBSA, CBSA_NAME, ntee_broad_clean, TAXYEAR) %>%
  summarise(
    # Market size
    n_orgs = n(),
    
    # Revenue totals
    total_private = sum(private_contributions, na.rm = TRUE),
    total_public = sum(public_grants, na.rm = TRUE),
    total_client = sum(client_services, na.rm = TRUE),
    total_revenue = total_private + total_public + total_client,
    
    # Active organizations by side
    n_private_active = sum(active_private),
    n_public_active = sum(active_public),
    n_client_active = sum(active_client),
    
    # Multi-homing
    n_multihoming = sum(is_multihoming),
    n_platform_orgs = sum(is_platform),
    pct_multihoming = 100 * n_multihoming / n_orgs,
    pct_platform = 100 * n_platform_orgs / n_orgs,
    
    # Revenue concentration (for HHI calculation in Script 04)
    # Sum of squared shares = HHI
    hhi_private = sum(private_share^2, na.rm = TRUE),
    hhi_public = sum(public_share^2, na.rm = TRUE),
    hhi_client = sum(client_share^2, na.rm = TRUE),
    
    # Top org concentration
    max_private_share = max(private_share, na.rm = TRUE),
    max_public_share = max(public_share, na.rm = TRUE),
    max_client_share = max(client_share, na.rm = TRUE),
    
    .groups = "drop"
  )

cat("✓ Market summaries calculated\n\n")

# Save market summaries
write_csv(
  market_summaries,
  here("data", "processed", "market_level_summaries.csv")
)

cat("✓ SECONDARY OUTPUT SAVED:\n")
cat("  File: data/processed/market_level_summaries.csv\n")
cat("  This provides Script 04 with pre-calculated HHI values\n\n")

# ==============================================================================
# 7. DESCRIPTIVE STATISTICS
# ==============================================================================

cat("SECTION 7: DESCRIPTIVE STATISTICS\n")
cat(strrep("-", 80), "\n\n")

cat("MARKET-LEVEL CONCENTRATION:\n")
hhi_summary <- market_summaries %>%
  summarise(
    across(
      c(hhi_private, hhi_public, hhi_client),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

print(t(hhi_summary))
cat("\n")

cat("MULTI-HOMING BY MARKET:\n")
multihoming_summary <- market_summaries %>%
  summarise(
    mean_pct_multihoming = mean(pct_multihoming, na.rm = TRUE),
    mean_pct_platform = mean(pct_platform, na.rm = TRUE),
    median_n_platform = median(n_platform_orgs, na.rm = TRUE)
  )

print(multihoming_summary)
cat("\n")

# ==============================================================================
# 8. SAMPLE COMPARISON: 2020-2021 vs. ALL YEARS
# ==============================================================================

cat("SECTION 8: TEMPORAL COMPARISON\n")
cat(strrep("-", 80), "\n\n")

cat("Comparing recent period (2020-2021) to full sample...\n\n")

temporal_comparison <- market_summaries %>%
  mutate(period = if_else(TAXYEAR %in% 2020:2021, "2020-2021", "2012-2019")) %>%
  group_by(period) %>%
  summarise(
    n_markets = n(),
    mean_hhi_private = mean(hhi_private, na.rm = TRUE),
    mean_hhi_public = mean(hhi_public, na.rm = TRUE),
    mean_hhi_client = mean(hhi_client, na.rm = TRUE),
    mean_pct_multihoming = mean(pct_multihoming, na.rm = TRUE),
    pct_markets_with_public = 100 * mean(n_public_active > 0)
  )

print(temporal_comparison)
cat("\n")

cat("KEY INSIGHT: Public grants coverage improves in 2020-2021\n")
cat("  → E-filing mandate increased public_grants reporting\n\n")

# ==============================================================================
# SECTION 9: FULL-SAMPLE HHI + CR4 BY SECTOR (TABLE 2)  # Renumber if needed
# ==============================================================================

cat("SECTION 9: BUILDING FULL-SAMPLE HHI + CR4 BY SECTOR (TABLE 2)\n")
cat(strrep("-", 80), "\n\n")

# --------------------------------------------------------------
# 1. Use the in-memory market_summaries (full 2012–2021)
# --------------------------------------------------------------
# (Already exists from earlier in this script)

# --------------------------------------------------------------
# 2. Load org-level shares to compute *exact* CR4
# --------------------------------------------------------------
# (org_shares_output already exists from earlier; rename if needed)
org_shares <- org_shares_output  # Or read_csv if not in memory

# --------------------------------------------------------------
# 3. Compute CR4 for each market-year (sum top-4 shares)
# --------------------------------------------------------------
cr4_by_market <- org_shares %>%
  group_by(CBSA, ntee_broad_clean, TAXYEAR) %>%
  summarise(
    private_cr4 = sum(sort(private_share, decreasing = TRUE)[1:4], na.rm = TRUE),
    public_cr4  = sum(sort(public_share,  decreasing = TRUE)[1:4], na.rm = TRUE),
    client_cr4  = sum(sort(client_share,  decreasing = TRUE)[1:4], na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------------------
# 4. Merge CR4 into market_summaries
# --------------------------------------------------------------
market_full <- market_summaries %>%
  left_join(cr4_by_market,
            by = c("CBSA", "ntee_broad_clean", "TAXYEAR"))

# --------------------------------------------------------------
# 5. Aggregate by sector (full 2012–2021 sample)
# --------------------------------------------------------------
table2_full <- market_full %>%
  group_by(ntee_broad_clean) %>%
  summarise(
    n_markets        = n(),
    mean_private_hhi = mean(hhi_private,   na.rm = TRUE),
    mean_private_cr4 = mean(private_cr4,   na.rm = TRUE),
    mean_public_hhi  = mean(hhi_public,    na.rm = TRUE),
    mean_public_cr4  = mean(public_cr4,    na.rm = TRUE),
    mean_client_hhi  = mean(hhi_client,    na.rm = TRUE),
    mean_client_cr4  = mean(client_cr4,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add grand-total row
  bind_rows(
    summarise(.,
              ntee_broad_clean = "Total",
              n_markets        = sum(n_markets),
              across(starts_with("mean_"), mean, na.rm = TRUE))
  )

# --------------------------------------------------------------
# 6. Save as the official Table 2
# --------------------------------------------------------------
out_path <- here("output", "tables", "table2_hhi_cr4_by_sector_2012_2021.csv")
write_csv(table2_full, out_path)

cat("✓ Table 2 (full 2012–2021) written to:\n")
cat("  ", out_path, "\n\n")

# ==============================================================================
# 9. FINAL SUMMARY
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("SCRIPT 03 COMPLETE\n")
cat(strrep("=", 80), "\n\n")

cat("OUTPUTS CREATED:\n")
cat("  1. org_level_market_shares.csv (",
    format(nrow(org_shares_output), big.mark = ","), " org-years)\n", sep = "")
cat("  2. market_level_summaries.csv (",
    format(nrow(market_summaries), big.mark = ","), " market-years)\n\n", sep = "")

cat("NEXT STEP:\n")
cat("  → Script 04 can now use org_level_market_shares.csv\n")
cat("  → This ensures HHI is calculated consistently from shares\n")
cat("  → Script 06 can use shares to measure TRUE platform coupling\n\n")

cat("KEY FINDING:\n")
cat("  Only ", round(multihoming$pct_three, 1), 
    "% of organizations operate as three-sided platforms\n", sep = "")
cat("  → This sets upper bound on how much 'platform coupling' we can find\n\n")

cat(strrep("=", 80), "\n")

sink()
