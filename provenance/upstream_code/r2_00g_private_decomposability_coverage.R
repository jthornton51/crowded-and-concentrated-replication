# ==============================================================================
# NVSQ R2 -- HOW MUCH OF THE PANEL CAN THE PRIVATE SIDE BE DECOMPOSED ON?
#
# r2_00f established that REV_CONTRIBUTIONS is Form 990 Part VIII line 1h, total
# contributions, which contains line 1e. Correcting the private side therefore
# means subtracting government grants. This audit measures the strict alternative
# to the adopted uniform blank-as-zero convention. On that alternative,
#
#     private = contributions - grants
#
# is independently determinable on part of the panel and unresolved on the rest.
# The governing decision retains blank-as-zero as the main specification; this
# script does not reopen that decision or establish that unobserved grants exist.
#
# This script measures the decomposable share before any definition is chosen. It
# reuses the classification vocabulary of scripts/audit_r2_grant_missingness.R and
# the component-reconciliation logic of scripts/audit_r2_grant_validation.R so the
# three analyses describe the same categories.
#
# Determinability classes, per organization-year:
#   determinable_reported          grant amount observed (positive, zero, or negative)
#   determinable_blank_is_zero     grant field blank, but the other contribution
#                                  components exactly account for the stated total,
#                                  so the blank is a zero under the accounting identity
#   indeterminate_blank_unresolved grant blank and the identity does not resolve
#                                  (total missing, or a positive residual)
#   indeterminate_no_source        no Part VIII source record for that organization-year
#   indeterminate_conflicting      repeated source records disagreeing on the amount,
#                                  or a parse failure
#
# The market-level question is not how many organizations are determinable but how
# much CONTRIBUTED REVENUE they hold. A market-year in which the single
# indeterminate organization is tiny is usable; one in which the largest
# contribution recipient is indeterminate is not, because its share drives the HHI.
# Both counts and revenue shares are reported, and the revenue share is the one to
# read.
#
# Reads:  data/raw/irs/F9-P08-T00-REVENUE-YYYY.csv  (2012-2021)
#         data/processed/pz_core_with_geo_sector.csv
# Writes (output/revisions/NVSQ_R2_2026-09/decomposability_<timestamp>/):
#         decomposability_org_year_by_class.csv
#         decomposability_market_year.csv
#         decomposability_thresholds_by_year.csv
#         decomposability_thresholds_by_size.csv
#         decomposability_sector_summary.csv
#
# Preserves production data. The R2 coupling sensitivity reads its conservative
# source_determinability_keys.csv classification; no production measure reads it.
# ==============================================================================

library(tidyverse)
library(data.table)
library(here)
setDTthreads(4L)
DIAG <- here("output", "revisions", "NVSQ_R2_2026-09", paste0("decomposability_", format(Sys.time(), "%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(DIAG))
dir.create(DIAG, recursive = TRUE)
sink(file.path(DIAG, "run_log.txt"), split = TRUE)

cat(strrep("=", 80), "\n")
cat("R2: PRIVATE-SIDE DECOMPOSABILITY COVERAGE\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

YEARS <- 2012:2021
TOL   <- 1
KEY   <- c("CBSA", "ntee_broad_clean", "TAXYEAR")

pick <- function(nms, exact, pattern, anti = NULL) {
  hit <- nms[toupper(nms) %in% toupper(exact)]
  if (!length(hit)) hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (!is.null(anti) && length(hit) > 1) {
    keep <- hit[!grepl(anti, hit, ignore.case = TRUE)]
    if (length(keep)) hit <- keep
  }
  if (length(hit)) hit[1] else NA_character_
}

# ------------------------------------------------------------------------------
# 1. Classify each source organization-year
# ------------------------------------------------------------------------------
cat("--- 1. Classifying Part VIII source records ---\n")

classify_year <- function(yr) {
  f <- here("data", "raw", "irs", paste0("F9-P08-T00-REVENUE-", yr, ".csv"))
  if (!file.exists(f)) stop("Missing required year: ", yr)
  nms <- names(fread(f, nrows = 0))

  ein   <- pick(nms, c("EIN2", "EIN", "F9_00_ORG_EIN", "FILEREIN"), "^ein2?$|^ein|filerein")
  total <- pick(nms, c("F9_08_REV_CONTR_TOT", "TOTACASHCONT"),
                "contr(ib)?.*tot|tot.*contr(ib)?",
                anti = "membship|member|dues|noncash|non_?cash|prog|govt|gov|_cy_|sched|sa_")
  govt  <- pick(nms, c("F9_08_REV_CONTR_GOVT_GRANT", "GOVERNGRANTS"),
                "contr.*govt.*grant|gov.*grant|grant.*gov", anti = "sched|sa_|value_svc")
  comp  <- c(pick(nms, c("F9_08_REV_CONTR_FED_CAMP", "FEDERACAMPAI"), "feder.*campai"),
             pick(nms, c("F9_08_REV_CONTR_MEMBSHIP_DUE", "MEMBERDUESUE"), "memb.*dues"),
             pick(nms, c("F9_08_REV_CONTR_FUNDR_EVNT", "FUNDRAEVENTS"), "fundrais.*ev(e)?nt"),
             pick(nms, c("F9_08_REV_CONTR_RLTD_ORG", "RELATEORGANI"), "relat.*org"),
             pick(nms, c("F9_08_REV_CONTR_OTH", "ALLOOTHECONT"), "all.*oth.*contr|contr.*all.*oth"))
  stopifnot(!is.na(ein), !is.na(total), !is.na(govt), length(comp) == 5L,
            !anyNA(comp), !anyDuplicated(comp))
  fwrite(data.table(year=yr, component=c("total","government","1a","1b","1c","1d","1f"),
                    source_column=c(total,govt,comp)), file.path(DIAG,paste0("source_columns_",yr,".csv")))

  if (is.na(ein) || is.na(govt)) {
    cat(sprintf("  %d: EIN or grant column not found, skipped\n", yr))
    cat("     CONTR/GRANT columns present: ",
        paste(grep("contr|grant", nms, ignore.case = TRUE, value = TRUE), collapse = ", "), "\n")
    return(NULL)
  }

  d <- fread(f, select = unique(c(ein, total, govt, comp)))
  setnames(d, ein, "EIN_RAW"); setnames(d, govt, "grant_raw")
  if (!is.na(total)) setnames(d, total, "total_raw") else d[, total_raw := NA]
  compnames <- paste0("cmp", seq_along(comp))
  if (length(comp)) setnames(d, comp, compnames)

  d[, EIN := as.numeric(gsub("-", "", gsub("^EIN-", "", as.character(EIN_RAW))))]
  d[, grant_blank := is.na(grant_raw) | trimws(as.character(grant_raw)) == ""]
  d[, grant_amt := suppressWarnings(as.numeric(grant_raw))]
  d[, total_amt := suppressWarnings(as.numeric(total_raw))]

  # Line 1g (noncash) is a memo line and is not a component; identity is
  # 1a + 1b + 1c + 1d + 1e + 1f = 1h, with 1e blank treated as the unknown.
  if (length(compnames)) {
    for (v in compnames) set(d, j = v, value = suppressWarnings(as.numeric(d[[v]])))
    d[, other_sum     := rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = compnames]
    d[, other_missing := rowSums(is.na(as.matrix(.SD))),        .SDcols = compnames]
    d[, other_neg     := rowSums(as.matrix(.SD) < 0, na.rm = TRUE), .SDcols = compnames]
  } else {
    d[, `:=`(other_sum = NA_real_, other_missing = NA_integer_, other_neg = NA_integer_)]
  }
  d[, residual := total_amt - other_sum]

  d[, cls := fcase(
    !grant_blank & !is.na(grant_amt), "determinable_reported",
    !grant_blank &  is.na(grant_amt), "indeterminate_conflicting",
    grant_blank & !is.na(residual) & !is.na(total_amt) & total_amt >= 0 &
      other_neg == 0 & other_missing == 0 & abs(residual) <= TOL,
      "determinable_blank_is_zero",
    default = "indeterminate_blank_unresolved")]
  d[, TAXYEAR := yr]

  # Repeated source keys that disagree on the amount are unresolvable here; the
  # filing-selection policy is a separate decision and is not pre-empted.
  # All duplicate candidates must agree, including missingness and classification.
  # This is a conservative EIN/year coverage audit, not exact filing linkage.
  d[, amt_id := .GRP, by=grant_amt]
  d[, cls_id := match(cls, sort(unique(cls)))]
  checks <- d[!is.na(EIN), .(min_amt=min(amt_id),max_amt=max(amt_id),
                             min_cls=min(cls_id),max_cls=max(cls_id)),by=.(EIN,TAXYEAR)]
  u <- unique(d[!is.na(EIN),.(EIN,TAXYEAR,cls,grant=grant_amt)],by=c("EIN","TAXYEAR"))
  bad <- checks[min_amt!=max_amt | min_cls!=max_cls]
  u[bad,on=.(EIN,TAXYEAR),cls:="indeterminate_conflicting"]
  cat(sprintf("  %d: %s source organization-years\n", yr, format(nrow(u), big.mark = ",")))
  u[, .(EIN, TAXYEAR, cls, grant)]
}

src <- rbindlist(lapply(YEARS, classify_year), fill = TRUE)
stopifnot(!is.null(src), nrow(src) > 0)
stopifnot(uniqueN(src$TAXYEAR)==10L, !anyDuplicated(src[,.(EIN,TAXYEAR)]))
fwrite(src,file.path(DIAG,"source_determinability_keys.csv"))
cat("Total classified source organization-years:", format(nrow(src), big.mark = ","), "\n\n")

# ------------------------------------------------------------------------------
# 2. Attach to the analytic panel
# ------------------------------------------------------------------------------
cat("--- 2. Building the analytic panel ---\n")

pz <- fread(here("data", "processed", "pz_core_with_geo_sector.csv"),
            select = c("EIN", "TAXYEAR", "CBSA", "ntee_broad", "TOTREV",
                       "REV_CONTRIBUTIONS"))
pz[, EIN := as.numeric(EIN)]

# Sector mapping reproduced from scripts/04_construct_constituency_revenue.R.
pz[, ntee_broad_clean := fcase(
  ntee_broad == "A", "A_Arts",
  ntee_broad %in% c("C", "D"), "CD_Environment_Animals",
  ntee_broad == "B", "B_Education",
  ntee_broad %in% c("E", "F", "G", "H"), "EFGH_Health",
  ntee_broad %in% c("I","J","K","L","M","N","O","P"), "IJKLMNOP_Human_Services",
  ntee_broad == "Q", "Q_International",
  ntee_broad %in% c("R","S","T","U","V","W"), "RSTUVW_Public_Benefit",
  ntee_broad == "X", "X_Religion",
  ntee_broad == "Y", "Y_Mutual_Benefit",
  default = "Z_Unknown")]

pz <- pz[!is.na(CBSA) & !is.na(ntee_broad_clean) & ntee_broad_clean != "Z_Unknown" &
         !is.na(TAXYEAR) & !is.na(TOTREV) & TOTREV > 0]
cat("Analytic organization-years:", format(nrow(pz), big.mark = ","), "\n")

pz <- merge(pz, src, by = c("EIN", "TAXYEAR"), all.x = TRUE)
pz[is.na(cls), cls := "indeterminate_no_source"]
pz[, determinable := startsWith(cls, "determinable")]
pz[, contrib := pmax(0, fcoalesce(as.numeric(REV_CONTRIBUTIONS), 0))]

by_class <- pz[, .(n = .N,
                   contributed_revenue = sum(contrib)), by = .(TAXYEAR, cls)][order(TAXYEAR, cls)]
by_class[, pct_of_year := round(100 * n / sum(n), 2), by = TAXYEAR]
by_class[, pct_revenue_of_year := round(100 * contributed_revenue /
                                        sum(contributed_revenue), 2), by = TAXYEAR]
write_csv(by_class, file.path(DIAG, "decomposability_org_year_by_class.csv"))

cat("\nOrganization-years by class and year (percent of year):\n")
print(dcast(by_class, TAXYEAR ~ cls, value.var = "pct_of_year", fill = 0))
cat("\nSame, weighted by contributed revenue:\n")
print(dcast(by_class, TAXYEAR ~ cls, value.var = "pct_revenue_of_year", fill = 0))
cat("\n")

# ------------------------------------------------------------------------------
# 3. Roll up to market-years
# ------------------------------------------------------------------------------
cat("--- 3. Market-year coverage ---\n")

mkt <- pz[, .(
  n_orgs              = .N,
  n_determinable      = sum(determinable),
  pct_orgs_determinable = 100 * mean(determinable),
  contrib_total       = sum(contrib),
  contrib_determinable= sum(contrib[determinable])
), by = KEY]
mkt[, pct_revenue_determinable := fifelse(contrib_total > 0,
                                          100 * contrib_determinable / contrib_total,
                                          NA_real_)]
mkt[, size_stratum := cut(n_orgs, c(0,1,2,4,9,24,Inf),
                          labels = c("1","2","3-4","5-9","10-24","25+"))]
write_csv(mkt, file.path(DIAG, "decomposability_market_year.csv"))
cat("Market-years:", format(nrow(mkt), big.mark = ","), "\n")

thresholds <- function(d, ...) {
  d[, .(
    n_market_years        = .N,
    pct_all_orgs_determinable = round(100 * mean(pct_orgs_determinable >= 100 - 1e-9), 2),
    pct_rev_100           = round(100 * mean(pct_revenue_determinable >= 100 - 1e-9, na.rm = TRUE), 2),
    pct_rev_ge_99         = round(100 * mean(pct_revenue_determinable >= 99, na.rm = TRUE), 2),
    pct_rev_ge_95         = round(100 * mean(pct_revenue_determinable >= 95, na.rm = TRUE), 2),
    pct_rev_ge_90         = round(100 * mean(pct_revenue_determinable >= 90, na.rm = TRUE), 2),
    median_pct_rev        = round(median(pct_revenue_determinable, na.rm = TRUE), 2)
  ), by = ...]
}

by_year <- thresholds(mkt, .(TAXYEAR))[order(TAXYEAR)]
by_size <- thresholds(mkt, .(size_stratum))[order(size_stratum)]
by_sect <- thresholds(mkt, .(ntee_broad_clean))[order(-n_market_years)]
write_csv(by_year, file.path(DIAG, "decomposability_thresholds_by_year.csv"))
write_csv(by_size, file.path(DIAG, "decomposability_thresholds_by_size.csv"))
write_csv(by_sect, file.path(DIAG, "decomposability_sector_summary.csv"))

cat("\nBy year (percentages are of market-years in that year):\n"); print(as.data.frame(by_year))
cat("\nBy market size:\n");   print(as.data.frame(by_size))
cat("\nBy sector:\n");        print(as.data.frame(by_sect))

# ------------------------------------------------------------------------------
# 4. Reading
# ------------------------------------------------------------------------------
mkt[, all_markets := "all"]          # a real column, so data.table accepts it as `by`
overall <- thresholds(mkt, .(all_markets))
cat("\n", strrep("-", 72), "\n", sep = "")
cat(sprintf("Across %s market-years, contributed revenue is fully decomposable in\n",
            format(nrow(mkt), big.mark = ",")))
cat(sprintf("%.1f%%, at least 99%% decomposable in %.1f%%, and at least 95%% in %.1f%%.\n",
            overall$pct_rev_100, overall$pct_rev_ge_99, overall$pct_rev_ge_95))
cat("\nRead the revenue columns, not the organization counts: an indeterminate\n")
cat("organization holding a trivial share of contributions barely moves the HHI,\n")
cat("while an indeterminate market leader invalidates it. Read the by-year table\n")
cat("next: if decomposability trends strongly with reporting improvements, a\n")
cat("decomposed sample is selected on year, which is the dimension the panel\n")
cat("analyses vary over.\n")
cat(strrep("-", 72), "\n\n")

cat(strrep("=", 80), "\n")
writeLines(capture.output(sessionInfo()),file.path(DIAG,"session_info.txt"))
writeLines(c("Completed all ten years with all five nongovernment components required.",
             "Strict coverage requires every other component observed; it differs from the conditional blank-as-zero audit.",
             "EIN/year candidates must agree on grant amount and strict class; no exact filing-period match is claimed."),
           file.path(DIAG,"SUCCESS.txt"))
sink()
cat("COVERAGE COUNT COMPLETE:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Written to:", DIAG, "\n")
cat(strrep("=", 80), "\n")
