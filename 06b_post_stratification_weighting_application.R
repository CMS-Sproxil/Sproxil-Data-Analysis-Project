# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Attach Recommended Post-Stratification Weight
# AUTHOR:  Corona Management Systems
# DATE:    18 March 2026
#
# PURPOSE:
#   Attach the recommended final post-stratification weight selected by the
#   integrated weighting feasibility and impact-review script.
#
# INPUT:
#   - Sproxil_Analysis_Ready.rds
#   - benchmarks/integrated_weighting/WEIGHT_SCHEME_RECOMMENDATION.csv
#   - benchmarks/integrated_weighting/SCHEME_TEST_WEIGHTS.csv
#
# OUTPUT:
#   - Sproxil_Analysis_Ready_Weighted.rds
#   - Sproxil_Analysis_Ready_Weighted.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# ------------------------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
INPUT_REC <- file.path("benchmarks", "integrated_weighting", "WEIGHT_SCHEME_RECOMMENDATION.csv")
INPUT_WTS <- file.path("benchmarks", "integrated_weighting", "SCHEME_TEST_WEIGHTS.csv")

OUTPUT_RDS <- "Sproxil_Analysis_Ready_Weighted.rds"
OUTPUT_CSV <- "Sproxil_Analysis_Ready_Weighted.csv"

# ------------------------------------------------------------------------------
# 2. Checks and load
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_RDS)) stop("Missing dataset: ", INPUT_RDS)
if (!file.exists(INPUT_REC)) stop("Missing recommendation file: ", INPUT_REC)
if (!file.exists(INPUT_WTS)) stop("Missing scheme weights file: ", INPUT_WTS)

df <- readRDS(INPUT_RDS) %>%
  mutate(row_id = row_number())

rec <- read_csv(INPUT_REC, show_col_types = FALSE)
wts <- read_csv(INPUT_WTS, show_col_types = FALSE)

if (!"recommended_scheme" %in% names(rec)) {
  stop("Recommendation file does not contain 'recommended_scheme'.")
}

final_scheme <- rec$recommended_scheme[1]

if (is.na(final_scheme) || final_scheme == "NONE") {
  stop("No valid recommended scheme found. Review weighting feasibility outputs first.")
}

message("Recommended scheme found: ", final_scheme)

# ------------------------------------------------------------------------------
# 3. Keep selected scheme only
# ------------------------------------------------------------------------------
wts_final <- wts %>%
  filter(scheme == final_scheme) %>%
  select(row_id, w_raw, w_norm_untrim, w_norm_trim)

if (nrow(wts_final) == 0) {
  stop("No weights found for recommended scheme: ", final_scheme)
}

# ------------------------------------------------------------------------------
# 4. Attach weights
# ------------------------------------------------------------------------------
df_weighted <- df %>%
  left_join(wts_final, by = "row_id") %>%
  mutate(
    selected_scheme = final_scheme,
    w_base = 1,
    w_calibrated_raw = w_raw,
    w_calibrated_trim = w_norm_trim,
    calib_ok = ifelse(!is.na(w_calibrated_trim) & is.finite(w_calibrated_trim) & w_calibrated_trim > 0, 1, 0)
  ) %>%
  select(-w_raw, -w_norm_untrim, -w_norm_trim)

# ------------------------------------------------------------------------------
# 5. Basic QC
# ------------------------------------------------------------------------------
cat("Rows in dataset: ", nrow(df_weighted), "\n")
cat("Recommended scheme: ", final_scheme, "\n")
cat("Rows with final calibrated weight: ", sum(!is.na(df_weighted$w_calibrated_trim)), "\n")
cat("Rows flagged calib_ok = 1: ", sum(df_weighted$calib_ok == 1, na.rm = TRUE), "\n")
cat("Mean final weight: ", mean(df_weighted$w_calibrated_trim, na.rm = TRUE), "\n")
cat("Min final weight: ", min(df_weighted$w_calibrated_trim, na.rm = TRUE), "\n")
cat("Max final weight: ", max(df_weighted$w_calibrated_trim, na.rm = TRUE), "\n")

# Stronger checks
if (sum(is.na(df_weighted$w_calibrated_trim)) > 0) {
  warning("Some rows did not receive a final calibrated weight.")
}

if (anyDuplicated(wts_final$row_id) > 0) {
  warning("Duplicate row_id values found in selected weights file.")
}

# ------------------------------------------------------------------------------
# 6. Save outputs
# ------------------------------------------------------------------------------
saveRDS(df_weighted, OUTPUT_RDS)
write.csv(df_weighted, OUTPUT_CSV, row.names = FALSE)

cat("✅ Final weighted dataset saved:\n")
cat(" - ", OUTPUT_RDS, "\n")
cat(" - ", OUTPUT_CSV, "\n")