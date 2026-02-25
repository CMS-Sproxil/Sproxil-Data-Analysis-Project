# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  06_Weighting_Diagnostics.R
# PURPOSE: Assess feasibility of Post-Stratification Weighting (Raking)
# ==============================================================================

library(tidyverse)
library(haven)
library(survey)

# --- 1. LOAD DATA ---
cat("--- 1. Loading Analytic Data ---\n")
df <- readRDS("Sproxil_Derived_Final.rds")

# --- 2. CLEAN & STRIP LABELLED FORMATS ---
df_clean <- df %>%
  # Force labelled columns into plain numerics before factoring
  mutate(
    zone_num = as.numeric(derived_zone),
    residence_num = as.numeric(derived_residence),
    edu_num = as.numeric(derived_edu_cat)
  ) %>%
  # Filter to KEEP ONLY mathematically valid categories
  filter(
    !is.na(zone_num)      & zone_num %in% 1:6,
    !is.na(residence_num) & residence_num %in% 1:2,
    !is.na(edu_num)       & edu_num %in% 1:4
  ) %>%
  # Now explicitly create the factors mapping exactly to those numbers
  mutate(
    zone_f = factor(zone_num, levels = 1:6, 
                    labels = c("North Central", "North East", "North West", 
                               "South East", "South South", "South West")),
    residence_f = factor(residence_num, levels = 1:2, 
                         labels = c("Urban", "Rural")),
    edu_f = factor(edu_num, levels = 1:4, 
                   labels = c("No Education", "Primary", "Secondary", "More than secondary"))
  )

# 2.1 FAIL-SAFE CHECK (This guarantees rake will not throw an error)
if(any(is.na(df_clean$zone_f) | is.na(df_clean$residence_f) | is.na(df_clean$edu_f))) {
  stop("FATAL ERROR: There are still NAs in your weighting factors.")
}

cat("Retained", nrow(df_clean), "out of", nrow(df), "records with complete demographic data.\n\n")

# ----- You can continue the rest of the script (Step 3 onwards) from here -----

# To use survey::rake, the frequencies must sum to the total rows in our dataset
N_total <- nrow(df_clean)

# Zone Targets
pop_zone <- data.frame(
  zone_f = c("North Central", "North East", "North West", "South East", "South South", "South West"),
  Freq = N_total * c(0.164, 0.166, 0.334, 0.077, 0.120, 0.140)
)

# Residence Targets
pop_residence <- data.frame(
  residence_f = c("Urban", "Rural"),
  Freq = N_total * c(0.321, 0.679)
)

# Education Targets
pop_edu <- data.frame(
  edu_f = c("No Education", "Primary", "Secondary", "More than secondary"),
  Freq = N_total * c(0.356, 0.144, 0.371, 0.129)
)

# --- 4. COMPARE UNWEIGHTED SAMPLE TO POPULATION TARGETS ---
cat("--- 3. Unweighted Sample vs. Target Population ---\n")

compare_margins <- function(var_sym, target_df) {
  df_clean %>%
    count(!!sym(var_sym)) %>%
    mutate(Sample_Pct = round(n / sum(n) * 100, 1)) %>%
    left_join(target_df %>% mutate(Target_Pct = round(Freq / sum(Freq) * 100, 1)), 
              by = setNames(names(target_df)[1], var_sym)) %>%
    select(Category = 1, Sample_Pct, Target_Pct) %>%
    mutate(Difference = Sample_Pct - Target_Pct)
}

print("RESIDENCE SKEW:")
print(compare_margins("residence_f", pop_residence))
cat("\nEDUCATION SKEW:\n")
print(compare_margins("edu_f", pop_edu))
cat("\nZONE SKEW:\n")
print(compare_margins("zone_f", pop_zone))

# --- 5. PERFORM RAKING (CALCULATE WEIGHTS) ---
cat("\n--- 4. Computing Post-Stratification Weights ---\n")

# Define unweighted survey design
svy_unweighted <- svydesign(ids = ~1, data = df_clean, weights = ~1)

# Apply Raking algorithm
svy_raked <- rake(
  design = svy_unweighted,
  sample.margins = list(~zone_f, ~residence_f, ~edu_f),
  population.margins = list(pop_zone, pop_residence, pop_edu),
  control = list(maxit = 50, epsilon = 1e-4) # Standard convergence limits
)

# Extract weights back into the dataframe
df_clean$raked_weight <- weights(svy_raked)

# --- 6. STATISTICAL DIAGNOSTICS & FEASIBILITY DECISION ---
cat("--- 5. Evaluating Weighting Feasibility ---\n")

# Calculate weight metrics
w_min <- min(df_clean$raked_weight)
w_max <- max(df_clean$raked_weight)
w_mean <- mean(df_clean$raked_weight)

# Calculate Kish's Design Effect due to weighting (DEff_w)
# DEff_w = 1 + CV(weights)^2
cv_weights <- sd(df_clean$raked_weight) / mean(df_clean$raked_weight)
deff_w <- 1 + (cv_weights ^ 2)

# Calculate Effective Sample Size (ESS)
ess <- N_total / deff_w

cat(sprintf("Minimum Weight: %.2f (Under-represented respondents)\n", w_min))
cat(sprintf("Maximum Weight: %.2f (Over-represented respondents)\n", w_max))
cat(sprintf("Kish's Design Effect (DEff): %.2f\n", deff_w))
cat(sprintf("Raw Sample Size: %d\n", N_total))
cat(sprintf("Effective Sample Size (ESS): %.0f\n", ess))
cat(sprintf("Statistical Power Lost: %.1f%%\n\n", (1 - (ess / N_total)) * 100))

# ==============================================================================
# 7. EXPORT DIAGNOSTICS REPORT TO CSV
# ==============================================================================
cat("\n--- 6. Exporting Diagnostics Report to CSV ---\n")

# Calculate Effective Sample Size (ESS) and Power Lost
ess <- N_total / deff_w
power_lost_pct <- (1 - (ess / N_total)) * 100

# Generate Recommendation String
rec_text <- if (w_max > 5 | deff_w > 2.0) {
  "STOP: WEIGHTING IS NOT VIABLE. Sample is too skewed; variance inflation is unacceptable."
} else {
  "PROCEED: WEIGHTING IS VIABLE. Skew is manageable."
}

# 1. Format the Weighting Metrics
metrics_df <- tibble(
  Metric = c(
    "Raw Sample Size (N)",
    "Effective Sample Size (ESS)",
    "Statistical Power Lost",
    "Minimum Weight",
    "Maximum Weight",
    "Kish's Design Effect (DEff)",
    "Final Recommendation"
  ),
  Value_or_Details = c(
    as.character(N_total),
    as.character(round(ess, 0)),
    sprintf("%.1f%%", power_lost_pct),
    sprintf("%.2f", w_min),
    sprintf("%.2f", w_max),
    sprintf("%.2f", deff_w),
    rec_text
  )
)

# 2. Format the Skew Data (Combining Residence and Zone)
# We pull the data directly from the compare_margins function we defined earlier
skew_res <- compare_margins("residence_f", pop_residence) %>%
  mutate(Metric = paste("Skew (Residence):", Category))

skew_zone <- compare_margins("zone_f", pop_zone) %>%
  mutate(Metric = paste("Skew (Zone):", Category))

skew_df <- bind_rows(skew_res, skew_zone) %>%
  mutate(
    Value_or_Details = sprintf("Sample: %.1f%% | Target: %.1f%% | Difference: %+.1f%%", 
                               Sample_Pct, Target_Pct, Difference)
  ) %>%
  select(Metric, Value_or_Details)

# 3. Combine into a single comprehensive report
diagnostics_report <- bind_rows(
  tibble(Metric = "--- OVERALL WEIGHTING METRICS ---", Value_or_Details = ""),
  metrics_df,
  tibble(Metric = "", Value_or_Details = ""), # Blank row for readability
  tibble(Metric = "--- DEMOGRAPHIC SKEW ANALYSIS ---", Value_or_Details = ""),
  skew_df
)

# 4. Export to CSV
CSV_NAME <- paste0("Sproxil_Weighting_Diagnostics_", format(Sys.Date(), "%d%b%Y"), ".csv")
write.csv(diagnostics_report, CSV_NAME, row.names = FALSE)

cat(sprintf("✅ Diagnostics successfully exported to '%s'\n", CSV_NAME))

cat("===================================================================\n")


cat("FINAL RECOMMENDATION ON WEIGHTING:\n")

if (w_max > 5 | deff_w > 2.0) {
  cat("⚠️ WARNING: WEIGHTING IS NOT STATISTICALLY VIABLE.\n")
  cat("Reason: Your convenience sample is heavily skewed compared to the general population.\n")
  cat("Applying these weights will artificially inflate the variance (Design Effect > 2.0 or Max Weight > 5),\n")
  cat("making your Confidence Intervals extremely wide and P-values unreliable.\n")
  cat("Action: Do not use weights. Publish findings as an 'Unweighted analysis of Sproxil users'.\n")
} else if (deff_w > 1.5) {
  cat("⚠️ CAUTION: WEIGHTING IS VIABLE BUT RISKY.\n")
  cat("Reason: The weights cause a moderate loss of statistical power.\n")
  cat("Action: You may use weights, but you must report the Design Effect in your methodology.\n")
} else {
  cat("✅ APPROVED: WEIGHTING IS HIGHLY VIABLE.\n")
  cat("Reason: Your sample closely mirrors the national population, or the adjustments are minor.\n")
  cat("Action: Proceed with using 'raked_weight' in your final analysis tables.\n")
}
cat("===================================================================\n")

# Optional: Save the weighted dataset if it passes the test
# saveRDS(df_clean, "Sproxil_Analytic_Weighted.rds")