# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  06_Hybrid_Benchmarks_and_Calibration_Gatekeeper.R
# PURPOSE: Create hybrid calibration weights using embedded benchmark figures
#          that are closer to true national structure, while still allowing
#          replacement later with final benchmark margins.
#
# INPUT:
#   - Sproxil_Analysis_Ready.rds
#
# OUTPUTS:
#   - If PASS: Sproxil_Analysis_Ready_Weighted.rds
#   - If FAIL: Sproxil_Analysis_Ready_NoWeights.rds
#   - Diagnostics in ./benchmarks/
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(survey)
  library(stringr)
})

# ------------------------------------------------------------------------------
# 1) CONFIG
# ------------------------------------------------------------------------------
SPROXIL_PATH <- "Sproxil_Analysis_Ready.rds"

OUT_DIR <- "benchmarks"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_SPROXIL_WT   <- "Sproxil_Analysis_Ready_Weighted.rds"
OUT_SPROXIL_NOWT <- "Sproxil_Analysis_Ready_NoWeights.rds"

TRIM_LO <- 0.30
TRIM_HI <- 3.00

THRESH_PCT_ELIGIBLE_MIN <- 0.80
THRESH_MAX_W_UNTRIM     <- 10
THRESH_PCT_TRIM_HI_MAX  <- 0.10
THRESH_ESS_RATIO_MIN    <- 0.50
THRESH_MAD_IMPROVE_MIN  <- 0.10
THRESH_MAXABS_DIFF_MAX  <- 0.03

USE_PLACEHOLDER_BENCHMARKS <- TRUE

# ------------------------------------------------------------------------------
# 2) HELPERS
# ------------------------------------------------------------------------------
safe_num <- function(x) suppressWarnings(as.integer(as.numeric(as.character(x))))

ess <- function(w) {
  w <- w[!is.na(w)]
  if (length(w) == 0) return(NA_real_)
  (sum(w)^2) / sum(w^2)
}

cv_w <- function(w) {
  w <- w[!is.na(w)]
  if (length(w) == 0) return(NA_real_)
  sd(w) / mean(w)
}

trim_rescale <- function(w, lo, hi) {
  w2 <- pmin(pmax(w, lo), hi)
  w2 / mean(w2, na.rm = TRUE)
}

check_support_1d <- function(sample_levels, pop_levels, margin_name) {
  missing <- setdiff(as.character(pop_levels), as.character(sample_levels))
  if (length(missing) > 0) {
    stop(margin_name, " has population levels absent in sample: ", paste(missing, collapse = ", "))
  }
}

share_1d <- function(df, var, w = NULL) {
  if (is.null(w)) {
    df %>%
      filter(!is.na(.data[[var]])) %>%
      count(.data[[var]], name = "n") %>%
      mutate(share = n / sum(n)) %>%
      transmute(level = as.character(.data[[var]]), share)
  } else {
    df %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[w]])) %>%
      group_by(.data[[var]]) %>%
      summarise(wt = sum(.data[[w]]), .groups = "drop") %>%
      mutate(share = wt / sum(wt)) %>%
      transmute(level = as.character(.data[[var]]), share)
  }
}

# ------------------------------------------------------------------------------
# 3) LOAD SPROXIL
# ------------------------------------------------------------------------------
if (!file.exists(SPROXIL_PATH)) stop("Missing file: ", SPROXIL_PATH)
spx <- readRDS(SPROXIL_PATH)

# ------------------------------------------------------------------------------
# 4) EMBEDDED BENCHMARKS
# ------------------------------------------------------------------------------

# State totals seeded from official 2020 projected state populations
state_target <- tibble::tribble(
  ~demo_state_num, ~Freq,
  1, 3941279,
  2, 4657314,
  3, 4847542,
  4, 5720035,
  5, 7788504,
  6, 2444028,
  7, 5905747,
  8, 5875471,
  9, 4253698,
  10, 5416738,
  11, 3084214,
  12, 4567512,
  13, 3431742,
  14, 4505928,
  15, 2820261,
  16, 3733100,
  17, 5265082,
  18, 7007317,
  19, 8549066,
  20, 14655311,
  21, 9639059,
  22, 5178123,
  23, 4253371,
  24, 3351720,
  25, 13012971,
  26, 2712349,
  27, 6407568,
  28, 6090740,
  29, 5084330,
  30, 4303366,
  31, 7667318,
  32, 4504272,
  33, 7183473,
  34, 6039289,
  35, 3421510,
  36, 3481567,
  37, 5482423
)

# 15-64 age groups seeded from official 2020 projected national age totals
age_target <- tibble::tribble(
  ~age_group_4, ~Freq,
  1, 38214473,  # 15-24
  2, 28394771,  # 25-34
  3, 22932908,  # 35-44
  4, 23046279   # 45-64
)

# Near-parity sex split placeholder; replace later with MIS-based 15-64 counts if desired
sex_target <- tibble::tribble(
  ~demo_gender_num, ~Freq,
  1, 56350000,
  2, 56238431
)

# Residence placeholder; replace later with MIS-derived 15-64 benchmark
res_target <- tibble::tribble(
  ~derived_residence, ~Freq,
  1, 58000000,  # Urban
  2, 54588431   # Rural
)

# Education placeholder; replace later with MIS-derived 15-64 benchmark
edu_target <- tibble::tribble(
  ~derived_edu_cat, ~Freq,
  1, 27000000,   # No education
  2, 21000000,   # Primary
  3, 41000000,   # Secondary
  4, 23588431    # More than secondary
)

if (USE_PLACEHOLDER_BENCHMARKS) {
  message("⚠️ Placeholder benchmark mode is active.")
  message("   State and age figures are near-real anchors; residence and education remain replaceable placeholders.")
}

# ------------------------------------------------------------------------------
# 5) STANDARDIZE SPROXIL CALIBRATION VARIABLES
# ------------------------------------------------------------------------------
spx <- spx %>%
  mutate(
    row_id = row_number(),
    demo_state_num    = safe_num(demo_state_num),
    demo_gender_num   = safe_num(demo_gender_num),
    age_group_4       = safe_num(age_group_4),
    derived_residence = safe_num(derived_residence),
    derived_edu_cat   = safe_num(derived_edu_cat)
  )

spx <- spx %>%
  mutate(
    calibration_eligible = ifelse(
      demo_state_num %in% 1:37 &
        demo_gender_num %in% c(1, 2) &
        age_group_4 %in% 1:4 &
        derived_residence %in% c(1, 2) &
        derived_edu_cat %in% 1:4,
      1L, 0L
    )
  )

spx_cal <- spx %>% filter(calibration_eligible == 1)

message("Calibration-eligible rows: ", nrow(spx_cal), " / ", nrow(spx))

# ------------------------------------------------------------------------------
# 6) SUPPORT CHECKS
# ------------------------------------------------------------------------------
check_support_1d(unique(spx_cal$demo_state_num), state_target$demo_state_num, "State margin")
check_support_1d(unique(spx_cal$demo_gender_num), sex_target$demo_gender_num, "Sex margin")
check_support_1d(unique(spx_cal$age_group_4), age_target$age_group_4, "Age-group margin")
check_support_1d(unique(spx_cal$derived_residence), res_target$derived_residence, "Residence margin")
check_support_1d(unique(spx_cal$derived_edu_cat), edu_target$derived_edu_cat, "Education margin")

# ------------------------------------------------------------------------------
# 7) RAKE USING SEPARATE MARGINS
# ------------------------------------------------------------------------------
spx_cal <- spx_cal %>%
  mutate(
    f_state = factor(demo_state_num),
    f_sex   = factor(demo_gender_num),
    f_age   = factor(age_group_4),
    f_res   = factor(derived_residence),
    f_edu   = factor(derived_edu_cat)
  )

des0 <- svydesign(ids = ~1, data = spx_cal, weights = ~1)

# Population margins must be data frames with matching variable names + Freq
pop_state <- state_target %>%
  transmute(
    f_state = factor(demo_state_num, levels = levels(spx_cal$f_state)),
    Freq = Freq
  )

pop_sex <- sex_target %>%
  transmute(
    f_sex = factor(demo_gender_num, levels = levels(spx_cal$f_sex)),
    Freq = Freq
  )

pop_age <- age_target %>%
  transmute(
    f_age = factor(age_group_4, levels = levels(spx_cal$f_age)),
    Freq = Freq
  )

pop_res <- res_target %>%
  transmute(
    f_res = factor(derived_residence, levels = levels(spx_cal$f_res)),
    Freq = Freq
  )

pop_edu <- edu_target %>%
  transmute(
    f_edu = factor(derived_edu_cat, levels = levels(spx_cal$f_edu)),
    Freq = Freq
  )

des_raked <- rake(
  design = des0,
  sample.margins = list(~f_state, ~f_sex, ~f_age, ~f_res, ~f_edu),
  population.margins = list(pop_state, pop_sex, pop_age, pop_res, pop_edu),
  control = list(maxit = 100, epsilon = 1e-06, verbose = TRUE)
)

spx_cal$w_calibrated <- as.numeric(weights(des_raked))
spx_cal$w_calibrated_trim <- trim_rescale(spx_cal$w_calibrated, TRIM_LO, TRIM_HI)

# ------------------------------------------------------------------------------
# 8) FIT DIAGNOSTICS
# ------------------------------------------------------------------------------
fit_state <- state_target %>%
  transmute(level = as.character(demo_state_num), target_share = Freq / sum(Freq)) %>%
  left_join(share_1d(spx_cal, "demo_state_num") %>% rename(unw_share = share), by = "level") %>%
  left_join(share_1d(spx_cal, "demo_state_num", "w_calibrated_trim") %>% rename(w_share = share), by = "level") %>%
  mutate(diff_unw = unw_share - target_share,
         diff_w   = w_share   - target_share)

fit_sex <- sex_target %>%
  transmute(level = as.character(demo_gender_num), target_share = Freq / sum(Freq)) %>%
  left_join(share_1d(spx_cal, "demo_gender_num") %>% rename(unw_share = share), by = "level") %>%
  left_join(share_1d(spx_cal, "demo_gender_num", "w_calibrated_trim") %>% rename(w_share = share), by = "level") %>%
  mutate(diff_unw = unw_share - target_share,
         diff_w   = w_share   - target_share)

fit_age <- age_target %>%
  transmute(level = as.character(age_group_4), target_share = Freq / sum(Freq)) %>%
  left_join(share_1d(spx_cal, "age_group_4") %>% rename(unw_share = share), by = "level") %>%
  left_join(share_1d(spx_cal, "age_group_4", "w_calibrated_trim") %>% rename(w_share = share), by = "level") %>%
  mutate(diff_unw = unw_share - target_share,
         diff_w   = w_share   - target_share)

fit_res <- res_target %>%
  transmute(level = as.character(derived_residence), target_share = Freq / sum(Freq)) %>%
  left_join(share_1d(spx_cal, "derived_residence") %>% rename(unw_share = share), by = "level") %>%
  left_join(share_1d(spx_cal, "derived_residence", "w_calibrated_trim") %>% rename(w_share = share), by = "level") %>%
  mutate(diff_unw = unw_share - target_share,
         diff_w   = w_share   - target_share)

fit_edu <- edu_target %>%
  transmute(level = as.character(derived_edu_cat), target_share = Freq / sum(Freq)) %>%
  left_join(share_1d(spx_cal, "derived_edu_cat") %>% rename(unw_share = share), by = "level") %>%
  left_join(share_1d(spx_cal, "derived_edu_cat", "w_calibrated_trim") %>% rename(w_share = share), by = "level") %>%
  mutate(diff_unw = unw_share - target_share,
         diff_w   = w_share   - target_share)

fit_summary <- tibble(
  Margin = c("State", "Sex", "Age-group", "Residence", "Education"),
  MAD_unweighted = c(
    mean(abs(fit_state$diff_unw), na.rm = TRUE),
    mean(abs(fit_sex$diff_unw), na.rm = TRUE),
    mean(abs(fit_age$diff_unw), na.rm = TRUE),
    mean(abs(fit_res$diff_unw), na.rm = TRUE),
    mean(abs(fit_edu$diff_unw), na.rm = TRUE)
  ),
  MAD_weighted = c(
    mean(abs(fit_state$diff_w), na.rm = TRUE),
    mean(abs(fit_sex$diff_w), na.rm = TRUE),
    mean(abs(fit_age$diff_w), na.rm = TRUE),
    mean(abs(fit_res$diff_w), na.rm = TRUE),
    mean(abs(fit_edu$diff_w), na.rm = TRUE)
  ),
  MaxAbsDiff_unweighted = c(
    max(abs(fit_state$diff_unw), na.rm = TRUE),
    max(abs(fit_sex$diff_unw), na.rm = TRUE),
    max(abs(fit_age$diff_unw), na.rm = TRUE),
    max(abs(fit_res$diff_unw), na.rm = TRUE),
    max(abs(fit_edu$diff_unw), na.rm = TRUE)
  ),
  MaxAbsDiff_weighted = c(
    max(abs(fit_state$diff_w), na.rm = TRUE),
    max(abs(fit_sex$diff_w), na.rm = TRUE),
    max(abs(fit_age$diff_w), na.rm = TRUE),
    max(abs(fit_res$diff_w), na.rm = TRUE),
    max(abs(fit_edu$diff_w), na.rm = TRUE)
  )
) %>%
  mutate(
    mad_improve = (MAD_unweighted - MAD_weighted) / MAD_unweighted
  )

write_csv(fit_state, file.path(OUT_DIR, "FIT_state.csv"))
write_csv(fit_sex,   file.path(OUT_DIR, "FIT_sex.csv"))
write_csv(fit_age,   file.path(OUT_DIR, "FIT_age.csv"))
write_csv(fit_res,   file.path(OUT_DIR, "FIT_residence.csv"))
write_csv(fit_edu,   file.path(OUT_DIR, "FIT_education.csv"))
write_csv(fit_summary, file.path(OUT_DIR, "FIT_summary.csv"))

# ------------------------------------------------------------------------------
# 9) WEIGHT DIAGNOSTICS
# ------------------------------------------------------------------------------
ESS_untrimmed <- ess(spx_cal$w_calibrated)
ESS_trimmed   <- ess(spx_cal$w_calibrated_trim)

N_cal <- nrow(spx_cal)
ESS_ratio_untrimmed <- ESS_untrimmed / N_cal
ESS_ratio_trimmed   <- ESS_trimmed / N_cal

CV_untrimmed <- cv_w(spx_cal$w_calibrated)
CV_trimmed   <- cv_w(spx_cal$w_calibrated_trim)

DEFF_untrimmed <- 1 + (CV_untrimmed^2)
DEFF_trimmed   <- 1 + (CV_trimmed^2)

prop_trim_lo <- mean(spx_cal$w_calibrated < TRIM_LO, na.rm = TRUE)
prop_trim_hi <- mean(spx_cal$w_calibrated > TRIM_HI, na.rm = TRUE)

wt_diag <- tibble(
  n = nrow(spx_cal),
  min_w = min(spx_cal$w_calibrated, na.rm = TRUE),
  p1_w  = quantile(spx_cal$w_calibrated, 0.01, na.rm = TRUE),
  med_w = median(spx_cal$w_calibrated, na.rm = TRUE),
  p99_w = quantile(spx_cal$w_calibrated, 0.99, na.rm = TRUE),
  max_w = max(spx_cal$w_calibrated, na.rm = TRUE),
  prop_trim_lo = prop_trim_lo,
  prop_trim_hi = prop_trim_hi
)
write_csv(wt_diag, file.path(OUT_DIR, "FIT_weight_diagnostics.csv"))

weight_qc <- tibble(
  N_total = nrow(spx),
  N_calibration_eligible = N_cal,
  pct_calibration_eligible = round(N_cal / nrow(spx), 4),
  
  TRIM_LO = TRIM_LO,
  TRIM_HI = TRIM_HI,
  prop_trim_lo = round(prop_trim_lo, 4),
  prop_trim_hi = round(prop_trim_hi, 4),
  
  min_w_untrim = round(min(spx_cal$w_calibrated, na.rm = TRUE), 4),
  p01_w_untrim = round(quantile(spx_cal$w_calibrated, 0.01, na.rm = TRUE), 4),
  med_w_untrim = round(median(spx_cal$w_calibrated, na.rm = TRUE), 4),
  p99_w_untrim = round(quantile(spx_cal$w_calibrated, 0.99, na.rm = TRUE), 4),
  max_w_untrim = round(max(spx_cal$w_calibrated, na.rm = TRUE), 4),
  
  min_w_trim = round(min(spx_cal$w_calibrated_trim, na.rm = TRUE), 4),
  p01_w_trim = round(quantile(spx_cal$w_calibrated_trim, 0.01, na.rm = TRUE), 4),
  med_w_trim = round(median(spx_cal$w_calibrated_trim, na.rm = TRUE), 4),
  p99_w_trim = round(quantile(spx_cal$w_calibrated_trim, 0.99, na.rm = TRUE), 4),
  max_w_trim = round(max(spx_cal$w_calibrated_trim, na.rm = TRUE), 4),
  
  ESS_untrimmed = round(ESS_untrimmed, 2),
  ESS_trimmed   = round(ESS_trimmed, 2),
  ESS_ratio_untrimmed = round(ESS_ratio_untrimmed, 4),
  ESS_ratio_trimmed   = round(ESS_ratio_trimmed, 4),
  
  CV_untrimmed = round(CV_untrimmed, 4),
  CV_trimmed   = round(CV_trimmed, 4),
  DEFF_untrimmed = round(DEFF_untrimmed, 4),
  DEFF_trimmed   = round(DEFF_trimmed, 4)
)

write_csv(weight_qc, file.path(OUT_DIR, "WEIGHTING_QC_REPORT.csv"))

# ------------------------------------------------------------------------------
# 10) GATEKEEPER
# ------------------------------------------------------------------------------
pct_elig <- weight_qc$pct_calibration_eligible[[1]]
max_w    <- weight_qc$max_w_untrim[[1]]
prop_thi <- weight_qc$prop_trim_hi[[1]]
ess_r    <- weight_qc$ESS_ratio_trimmed[[1]]

worst_mad_improve <- min(fit_summary$mad_improve, na.rm = TRUE)
worst_maxabs_w    <- max(fit_summary$MaxAbsDiff_weighted, na.rm = TRUE)

fail_reasons <- c()

if (is.na(pct_elig) || pct_elig < THRESH_PCT_ELIGIBLE_MIN)
  fail_reasons <- c(fail_reasons, paste0("pct_calibration_eligible=", pct_elig, " < ", THRESH_PCT_ELIGIBLE_MIN))

if (is.na(max_w) || max_w > THRESH_MAX_W_UNTRIM)
  fail_reasons <- c(fail_reasons, paste0("max_w_untrim=", max_w, " > ", THRESH_MAX_W_UNTRIM))

if (is.na(prop_thi) || prop_thi > THRESH_PCT_TRIM_HI_MAX)
  fail_reasons <- c(fail_reasons, paste0("prop_trim_hi=", prop_thi, " > ", THRESH_PCT_TRIM_HI_MAX))

if (is.na(ess_r) || ess_r < THRESH_ESS_RATIO_MIN)
  fail_reasons <- c(fail_reasons, paste0("ESS_ratio_trimmed=", ess_r, " < ", THRESH_ESS_RATIO_MIN))

if (is.na(worst_mad_improve) || worst_mad_improve < THRESH_MAD_IMPROVE_MIN)
  fail_reasons <- c(fail_reasons, paste0("worst MAD improvement=", worst_mad_improve, " < ", THRESH_MAD_IMPROVE_MIN))

if (is.na(worst_maxabs_w) || worst_maxabs_w > THRESH_MAXABS_DIFF_MAX)
  fail_reasons <- c(fail_reasons, paste0("worst MaxAbsDiff_weighted=", worst_maxabs_w, " > ", THRESH_MAXABS_DIFF_MAX))

decision_log <- tibble(
  decision = ifelse(length(fail_reasons) == 0, "PASS: weights acceptable", "FAIL: weights suppressed"),
  reasons  = ifelse(length(fail_reasons) == 0, "None", paste(fail_reasons, collapse = " | "))
)
write_csv(decision_log, file.path(OUT_DIR, "WEIGHTING_DECISION_LOG.csv"))

# ------------------------------------------------------------------------------
# 11) MERGE BACK + EXPORT
# ------------------------------------------------------------------------------
if (length(fail_reasons) > 0) {
  message("⚠️ Weighting gatekeeper triggered. Weights will NOT be used.")
  message("Reasons: ", paste(fail_reasons, collapse = " | "))
  
  spx_nowt <- spx %>%
    mutate(
      w_calibrated = NA_real_,
      w_calibrated_trim = NA_real_
    )
  
  saveRDS(spx_nowt, OUT_SPROXIL_NOWT)
  message("✅ Saved: ", OUT_SPROXIL_NOWT)
  stop("Weighting suppressed by gatekeeper. See benchmarks/WEIGHTING_DECISION_LOG.csv")
}

spx_out <- spx %>%
  left_join(
    spx_cal %>% select(row_id, calibration_eligible, w_calibrated, w_calibrated_trim),
    by = "row_id"
  ) %>%
  mutate(
    calibration_eligible = ifelse(is.na(calibration_eligible), 0L, calibration_eligible)
  )

saveRDS(spx_out, OUT_SPROXIL_WT)

message("✅ Saved: ", OUT_SPROXIL_WT)
message("✅ Diagnostics written to: ", OUT_DIR)