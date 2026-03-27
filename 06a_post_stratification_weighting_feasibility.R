# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Integrated Weighting Feasibility, Impact Review, Finalisation, and Wealth Reconstruction
# AUTHOR:  Corona Management Systems
# DATE:    18 March 2026
#
# PURPOSE:
#   Run the full post-stratification weighting workflow in one script:
#
#   1. load the analysis-ready dataset
#   2. build and test candidate calibration schemes:
#        - state_only
#        - state_sex
#        - state_sex_age
#   3. assess support, fit, and weight stability
#   4. evaluate impact on flagship malaria indicators and subgroup/state estimates
#   5. choose the final scheme using scorecard logic
#   6. attach the selected final trimmed calibrated weight
#   7. rebuild the wealth index using the final weight
#   8. export the final weighted dataset and diagnostic outputs
#
# INPUT:
#   - Sproxil_Analysis_Ready.rds
#   - Sproxil_dictionary.rds
#
# OUTPUT:
#   - benchmarks/integrated_weighting/PREWEIGHT_benchmark_comparison.csv
#   - benchmarks/integrated_weighting/PREWEIGHT_fit_summary.csv
#   - benchmarks/integrated_weighting/SUPPORT_state.csv
#   - benchmarks/integrated_weighting/SUPPORT_state_sex.csv
#   - benchmarks/integrated_weighting/SUPPORT_state_sex_age.csv
#   - benchmarks/integrated_weighting/SCHEME_SUMMARY.csv
#   - benchmarks/integrated_weighting/SCHEME_FIT_SUMMARY.csv
#   - benchmarks/integrated_weighting/SCHEME_MARGIN_COMPARISONS.csv
#   - benchmarks/integrated_weighting/SCHEME_TEST_WEIGHTS.csv
#   - benchmarks/integrated_weighting/WEIGHT_IMPACT_NATIONAL.csv
#   - benchmarks/integrated_weighting/WEIGHT_IMPACT_SUBGROUP.csv
#   - benchmarks/integrated_weighting/WEIGHT_IMPACT_STATE.csv
#   - benchmarks/integrated_weighting/WEIGHT_SCHEME_SCORECARD.csv
#   - benchmarks/integrated_weighting/WEIGHT_SCHEME_RECOMMENDATION.csv
#   - Sproxil_Analysis_Ready_Weighted_Wealth.rds
#   - Sproxil_Analysis_Ready_Weighted_Wealth.csv
#   - Sproxil_Analysis_Ready_Weighted_Wealth.sav
#   - Sproxil_Weighted_Wealth_QC.csv
#
# INTERPRETATION NOTE:
#   The final weight is a calibration / post-stratification weight for the
#   respondent survey, not a probability-sample design weight.
#
#   The resulting wealth quintile is a weighted sample-derived living-standards
#   index for the Sproxil survey, not a DHS/MIS-equivalent national wealth index.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(survey)
  library(purrr)
  library(stringr)
  library(haven)
  library(labelled)
  library(FactoMineR)
})

# ------------------------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------------------------
INPUT_RDS  <- "Sproxil_Analysis_Ready.rds"
INPUT_DICT <- "Sproxil_dictionary.rds"

OUT_DIR <- file.path("benchmarks", "integrated_weighting")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

FINAL_OUTPUT_RDS <- "Sproxil_Analysis_Ready_Weighted_Wealth.rds"
FINAL_OUTPUT_CSV <- "Sproxil_Analysis_Ready_Weighted_Wealth.csv"
FINAL_OUTPUT_SAV <- "Sproxil_Analysis_Ready_Weighted_Wealth.sav"
WEALTH_QC_OUTPUT <- "Sproxil_Weighted_Wealth_QC.csv"

FINAL_WEIGHT_VAR <- "w_calibrated_trim"

# Candidate schemes
SCHEME_ORDER <- tibble(
  scheme = c("state_only", "state_sex", "state_sex_age"),
  complexity_rank = c(1, 2, 3)
)

# Trim band on NORMALIZED weights
TRIM_LO <- 0.30
TRIM_HI <- 3.00

# Hard-stop feasibility rules
HARD_MIN_PCT_ELIGIBLE <- 0.60

# Support warning thresholds
WARN_MIN_STATE_N         <- 30
WARN_MIN_STATE_SEX_N     <- 15
WARN_MIN_STATE_SEX_AGE_N <- 5

# Raking controls
RAKE_MAXIT   <- 100
RAKE_EPSILON <- 1e-06

# Flagship indicators for national impact testing
FLAGSHIP_INDICATORS <- c(
  "derived_hh_has_any_net",
  "derived_hh_has_itn",
  "derived_access_itn",
  "derived_net_use_any_last_night",
  "derived_anc_any",
  "derived_anc_skilled",
  "derived_anc_4plus",
  "derived_iptp_2plus",
  "derived_iptp_3plus",
  "derived_child_fever",
  "derived_fever_seek_advice",
  "derived_fever_prompt_care",
  "derived_fever_tested",
  "derived_fever_took_act",
  "derived_percep_affordable"
)

# Smaller set for state stability testing
STATE_IMPACT_INDICATORS <- c(
  "derived_hh_has_any_net",
  "derived_access_itn",
  "derived_anc_any",
  "derived_iptp_3plus",
  "derived_fever_seek_advice",
  "derived_fever_took_act"
)

# Subgroup variables for estimate stability review
SUBGROUP_VARS <- c(
  "derived_residence",
  "demo_gender_num",
  "age_group_4",
  "derived_edu_cat"
)

MIN_UNWTD_N_SUBGROUP <- 30
MIN_UNWTD_N_STATE    <- 50

# Wealth variable screening thresholds
MIN_NONMISS_N_WEALTH <- 30
MIN_PREV_WEALTH      <- 0.01
MAX_PREV_WEALTH      <- 0.99

# ------------------------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------------------------
safe_num <- function(x) suppressWarnings(as.integer(as.numeric(as.character(x))))

ess <- function(w) {
  w <- w[is.finite(w) & !is.na(w) & w > 0]
  if (length(w) == 0) return(NA_real_)
  denom <- sum(w^2)
  if (denom <= 0) return(NA_real_)
  (sum(w)^2) / denom
}

cv_w <- function(w) {
  w <- w[is.finite(w) & !is.na(w) & w > 0]
  if (length(w) == 0) return(NA_real_)
  mu <- mean(w)
  if (!is.finite(mu) || mu == 0) return(NA_real_)
  sd(w) / mu
}

normalize_weights <- function(w) {
  mu <- mean(w, na.rm = TRUE)
  if (!is.finite(mu) || mu == 0) return(rep(NA_real_, length(w)))
  w / mu
}

trim_rescale <- function(w, lo, hi) {
  w2 <- pmin(pmax(w, lo), hi)
  mu <- mean(w2, na.rm = TRUE)
  if (!is.finite(mu) || mu == 0) return(rep(NA_real_, length(w2)))
  w2 / mu
}

check_support_1d <- function(sample_levels, pop_levels) {
  setdiff(as.character(pop_levels), as.character(sample_levels))
}

compare_margin <- function(df, var, target_df, target_var, weight_var = NULL) {
  target_comp <- target_df %>%
    transmute(
      level = as.character(.data[[target_var]]),
      benchmark_n = Freq,
      benchmark_share = Freq / sum(Freq)
    )
  
  sample_comp <- if (is.null(weight_var)) {
    df %>%
      filter(!is.na(.data[[var]])) %>%
      count(.data[[var]], name = "sample_n") %>%
      mutate(sample_share = sample_n / sum(sample_n)) %>%
      transmute(level = as.character(.data[[var]]), sample_n, sample_share)
  } else {
    df %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[weight_var]]), is.finite(.data[[weight_var]]), .data[[weight_var]] > 0) %>%
      group_by(.data[[var]]) %>%
      summarise(
        sample_n = n(),
        weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(sample_share = weighted_n / sum(weighted_n)) %>%
      transmute(level = as.character(.data[[var]]), sample_n, weighted_n, sample_share)
  }
  
  target_comp %>%
    left_join(sample_comp, by = "level") %>%
    mutate(abs_diff = abs(sample_share - benchmark_share))
}

margin_summary <- function(comp_df, scheme_name, margin_name) {
  tibble(
    scheme = scheme_name,
    margin = margin_name,
    MAD = mean(abs(comp_df$sample_share - comp_df$benchmark_share), na.rm = TRUE),
    MaxAbsDiff = max(abs(comp_df$sample_share - comp_df$benchmark_share), na.rm = TRUE)
  )
}

safe_mean <- function(x, w = NULL) {
  ok <- is.finite(x) & !is.na(x)
  if (is.null(w)) {
    if (!any(ok)) return(NA_real_)
    return(mean(x[ok]))
  }
  ok <- ok & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

safe_n <- function(x) {
  sum(!is.na(x) & is.finite(x))
}

rate_band <- function(x, good, acceptable, concerning, higher_is_better = TRUE) {
  if (is.na(x)) return("missing")
  if (higher_is_better) {
    if (x >= good) return("good")
    if (x >= acceptable) return("acceptable")
    if (x >= concerning) return("concerning")
    return("poor")
  } else {
    if (x <= good) return("good")
    if (x <= acceptable) return("acceptable")
    if (x <= concerning) return("concerning")
    return("poor")
  }
}

band_score <- function(band) {
  case_when(
    band == "good" ~ 3,
    band == "acceptable" ~ 2,
    band == "concerning" ~ 1,
    band == "poor" ~ 0,
    TRUE ~ NA_real_
  )
}

make_group_label <- function(x, varname) {
  case_when(
    varname == "derived_residence" & x == 1 ~ "Urban",
    varname == "derived_residence" & x == 2 ~ "Rural",
    varname == "demo_gender_num" & x == 1 ~ "Male",
    varname == "demo_gender_num" & x == 2 ~ "Female",
    varname == "age_group_4" & x == 1 ~ "15-24",
    varname == "age_group_4" & x == 2 ~ "25-34",
    varname == "age_group_4" & x == 3 ~ "35-44",
    varname == "age_group_4" & x == 4 ~ "45+",
    varname == "derived_edu_cat" & x == 1 ~ "No education",
    varname == "derived_edu_cat" & x == 2 ~ "Primary",
    varname == "derived_edu_cat" & x == 3 ~ "Secondary",
    varname == "derived_edu_cat" & x == 4 ~ "More than secondary",
    TRUE ~ as.character(x)
  )
}

weighted_indicator_table <- function(df, indicators, scheme_name, weight_var = NULL) {
  map_dfr(indicators, function(v) {
    if (!v %in% names(df)) return(NULL)
    x <- df[[v]]
    tibble(
      scheme = scheme_name,
      indicator = v,
      estimate = safe_mean(x, if (is.null(weight_var)) NULL else df[[weight_var]]),
      n_unwtd = safe_n(x),
      ess = if (is.null(weight_var)) NA_real_ else ess(df[[weight_var]][!is.na(x) & is.finite(x)])
    )
  })
}

weighted_subgroup_table <- function(df, indicators, subgroup_var, scheme_name, weight_var = NULL) {
  if (!subgroup_var %in% names(df)) return(NULL)
  map_dfr(indicators, function(v) {
    if (!v %in% names(df)) return(NULL)
    tmp <- df %>%
      mutate(.x = .data[[v]], .g = .data[[subgroup_var]]) %>%
      filter(!is.na(.g), !is.na(.x), is.finite(.x)) %>%
      group_by(.g) %>%
      summarise(
        estimate = safe_mean(.x, if (is.null(weight_var)) NULL else .data[[weight_var]]),
        n_unwtd = n(),
        ess = if (is.null(weight_var)) NA_real_ else ess(.data[[weight_var]]),
        .groups = "drop"
      ) %>%
      mutate(
        subgroup_var = subgroup_var,
        subgroup = make_group_label(.g, subgroup_var),
        scheme = scheme_name,
        indicator = v
      ) %>%
      select(scheme, subgroup_var, subgroup, indicator, estimate, n_unwtd, ess)
    tmp
  })
}

weighted_state_table <- function(df, indicators, scheme_name, weight_var = NULL) {
  if (!"demo_state_num" %in% names(df)) return(NULL)
  map_dfr(indicators, function(v) {
    if (!v %in% names(df)) return(NULL)
    tmp <- df %>%
      mutate(.x = .data[[v]]) %>%
      filter(!is.na(demo_state_num), demo_state_num %in% 1:37, !is.na(.x), is.finite(.x)) %>%
      group_by(demo_state_num) %>%
      summarise(
        estimate = safe_mean(.x, if (is.null(weight_var)) NULL else .data[[weight_var]]),
        n_unwtd = n(),
        ess = if (is.null(weight_var)) NA_real_ else ess(.data[[weight_var]]),
        .groups = "drop"
      ) %>%
      mutate(
        scheme = scheme_name,
        indicator = v
      ) %>%
      select(scheme, demo_state_num, indicator, estimate, n_unwtd, ess)
    tmp
  })
}

to_num <- function(x) {
  if (is.factor(x)) return(suppressWarnings(as.numeric(as.character(x))))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  suppressWarnings(as.numeric(x))
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) {
    y <- x[is.finite(x) & !is.na(x)]
    if (length(y) == 0) return(NA_real_)
    return(mean(y))
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_prev_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

make_dummy <- function(x, code, na_codes = c(8, 9, 98, 99, 95, 96)) {
  case_when(
    is.na(x) ~ NA_real_,
    x == code ~ 1,
    x %in% na_codes ~ NA_real_,
    TRUE ~ 0
  )
}

weighted_quintile <- function(score, w) {
  out <- rep(NA_real_, length(score))
  ok <- is.finite(score) & !is.na(score) & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) return(out)
  
  ord <- order(score[ok], seq_along(score[ok]))
  idx <- which(ok)[ord]
  w_ord <- w[idx]
  cw <- cumsum(w_ord) / sum(w_ord)
  
  q <- case_when(
    cw <= 0.20 ~ 1,
    cw <= 0.40 ~ 2,
    cw <= 0.60 ~ 3,
    cw <= 0.80 ~ 4,
    TRUE ~ 5
  )
  
  out[idx] <- q
  out
}

# ------------------------------------------------------------------------------
# 3. Load inputs
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_RDS)) stop("Missing dataset: ", INPUT_RDS)
if (!file.exists(INPUT_DICT)) stop("Missing dictionary: ", INPUT_DICT)

spx <- readRDS(INPUT_RDS)
meta <- readRDS(INPUT_DICT)

var_labels <- meta$variable_labels
val_labels_meta <- meta$value_labels

# ------------------------------------------------------------------------------
# 4. Embedded benchmarks
# ------------------------------------------------------------------------------
state_total_2020 <- tibble::tribble(
  ~demo_state_num, ~Freq_total_2020,
  1,  3941279,
  2,  4657314,
  3,  4847542,
  4,  5720035,
  5,  7788504,
  6,  2444028,
  7,  5905747,
  8,  5875471,
  9,  4253698,
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

age_target <- tibble::tribble(
  ~age_group_4, ~Freq,
  1, 38214473,
  2, 28394771,
  3, 22932908,
  4, 23046279
)

sex_target <- tibble::tribble(
  ~demo_gender_num, ~Freq,
  1, 56068035,
  2, 56520396
)

NATIONAL_TOTAL_2020 <- 206283338
NATIONAL_15_64_2020 <- 112588431
WORKING_AGE_SHARE_2020 <- NATIONAL_15_64_2020 / NATIONAL_TOTAL_2020

state_target <- state_total_2020 %>%
  mutate(Freq_raw_15_64 = Freq_total_2020 * WORKING_AGE_SHARE_2020)

state_floor <- floor(state_target$Freq_raw_15_64)
state_remainder <- state_target$Freq_raw_15_64 - state_floor
gap_to_fill <- NATIONAL_15_64_2020 - sum(state_floor)

state_target$Freq <- state_floor
if (gap_to_fill > 0) {
  add_idx <- order(state_remainder, decreasing = TRUE)[seq_len(gap_to_fill)]
  state_target$Freq[add_idx] <- state_target$Freq[add_idx] + 1L
}

state_target <- state_target %>%
  select(demo_state_num, Freq)

stopifnot(sum(state_target$Freq) == NATIONAL_15_64_2020)
stopifnot(sum(age_target$Freq)   == NATIONAL_15_64_2020)
stopifnot(sum(sex_target$Freq)   == NATIONAL_15_64_2020)

write_csv(state_target, file.path(OUT_DIR, "BENCHMARK_state_15_64_hybrid.csv"))
write_csv(sex_target,   file.path(OUT_DIR, "BENCHMARK_sex_15_64_official.csv"))
write_csv(age_target,   file.path(OUT_DIR, "BENCHMARK_age_15_64_official.csv"))

# ------------------------------------------------------------------------------
# 5. Standardise variables and row id
# ------------------------------------------------------------------------------
spx <- spx %>%
  mutate(
    row_id = row_number(),
    demo_state_num  = safe_num(demo_state_num),
    demo_gender_num = safe_num(demo_gender_num),
    age_group_4     = safe_num(age_group_4)
  )

if ("u_resp_15_64" %in% names(spx)) {
  spx <- spx %>% filter(u_resp_15_64 == 1)
}

# ------------------------------------------------------------------------------
# 6. Pre-weighting benchmark comparison
# ------------------------------------------------------------------------------
pre_state <- compare_margin(
  df = spx %>% filter(demo_state_num %in% 1:37),
  var = "demo_state_num",
  target_df = state_target,
  target_var = "demo_state_num"
) %>%
  mutate(margin = "state")

pre_sex <- compare_margin(
  df = spx %>% filter(demo_gender_num %in% c(1, 2)),
  var = "demo_gender_num",
  target_df = sex_target,
  target_var = "demo_gender_num"
) %>%
  mutate(margin = "sex")

pre_age <- compare_margin(
  df = spx %>% filter(age_group_4 %in% 1:4),
  var = "age_group_4",
  target_df = age_target,
  target_var = "age_group_4"
) %>%
  mutate(margin = "age_group_4")

pre_benchmark_comparison <- bind_rows(pre_state, pre_sex, pre_age)
write_csv(pre_benchmark_comparison, file.path(OUT_DIR, "PREWEIGHT_benchmark_comparison.csv"))

pre_fit_summary <- bind_rows(
  margin_summary(pre_state, "unweighted", "state"),
  margin_summary(pre_sex,   "unweighted", "sex"),
  margin_summary(pre_age,   "unweighted", "age_group_4")
)

write_csv(pre_fit_summary, file.path(OUT_DIR, "PREWEIGHT_fit_summary.csv"))

# ------------------------------------------------------------------------------
# 7. Support diagnostics
# ------------------------------------------------------------------------------
support_state <- spx %>%
  filter(demo_state_num %in% 1:37) %>%
  count(demo_state_num, name = "n_state") %>%
  mutate(flag_low_state_n = n_state < WARN_MIN_STATE_N)

support_state_sex <- spx %>%
  filter(demo_state_num %in% 1:37, demo_gender_num %in% c(1, 2)) %>%
  count(demo_state_num, demo_gender_num, name = "n_state_sex") %>%
  mutate(flag_low_state_sex_n = n_state_sex < WARN_MIN_STATE_SEX_N)

support_state_sex_age <- spx %>%
  filter(demo_state_num %in% 1:37, demo_gender_num %in% c(1, 2), age_group_4 %in% 1:4) %>%
  count(demo_state_num, demo_gender_num, age_group_4, name = "n_state_sex_age") %>%
  mutate(flag_low_state_sex_age_n = n_state_sex_age < WARN_MIN_STATE_SEX_AGE_N)

write_csv(support_state, file.path(OUT_DIR, "SUPPORT_state.csv"))
write_csv(support_state_sex, file.path(OUT_DIR, "SUPPORT_state_sex.csv"))
write_csv(support_state_sex_age, file.path(OUT_DIR, "SUPPORT_state_sex_age.csv"))

# ------------------------------------------------------------------------------
# 8. Candidate weighting schemes
# ------------------------------------------------------------------------------
scheme_specs <- list(
  state_only = list(
    vars = c("demo_state_num"),
    valid_expr = quote(demo_state_num %in% 1:37),
    factors = c("f_state"),
    sample_margins = list(~f_state)
  ),
  state_sex = list(
    vars = c("demo_state_num", "demo_gender_num"),
    valid_expr = quote(demo_state_num %in% 1:37 & demo_gender_num %in% c(1, 2)),
    factors = c("f_state", "f_sex"),
    sample_margins = list(~f_state, ~f_sex)
  ),
  state_sex_age = list(
    vars = c("demo_state_num", "demo_gender_num", "age_group_4"),
    valid_expr = quote(demo_state_num %in% 1:37 & demo_gender_num %in% c(1, 2) & age_group_4 %in% 1:4),
    factors = c("f_state", "f_sex", "f_age"),
    sample_margins = list(~f_state, ~f_sex, ~f_age)
  )
)

# ------------------------------------------------------------------------------
# 9. Scheme runner
# ------------------------------------------------------------------------------
run_scheme <- function(df, scheme_name, scheme_spec) {
  
  elig_vec <- with(df, eval(scheme_spec$valid_expr))
  
  df_scheme <- df %>%
    mutate(calibration_eligible = ifelse(elig_vec, 1L, 0L))
  
  df_cal <- df_scheme %>% filter(calibration_eligible == 1)
  pct_eligible <- nrow(df_cal) / nrow(df_scheme)
  
  if (nrow(df_cal) == 0) {
    return(list(
      summary = tibble(
        scheme = scheme_name,
        status = "FAIL",
        reason = "No eligible records",
        N_total = nrow(df_scheme),
        N_eligible = 0,
        pct_eligible = 0
      ),
      fit = NULL,
      comparisons = NULL,
      weights = NULL
    ))
  }
  
  if ("demo_state_num" %in% scheme_spec$vars) {
    miss <- check_support_1d(unique(df_cal$demo_state_num), state_target$demo_state_num)
    if (length(miss) > 0) {
      return(list(
        summary = tibble(
          scheme = scheme_name,
          status = "FAIL",
          reason = paste0("State support missing: ", paste(miss, collapse = ",")),
          N_total = nrow(df_scheme),
          N_eligible = nrow(df_cal),
          pct_eligible = round(pct_eligible, 4)
        ),
        fit = NULL,
        comparisons = NULL,
        weights = NULL
      ))
    }
    df_cal <- df_cal %>%
      mutate(f_state = factor(demo_state_num, levels = sort(unique(state_target$demo_state_num))))
  }
  
  if ("demo_gender_num" %in% scheme_spec$vars) {
    miss <- check_support_1d(unique(df_cal$demo_gender_num), sex_target$demo_gender_num)
    if (length(miss) > 0) {
      return(list(
        summary = tibble(
          scheme = scheme_name,
          status = "FAIL",
          reason = paste0("Sex support missing: ", paste(miss, collapse = ",")),
          N_total = nrow(df_scheme),
          N_eligible = nrow(df_cal),
          pct_eligible = round(pct_eligible, 4)
        ),
        fit = NULL,
        comparisons = NULL,
        weights = NULL
      ))
    }
    df_cal <- df_cal %>%
      mutate(f_sex = factor(demo_gender_num, levels = sort(unique(sex_target$demo_gender_num))))
  }
  
  if ("age_group_4" %in% scheme_spec$vars) {
    miss <- check_support_1d(unique(df_cal$age_group_4), age_target$age_group_4)
    if (length(miss) > 0) {
      return(list(
        summary = tibble(
          scheme = scheme_name,
          status = "FAIL",
          reason = paste0("Age support missing: ", paste(miss, collapse = ",")),
          N_total = nrow(df_scheme),
          N_eligible = nrow(df_cal),
          pct_eligible = round(pct_eligible, 4)
        ),
        fit = NULL,
        comparisons = NULL,
        weights = NULL
      ))
    }
    df_cal <- df_cal %>%
      mutate(f_age = factor(age_group_4, levels = sort(unique(age_target$age_group_4))))
  }
  
  des0 <- svydesign(ids = ~1, data = df_cal, weights = ~1)
  
  pop_margins <- list()
  
  if ("f_state" %in% scheme_spec$factors) {
    pop_margins <- c(pop_margins, list(
      state_target %>%
        transmute(f_state = factor(demo_state_num, levels = levels(df_cal$f_state)), Freq = Freq)
    ))
  }
  
  if ("f_sex" %in% scheme_spec$factors) {
    pop_margins <- c(pop_margins, list(
      sex_target %>%
        transmute(f_sex = factor(demo_gender_num, levels = levels(df_cal$f_sex)), Freq = Freq)
    ))
  }
  
  if ("f_age" %in% scheme_spec$factors) {
    pop_margins <- c(pop_margins, list(
      age_target %>%
        transmute(f_age = factor(age_group_4, levels = levels(df_cal$f_age)), Freq = Freq)
    ))
  }
  
  des_raked <- tryCatch(
    rake(
      design = des0,
      sample.margins = scheme_spec$sample_margins,
      population.margins = pop_margins,
      control = list(maxit = RAKE_MAXIT, epsilon = RAKE_EPSILON, verbose = FALSE)
    ),
    error = function(e) e
  )
  
  if (inherits(des_raked, "error")) {
    return(list(
      summary = tibble(
        scheme = scheme_name,
        status = "FAIL",
        reason = paste0("Raking failed: ", des_raked$message),
        N_total = nrow(df_scheme),
        N_eligible = nrow(df_cal),
        pct_eligible = round(pct_eligible, 4)
      ),
      fit = NULL,
      comparisons = NULL,
      weights = NULL
    ))
  }
  
  df_cal$w_raw <- as.numeric(weights(des_raked))
  df_cal$w_norm_untrim <- normalize_weights(df_cal$w_raw)
  df_cal$w_norm_trim <- trim_rescale(df_cal$w_norm_untrim, TRIM_LO, TRIM_HI)
  
  if (all(is.na(df_cal$w_norm_trim))) {
    return(list(
      summary = tibble(
        scheme = scheme_name,
        status = "FAIL",
        reason = "Trimmed normalized weights are all NA",
        N_total = nrow(df_scheme),
        N_eligible = nrow(df_cal),
        pct_eligible = round(pct_eligible, 4)
      ),
      fit = NULL,
      comparisons = NULL,
      weights = NULL
    ))
  }
  
  comp_list <- list()
  
  if ("demo_state_num" %in% scheme_spec$vars) {
    comp_list <- c(comp_list, list(
      compare_margin(df_cal, "demo_state_num", state_target, "demo_state_num") %>%
        mutate(scheme = scheme_name, stage = "unweighted", margin = "state"),
      compare_margin(df_cal, "demo_state_num", state_target, "demo_state_num", "w_norm_trim") %>%
        mutate(scheme = scheme_name, stage = "weighted_trimmed", margin = "state")
    ))
  }
  
  if ("demo_gender_num" %in% scheme_spec$vars) {
    comp_list <- c(comp_list, list(
      compare_margin(df_cal, "demo_gender_num", sex_target, "demo_gender_num") %>%
        mutate(scheme = scheme_name, stage = "unweighted", margin = "sex"),
      compare_margin(df_cal, "demo_gender_num", sex_target, "demo_gender_num", "w_norm_trim") %>%
        mutate(scheme = scheme_name, stage = "weighted_trimmed", margin = "sex")
    ))
  }
  
  if ("age_group_4" %in% scheme_spec$vars) {
    comp_list <- c(comp_list, list(
      compare_margin(df_cal, "age_group_4", age_target, "age_group_4") %>%
        mutate(scheme = scheme_name, stage = "unweighted", margin = "age_group_4"),
      compare_margin(df_cal, "age_group_4", age_target, "age_group_4", "w_norm_trim") %>%
        mutate(scheme = scheme_name, stage = "weighted_trimmed", margin = "age_group_4")
    ))
  }
  
  comparisons <- bind_rows(comp_list)
  
  fit_summary <- comparisons %>%
    group_by(scheme, stage, margin) %>%
    summarise(
      MAD = mean(abs(sample_share - benchmark_share), na.rm = TRUE),
      MaxAbsDiff = max(abs(sample_share - benchmark_share), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = stage,
      values_from = c(MAD, MaxAbsDiff)
    ) %>%
    mutate(
      mad_improve = ifelse(
        is.na(MAD_unweighted) | MAD_unweighted == 0,
        NA_real_,
        (MAD_unweighted - MAD_weighted_trimmed) / MAD_unweighted
      )
    )
  
  ESS_untrim <- ess(df_cal$w_norm_untrim)
  ESS_trim   <- ess(df_cal$w_norm_trim)
  
  CV_untrim <- cv_w(df_cal$w_norm_untrim)
  CV_trim   <- cv_w(df_cal$w_norm_trim)
  
  DEFF_untrim <- ifelse(is.na(CV_untrim), NA_real_, 1 + CV_untrim^2)
  DEFF_trim   <- ifelse(is.na(CV_trim), NA_real_, 1 + CV_trim^2)
  
  prop_trim_lo <- mean(df_cal$w_norm_untrim < TRIM_LO, na.rm = TRUE)
  prop_trim_hi <- mean(df_cal$w_norm_untrim > TRIM_HI, na.rm = TRUE)
  
  worst_mad_improve <- min(fit_summary$mad_improve, na.rm = TRUE)
  worst_maxabs_trim <- max(fit_summary$MaxAbsDiff_weighted_trimmed, na.rm = TRUE)
  
  fail_reasons <- c()
  
  if (is.na(pct_eligible) || pct_eligible < HARD_MIN_PCT_ELIGIBLE) {
    fail_reasons <- c(fail_reasons, paste0("pct_eligible=", round(pct_eligible, 4), " < ", HARD_MIN_PCT_ELIGIBLE))
  }
  
  max_w_norm_untrim <- max(df_cal$w_norm_untrim, na.rm = TRUE)
  max_w_norm_trim   <- max(df_cal$w_norm_trim, na.rm = TRUE)
  
  summary_row <- tibble(
    scheme = scheme_name,
    status = ifelse(length(fail_reasons) == 0, "PASS", "FAIL"),
    reason = ifelse(length(fail_reasons) == 0, "None", paste(fail_reasons, collapse = " | ")),
    
    N_total = nrow(df_scheme),
    N_eligible = nrow(df_cal),
    pct_eligible = round(pct_eligible, 4),
    
    min_w_norm_untrim = round(min(df_cal$w_norm_untrim, na.rm = TRUE), 4),
    p01_w_norm_untrim = round(as.numeric(quantile(df_cal$w_norm_untrim, 0.01, na.rm = TRUE)), 4),
    med_w_norm_untrim = round(median(df_cal$w_norm_untrim, na.rm = TRUE), 4),
    p99_w_norm_untrim = round(as.numeric(quantile(df_cal$w_norm_untrim, 0.99, na.rm = TRUE)), 4),
    max_w_norm_untrim = round(max_w_norm_untrim, 4),
    
    min_w_norm_trim = round(min(df_cal$w_norm_trim, na.rm = TRUE), 4),
    p01_w_norm_trim = round(as.numeric(quantile(df_cal$w_norm_trim, 0.01, na.rm = TRUE)), 4),
    med_w_norm_trim = round(median(df_cal$w_norm_trim, na.rm = TRUE), 4),
    p99_w_norm_trim = round(as.numeric(quantile(df_cal$w_norm_trim, 0.99, na.rm = TRUE)), 4),
    max_w_norm_trim = round(max_w_norm_trim, 4),
    
    prop_trim_lo = round(prop_trim_lo, 4),
    prop_trim_hi = round(prop_trim_hi, 4),
    
    ESS_untrim = round(ESS_untrim, 2),
    ESS_trim = round(ESS_trim, 2),
    ESS_ratio_trim = round(ESS_trim / nrow(df_cal), 4),
    
    CV_untrim = round(CV_untrim, 4),
    CV_trim = round(CV_trim, 4),
    DEFF_untrim = round(DEFF_untrim, 4),
    DEFF_trim = round(DEFF_trim, 4),
    
    worst_mad_improve = round(worst_mad_improve, 4),
    worst_maxabs_trim = round(worst_maxabs_trim, 4)
  )
  
  weights_out <- df_cal %>%
    select(row_id, all_of(scheme_spec$vars), w_raw, w_norm_untrim, w_norm_trim)
  
  list(
    summary = summary_row,
    fit = fit_summary,
    comparisons = comparisons,
    weights = weights_out
  )
}

# ------------------------------------------------------------------------------
# 10. Run candidate schemes
# ------------------------------------------------------------------------------
results <- imap(scheme_specs, ~ run_scheme(spx, .y, .x))

scheme_summary <- bind_rows(map(results, "summary"))
scheme_fit <- bind_rows(compact(map(results, "fit")))
scheme_comparisons <- bind_rows(compact(map(results, "comparisons")))
scheme_weights <- bind_rows(
  map2(
    results,
    names(results),
    ~ {
      w <- .x$weights
      if (is.null(w)) return(NULL)
      w %>% mutate(scheme = .y)
    }
  )
)

write_csv(scheme_summary, file.path(OUT_DIR, "SCHEME_SUMMARY.csv"))
write_csv(scheme_fit, file.path(OUT_DIR, "SCHEME_FIT_SUMMARY.csv"))
write_csv(scheme_comparisons, file.path(OUT_DIR, "SCHEME_MARGIN_COMPARISONS.csv"))
write_csv(scheme_weights, file.path(OUT_DIR, "SCHEME_TEST_WEIGHTS.csv"))

# ------------------------------------------------------------------------------
# 11. Build analysis dataset for each scheme for impact review
# ------------------------------------------------------------------------------
scheme_data <- list()

scheme_data[["unweighted"]] <- spx %>%
  mutate(w_use = 1)

valid_schemes <- scheme_summary %>%
  filter(!is.na(scheme)) %>%
  pull(scheme)

for (sc in valid_schemes) {
  w_sc <- scheme_weights %>%
    filter(scheme == sc) %>%
    select(row_id, w_norm_trim)
  
  scheme_data[[sc]] <- spx %>%
    left_join(w_sc, by = "row_id") %>%
    mutate(w_use = w_norm_trim)
}

# ------------------------------------------------------------------------------
# 12. National impact review
# ------------------------------------------------------------------------------
national_impact <- bind_rows(
  weighted_indicator_table(scheme_data[["unweighted"]], FLAGSHIP_INDICATORS, "unweighted", NULL),
  map_dfr(valid_schemes, function(sc) {
    weighted_indicator_table(scheme_data[[sc]], FLAGSHIP_INDICATORS, sc, "w_use")
  })
)

national_compare <- national_impact %>%
  select(scheme, indicator, estimate, n_unwtd, ess) %>%
  pivot_wider(
    names_from = scheme,
    values_from = c(estimate, n_unwtd, ess),
    names_sep = "__"
  )

for (sc in valid_schemes) {
  est_sc <- paste0("estimate__", sc)
  est_u  <- "estimate__unweighted"
  diff_nm <- paste0("pp_diff__", sc)
  if (est_sc %in% names(national_compare) && est_u %in% names(national_compare)) {
    national_compare[[diff_nm]] <- abs(100 * (national_compare[[est_sc]] - national_compare[[est_u]]))
  }
}

write_csv(national_compare, file.path(OUT_DIR, "WEIGHT_IMPACT_NATIONAL.csv"))

# ------------------------------------------------------------------------------
# 13. Subgroup impact review
# ------------------------------------------------------------------------------
subgroup_impact <- bind_rows(
  map_dfr(SUBGROUP_VARS, function(g) {
    weighted_subgroup_table(scheme_data[["unweighted"]], FLAGSHIP_INDICATORS, g, "unweighted", NULL)
  }),
  map_dfr(valid_schemes, function(sc) {
    map_dfr(SUBGROUP_VARS, function(g) {
      weighted_subgroup_table(scheme_data[[sc]], FLAGSHIP_INDICATORS, g, sc, "w_use")
    })
  })
) %>%
  filter(n_unwtd >= MIN_UNWTD_N_SUBGROUP)

subgroup_compare <- subgroup_impact %>%
  select(scheme, subgroup_var, subgroup, indicator, estimate, n_unwtd, ess) %>%
  pivot_wider(
    names_from = scheme,
    values_from = c(estimate, n_unwtd, ess),
    names_sep = "__"
  )

for (sc in valid_schemes) {
  sc_est <- paste0("estimate__", sc)
  if (sc_est %in% names(subgroup_compare) && "estimate__unweighted" %in% names(subgroup_compare)) {
    subgroup_compare[[paste0("pp_diff__", sc)]] <-
      abs(100 * (subgroup_compare[[sc_est]] - subgroup_compare[["estimate__unweighted"]]))
  }
}

write_csv(subgroup_compare, file.path(OUT_DIR, "WEIGHT_IMPACT_SUBGROUP.csv"))

# ------------------------------------------------------------------------------
# 14. State impact review
# ------------------------------------------------------------------------------
state_impact <- bind_rows(
  weighted_state_table(scheme_data[["unweighted"]], STATE_IMPACT_INDICATORS, "unweighted", NULL),
  map_dfr(valid_schemes, function(sc) {
    weighted_state_table(scheme_data[[sc]], STATE_IMPACT_INDICATORS, sc, "w_use")
  })
) %>%
  filter(n_unwtd >= MIN_UNWTD_N_STATE)

state_compare <- state_impact %>%
  select(scheme, demo_state_num, indicator, estimate, n_unwtd, ess) %>%
  pivot_wider(
    names_from = scheme,
    values_from = c(estimate, n_unwtd, ess),
    names_sep = "__"
  )

for (sc in valid_schemes) {
  sc_est <- paste0("estimate__", sc)
  if (sc_est %in% names(state_compare) && "estimate__unweighted" %in% names(state_compare)) {
    state_compare[[paste0("pp_diff__", sc)]] <-
      abs(100 * (state_compare[[sc_est]] - state_compare[["estimate__unweighted"]]))
  }
}

write_csv(state_compare, file.path(OUT_DIR, "WEIGHT_IMPACT_STATE.csv"))

# ------------------------------------------------------------------------------
# 15. Summarise estimate impact by scheme
# ------------------------------------------------------------------------------
estimate_impact_summary <- map_dfr(valid_schemes, function(sc) {
  nat_pp <- if (paste0("pp_diff__", sc) %in% names(national_compare)) national_compare[[paste0("pp_diff__", sc)]] else NA_real_
  sub_pp <- if (paste0("pp_diff__", sc) %in% names(subgroup_compare)) subgroup_compare[[paste0("pp_diff__", sc)]] else NA_real_
  st_pp  <- if (paste0("pp_diff__", sc) %in% names(state_compare)) state_compare[[paste0("pp_diff__", sc)]] else NA_real_
  
  tibble(
    scheme = sc,
    n_flagship_national = sum(!is.na(nat_pp)),
    n_nat_pp_ge2  = sum(nat_pp >= 2, na.rm = TRUE),
    n_nat_pp_ge5  = sum(nat_pp >= 5, na.rm = TRUE),
    n_nat_pp_ge10 = sum(nat_pp >= 10, na.rm = TRUE),
    median_nat_pp = median(nat_pp, na.rm = TRUE),
    max_nat_pp    = max(nat_pp, na.rm = TRUE),
    
    n_subgroup_pp_ge5 = sum(sub_pp >= 5, na.rm = TRUE),
    n_subgroup_pp_ge7 = sum(sub_pp >= 7, na.rm = TRUE),
    median_subgroup_pp = median(sub_pp, na.rm = TRUE),
    max_subgroup_pp    = max(sub_pp, na.rm = TRUE),
    
    n_state_pp_ge5 = sum(st_pp >= 5, na.rm = TRUE),
    n_state_pp_ge10 = sum(st_pp >= 10, na.rm = TRUE),
    median_state_pp = median(st_pp, na.rm = TRUE),
    max_state_pp    = max(st_pp, na.rm = TRUE)
  )
})

# ------------------------------------------------------------------------------
# 16. Scorecard
# ------------------------------------------------------------------------------
scorecard <- scheme_summary %>%
  left_join(estimate_impact_summary, by = "scheme") %>%
  mutate(
    band_pct_eligible = case_when(
      is.na(pct_eligible) ~ "missing",
      pct_eligible >= 0.90 ~ "good",
      pct_eligible >= 0.80 ~ "acceptable",
      pct_eligible >= 0.60 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_ess_ratio = case_when(
      is.na(ESS_ratio_trim) ~ "missing",
      ESS_ratio_trim >= 0.70 ~ "good",
      ESS_ratio_trim >= 0.50 ~ "acceptable",
      ESS_ratio_trim >= 0.30 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_max_w = case_when(
      is.na(max_w_norm_trim) ~ "missing",
      max_w_norm_trim <= 2.5 ~ "good",
      max_w_norm_trim <= 4.0 ~ "acceptable",
      max_w_norm_trim <= 6.0 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_trim_hi = case_when(
      is.na(prop_trim_hi) ~ "missing",
      prop_trim_hi <= 0.05 ~ "good",
      prop_trim_hi <= 0.10 ~ "acceptable",
      prop_trim_hi <= 0.20 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_fit_improve = case_when(
      is.na(worst_mad_improve) ~ "missing",
      worst_mad_improve >= 0.30 ~ "good",
      worst_mad_improve >= 0.15 ~ "acceptable",
      worst_mad_improve >= 0.05 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_fit_residual = case_when(
      is.na(worst_maxabs_trim) ~ "missing",
      worst_maxabs_trim <= 0.01 ~ "good",
      worst_maxabs_trim <= 0.03 ~ "acceptable",
      worst_maxabs_trim <= 0.05 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_nat_impact = case_when(
      is.na(max_nat_pp) ~ "missing",
      max_nat_pp < 5 ~ "good",
      max_nat_pp < 10 ~ "acceptable",
      max_nat_pp < 15 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_subgroup_stability = case_when(
      is.na(max_subgroup_pp) ~ "missing",
      max_subgroup_pp < 7 ~ "good",
      max_subgroup_pp < 12 ~ "acceptable",
      max_subgroup_pp < 20 ~ "concerning",
      TRUE ~ "poor"
    ),
    
    band_state_stability = case_when(
      is.na(max_state_pp) ~ "missing",
      max_state_pp < 7 ~ "good",
      max_state_pp < 12 ~ "acceptable",
      max_state_pp < 20 ~ "concerning",
      TRUE ~ "poor"
    )
  ) %>%
  mutate(
    score_pct_eligible = band_score(band_pct_eligible),
    score_ess_ratio = band_score(band_ess_ratio),
    score_max_w = band_score(band_max_w),
    score_trim_hi = band_score(band_trim_hi),
    score_fit_improve = band_score(band_fit_improve),
    score_fit_residual = band_score(band_fit_residual),
    score_nat_impact = band_score(band_nat_impact),
    score_subgroup_stability = band_score(band_subgroup_stability),
    score_state_stability = band_score(band_state_stability)
  ) %>%
  mutate(
    score_total =
      0.15 * score_pct_eligible +
      0.15 * score_ess_ratio +
      0.10 * score_max_w +
      0.10 * score_trim_hi +
      0.15 * score_fit_improve +
      0.10 * score_fit_residual +
      0.10 * score_nat_impact +
      0.075 * score_subgroup_stability +
      0.075 * score_state_stability
  ) %>%
  left_join(SCHEME_ORDER, by = "scheme")

write_csv(scorecard, file.path(OUT_DIR, "WEIGHT_SCHEME_SCORECARD.csv"))

# ------------------------------------------------------------------------------
# 17. Final recommendation
# ------------------------------------------------------------------------------
recommendation_tbl <- scorecard %>%
  mutate(
    hard_fail = case_when(
      is.na(pct_eligible) ~ 1L,
      pct_eligible < HARD_MIN_PCT_ELIGIBLE ~ 1L,
      str_detect(coalesce(reason, ""), "Raking failed|No eligible|support missing|all NA") ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  filter(hard_fail == 0L) %>%
  arrange(desc(score_total), complexity_rank)

if (nrow(recommendation_tbl) == 0) {
  stop("No weighting scheme passed minimum hard-failure checks after full review.")
}

best_score <- recommendation_tbl$score_total[1]
top_set <- recommendation_tbl %>%
  filter(score_total >= best_score - 0.15) %>%
  arrange(complexity_rank, desc(score_total))

best_scheme <- top_set$scheme[1]

final_recommendation <- tibble(
  recommended_scheme = best_scheme,
  note = paste0(
    "Recommended scheme: ", best_scheme,
    ". Selection is based on benchmark fit, weight stability, and impact on flagship estimates, ",
    "with preference for the simplest scheme when competing schemes perform similarly."
  )
)

write_csv(final_recommendation, file.path(OUT_DIR, "WEIGHT_SCHEME_RECOMMENDATION.csv"))

message("Recommended scheme: ", best_scheme)

# ------------------------------------------------------------------------------
# 18. Attach final selected scheme weights
# ------------------------------------------------------------------------------
final_weights <- scheme_weights %>%
  filter(scheme == best_scheme) %>%
  select(row_id, w_raw, w_norm_untrim, w_norm_trim)

if (nrow(final_weights) == 0) {
  stop("No weights found for selected scheme: ", best_scheme)
}

df_final <- spx %>%
  left_join(final_weights, by = "row_id") %>%
  mutate(
    w_base = 1,
    w_calibrated_raw = w_raw,
    w_calibrated_trim = w_norm_trim,
    calib_ok = ifelse(!is.na(w_calibrated_trim) & is.finite(w_calibrated_trim) & w_calibrated_trim > 0, 1, 0)
  ) %>%
  select(-w_raw, -w_norm_untrim, -w_norm_trim)

# ------------------------------------------------------------------------------
# 19. Post-weighting wealth reconstruction
# ------------------------------------------------------------------------------
message("Rebuilding weighted wealth index...")

# 19a. Base fields
df_final <- df_final %>%
  mutate(
    row_id_wealth = row_number(),
    w_use = to_num(w_calibrated_trim),
    
    hh_size_num = to_num(hh_total_persons_usually_v3),
    sleep_rooms_num = to_num(demo_hh_sleeping_rooms),
    
    crowding_ratio = case_when(
      hh_size_num %in% c(98, 99) ~ NA_real_,
      sleep_rooms_num %in% c(98, 99) ~ NA_real_,
      is.na(hh_size_num) | is.na(sleep_rooms_num) ~ NA_real_,
      sleep_rooms_num <= 0 ~ NA_real_,
      TRUE ~ hh_size_num / sleep_rooms_num
    ),
    
    crowd_le2 = case_when(
      is.na(crowding_ratio) ~ NA_real_,
      crowding_ratio <= 2 ~ 1,
      crowding_ratio > 2 ~ 0
    ),
    crowd_gt2_le3 = case_when(
      is.na(crowding_ratio) ~ NA_real_,
      crowding_ratio > 2 & crowding_ratio <= 3 ~ 1,
      TRUE ~ 0
    ),
    crowd_gt3 = case_when(
      is.na(crowding_ratio) ~ NA_real_,
      crowding_ratio > 3 ~ 1,
      TRUE ~ 0
    )
  )

# 19b. Clean ownership assets
core_asset_vars <- c(
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac",
  "hh_has_electric_iron", "hh_has_generator", "hh_has_fan",
  "hh_own_watch", "hh_own_mobile_phone", "hh_own_bicycle", "hh_own_motorcycle",
  "hh_own_animal_cart", "hh_own_car_truck", "hh_own_motor_boat",
  "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account",
  "hh_mobile_money_usage"
)
core_asset_vars <- core_asset_vars[core_asset_vars %in% names(df_final)]

df_final <- df_final %>%
  mutate(
    across(all_of(core_asset_vars), ~ case_when(
      .x == 1 ~ 1,
      .x == 0 ~ 0,
      .x %in% c(8, 9, 98, 99) ~ NA_real_,
      is.na(.x) ~ NA_real_,
      TRUE ~ as.numeric(.x)
    ))
  )

# 19c. Disaggregated dummies
df_final <- df_final %>%
  mutate(
    water_11 = make_dummy(hh_drinking_water_source, 11, na_codes = c(96, 99)),
    water_12 = make_dummy(hh_drinking_water_source, 12, na_codes = c(96, 99)),
    water_13 = make_dummy(hh_drinking_water_source, 13, na_codes = c(96, 99)),
    water_21 = make_dummy(hh_drinking_water_source, 21, na_codes = c(96, 99)),
    water_31 = make_dummy(hh_drinking_water_source, 31, na_codes = c(96, 99)),
    water_32 = make_dummy(hh_drinking_water_source, 32, na_codes = c(96, 99)),
    water_41 = make_dummy(hh_drinking_water_source, 41, na_codes = c(96, 99)),
    water_42 = make_dummy(hh_drinking_water_source, 42, na_codes = c(96, 99)),
    water_51 = make_dummy(hh_drinking_water_source, 51, na_codes = c(96, 99)),
    water_61 = make_dummy(hh_drinking_water_source, 61, na_codes = c(96, 99)),
    water_71 = make_dummy(hh_drinking_water_source, 71, na_codes = c(96, 99)),
    water_81 = make_dummy(hh_drinking_water_source, 81, na_codes = c(96, 99)),
    water_91 = make_dummy(hh_drinking_water_source, 91, na_codes = c(96, 99)),
    water_92 = make_dummy(hh_drinking_water_source, 92, na_codes = c(96, 99)),
    
    toilet_11 = make_dummy(hh_toilet_type, 11, na_codes = c(96, 99)),
    toilet_12 = make_dummy(hh_toilet_type, 12, na_codes = c(96, 99)),
    toilet_13 = make_dummy(hh_toilet_type, 13, na_codes = c(96, 99)),
    toilet_14 = make_dummy(hh_toilet_type, 14, na_codes = c(96, 99)),
    toilet_15 = make_dummy(hh_toilet_type, 15, na_codes = c(96, 99)),
    toilet_21 = make_dummy(hh_toilet_type, 21, na_codes = c(96, 99)),
    toilet_22 = make_dummy(hh_toilet_type, 22, na_codes = c(96, 99)),
    toilet_23 = make_dummy(hh_toilet_type, 23, na_codes = c(96, 99)),
    toilet_31 = make_dummy(hh_toilet_type, 31, na_codes = c(96, 99)),
    toilet_41 = make_dummy(hh_toilet_type, 41, na_codes = c(96, 99)),
    toilet_51 = make_dummy(hh_toilet_type, 51, na_codes = c(96, 99)),
    toilet_61 = make_dummy(hh_toilet_type, 61, na_codes = c(96, 99)),
    
    toilet_shared_yes = case_when(
      hh_toilet_shared == 1 ~ 1,
      hh_toilet_shared == 0 ~ 0,
      hh_toilet_shared %in% c(8, 9, 98, 99) ~ NA_real_,
      is.na(hh_toilet_shared) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    floor_11 = make_dummy(hh_floor_material, 11, na_codes = c(96, 99)),
    floor_12 = make_dummy(hh_floor_material, 12, na_codes = c(96, 99)),
    floor_21 = make_dummy(hh_floor_material, 21, na_codes = c(96, 99)),
    floor_22 = make_dummy(hh_floor_material, 22, na_codes = c(96, 99)),
    floor_31 = make_dummy(hh_floor_material, 31, na_codes = c(96, 99)),
    floor_32 = make_dummy(hh_floor_material, 32, na_codes = c(96, 99)),
    floor_33 = make_dummy(hh_floor_material, 33, na_codes = c(96, 99)),
    floor_34 = make_dummy(hh_floor_material, 34, na_codes = c(96, 99)),
    floor_35 = make_dummy(hh_floor_material, 35, na_codes = c(96, 99)),
    
    roof_11 = make_dummy(hh_roof_material, 11, na_codes = c(96, 99)),
    roof_12 = make_dummy(hh_roof_material, 12, na_codes = c(96, 99)),
    roof_13 = make_dummy(hh_roof_material, 13, na_codes = c(96, 99)),
    roof_21 = make_dummy(hh_roof_material, 21, na_codes = c(96, 99)),
    roof_22 = make_dummy(hh_roof_material, 22, na_codes = c(96, 99)),
    roof_23 = make_dummy(hh_roof_material, 23, na_codes = c(96, 99)),
    roof_24 = make_dummy(hh_roof_material, 24, na_codes = c(96, 99)),
    roof_31 = make_dummy(hh_roof_material, 31, na_codes = c(96, 99)),
    roof_32 = make_dummy(hh_roof_material, 32, na_codes = c(96, 99)),
    roof_33 = make_dummy(hh_roof_material, 33, na_codes = c(96, 99)),
    roof_34 = make_dummy(hh_roof_material, 34, na_codes = c(96, 99)),
    roof_35 = make_dummy(hh_roof_material, 35, na_codes = c(96, 99)),
    roof_36 = make_dummy(hh_roof_material, 36, na_codes = c(96, 99)),
    roof_37 = make_dummy(hh_roof_material, 37, na_codes = c(96, 99)),
    
    wall_11 = make_dummy(hh_wall_material, 11, na_codes = c(96, 99)),
    wall_12 = make_dummy(hh_wall_material, 12, na_codes = c(96, 99)),
    wall_13 = make_dummy(hh_wall_material, 13, na_codes = c(96, 99)),
    wall_21 = make_dummy(hh_wall_material, 21, na_codes = c(96, 99)),
    wall_22 = make_dummy(hh_wall_material, 22, na_codes = c(96, 99)),
    wall_23 = make_dummy(hh_wall_material, 23, na_codes = c(96, 99)),
    wall_24 = make_dummy(hh_wall_material, 24, na_codes = c(96, 99)),
    wall_25 = make_dummy(hh_wall_material, 25, na_codes = c(96, 99)),
    wall_26 = make_dummy(hh_wall_material, 26, na_codes = c(96, 99)),
    wall_31 = make_dummy(hh_wall_material, 31, na_codes = c(96, 99)),
    wall_32 = make_dummy(hh_wall_material, 32, na_codes = c(96, 99)),
    wall_33 = make_dummy(hh_wall_material, 33, na_codes = c(96, 99)),
    wall_34 = make_dummy(hh_wall_material, 34, na_codes = c(96, 99)),
    wall_35 = make_dummy(hh_wall_material, 35, na_codes = c(96, 99)),
    wall_36 = make_dummy(hh_wall_material, 36, na_codes = c(96, 99)),
    
    stove_1 = make_dummy(hh_cookstove_type, 1, na_codes = c(95, 96, 99)),
    stove_2 = make_dummy(hh_cookstove_type, 2, na_codes = c(95, 96, 99)),
    stove_3 = make_dummy(hh_cookstove_type, 3, na_codes = c(95, 96, 99)),
    stove_4 = make_dummy(hh_cookstove_type, 4, na_codes = c(95, 96, 99)),
    stove_5 = make_dummy(hh_cookstove_type, 5, na_codes = c(95, 96, 99)),
    stove_6 = make_dummy(hh_cookstove_type, 6, na_codes = c(95, 96, 99)),
    stove_7 = make_dummy(hh_cookstove_type, 7, na_codes = c(95, 96, 99)),
    stove_8 = make_dummy(hh_cookstove_type, 8, na_codes = c(95, 96, 99)),
    stove_9 = make_dummy(hh_cookstove_type, 9, na_codes = c(95, 96, 99)),
    
    fuel_1  = make_dummy(hh_cookstove_fuel, 1, na_codes = c(96, 99)),
    fuel_2  = make_dummy(hh_cookstove_fuel, 2, na_codes = c(96, 99)),
    fuel_3  = make_dummy(hh_cookstove_fuel, 3, na_codes = c(96, 99)),
    fuel_4  = make_dummy(hh_cookstove_fuel, 4, na_codes = c(96, 99)),
    fuel_5  = make_dummy(hh_cookstove_fuel, 5, na_codes = c(96, 99)),
    fuel_6  = make_dummy(hh_cookstove_fuel, 6, na_codes = c(96, 99)),
    fuel_7  = make_dummy(hh_cookstove_fuel, 7, na_codes = c(96, 99)),
    fuel_8  = make_dummy(hh_cookstove_fuel, 8, na_codes = c(96, 99)),
    fuel_9  = make_dummy(hh_cookstove_fuel, 9, na_codes = c(96, 99)),
    fuel_10 = make_dummy(hh_cookstove_fuel, 10, na_codes = c(96, 99)),
    fuel_11 = make_dummy(hh_cookstove_fuel, 11, na_codes = c(96, 99)),
    fuel_12 = make_dummy(hh_cookstove_fuel, 12, na_codes = c(96, 99))
  )

# 19d. Rural asset groupings
df_final <- df_final %>%
  mutate(
    agri_land_yes = case_when(
      hh_owns_agri_land == 1 ~ 1,
      hh_owns_agri_land == 0 ~ 0,
      hh_owns_agri_land %in% c(8, 9, 98, 99) ~ NA_real_,
      is.na(hh_owns_agri_land) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    agri_plots_num = to_num(hh_num_agri_plots),
    agri_plots_1_4 = case_when(
      agri_plots_num %in% 1:4 ~ 1,
      agri_plots_num == 0 ~ 0,
      agri_plots_num %in% c(95, 98, 99) ~ NA_real_,
      agri_plots_num >= 5 & agri_plots_num < 95 ~ 0,
      TRUE ~ NA_real_
    ),
    agri_plots_5p = case_when(
      agri_plots_num >= 5 & agri_plots_num < 95 ~ 1,
      agri_plots_num %in% 0:4 ~ 0,
      agri_plots_num %in% c(95, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    livestock_yes = case_when(
      hh_owns_livestock == 1 ~ 1,
      hh_owns_livestock == 0 ~ 0,
      hh_owns_livestock %in% c(8, 9, 98, 99) ~ NA_real_,
      is.na(hh_owns_livestock) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    cows_num = to_num(hh_num_cows_bulls),
    cows_1_4 = case_when(
      cows_num %in% 1:4 ~ 1,
      cows_num == 0 ~ 0,
      cows_num %in% c(95, 98, 99) ~ NA_real_,
      cows_num >= 5 & cows_num < 95 ~ 0,
      TRUE ~ NA_real_
    ),
    cows_5p = case_when(
      cows_num >= 5 & cows_num < 95 ~ 1,
      cows_num %in% 0:4 ~ 0,
      cows_num %in% c(95, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    goats_num = to_num(hh_num_goats),
    goats_1_4 = case_when(
      goats_num %in% 1:4 ~ 1,
      goats_num == 0 ~ 0,
      goats_num %in% c(95, 98, 99) ~ NA_real_,
      goats_num >= 5 & goats_num < 95 ~ 0,
      TRUE ~ NA_real_
    ),
    goats_5p = case_when(
      goats_num >= 5 & goats_num < 95 ~ 1,
      goats_num %in% 0:4 ~ 0,
      goats_num %in% c(95, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    poultry_num = to_num(hh_num_poultry),
    poultry_1_9 = case_when(
      poultry_num %in% 1:9 ~ 1,
      poultry_num == 0 ~ 0,
      poultry_num %in% c(95, 98, 99) ~ NA_real_,
      poultry_num >= 10 & poultry_num < 95 ~ 0,
      TRUE ~ NA_real_
    ),
    poultry_10p = case_when(
      poultry_num >= 10 & poultry_num < 95 ~ 1,
      poultry_num %in% 0:9 ~ 0,
      poultry_num %in% c(95, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# 19e. Candidate wealth variable list and screening
wealth_vars <- c(
  core_asset_vars,
  "water_11","water_12","water_13","water_21","water_31","water_32","water_41","water_42","water_51","water_61","water_71","water_81","water_91","water_92",
  "toilet_11","toilet_12","toilet_13","toilet_14","toilet_15","toilet_21","toilet_22","toilet_23","toilet_31","toilet_41","toilet_51","toilet_61","toilet_shared_yes",
  "floor_11","floor_12","floor_21","floor_22","floor_31","floor_32","floor_33","floor_34","floor_35",
  "roof_11","roof_12","roof_13","roof_21","roof_22","roof_23","roof_24","roof_31","roof_32","roof_33","roof_34","roof_35","roof_36","roof_37",
  "wall_11","wall_12","wall_13","wall_21","wall_22","wall_23","wall_24","wall_25","wall_26","wall_31","wall_32","wall_33","wall_34","wall_35","wall_36",
  "stove_1","stove_2","stove_3","stove_4","stove_5","stove_6","stove_7","stove_8","stove_9",
  "fuel_1","fuel_2","fuel_3","fuel_4","fuel_5","fuel_6","fuel_7","fuel_8","fuel_9","fuel_10","fuel_11","fuel_12",
  "agri_land_yes","agri_plots_1_4","agri_plots_5p",
  "livestock_yes","cows_1_4","cows_5p","goats_1_4","goats_5p","poultry_1_9","poultry_10p",
  "crowd_le2","crowd_gt2_le3","crowd_gt3"
)
wealth_vars <- wealth_vars[wealth_vars %in% names(df_final)]

wealth_screen <- lapply(wealth_vars, function(v) {
  x <- df_final[[v]]
  n_nonmiss <- sum(!is.na(x) & is.finite(x))
  prev <- weighted_prev_safe(x, df_final$w_use)
  distinct_nonmiss <- length(unique(x[!is.na(x) & is.finite(x)]))
  
  tibble(
    variable = v,
    n_nonmiss = n_nonmiss,
    weighted_mean = prev,
    distinct_nonmiss = distinct_nonmiss,
    keep = ifelse(
      n_nonmiss >= MIN_NONMISS_N_WEALTH &
        distinct_nonmiss > 1 &
        (is.na(prev) | (prev > MIN_PREV_WEALTH & prev < MAX_PREV_WEALTH)),
      1, 0
    )
  )
}) %>% bind_rows()

wealth_vars_keep <- wealth_screen %>%
  filter(keep == 1) %>%
  pull(variable)

if (length(wealth_vars_keep) < 2) {
  stop("Too few wealth variables retained after screening.")
}

# 19f. PCA matrix and imputation
wealth_pca_df <- df_final %>%
  select(all_of(wealth_vars_keep)) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

wealth_pca_imp <- wealth_pca_df
for (j in seq_along(wealth_pca_imp)) {
  mu_j <- weighted_mean_safe(wealth_pca_imp[[j]], df_final$w_use)
  wealth_pca_imp[[j]][is.na(wealth_pca_imp[[j]])] <- mu_j
}

wealth_complete_case <- wealth_pca_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), 1, 0))) %>%
  rowSums()

df_final$pca_complete_case_weighted <- ifelse(wealth_complete_case == 0, 1, 0)

# 19g. Run PCA
pca_res <- FactoMineR::PCA(
  wealth_pca_imp,
  graph = FALSE,
  ncp = 1
)

pc1_scores <- pca_res$ind$coord[, 1]
pc1_loadings <- pca_res$var$coord[, 1]

anchor_vars <- intersect(
  c("hh_has_electricity", "hh_has_refrigerator", "hh_has_computer", "hh_has_bank_account", "hh_own_car_truck", "hh_has_ac"),
  names(wealth_pca_imp)
)

if (length(anchor_vars) > 0) {
  anchor_mean <- mean(pc1_loadings[anchor_vars], na.rm = TRUE)
  if (is.finite(anchor_mean) && anchor_mean < 0) {
    pc1_scores <- -1 * pc1_scores
    pc1_loadings <- -1 * pc1_loadings
  }
}

df_final <- df_final %>%
  mutate(
    derived_wealth_score = pc1_scores,
    derived_wealth_quintile = weighted_quintile(derived_wealth_score, w_use),
    wealth_q = derived_wealth_quintile
  )

# ------------------------------------------------------------------------------
# 20. Wealth QC
# ------------------------------------------------------------------------------
wealth_qc_overall <- tibble(
  metric = c(
    "recommended_scheme",
    "N_total",
    "N_nonmissing_weight",
    "N_nonmissing_wealth_score",
    "N_nonmissing_wealth_quintile",
    "Pct_nonmissing_wealth_quintile",
    "N_complete_case_preimputation",
    "Pct_complete_case_preimputation",
    "N_pca_vars_retained"
  ),
  value = c(
    as.character(best_scheme),
    as.character(nrow(df_final)),
    as.character(sum(is.finite(df_final$w_use) & !is.na(df_final$w_use) & df_final$w_use > 0)),
    as.character(sum(is.finite(df_final$derived_wealth_score) & !is.na(df_final$derived_wealth_score))),
    as.character(sum(!is.na(df_final$derived_wealth_quintile))),
    as.character(round(100 * mean(!is.na(df_final$derived_wealth_quintile)), 1)),
    as.character(sum(df_final$pca_complete_case_weighted == 1, na.rm = TRUE)),
    as.character(round(100 * mean(df_final$pca_complete_case_weighted == 1, na.rm = TRUE), 1)),
    as.character(length(wealth_vars_keep))
  )
)

wealth_top_loadings <- tibble(
  variable = names(pc1_loadings),
  loading = as.numeric(pc1_loadings)
) %>%
  arrange(desc(loading))

wealth_bottom_loadings <- tibble(
  variable = names(pc1_loadings),
  loading = as.numeric(pc1_loadings)
) %>%
  arrange(loading)

wealth_quintile_distribution <- df_final %>%
  filter(!is.na(derived_wealth_quintile), is.finite(w_use), !is.na(w_use), w_use > 0) %>%
  group_by(derived_wealth_quintile) %>%
  summarise(
    unwtd_n = n(),
    weighted_n = sum(w_use),
    weighted_pct = round(100 * weighted_n / sum(weighted_n), 2),
    .groups = "drop"
  )

wealth_qc_out <- bind_rows(
  wealth_qc_overall %>%
    mutate(section = "overall") %>%
    select(section, metric, value),
  
  tibble(
    section = "top_positive_loadings",
    metric = wealth_top_loadings$variable[1:min(15, nrow(wealth_top_loadings))],
    value = as.character(round(
      wealth_top_loadings$loading[1:min(15, nrow(wealth_top_loadings))], 4
    ))
  ),
  
  tibble(
    section = "top_negative_loadings",
    metric = wealth_bottom_loadings$variable[1:min(15, nrow(wealth_bottom_loadings))],
    value = as.character(round(
      wealth_bottom_loadings$loading[1:min(15, nrow(wealth_bottom_loadings))], 4
    ))
  )
)

write_csv(wealth_qc_out, WEALTH_QC_OUTPUT)

# ------------------------------------------------------------------------------
# 21. Update labels for final export
# ------------------------------------------------------------------------------
extra_var_labels <- c(
  "w_base" = "Base weight for calibration (set to 1 for all eligible records)",
  "w_calibrated_raw" = "Raw calibrated post-stratification weight before trimming",
  "w_calibrated_trim" = "Trimmed and rescaled calibrated post-stratification weight",
  "calib_ok" = "Eligible for calibration weighting based on complete benchmark variables",
  "derived_wealth_score" = "Weighted survey-specific living standards score (PCA first component)",
  "derived_wealth_quintile" = "Weighted survey-specific wealth quintile (1=Lowest ... 5=Highest)",
  "wealth_q" = "Weighted survey-specific wealth quintile (numeric copy)",
  "pca_complete_case_weighted" = "Complete-case flag for revised weighted wealth PCA before imputation",
  "crowding_ratio" = "Household members per sleeping room",
  "crowd_le2" = "Crowding category: <=2 members per sleeping room",
  "crowd_gt2_le3" = "Crowding category: >2 to <=3 members per sleeping room",
  "crowd_gt3" = "Crowding category: >3 members per sleeping room"
)

generated_dummy_vars <- c(
  names(df_final)[grepl("^water_", names(df_final))],
  names(df_final)[grepl("^toilet_", names(df_final))],
  names(df_final)[grepl("^floor_", names(df_final))],
  names(df_final)[grepl("^roof_", names(df_final))],
  names(df_final)[grepl("^wall_", names(df_final))],
  names(df_final)[grepl("^stove_", names(df_final))],
  names(df_final)[grepl("^fuel_", names(df_final))],
  "agri_land_yes","agri_plots_1_4","agri_plots_5p",
  "livestock_yes","cows_1_4","cows_5p","goats_1_4","goats_5p","poultry_1_9","poultry_10p"
)
generated_dummy_vars <- generated_dummy_vars[generated_dummy_vars %in% names(df_final)]

generated_dummy_labels <- setNames(
  paste0("Wealth PCA input: ", generated_dummy_vars),
  generated_dummy_vars
)

final_var_labels <- c(var_labels, extra_var_labels, generated_dummy_labels)
final_var_labels <- final_var_labels[!duplicated(names(final_var_labels), fromLast = TRUE)]
final_var_labels <- final_var_labels[names(final_var_labels) %in% names(df_final)]

df_final_sav <- df_final %>%
  labelled::set_variable_labels(.labels = as.list(final_var_labels))

if ("derived_wealth_quintile" %in% names(df_final_sav)) {
  val_labels(df_final_sav$derived_wealth_quintile) <- val_labels_meta$wealth_quintile
}
if ("wealth_q" %in% names(df_final_sav)) {
  val_labels(df_final_sav$wealth_q) <- val_labels_meta$wealth_quintile
}
if ("pca_complete_case_weighted" %in% names(df_final_sav)) {
  val_labels(df_final_sav$pca_complete_case_weighted) <- val_labels_meta$yesno_simple
}
for (v in c("crowd_le2", "crowd_gt2_le3", "crowd_gt3")) {
  if (v %in% names(df_final_sav)) {
    val_labels(df_final_sav[[v]]) <- val_labels_meta$yesno_simple
  }
}

# ------------------------------------------------------------------------------
# 22. Export final outputs
# ------------------------------------------------------------------------------
saveRDS(df_final, FINAL_OUTPUT_RDS)
write.csv(df_final, FINAL_OUTPUT_CSV, row.names = FALSE)
write_sav(df_final_sav, FINAL_OUTPUT_SAV)

message("✅ Integrated weighting feasibility, impact review, finalisation, and wealth reconstruction complete.")
message("Recommended scheme: ", best_scheme)
message("Final outputs:")
message(" - ", FINAL_OUTPUT_RDS)
message(" - ", FINAL_OUTPUT_CSV)
message(" - ", FINAL_OUTPUT_SAV)
message(" - ", WEALTH_QC_OUTPUT)
message("Diagnostics in: ", OUT_DIR)