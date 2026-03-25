# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Post-Weighting Wealth Index Reconstruction
# AUTHOR:  Ikechukwu Onuko / OpenAI revision
# DATE:    18 March 2026
#
# PURPOSE:
#   Rebuild the survey-specific wealth index AFTER final calibration weights
#   have been attached, using a richer asset structure and weighted quintiles.
#
#   This script:
#   1. loads the weighted analysis-ready dataset,
#   2. creates disaggregated wealth-input variables,
#   3. screens low-information variables,
#   4. imputes missing PCA inputs using weighted means,
#   5. computes PCA-based living-standards score,
#   6. aligns score direction so higher = better living standards,
#   7. creates WEIGHTED wealth quintiles using w_calibrated_trim,
#   8. updates labels for SAV export,
#   9. writes QC outputs and final datasets.
#
# INPUT:
#   - Sproxil_Analysis_Ready_Weighted.rds
#   - Sproxil_dictionary.rds
#
# OUTPUT:
#   - Sproxil_Analysis_Ready_Weighted_Wealth.rds
#   - Sproxil_Analysis_Ready_Weighted_Wealth.sav
#   - Sproxil_Analysis_Ready_Weighted_Wealth.csv
#   - Sproxil_Weighted_Wealth_QC.csv
#
# INTERPRETATION NOTE:
#   The resulting wealth quintile is a weighted sample-derived living-standards
#   stratifier for the Sproxil survey. It is not a DHS/MIS-equivalent national
#   wealth quintile and should be interpreted only within the survey context.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(haven)
  library(labelled)
  library(FactoMineR)
})

# ------------------------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------------------------
INPUT_RDS  <- "Sproxil_Analysis_Ready_Weighted.rds"
INPUT_DICT <- "Sproxil_dictionary.rds"

OUTPUT_RDS <- "Sproxil_Analysis_Ready_Weighted.rds"
OUTPUT_SAV <- "Sproxil_Analysis_Ready_Weighted.sav"
OUTPUT_CSV <- "Sproxil_Analysis_Ready_Weighted.csv"
OUTPUT_QC  <- "Sproxil_Weighted_Wealth_QC.csv"

WEIGHT_VAR <- "w_calibrated_trim"

# Variable screening thresholds
MIN_NONMISS_N <- 30        # minimum non-missing observations
MIN_PREV      <- 0.01      # minimum weighted prevalence for binary variables
MAX_PREV      <- 0.99      # maximum weighted prevalence for binary variables

# ------------------------------------------------------------------------------
# 2. Checks and load
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_RDS)) stop("Missing dataset: ", INPUT_RDS)
if (!file.exists(INPUT_DICT)) stop("Missing metadata dictionary: ", INPUT_DICT)

df <- readRDS(INPUT_RDS)
meta <- readRDS(INPUT_DICT)

var_labels <- meta$variable_labels
val_labels_meta <- meta$value_labels

if (!WEIGHT_VAR %in% names(df)) {
  stop("Weight variable not found: ", WEIGHT_VAR,
       ". Run weight finalisation and attachment first.")
}

message("Loaded weighted analysis-ready dataset: ", nrow(df), " rows, ", ncol(df), " columns.")

# ------------------------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------------------------
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
# 4. Standardise base fields used in wealth construction
# ------------------------------------------------------------------------------
message("Step 1: Standardising base fields used in wealth construction...")

df <- df %>%
  mutate(
    row_id_wealth = row_number(),
    w_use = to_num(.data[[WEIGHT_VAR]]),
    
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

# ------------------------------------------------------------------------------
# 5. Recode ownership assets to clean binary 0/1/NA
# ------------------------------------------------------------------------------
message("Step 2: Cleaning ownership asset variables...")

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

core_asset_vars <- core_asset_vars[core_asset_vars %in% names(df)]

df <- df %>%
  mutate(
    across(all_of(core_asset_vars), ~ case_when(
      .x == 1 ~ 1,
      .x == 0 ~ 0,
      .x %in% c(8, 9, 98, 99) ~ NA_real_,
      is.na(.x) ~ NA_real_,
      TRUE ~ as.numeric(.x)
    ))
  )

# ------------------------------------------------------------------------------
# 6. Create disaggregated housing, WASH, cooking, and rural asset dummies
# ------------------------------------------------------------------------------
message("Step 3: Creating disaggregated housing, WASH, cooking, and rural-asset dummies...")

df <- df %>%
  mutate(
    # Water source
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
    
    # Toilet type
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
    
    # Floor material
    floor_11 = make_dummy(hh_floor_material, 11, na_codes = c(96, 99)),
    floor_12 = make_dummy(hh_floor_material, 12, na_codes = c(96, 99)),
    floor_21 = make_dummy(hh_floor_material, 21, na_codes = c(96, 99)),
    floor_22 = make_dummy(hh_floor_material, 22, na_codes = c(96, 99)),
    floor_31 = make_dummy(hh_floor_material, 31, na_codes = c(96, 99)),
    floor_32 = make_dummy(hh_floor_material, 32, na_codes = c(96, 99)),
    floor_33 = make_dummy(hh_floor_material, 33, na_codes = c(96, 99)),
    floor_34 = make_dummy(hh_floor_material, 34, na_codes = c(96, 99)),
    floor_35 = make_dummy(hh_floor_material, 35, na_codes = c(96, 99)),
    
    # Roof material
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
    
    # Wall material
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
    
    # Cookstove type
    stove_1 = make_dummy(hh_cookstove_type, 1, na_codes = c(95, 96, 99)),
    stove_2 = make_dummy(hh_cookstove_type, 2, na_codes = c(95, 96, 99)),
    stove_3 = make_dummy(hh_cookstove_type, 3, na_codes = c(95, 96, 99)),
    stove_4 = make_dummy(hh_cookstove_type, 4, na_codes = c(95, 96, 99)),
    stove_5 = make_dummy(hh_cookstove_type, 5, na_codes = c(95, 96, 99)),
    stove_6 = make_dummy(hh_cookstove_type, 6, na_codes = c(95, 96, 99)),
    stove_7 = make_dummy(hh_cookstove_type, 7, na_codes = c(95, 96, 99)),
    stove_8 = make_dummy(hh_cookstove_type, 8, na_codes = c(95, 96, 99)),
    stove_9 = make_dummy(hh_cookstove_type, 9, na_codes = c(95, 96, 99)),
    
    # Cooking fuel
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

# ------------------------------------------------------------------------------
# 7. Group rural asset variables
# ------------------------------------------------------------------------------
message("Step 4: Creating grouped land and livestock variables...")

df <- df %>%
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

# ------------------------------------------------------------------------------
# 8. Assemble candidate wealth variable list
# ------------------------------------------------------------------------------
message("Step 5: Assembling and screening candidate wealth variables...")

wealth_vars <- c(
  core_asset_vars,
  
  # Water
  "water_11","water_12","water_13","water_21","water_31","water_32","water_41",
  "water_42","water_51","water_61","water_71","water_81","water_91","water_92",
  
  # Toilet
  "toilet_11","toilet_12","toilet_13","toilet_14","toilet_15","toilet_21",
  "toilet_22","toilet_23","toilet_31","toilet_41","toilet_51","toilet_61",
  "toilet_shared_yes",
  
  # Floor
  "floor_11","floor_12","floor_21","floor_22","floor_31","floor_32","floor_33","floor_34","floor_35",
  
  # Roof
  "roof_11","roof_12","roof_13","roof_21","roof_22","roof_23","roof_24","roof_31",
  "roof_32","roof_33","roof_34","roof_35","roof_36","roof_37",
  
  # Wall
  "wall_11","wall_12","wall_13","wall_21","wall_22","wall_23","wall_24","wall_25",
  "wall_26","wall_31","wall_32","wall_33","wall_34","wall_35","wall_36",
  
  # Stove / fuel
  "stove_1","stove_2","stove_3","stove_4","stove_5","stove_6","stove_7","stove_8","stove_9",
  "fuel_1","fuel_2","fuel_3","fuel_4","fuel_5","fuel_6","fuel_7","fuel_8","fuel_9","fuel_10","fuel_11","fuel_12",
  
  # Rural asset markers
  "agri_land_yes","agri_plots_1_4","agri_plots_5p",
  "livestock_yes","cows_1_4","cows_5p","goats_1_4","goats_5p","poultry_1_9","poultry_10p",
  
  # Crowding
  "crowd_le2","crowd_gt2_le3","crowd_gt3"
)

wealth_vars <- wealth_vars[wealth_vars %in% names(df)]

# Screen variables with too few non-missing observations or no useful variation
wealth_screen <- lapply(wealth_vars, function(v) {
  x <- df[[v]]
  n_nonmiss <- sum(!is.na(x) & is.finite(x))
  prev <- weighted_prev_safe(x, df$w_use)
  distinct_nonmiss <- length(unique(x[!is.na(x) & is.finite(x)]))
  
  tibble(
    variable = v,
    n_nonmiss = n_nonmiss,
    weighted_mean = prev,
    distinct_nonmiss = distinct_nonmiss,
    keep = ifelse(
      n_nonmiss >= MIN_NONMISS_N &
        distinct_nonmiss > 1 &
        (is.na(prev) | (prev > MIN_PREV & prev < MAX_PREV)),
      1, 0
    )
  )
}) %>% bind_rows()

wealth_vars_keep <- wealth_screen %>%
  filter(keep == 1) %>%
  pull(variable)

message("Candidate wealth variables before screening: ", length(wealth_vars))
message("Candidate wealth variables retained: ", length(wealth_vars_keep))

if (length(wealth_vars_keep) < 2) {
  stop("Too few wealth variables retained after screening. Review data and thresholds.")
}

# ------------------------------------------------------------------------------
# 9. Prepare PCA input and impute missing values
# ------------------------------------------------------------------------------
message("Step 6: Preparing PCA matrix and imputing missing values...")

wealth_pca_df <- df %>%
  select(all_of(wealth_vars_keep)) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

wealth_pca_imp <- wealth_pca_df

for (j in seq_along(wealth_pca_imp)) {
  mu_j <- weighted_mean_safe(wealth_pca_imp[[j]], df$w_use)
  wealth_pca_imp[[j]][is.na(wealth_pca_imp[[j]])] <- mu_j
}

# Keep track of whether each row had complete observed input before imputation
wealth_complete_case <- wealth_pca_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), 1, 0))) %>%
  rowSums()

df$pca_complete_case_weighted <- ifelse(wealth_complete_case == 0, 1, 0)

# ------------------------------------------------------------------------------
# 10. Run PCA
# ------------------------------------------------------------------------------
message("Step 7: Running PCA...")

if (nrow(wealth_pca_imp) < 30 || ncol(wealth_pca_imp) < 2) {
  stop("Insufficient rows or variables to run PCA.")
}

pca_res <- FactoMineR::PCA(
  wealth_pca_imp,
  graph = FALSE,
  ncp = 1
)

pc1_scores <- pca_res$ind$coord[, 1]
pc1_loadings <- pca_res$var$coord[, 1]

# ------------------------------------------------------------------------------
# 11. Orient score so higher = better living standards
# ------------------------------------------------------------------------------
message("Step 8: Orienting PCA score...")

anchor_vars <- intersect(
  c("hh_has_electricity", "hh_has_refrigerator", "hh_has_computer",
    "hh_has_bank_account", "hh_own_car_truck", "hh_has_ac"),
  names(wealth_pca_imp)
)

if (length(anchor_vars) > 0) {
  anchor_mean <- mean(pc1_loadings[anchor_vars], na.rm = TRUE)
  if (is.finite(anchor_mean) && anchor_mean < 0) {
    pc1_scores <- -1 * pc1_scores
    pc1_loadings <- -1 * pc1_loadings
    message("PCA score direction flipped: higher score now indicates higher living standards.")
  }
}

df <- df %>%
  mutate(
    derived_wealth_score = pc1_scores
  )

# ------------------------------------------------------------------------------
# 12. Create weighted wealth quintiles
# ------------------------------------------------------------------------------
message("Step 9: Creating weighted wealth quintiles...")

df <- df %>%
  mutate(
    derived_wealth_quintile = weighted_quintile(derived_wealth_score, w_use),
    wealth_q = derived_wealth_quintile
  )

# ------------------------------------------------------------------------------
# 13. QC summaries
# ------------------------------------------------------------------------------
message("Step 10: Generating wealth QC outputs...")

wealth_qc_overall <- tibble(
  metric = c(
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
    nrow(df),
    sum(is.finite(df$w_use) & !is.na(df$w_use) & df$w_use > 0),
    sum(is.finite(df$derived_wealth_score) & !is.na(df$derived_wealth_score)),
    sum(!is.na(df$derived_wealth_quintile)),
    round(100 * mean(!is.na(df$derived_wealth_quintile)), 1),
    sum(df$pca_complete_case_weighted == 1, na.rm = TRUE),
    round(100 * mean(df$pca_complete_case_weighted == 1, na.rm = TRUE), 1),
    length(wealth_vars_keep)
  )
)

wealth_qc_residence <- NULL
if ("derived_residence" %in% names(df)) {
  wealth_qc_residence <- df %>%
    mutate(
      residence_lab = case_when(
        derived_residence == 1 ~ "Urban",
        derived_residence == 2 ~ "Rural",
        TRUE ~ "Missing"
      )
    ) %>%
    group_by(residence_lab) %>%
    summarise(
      N_total = n(),
      N_nonmissing_wealth = sum(!is.na(derived_wealth_quintile)),
      pct_nonmissing_wealth = round(100 * N_nonmissing_wealth / N_total, 1),
      weighted_share = round(100 * sum(w_use, na.rm = TRUE) / sum(df$w_use, na.rm = TRUE), 1),
      .groups = "drop"
    )
}

wealth_qc_zone <- NULL
if ("derived_zone" %in% names(df)) {
  wealth_qc_zone <- df %>%
    mutate(
      zone_lab = case_when(
        derived_zone == 1 ~ "North Central",
        derived_zone == 2 ~ "North East",
        derived_zone == 3 ~ "North West",
        derived_zone == 4 ~ "South East",
        derived_zone == 5 ~ "South South",
        derived_zone == 6 ~ "South West",
        TRUE ~ "Missing"
      )
    ) %>%
    group_by(zone_lab) %>%
    summarise(
      N_total = n(),
      N_nonmissing_wealth = sum(!is.na(derived_wealth_quintile)),
      pct_nonmissing_wealth = round(100 * N_nonmissing_wealth / N_total, 1),
      weighted_share = round(100 * sum(w_use, na.rm = TRUE) / sum(df$w_use, na.rm = TRUE), 1),
      .groups = "drop"
    )
}

wealth_quintile_distribution <- df %>%
  filter(!is.na(derived_wealth_quintile), is.finite(w_use), !is.na(w_use), w_use > 0) %>%
  group_by(derived_wealth_quintile) %>%
  summarise(
    unwtd_n = n(),
    weighted_n = sum(w_use),
    weighted_pct = round(100 * weighted_n / sum(weighted_n), 2),
    .groups = "drop"
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

qc_out <- bind_rows(
  wealth_qc_overall %>% mutate(section = "overall") %>% select(section, metric, value),
  tibble(
    section = "top_positive_loadings",
    metric = wealth_top_loadings$variable[1:min(15, nrow(wealth_top_loadings))],
    value = round(wealth_top_loadings$loading[1:min(15, nrow(wealth_top_loadings))], 4)
  ),
  tibble(
    section = "top_negative_loadings",
    metric = wealth_bottom_loadings$variable[1:min(15, nrow(wealth_bottom_loadings))],
    value = round(wealth_bottom_loadings$loading[1:min(15, nrow(wealth_bottom_loadings))], 4)
  )
)

write_csv(qc_out, OUTPUT_QC)

# Optional console output
print(wealth_qc_overall)
if (!is.null(wealth_qc_residence)) print(wealth_qc_residence)
if (!is.null(wealth_qc_zone)) print(wealth_qc_zone)
print(wealth_quintile_distribution)

# ------------------------------------------------------------------------------
# 14. Update variable labels for new/changed fields
# ------------------------------------------------------------------------------
message("Step 11: Updating labels for export...")

extra_var_labels <- c(
  "derived_wealth_score" = "Weighted survey-specific living standards score (PCA first component)",
  "derived_wealth_quintile" = "Weighted survey-specific wealth quintile (1=Lowest ... 5=Highest)",
  "wealth_q" = "Weighted survey-specific wealth quintile (numeric copy)",
  "pca_complete_case_weighted" = "Complete-case flag for revised weighted wealth PCA before imputation",
  "crowding_ratio" = "Household members per sleeping room",
  "crowd_le2" = "Crowding category: <=2 members per sleeping room",
  "crowd_gt2_le3" = "Crowding category: >2 to <=3 members per sleeping room",
  "crowd_gt3" = "Crowding category: >3 members per sleeping room"
)

# Add labels for generated dummies if not present
generated_dummy_vars <- c(
  names(df)[grepl("^water_", names(df))],
  names(df)[grepl("^toilet_", names(df))],
  names(df)[grepl("^floor_", names(df))],
  names(df)[grepl("^roof_", names(df))],
  names(df)[grepl("^wall_", names(df))],
  names(df)[grepl("^stove_", names(df))],
  names(df)[grepl("^fuel_", names(df))],
  "agri_land_yes","agri_plots_1_4","agri_plots_5p",
  "livestock_yes","cows_1_4","cows_5p","goats_1_4","goats_5p","poultry_1_9","poultry_10p"
)

generated_dummy_vars <- generated_dummy_vars[generated_dummy_vars %in% names(df)]
generated_dummy_labels <- setNames(
  paste0("Wealth PCA input: ", generated_dummy_vars),
  generated_dummy_vars
)

final_var_labels <- c(var_labels, extra_var_labels, generated_dummy_labels)
final_var_labels <- final_var_labels[!duplicated(names(final_var_labels), fromLast = TRUE)]
final_var_labels <- final_var_labels[names(final_var_labels) %in% names(df)]

# ------------------------------------------------------------------------------
# 15. Prepare SAV export with labels
# ------------------------------------------------------------------------------
message("Step 12: Preparing labelled SAV export...")

df_sav <- df %>%
  labelled::set_variable_labels(.labels = as.list(final_var_labels))

# Apply value labels
if ("derived_wealth_quintile" %in% names(df_sav)) {
  val_labels(df_sav$derived_wealth_quintile) <- val_labels_meta$wealth_quintile
}
if ("wealth_q" %in% names(df_sav)) {
  val_labels(df_sav$wealth_q) <- val_labels_meta$wealth_quintile
}
if ("pca_complete_case_weighted" %in% names(df_sav)) {
  val_labels(df_sav$pca_complete_case_weighted) <- val_labels_meta$yesno_simple
}
for (v in c("crowd_le2", "crowd_gt2_le3", "crowd_gt3")) {
  if (v %in% names(df_sav)) {
    val_labels(df_sav[[v]]) <- val_labels_meta$yesno_simple
  }
}

# ------------------------------------------------------------------------------
# 16. Export outputs
# ------------------------------------------------------------------------------
message("Step 13: Writing outputs...")

saveRDS(df, OUTPUT_RDS)
write.csv(df, OUTPUT_CSV, row.names = FALSE)
write_sav(df_sav, OUTPUT_SAV)

message("✅ Post-weighting wealth reconstruction complete.")
message("Saved files:")
message(" - ", OUTPUT_RDS)
message(" - ", OUTPUT_SAV)
message(" - ", OUTPUT_CSV)
message(" - ", OUTPUT_QC)