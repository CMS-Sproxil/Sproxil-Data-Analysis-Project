# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  05_Derived_Variables_From_Scratch.R
# PURPOSE: Build an analysis-ready dataset for MIS-style tabulations
#
# INPUT:  Sproxil_Recoded_Final.rds
# OUTPUT: Sproxil_Analysis_Ready.rds (+ .sav + .csv + QC summary)
#
# DESIGN PRINCIPLES
# - Keep ALL recoded variables + dummies (do NOT drop columns)
# - Add: (i) standardized stratifiers, (ii) explicit universe flags, (iii) derived indicators
# - Derived indicators are 0/1/NA with strict denominator discipline:
#     * NA = out-of-universe
#     * DK/Missing codes (8/9/98/99) -> NA in derived indicators unless explicitly needed as a category
# - Wealth index: PCA on 0/1 assets only; DK/Missing -> NA; PCA uses complete cases only
# - Top-coded count values are retained as lower-bound numeric values AND flagged explicitly
# - ANC/pregnancy timing variables coded as 0 represent LESS THAN ONE MONTH,
#   including responses such as 2 weeks and 3 weeks
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(labelled)
  library(FactoMineR)
})

# ---------------------------
# 1) CONFIGURATION
# ---------------------------
INPUT_FILE  <- "Sproxil_Recoded_Final.rds"
OUTPUT_RDS  <- "Sproxil_Analysis_Ready.rds"
OUTPUT_SAV  <- "Sproxil_Analysis_Ready.sav"
OUTPUT_CSV  <- "Sproxil_Analysis_Ready.csv"
OUTPUT_QC   <- "Sproxil_AnalysisReady_QC_Summary.csv"

if (!file.exists(INPUT_FILE)) stop("Recoded dataset not found. Run Script 04 first.")
df <- readRDS(INPUT_FILE)
metadata <- readRDS("Sproxil_dictionary.rds")
variable_labels <- metadata$variable_labels
value_labels <- metadata$value_labels

message("Loaded recoded dataset: ", nrow(df), " rows, ", ncol(df), " columns.")

# ---------------------------
# 2) HELPERS
# ---------------------------
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

is_miss_bin <- function(x) x %in% c(8, 9) | is.na(x)
is_miss_cat <- function(x) x %in% c(98, 99, 9) | is.na(x)

bin01_na <- function(x) case_when(
  x %in% c(0, 1) ~ as.numeric(x),
  x %in% c(8, 9) ~ NA_real_,
  TRUE ~ NA_real_
)

cat_na <- function(x) case_when(
  x %in% c(98, 99, 9) ~ NA_real_,
  is.na(x) ~ NA_real_,
  TRUE ~ as.numeric(x)
)

# Helper: valid count excluding top-code flags
count_valid <- function(x) case_when(
  is.na(x) ~ NA_real_,
  x %in% c(98, 99) ~ NA_real_,
  TRUE ~ as.numeric(x)
)

# ---------------------------
# 3) STANDARDIZE CORE FIELDS
# ---------------------------
message("Step 1: Standardizing stratifiers (type-safe numeric codes)...")

df <- df %>%
  mutate(
    demo_gender_num = to_num(demo_gender),   # 1=Male, 2=Female, 9=Missing
    demo_age_num    = to_num(demo_age),
    demo_state_num  = to_num(demo_state)
  )

# ---------------------------
# 4) WEALTH INDEX (PCA)
# ---------------------------
message("Step 2: Computing wealth index (PCA) if needed...")

asset_vars <- c(
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", "hh_has_electric_iron",
  "hh_has_generator", "hh_has_fan", "hh_own_watch", "hh_own_mobile_phone",
  "hh_own_bicycle", "hh_own_motorcycle", "hh_own_car_truck", "hh_has_bank_account"
)

df_pca_prep <- df %>%
  select(any_of(asset_vars)) %>%
  mutate(across(everything(), ~ case_when(
    .x %in% c(0, 1) ~ as.numeric(.x),
    .x %in% c(8, 9, 98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  ))) %>%
  select(where(~ n_distinct(na.omit(.x)) > 1))

pca_complete_idx <- complete.cases(df_pca_prep)
n_complete <- sum(pca_complete_idx)
p <- ncol(df_pca_prep)

if (n_complete < 30 || p < 2) {
  warning("Wealth PCA skipped: insufficient complete cases or too few varying asset variables.")
  df <- df %>%
    mutate(
      derived_wealth_score = NA_real_,
      derived_wealth_quintile = NA_real_
    )
} else {
  pca_res <- PCA(df_pca_prep[pca_complete_idx, , drop = FALSE], graph = FALSE, ncp = 1)

  df <- df %>%
    mutate(derived_wealth_score = NA_real_) %>%
    { .$derived_wealth_score[pca_complete_idx] <- pca_res$ind$coord[, 1]; . } %>%
    mutate(
      derived_wealth_quintile = ifelse(!is.na(derived_wealth_score),
                                       ntile(derived_wealth_score, 5),
                                       NA_real_)
    )
}

df <- df %>% mutate(wealth_q = to_num(derived_wealth_quintile))

# ---------------------------
# 5) ZONE / RESIDENCE / EDUCATION / AGE GROUP STRATIFIERS
# ---------------------------
message("Step 3: Deriving/standardizing zone, residence, education, age group...")

df <- df %>%
  mutate(
    derived_zone = case_when(
      demo_state_num %in% c(7, 15, 23, 24, 26, 27, 32) ~ 1,   # North Central
      demo_state_num %in% c(2, 5, 8, 16, 35, 36) ~ 2,         # North East
      demo_state_num %in% c(18, 19, 20, 21, 22, 34, 37) ~ 3,  # North West
      demo_state_num %in% c(1, 4, 11, 14, 17) ~ 4,            # South East
      demo_state_num %in% c(3, 6, 9, 10, 12, 33) ~ 5,         # South South
      demo_state_num %in% c(13, 25, 28, 29, 30, 31) ~ 6,      # South West
      TRUE ~ 9
    ),

    derived_residence = case_when(
      to_num(urban_status) == 1 ~ 1,  # Urban
      to_num(urban_status) == 0 ~ 2,  # Rural
      TRUE ~ 9
    ),

    derived_edu_cat = case_when(
      to_num(demo_edu_level) == 0 ~ 1,  # No education
      to_num(demo_edu_level) == 1 ~ 2,  # Primary
      to_num(demo_edu_level) == 2 ~ 3,  # Secondary
      to_num(demo_edu_level) %in% c(3, 4) ~ 4,  # More than secondary, includes vocational
      TRUE ~ 9
    ),

    derived_age_group_w = case_when(
      demo_age_num >= 15 & demo_age_num <= 19 ~ 1,
      demo_age_num >= 20 & demo_age_num <= 24 ~ 2,
      demo_age_num >= 25 & demo_age_num <= 29 ~ 3,
      demo_age_num >= 30 & demo_age_num <= 34 ~ 4,
      demo_age_num >= 35 & demo_age_num <= 39 ~ 5,
      demo_age_num >= 40 & demo_age_num <= 44 ~ 6,
      demo_age_num >= 45 & demo_age_num <= 49 ~ 7,
      TRUE ~ 9
    ),

    # Optional broader age groups for weighting/reporting
    derived_age_group_4 = case_when(
      demo_age_num >= 15 & demo_age_num <= 24 ~ 1,
      demo_age_num >= 25 & demo_age_num <= 34 ~ 2,
      demo_age_num >= 35 & demo_age_num <= 44 ~ 3,
      demo_age_num >= 45 ~ 4,
      TRUE ~ 9
    )
  ) %>%
  mutate(
    zone_num      = to_num(derived_zone),
    residence_num = to_num(derived_residence),
    edu_cat       = to_num(derived_edu_cat),
    age_group_4   = to_num(derived_age_group_4)
  )

# ---------------------------
# 6) TOP-CODED COUNT FLAGS
# ---------------------------
message("Step 4: Creating explicit top-code flags for count variables...")

df <- df %>%
  mutate(
    # Household / prevention counts
    tc_demo_hh_children_under5        = ifelse(to_num(demo_hh_children_under5) >= 8 & !is_miss_cat(to_num(demo_hh_children_under5)), 1, 0),
    tc_demo_hh_sleeping_rooms         = ifelse(to_num(demo_hh_sleeping_rooms) >= 5 & !is_miss_cat(to_num(demo_hh_sleeping_rooms)), 1, 0),
    tc_prev_num_mosquito_nets         = ifelse(to_num(prev_num_mosquito_nets) >= 7 & !is_miss_cat(to_num(prev_num_mosquito_nets)), 1, 0),
    tc_prev_num_people_slept_net      = ifelse(to_num(prev_num_people_slept_net) >= 10 & !is_miss_cat(to_num(prev_num_people_slept_net)), 1, 0),
    tc_prev_months_since_net_obtained = ifelse(to_num(prev_months_since_net_obtained) >= 37 & !is_miss_cat(to_num(prev_months_since_net_obtained)), 1, 0),

    # Maternal / child counts
    tc_women_births_2020_2025         = ifelse(to_num(women_births_2020_2025) >= 10 & !is_miss_cat(to_num(women_births_2020_2025)), 1, 0),
    tc_women_anc_total_visits         = ifelse(to_num(women_anc_total_visits) >= 10 & !is_miss_cat(to_num(women_anc_total_visits)), 1, 0),
    tc_women_sp_fansidar_doses        = ifelse(to_num(women_sp_fansidar_doses) >= 10 & !is_miss_cat(to_num(women_sp_fansidar_doses)), 1, 0),

    # Household composition / WASH
    tc_hh_total_persons_v1            = ifelse(to_num(hh_total_persons_v1) >= 10 & !is_miss_cat(to_num(hh_total_persons_v1)), 1, 0),
    tc_hh_total_persons_v2            = ifelse(to_num(hh_total_persons_v2) >= 10 & !is_miss_cat(to_num(hh_total_persons_v2)), 1, 0),
    tc_hh_members_under5              = ifelse(to_num(hh_members_under5) >= 10 & !is_miss_cat(to_num(hh_members_under5)), 1, 0),
    tc_hh_total_persons_usually_v3    = ifelse(to_num(hh_total_persons_usually_v3) >= 10 & !is_miss_cat(to_num(hh_total_persons_usually_v3)), 1, 0),
    tc_hh_water_time_trip             = ifelse(to_num(hh_water_time_trip) >= 31 & !is_miss_cat(to_num(hh_water_time_trip)), 1, 0),
    tc_hh_toilet_share_count          = ifelse(to_num(hh_toilet_share_count) >= 10 & !is_miss_cat(to_num(hh_toilet_share_count)), 1, 0),

    # Livestock / land
    tc_hh_num_cows_bulls              = ifelse(to_num(hh_num_cows_bulls) >= 95 & !is_miss_cat(to_num(hh_num_cows_bulls)), 1, 0),
    tc_hh_num_other_cattle            = ifelse(to_num(hh_num_other_cattle) >= 95 & !is_miss_cat(to_num(hh_num_other_cattle)), 1, 0),
    tc_hh_num_horses_donkeys          = ifelse(to_num(hh_num_horses_donkeys) >= 95 & !is_miss_cat(to_num(hh_num_horses_donkeys)), 1, 0),
    tc_hh_num_goats                   = ifelse(to_num(hh_num_goats) >= 95 & !is_miss_cat(to_num(hh_num_goats)), 1, 0),
    tc_hh_num_sheep                   = ifelse(to_num(hh_num_sheep) >= 95 & !is_miss_cat(to_num(hh_num_sheep)), 1, 0),
    tc_hh_num_poultry                 = ifelse(to_num(hh_num_poultry) >= 95 & !is_miss_cat(to_num(hh_num_poultry)), 1, 0),
    tc_hh_num_pigs                    = ifelse(to_num(hh_num_pigs) >= 95 & !is_miss_cat(to_num(hh_num_pigs)), 1, 0),
    tc_hh_num_camels                  = ifelse(to_num(hh_num_camels) >= 95 & !is_miss_cat(to_num(hh_num_camels)), 1, 0),
    tc_hh_num_agri_plots              = ifelse(to_num(hh_num_agri_plots) >= 95 & !is_miss_cat(to_num(hh_num_agri_plots)), 1, 0)
  )

# ---------------------------
# 7) UNIVERSE FLAGS
# ---------------------------
message("Step 5: Creating explicit universe flags...")

df <- df %>%
  mutate(
    u_all = 1,
    u_household = 1,

    u_women_15_49 = ifelse(demo_gender_num == 2 & demo_age_num >= 15 & demo_age_num <= 49, 1, 0),

    births_2020_2025_num = to_num(women_births_2020_2025),
    u_recent_birth = case_when(
      u_women_15_49 != 1 ~ 0,
      births_2020_2025_num %in% c(98, 99) ~ 0,
      births_2020_2025_num > 0 ~ 1,
      births_2020_2025_num == 0 ~ 0,
      TRUE ~ 0
    ),

    hh_u5_num = to_num(demo_hh_children_under5),
    u_hh_has_u5 = case_when(
      is.na(hh_u5_num) ~ NA_real_,
      hh_u5_num %in% c(98, 99) ~ NA_real_,
      hh_u5_num > 0 ~ 1,
      hh_u5_num == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    u_child_youngest = case_when(
      u_hh_has_u5 == 1 ~ 1,
      u_hh_has_u5 == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    u_child_fever = case_when(
      u_child_youngest != 1 ~ NA_real_,
      to_num(women_child_fever_2weeks) == 1 ~ 1,
      to_num(women_child_fever_2weeks) == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    u_msg = ifelse(to_num(bg_heard_malaria_msg_6months) == 1, 1, 0),
    u_avoid = ifelse(to_num(bg_aware_avoidance) == 1, 1, 0),
    u_internet = ifelse(to_num(bg_internet_ever_used) == 1, 1, 0),

    # General affordability question: all respondents
    u_affordability = ifelse(!is.na(to_num(treat_drug_affordability)), 1, 0),

    # Fever-path only follow-ups
    u_recent_fever_hh = case_when(
      to_num(treat_hh_fever_last_2weeks) == 1 ~ 1,
      to_num(treat_hh_fever_last_2weeks) == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ---------------------------
# 8) DERIVED INDICATORS
# ---------------------------
message("Step 6: Deriving indicators (strict denominators)...")

df <- df %>%
  mutate(
    # ----------------------------------------------------------
    # 8A) NETS / ITNs
    # ----------------------------------------------------------
    derived_hh_has_any_net = case_when(
      to_num(prev_has_mosquito_nets) == 1 ~ 1,
      to_num(prev_has_mosquito_nets) == 0 ~ 0,
      to_num(prev_has_mosquito_nets) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    prev_is_itn_num = to_num(prev_is_itn),
    prev_net_brand_num = to_num(prev_net_brand),

    derived_hh_has_itn = case_when(
      to_num(prev_has_mosquito_nets) != 1 ~ NA_real_,
      prev_is_itn_num == 1 ~ 1,
      prev_net_brand_num %in% 10:26 ~ 1,
      prev_is_itn_num == 0 ~ 0,
      prev_is_itn_num %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    hh_nets_num = to_num(prev_num_mosquito_nets),
    derived_num_itns = case_when(
      derived_hh_has_itn == 1 & !is_miss_cat(hh_nets_num) ~ hh_nets_num,
      derived_hh_has_itn == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    hh_size_num = to_num(hh_total_persons_v1),
    derived_access_itn = case_when(
      is.na(derived_num_itns) ~ NA_real_,
      is_miss_cat(hh_size_num) ~ NA_real_,
      hh_size_num <= 0 ~ NA_real_,
      (derived_num_itns * 2 >= hh_size_num) ~ 1,
      TRUE ~ 0
    ),

    derived_net_use_any_last_night = case_when(
      to_num(prev_slept_under_net_last_night) == 1 ~ 1,
      to_num(prev_slept_under_net_last_night) == 0 ~ 0,
      to_num(prev_slept_under_net_last_night) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    ppl_slept_net_num = to_num(prev_num_people_slept_net),
    derived_net_use_people_count = case_when(
      derived_net_use_any_last_night != 1 ~ NA_real_,
      ppl_slept_net_num %in% c(98, 99) ~ NA_real_,
      TRUE ~ ppl_slept_net_num
    ),

    # Useful continuous proxy: proportion of household sleeping under net
    derived_prop_hh_slept_under_net = case_when(
      is.na(derived_net_use_people_count) ~ NA_real_,
      is_miss_cat(hh_size_num) ~ NA_real_,
      hh_size_num <= 0 ~ NA_real_,
      TRUE ~ pmin(derived_net_use_people_count / hh_size_num, 1)
    ),

    # ----------------------------------------------------------
    # 8B) ANC / IPTp
    # ----------------------------------------------------------
    derived_anc_any = case_when(
      u_recent_birth != 1 ~ NA_real_,
      to_num(women_anc_seen) == 1 ~ 1,
      to_num(women_anc_seen) == 0 ~ 0,
      to_num(women_anc_seen) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    derived_anc_skilled = case_when(
      u_recent_birth != 1 ~ NA_real_,
      to_num(women_anc_seen) == 0 ~ 0,
      to_num(women_anc_seen) %in% c(8, 9) ~ NA_real_,
      to_num(women_anc_seen) == 1 &
        (to_num(anc_prov_doc) == 1 | to_num(anc_prov_nurse) == 1 | to_num(anc_prov_aux) == 1) ~ 1,
      to_num(women_anc_seen) == 1 ~ 0,
      TRUE ~ NA_real_
    ),

    anc_visits_num = to_num(women_anc_total_visits),
    derived_anc_4plus = case_when(
      u_recent_birth != 1 ~ NA_real_,
      to_num(women_anc_seen) == 0 ~ 0,
      to_num(women_anc_seen) %in% c(8, 9) ~ NA_real_,
      anc_visits_num %in% c(98, 99) ~ NA_real_,
      anc_visits_num >= 4 ~ 1,
      TRUE ~ 0
    ),

    # ANC timing variable:
    # 0 = less than one month, including 2 weeks and 3 weeks
    anc_first_month_num = to_num(women_anc_first_visit_month),
    derived_anc_first_trimester = case_when(
      u_recent_birth != 1 ~ NA_real_,
      anc_first_month_num %in% c(98, 99) ~ NA_real_,
      anc_first_month_num >= 0 & anc_first_month_num <= 3 ~ 1,
      anc_first_month_num > 3 ~ 0,
      TRUE ~ NA_real_
    ),

    sp_taken_num = to_num(women_took_sp_fansidar),
    sp_doses_num = to_num(women_sp_fansidar_doses),

    derived_iptp_1plus = case_when(
      u_recent_birth != 1 ~ NA_real_,
      sp_taken_num == 0 ~ 0,
      sp_taken_num %in% c(8, 9) ~ NA_real_,
      sp_doses_num %in% c(98, 99) ~ NA_real_,
      sp_doses_num >= 1 ~ 1,
      TRUE ~ 0
    ),
    derived_iptp_2plus = case_when(
      u_recent_birth != 1 ~ NA_real_,
      sp_taken_num == 0 ~ 0,
      sp_taken_num %in% c(8, 9) ~ NA_real_,
      sp_doses_num %in% c(98, 99) ~ NA_real_,
      sp_doses_num >= 2 ~ 1,
      TRUE ~ 0
    ),
    derived_iptp_3plus = case_when(
      u_recent_birth != 1 ~ NA_real_,
      sp_taken_num == 0 ~ 0,
      sp_taken_num %in% c(8, 9) ~ NA_real_,
      sp_doses_num %in% c(98, 99) ~ NA_real_,
      sp_doses_num >= 3 ~ 1,
      TRUE ~ 0
    ),

    # ----------------------------------------------------------
    # 8C) CURRENT PREGNANCY
    # ----------------------------------------------------------
    preg_month_num = to_num(women_pregnancy_duration_months),
    derived_currently_pregnant = case_when(
      to_num(women_currently_pregnant) == 1 ~ 1,
      to_num(women_currently_pregnant) == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    # 0 = less than one month, including 2 weeks and 3 weeks
    derived_pregnancy_first_trimester = case_when(
      derived_currently_pregnant != 1 ~ NA_real_,
      preg_month_num %in% c(98, 99) ~ NA_real_,
      preg_month_num >= 0 & preg_month_num <= 3 ~ 1,
      preg_month_num > 3 ~ 0,
      TRUE ~ NA_real_
    ),

    # ----------------------------------------------------------
    # 8D) CHILD FEVER MANAGEMENT
    # ----------------------------------------------------------
    derived_child_fever = case_when(
      u_child_youngest != 1 ~ NA_real_,
      to_num(women_child_fever_2weeks) == 1 ~ 1,
      to_num(women_child_fever_2weeks) == 0 ~ 0,
      to_num(women_child_fever_2weeks) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    derived_fever_seek_advice = case_when(
      u_child_fever != 1 ~ NA_real_,
      to_num(women_child_seek_advice) == 1 ~ 1,
      to_num(women_child_seek_advice) == 0 ~ 0,
      to_num(women_child_seek_advice) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    delay_num = to_num(women_child_advice_delay_days),
    derived_fever_prompt_care = case_when(
      u_child_fever != 1 ~ NA_real_,
      to_num(women_child_seek_advice) != 1 ~ 0,
      delay_num %in% c(8, 9) ~ NA_real_,
      is.na(delay_num) ~ NA_real_,
      delay_num <= 1 ~ 1,
      TRUE ~ 0
    ),

    derived_fever_tested = case_when(
      u_child_fever != 1 ~ NA_real_,
      to_num(women_child_blood_sample) == 1 ~ 1,
      to_num(women_child_blood_sample) == 0 ~ 0,
      to_num(women_child_blood_sample) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    derived_fever_took_any_medicine = case_when(
      u_child_fever != 1 ~ NA_real_,
      to_num(women_child_took_medicine) == 1 ~ 1,
      to_num(women_child_took_medicine) == 0 ~ 0,
      to_num(women_child_took_medicine) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # Antimalarial composite ONLY from explicit antimalarial classes
    derived_fever_took_anti = case_when(
      u_child_fever != 1 ~ NA_real_,
      to_num(women_child_took_medicine) != 1 ~ 0,
      (to_num(med_act) == 1 | to_num(med_sp) == 1 | to_num(med_chloro) == 1 |
         to_num(med_amod) == 1 | to_num(med_artesun) == 1 | to_num(med_quinine) == 1 |
         to_num(med_inject) == 1) ~ 1,
      TRUE ~ 0
    ),

    derived_fever_took_act    = case_when(u_child_fever != 1 ~ NA_real_, to_num(women_child_took_medicine) != 1 ~ 0, to_num(med_act) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_sp     = case_when(u_child_fever != 1 ~ NA_real_, to_num(women_child_took_medicine) != 1 ~ 0, to_num(med_sp) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_chloro = case_when(u_child_fever != 1 ~ NA_real_, to_num(women_child_took_medicine) != 1 ~ 0, to_num(med_chloro) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_amod   = case_when(u_child_fever != 1 ~ NA_real_, to_num(women_child_took_medicine) != 1 ~ 0, to_num(med_amod) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_quin   = case_when(u_child_fever != 1 ~ NA_real_, to_num(women_child_took_medicine) != 1 ~ 0, to_num(med_quinine) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_artes  = case_when(u_child_fever != 1 ~ NA_real_, to_num(women_child_took_medicine) != 1 ~ 0, to_num(med_artesun) == 1 ~ 1, TRUE ~ 0),

    act_delay_num = to_num(women_child_act_delay),
    derived_act_prompt = case_when(
      u_child_fever != 1 ~ NA_real_,
      derived_fever_took_act != 1 ~ NA_real_,
      act_delay_num %in% c(8, 9) ~ NA_real_,
      is.na(act_delay_num) ~ NA_real_,
      act_delay_num %in% c(0, 1) ~ 1,
      TRUE ~ 0
    ),
    derived_act_effective = case_when(
      u_child_fever != 1 ~ NA_real_,
      derived_fever_took_act != 1 ~ NA_real_,
      to_num(women_child_act_effective) == 1 ~ 1,
      to_num(women_child_act_effective) == 0 ~ 0,
      to_num(women_child_act_effective) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # ----------------------------------------------------------
    # 8E) PERCEPTIONS & EXPERIENCES
    # ----------------------------------------------------------
    afford_num = to_num(treat_drug_affordability),
    derived_percep_affordable = case_when(
      afford_num %in% c(1, 2) ~ 1,
      afford_num %in% c(3, 4, 5) ~ 0,
      afford_num %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    gov_num = to_num(feedback_gov_effort_rating),
    derived_gov_effective = case_when(
      gov_num %in% c(4, 5) ~ 1,
      gov_num %in% c(1, 2, 3) ~ 0,
      gov_num == 9 ~ NA_real_,
      TRUE ~ NA_real_
    ),

    stockout_num = to_num(feedback_drug_stockout_6months),
    free_num     = to_num(feedback_free_treatment_6months),

    u_govt_visit_6m_stockout = ifelse(stockout_num %in% c(0, 1), 1, 0),
    u_govt_visit_6m_free     = ifelse(free_num %in% c(0, 1), 1, 0),

    derived_exp_stockout = case_when(
      u_govt_visit_6m_stockout != 1 ~ NA_real_,
      stockout_num == 1 ~ 1,
      stockout_num == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    derived_exp_free_tx = case_when(
      u_govt_visit_6m_free != 1 ~ NA_real_,
      free_num == 1 ~ 1,
      free_num == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    # Fever-path costs
    test_cost_num = to_num(treat_test_cost),
    drug_cost_num = to_num(treat_drug_cost),
    drug_buy_time_num = to_num(treat_drug_purchase_time),

    derived_test_paid = case_when(
      u_recent_fever_hh != 1 ~ NA_real_,
      test_cost_num == 0 ~ 0,
      test_cost_num %in% 1:16 ~ 1,
      TRUE ~ NA_real_
    ),
    derived_drug_paid = case_when(
      u_recent_fever_hh != 1 ~ NA_real_,
      drug_cost_num == 0 ~ 0,
      drug_cost_num %in% 1:16 ~ 1,
      TRUE ~ NA_real_
    )
  )

# ---------------------------
# 9) APPLY LABELS
# ---------------------------
df_labelled <- df

message("Step 7: Applying labels to key derived fields...")

present_lbls <- variable_labels[names(variable_labels) %in% names(df_labelled)]
missing_labels <- setdiff(names(df_labelled), names(present_lbls))

if (length(missing_labels) > 0) {
  message("The following columns are missing labels: ", paste(missing_labels, collapse = ", "))
} else {
  message("All variable labels are present.")
}

df_labelled <- labelled::set_variable_labels(df_labelled, .labels = present_lbls)


val_labels(df_labelled$derived_zone)            <- value_labels$zone
val_labels(df_labelled$derived_residence)       <- value_labels$derived_residence
val_labels(df_labelled$derived_wealth_quintile) <- value_labels$wealth_quintile
val_labels(df_labelled$derived_edu_cat)         <- value_labels$edu4
val_labels(df_labelled$derived_age_group_w)     <- value_labels$age_group_women

yn_vars <- c(
  "derived_hh_has_any_net","derived_hh_has_itn","derived_access_itn",
  "derived_net_use_any_last_night","derived_anc_any","derived_anc_skilled",
  "derived_anc_4plus","derived_anc_first_trimester","derived_iptp_1plus","derived_iptp_2plus","derived_iptp_3plus",
  "derived_currently_pregnant","derived_pregnancy_first_trimester",
  "derived_child_fever","derived_fever_seek_advice","derived_fever_prompt_care","derived_fever_tested",
  "derived_fever_took_any_medicine","derived_fever_took_anti","derived_fever_took_act","derived_fever_took_sp",
  "derived_fever_took_chloro","derived_fever_took_amod","derived_fever_took_quin","derived_fever_took_artes",
  "derived_act_prompt","derived_act_effective","derived_percep_affordable","derived_gov_effective",
  "derived_exp_stockout","derived_exp_free_tx","derived_test_paid","derived_drug_paid"
)

for (v in yn_vars) {
  if (v %in% names(df_labelled)) val_labels(df_labelled[[v]]) <- value_labels$yesno_simple
}

# --- Apply value labels for newly added derived variables ---

# Broader age groups
if ("derived_age_group_4" %in% names(df_labelled)) {
  val_labels(df_labelled$derived_age_group_4) <- value_labels$age_group_4
}
if ("age_group_4" %in% names(df_labelled)) {
  val_labels(df_labelled$age_group_4) <- value_labels$age_group_4
}

# Yes/No/Missing style derived variables
new_yn_vars <- c(
  "derived_anc_first_trimester",
  "derived_currently_pregnant",
  "derived_pregnancy_first_trimester",
  "derived_test_paid",
  "derived_drug_paid"
)

for (v in new_yn_vars) {
  if (v %in% names(df_labelled)) val_labels(df_labelled[[v]]) <- value_labels$yesno_simple
}

# Top-code flags
topcode_vars <- c(
  "tc_demo_hh_children_under5",
  "tc_demo_hh_sleeping_rooms",
  "tc_prev_num_mosquito_nets",
  "tc_prev_num_people_slept_net",
  "tc_prev_months_since_net_obtained",
  "tc_women_births_2020_2025",
  "tc_women_anc_total_visits",
  "tc_women_sp_fansidar_doses",
  "tc_hh_total_persons_v1",
  "tc_hh_total_persons_v2",
  "tc_hh_members_under5",
  "tc_hh_total_persons_usually_v3",
  "tc_hh_water_time_trip",
  "tc_hh_toilet_share_count",
  "tc_hh_num_cows_bulls",
  "tc_hh_num_other_cattle",
  "tc_hh_num_horses_donkeys",
  "tc_hh_num_goats",
  "tc_hh_num_sheep",
  "tc_hh_num_poultry",
  "tc_hh_num_pigs",
  "tc_hh_num_camels",
  "tc_hh_num_agri_plots"
)

for (v in topcode_vars) {
  if (v %in% names(df_labelled)) val_labels(df_labelled[[v]]) <- value_labels$topcode_flag
}

# ---------------------------
# 10) QC SUMMARY
# ---------------------------
message("Step 8: Generating QC summary...")

qc <- tibble(
  Metric = c(
    "N_total",
    "N_women_15_49",
    "N_recent_birth_proxy",
    "N_hh_has_u5",
    "N_child_fever_universe",
    "N_recent_fever_households",
    "Wealth_quintile_nonmissing"
  ),
  Value = c(
    nrow(df),
    sum(df$u_women_15_49 == 1, na.rm = TRUE),
    sum(df$u_recent_birth == 1, na.rm = TRUE),
    sum(df$u_hh_has_u5 == 1, na.rm = TRUE),
    sum(df$u_child_fever == 1, na.rm = TRUE),
    sum(df$u_recent_fever_hh == 1, na.rm = TRUE),
    sum(!is.na(to_num(df$derived_wealth_quintile)), na.rm = TRUE)
  )
) %>%
  mutate(Value = as.character(Value))

key_inds <- c(
  "derived_hh_has_any_net", "derived_hh_has_itn", "derived_access_itn",
  "derived_anc_any", "derived_anc_skilled", "derived_anc_4plus", "derived_anc_first_trimester",
  "derived_iptp_1plus", "derived_iptp_2plus", "derived_iptp_3plus",
  "derived_currently_pregnant", "derived_pregnancy_first_trimester",
  "derived_child_fever", "derived_fever_seek_advice", "derived_fever_prompt_care",
  "derived_fever_tested", "derived_fever_took_act",
  "derived_percep_affordable", "derived_gov_effective",
  "derived_exp_stockout", "derived_exp_free_tx",
  "derived_test_paid", "derived_drug_paid"
)

miss_df <- tibble(
  Indicator = key_inds,
  Missing_N = sapply(key_inds, function(v) sum(is.na(to_num(df[[v]])), na.rm = TRUE)),
  NonMissing_N = sapply(key_inds, function(v) sum(!is.na(to_num(df[[v]])), na.rm = TRUE))
)

qc_out <- bind_rows(
  qc %>% mutate(Section = "Universe counts") %>% select(Section, Metric, Value),
  miss_df %>% transmute(
    Section = "Indicator missingness",
    Metric  = Indicator,
    Value   = paste0("Missing=", Missing_N, "; NonMissing=", NonMissing_N)
  )
)

out_dir <- dirname(OUTPUT_QC)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(qc_out, OUTPUT_QC, row.names = FALSE)

# ---------------------------
# 11) EXPORT
# ---------------------------
message("Step 9: Exporting datasets...")

saveRDS(df_labelled, OUTPUT_RDS)
write_sav(df_labelled, OUTPUT_SAV)
write.csv(df_labelled, OUTPUT_CSV, row.names = FALSE)

df_shareable <- df_labelled %>%
  select(-c(meta_latitude, meta_longitude))

write.csv(df_shareable, "Sproxil_Shareable.csv", row.names = FALSE)

message("✅ DONE: Datasets saved:")
message(" - ", OUTPUT_RDS)
message(" - ", OUTPUT_SAV)
message(" - ", OUTPUT_CSV)
message(" - Sproxil_Shareable.csv")