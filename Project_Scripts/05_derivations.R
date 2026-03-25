# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Indicators and Denominators Derivations
# AUTHOR:  Corona Management Systems
# DATE:    26 November, 2025
#
# PURPOSE:
# Builds the analysis-ready dataset used for MIS-style tabulations.
#
# This script:
# 1. loads the recoded dataset and metadata,
# 2. standardises core numeric stratifiers,
# 3. derives standard stratification variables,
# 4. creates explicit top-code flags,
# 5. defines universe flags for denominator control,
# 6. derives analysis indicators with strict denominator rules,
# 7. derives grouped living-standards variables and computes the wealth PCA,
# 8. prepares separate unlabelled and labelled export objects,
# 9. generates a QC summary, and
# 10. exports the analysis-ready datasets.
#
# INPUT:
#   - Sproxil_Recoded_Final.rds
#
# OUTPUT:
#   - Sproxil_Analysis_Ready.rds          [unlabelled R-native]
#   - Sproxil_Analysis_Ready.sav          [labelled for SPSS]
#   - Sproxil_Analysis_Ready.csv          [unlabelled]
#   - Sproxil_AnalysisReady_QC_Summary.csv
#   - Sproxil_Shareable.csv               [unlabelled]
#
# REPLICATION NOTE:
# This file preserves all recoded variables and dummies, then adds standardised
# stratifiers, explicit universe flags, and derived indicators for tabulation.
# Derived indicators follow strict denominator discipline, with NA used for
# out-of-universe observations and non-informative missing codes excluded unless
# they are explicitly needed as categories.
#
# DESIGN PRINCIPLES:
# - Keep all recoded variables and dummies.
# - Add standardised stratifiers, explicit universe flags, and derived indicators.
# - Derived indicators use 0/1/NA with strict denominator discipline.
# - DK and missing codes are set to NA in derived indicators unless required.
# - Wealth PCA uses a broader sample-derived living-standards specification.
# - Top-coded count values are retained as lower-bound numeric values and flagged.
# - R-native outputs remain unlabelled for robust downstream R analysis.
# - SAV output carries labels for SPSS and metadata-rich handoff.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(labelled)
  library(FactoMineR)
})

# ------------------------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------------------------
S05_INPUT_FILE <- "Sproxil_Recoded_Final.rds"
S05_OUTPUT_RDS <- "Sproxil_Analysis_Ready.rds"
S05_OUTPUT_SAV <- "Sproxil_Analysis_Ready.sav"
S05_OUTPUT_CSV <- "Sproxil_Analysis_Ready.csv"
S05_OUTPUT_QC  <- "Sproxil_AnalysisReady_QC_Summary.csv"

if (!file.exists(S05_INPUT_FILE)) {
  stop("Recoded dataset not found. Run Script 04 first.")
}

s05_df_recoded <- readRDS(S05_INPUT_FILE)
s05_metadata <- readRDS("Sproxil_dictionary.rds")
s05_variable_labels <- s05_metadata$variable_labels
s05_value_labels <- s05_metadata$value_labels

message(
  "Loaded recoded dataset: ",
  nrow(s05_df_recoded),
  " rows, ",
  ncol(s05_df_recoded),
  " columns."
)

# ------------------------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------------------------
s05_to_num <- function(x) {
  if (is.factor(x)) {
    return(suppressWarnings(as.numeric(as.character(x))))
  }
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  suppressWarnings(as.numeric(x))
}

s05_is_miss_cat <- function(x) x %in% c(98, 99, 9) | is.na(x)

# ------------------------------------------------------------------------------
# 3. Standardise core fields
# ------------------------------------------------------------------------------
message("Step 1: Standardising stratifiers (type-safe numeric codes)...")

s05_df_analysis <- s05_df_recoded %>%
  mutate(
    demo_gender_num = s05_to_num(demo_gender),
    demo_age_num    = s05_to_num(demo_age),
    demo_state_num  = s05_to_num(demo_state)
  )

# ------------------------------------------------------------------------------
# 4. Derive zone, residence, education, and age group stratifiers
# ------------------------------------------------------------------------------
message("Step 2: Deriving and standardising zone, residence, education, and age group...")

s05_df_analysis <- s05_df_analysis %>%
  mutate(
    derived_zone = case_when(
      demo_state_num %in% c(7, 15, 23, 24, 26, 27, 32) ~ 1,
      demo_state_num %in% c(2, 5, 8, 16, 35, 36) ~ 2,
      demo_state_num %in% c(18, 19, 20, 21, 22, 34, 37) ~ 3,
      demo_state_num %in% c(1, 4, 11, 14, 17) ~ 4,
      demo_state_num %in% c(3, 6, 9, 10, 12, 33) ~ 5,
      demo_state_num %in% c(13, 25, 28, 29, 30, 31) ~ 6,
      TRUE ~ 9
    ),
    
    derived_residence = case_when(
      s05_to_num(urban_status) == 1 ~ 1,
      s05_to_num(urban_status) == 0 ~ 2,
      TRUE ~ 9
    ),
    
    derived_edu_cat = case_when(
      s05_to_num(demo_edu_level) == 0 ~ 1,
      s05_to_num(demo_edu_level) == 1 ~ 2,
      s05_to_num(demo_edu_level) == 2 ~ 3,
      s05_to_num(demo_edu_level) %in% c(3, 4) ~ 4,
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
      demo_age_num == 997 ~ 8, 
      TRUE ~ 9
    ),
    
    derived_age_group_4 = case_when(
      demo_age_num >= 15 & demo_age_num <= 24 ~ 1,
      demo_age_num >= 25 & demo_age_num <= 34 ~ 2,
      demo_age_num >= 35 & demo_age_num <= 44 ~ 3,
      demo_age_num >= 45 & demo_age_num <= 64 ~ 4,
      demo_age_num == 997  ~ 5,
      TRUE ~ 9
    )
  ) %>%
  mutate(
    zone_num      = s05_to_num(derived_zone),
    residence_num = s05_to_num(derived_residence),
    edu_cat       = s05_to_num(derived_edu_cat),
    age_group_4   = s05_to_num(derived_age_group_4)
  )

# ------------------------------------------------------------------------------
# 5. Create explicit top-code flags
# ------------------------------------------------------------------------------
message("Step 3: Creating explicit top-code flags for count variables...")

s05_df_analysis <- s05_df_analysis %>%
  mutate(
    tc_demo_hh_children_under5        = ifelse(s05_to_num(demo_hh_children_under5) >= 8  & !s05_is_miss_cat(s05_to_num(demo_hh_children_under5)), 1, 0),
    tc_demo_hh_sleeping_rooms         = ifelse(s05_to_num(demo_hh_sleeping_rooms) >= 5   & !s05_is_miss_cat(s05_to_num(demo_hh_sleeping_rooms)), 1, 0),
    tc_prev_num_mosquito_nets         = ifelse(s05_to_num(prev_num_mosquito_nets) >= 7   & !s05_is_miss_cat(s05_to_num(prev_num_mosquito_nets)), 1, 0),
    tc_prev_num_people_slept_net      = ifelse(s05_to_num(prev_num_people_slept_net) >= 10 & !s05_is_miss_cat(s05_to_num(prev_num_people_slept_net)), 1, 0),
    tc_prev_months_since_net_obtained = ifelse(s05_to_num(prev_months_since_net_obtained) >= 37 & !s05_is_miss_cat(s05_to_num(prev_months_since_net_obtained)), 1, 0),
    
    tc_women_births_2020_2025         = ifelse(s05_to_num(women_births_2020_2025) >= 10 & !s05_is_miss_cat(s05_to_num(women_births_2020_2025)), 1, 0),
    tc_women_anc_total_visits         = ifelse(s05_to_num(women_anc_total_visits) >= 10 & !s05_is_miss_cat(s05_to_num(women_anc_total_visits)), 1, 0),
    tc_women_sp_fansidar_doses        = ifelse(s05_to_num(women_sp_fansidar_doses) >= 10 & !s05_is_miss_cat(s05_to_num(women_sp_fansidar_doses)), 1, 0),
    
    tc_hh_total_persons_v1            = ifelse(s05_to_num(hh_total_persons_v1) >= 10 & !s05_is_miss_cat(s05_to_num(hh_total_persons_v1)), 1, 0),
    tc_hh_total_persons_v2            = ifelse(s05_to_num(hh_total_persons_v2) >= 10 & !s05_is_miss_cat(s05_to_num(hh_total_persons_v2)), 1, 0),
    tc_hh_members_under5              = ifelse(s05_to_num(hh_members_under5) >= 10 & !s05_is_miss_cat(s05_to_num(hh_members_under5)), 1, 0),
    tc_hh_total_persons_usually_v3    = ifelse(s05_to_num(hh_total_persons_usually_v3) >= 10 & !s05_is_miss_cat(s05_to_num(hh_total_persons_usually_v3)), 1, 0),
    tc_hh_water_time_trip             = ifelse(s05_to_num(hh_water_time_trip) >= 31 & !s05_is_miss_cat(s05_to_num(hh_water_time_trip)), 1, 0),
    tc_hh_toilet_share_count          = ifelse(s05_to_num(hh_toilet_share_count) >= 10 & !s05_is_miss_cat(s05_to_num(hh_toilet_share_count)), 1, 0),
    
    tc_hh_num_cows_bulls              = ifelse(s05_to_num(hh_num_cows_bulls) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_cows_bulls)), 1, 0),
    tc_hh_num_other_cattle            = ifelse(s05_to_num(hh_num_other_cattle) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_other_cattle)), 1, 0),
    tc_hh_num_horses_donkeys          = ifelse(s05_to_num(hh_num_horses_donkeys) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_horses_donkeys)), 1, 0),
    tc_hh_num_goats                   = ifelse(s05_to_num(hh_num_goats) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_goats)), 1, 0),
    tc_hh_num_sheep                   = ifelse(s05_to_num(hh_num_sheep) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_sheep)), 1, 0),
    tc_hh_num_poultry                 = ifelse(s05_to_num(hh_num_poultry) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_poultry)), 1, 0),
    tc_hh_num_pigs                    = ifelse(s05_to_num(hh_num_pigs) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_pigs)), 1, 0),
    tc_hh_num_camels                  = ifelse(s05_to_num(hh_num_camels) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_camels)), 1, 0),
    tc_hh_num_agri_plots              = ifelse(s05_to_num(hh_num_agri_plots) >= 95 & !s05_is_miss_cat(s05_to_num(hh_num_agri_plots)), 1, 0)
  )

# ------------------------------------------------------------------------------
# 6. Create universe flags
# ------------------------------------------------------------------------------
message("Step 4: Creating explicit universe flags...")

s05_df_analysis <- s05_df_analysis %>%
  mutate(
    u_all = 1,
    u_household = 1,
    
    u_women_15_49 = ifelse(demo_gender_num == 2 & demo_age_num >= 15 & demo_age_num <= 49, 1, 0),
    
    births_2020_2025_num = s05_to_num(women_births_2020_2025),
    u_recent_birth = case_when(
      u_women_15_49 != 1 ~ 0,
      births_2020_2025_num %in% c(98, 99) ~ 0,
      births_2020_2025_num > 0 ~ 1,
      births_2020_2025_num == 0 ~ 0,
      TRUE ~ 0
    ),
    
    hh_u5_num = s05_to_num(demo_hh_children_under5),
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
      s05_to_num(women_child_fever_2weeks) == 1 ~ 1,
      s05_to_num(women_child_fever_2weeks) == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    u_msg = ifelse(s05_to_num(bg_heard_malaria_msg_6months) == 1, 1, 0),
    u_avoid = ifelse(s05_to_num(bg_aware_avoidance) == 1, 1, 0),
    u_internet = ifelse(s05_to_num(bg_internet_ever_used) == 1, 1, 0),
    
    u_affordability = ifelse(!is.na(s05_to_num(treat_drug_affordability)), 1, 0),
    
    u_recent_fever_hh = case_when(
      s05_to_num(treat_hh_fever_last_2weeks) == 1 ~ 1,
      s05_to_num(treat_hh_fever_last_2weeks) == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------------------------
# 7. Derive analysis indicators
# ------------------------------------------------------------------------------
message("Step 5: Deriving indicators with strict denominators...")

s05_df_analysis <- s05_df_analysis %>%
  mutate(
    derived_hh_has_any_net = case_when(
      s05_to_num(prev_has_mosquito_nets) == 1 ~ 1,
      s05_to_num(prev_has_mosquito_nets) == 0 ~ 0,
      s05_to_num(prev_has_mosquito_nets) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    prev_is_itn_num = s05_to_num(prev_is_itn),
    prev_net_brand_num = s05_to_num(prev_net_brand),
    
    derived_hh_has_itn = case_when(
      derived_hh_has_any_net != 1 ~ NA_real_,
      prev_is_itn_num == 1 ~ 1,
      prev_net_brand_num %in% 10:26 ~ 1,
      prev_is_itn_num == 0 & prev_net_brand_num == 95 ~ 0,
      prev_is_itn_num %in% c(8, 9) & prev_net_brand_num %in% c(98, 99, 96) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    hh_nets_num = s05_to_num(prev_num_mosquito_nets),
    
    # Conservative proxy only: at least one ITN confirmed
    derived_num_itns = case_when(
      derived_hh_has_itn == 1 ~ 1,
      derived_hh_has_itn == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    hh_size_num = s05_to_num(hh_total_persons_usually_v3),
    derived_access_itn = case_when(
      is.na(derived_num_itns) ~ NA_real_,
      s05_is_miss_cat(hh_size_num) ~ NA_real_,
      hh_size_num <= 0 ~ NA_real_,
      (derived_num_itns * 2 >= hh_size_num) ~ 1,
      TRUE ~ 0
    ),
    
    derived_net_use_any_last_night = case_when(
      s05_to_num(prev_slept_under_net_last_night) == 1 ~ 1,
      s05_to_num(prev_slept_under_net_last_night) == 0 ~ 0,
      s05_to_num(prev_slept_under_net_last_night) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    ppl_slept_net_num = s05_to_num(prev_num_people_slept_net),
    derived_net_use_people_count = case_when(
      derived_net_use_any_last_night != 1 ~ NA_real_,
      ppl_slept_net_num %in% c(98, 99) ~ NA_real_,
      TRUE ~ ppl_slept_net_num
    ),
    
    derived_prop_hh_slept_under_net = case_when(
      is.na(derived_net_use_people_count) ~ NA_real_,
      s05_is_miss_cat(hh_size_num) ~ NA_real_,
      hh_size_num <= 0 ~ NA_real_,
      TRUE ~ pmin(derived_net_use_people_count / hh_size_num, 1)
    ),
    
    derived_anc_any = case_when(
      u_recent_birth != 1 ~ NA_real_,
      s05_to_num(women_anc_seen) == 1 ~ 1,
      s05_to_num(women_anc_seen) == 0 ~ 0,
      s05_to_num(women_anc_seen) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    derived_anc_skilled = case_when(
      u_recent_birth != 1 ~ NA_real_,
      s05_to_num(women_anc_seen) == 0 ~ 0,
      s05_to_num(women_anc_seen) %in% c(8, 9) ~ NA_real_,
      s05_to_num(women_anc_seen) == 1 &
        (s05_to_num(anc_prov_doc) == 1 | s05_to_num(anc_prov_nurse) == 1 | s05_to_num(anc_prov_aux) == 1) ~ 1,
      s05_to_num(women_anc_seen) == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    anc_visits_num = s05_to_num(women_anc_total_visits),
    derived_anc_4plus = case_when(
      u_recent_birth != 1 ~ NA_real_,
      s05_to_num(women_anc_seen) == 0 ~ 0,
      s05_to_num(women_anc_seen) %in% c(8, 9) ~ NA_real_,
      anc_visits_num %in% c(98, 99) ~ NA_real_,
      anc_visits_num >= 4 ~ 1,
      TRUE ~ 0
    ),
    
    anc_first_month_num = s05_to_num(women_anc_first_visit_month),
    derived_anc_first_trimester = case_when(
      u_recent_birth != 1 ~ NA_real_,
      anc_first_month_num %in% c(98, 99) ~ NA_real_,
      anc_first_month_num >= 0 & anc_first_month_num <= 3 ~ 1,
      anc_first_month_num > 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    sp_taken_num = s05_to_num(women_took_sp_fansidar),
    sp_doses_num = s05_to_num(women_sp_fansidar_doses),
    
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
    
    preg_month_num = s05_to_num(women_pregnancy_duration_months),
    derived_currently_pregnant = case_when(
      s05_to_num(women_currently_pregnant) == 1 ~ 1,
      s05_to_num(women_currently_pregnant) == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    derived_pregnancy_first_trimester = case_when(
      derived_currently_pregnant != 1 ~ NA_real_,
      preg_month_num %in% c(98, 99) ~ NA_real_,
      preg_month_num >= 0 & preg_month_num <= 3 ~ 1,
      preg_month_num > 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    derived_child_fever = case_when(
      u_child_youngest != 1 ~ NA_real_,
      s05_to_num(women_child_fever_2weeks) == 1 ~ 1,
      s05_to_num(women_child_fever_2weeks) == 0 ~ 0,
      s05_to_num(women_child_fever_2weeks) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    derived_fever_seek_advice = case_when(
      u_child_fever != 1 ~ NA_real_,
      s05_to_num(women_child_seek_advice) == 1 ~ 1,
      s05_to_num(women_child_seek_advice) == 0 ~ 0,
      s05_to_num(women_child_seek_advice) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    delay_num = s05_to_num(women_child_advice_delay_days),
    derived_fever_prompt_care = case_when(
      u_child_fever != 1 ~ NA_real_,
      s05_to_num(women_child_seek_advice) != 1 ~ 0,
      delay_num %in% c(8, 9) ~ NA_real_,
      is.na(delay_num) ~ NA_real_,
      delay_num <= 1 ~ 1,
      TRUE ~ 0
    ),
    
    derived_fever_tested = case_when(
      u_child_fever != 1 ~ NA_real_,
      s05_to_num(women_child_blood_sample) == 1 ~ 1,
      s05_to_num(women_child_blood_sample) == 0 ~ 0,
      s05_to_num(women_child_blood_sample) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    derived_fever_took_any_medicine = case_when(
      u_child_fever != 1 ~ NA_real_,
      s05_to_num(women_child_took_medicine) == 1 ~ 1,
      s05_to_num(women_child_took_medicine) == 0 ~ 0,
      s05_to_num(women_child_took_medicine) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    derived_fever_took_anti = case_when(
      u_child_fever != 1 ~ NA_real_,
      s05_to_num(women_child_took_medicine) != 1 ~ 0,
      (s05_to_num(med_act) == 1 | s05_to_num(med_sp) == 1 | s05_to_num(med_chloro) == 1 |
         s05_to_num(med_amod) == 1 | s05_to_num(med_artesun) == 1 | s05_to_num(med_quinine) == 1 |
         s05_to_num(med_inject) == 1) ~ 1,
      TRUE ~ 0
    ),
    
    derived_fever_took_act    = case_when(u_child_fever != 1 ~ NA_real_, s05_to_num(women_child_took_medicine) != 1 ~ 0, s05_to_num(med_act) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_sp     = case_when(u_child_fever != 1 ~ NA_real_, s05_to_num(women_child_took_medicine) != 1 ~ 0, s05_to_num(med_sp) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_chloro = case_when(u_child_fever != 1 ~ NA_real_, s05_to_num(women_child_took_medicine) != 1 ~ 0, s05_to_num(med_chloro) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_amod   = case_when(u_child_fever != 1 ~ NA_real_, s05_to_num(women_child_took_medicine) != 1 ~ 0, s05_to_num(med_amod) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_quin   = case_when(u_child_fever != 1 ~ NA_real_, s05_to_num(women_child_took_medicine) != 1 ~ 0, s05_to_num(med_quinine) == 1 ~ 1, TRUE ~ 0),
    derived_fever_took_artes  = case_when(u_child_fever != 1 ~ NA_real_, s05_to_num(women_child_took_medicine) != 1 ~ 0, s05_to_num(med_artesun) == 1 ~ 1, TRUE ~ 0),
    
    act_delay_num = s05_to_num(women_child_act_delay),
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
      s05_to_num(women_child_act_effective) == 1 ~ 1,
      s05_to_num(women_child_act_effective) == 0 ~ 0,
      s05_to_num(women_child_act_effective) %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    afford_num = s05_to_num(treat_drug_affordability),
    derived_percep_affordable = case_when(
      afford_num %in% c(1, 2) ~ 1,
      afford_num %in% c(3, 4, 5) ~ 0,
      afford_num %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    gov_num = s05_to_num(feedback_gov_effort_rating),
    derived_gov_effective = case_when(
      gov_num %in% c(4, 5) ~ 1,
      gov_num %in% c(1, 2, 3) ~ 0,
      gov_num == 9 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    stockout_num = s05_to_num(feedback_drug_stockout_6months),
    free_num     = s05_to_num(feedback_free_treatment_6months),
    
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
    
    test_cost_num = s05_to_num(treat_test_cost),
    drug_cost_num = s05_to_num(treat_drug_cost),
    drug_buy_time_num = s05_to_num(treat_drug_purchase_time),
    
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


# ------------------------------------------------------------------------------
# 9. Prepare two export objects
# ------------------------------------------------------------------------------
message("Step 7: Preparing export objects...")

s05_df_export_r <- s05_df_analysis
s05_df_export_sav <- s05_df_analysis

# ------------------------------------------------------------------------------
# 10. Apply labels ONLY to SAV export object
# ------------------------------------------------------------------------------
message("Step 8: Applying labels to SAV export object...")

s05_present_lbls <- s05_variable_labels[names(s05_variable_labels) %in% names(s05_df_export_sav)]
s05_missing_labels <- setdiff(names(s05_df_export_sav), names(s05_present_lbls))

if (length(s05_missing_labels) > 0) {
  message("The following columns are missing labels: ", paste(s05_missing_labels, collapse = ", "))
} else {
  message("All variable labels are present.")
}

s05_df_export_sav <- labelled::set_variable_labels(s05_df_export_sav, .labels = s05_present_lbls)

if ("derived_zone" %in% names(s05_df_export_sav)) {
  val_labels(s05_df_export_sav$derived_zone) <- s05_value_labels$zone
}
if ("derived_residence" %in% names(s05_df_export_sav)) {
  val_labels(s05_df_export_sav$derived_residence) <- s05_value_labels$derived_residence
}
if ("derived_wealth_quintile" %in% names(s05_df_export_sav)) {
  val_labels(s05_df_export_sav$derived_wealth_quintile) <- s05_value_labels$wealth_quintile
}
if ("derived_edu_cat" %in% names(s05_df_export_sav)) {
  val_labels(s05_df_export_sav$derived_edu_cat) <- s05_value_labels$edu4
}
if ("derived_age_group_w" %in% names(s05_df_export_sav)) {
  val_labels(s05_df_export_sav$derived_age_group_w) <- s05_value_labels$age_group_women
}

s05_yn_vars <- c(
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

for (v in s05_yn_vars) {
  if (v %in% names(s05_df_export_sav)) {
    val_labels(s05_df_export_sav[[v]]) <- s05_value_labels$yesno_simple
  }
}

s05_pca_yn_vars <- c(
  "pca_floor_improved",
  "pca_roof_improved",
  "pca_wall_improved",
  "pca_water_improved",
  "pca_sanitation_improved",
  "pca_toilet_not_shared",
  "pca_clean_cooking",
  "pca_low_crowding",
  "pca_complete_case"
)

for (v in s05_pca_yn_vars) {
  if (v %in% names(s05_df_export_sav)) {
    val_labels(s05_df_export_sav[[v]]) <- s05_value_labels$yesno_simple
  }
}

if ("derived_age_group_4" %in% names(s05_df_export_sav)) {
  val_labels(s05_df_export_sav$derived_age_group_4) <- s05_value_labels$age_group_4
}
if ("age_group_4" %in% names(s05_df_export_sav)) {
  val_labels(s05_df_export_sav$age_group_4) <- s05_value_labels$age_group_4
}

s05_topcode_vars <- c(
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

for (v in s05_topcode_vars) {
  if (v %in% names(s05_df_export_sav)) {
    val_labels(s05_df_export_sav[[v]]) <- s05_value_labels$topcode_flag
  }
}

# ------------------------------------------------------------------------------
# 11. Generate QC summary from unlabelled R object
# ------------------------------------------------------------------------------
message("Step 9: Generating QC summary...")

s05_qc <- tibble(
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
    nrow(s05_df_export_r),
    sum(s05_df_export_r$u_women_15_49 == 1, na.rm = TRUE),
    sum(s05_df_export_r$u_recent_birth == 1, na.rm = TRUE),
    sum(s05_df_export_r$u_hh_has_u5 == 1, na.rm = TRUE),
    sum(s05_df_export_r$u_child_fever == 1, na.rm = TRUE),
    sum(s05_df_export_r$u_recent_fever_hh == 1, na.rm = TRUE),
    sum(!is.na(s05_df_export_r$derived_wealth_quintile), na.rm = TRUE)
  )
) %>%
  mutate(Value = as.character(Value))

s05_key_inds <- c(
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

s05_miss_df <- tibble(
  Indicator = s05_key_inds,
  Missing_N = sapply(s05_key_inds, function(v) sum(is.na(s05_df_export_r[[v]]), na.rm = TRUE)),
  NonMissing_N = sapply(s05_key_inds, function(v) sum(!is.na(s05_df_export_r[[v]]), na.rm = TRUE))
)

s05_qc_out <- bind_rows(
  s05_qc %>% mutate(Section = "Universe counts") %>% select(Section, Metric, Value),
  s05_miss_df %>% transmute(
    Section = "Indicator missingness",
    Metric  = Indicator,
    Value   = paste0("Missing=", Missing_N, "; NonMissing=", NonMissing_N)
  )
)

s05_out_dir <- dirname(S05_OUTPUT_QC)
if (!dir.exists(s05_out_dir)) dir.create(s05_out_dir, recursive = TRUE)

write.csv(s05_qc_out, S05_OUTPUT_QC, row.names = FALSE)

# ------------------------------------------------------------------------------
# 12. Export datasets
# ------------------------------------------------------------------------------
message("Step 10: Exporting datasets...")

saveRDS(s05_df_export_r, S05_OUTPUT_RDS)
write.csv(s05_df_export_r, S05_OUTPUT_CSV, row.names = FALSE)

write_sav(s05_df_export_sav, S05_OUTPUT_SAV)

s05_df_shareable <- s05_df_export_r %>%
  select(-c(meta_latitude, meta_longitude))

write.csv(s05_df_shareable, "Sproxil_Shareable.csv", row.names = FALSE)

message("✅ DONE: Datasets saved:")
message(" - ", S05_OUTPUT_RDS, " [unlabelled R-native]")
message(" - ", S05_OUTPUT_SAV, " [labelled for SPSS]")
message(" - ", S05_OUTPUT_CSV, " [unlabelled CSV]")
message(" - Sproxil_Shareable.csv [unlabelled CSV]")
