# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  04_Final_Recoding_Master.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    09 December 2025
# DESCRIPTION: 
# Recodes ALL variables from Text/Factor to Numeric for analysis.
# Strictly follows DHS MIS 2021 Model Questionnaire coding for categories.
# Un-lumped values for Water, Toilet, Education, and Likert scales.
# ==============================================================================

# --- 1. SETUP ---
library(tidyverse)
library(haven)
library(labelled)
library(stringr)

# Input/Output
INPUT_FILE <- "Sproxil_Resolved.rds"
OUT_SPSS   <- "Sproxil_Recoded.sav"
OUT_RDS    <- "Sproxil_Recoded.rds"
OUT_CSV    <- "Sproxil_Recoded.csv"

if(!file.exists(INPUT_FILE)) stop(paste("Input file not found:", INPUT_FILE))
df <- readRDS(INPUT_FILE)

library(tidyverse)

# Get unique values for all columns, excluding ID
unique_vals_list <- df %>%
  # 1. Remove the ID variable
  select(-matches("meta_respondent_id")) %>%
  # 2. Keep only Text (Character) or Factor columns
  select(where(is.character) | where(is.factor)) %>%
  # 3. Extract unique values and sort them alphabetically
  map(~ sort(unique(.)))

# Print the list to the console
print(unique_vals_list)


# --- 2. DUMMIFICATION (MULTI-SELECT VARIABLES) ---
# Splitting text strings into binary 1/0 columns
message("Step 1: Creating Dummy Variables...")

# Helper to protect NAs during dummification
dummify <- function(column, pattern) {
  case_when(
    is.na(column) ~ NA_real_,
    str_detect(column, pattern) ~ 1,
    TRUE ~ 0
  )
}

df <- df %>%
  mutate(
    # 1. Informal Education
    edu_inf_none     = dummify(demo_edu_informal, "NEVER|NONE"),
    edu_inf_adult    = dummify(demo_edu_informal, "ADULT"),
    edu_inf_tsangaya = dummify(demo_edu_informal, "TSANGAYA"),
    edu_inf_quranic  = dummify(demo_edu_informal, "QUARANIC"),
    edu_inf_other    = dummify(demo_edu_informal, "OTHER"),
    
    # 2. Repellent Methods
    repel_coils    = dummify(prev_repellent_methods, "COILS"),
    repel_spray    = dummify(prev_repellent_methods, "SPRAYS|INSECTICIDES"),
    repel_cream    = dummify(prev_repellent_methods, "CREAMS"),
    repel_electric = dummify(prev_repellent_methods, "ELECTRONIC"),
    repel_none     = dummify(prev_repellent_methods, "NONE"),
    
    # 3. ANC Provider
    anc_prov_doc   = dummify(women_anc_provider, "DOCTOR"),
    anc_prov_nurse = dummify(women_anc_provider, "NURSE|MIDWIFE"),
    anc_prov_aux   = dummify(women_anc_provider, "AUXILIARY"),
    anc_prov_chew  = dummify(women_anc_provider, "COMMUNITY EXTENSION"),
    anc_prov_tba   = dummify(women_anc_provider, "TRADITIONAL"),
    anc_prov_field = dummify(women_anc_provider, "COMMUNITY HEALTH"),
    anc_prov_other = dummify(women_anc_provider, "OTHERS"),
    
    # 4. ANC Location
    anc_loc_govhos  = dummify(women_anc_location, "GOVERNMENT HOSPITAL"),
    anc_loc_govcen  = dummify(women_anc_location, "GOVERNMENT HEALTH CENTER"),
    anc_loc_govpos  = dummify(women_anc_location, "GOVERNMENT HEALTH POST"),
    anc_loc_pvt  = dummify(women_anc_location, "PRIVATE"),
    anc_loc_ngohos  = dummify(women_anc_location, "NGO HOSPITAL"),
    anc_loc_ngoclin  = dummify(women_anc_location, "NGO CLINIC"),
    anc_loc_herhome = dummify(women_anc_location, "HER HOME"),
    anc_loc_yourhome = dummify(women_anc_location, "YOUR HOME"),
    anc_loc_other = dummify(women_anc_location, "OTHERS"),
    
    # 5. SP/Fansidar Source
    sp_src_anc   = dummify(women_sp_fansidar_source, "ANTENATAL VISIT"),
    sp_src_nonanc = dummify(women_sp_fansidar_source, "NON ANTENATAL"),
    sp_src_pharm = dummify(women_sp_fansidar_source, "PHARMACY|CHEMIST"),
    sp_src_chew  = dummify(women_sp_fansidar_source, "COMMUNITY HEALTH"),
    sp_src_other  = dummify(women_sp_fansidar_source, "OTHERS"),
    
    # 6. Child Treatment Advice Location
    child_adv_gov   = dummify(women_child_advice_location, "GOVERNMENT"),
    child_adv_pvth  = dummify(women_child_advice_location, "PRIVATE HOSPITAL"),
    child_adv_ngo   = dummify(women_child_advice_location, "NGO"),
    child_adv_mob   = dummify(women_child_advice_location, "MOBILE"),
    child_adv_pvtd  = dummify(women_child_advice_location, "PRIVATE DOCTOR"),
    child_adv_com   = dummify(women_child_advice_location, "COMMUNITY"),
    child_adv_pharm = dummify(women_child_advice_location, "PHARMACY"),
    child_adv_chem  = dummify(women_child_advice_location, "CHEMIST"),
    child_adv_trad  = dummify(women_child_advice_location, "TRADITIONAL"),
    child_adv_rel   = dummify(women_child_advice_location, "RELIGIOUS"),
    
    # 7. Child Medicine Type
    med_act     = dummify(women_child_medicine_type, "ARTEMISININ|ACT"),
    med_sp      = dummify(women_child_medicine_type, "SP/FANSIDAR"),
    med_chloro  = dummify(women_child_medicine_type, "CHLOROQUINE"),
    med_amod    = dummify(women_child_medicine_type, "AMODIAQUINE"),
    med_artesun = dummify(women_child_medicine_type, "ARTESUNATE"),
    med_quinine = dummify(women_child_medicine_type, "QUININE"),
    med_inject  = dummify(women_child_medicine_type, "INJECTION"),
    med_other   = dummify(women_child_medicine_type, "OTHERS"),
    
    # 8. Malaria Message Source
    msg_radio  = dummify(bg_malaria_msg_source, "RADIO"),
    msg_tv     = dummify(bg_malaria_msg_source, "TELEVISION"),
    msg_poster = dummify(bg_malaria_msg_source, "POSTER|BILLBOARD"),
    msg_news   = dummify(bg_malaria_msg_source, "NEWSPAPER"),
    msg_leaf   = dummify(bg_malaria_msg_source, "LEAFLET"),
    msg_hcp    = dummify(bg_malaria_msg_source, "HEALTHCARE"),
    msg_chw    = dummify(bg_malaria_msg_source, "COMMUNITY HEALTH"),
    msg_social = dummify(bg_malaria_msg_source, "SOCIAL MEDIA"),
    msg_town   = dummify(bg_malaria_msg_source, "TOWN"),
    msg_ipc    = dummify(bg_malaria_msg_source, "INTER-PERSONAL"),
    msg_family = dummify(bg_malaria_msg_source, "FAMILY|FRIENDS"),
    
    # 9. Prevention Knowledge
    know_net     = dummify(bg_prevention_knowledge, "SLEEP INSIDE A MOSQUITO NET"),
    know_itn     = dummify(bg_prevention_knowledge, "INSECTICIDE-TREATED"),
    know_repel   = dummify(bg_prevention_knowledge, "REPELLANT"),
    know_stag    = dummify(bg_prevention_knowledge, "STAGNANT"),
    know_spray   = dummify(bg_prevention_knowledge, "SPRAY"),
    know_meds    = dummify(bg_prevention_knowledge, "MEDICATIONS"),
    know_clean   = dummify(bg_prevention_knowledge, "CLEAN"),
    know_screens = dummify(bg_prevention_knowledge, "SCREEN"),
    know_other   = dummify(bg_prevention_knowledge, "OTHER"),
    know_dont    = dummify(bg_prevention_knowledge, "DON'T KNOW")
  )

# --- 3. RECODING (TEXT TO NUMERIC) ---
message("Step 2: Recoding Variables...")

df_recoded <- df %>%
  mutate(
    # --- A. BINARY VARIABLES (1=Yes, 0=No, 8=DK) ---
    across(c(
      prev_has_mosquito_nets, prev_home_sprayed_interior,
      treat_hh_fever_last_2weeks, treat_blood_sample_taken, treat_heard_smc,
      women_ever_given_birth, women_anc_seen, women_took_sp_fansidar,
      women_child_fever_2weeks, women_child_blood_sample, women_child_malaria_diagnosis,
      women_child_seek_advice, women_child_referral, women_child_took_medicine,
      women_child_act_effective, women_currently_pregnant,
      bg_own_smartphone, bg_internet_ever_used,
      bg_heard_malaria_msg_6months, bg_aware_avoidance, 
      hh_toilet_shared, hh_owns_livestock, hh_owns_agri_land,
      # Assets
      hh_has_electricity, hh_has_radio, hh_has_tv, hh_has_non_mobile_phone,
      hh_has_computer, hh_has_refrigerator, hh_has_table, hh_has_chair,
      hh_has_bed, hh_has_sofa, hh_has_cupboard, hh_has_ac, hh_has_electric_iron,
      hh_has_generator, hh_has_fan, hh_own_watch, hh_own_mobile_phone,
      hh_own_bicycle, hh_own_motorcycle, hh_own_animal_cart, hh_own_car_truck,
      hh_own_motor_boat, hh_own_canoe, hh_own_keke_napep, hh_has_bank_account,
      hh_mobile_money_usage
    ), ~case_when(
      is.na(.) ~ NA_real_,
      . == "YES" ~ 1,
      . == "NO" ~ 0,
      str_detect(., "DON'T KNOW|NOT SURE") ~ 8,
      TRUE ~ 9
    )),
    
    # --- B. DEMOGRAPHICS ---
    demo_gender = case_when(
      is.na(demo_gender) ~ NA_real_,
      demo_gender == "MALE" ~ 1, 
      demo_gender == "FEMALE" ~ 2, 
      TRUE ~ 9
    ),
    
    demo_edu_level = case_when(
      is.na(demo_edu_level) ~ NA_real_,
      str_detect(demo_edu_level, "NEVER") ~ 0,
      str_detect(demo_edu_level, "PRIMARY") ~ 1,
      str_detect(demo_edu_level, "SECONDARY") ~ 2,
      str_detect(demo_edu_level, "HIGHER") ~ 3,
      TRUE ~ 9
    ),
    
    # --- C. NETS ---
    prev_net_brand = case_when(
      is.na(prev_net_brand) ~ NA_real_,
      prev_net_brand == "LLIN" ~ 10, prev_net_brand == "OLYSET LLIN" ~ 11, prev_net_brand == "ICONLIFE LLIN" ~ 12, 
      prev_net_brand == "DURANET LLIN" ~ 13,
      prev_net_brand == "NETPROTECT LLIN" ~ 14, prev_net_brand == "BASF INTERCEPTOR LLIN" ~ 15, prev_net_brand == "YORKOOL LLIN" ~ 16,
      prev_net_brand == "MAGNET LLIN" ~ 17, prev_net_brand == "DAWAPLUS 2.0 LLIN" ~ 18, prev_net_brand == "ROYAL SECURITY LLIN" ~ 19,
      prev_net_brand == "ROYAL SENTRY LLIN" ~ 20, prev_net_brand == "PERMANET 2.0 LLIN" ~ 21, prev_net_brand == "PERMANET 3.0 LLIN" ~ 22,
      prev_net_brand == "VEERALIN LLIN" ~ 23, prev_net_brand == "INTERCEPTOR G2 LLIN" ~ 24, prev_net_brand == "ROYAL GUARD LLIN" ~ 25,
      prev_net_brand == "OTHER/DON'T KNOW BRAND BUT (LLIN)" ~ 26, 
      prev_net_brand == "OTHER TYPE (NOT LLIN)" ~ 95, 
      prev_net_brand == "OTHER" ~ 96,
      str_detect(prev_net_brand, "DON'T KNOW") ~ 98, 
      TRUE ~ 99
    ),
    
    prev_net_obtained_how = case_when(
      is.na(prev_net_obtained_how) ~ NA_real_,
      str_detect(prev_net_obtained_how, "MASS") ~ 1,
      str_detect(prev_net_obtained_how, "ANTE NATAL") ~ 2,
      str_detect(prev_net_obtained_how, "IMMUNIZATION") ~ 3,
      str_detect(prev_net_obtained_how, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    prev_net_obtained_where = case_when(
      is.na(prev_net_obtained_where) ~ NA_real_,
      str_detect(prev_net_obtained_where, "GOVERNMENT") ~ 1,
      str_detect(prev_net_obtained_where, "PRIVATE") ~ 2,
      str_detect(prev_net_obtained_where, "PHARMACY") ~ 3,
      str_detect(prev_net_obtained_where, "SHOP|MARKET") ~ 4,
      str_detect(prev_net_obtained_where, "CHW") ~ 5,
      str_detect(prev_net_obtained_where, "RELIGIOUS") ~ 6,
      str_detect(prev_net_obtained_where, "SCHOOL") ~ 7,
      str_detect(prev_net_obtained_where, "OTHER") ~ 96,
      str_detect(prev_net_obtained_where, "DON'T KNOW") ~ 98,
      TRUE ~ 99
    ),
    
    # --- D. TREATMENT & LOCATIONS ---
    prev_first_treatment_location = case_when(    
      is.na(prev_first_treatment_location) ~ NA_real_,
      str_detect(prev_first_treatment_location, "GOVERNMENT") ~ 1,
      str_detect(prev_first_treatment_location, "PRIVATE HOSPITAL") ~ 2,
      str_detect(prev_first_treatment_location, "NGO") ~ 3,
      str_detect(prev_first_treatment_location, "MOBILE") ~ 4,
      str_detect(prev_first_treatment_location, "PRIVATE DOCTOR") ~ 5,
      str_detect(prev_first_treatment_location, "COMMUNITY") ~ 6,
      str_detect(prev_first_treatment_location, "PHARMACY") ~ 7,
      str_detect(prev_first_treatment_location, "CHEMIST") ~ 8,
      str_detect(prev_first_treatment_location, "TRADITIONAL") ~ 10,
      str_detect(prev_first_treatment_location, "RELIGIOUS") ~ 11,
      TRUE ~ 99
    ),
    
    prev_time_to_treatment_facility = case_when(
      is.na(prev_time_to_treatment_facility) ~ NA_real_,
      str_detect(prev_time_to_treatment_facility, "LESS") ~ 1,
      str_detect(prev_time_to_treatment_facility, "30 MINS") ~ 2,
      str_detect(prev_time_to_treatment_facility, "MORE") ~ 3,
      str_detect(prev_time_to_treatment_facility, "DON'T KNOW") ~ 8,
      TRUE ~ 9
    ),
    
    # --- E. COSTS ---
    across(c(treat_test_cost, treat_drug_cost), ~ case_when(
      is.na(.) ~ NA_real_,
      str_detect(., "FREE") ~ 0,
      str_detect(., "N1 - N999") ~ 1,
      str_detect(., "N1,000 - N1,999") ~ 2,
      str_detect(., "N2,000 - N2,999") ~ 3,
      str_detect(., "N3,000 - N3,999") ~ 4,
      str_detect(., "N4,000 - N4,999") ~ 5,
      str_detect(., "N5,000 - N5,999") ~ 6,
      str_detect(., "N6,000 - N6,999") ~ 7,
      str_detect(., "N7,000 - N7,999") ~ 8,
      str_detect(., "N8,000 - N8,999") ~ 9,
      str_detect(., "N9,000 - N9,999") ~ 10,
      str_detect(., "N10,000 - N10,999") ~ 11,
      str_detect(., "N11,000 - N11,999") ~ 12,
      str_detect(., "N12,000 - N12,999") ~ 13,
      str_detect(., "N13,000 - N13,999") ~ 14,
      str_detect(., "N14,000 - N14,999") ~ 15,
      str_detect(., "ABOVE N15,000") ~ 16,
      str_detect(., "DON'T KNOW|DONT KNOW|DK") ~ 98,
      TRUE ~ 99
    )),
    
    treat_transport_cost = case_when(
      is.na(treat_transport_cost) ~ NA_real_,
      str_detect(treat_transport_cost, "FREE") ~ 0,
      str_detect(treat_transport_cost, "N1 - N999") ~ 1,
      str_detect(treat_transport_cost, "N1,000 - N1,999") ~ 2,
      str_detect(treat_transport_cost, "N2,000 - N2,999") ~ 3,
      str_detect(treat_transport_cost, "N3,000 - N3,999") ~ 4,
      str_detect(treat_transport_cost, "N4,000 - N4,999") ~ 5,
      str_detect(treat_transport_cost, "N5,000 - N5,999") ~ 6,
      str_detect(treat_transport_cost, "N6,000 - N6,999") ~ 7,
      str_detect(treat_transport_cost, "N7,000 - N7,999") ~ 8,
      str_detect(treat_transport_cost, "N8,000 - N8,999") ~ 9,
      str_detect(treat_transport_cost, "N9,000 - N9,999") ~ 10,
      str_detect(treat_transport_cost, "ABOVE N10,000") ~ 11,
      str_detect(treat_transport_cost, "DON'T KNOW|DONT KNOW|DK") ~ 98,
      TRUE ~ 99
    ),
    
    treat_drug_purchase_time = case_when(
      is.na(treat_drug_purchase_time) ~ NA_real_,
      str_detect(treat_drug_purchase_time, "0 ?[-–] ?4 WEEK|0[-–]4 WEEK") ~ 1,   
      str_detect(treat_drug_purchase_time, "1 ?[-–] ?5 MONTH|5 MONTH") ~ 2,   
      str_detect(treat_drug_purchase_time, "6 ?[-–] ?12 MONTH|6[-–]12 MONTH") ~ 3, 
      str_detect(treat_drug_purchase_time, "OVER ONE YEAR|> ?1 YEAR") ~ 4, 
      str_detect(treat_drug_purchase_time, "DON'?T KNOW|DONT KNOW|DK") ~ 8,       
      TRUE ~ 9 
    ),
    
    treat_drug_affordability = case_when(
      is.na(treat_drug_affordability) ~ NA_real_,
      str_detect(treat_drug_affordability, "VERY AFFORDABLE") ~ 1,
      str_detect(treat_drug_affordability, "SOMEWHAT AFFORDABLE") ~ 2,
      str_detect(treat_drug_affordability, "NEUTRAL") ~ 3,
      str_detect(treat_drug_affordability, "SOMEWHAT EXPENSIVE") ~ 4,
      str_detect(treat_drug_affordability, "VERY EXPENSIVE") ~ 5, 
      str_detect(treat_drug_affordability, "DON'T KNOW") ~ 8,
      TRUE ~ 9
    ),
    
    # --- F. SMC & VACCINE ---
    treat_children_received_smc = case_when(
      is.na(treat_children_received_smc) ~ NA_real_,
      treat_children_received_smc == "YES" ~ 1,
      str_detect(treat_children_received_smc, "NO AND I HAVE A CHILD") ~ 0,
      str_detect(treat_children_received_smc, "NO, I DO NOT HAVE A CHILD") ~ 6, 
      TRUE ~ 9
    ),
    
    treat_children_received_vaccine = case_when(
      is.na(treat_children_received_vaccine) ~ NA_real_,
      treat_children_received_vaccine == "YES" ~ 1,
      str_detect(treat_children_received_vaccine, "NO AND I HAVE A CHILD") ~ 0,
      str_detect(treat_children_received_vaccine, "NO, I DO NOT HAVE A CHILD") ~ 6, 
      TRUE ~ 9
    ),
    
    treat_know_smc_drug = case_when(
      is.na(treat_know_smc_drug) ~ NA_real_,
      str_detect(treat_know_smc_drug, "SULFADOXINE-PYRIMETHAMINE \\+ AMODIAQUINE") ~ 1,
      str_detect(treat_know_smc_drug, "SULFADOXINE-PYRIMETHAMIN") ~ 2,
      str_detect(treat_know_smc_drug, "ARTEMETHER-LUMEFANTRINE") ~ 3,
      str_detect(treat_know_smc_drug, "ARTESUNATE \\+ AMODIAQUINE") ~ 4,
      str_detect(treat_know_smc_drug, "DIHYDROARTEMISININ-PIPERAQUINE") ~ 5,
      str_detect(treat_know_smc_drug, "PROGUANIL") ~ 6,
      str_detect(treat_know_smc_drug, "OTHERS") ~ 96,
      str_detect(treat_know_smc_drug, "DON'T KNOW") ~ 98,
      TRUE ~ 99
    ),
    
    treat_vaccine_age_knowledge = case_when(
      is.na(treat_vaccine_age_knowledge) ~ NA_real_,
      treat_vaccine_age_knowledge == "LESS THAN 12 MONTHS" ~ 1,
      treat_vaccine_age_knowledge == "1 YEAR" ~ 2, treat_vaccine_age_knowledge == "2 YEARS" ~ 3, 
      treat_vaccine_age_knowledge == "3 YEARS" ~ 4, treat_vaccine_age_knowledge == "4 YEARS" ~ 5, 
      treat_vaccine_age_knowledge == "5 YEARS" ~ 6, treat_vaccine_age_knowledge == "ABOVE 5 YEARS" ~ 7,
      str_detect(treat_vaccine_age_knowledge, "DON'T KNOW") ~ 98,
      TRUE ~ 99
    ),
    
    # --- G. FEEDBACK ---
    across(c(feedback_free_treatment_6months, feedback_drug_stockout_6months), ~case_when(
      is.na(.) ~ NA_real_,
      . == "YES" ~ 1,
      str_detect(., "NO AND I WENT") ~ 0,
      str_detect(., "DID NOT GO") ~ 6,
      TRUE ~ 9
    )),
    
    feedback_gov_effort_rating = case_when(
      is.na(feedback_gov_effort_rating) ~ NA_real_,
      str_detect(feedback_gov_effort_rating, "VERY EFFECTIVE") ~ 5,
      str_detect(feedback_gov_effort_rating, "SOMEWHAT EFFECTIVE") ~ 4,
      str_detect(feedback_gov_effort_rating, "NEUTRAL") ~ 3,
      str_detect(feedback_gov_effort_rating, "SOMEWHAT INEFFECTIVE") ~ 2,
      str_detect(feedback_gov_effort_rating, "VERY INEFFECTIVE") ~ 1,
      TRUE ~ 9
    ),
    
    # --- H. WOMEN'S TIMINGS ---
    women_child_first_advice_location = case_when(
      is.na(women_child_first_advice_location) ~ NA_real_,
      str_detect(women_child_first_advice_location, "GOVERNMENT") ~ 1,
      str_detect(women_child_first_advice_location, "PRIVATE HOSPITAL") ~ 2,
      str_detect(women_child_first_advice_location, "NGO") ~ 3,
      str_detect(women_child_first_advice_location, "MOBILE") ~ 4,
      str_detect(women_child_first_advice_location, "PRIVATE DOCTOR") ~ 5,
      str_detect(women_child_first_advice_location, "COMMUNITY") ~ 6,
      str_detect(women_child_first_advice_location, "PHARMACY") ~ 7,
      str_detect(women_child_first_advice_location, "CHEMIST") ~ 8,
      str_detect(women_child_first_advice_location, "TRADITIONAL") ~ 10,
      str_detect(women_child_first_advice_location, "RELIGIOUS") ~ 11,
      TRUE ~ 99
    ),
    
    women_child_act_delay = case_when(
      is.na(women_child_act_delay) ~ NA_real_,
      str_detect(women_child_act_delay, "SAME DAY") ~ 0,
      str_detect(women_child_act_delay, "NEXT DAY") ~ 1,
      str_detect(women_child_act_delay, "TWO DAYS") ~ 2,
      str_detect(women_child_act_delay, "THREE OR MORE") ~ 3,
      str_detect(women_child_act_delay, "DON'T KNOW") ~ 8,
      TRUE ~ 9
    ),
    
    women_child_advice_delay_days = case_when(
      is.na(women_child_advice_delay_days) ~ NA_real_,
      str_detect(women_child_advice_delay_days, "SAME DAY") ~ 0,
      str_detect(women_child_advice_delay_days, "1 DAY") ~ 1,
      str_detect(women_child_advice_delay_days, "2 DAYS") ~ 2,
      str_detect(women_child_advice_delay_days, "3+ DAYS|OVER") ~ 3,
      TRUE ~ 9
    ),
    
    # --- I. BACKGROUND & ATTITUDES ---
    bg_tv_frequency = case_when(
      is.na(bg_tv_frequency) ~ NA_real_,
      str_detect(bg_tv_frequency, "AT LEAST ONCE") ~ 1,
      str_detect(bg_tv_frequency, "WITHOUT WATCHNG") ~ 2,
      str_detect(bg_tv_frequency, "NOT AT ALL") ~ 3,
      TRUE ~ 9
    ),
    
    bg_internet_frequency = case_when(
      is.na(bg_internet_frequency) ~ NA_real_,
      str_detect(bg_internet_frequency, "EVERY DAY") ~ 1,
      str_detect(bg_internet_frequency, "ONCE A WEEK") ~ 2,
      str_detect(bg_internet_frequency, "WITHOUT USING") ~ 3,
      TRUE ~ 9
    ),
    
    bg_religion = case_when(
      is.na(bg_religion) ~ NA_real_,
      str_detect(bg_religion, "CATHOLIC") ~ 1,
      str_detect(bg_religion, "CHRISTIAN") ~ 2,
      str_detect(bg_religion, "ISLAM") ~ 3,
      str_detect(bg_religion, "TRADITIONAL") ~ 4,
      str_detect(bg_religion, "OTHERS") ~ 6,
      TRUE ~ 9
    ),
    
    across(c(
      att_rainy_season_only, att_fever_worry_malaria, att_malaria_easily_treated,
      att_weak_children_die, att_net_use_mosquito_density, att_net_use_warm_weather,
      att_home_meds_first, att_full_dose_importance, att_seek_care_immediate,
      att_community_net_usage
    ), ~case_when(
      is.na(.) ~ NA_real_,
      str_detect(., "STRONGLY AGREE") ~ 1,
      str_detect(., "STRONGLY DISAGREE") ~ 5, 
      str_detect(., "AGREE") ~ 2,
      str_detect(., "NEUTRAL|NEITHER") ~ 3,
      str_detect(., "DISAGREE") ~ 4,
      str_detect(., "DON'T KNOW|UNCERTAIN") ~ 8,
      TRUE ~ 9
    )),
    
    # --- J. HOUSEHOLD METADATA ---
    hh_drinking_water_source = case_when(
      is.na(hh_drinking_water_source) ~ NA_real_,
      str_detect(hh_drinking_water_source, "PIPED INTO DWELLING|YARD") ~ 11,
      str_detect(hh_drinking_water_source, "PIPED TO NEIGHBOR") ~ 12,
      str_detect(hh_drinking_water_source, "PUBLIC TAP|STANDPIPE") ~ 13,
      str_detect(hh_drinking_water_source, "TUBE|BOREHOLE") ~ 21,
      str_detect(hh_drinking_water_source, "PROTECTED WELL") ~ 31,
      str_detect(hh_drinking_water_source, "UNPROTECTED WELL|DUG WELL") ~ 32,
      str_detect(hh_drinking_water_source, "PROTECTED SPRING") ~ 41,
      str_detect(hh_drinking_water_source, "UNPROTECTED SPRING") ~ 42,
      str_detect(hh_drinking_water_source, "RAIN") ~ 51,
      str_detect(hh_drinking_water_source, "TANKER") ~ 61,
      str_detect(hh_drinking_water_source, "CART") ~ 71,
      str_detect(hh_drinking_water_source, "SURFACE") ~ 81,
      str_detect(hh_drinking_water_source, "BOTTLED") ~ 91,
      str_detect(hh_drinking_water_source, "SACHET") ~ 92,
      str_detect(hh_drinking_water_source, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_other_water_source = case_when(
      is.na(hh_other_water_source) ~ NA_real_,
      str_detect(hh_other_water_source, "PIPED INTO DWELLING|YARD") ~ 11,
      str_detect(hh_other_water_source, "PIPED TO NEIGHBOR") ~ 12,
      str_detect(hh_other_water_source, "PUBLIC TAP|STANDPIPE") ~ 13,
      str_detect(hh_other_water_source, "TUBE|BOREHOLE") ~ 21,
      str_detect(hh_other_water_source, "PROTECTED WELL") ~ 31,
      str_detect(hh_other_water_source, "UNPROTECTED WELL|DUG WELL") ~ 32,
      str_detect(hh_other_water_source, "PROTECTED SPRING") ~ 41,
      str_detect(hh_other_water_source, "UNPROTECTED SPRING") ~ 42,
      str_detect(hh_other_water_source, "RAIN") ~ 51,
      str_detect(hh_other_water_source, "TANKER") ~ 61,
      str_detect(hh_other_water_source, "CART") ~ 71,
      str_detect(hh_other_water_source, "SURFACE") ~ 81,
      str_detect(hh_other_water_source, "BOTTLED") ~ 91,
      str_detect(hh_other_water_source, "SACHET") ~ 92,
      str_detect(hh_other_water_source, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_toilet_type = case_when(
      is.na(hh_toilet_type) ~ NA_real_,
      str_detect(hh_toilet_type, "FLUSH TO PIPED") ~ 11,
      str_detect(hh_toilet_type, "FLUSH TO SEPTIC") ~ 12,
      str_detect(hh_toilet_type, "FLUSH TO PIT") ~ 13,
      str_detect(hh_toilet_type, "FLUSH TO SOMEWHERE") ~ 14,
      str_detect(hh_toilet_type, "FLUSH DON'T KNOW") ~ 15, 
      str_detect(hh_toilet_type, "VENTILATED") ~ 21,
      str_detect(hh_toilet_type, "PIT LATRINE WITH SLAB") ~ 22,
      str_detect(hh_toilet_type, "PIT LATRINE WITHOUT") ~ 23, 
      str_detect(hh_toilet_type, "COMPOSTING") ~ 31,
      str_detect(hh_toilet_type, "BUCKET") ~ 41,
      str_detect(hh_toilet_type, "HANGING") ~ 51,
      str_detect(hh_toilet_type, "NO FACILITY|BUSH") ~ 61,
      str_detect(hh_toilet_type, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_cookstove_fuel = case_when(
      is.na(hh_cookstove_fuel) ~ NA_real_,
      str_detect(hh_cookstove_fuel, "ALCOHOL") ~ 01,
      str_detect(hh_cookstove_fuel, "GASOLINE") ~ 02,
      str_detect(hh_cookstove_fuel, "KEROSENE") ~ 03,
      str_detect(hh_cookstove_fuel, "COAL") ~ 04,
      str_detect(hh_cookstove_fuel, "CHARCOAL") ~ 05,
      str_detect(hh_cookstove_fuel, "WOOD") ~ 06,
      str_detect(hh_cookstove_fuel, "STRAW") ~ 07,
      str_detect(hh_cookstove_fuel, "CROP") ~ 08,
      str_detect(hh_cookstove_fuel, "DUNG") ~ 09,
      str_detect(hh_cookstove_fuel, "BIOMASS") ~ 10,
      str_detect(hh_cookstove_fuel, "GARBAGE") ~ 11,
      str_detect(hh_cookstove_fuel, "SAWDUST") ~ 12,
      str_detect(hh_cookstove_fuel, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_cookstove_type = case_when(
      is.na(hh_cookstove_type) ~ NA_real_,
      str_detect(hh_cookstove_type, "ELECTRIC") ~ 1,
      str_detect(hh_cookstove_type, "SOLAR") ~ 2,
      str_detect(hh_cookstove_type, "COOKING GAS|LPG") ~ 3,
      str_detect(hh_cookstove_type, "PIPED NATURAL GAS") ~ 4,
      str_detect(hh_cookstove_type, "BIOGAS") ~ 5,
      str_detect(hh_cookstove_type, "KEROSENE") ~ 6,
      str_detect(hh_cookstove_type, "MANUFACTURED|IMPROVED") ~ 7, 
      str_detect(hh_cookstove_type, "TRADITIONAL|ANIMAL DUNG") ~ 8,      
      str_detect(hh_cookstove_type, "OPEN FIRE|THREE STONE|WOOD") ~ 9,
      str_detect(hh_cookstove_type, "NO FOOD") ~ 95,
      str_detect(hh_cookstove_type, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_relation_to_head = case_when(
      is.na(hh_relation_to_head) ~ NA_real_,
      str_detect(hh_relation_to_head, "HEAD") ~ 1,
      str_detect(hh_relation_to_head, "WIFE|HUSBAND") ~ 2,
      str_detect(hh_relation_to_head, "SON|DAUGHTER|CHILD") ~ 3,
      str_detect(hh_relation_to_head, "SON-IN-LAW") ~ 4,
      str_detect(hh_relation_to_head, "GRANDCHILD") ~ 5,
      str_detect(hh_relation_to_head, "PARENT") ~ 6,
      str_detect(hh_relation_to_head, "PARENT-IN-LAW") ~ 7,
      str_detect(hh_relation_to_head, "BROTHER|SISTER") ~ 8,
      str_detect(hh_relation_to_head, "OTHER RELATIVE") ~ 9,
      str_detect(hh_relation_to_head, "ADOPTED") ~ 10,
      str_detect(hh_relation_to_head, "NOT RELATED|CO-WIFE") ~ 11,
      str_detect(hh_relation_to_head, "DON'T KNOW|DK") ~ 98,      
      TRUE ~ 99
    ),
    
    across(c(hh_water_location, hh_toilet_location), ~case_when(
      is.na(.) ~ NA_real_,
      str_detect(., "OWN DWELLING") ~ 1,
      str_detect(., "OWN YARD") ~ 2,
      str_detect(., "ELSEWHERE") ~ 3,
      TRUE ~ 9
    )),
    
    hh_floor_material = case_when(
      is.na(hh_floor_material) ~ NA_real_,
      str_detect(hh_floor_material, "EARTH|SAND") ~ 11,
      str_detect(hh_floor_material, "DUNG") ~ 12,
      str_detect(hh_floor_material, "WOOD PLANKS") ~ 21,
      str_detect(hh_floor_material, "PALM|BAMBOO") ~ 22,
      str_detect(hh_floor_material, "PARQUET") ~ 31,
      str_detect(hh_floor_material, "VINYL|ASPHALT") ~ 32,
      str_detect(hh_floor_material, "CERAMIC") ~ 33,
      str_detect(hh_floor_material, "CEMENT") ~ 34,
      str_detect(hh_floor_material, "CARPET") ~ 35,
      str_detect(hh_floor_material, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_wall_material = case_when(
      is.na(hh_wall_material) ~ NA_real_,
      str_detect(hh_wall_material, "CANE|PALM|TRUNKS") ~ 11,
      str_detect(hh_wall_material, "MUD|DIRT") ~ 12, 
      str_detect(hh_wall_material, "BAMBOO WITH MUD") ~ 21,
      str_detect(hh_wall_material, "STONE WITH MUD") ~ 22,
      str_detect(hh_wall_material, "PLYWOOD") ~ 23,
      str_detect(hh_wall_material, "CARDBOARD") ~ 24,
      str_detect(hh_wall_material, "CEMENT") ~ 31, 
      str_detect(hh_wall_material, "STONE WITH LIME") ~ 32,
      str_detect(hh_wall_material, "BRICKS") ~ 33,
      str_detect(hh_wall_material, "COVERED ADOBE") ~ 34,
      str_detect(hh_wall_material, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_roof_material = case_when(
      is.na(hh_roof_material) ~ NA_real_,
      str_detect(hh_roof_material, "THATCH|PALM|LEAF") ~ 11,
      str_detect(hh_roof_material, "MUD") ~ 12,
      str_detect(hh_roof_material, "SOD") ~ 13,
      str_detect(hh_roof_material, "RUSTIC MAT") ~ 21,
      str_detect(hh_roof_material, "PALM|BAMBOO") ~ 22,
      str_detect(hh_roof_material, "WOOD PLANKS") ~ 23,
      str_detect(hh_roof_material, "METAL|ZINC") ~ 31,
      str_detect(hh_roof_material, "\\bWOOD\\b") ~ 32,
      str_detect(hh_roof_material, "CALAMINE|CEMENT FIBER") ~ 33,
      str_detect(hh_roof_material, "CERAMIC TILES") ~ 34,
      str_detect(hh_roof_material, "CEMENT") ~ 35,
      str_detect(hh_roof_material, "SHINGLES") ~ 36,
      str_detect(hh_roof_material, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    # --- K. NUMERIC PARSING ---
    across(c(
      demo_hh_children_under5, demo_hh_sleeping_rooms,
      prev_num_mosquito_nets, prev_num_people_slept_net, prev_months_since_net_obtained,
      women_births_2020_2025, women_anc_first_visit_month, women_anc_total_visits,
      women_sp_fansidar_doses, women_pregnancy_duration_months,
      hh_total_persons_v1, hh_total_persons_v2, hh_members_under5, hh_total_persons_usually_v3,
      hh_water_time_trip, hh_toilet_share_count,
      hh_num_cows_bulls, hh_num_other_cattle, hh_num_horses_donkeys, hh_num_goats,
      hh_num_sheep, hh_num_poultry, hh_num_pigs, hh_num_camels, hh_num_agri_plots
    ), ~case_when(
      is.na(.) ~ NA_real_,
      str_detect(., "DON'T KNOW|NOT SURE") ~ 98,
      str_detect(., "LESS THAN") ~ 0,
      str_detect(., "OR MORE") ~ as.numeric(str_extract(., "\\d+")),
      str_detect(., "MORE THAN") ~ as.numeric(str_extract(., "\\d+")) + 1,
      TRUE ~ as.numeric(str_extract(., "\\d+"))
    ))
  )

# ==============================================================================
# 5. LABELLING (APPLYING METADATA FOR SPSS)
# ==============================================================================

# ------------------------------------------------------------------------------
# A. DEFINE VALUE LABEL MAPPINGS
# ------------------------------------------------------------------------------

# Common SCALES
val_lbl_yesno   <- c("No" = 0, "Yes" = 1, "Don't Know" = 8, "Other/Missing" = 9)
val_lbl_gender  <- c("Male" = 1, "Female" = 2, "Other/Missing" = 9)
val_lbl_edu     <- c("No education" = 0, "Primary" = 1, "Secondary" = 2, "Higher" = 3, "Other/Missing" = 9)

val_lbl_likert <- c(
  "Strongly Agree" = 1, "Agree" = 2, "Neutral" = 3, 
  "Disagree" = 4, "Strongly Disagree" = 5, "Don't Know" = 8, "Other/Missing" = 9
)

val_lbl_loc <- c(
  "Government" = 1, "Private Hospital" = 2, "NGO hospital" = 3, "Mobile Clinic" = 4,
  "Private Doctor" = 5, "Community Health Worker" = 6, "Pharmacy" = 7, 
  "Chemist" = 8, "Traditional Practitioner" = 10, "Religious Institution" = 11, "Other/Missing" = 99
)

# Financial & Time
val_lbl_cost <- c(
  "Free" = 0, "N1 - N999" = 1, "N1,000 - N1,999" = 2, "N2,000 - N2,999" = 3,
  "N3,000 - N3,999" = 4, "N4,000 - N4,999" = 5, "N5,000 - N5,999" = 6,
  "N6,000 - N6,999" = 7, "N7,000 - N7,999" = 8, "N8,000 - N8,999" = 9,
  "N9,000 - N9,999" = 10, "N10,000 - N10,999" = 11, "N11,000 - N11,999" = 12,
  "N12,000 - N12,999" = 13, "N13,000 - N13,999" = 14, "N14,000 - N14,999" = 15,
  "Above N15,000" = 16, "Don't Know" = 98, "Other/Missing" = 99
)

val_lbl_time    <- c("Less than 30 mins" = 1, "30 mins" = 2, "More than 30 mins" = 3, "Don't Know" = 8, "Other/Missing" = 9)
val_lbl_purch   <- c("0-4 Weeks ago" = 1, "1-5 Months ago" = 2, "6-12 Months ago" = 3, "Over one year ago" = 4, "Don't Know" = 8, "Other/Missing" = 9)
val_lbl_afford  <- c("Very Affordable" = 1, "Somewhat Affordable" = 2, "Neutral" = 3, "Somewhat Expensive" = 4, "Very Expensive" = 5, "Don't Know" = 8, "Other/Missing" = 9)

# Household Characteristics
val_lbl_water <- c(
  "Piped into Dwelling/Yard/Plot" = 11, "Piped to Neighbour" = 12, "Public Tap/Standpipe" = 13,
  "Tube Well or Borehole" = 21, "Protected Well" = 31, "Unprotected Well" = 32,
  "Protected Spring" = 41, "Unprotected Spring" = 42, "Rainwater" = 51,
  "Tanker Truck" = 61, "Cart with Small Tank" = 71, "Surface Water" = 81,
  "Bottled Water" = 91, "Sachet Water" = 92, "Other" = 96, "Other/Missing" = 99
)

val_lbl_toilet <- c(
  "Flush to Piped Sewer" = 11, "Flush to Septic Tank" = 12, "Flush to Pit Latrine" = 13, 
  "Flush to Somewhere Else" = 14, "Flush Don't Know" = 15, "VIP Latrine" = 21, 
  "Pit Latrine with Slab" = 22, "Pit Latrine without Slab/Open Pit" = 23, 
  "Composting Toilet" = 31, "Bucket Toilet" = 41, "Hanging Toilet/Latrine" = 51,
  "No Facility/Bush/Field" = 61, "Other" = 96, "Other/Missing" = 99
)

val_lbl_facility_loc <- c(
  "In own dwelling" = 1, "In own yard/plot" = 2, "Elsewhere" = 3, "Missing" = 9
)

val_lbl_fuel <- c(
  "Alcohol" = 01, "Gasoline" = 02, "Kerosene" = 03, "Coal/Lignite" = 04,
  "Charcoal" = 05, "Wood" = 06, "Straw/Shrubs/Grass" = 07, "Agri Crop" = 08,
  "Animal Dung" = 09, "Biomass" = 10, "Garbage" = 11, "Sawdust" = 12,
  "Other" = 96, "Other/Missing" = 99
)

val_lbl_stove_type <- c(
  "Electric" = 1, "Solar" = 2, "LPG/Natural Gas" = 3, "Piped Natural Gas" = 4,
  "Biogas" = 5, "Kerosene" = 6, "Manufactured/Improved Solid Fuel" = 7, 
  "Traditional/Animal Dung" = 8, "Open Fire/Three Stone" = 9,
  "No Food Cooked" = 95, "Other" = 96, "Other/Missing" = 99
)

val_lbl_mat_flr <- c(
  "Earth/Sand" = 11, "Dung" = 12, "Wood Planks" = 21, "Palm/Bamboo" = 22,
  "Parquet/Polished Wood" = 31, "Vinyl/Asphalt" = 32, "Ceramic Tiles" = 33, 
  "Cement" = 34, "Carpet" = 35, "Other" = 96, "Other/Missing" = 99
)

val_lbl_mat_wall <- c(
  "Cane/Palm/Trunks" = 11, "Mud/Dirt" = 12, "Bamboo with Mud" = 21, 
  "Stone with Mud" = 22, "Plywood" = 23, "Cardboard" = 24, "Cement" = 31, 
  "Stone with Lime" = 32, "Bricks" = 33, "Covered Adobe" = 34, "Other" = 96, "Other/Missing" = 99
)

val_lbl_mat_roof <- c(
  "Thatch/Palm/Leaf" = 11, "Mud" = 12, "Sod" = 13, "Rustic Mat" = 21, 
  "Palm/Bamboo" = 22, "Wood Planks" = 23, "Metal/Zinc" = 31, "Wood" = 32, 
  "Calamine/Cement Fiber" = 33, "Ceramic Tiles" = 34, "Cement" = 35, 
  "Shingles" = 36, "Other" = 96, "Other/Missing" = 99
)

# Net Brands & Acquisition
val_lbl_brands <- c(
  "LLIN" = 10, "Olyset LLIN" = 11, "Iconlife LLIN" = 12, "Duranet LLIN" = 13,
  "Netprotect LLIN" = 14, "BASF Interceptor LLIN" = 15, "Yorkool LLIN" = 16,
  "Magnet LLIN" = 17, "Dawaplus 2.0 LLIN" = 18, "Royal Security LLIN" = 19,
  "Royal Sentry LLIN" = 20, "PermaNet 2.0 LLIN" = 21, "PermaNet 3.0 LLIN" = 22,
  "Veeralin LLIN" = 23, "Interceptor G2 LLIN" = 24, "Royal Guard LLIN" = 25,
  "Other LLIN/DK Brand" = 26, "Other (Not LLIN)" = 95, "Other" = 96, 
  "Don't Know" = 98, "Other/Missing" = 99
)

val_lbl_net_how <- c("Mass Campaign" = 1, "ANC" = 2, "Immunisation" = 3, "Other" = 96, "Other/Missing" = 99)
val_lbl_net_where <- c(
  "Government" = 1, "Private Sector" = 2, "Pharmacy" = 3, "Shop/Market" = 4,
  "CHW" = 5, "Religious Inst" = 6, "School" = 7, "Other" = 96, "Don't Know" = 98, "Other/Missing" = 99
)

# SMC & Vaccine
val_lbl_smc_drug <- c(
  "SP + AQ" = 1, "SP Only" = 2, "AL (Coartem)" = 3, "AS + AQ" = 4, 
  "DHA-PPQ" = 5, "Proguanil" = 6, "Other" = 96, "Don't Know" = 98, "Other/Missing" = 99
)

val_lbl_vaccine_age <- c(
  "Less than 12 months" = 1, "1 year" = 2, "2 years" = 3, "3 years" = 4, 
  "4 years" = 5, "5 years" = 6, "Above 5 years" = 7, "Don't Know" = 98, "Other/Missing" = 99
)

val_lbl_relation <- c(
  "Head" = 1, "Wife/Husband" = 2, "Son/Daughter" = 3, "Son-in-law" = 4,
  "Grandchild" = 5, "Parent" = 6, "Parent-in-law" = 7, "Brother/Sister" = 8,
  "Other Relative" = 9, "Adopted" = 10, "Not Related" = 11, "Don't Know" = 98, "Other/Missing" = 99
)

val_lbl_smc_elig <- c("Yes" = 1, "No (Has Child)" = 0, "No (No Child)" = 6, "Other/Missing" = 9)
val_lbl_rating   <- c("Very Effective" = 5, "Somewhat Effective" = 4, "Neutral" = 3, "Somewhat Ineffective" = 2, "Very Ineffective" = 1, "Other/Missing" = 9)
val_lbl_freq     <- c("Daily/Often" = 1, "Weekly/Rarely" = 2, "Never" = 3, "Other/Missing" = 9)
val_lbl_relig    <- c("Catholic" = 1, "Christian" = 2, "Islam" = 3, "Traditional" = 4, "Other" = 6, "Other/Missing" = 9)
val_lbl_delays   <- c("Same Day" = 0, "Next Day" = 1, "2 Days" = 2, "3+ Days" = 3, "Don't Know" = 8, "Other/Missing" = 9)

# ------------------------------------------------------------------------------
# B. APPLY VALUE LABELS TO COLUMNS
# ------------------------------------------------------------------------------

survey_labelled <- df_recoded %>%
  set_value_labels(
    demo_gender = val_lbl_gender,
    demo_edu_level = val_lbl_edu,
    prev_first_treatment_location = val_lbl_loc,
    prev_net_brand = val_lbl_brands,
    prev_net_obtained_how = val_lbl_net_how,
    prev_net_obtained_where = val_lbl_net_where,
    prev_time_to_treatment_facility = val_lbl_time,
    treat_transport_cost = val_lbl_cost,
    treat_test_cost = val_lbl_cost,
    treat_know_smc_drug = val_lbl_smc_drug,
    treat_drug_cost = val_lbl_cost,
    treat_drug_purchase_time = val_lbl_purch,
    treat_drug_affordability = val_lbl_afford,
    treat_children_received_smc = val_lbl_smc_elig,
    treat_children_received_vaccine = val_lbl_smc_elig,
    feedback_free_treatment_6months = val_lbl_smc_elig,
    feedback_drug_stockout_6months = val_lbl_smc_elig,
    feedback_gov_effort_rating = val_lbl_rating,
    treat_vaccine_age_knowledge = val_lbl_vaccine_age,
    women_child_first_advice_location = val_lbl_loc,
    women_child_act_delay = val_lbl_delays,
    women_child_advice_delay_days = val_lbl_delays,
    bg_tv_frequency = val_lbl_freq,
    bg_internet_frequency = val_lbl_freq,
    bg_religion = val_lbl_relig,
    hh_drinking_water_source = val_lbl_water,
    hh_other_water_source = val_lbl_water,
    hh_toilet_type = val_lbl_toilet,
    hh_cookstove_fuel = val_lbl_fuel,
    hh_cookstove_type = val_lbl_stove_type,
    hh_relation_to_head = val_lbl_relation,
    hh_water_location = val_lbl_facility_loc,
    hh_toilet_location = val_lbl_facility_loc,
    hh_floor_material = val_lbl_mat_flr,
    hh_wall_material = val_lbl_mat_wall,
    hh_roof_material = val_lbl_mat_roof
  ) %>%
  mutate(across(
    c(
      prev_has_mosquito_nets, prev_home_sprayed_interior, treat_hh_fever_last_2weeks, 
      treat_blood_sample_taken, treat_heard_smc, women_ever_given_birth, women_anc_seen,
      women_took_sp_fansidar, women_child_fever_2weeks, women_child_blood_sample,
      women_child_malaria_diagnosis, women_child_seek_advice, women_child_referral,
      women_child_took_medicine, women_child_act_effective, women_currently_pregnant,
      bg_own_smartphone, bg_internet_ever_used, bg_heard_malaria_msg_6months, 
      bg_aware_avoidance, hh_toilet_shared, hh_owns_livestock, hh_owns_agri_land,
      starts_with("hh_has_"), starts_with("hh_own_")
    ),
    ~ set_value_labels(.x, val_lbl_yesno)
  )) %>%
  mutate(across(
    c(
      att_rainy_season_only, att_fever_worry_malaria, att_malaria_easily_treated,
      att_weak_children_die, att_net_use_mosquito_density, att_net_use_warm_weather,
      att_home_meds_first, att_full_dose_importance, att_seek_care_immediate,
      att_community_net_usage
    ),
    ~ set_value_labels(.x, val_lbl_likert)
  ))

# ------------------------------------------------------------------------------
# C. APPLY VARIABLE LABELS (DESCRIPTIONS)
# ------------------------------------------------------------------------------
# Assigns a readable question string to every column.

var_label(survey_labelled$meta_respondent_id) <- "Unique Respondent Identifier"
var_label(survey_labelled$meta_status) <- "Interview Status"

# Demographics
var_label(survey_labelled$demo_gender) <- "What is your gender?"
var_label(survey_labelled$demo_edu_level) <- "Highest level of school attended"
var_label(survey_labelled$demo_hh_children_under5) <- "Number of children under 5 in household"
var_label(survey_labelled$demo_hh_sleeping_rooms) <- "Number of sleeping rooms in household"

# Malaria Prevention
var_label(survey_labelled$prev_has_mosquito_nets) <- "Does household have mosquito nets?"
var_label(survey_labelled$prev_num_mosquito_nets) <- "Number of mosquito nets owned"
var_label(survey_labelled$prev_months_since_net_obtained) <- "Months since net obtained"
var_label(survey_labelled$prev_net_brand) <- "Brand of mosquito net"
var_label(survey_labelled$prev_net_obtained_how) <- "How was net obtained?"
var_label(survey_labelled$prev_net_obtained_where) <- "Where was net obtained?"
var_label(survey_labelled$prev_num_people_slept_net) <- "People who slept in net last night"
var_label(survey_labelled$prev_home_sprayed_interior) <- "Home sprayed for mosquitoes in last 12m?"
var_label(survey_labelled$prev_first_treatment_location) <- "First location for malaria treatment"
var_label(survey_labelled$prev_time_to_treatment_facility) <- "Time to travel to treatment facility"

# Treatment
var_label(survey_labelled$treat_transport_cost) <- "Transport cost to facility"
var_label(survey_labelled$treat_hh_fever_last_2weeks) <- "Household fever in last 2 weeks?"
var_label(survey_labelled$treat_blood_sample_taken) <- "Blood sample taken for testing?"
var_label(survey_labelled$treat_test_cost) <- "Cost of malaria test"
var_label(survey_labelled$treat_drug_cost) <- "Cost of malaria drugs"
var_label(survey_labelled$treat_drug_purchase_time) <- "Time since last drug purchase"
var_label(survey_labelled$treat_drug_affordability) <- "Rating of drug affordability"
var_label(survey_labelled$treat_heard_smc) <- "Heard of SMC?"
var_label(survey_labelled$treat_children_received_smc) <- "Children received SMC?"
var_label(survey_labelled$treat_know_smc_drug) <- "Knows SMC drug name"
var_label(survey_labelled$treat_vaccine_age_knowledge) <- "Knowledge of vaccine age"
var_label(survey_labelled$treat_children_received_vaccine) <- "Children received vaccine?"

# Feedback
var_label(survey_labelled$feedback_free_treatment_6months) <- "Received free treatment (last 6m)?"
var_label(survey_labelled$feedback_drug_stockout_6months) <- "Experienced drug stockout (last 6m)?"
var_label(survey_labelled$feedback_gov_effort_rating) <- "Rating of Govt Malaria Efforts"

# Women
var_label(survey_labelled$women_ever_given_birth) <- "Ever given birth?"
var_label(survey_labelled$women_births_2020_2025) <- "Births between 2020-2025"
var_label(survey_labelled$women_anc_seen) <- "Received Antenatal Care (ANC)?"
var_label(survey_labelled$women_anc_first_visit_month) <- "Month of pregnancy at first ANC"
var_label(survey_labelled$women_anc_total_visits) <- "Total ANC visits"
var_label(survey_labelled$women_took_sp_fansidar) <- "Took SP/Fansidar during pregnancy?"
var_label(survey_labelled$women_sp_fansidar_doses) <- "Doses of SP/Fansidar taken"
var_label(survey_labelled$women_child_fever_2weeks) <- "Child had fever in last 2 weeks?"
var_label(survey_labelled$women_child_blood_sample) <- "Child blood sample taken?"
var_label(survey_labelled$women_child_malaria_diagnosis) <- "Child diagnosed with malaria?"
var_label(survey_labelled$women_child_seek_advice) <- "Sought advice for child fever?"
var_label(survey_labelled$women_child_first_advice_location) <- "First location sought for child fever"
var_label(survey_labelled$women_child_advice_delay_days) <- "Days delayed before seeking care"
var_label(survey_labelled$women_child_referral) <- "Child referred to higher care?"
var_label(survey_labelled$women_child_took_medicine) <- "Child took medicine?"
var_label(survey_labelled$women_child_act_delay) <- "Delay before taking ACT"
var_label(survey_labelled$women_child_act_effective) <- "Was ACT effective?"
var_label(survey_labelled$women_currently_pregnant) <- "Currently pregnant?"
var_label(survey_labelled$women_pregnancy_duration_months) <- "Duration of current pregnancy (months)"

# Background
var_label(survey_labelled$bg_tv_frequency) <- "Frequency of TV watching"
var_label(survey_labelled$bg_own_smartphone) <- "Owns smartphone?"
var_label(survey_labelled$bg_internet_ever_used) <- "Ever used internet?"
var_label(survey_labelled$bg_internet_frequency) <- "Frequency of internet use"
var_label(survey_labelled$bg_religion) <- "Religion"
var_label(survey_labelled$bg_heard_malaria_msg_6months) <- "Heard malaria message (last 6m)?"
var_label(survey_labelled$bg_aware_avoidance) <- "Aware of malaria avoidance?"

# Attitudes
var_label(survey_labelled$att_rainy_season_only) <- "Attitude: Malaria only in rainy season"
var_label(survey_labelled$att_fever_worry_malaria) <- "Attitude: Worry fever is malaria"
var_label(survey_labelled$att_malaria_easily_treated) <- "Attitude: Malaria easily treated"
var_label(survey_labelled$att_weak_children_die) <- "Attitude: Only weak children die"
var_label(survey_labelled$att_net_use_mosquito_density) <- "Attitude: Net use only when mosquitoes many"
var_label(survey_labelled$att_net_use_warm_weather) <- "Attitude: Dislike net in warm weather"
var_label(survey_labelled$att_home_meds_first) <- "Attitude: Home meds first"
var_label(survey_labelled$att_full_dose_importance) <- "Attitude: Important to finish dose"
var_label(survey_labelled$att_seek_care_immediate) <- "Attitude: Community seeks care immediately"
var_label(survey_labelled$att_community_net_usage) <- "Attitude: Community sleeps in nets"

# Household
var_label(survey_labelled$hh_total_persons_v1) <- "Total persons in household"
var_label(survey_labelled$hh_drinking_water_source) <- "Main drinking water source"
var_label(survey_labelled$hh_other_water_source) <- "Other water source"
var_label(survey_labelled$hh_water_location) <- "Location of water source"
var_label(survey_labelled$hh_water_time_trip) <- "Time to collect water"
var_label(survey_labelled$hh_toilet_type) <- "Toilet facility type"
var_label(survey_labelled$hh_toilet_shared) <- "Shared toilet?"
var_label(survey_labelled$hh_toilet_share_count) <- "Number of households sharing toilet"
var_label(survey_labelled$hh_toilet_location) <- "Location of toilet"
var_label(survey_labelled$hh_cookstove_type) <- "Main cookstove type"
var_label(survey_labelled$hh_cookstove_fuel) <- "Main cooking fuel"
var_label(survey_labelled$hh_owns_livestock) <- "Owns livestock?"
var_label(survey_labelled$hh_num_cows_bulls) <- "Number of Cows/Bulls"
var_label(survey_labelled$hh_num_goats) <- "Number of Goats"
var_label(survey_labelled$hh_num_sheep) <- "Number of Sheep"
var_label(survey_labelled$hh_num_poultry) <- "Number of Poultry"
var_label(survey_labelled$hh_owns_agri_land) <- "Owns agricultural land?"
var_label(survey_labelled$hh_num_agri_plots) <- "Number of agricultural plots"
var_label(survey_labelled$hh_has_electricity) <- "Has Electricity"
var_label(survey_labelled$hh_has_radio) <- "Has Radio"
var_label(survey_labelled$hh_has_tv) <- "Has TV"
var_label(survey_labelled$hh_has_non_mobile_phone) <- "Has Non-Mobile Phone"
var_label(survey_labelled$hh_has_computer) <- "Has Computer"
var_label(survey_labelled$hh_has_refrigerator) <- "Has Refrigerator"
var_label(survey_labelled$hh_own_mobile_phone) <- "Owns Mobile Phone"
var_label(survey_labelled$hh_has_bank_account) <- "Has Bank Account"
var_label(survey_labelled$hh_mobile_money_usage) <- "Uses Mobile Money"
var_label(survey_labelled$hh_floor_material) <- "Main Floor Material"
var_label(survey_labelled$hh_roof_material) <- "Main Roof Material"
var_label(survey_labelled$hh_wall_material) <- "Main Wall Material"

# Labelling Dummy Variables

# --- 1. Informal Education Dummies ---
var_label(survey_labelled$edu_inf_none)           <- "Informal education: None"
var_label(survey_labelled$edu_inf_adult)          <- "Informal education: Adult literacy"
var_label(survey_labelled$edu_inf_tsangaya)       <- "Informal education: Tsangaya"
var_label(survey_labelled$edu_inf_quranic)        <- "Informal education: Quranic"
var_label(survey_labelled$edu_inf_other)          <- "Informal education: Other"

# --- 2. Repellent Methods ---
var_label(survey_labelled$repel_coils)            <- "Prevention: Use mosquito coils"
var_label(survey_labelled$repel_spray)            <- "Prevention: Use insecticide sprays"
var_label(survey_labelled$repel_cream)            <- "Prevention: Use repellent creams"
var_label(survey_labelled$repel_electric)         <- "Prevention: Use electronic mosquito destroyers"
var_label(survey_labelled$repel_none)             <- "Prevention: No repellent methods used"

# --- 3. ANC Provider ---
var_label(survey_labelled$anc_prov_doc)           <- "ANC Provider: Doctor"
var_label(survey_labelled$anc_prov_nurse)         <- "ANC Provider: Nurse/Midwife"
var_label(survey_labelled$anc_prov_aux)           <- "ANC Provider: Auxiliary Nurse/Midwife"
var_label(survey_labelled$anc_prov_chew)          <- "ANC Provider: Community Extension Worker (CHEW)"
var_label(survey_labelled$anc_prov_tba)           <- "ANC Provider: Traditional Birth Attendant"
var_label(survey_labelled$anc_prov_field)         <- "ANC Provider: Community Health Field Worker"
var_label(survey_labelled$anc_prov_other)         <- "ANC Provider: Other"

# --- 4. ANC Location ---
var_label(survey_labelled$anc_loc_govhos)            <- "ANC Location: Government hospital"
var_label(survey_labelled$anc_loc_govcen)            <- "ANC Location: Government health center"
var_label(survey_labelled$anc_loc_govpost)            <- "ANC Location: Government health post"
var_label(survey_labelled$anc_loc_pvt)            <- "ANC Location: Private facility"
var_label(survey_labelled$anc_loc_ngohos)            <- "ANC Location: NGO hospital"
var_label(survey_labelled$anc_loc_ngoclin)            <- "ANC Location: NGO clinic"
var_label(survey_labelled$anc_loc_herhome)           <- "ANC Location: Her home"
var_label(survey_labelled$anc_loc_yourhome)            <- "ANC Location: Your home"
var_label(survey_labelled$anc_loc_other)            <- "ANC Location: Others"

# --- 5. SP/Fansidar Source ---
var_label(survey_labelled$sp_src_anc)             <- "Source of SP/Fansidar: Antenatal visit"
var_label(survey_labelled$sp_src_nonanc)           <- "Source of SP/Fansidar: Another facility (non-antenatal)"
var_label(survey_labelled$sp_src_pharm)           <- "Source of SP/Fansidar: Pharmacy/Chemist"
var_label(survey_labelled$sp_src_chew)            <- "Source of SP/Fansidar: Community health worker"
var_label(survey_labelled$sp_src_other)            <- "Source of SP/Fansidar: Others"

# --- 6. Child Treatment Advice Location ---
var_label(survey_labelled$child_adv_gov)          <- "Advice Source: Government facility"
var_label(survey_labelled$child_adv_pvth)         <- "Advice Source: Private hospital"
var_label(survey_labelled$child_adv_ngo)          <- "Advice Source: NGO facility"
var_label(survey_labelled$child_adv_mob)          <- "Advice Source: Mobile clinic"
var_label(survey_labelled$child_adv_pvtd)         <- "Advice Source: Private doctor"
var_label(survey_labelled$child_adv_com)          <- "Advice Source: Community health worker"
var_label(survey_labelled$child_adv_pharm)        <- "Advice Source: Pharmacy"
var_label(survey_labelled$child_adv_chem)         <- "Advice Source: Chemist"
var_label(survey_labelled$child_adv_trad)         <- "Advice Source: Traditional practitioner"
var_label(survey_labelled$child_adv_rel)          <- "Advice Source: Religious institution"

# --- 7. Child Medicine Type ---
var_label(survey_labelled$med_act)                <- "Medicine: Artemisinin-based Combination Therapy (ACT)"
var_label(survey_labelled$med_sp)                 <- "Medicine: SP/Fansidar"
var_label(survey_labelled$med_chloro)             <- "Medicine: Chloroquine"
var_label(survey_labelled$med_amod)               <- "Medicine: Amodiaquine"
var_label(survey_labelled$med_artesun)            <- "Medicine: Artesunate"
var_label(survey_labelled$med_quinine)            <- "Medicine: Quinine"
var_label(survey_labelled$med_inject)             <- "Medicine: Injection"
var_label(survey_labelled$med_other)              <- "Medicine: Other medicine"

# --- 8. Malaria Message Source ---
var_label(survey_labelled$msg_radio)              <- "Malaria Message Source: Radio"
var_label(survey_labelled$msg_tv)                 <- "Malaria Message Source: Television"
var_label(survey_labelled$msg_poster)             <- "Malaria Message Source: Poster/Billboard"
var_label(survey_labelled$msg_news)               <- "Malaria Message Source: Newspaper"
var_label(survey_labelled$msg_leaf)               <- "Malaria Message Source: Leaflet"
var_label(survey_labelled$msg_hcp)                <- "Malaria Message Source: Healthcare provider"
var_label(survey_labelled$msg_chw)                <- "Malaria Message Source: Community health worker"
var_label(survey_labelled$msg_social)             <- "Malaria Message Source: Social media"
var_label(survey_labelled$msg_town)               <- "Malaria Message Source: Town announcer"
var_label(survey_labelled$msg_ipc)                <- "Malaria Message Source: Inter-personal communication"
var_label(survey_labelled$msg_family)             <- "Malaria Message Source: Family/Friends"

# --- 9. Prevention Knowledge ---
var_label(survey_labelled$know_net)               <- "Knowledge: Sleep inside a mosquito net"
var_label(survey_labelled$know_itn)               <- "Knowledge: Use insecticide-treated net (ITN)"
var_label(survey_labelled$know_repel)             <- "Knowledge: Use mosquito repellent"
var_label(survey_labelled$know_stag)              <- "Knowledge: Clear stagnant water"
var_label(survey_labelled$know_spray)             <- "Knowledge: Spray house with insecticide"
var_label(survey_labelled$know_meds)              <- "Knowledge: Take preventative medications"
var_label(survey_labelled$know_clean)             <- "Knowledge: Keep surroundings clean"
var_label(survey_labelled$know_screens)           <- "Knowledge: Put screens on windows"
var_label(survey_labelled$know_other)             <- "Knowledge: Other way to avoid malaria"
var_label(survey_labelled$know_dont)              <- "Knowledge: Does not know any way"

# --- 5. CLEANUP & EXPORT ---
message("Step 4: Cleanup and Export...")

# Remove redundant text columns (Multi-select sources)
df_final <- survey_labelled %>%
  select(-c(
    meta_status, demo_edu_informal, prev_repellent_methods, women_anc_provider, 
    women_anc_location, women_sp_fansidar_source, women_child_advice_location,
    women_child_medicine_type, bg_malaria_msg_source, bg_prevention_knowledge
  ))

# Convert to SPSS Factors
df_convert <- df_final %>% mutate(across(where(is.character), as.factor))

# Export
write_sav(df_final, OUT_SPSS)
saveRDS(df_final, OUT_RDS)
write_csv(df_final, OUT_CSV)

cat("Recoding Complete. Files saved.\n")