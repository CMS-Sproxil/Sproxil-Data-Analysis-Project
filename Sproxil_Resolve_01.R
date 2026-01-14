# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  03_Data_Resolve.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    27 November 2025
#
# DESCRIPTION: 
# This script enforces logic on the dataset in two directions:
# 1. BLANKING (Commission Errors): Removes values where the respondent should have skipped.
# 2. FILLING (Omission Errors): Fills "NO RESPONSE" where the respondent should have answered.
# ==============================================================================

# --- 1. SETUP ---
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(haven)

# Input/Output Paths
INPUT_FILE  <- "Sproxil_Flagged.rds"
OUT_RDS     <- "Sproxil_Resolved.rds"
OUT_SPSS    <- "Sproxil_Resolved.sav"

if(!file.exists(INPUT_FILE)) stop("Input file not found. Run Cleaning Script first.")
sproxil_resolve <- readRDS(INPUT_FILE)

# --- 2. HELPER FUNCTIONS ---

# Function A: Force Blank (NA) if the Logic Condition is NOT met
# Usage: If they said "NO" to Nets, force Brand to NA.
force_blank <- function(target, trigger_col, trigger_keep_val) {
  # If trigger is NOT the 'Keep Value' (e.g., it is "NO"), wipe the target
  ifelse(trigger_col != trigger_keep_val, NA, target)
}

# Function B: Force Blank (NA) if a specific "Bad" Condition IS met
# Usage: If Gender is "MALE", force Pregnancy to NA.
force_blank_if <- function(target, trigger_col, trigger_bad_val) {
  ifelse(trigger_col == trigger_bad_val, NA, target)
}

# Function C: Fill Text Gaps (Imputation)
fix_text_cond <- function(target, trigger_col, trigger_val, replacement="NO RESPONSE") {
  ifelse(trigger_col == trigger_val & is.na(target), replacement, target)
}


# ==============================================================================
# 3. MAIN RESOLUTION PIPELINE
# ==============================================================================

sproxil_resolve <- sproxil_resolve %>%
  
  # ----------------------------------------------------------------------------
# PHASE 1: BLANKING (Removing Unexpected Values / Commission Errors)
# ----------------------------------------------------------------------------
# This ensures that if a respondent skipped a section, the data is truly NA.
mutate(
  
  # --- Demographics ---
  # If Formal Education exists, they SKIP Informal Education (PDF Q4 logic)
  demo_edu_informal = ifelse(demo_edu_level != "I NEVER HAD ANY FORMAL EDUCATION", NA, demo_edu_informal),
  
  # --- Nets ---
  # If No Nets, wipe Details
  across(c(prev_num_mosquito_nets, prev_net_brand, prev_net_obtained_how, prev_net_obtained_where), 
         ~force_blank(., prev_has_mosquito_nets, "YES")),
  
  # --- Treatment ---
  # If No Fever, wipe Blood Sample and Drugs
  across(c(treat_blood_sample_taken, treat_drug_cost, treat_drug_purchase_time, treat_drug_affordability), 
         ~force_blank(., treat_hh_fever_last_2weeks, "YES")),
  
  # If No Blood Sample, wipe Test Cost
  treat_test_cost = force_blank(treat_test_cost, treat_blood_sample_taken, "YES"),
  
  # --- SMC ---
  # If Not Heard SMC, wipe Received
  treat_children_received_smc = force_blank(treat_children_received_smc, treat_heard_smc, "YES"),
  
  # If Not Received SMC, wipe Drug Name
  treat_know_smc_drug = force_blank(treat_know_smc_drug, treat_children_received_smc, "YES"),
  
  # --- Vaccine ---
  # If Don't Know about Vaccine, wipe Received status
  treat_children_received_vaccine = ifelse(str_detect(treat_vaccine_age_knowledge, "DON'T KNOW"), NA, treat_children_received_vaccine),
  
  # --- Women's Questionnaire ---
  # If Male, wipe EVERYTHING in Women's Section
  across(starts_with("women_"), ~force_blank_if(., demo_gender, "MALE")),
  
  # If Female but Never Gave Birth, wipe Birth/ANC details
  across(c(women_births_2020_2025, women_anc_seen), 
         ~force_blank(., women_ever_given_birth, "YES")),
  
  # If No ANC, wipe Provider/Location
  across(c(women_anc_provider, women_anc_location), 
         ~force_blank(., women_anc_seen, "YES")),
  
  # If No SP/Fansidar, wipe Doses/Source
  across(c(women_sp_fansidar_doses, women_sp_fansidar_source), 
         ~force_blank(., women_took_sp_fansidar, "YES")),
  
  # If No Child Fever, wipe Diagnosis/Advice
  across(c(women_child_blood_sample, women_child_malaria_diagnosis, women_child_seek_advice), 
         ~force_blank(., women_child_fever_2weeks, "YES")),
  
  # If No Child Meds, wipe Med Type
  women_child_medicine_type = force_blank(women_child_medicine_type, women_child_took_medicine, "YES"),
  
  # If Not Pregnant, wipe Duration
  women_pregnancy_duration_months = force_blank(women_pregnancy_duration_months, women_currently_pregnant, "YES"),
  
  # --- Background ---
  # If No Internet, wipe Frequency
  bg_internet_frequency = force_blank(bg_internet_frequency, bg_internet_ever_used, "YES"),
  
  # If No Message Heard, wipe Source
  bg_malaria_msg_source = force_blank(bg_malaria_msg_source, bg_heard_malaria_msg_6months, "YES"),
  
  # If No Avoidance Awareness, wipe Methods
  bg_prevention_knowledge = force_blank(bg_prevention_knowledge, bg_aware_avoidance, "YES"),
  
  # --- Household ---
  # If No Facility (Toilet), wipe Shared/Location
  across(c(hh_toilet_shared, hh_toilet_location), 
         ~ifelse(str_detect(hh_toilet_type, "NO FACILITY"), NA, .)),
  
  # If Toilet Not Shared, wipe Count
  hh_toilet_share_count = force_blank(hh_toilet_share_count, hh_toilet_shared, "YES"),
  
  # If No Livestock, wipe Animal Counts AND Agri Land (PDF Page 27 Skip Logic)
  across(c(hh_num_cows_bulls, hh_num_other_cattle, hh_num_horses_donkeys, hh_num_goats, hh_num_sheep, hh_num_poultry, hh_num_pigs, hh_num_camels), 
         ~force_blank(., hh_owns_livestock, "YES")),
  
  # If No Agri Land (Nested check), wipe Plots
  hh_num_agri_plots = force_blank(hh_num_agri_plots, hh_owns_agri_land, "YES")
) %>%
  
  # ----------------------------------------------------------------------------
# PHASE 2: UNIVERSAL VARIABLES (Fill Mandatory Gaps)
# ----------------------------------------------------------------------------
mutate(
  # Fill Universals
  across(c(
    meta_respondent_id, meta_status, demo_gender, demo_edu_level, 
    demo_hh_children_under5, demo_hh_sleeping_rooms,
    prev_has_mosquito_nets, prev_home_sprayed_interior, prev_repellent_methods,
    prev_first_treatment_location, prev_time_to_treatment_facility,
    treat_transport_cost, treat_hh_fever_last_2weeks, treat_heard_smc, 
    treat_vaccine_age_knowledge,
    feedback_free_treatment_6months, feedback_drug_stockout_6months, 
    feedback_gov_effort_rating,
    bg_tv_frequency, bg_own_smartphone, bg_internet_ever_used, 
    bg_religion, bg_heard_malaria_msg_6months, bg_aware_avoidance,
    att_rainy_season_only, att_fever_worry_malaria, att_malaria_easily_treated, 
    att_weak_children_die, att_net_use_mosquito_density, 
    att_net_use_warm_weather, att_home_meds_first, att_full_dose_importance, 
    att_seek_care_immediate, att_community_net_usage,
    hh_total_persons_v1, hh_drinking_water_source, hh_toilet_type,
    hh_cookstove_type, hh_owns_livestock, hh_owns_agri_land,
    hh_floor_material, hh_roof_material, hh_wall_material,
    hh_has_electricity, hh_has_radio, hh_has_tv, hh_has_non_mobile_phone,
    hh_has_computer, hh_has_refrigerator, hh_has_table, hh_has_chair,
    hh_has_bed, hh_has_sofa, hh_has_cupboard, hh_has_ac, 
    hh_has_electric_iron, hh_has_generator, hh_has_fan, hh_own_watch, 
    hh_own_mobile_phone, hh_own_bicycle, hh_own_motorcycle, 
    hh_own_animal_cart, hh_own_car_truck, hh_own_motor_boat, 
    hh_own_canoe, hh_own_keke_napep, hh_has_bank_account, 
    hh_mobile_money_usage
  ), ~replace_na(., "NO RESPONSE"))
) %>%
  
  # ----------------------------------------------------------------------------
# PHASE 3: CONDITIONAL VARIABLES (Fill Omission Errors)
# ----------------------------------------------------------------------------
mutate(
  # --- Education ---
  # Only fill Informal if they have Formal Education (otherwise it stays NA from Phase 1)
  demo_edu_informal = ifelse(demo_edu_level != "I NEVER HAD ANY FORMAL EDUCATION" & is.na(demo_edu_informal),
                             "NO RESPONSE", demo_edu_informal),
  
  # --- Nets ---
  prev_net_brand = fix_text_cond(prev_net_brand, prev_has_mosquito_nets, "YES"),
  prev_net_obtained_how = fix_text_cond(prev_net_obtained_how, prev_has_mosquito_nets, "YES"),
  prev_net_obtained_where = fix_text_cond(prev_net_obtained_where, prev_has_mosquito_nets, "YES"),
  prev_num_mosquito_nets = fix_text_cond(prev_num_mosquito_nets, prev_has_mosquito_nets, "YES"),
  
  # --- Treatment ---
  treat_blood_sample_taken = fix_text_cond(treat_blood_sample_taken, treat_hh_fever_last_2weeks, "YES"),
  treat_test_cost = fix_text_cond(treat_test_cost, treat_blood_sample_taken, "YES"),
  
  # --- SMC & Vaccine ---
  treat_children_received_smc = fix_text_cond(treat_children_received_smc, treat_heard_smc, "YES"),
  treat_know_smc_drug = fix_text_cond(treat_know_smc_drug, treat_children_received_smc, "YES"),
  treat_children_received_vaccine = ifelse(!str_detect(treat_vaccine_age_knowledge, "DON'T KNOW") & is.na(treat_children_received_vaccine),
                                           "NO RESPONSE", treat_children_received_vaccine),
  
  # --- Women ---
  women_ever_given_birth = fix_text_cond(women_ever_given_birth, demo_gender, "FEMALE"),
  women_births_2020_2025 = fix_text_cond(women_births_2020_2025, women_ever_given_birth, "YES"),
  women_anc_seen = fix_text_cond(women_anc_seen, women_ever_given_birth, "YES"),
  women_anc_provider = fix_text_cond(women_anc_provider, women_anc_seen, "YES"),
  women_anc_location = fix_text_cond(women_anc_location, women_anc_seen, "YES"),
  women_sp_fansidar_doses = fix_text_cond(women_sp_fansidar_doses, women_took_sp_fansidar, "YES"),
  women_sp_fansidar_source = fix_text_cond(women_sp_fansidar_source, women_took_sp_fansidar, "YES"),
  women_child_blood_sample = fix_text_cond(women_child_blood_sample, women_child_fever_2weeks, "YES"),
  women_child_malaria_diagnosis = fix_text_cond(women_child_malaria_diagnosis, women_child_fever_2weeks, "YES"),
  women_child_seek_advice = fix_text_cond(women_child_seek_advice, women_child_fever_2weeks, "YES"),
  women_child_medicine_type = fix_text_cond(women_child_medicine_type, women_child_took_medicine, "YES"),
  women_pregnancy_duration_months = fix_text_cond(women_pregnancy_duration_months, women_currently_pregnant, "YES"),
  
  # --- Background ---
  bg_internet_frequency = fix_text_cond(bg_internet_frequency, bg_internet_ever_used, "YES"),
  bg_malaria_msg_source = fix_text_cond(bg_malaria_msg_source, bg_heard_malaria_msg_6months, "YES"),
  bg_prevention_knowledge = fix_text_cond(bg_prevention_knowledge, bg_aware_avoidance, "YES"),
  
  # --- Household ---
  hh_toilet_shared = ifelse(!str_detect(hh_toilet_type, "NO FACILITY") & is.na(hh_toilet_shared), 
                            "NO RESPONSE", hh_toilet_shared),
  hh_toilet_location = ifelse(!str_detect(hh_toilet_type, "NO FACILITY") & is.na(hh_toilet_location), 
                              "NO RESPONSE", hh_toilet_location),
  hh_toilet_share_count = fix_text_cond(hh_toilet_share_count, hh_toilet_shared, "YES"),
  
  # --- Counts --- 
  hh_num_cows_bulls = fix_text_cond(hh_num_cows_bulls, hh_owns_livestock, "YES"),
  hh_num_goats = fix_text_cond(hh_num_goats, hh_owns_livestock, "YES"),
  hh_num_sheep = fix_text_cond(hh_num_sheep, hh_owns_livestock, "YES"),
  hh_num_poultry = fix_text_cond(hh_num_poultry, hh_owns_livestock, "YES"),
  hh_num_agri_plots = fix_text_cond(hh_num_agri_plots, hh_owns_agri_land, "YES")
)

# --- 4. FINAL CLEANUP ---
sproxil_resolve <- sproxil_resolve %>%
  filter(meta_status %in% c("USED"))

# Remove the temporary flag columns before final save
sproxil_resolved <- sproxil_resolve %>%
  select(-starts_with("flag_"))

# --- 5. EXPORT ---
cat("Exporting resolved datasets...\n")
saveRDS(sproxil_resolved, OUT_RDS)
write_sav(sproxil_resolved, OUT_SPSS)
cat("Files saved successfully.\n")