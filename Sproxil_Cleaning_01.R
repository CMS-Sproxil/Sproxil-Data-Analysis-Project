# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Data_Cleaning
# AUTHOR:  Ikechukwu Onuko
# DATE:    26 November 2025
#
# DESCRIPTION:
# This script performs:
# 1. Standardization of column names.
# 2. Text cleaning (Trim & Uppercase).
# 3. Duplicate checks.
# 4. Validity checks against the Data Dictionary.
# 5. Conditional Missing Value checks.
# ==============================================================================

# --- 1. SETUP: LOAD REQUIRED LIBRARIES ---
library(dplyr)    
library(tidyr)    
library(knitr)    
library(openxlsx) 
library(readr)    
library(stringr)  

# --- 2. CONFIGURATION ---
INPUT_FILE_PATH <- "Sproxil_Malaria_Data.xlsx"
OUTPUT_FILE_PATH <- "Sproxil_Cleaned.rds"

# ==============================================================================
# 3. DATA PREPARATION ASSETS (COPIED FROM PREP SCRIPT)
# ==============================================================================

# A. STANDARD NAMES VECTOR (Must match your Excel columns 1-to-1)
standard_names_vector <- c(
  "meta_respondent_id", "meta_status",
  "demo_gender", "demo_edu_level", "demo_edu_informal", "demo_hh_children_under5", "demo_hh_sleeping_rooms",
  "prev_has_mosquito_nets", "prev_num_mosquito_nets", "prev_months_since_net_obtained", "prev_net_brand", 
  "prev_net_obtained_how", "prev_net_obtained_where", "prev_num_people_slept_net", "prev_home_sprayed_interior", 
  "prev_repellent_methods", "prev_first_treatment_location", "prev_time_to_treatment_facility",
  "treat_transport_cost", "treat_hh_fever_last_2weeks", "treat_blood_sample_taken", "treat_test_cost", 
  "treat_drug_cost", "treat_drug_purchase_time", "treat_drug_affordability", "treat_heard_smc", 
  "treat_children_received_smc", "treat_know_smc_drug", "treat_vaccine_age_knowledge", "treat_children_received_vaccine",
  "feedback_free_treatment_6months", "feedback_drug_stockout_6months", "feedback_gov_effort_rating",
  "women_ever_given_birth", "women_births_2020_2025", "women_anc_seen", "women_anc_provider", "women_anc_location", 
  "women_anc_first_visit_month", "women_anc_total_visits", "women_took_sp_fansidar", "women_sp_fansidar_doses", 
  "women_sp_fansidar_source", "women_child_fever_2weeks", "women_child_blood_sample", "women_child_malaria_diagnosis", 
  "women_child_seek_advice", "women_child_advice_location", "women_child_first_advice_location", "women_child_advice_delay_days", 
  "women_child_referral", "women_child_took_medicine", "women_child_medicine_type", "women_child_act_delay", 
  "women_child_act_effective", "women_currently_pregnant", "women_pregnancy_duration_months",
  "bg_tv_frequency", "bg_own_smartphone", "bg_internet_ever_used", "bg_internet_frequency", "bg_religion", 
  "bg_heard_malaria_msg_6months", "bg_malaria_msg_source", "bg_aware_avoidance", "bg_prevention_knowledge",
  "att_rainy_season_only", "att_fever_worry_malaria", "att_malaria_easily_treated", "att_weak_children_die", 
  "att_net_use_mosquito_density", "att_net_use_warm_weather", "att_home_meds_first", "att_full_dose_importance", 
  "att_seek_care_immediate", "att_community_net_usage",
  "hh_total_persons_v1", "hh_total_persons_v2", "hh_members_under5", "hh_total_persons_usually_v3", 
  "hh_relation_to_head", "hh_drinking_water_source", "hh_other_water_source", "hh_water_location", 
  "hh_water_time_trip", "hh_toilet_type", "hh_toilet_shared", "hh_toilet_share_count", "hh_toilet_location", 
  "hh_cookstove_type", "hh_cookstove_fuel", "hh_owns_livestock", "hh_num_cows_bulls", "hh_num_other_cattle", 
  "hh_num_horses_donkeys", "hh_num_goats", "hh_num_sheep", "hh_num_poultry", "hh_num_pigs", "hh_num_camels", 
  "hh_owns_agri_land", "hh_num_agri_plots", "hh_has_electricity", "hh_has_radio", "hh_has_tv", 
  "hh_has_non_mobile_phone", "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair", 
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", "hh_has_electric_iron", "hh_has_generator", 
  "hh_has_fan", "hh_own_watch", "hh_own_mobile_phone", "hh_own_bicycle", "hh_own_motorcycle", 
  "hh_own_animal_cart", "hh_own_car_truck", "hh_own_motor_boat", "hh_own_canoe", "hh_own_keke_napep", 
  "hh_has_bank_account", "hh_mobile_money_usage", "hh_floor_material", "hh_roof_material", "hh_wall_material"
)

# B. DICTIONARY (Condensed for space - Load the full RDS usually, but defined here for robustness)
# Check if RDS exists, if not, define empty or load (Here assuming you have the RDS from Step 1)
if(file.exists("Sproxil_mis_dictionary.rds")) {
  mis_data_dictionary <- readRDS("Sproxil_mis_dictionary.rds")
} else {
  stop("CRITICAL ERROR: 'Sproxil_mis_dictionary.rds' not found. Please run the Data Preparation script first.")
}

# C. STANDARDIZATION FUNCTION
standardize_column_names <- function(dataframe, standard_names_vector) {
  if (ncol(dataframe) != length(standard_names_vector)) {
    stop(paste0("Column mismatch! Data has ", ncol(dataframe), ", Script has ", length(standard_names_vector)))
  }
  names(dataframe) <- standard_names_vector
  return(dataframe)
}

# ==============================================================================
# 4. MAIN WORKFLOW
# ==============================================================================

# --- Step 4.1: Load & Standardize ---
cat("--- 1. Loading and Standardizing Data ---\n")
# Added 'sheet = 1' to ensure we don't accidentally read a "Readme" sheet
raw_data_df <- read.xlsx(INPUT_FILE_PATH, sheet = 3) 

sproxil_df <- standardize_column_names(raw_data_df, standard_names_vector)

# --- Step 4.2: Basic Cleaning (Text Standardization) ---
# Convert to UPPERCASE to match dictionary
sproxil_df <- sproxil_df %>%
  mutate(across(where(is.character), ~ str_squish(toupper(.))))

cat("Data loaded, columns standardized, and text converted to uppercase.\n")
cat(paste("Dimensions:", nrow(sproxil_df), "Rows,", ncol(sproxil_df), "Columns.\n\n"))

# --- Step 4.3: Duplicate Check (Robust) ---
cat("--- 2. Duplicate Check (ID Based) ---\n")
# Check for duplicate IDs
dup_ids <- sproxil_df %>%
  filter(duplicated(meta_respondent_id)) %>%
  pull(meta_respondent_id)

if (length(dup_ids) > 0) {
  cat(paste("WARNING:", length(dup_ids), "duplicate Respondent IDs found!\n"))
  print(head(dup_ids))
} else {
  cat("Success: No duplicate Respondent IDs found.\n")
}
cat("\n")

# --- Step 4.4: Dictionary Content Validation ---
cat("--- 3. Data Content Validation (Typos & Invalid Options) ---\n")
# This checks if values in the excel match the allowed dictionary values
# Subject to receiving the questionnaire, otherwise, this batch is redundant
invalid_report <- list()

for (col in names(sproxil_df)) {
  # Only check if column exists in our dictionary
  if (col %in% names(mis_data_dictionary)) {
    
    allowed_vals <- mis_data_dictionary[[col]]
    actual_vals <- unique(na.omit(sproxil_df[[col]]))
    
    # Find values in data that are NOT in dictionary
    bad_vals <- setdiff(actual_vals, allowed_vals)
    
    if (length(bad_vals) > 0) {
      invalid_report[[col]] <- paste(bad_vals, collapse = "; ")
    }
  }
}

if (length(invalid_report) > 0) {
  cat("WARNING: Found values not matching the Data Dictionary (Potential Typos):\n")
  print(kable(as.data.frame(invalid_report), format = "simple"))
} else {
  cat("Success: All categorical values match the Data Dictionary.\n")
}
cat("\n")

# --- Step 4.5: Conditional Missing Value Analysis (Skip Logic) ---
cat("--- 4. Missing Value Analysis (Respecting Skip Logic) ---\n")
# ==============================================================================
# REPLACEMENT FOR STEP 4.5: COMPREHENSIVE MISSING VALUE ANALYSIS
# ==============================================================================
cat("--- 4. Comprehensive Missing Value Analysis ---\n")

# ------------------------------------------------------------------------------
# GROUP A: UNIVERSAL VARIABLES (MUST NEVER BE MISSING)
# ------------------------------------------------------------------------------
# Based on the PDF, every respondent must answer these questions.
# Any NA here is a data collection error.

universal_vars <- c(
  # Meta & Demo
  "meta_respondent_id", "meta_status", "demo_gender", "demo_edu_level", "demo_hh_children_under5", "demo_hh_sleeping_rooms",
  
  # Prevention (Base Questions)
  "prev_has_mosquito_nets", "prev_home_sprayed_interior", "prev_repellent_methods",
  "prev_first_treatment_location", "prev_time_to_treatment_facility",
  
  # Treatment (Base Questions)
  "treat_transport_cost", "treat_hh_fever_last_2weeks", "treat_heard_smc", 
  "treat_vaccine_age_knowledge",
  
  # Feedback
  "feedback_free_treatment_6months", "feedback_drug_stockout_6months", 
  "feedback_gov_effort_rating",
  
  # Background (Base Questions)
  "bg_tv_frequency", "bg_own_smartphone", "bg_internet_ever_used", 
  "bg_religion", "bg_heard_malaria_msg_6months", "bg_aware_avoidance",
  
  # Attitudes (All are mandatory)
  "att_rainy_season_only", "att_fever_worry_malaria", "att_malaria_easily_treated", 
  "att_weak_children_die", "att_net_use_mosquito_density", 
  "att_net_use_warm_weather", "att_home_meds_first", "att_full_dose_importance", 
  "att_seek_care_immediate", "att_community_net_usage",
  
  # Household (Base Questions)
  "hh_total_persons_v1", "hh_drinking_water_source", "hh_toilet_facility_type",
  "hh_cookstove_type", "hh_owns_livestock", "hh_owns_agri_land",
  "hh_floor_material", "hh_roof_material", "hh_wall_material",
  
  # Assets (All Yes/No)
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", 
  "hh_has_electric_iron", "hh_has_generator", "hh_has_fan", "hh_own_watch", 
  "hh_own_mobile_phone", "hh_own_bicycle", "hh_own_motorcycle", 
  "hh_own_animal_cart", "hh_own_car_truck", "hh_own_motor_boat", 
  "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account", 
  "hh_mobile_money_usage"
)

# Check Universal Variables
missing_universal <- sproxil_df %>%
  select(any_of(universal_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  filter(Missing_Count > 0) %>%
  mutate(Type = "Universal (Must Answer)")

# ------------------------------------------------------------------------------
# GROUP B: CONDITIONAL VARIABLES (SKIP LOGIC CHECKS)
# ------------------------------------------------------------------------------
# Checks if a question is missing when the logic dictates it SHOULD be answered.

missing_conditional <- sproxil_df %>%
  summarise(
    # --- Malaria Prevention Logic ---
    # If Has Nets = YES, Must answer Quantity, Brand, Source, etc.
    prev_nets_details = sum(prev_has_mosquito_nets == "YES" & 
                              (is.na(prev_num_mosquito_nets) | is.na(prev_net_brand) | 
                                 is.na(prev_net_obtained_how)), na.rm = TRUE),
    
    # --- Malaria Treatment Logic ---
    # If Fever = YES, Must answer Blood Sample?
    treat_fever_flow = sum(treat_hh_fever_last_2weeks == "YES" & 
                             is.na(treat_blood_sample_taken), na.rm = TRUE),
    
    # If Blood Sample = YES, Must answer Test Cost?
    treat_test_cost_flow = sum(treat_blood_sample_taken == "YES" & 
                                 is.na(treat_test_cost), na.rm = TRUE),
    
    # If Heard SMC = YES, Must answer Recieved SMC? (PDF Q28->Q29)
    treat_smc_flow = sum(treat_heard_smc == "YES" & 
                           is.na(treat_children_received_smc), na.rm = TRUE),
    
    # --- Women's Questionnaire Logic ---
    # Only Females answer Section 5 (implied)
    women_q_base = sum(demo_gender == "FEMALE" & 
                         is.na(women_ever_given_birth), na.rm = TRUE),
    
    # If Birth = YES, Answer Q2
    women_birth_flow = sum(women_ever_given_birth == "YES" & 
                             is.na(women_births_2020_2025), na.rm = TRUE),
    
    # If ANC = YES, Answer Provider & Location
    women_anc_flow = sum(women_anc_seen == "YES" & 
                           (is.na(women_anc_provider) | is.na(women_anc_location)), na.rm = TRUE),
    
    # If Took SP = YES, Answer Doses
    women_sp_flow = sum(women_took_sp_fansidar == "YES" & 
                          is.na(women_sp_fansidar_doses), na.rm = TRUE),
    
    # If Child Fever = YES, Answer Blood Sample
    women_child_fever_flow = sum(women_child_fever_2weeks == "YES" & 
                                   is.na(women_child_blood_sample), na.rm = TRUE),
    
    # If Took Meds = YES, Answer Type
    women_child_meds_flow = sum(women_child_took_medicine == "YES" & 
                                  is.na(women_child_medicine_type), na.rm = TRUE),
    
    # If Pregnant = YES, Answer Duration
    women_preg_flow = sum(women_currently_pregnant == "YES" & 
                            is.na(women_pregnancy_duration_months), na.rm = TRUE),
    
    # --- Background Logic ---
    # If Internet = YES, Answer Frequency
    bg_internet_flow = sum(bg_internet_ever_used == "YES" & 
                             is.na(bg_internet_frequency), na.rm = TRUE),
    
    # If Heard Msg = YES, Answer Source
    bg_msg_flow = sum(bg_heard_malaria_msg_6months == "YES" & 
                        is.na(bg_malaria_msg_source), na.rm = TRUE),
    
    # If Aware Avoidance = YES, Answer Methods
    bg_avoid_flow = sum(bg_aware_avoidance == "YES" & 
                          is.na(bg_prevention_knowledge), na.rm = TRUE),
    
    # --- Household Logic ---
    # If Toilet != No Facility, Answer Shared & Location
    # Note: Using str_detect because "No Facility" might be part of a longer string
    hh_toilet_flow = sum(!str_detect(hh_toilet_type, "NO FACILITY") & 
                           (is.na(hh_toilet_shared) | is.na(hh_toilet_location)), na.rm = TRUE),
    
    # If Shared = YES, Answer Count
    hh_shared_flow = sum(hh_toilet_shared == "YES" & 
                           is.na(hh_toilet_share_count), na.rm = TRUE),
    
    # If Livestock = YES, Answer Counts (Cows, Goats, etc.)
    hh_livestock_flow = sum(hh_owns_livestock == "YES" & 
                              (is.na(hh_num_cows_bulls) | is.na(hh_num_goats)), na.rm = TRUE),
    
    # If Land = YES, Answer Plots
    hh_land_flow = sum(hh_owns_agri_land == "YES" & 
                         is.na(hh_num_agri_plots), na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  filter(Missing_Count > 0) %>%
  mutate(Type = "Conditional Logic Error (Skip Pattern)")

# Combine and Print
final_missing_report <- bind_rows(missing_universal, missing_conditional)

if(nrow(final_missing_report) > 0) {
  cat("WARNING: Unexpected Missing Values Found!\n")
  print(kable(final_missing_report, 
              col.names = c("Variable/Logic Check", "Missing Count", "Error Type"),
              caption = "Missing Data Report"))
} else {
  cat("SUCCESS: No missing data found in Universal variables or Conditional Logic paths.\n")
}

# --- Step 4.6: Save ---
cat("\n--- 5. Saving Processed Data ---\n")
saveRDS(sproxil_df, file = OUTPUT_FILE_PATH)
cat("File saved to:", OUTPUT_FILE_PATH, "\n")