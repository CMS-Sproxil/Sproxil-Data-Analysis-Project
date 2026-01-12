# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  02_Data_Cleaning.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    26 November 2025
# ==============================================================================

# --- 1. SETUP: LOAD REQUIRED LIBRARIES ---
library(dplyr)    
library(tidyr)    
library(knitr)    
library(openxlsx) 
library(readr)    
library(stringr)  
library(haven)    # Crucial for SPSS (.sav) export

# --- 2. CONFIGURATION ---
INPUT_FILE_PATH <- "Sproxil_Malaria_Data.xlsx"
OUTPUT_RDS_PATH <- "Sproxil_Prepared.rds"
OUTPUT_SPSS_PATH <- "Sproxil_Prepared.sav"

# ==============================================================================
# 3. DEFINITIONS: NAMES & LABELS
# ==============================================================================

# A. STANDARD NAMES (Internal R Names)
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

# B. VARIABLE LABELS 
variable_labels_map <- c(
  "meta_respondent_id" = "respondent_id",
  "meta_status" = "Status",
  "demo_gender" = "What is your gender?",
  "demo_edu_level" = "What is your highest level of education?",
  "demo_edu_informal" = "What type of informal education have you attended?",
  "demo_hh_children_under5" = "How many children under the age of 5 live in your household?",
  "demo_hh_sleeping_rooms" = "How many rooms in your house are used for sleeping",
  "prev_has_mosquito_nets" = "Does your household have any mosquito nets?",
  "prev_num_mosquito_nets" = "How many mosquito nets does your household have?",
  "prev_months_since_net_obtained" = "How many months ago did your household get the mosquito net?",
  "prev_net_brand" = "What brand of mosquito nets do members of this household use? (LLLNG means Long-lasting insecticide-treated net)",
  "prev_net_obtained_how" = "How did you get the mosquito net?",
  "prev_net_obtained_where" = "Where did you get the net?",
  "prev_num_people_slept_net" = "How many people slept inside this mosquito net last night?",
  "prev_home_sprayed_interior" = "In the past 12 months, has anyone entered your home to spray the interior walls for mosquito control?",
  "prev_repellent_methods" = "Do you use any of these mosquito repellent methods?",
  "prev_first_treatment_location" = "Where do you usually go first to seek malaria treatment?",
  "prev_time_to_treatment_facility" = "How long does it take to reach the nearest healthcare facility to seek malaria treatment?",
  "treat_transport_cost" = "How much do you spend on transport cost (to and fro) to visit the nearest healthcare facility to seek malaria treatment",
  "treat_hh_fever_last_2weeks" = "Have you or anyone in your household had fever in the last 2 weeks?",
  "treat_blood_sample_taken" = "Was a blood sample taken from any part of their body for testing before treatment was given?",
  "treat_test_cost" = "How much did the test cost you?",
  "treat_drug_cost" = "How much did your last malaria drugs cost?",
  "treat_drug_purchase_time" = "When did you buy your last malaria drug?",
  "treat_drug_affordability" = "Rate the affordability of malaria drugs in your community",
  "treat_heard_smc" = "Have you heard about Seasonal Malaria Chemoprevention (SMC)?",
  "treat_children_received_smc" = "Have your children ever received Seasonal Malaria Chemoprevention (SMC) treatment?",
  "treat_know_smc_drug" = "Do you know the drug that was administered to your children during the Seasonal Malaria Chemoprevention?",
  "treat_vaccine_age_knowledge" = "At what age should a child receive the first dose of the malaria vaccine",
  "treat_children_received_vaccine" = "Have your children ever received a malaria vaccine dose?",
  "feedback_free_treatment_6months" = "Have you received free malaria treatment at a government facility in the last 6 months?",
  "feedback_drug_stockout_6months" = "Have you experienced any case where there was no malaria drugs available at a government health facilities in the last 6 months?",
  "feedback_gov_effort_rating" = "How would you rate the government's efforts in combating malaria?",
  "women_ever_given_birth" = "Have you ever given birth?",
  "women_births_2020_2025" = "How many children have you given birth to between 2020 and 2025?",
  "women_anc_seen" = "While you were pregnant with your last child that you gave birth to alive, did you see anyone for antenatal care for this pregnancy?",
  "women_anc_provider" = "Whom did you see for the antenatal care?",
  "women_anc_location" = "Where did you receive the antenal care?",
  "women_anc_first_visit_month" = "How many months/weeks pregnant were you when you first received antenatal care for this pregnancy?",
  "women_anc_total_visits" = "How many times did you receive antenatal care during this pregnancy?",
  "women_took_sp_fansidar" = "During this pregnancy, did you take SP/Fansidar to keep you from getting malaria?",
  "women_sp_fansidar_doses" = "How many times did you take SP/Fansidar during this pregnancy?",
  "women_sp_fansidar_source" = "How did you get the SP/Fansidar?",
  "women_child_fever_2weeks" = "Has your youngest child (born in the last 5 years) been ill with a fever at any time in the last 2 weeks?",
  "women_child_blood_sample" = "At any time during the illness, did the child have blood sample sample taken for testing?",
  "women_child_malaria_diagnosis" = "Were you told by a healthcare provider that the child had malaria?",
  "women_child_seek_advice" = "Did you seek advice or treatment for the illness from any source?",
  "women_child_advice_location" = "Where did you seek advice or treatment for the fever?",
  "women_child_first_advice_location" = "Where did you first seek advice or treatment?",
  "women_child_advice_delay_days" = "How many days after the illness began did you first seek advice or treatment?",
  "women_child_referral" = "While your child was sick with this fever were you referred to go to a higher level of care?",
  "women_child_took_medicine" = "At any time during the illness, did your child take any medicine for the illness?",
  "women_child_medicine_type" = "What medicine did your child take during the illness?",
  "women_child_act_delay" = "How long after the fever started did your child first take an artemisinin combination therapy?",
  "women_child_act_effective" = "After your child took an artemisinin combination therapy, did the fever go away?",
  "women_currently_pregnant" = "Are you pregnant now?",
  "women_pregnancy_duration_months" = "How many weeks/months pregnant are you?",
  "bg_tv_frequency" = "How often do you watch TV?",
  "bg_own_smartphone" = "Do you own a smart phone?",
  "bg_internet_ever_used" = "Have you ever used the Internet before?",
  "bg_internet_frequency" = "How often did you use the Internet?",
  "bg_religion" = "What is your religion?",
  "bg_heard_malaria_msg_6months" = "In the past 6 months, have you seen or heard any messages about malaria?",
  "bg_malaria_msg_source" = "Where did you see or hear the messages about malaria?",
  "bg_aware_avoidance" = "Are you aware of any way to avoid malaria?",
  "bg_prevention_knowledge" = "What are the things that people can do to prevent themselves from getting malaria?",
  "att_rainy_season_only" = "People in this community only get malaria during the rainy season?",
  "att_fever_worry_malaria" = "When a child has a fever, do you almost always worry it might be malaria?",
  "att_malaria_easily_treated" = "Getting malaria is not a problem because it can be easily treated.",
  "att_weak_children_die" = "Only weak children can die from malaria.",
  "att_net_use_mosquito_density" = "You only need to sleep inside a mosquito net for the entire night when there are lots of mosquitoes.",
  "att_net_use_warm_weather" = "You do not like sleeping inside a mosquito net when the weather is too warm.",
  "att_home_meds_first" = "When a child has a fever, it is best to start by giving them any medicine you have at home.",
  "att_full_dose_importance" = "It is important that children take the full dose of medicine that they are prescribed for malaria",
  "att_seek_care_immediate" = "People in your community usually take their children to a health care provider on the same day or day after they develop a fever.",
  "att_community_net_usage" = "People in your community who have a mosquito net usually sleep inside a mosquito net every night.",
  "hh_total_persons_v1" = "How many persons live usually in your household - including children, domestic servants, lodgers and guests of the household?",
  "hh_total_persons_v2" = "How many people live in your house hold?",
  "hh_members_under5" = "How many people that live in this household are under 5 years old?",
  "hh_total_persons_usually_v3" = "How many persons USUALLY live in your household - including children, domestic servants, lodgers and guests of the household?",
  "hh_relation_to_head" = "What is your relationship with the head of the household?",
  "hh_drinking_water_source" = "What is the MAIN source of drinking water for members of your household?",
  "hh_other_water_source" = "What is the MAIN source of water used by your household for other purposes such as cooking and handwashing?",
  "hh_water_location" = "Where is that water source located?",
  "hh_water_time_trip" = "How long does it take to go there, get water, and come back?",
  "hh_toilet_type" = "What kind of toilet facility do members of your household usually use?",
  "hh_toilet_shared" = "Do you share this toilet facility with other households?",
  "hh_toilet_share_count" = "Including your own household, how many households use this toilet facility?",
  "hh_toilet_location" = "Where is this toilet facility located?",
  "hh_cookstove_type" = "In your household, what type of cookstove is MAINLY used for cooking?",
  "hh_cookstove_fuel" = "What type of fuel or energy source is used in this cookstove?",
  "hh_owns_livestock" = "Does this household own any livestock, herds, other farm animals, or poultry?",
  "hh_num_cows_bulls" = "How many cow/bulls does this household own?",
  "hh_num_other_cattle" = "How many other cattles does this household own?",
  "hh_num_horses_donkeys" = "How many horses/donkeys/mules does this household own?",
  "hh_num_goats" = "How many goats does this household own?",
  "hh_num_sheep" = "How many sheep does this household own?",
  "hh_num_poultry" = "How many chickens/other poultry does this household own?",
  "hh_num_pigs" = "How many pigs does this household own?",
  "hh_num_camels" = "How many camels does this household own?",
  "hh_owns_agri_land" = "Does any member of this household own any agricultural land?",
  "hh_num_agri_plots" = "How many plots of agricultural land do members of this household own?",
  "hh_has_electricity" = "Does your household have electricity?",
  "hh_has_radio" = "Does your household have a radio?",
  "hh_has_tv" = "Does your household have a television?",
  "hh_has_non_mobile_phone" = "Does your household have a non-mobile phone?",
  "hh_has_computer" = "Does your household have a computer?",
  "hh_has_refrigerator" = "Does your household have a refrigerator?",
  "hh_has_table" = "Does your household have a table?",
  "hh_has_chair" = "Does your household have a chair?",
  "hh_has_bed" = "Does your household have a bed?",
  "hh_has_sofa" = "Does your household have a sofa?",
  "hh_has_cupboard" = "Does your household have a cupboard?",
  "hh_has_ac" = "Does your household have an air conditioner?",
  "hh_has_electric_iron" = "Does your household have an electric iron?",
  "hh_has_generator" = "Does your household have a generator?",
  "hh_has_fan" = "Does your household have a fan?",
  "hh_own_watch" = "Does any member of this household own a watch?",
  "hh_own_mobile_phone" = "Does any member of this household own a mobile phone?",
  "hh_own_bicycle" = "Does any member of this household own a bicycle?",
  "hh_own_motorcycle" = "Does any member of this household own a motorcycle or motor scooter?",
  "hh_own_animal_cart" = "Does any member of this household own an animal-drawn cart?",
  "hh_own_car_truck" = "Does any member of this household own a car or truck?",
  "hh_own_motor_boat" = "Does any member of this household own a boat with a motor?",
  "hh_own_canoe" = "Does any member of this household own a canoe?",
  "hh_own_keke_napep" = "Does any member of this household own a keke napep?",
  "hh_has_bank_account" = "Does any member of this household have an account in a bank or other financial institution?",
  "hh_mobile_money_usage" = "Does any member of this household use a mobile phone to make financial transactions?",
  "hh_floor_material" = "What is the MAIN material used for the FLOOR of the house you live in?",
  "hh_roof_material" = "What is the MAIN material used for the ROOF of the house you live in?",
  "hh_wall_material" = "What is the MAIN material used for the WALL of the house you live in?"
)

# C. LOAD DICTIONARY
if(file.exists("Sproxil_mis_dictionary.rds")) {
  mis_data_dictionary <- readRDS("Sproxil_mis_dictionary.rds")
} else {
  stop("CRITICAL ERROR: 'Sproxil_mis_dictionary.rds' not found.")
}

# C. HELPER FUNCTIONS
standardize_column_names <- function(dataframe, standard_names_vector) {
  if (ncol(dataframe) != length(standard_names_vector)) {
    stop(paste0("Column mismatch! Data has ", ncol(dataframe), ", Script expects ", length(standard_names_vector)))
  }
  names(dataframe) <- standard_names_vector
  return(dataframe)
}

# ==============================================================================
# 4. MAIN WORKFLOW
# ==============================================================================

# --- Step 4.1: Load & Standardize ---
cat("--- 1. Loading and Standardizing Data ---\n")
raw_data_df <- read.xlsx(INPUT_FILE_PATH, sheet = 3) 
sproxil_df <- standardize_column_names(raw_data_df, standard_names_vector)

# --- Step 4.2: Text Cleaning ---
sproxil_df <- sproxil_df %>%
  mutate(across(where(is.character), ~ str_squish(toupper(.))))

# --- Step 4.3: Content Validation (Dictionary) ---
cat("--- 2. Data Content Validation ---\n")
invalid_report <- list()
for (col in names(sproxil_df)) {
  if (col %in% names(mis_data_dictionary)) {
    allowed_vals <- mis_data_dictionary[[col]]
    actual_vals <- unique(na.omit(sproxil_df[[col]]))
    bad_vals <- setdiff(actual_vals, allowed_vals)
    if (length(bad_vals) > 0) invalid_report[[col]] <- paste(bad_vals, collapse = "; ")
  }
}
if (length(invalid_report) > 0) {
  cat("WARNING: Unknown values found:\n")
  print(kable(as.data.frame(invalid_report), format = "simple"))
} else {
  cat("Success: All categorical values match the Data Dictionary.\n")
}

# --- Step 4.4: Logic-Aware Missing Value Analysis ---
cat("--- 3. Comprehensive Missing Value Analysis ---\n")


# FILTER: Create a subset just for this check
missing_df <- sproxil_df %>% 
  filter(meta_status == "USED")

if(nrow(missing_df) == 0) {
  stop("CRITICAL ERROR: No rows found with Status == 'USED'. Check your spelling in the Excel file.")
}

cat(paste("Performing missing value analysis on", nrow(missing_df), "completed surveys.\n"))

# A. UNIVERSAL VARIABLES (Group A)
universal_vars <- c(
  "meta_respondent_id", "meta_status", "demo_gender", "demo_edu_level", 
  "demo_hh_children_under5", "demo_hh_sleeping_rooms",
  "prev_has_mosquito_nets", "prev_home_sprayed_interior", "prev_repellent_methods",
  "prev_first_treatment_location", "prev_time_to_treatment_facility",
  "treat_transport_cost", "treat_hh_fever_last_2weeks", "treat_heard_smc", 
  "treat_vaccine_age_knowledge",
  "feedback_free_treatment_6months", "feedback_drug_stockout_6months", 
  "feedback_gov_effort_rating",
  "bg_tv_frequency", "bg_own_smartphone", "bg_internet_ever_used", 
  "bg_religion", "bg_heard_malaria_msg_6months", "bg_aware_avoidance",
  "att_rainy_season_only", "att_fever_worry_malaria", "att_malaria_easily_treated", 
  "att_weak_children_die", "att_net_use_mosquito_density", 
  "att_net_use_warm_weather", "att_home_meds_first", "att_full_dose_importance", 
  "att_seek_care_immediate", "att_community_net_usage",
  "hh_total_persons_v1", "hh_drinking_water_source", "hh_toilet_type",
  "hh_cookstove_type", "hh_owns_livestock", "hh_owns_agri_land",
  "hh_floor_material", "hh_roof_material", "hh_wall_material",
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", 
  "hh_has_electric_iron", "hh_has_generator", "hh_has_fan", "hh_own_watch", 
  "hh_own_mobile_phone", "hh_own_bicycle", "hh_own_motorcycle", 
  "hh_own_animal_cart", "hh_own_car_truck", "hh_own_motor_boat", 
  "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account", 
  "hh_mobile_money_usage"
)


missing_universal <- missing_df %>%
  select(any_of(universal_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  filter(Missing_Count > 0) %>%
  mutate(Type = "Universal Missing")

# B. CONDITIONAL VARIABLES (Group B)
missing_conditional <- missing_df %>%
  summarise(
    # Prev: Nets Logic
    prev_nets_logic = sum(prev_has_mosquito_nets == "YES" & 
                            (is.na(prev_num_mosquito_nets) | is.na(prev_net_brand) | 
                               is.na(prev_net_obtained_how) | is.na(prev_net_obtained_where) | 
                               is.na(prev_num_people_slept_net)), na.rm = TRUE),
    
    # Treat: Fever Logic (Flow to Blood)
    treat_fever_logic = sum(treat_hh_fever_last_2weeks == "YES" & 
                              is.na(treat_blood_sample_taken), na.rm = TRUE),
    
    # Treat: Blood Logic (Flow to Test Cost)
    treat_blood_logic = sum(treat_blood_sample_taken == "YES" & 
                              is.na(treat_test_cost), na.rm = TRUE),
    
    # Treat: SMC Heard -> Recieved
    treat_smc_heard_logic = sum(treat_heard_smc == "YES" & 
                                  is.na(treat_children_received_smc), na.rm = TRUE),
    
    # Treat: SMC Recieved -> Know Drug
    treat_smc_rec_logic = sum(treat_children_received_smc == "YES" & 
                                is.na(treat_know_smc_drug), na.rm = TRUE),
    
    # Treat: Vaccine Know -> Recieved
    treat_vaccine_logic = sum(!str_detect(treat_vaccine_age_knowledge, "DON'T KNOW") & 
                                is.na(treat_children_received_vaccine), na.rm = TRUE),
    
    # Women: Base Gender Check
    women_gender_logic = sum(demo_gender == "FEMALE" & 
                               is.na(women_ever_given_birth), na.rm = TRUE),
    
    # Women: Birth -> Count
    women_birth_logic = sum(women_ever_given_birth == "YES" & 
                              (is.na(women_births_2020_2025) | is.na(women_anc_seen)), na.rm = TRUE),
    
    # Women: ANC -> Details
    women_anc_logic = sum(women_anc_seen == "YES" & 
                            (is.na(women_anc_provider) | is.na(women_anc_location)), na.rm = TRUE),
    
    # Women: SP -> Doses/Source
    women_sp_logic = sum(women_took_sp_fansidar == "YES" & 
                           (is.na(women_sp_fansidar_doses) | is.na(women_sp_fansidar_source)), na.rm = TRUE),
    
    # Women: Child Fever -> Details
    women_child_fever_logic = sum(women_child_fever_2weeks == "YES" & 
                                    (is.na(women_child_blood_sample) | is.na(women_child_malaria_diagnosis)), na.rm = TRUE),
    
    # Women: Meds -> Type
    women_child_meds_logic = sum(women_child_took_medicine == "YES" & 
                                   is.na(women_child_medicine_type), na.rm = TRUE),
    
    # Women: Pregnant -> Duration
    women_preg_logic = sum(women_currently_pregnant == "YES" & 
                             is.na(women_pregnancy_duration_months), na.rm = TRUE),
    
    # Background: Internet -> Freq
    bg_internet_logic = sum(bg_internet_ever_used == "YES" & 
                              is.na(bg_internet_frequency), na.rm = TRUE),
    
    # Background: Msg -> Source
    bg_msg_logic = sum(bg_heard_malaria_msg_6months == "YES" & 
                         is.na(bg_malaria_msg_source), na.rm = TRUE),
    
    # Background: Avoid -> Methods
    bg_avoid_logic = sum(bg_aware_avoidance == "YES" & 
                           is.na(bg_prevention_knowledge), na.rm = TRUE),
    
    # Household: Toilet -> Shared/Location (If not No Facility)
    hh_toilet_logic = sum(!str_detect(hh_toilet_type, "NO FACILITY") & 
                            (is.na(hh_toilet_shared) | is.na(hh_toilet_location)), na.rm = TRUE),
    
    # Household: Livestock -> Counts
    hh_livestock_logic = sum(hh_owns_livestock == "YES" & 
                               (is.na(hh_num_cows_bulls) | is.na(hh_num_goats)), na.rm = TRUE),
    
    # Household: Agri -> Plots
    hh_agri_logic = sum(hh_owns_agri_land == "YES" & 
                          is.na(hh_num_agri_plots), na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  filter(Missing_Count > 0) %>%
  mutate(Type = "Conditional Logic Missing")

final_missing_report <- bind_rows(missing_universal, missing_conditional)

if(nrow(final_missing_report) > 0) {
  cat("WARNING: Unexpected Missing Values Found!\n")
  print(kable(final_missing_report))
} else {
  cat("SUCCESS: No missing data found in Universal vars or Conditional paths.\n")
}

# ==============================================================================
# 5. EXPORT
# ==============================================================================

# Save RDS
saveRDS(missing_df, file = OUTPUT_RDS_PATH)
cat("\nRDS file saved to:", OUTPUT_RDS_PATH, "\n")

# Apply Labels and Save SPSS
# We iterate through the standard names and apply the mapped label
sproxil_spss <- missing_df
for (col_name in names(sproxil_spss)) {
  # Look up the label from our map
  lbl <- variable_labels_map[col_name]
  
  # If a label exists, apply it
  if (!is.na(lbl)) {
    attr(sproxil_spss[[col_name]], "label") <- lbl
  }
}

write_sav(sproxil_spss, OUTPUT_SPSS_PATH)
cat("SPSS file (.sav) saved with labels to:", OUTPUT_SPSS_PATH, "\n")