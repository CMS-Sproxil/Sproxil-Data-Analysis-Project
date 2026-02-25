# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  03_Data_Validation.R
# PURPOSE: Exhaustive Resolution (Holes/Ghosts) & Categorical Audit
# ==============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(sf)

# --- 1. SETUP & LOAD ---
df <- readRDS("Sproxil_Prepared.rds")
metadata <- readRDS("Sproxil_dictionary.rds")
dict <- metadata$mis_data_dictionary
mapping <- metadata$column_mapping

# --- 1.1 DEFINE EXHAUSTIVE UNIVERSAL VARIABLES ---
universal_vars <- c(
  "meta_respondent_id", "meta_status", "demo_state", "demo_lga", "demo_town", 
  "demo_year_of_birth", "demo_gender", "demo_edu_level", "demo_hh_children_under5", 
  "demo_hh_sleeping_rooms", "prev_home_sprayed_interior", "prev_repellent_methods", 
  "prev_first_treatment_location", "prev_time_to_treatment_facility",
  "treat_transport_cost", "treat_hh_fever_last_2weeks", "treat_heard_smc", 
  "treat_vaccine_age_knowledge", "feedback_free_treatment_6months", 
  "feedback_drug_stockout_6months", "feedback_gov_effort_rating",
  "bg_languages", "bg_tv_frequency", "bg_own_smartphone", "bg_internet_ever_used", 
  "bg_religion", "bg_ethnic_group", "bg_heard_malaria_msg_6months", "bg_aware_avoidance",
  "att_rainy_season_only", "att_fever_worry_malaria", "att_malaria_easily_treated", 
  "att_weak_children_die", "att_net_use_mosquito_density", "att_net_use_warm_weather", 
  "att_home_meds_first", "att_full_dose_importance", "att_seek_care_immediate", 
  "att_community_net_usage", "hh_total_persons_v1", "hh_relation_to_head", 
  "hh_drinking_water_source", "hh_other_water_source", "hh_water_location",
  "hh_toilet_type", "hh_cookstove_type", "hh_owns_livestock", "hh_owns_agri_land",
  "hh_floor_material", "hh_roof_material", "hh_wall_material",
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", 
  "hh_has_electric_iron", "hh_has_generator", "hh_has_fan", "hh_own_watch", 
  "hh_own_mobile_phone", "hh_own_bicycle", "hh_own_motorcycle", 
  "hh_own_animal_cart", "hh_own_car_truck", "hh_own_motor_boat", 
  "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account", "hh_mobile_money_usage"
)

# --- BLOCK 1: DEFINE AUDIT LOGIC & FUNCTION ---

# Define the Logic Map (Parent Variable -> Dependent Children)
logic_map <- list(
  # 1. EDUCATION
  demo_edu_level = c("demo_edu_informal"),
  
  # 2. MOSQUITO NETS
  prev_has_mosquito_nets = c("prev_is_itn", "prev_num_mosquito_nets", "prev_months_since_net_obtained", 
                             "prev_net_brand", "prev_net_obtained_how", 
                             "prev_net_obtained_where", "prev_slept_under_net_last_night"),
  
  prev_slept_under_net_last_night = c("prev_num_people_slept_net", "prev_net_not_used_reason"),
  
  # 3. FEVER & TREATMENT
  treat_hh_fever_last_2weeks = c("treat_blood_sample_taken", "treat_test_cost", 
                                 "treat_drug_cost", "treat_drug_purchase_time", 
                                 "treat_drug_affordability"),
  
  treat_heard_smc = c("treat_children_received_smc"),
  
  treat_children_received_smc = c("treat_know_smc_drug"),
  
  treat_vaccine_age_knowledge = c("treat_children_received_vaccine"),
  
  # 4. WOMEN'S REPRODUCTIVE HEALTH
  demo_gender = c("women_ever_given_birth", "women_currently_pregnant"),
  
  women_ever_given_birth = c("women_births_2020_2025", "women_anc_seen"),
  
  women_anc_seen = c("women_anc_provider", "women_anc_location", 
                     "women_anc_first_visit_month", "women_anc_total_visits"),
  
  women_took_sp_fansidar = c("women_sp_fansidar_doses", "women_sp_fansidar_source"),
  
  women_currently_pregnant = c("women_pregnancy_duration_months"),
  
  # 5. CHILD HEALTH (Youngest child under 5)
  women_child_fever_2weeks = c("women_child_blood_sample", "women_child_malaria_diagnosis", 
                               "women_child_seek_advice", "women_child_took_medicine"),
  
  women_child_seek_advice = c("women_child_advice_location", "women_child_first_advice_location", 
                              "women_child_advice_delay_days", "women_child_referral"),
  
  women_child_took_medicine = c("women_child_medicine_type"),
  
  women_child_medicine_type = c("women_child_act_delay", "women_child_act_effective"),
  
  # 6. BACKGROUND & AWARENESS
  bg_internet_ever_used = c("bg_internet_frequency"),
  
  bg_heard_malaria_msg_6months = c("bg_malaria_msg_source"),
  
  bg_aware_avoidance = c("bg_prevention_knowledge"),
  
  # 7. HOUSEHOLD INFRASTRUCTURE & ASSETS
  hh_toilet_type = c("hh_toilet_shared", "hh_toilet_location"),
  
  hh_toilet_shared = c("hh_toilet_share_count"),
  
  hh_owns_livestock = c("hh_num_cows_bulls", "hh_num_goats", "hh_num_other_cattle", 
                        "hh_num_sheep", "hh_num_poultry", "hh_num_horses_donkeys", 
                        "hh_num_pigs", "hh_num_camels"),
  
  hh_owns_agri_land = c("hh_num_agri_plots")
)

# --- BLOCK 2: SPATIAL VALIDATION (With Temporary Shapefile Mapping) ---

# --- 1. HELPER: STANDARDIZATION ---
clean_name_standard <- function(x) {
  x %>% str_to_upper() %>% str_squish()
}

# Load Shapefile (Update path to your actual .shp)
admin_boundaries <- st_read("shapefile/grid3_nga_boundary_vaccwards.shp") %>%
  mutate(clean_shp_state = str_to_upper(str_squish(statename)),
         clean_shp_lga   = str_to_upper(str_squish(lganame)))

# Convert Dataframe to Spatial Object
survey_sf <- df %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
cat("--- Running Spatial Join and Distance Verification ---\n")

# --- 2. PREPARE TEMPORARY SHAPEFILE KEYS ---
# We create these columns JUST for the join and distance check. 
# They will be dropped before the final RDS export.

df_spatial_temp <- df %>%
  mutate(
    # Map reported States to Shapefile naming convention
    state_shp_key = case_when(
      demo_state %in% c("ABUJA", "FEDERAL CAPITAL TERRITORY", "FCT ABUJA") ~ "FCT",
      demo_state == "NASSARAWA" ~ "NASARAWA",
      TRUE ~ clean_name_standard(demo_state)
    ),
    # Map reported LGAs to the "Wrong" spellings in your specific shapefile
    lga_shp_key = case_match(clean_name_standard(demo_lga),
                             "OBI NGWA"                     ~ "OBI NWGA",
                             "UMUNNEOCHI"                   ~ "UMU NNEOCHI",
                             "YENAGOA"                      ~ "YENEGOA",
                             "IGUEBEN"                      ~ "IGUEGBEN",
                             "ABUJA MUNICIPAL AREA COUNCIL" ~ "MUNICIPAL AREA COUNCIL",
                             "SHONGOM"                      ~ "SHOMGOM",
                             "MBAITOLI"                     ~ "MBATOLI",
                             "ONUIMO"                       ~ "UNUIMO",
                             "EZINIHITTE MBAISE"            ~ "EZINIHITTE",
                             "OMUMA"                        ~ "OMUMMA",
                             "AIYEDIRE"                     ~ "AYEDIRE",
                             "KUMI"                         ~ "KURMI",
                             "CHAFE"                        ~ "TSAFE",
                             "DAMBAN"                       ~ "DAMBAM",
                             "TARMUA"                       ~ "TARMUWA",
                             "GARUN MALLAM"                 ~ "GARUN MALAM",
                             "DAWAKIN KUDU"                 ~ "KAWON KUDU",
                             "NASARAWA"                     ~ "NASSARAWA",
                             "BAGUDO"                       ~ "BAGUDU",
                             "IFAKO IJAIYE"                 ~ "IFAKO IJAYE",
                             "MUYA"                         ~ "MUNYA",
                             "ILESA EAST"                   ~ "ILESHA EAST",
                             "ILESA WEST"                   ~ "ILESHA WEST",
                             "WAMAKO"                       ~ "WAMAKKO",
                             .default = clean_name_standard(demo_lga)
    )
  )

# --- 3. CONVERT TO SF OBJECT ---
survey_sf <- df_spatial_temp %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# --- 4. SPATIAL JOIN (POINT IN POLYGON) ---
# admin_boundaries should already be clean_name_standardized
joined_sf <- st_join(survey_sf, 
                     admin_boundaries %>% select(clean_shp_state, clean_shp_lga, urban), 
                     join = st_intersects)

spatial_results <- joined_sf %>%
  st_drop_geometry() %>%
  select(meta_respondent_id, 
         detected_state = clean_shp_state, 
         detected_lga   = clean_shp_lga,
         residency      = urban
         ) %>%
         mutate(residency      = str_to_upper(residency))

# --- 5. DISTANCE CALCULATION (Using the SHP Keys) ---
survey_sf_temp <- survey_sf %>%
  left_join(spatial_results, by = "meta_respondent_id")

lga_mismatch_indices <- which(
  !is.na(survey_sf_temp$detected_lga) & 
    (survey_sf_temp$lga_shp_key != survey_sf_temp$detected_lga)
)

dist_km_vector <- rep(0, nrow(survey_sf))

if(length(lga_mismatch_indices) > 0) {
  # Use shp_keys for distance matching to ensure we find the right polygon
  survey_key_claimed <- paste0(survey_sf_temp$state_shp_key[lga_mismatch_indices], "_", 
                               survey_sf_temp$lga_shp_key[lga_mismatch_indices])
  
  shape_key <- paste0(admin_boundaries$clean_shp_state, "_", admin_boundaries$clean_shp_lga)
  
  target_shape_indices <- match(survey_key_claimed, shape_key)
  valid_targets_mask   <- !is.na(target_shape_indices)
  
  if(any(valid_targets_mask)) {
    dist_calc <- st_distance(
      survey_sf[lga_mismatch_indices[valid_targets_mask], ], 
      st_geometry(admin_boundaries)[target_shape_indices[valid_targets_mask]], 
      by_element = TRUE
    )
    dist_km_vector[lga_mismatch_indices[valid_targets_mask]] <- as.numeric(dist_calc) / 1000
  }
}

# --- 6. INTEGRATE & CLEAN UP ---
df_spatial <- df %>%
  left_join(spatial_results, by = "meta_respondent_id") %>%
  left_join(
    data.frame(
      meta_respondent_id = survey_sf$meta_respondent_id,
      lga_dist_error_km = round(dist_km_vector, 2),
      lga_shp_key = survey_sf_temp$lga_shp_key,     # Keep temporarily for categorization
      state_shp_key = survey_sf_temp$state_shp_key  # Keep temporarily for categorization
    ), 
    by = "meta_respondent_id"
  ) %>%
  mutate(
    gps_state_match = case_when(
      is.na(detected_state) ~ "Outside Boundary",
      state_shp_key == detected_state ~ "Match",
      TRUE ~ "Mismatch"
    ),
    gps_lga_match = case_when(
      is.na(detected_lga) ~ "Outside Boundary",
      lga_shp_key == detected_lga ~ "Match",
      TRUE ~ "Mismatch"
    )
  ) %>%
  # DROP the temporary shapefile keys before moving to Logic Resolution
  select(-lga_shp_key, -state_shp_key)


# --- BLOCK 2: SMART AUDIT FUNCTION ---

generate_variable_audit <- function(data_to_audit) {
  audit_results <- list()
  
  # 1. Audit Universal Variables
  for (var in universal_vars) {
    if(var %in% names(data_to_audit)) {
      audit_results[[var]] <- tibble(
        Variable = var,
        Missing_Holes = sum(is.na(data_to_audit[[var]]) | data_to_audit[[var]] == "", na.rm = TRUE),
        Ghost_Values = 0
      )
    }
  }
  
  # 2. Audit Conditional Variables
  for (parent in names(logic_map)) {
    children <- logic_map[[parent]]
    for (child in children) {
      if(parent %in% names(data_to_audit) & child %in% names(data_to_audit)) {
        
        # SPECIAL CASE: Trigger is "NO"
        if (child == "prev_net_not_used_reason") {
          holes <- sum(data_to_audit[[parent]] == "NO" & (is.na(data_to_audit[[child]]) | data_to_audit[[child]] == ""), na.rm = TRUE)
          ghosts <- sum(data_to_audit[[parent]] != "NO" & !is.na(data_to_audit[[child]]) & data_to_audit[[child]] != "NO RESPONSE", na.rm = TRUE)
        } 
        # STANDARD CASE: Trigger is "YES"
        else {
          holes <- sum(data_to_audit[[parent]] == "YES" & (is.na(data_to_audit[[child]]) | data_to_audit[[child]] == ""), na.rm = TRUE)
          ghosts <- sum(data_to_audit[[parent]] == "NO" & !is.na(data_to_audit[[child]]) & data_to_audit[[child]] != "NO RESPONSE", na.rm = TRUE)
        }
        
        audit_results[[child]] <- tibble(
          Variable = child,
          Missing_Holes = holes,
          Ghost_Values = ghosts
        )
      }
    }
  }
  return(bind_rows(audit_results))
}


# ------------------------------------------------------------------------------
# STEP 2: CATEGORICAL DICTIONARY AUDIT
# ------------------------------------------------------------------------------
cat("--- Running Dictionary Audit (Multi-Select Aware) ---\n")

# Define which columns are allowed to have multiple answers
multi_select_cols <- c(
  "demo_edu_informal", "prev_repellent_methods", "women_anc_provider", 
  "women_anc_location", "women_sp_fansidar_source", "women_child_advice_location",
  "women_child_medicine_type", "bg_languages", "bg_malaria_msg_source", "bg_prevention_knowledge"
)

# Identify the columns to check
audit_cols <- setdiff(intersect(names(df_spatial), names(dict)), c("demo_town", "demo_lga"))

invalid_values_report <- df_spatial %>%
  select(any_of(audit_cols)) %>%
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = "Variable_Name", values_to = "Found_Value") %>%
  filter(!is.na(Found_Value), Found_Value != "") %>%
  # If the variable is multi-select, split the string by semicolon or comma
  mutate(Found_Value = ifelse(Variable_Name %in% multi_select_cols, 
                              str_split(Found_Value, "[;,]+"), 
                              Found_Value)) %>% 
  unnest(Found_Value) %>% 
  mutate(Found_Value = str_trim(Found_Value)) %>%
  filter(Found_Value != "") %>%
  # ------------------------------------------
distinct(Variable_Name, Found_Value) %>%
  rowwise() %>%
  filter(!(toupper(Found_Value) %in% toupper(dict[[Variable_Name]]))) %>%
  ungroup() %>%
  arrange(Variable_Name)

# Save the unique summary
write.csv(invalid_values_report, "Sproxil_Unique_Invalid_Values.csv", row.names = FALSE)
cat("✅ Dictionary Audit Complete. Multi-select answers handled correctly.\n")


cat("--- 2.5. Repairing Categorical Discrepancies ---\n")

df_repair <- df_spatial %>%
  mutate(
    # 1. FIX COLUMN SHIFTS & MIS-CATEGORIZED RESPONSES
    # Religion accidentally entered in Internet Frequency
    bg_internet_frequency = ifelse(bg_internet_frequency == "CHRISTIAN", "NO RESPONSE", bg_internet_frequency),
    
    # Advice location answers in specific women's health columns
    women_child_advice_location = ifelse(women_child_advice_location == "ANTENATAL VISIT TO A HEATH FACILITY", "OTHERS", women_child_advice_location),
    
    
    # Fix typos in dictionary (SHRUBS vs SHUBS)
    hh_cookstove_fuel = str_replace(hh_cookstove_fuel, "STRAW/SHUBS/GRASS", "STRAW/SHRUBS/GRASS"),
    
    treat_know_smc_drug = str_replace(treat_know_smc_drug, "SULFADOXINE-PYRIMETHAMIN", "SULFADOXINE-PYRIMETHAMINE"),
    
    treat_know_smc_drug = str_replace(treat_know_smc_drug, "SULFADOXINE-PYRIMETHAMINEE + AMODIAQUINE", "SULFADOXINE-PYRIMETHAMINE + AMODIAQUINE"),
    
    # Pregnancy and ANC Month formatting
    prev_months_since_net_obtained = case_match(prev_months_since_net_obtained,
                                             "36 MOTHS AGO" ~ "36 MONTHS AGO", 
                                             .default = prev_months_since_net_obtained
    ),

    # Household & Cookstove logic
    hh_toilet_type = case_match(hh_toilet_type,
                                "I DON'T KNOW WHERE" ~ "FLUSH DON'T KNOW WHERE",
                                .default = hh_toilet_type
    ),
    
    hh_cookstove_type = case_match(hh_cookstove_type,
                                   "WOOD"               ~ "OTHER",
                                   "ANIMAL DUNG/WASTE"  ~ "OTHER",
                                   .default = hh_cookstove_type
    )
  )

cat("✅ Categorical Repair Block applied successfully.\n")

# ------------------------------------------------------------------------------
# STEP 3: EXHAUSTIVE LOGIC RESOLUTION
# ------------------------------------------------------------------------------
cat("--- Generating 'BEFORE' Audit Report ---\n")
audit_before <- generate_variable_audit(df_spatial)
write.csv(audit_before, "Sproxil_Audit_BEFORE_Cleaning.csv", row.names = FALSE)

cat("--- Running Exhaustive Resolution (Holes & Ghosts) ---\n")
df_original <- df_repair

df_resolved <- df_repair %>%
  mutate(
    # --- 3.1 EDUCATION ---
    demo_edu_informal = case_when(
      demo_edu_level == "I NEVER HAD ANY FORMAL EDUCATION" & is.na(demo_edu_informal) ~ "NO RESPONSE",
      demo_edu_level != "I NEVER HAD ANY FORMAL EDUCATION" & !is.na(demo_edu_informal) ~ NA_character_,
      TRUE ~ as.character(demo_edu_informal)
    ),
    
    # --- 3.2 NETS & USAGE ---
    across(c(prev_num_mosquito_nets, prev_months_since_net_obtained, prev_net_brand, 
             prev_net_obtained_how, prev_net_obtained_where, prev_slept_under_net_last_night),
           ~ case_when(
             prev_has_mosquito_nets == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             prev_has_mosquito_nets == "NO" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    prev_num_people_slept_net = case_when(
      prev_slept_under_net_last_night == "YES" & (is.na(prev_num_people_slept_net) | prev_num_people_slept_net == "") ~ "NO RESPONSE",
      prev_slept_under_net_last_night != "YES" & !is.na(prev_num_people_slept_net) ~ NA_character_,
      TRUE ~ as.character(prev_num_people_slept_net)
    ),
    
    prev_net_not_used_reason = case_when(
      prev_slept_under_net_last_night == "NO" & (is.na(prev_net_not_used_reason) | prev_net_not_used_reason == "") ~ "NO RESPONSE",
      prev_slept_under_net_last_night != "NO" & !is.na(prev_net_not_used_reason) ~ NA_character_,
      TRUE ~ as.character(prev_net_not_used_reason)
    ),
    
    # --- 3.3 FEVER & TREATMENT ---
    # Added treat_test_cost here
    across(c(treat_blood_sample_taken, treat_test_cost, treat_drug_cost, 
             treat_drug_purchase_time, treat_drug_affordability),
           ~ case_when(
             treat_hh_fever_last_2weeks == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             treat_hh_fever_last_2weeks == "NO" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    # --- 3.4 SMC & VACCINE ---
    treat_children_received_smc = case_when(
      treat_heard_smc == "YES" & (is.na(treat_children_received_smc) | treat_children_received_smc == "") ~ "NO RESPONSE",
      treat_heard_smc == "NO" & !is.na(treat_children_received_smc) ~ NA_character_,
      TRUE ~ as.character(treat_children_received_smc)
    ),
    
    treat_know_smc_drug = case_when(
      str_detect(treat_children_received_smc, "^YES") & (is.na(treat_know_smc_drug) | treat_know_smc_drug == "") ~ "NO RESPONSE",
      !str_detect(treat_children_received_smc, "^YES") & !is.na(treat_know_smc_drug) ~ NA_character_,
      TRUE ~ as.character(treat_know_smc_drug)
    ),
    
    treat_children_received_vaccine = case_when(
      !str_detect(treat_vaccine_age_knowledge, "I DON'T KNOW") & (is.na(treat_children_received_vaccine) | treat_children_received_vaccine == "") ~ "NO RESPONSE",
      str_detect(treat_vaccine_age_knowledge, "I DON'T KNOW") & !is.na(treat_children_received_vaccine) ~ NA_character_,
      TRUE ~ as.character(treat_children_received_vaccine)
    ),
    
    # --- 3.5 WOMEN & CHILD ---
    across(c(women_births_2020_2025, women_anc_seen),
           ~ case_when(
             women_ever_given_birth == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             women_ever_given_birth == "NO" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    across(c(women_anc_provider, women_anc_location, women_anc_first_visit_month, women_anc_total_visits),
           ~ case_when(
             women_anc_seen == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             women_anc_seen == "NO" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    across(c(women_sp_fansidar_doses, women_sp_fansidar_source),
           ~ case_when(
             women_took_sp_fansidar == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             women_took_sp_fansidar == "NO" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    women_currently_pregnant = case_when(
      demo_gender == "FEMALE" & is.na(women_currently_pregnant) ~ "NO RESPONSE",
      demo_gender == "MALE" & !is.na(women_currently_pregnant) ~ NA_character_,
      TRUE ~ as.character(women_currently_pregnant)
    ),
    
    women_pregnancy_duration_months = case_when(
      women_currently_pregnant == "YES" & (is.na(women_pregnancy_duration_months) | women_pregnancy_duration_months == "") ~ "NO RESPONSE",
      women_currently_pregnant != "YES" & !is.na(women_pregnancy_duration_months) ~ NA_character_,
      TRUE ~ as.character(women_pregnancy_duration_months)
    ),
    
    # Child medical details
    across(c(women_child_blood_sample, women_child_malaria_diagnosis, women_child_seek_advice, women_child_took_medicine),
           ~ case_when(
             women_child_fever_2weeks == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             women_child_fever_2weeks == "NO" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    across(c(women_child_advice_location, women_child_first_advice_location, women_child_advice_delay_days, women_child_referral),
           ~ case_when(
             women_child_seek_advice == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             women_child_seek_advice != "YES" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    women_child_medicine_type = case_when(
      women_child_took_medicine == "YES" & (is.na(women_child_medicine_type) | women_child_medicine_type == "") ~ "NO RESPONSE",
      women_child_took_medicine != "YES" & !is.na(women_child_medicine_type) ~ NA_character_,
      TRUE ~ as.character(women_child_medicine_type)
    ),
    
    # --- 3.6 BACKGROUND ---
    bg_internet_frequency = case_when(
      bg_internet_ever_used == "YES" & (is.na(bg_internet_frequency) | bg_internet_frequency == "") ~ "NO RESPONSE",
      bg_internet_ever_used != "YES" & !is.na(bg_internet_frequency) ~ NA_character_,
      TRUE ~ as.character(bg_internet_frequency)
    ),
    
    bg_malaria_msg_source = case_when(
      bg_heard_malaria_msg_6months == "YES" & (is.na(bg_malaria_msg_source) | bg_malaria_msg_source == "") ~ "NO RESPONSE",
      bg_heard_malaria_msg_6months != "YES" & !is.na(bg_malaria_msg_source) ~ NA_character_,
      TRUE ~ as.character(bg_malaria_msg_source)
    ),
    
    bg_prevention_knowledge = case_when(
      bg_aware_avoidance == "YES" & (is.na(bg_prevention_knowledge) | bg_prevention_knowledge == "") ~ "NO RESPONSE",
      bg_aware_avoidance != "YES" & !is.na(bg_prevention_knowledge) ~ NA_character_,
      TRUE ~ as.character(bg_prevention_knowledge)
    ),
    
    # --- 3.7 HOUSEHOLD ---
    hh_toilet_share_count = case_when(
      hh_toilet_shared == "YES" & (is.na(hh_toilet_share_count) | hh_toilet_share_count == "") ~ "NO RESPONSE",
      hh_toilet_shared != "YES" & !is.na(hh_toilet_share_count) ~ NA_character_,
      TRUE ~ as.character(hh_toilet_share_count)
    ),
    
    across(c(hh_num_cows_bulls, hh_num_goats, hh_num_other_cattle, hh_num_sheep, 
             hh_num_poultry, hh_num_horses_donkeys, hh_num_pigs, hh_num_camels),
           ~ case_when(
             hh_owns_livestock == "YES" & (is.na(.) | . == "") ~ "NO RESPONSE",
             hh_owns_livestock == "NO" & !is.na(.) ~ NA_character_,
             TRUE ~ as.character(.)
           )),
    
    hh_num_agri_plots = case_when(
      hh_owns_agri_land == "YES" & (is.na(hh_num_agri_plots) | hh_num_agri_plots == "") ~ "NO RESPONSE",
      hh_owns_agri_land == "NO" & !is.na(hh_num_agri_plots) ~ NA_character_,
      TRUE ~ as.character(hh_num_agri_plots)
    )
  )

# --- 3.8 DEMOGRAPHIC CONSISTENCY CHECK (SMC/Vaccine vs HH Members) ---
df_resolved <- df_resolved %>%
  mutate(
    # If no children under 5 are reported in the household count...
    across(c(treat_children_received_smc, treat_children_received_vaccine, women_child_fever_2weeks),
           ~ case_when(
             # ...but the respondent claimed a child received treatment/had fever...
             (demo_hh_children_under5 == "0" | demo_hh_children_under5 == 0) & . == "YES" ~ "NO RESPONSE",
             
             # ...or if the respondent correctly identifies they have no children <5
             (demo_hh_children_under5 == "0" | demo_hh_children_under5 == 0) & 
               str_detect(., "NO, I DO NOT HAVE A CHILD") ~ NA_character_,
             
             TRUE ~ as.character(.)
           ))
  )

# ------------------------------------------------------------------------------
# STEP 4: UNIVERSAL PATCHING & RESOLUTION LOG
# ------------------------------------------------------------------------------
cat("--- 4. Patching Universal Variables & Generating Resolution Log ---\n")

# 4.1: Final Sweep of Universal Variables
# Any NA found in variables that are NOT part of skip logic is replaced with "NO RESPONSE"
df_resolved <- df_resolved %>%
  mutate(across(any_of(universal_vars), ~ ifelse(is.na(.), "NO RESPONSE", .)))

# 4.2: Create the Comprehensive Resolution Log
cat("   - Comparing original data to resolved data... (Null-Safe Comparison)\n")

res_log <- df_original %>%
  select(meta_respondent_id, everything()) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-meta_respondent_id, names_to = "Variable", values_to = "Original_Value") %>%
  left_join(
    df_resolved %>% 
      select(meta_respondent_id, everything()) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(-meta_respondent_id, names_to = "Variable", values_to = "Resolved_Value"),
    by = c("meta_respondent_id", "Variable")
  ) %>%
  # Use tidyr::replace_na to make NAs comparable as strings
  mutate(
    temp_orig = replace_na(Original_Value, "[[EMPTY]]"),
    temp_res  = replace_na(Resolved_Value, "[[EMPTY]]")
  ) %>%
  filter(temp_orig != temp_res) %>%
  select(-temp_orig, -temp_res) %>%
  mutate(Resolution_Type = case_when(
    is.na(Original_Value) & Resolved_Value == "NO RESPONSE" ~ "HOLE: Missing value filled with NO RESPONSE",
    !is.na(Original_Value) & is.na(Resolved_Value) ~ "GHOST: Skip-logic violation purged to BLANK (NA)",
    TRUE ~ "DATA_PATCH: Categorical or logic correction"
  ))

write.csv(res_log, "Sproxil_Resolution_Log.csv", row.names = FALSE)

cat("--- Generating 'AFTER' Audit Report ---\n")
# Note: In the cleaned data, Missing_Holes should be 0 because NAs were replaced by "NO RESPONSE"
# Ghost_Values should be 0 because they were replaced by NA.
audit_after <- generate_variable_audit(df_resolved)
write.csv(audit_after, "Sproxil_Audit_AFTER_Cleaning.csv", row.names = FALSE)

# Print comparison to console
cat("\nVerification Summary:\n")
cat("Issues Before:", sum(audit_before$Missing_Holes + audit_before$Ghost_Values), "\n")
cat("Issues After: ", sum(audit_after$Missing_Holes + audit_after$Ghost_Values), "\n")

# ------------------------------------------------------------------------------
# STEP 5: FINAL EXPORT WITH LABELS
# ------------------------------------------------------------------------------
cat("--- 5. Exporting Final Datasets ---\n")

# 5.1: Re-apply Variable Labels (For SPSS usability)
# This uses the mapping dictionary to attach full questions as metadata
for (col_name in names(df_resolved)) {
  # Look up the question in the mapping
  full_question <- mapping[col_name]
  
  if (!is.na(full_question)) {
    # We use as.character to remove the name attribute from the vector
    attr(df_resolved[[col_name]], "label") <- as.character(full_question)
  }
}

# 5.2: Save as R Master File (RDS)
saveRDS(df_resolved, "Sproxil_Cleaned_Final.rds")
cat("✅ Final RDS file saved: Sproxil_Cleaned_Final.rds\n")

# 5.3: Save as SPSS File (.sav)
# haven::write_sav handles the variable labels we just attached
# write_sav(df_resolved, "Sproxil_Cleaned_Final.sav")
# cat("✅ Final SPSS file saved: Sproxil_Cleaned_Final.sav\n")

# 5.4: Optional Summary Console Output
cat("\n==================================================================\n")
cat("DATA VALIDATION SUMMARY\n")
cat("------------------------------------------------------------------\n")
cat("Total Records Processed:  ", nrow(df_resolved), "\n")
cat("Total Logical Corrections:", nrow(res_log), "\n")
cat("Logic Violations Purged:  ", sum(res_log$Resolution_Type == "GHOST: Skip-logic violation purged to BLANK (NA)"), "\n")
cat("Missing Gaps Resolved:    ", sum(res_log$Resolution_Type == "HOLE: Missing value filled with NO RESPONSE"), "\n")
cat("==================================================================\n")