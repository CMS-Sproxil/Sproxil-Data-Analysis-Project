# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  04_Final_Verification_Audit.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    27 November 2025
# DESCRIPTION:
# This script audits the "Resolved" dataset to prove:
# 1. No Universal Variables are missing.
# 2. No Conditional Variables are missing (if the Logic Trigger was met).
# 3. Reports on the volume of "NO RESPONSE" injections.
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(stringr)

# --- 1. LOAD RESOLVED DATA ---
INPUT_FILE <- "Sproxil_Resolved.rds"

if(!file.exists(INPUT_FILE)) stop("Resolved file not found. Run Resolution Script first.")
df_audit <- readRDS(INPUT_FILE)

cat(paste("AUDIT START: Checking", nrow(df_audit), "rows and", ncol(df_audit), "columns.\n\n"))

# ==============================================================================
# TEST 1: UNIVERSAL VARIABLES CHECK
# ==============================================================================
# Expectation: 0 NA values. Everything should be real data, "NO RESPONSE".

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


cat("--- TEST 1: Universal Variable Completeness ---\n")
univ_failures <- df_audit %>%
  select(any_of(universal_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  filter(NA_Count > 0)

if(nrow(univ_failures) == 0) {
  cat("✅ PASS: All Universal Variables are fully populated.\n")
} else {
  cat("❌ FAIL: Found residual NAs in Universal Variables:\n")
  print(kable(univ_failures))
}
cat("\n")

# ==============================================================================
# TEST 2: SKIP LOGIC RESOLUTION CHECK (OMISSION AUDIT)
# ==============================================================================
# Expectation: 0 NA values. Gaps should have been filled with "NO RESPONSE".

cat("--- TEST 2: Conditional Logic Resolution (Omission Check) ---\n")

omission_audit <- df_audit %>%
  summarise(
    # Nets: Trigger YES -> Target NA?
    Fail_Nets_Brand = sum(prev_has_mosquito_nets == "YES" & is.na(prev_net_brand), na.rm=T),
    
    # Treatment: Trigger YES -> Target NA?
    Fail_Blood_Sample = sum(treat_hh_fever_last_2weeks == "YES" & is.na(treat_blood_sample_taken), na.rm=T),
    
    # Women: Trigger FEMALE -> Target NA?
    Fail_Birth_Status = sum(demo_gender == "FEMALE" & is.na(women_ever_given_birth), na.rm=T),
    
    # Women: Trigger YES -> Target NA?
    Fail_Pregnancy_Dur = sum(women_currently_pregnant == "YES" & is.na(women_pregnancy_duration_months), na.rm=T),
    
    # Internet: Trigger YES -> Target NA?
    Fail_Internet_Freq = sum(bg_internet_ever_used == "YES" & is.na(bg_internet_frequency), na.rm=T),
    
    # Livestock: Trigger YES -> Target NA?
    Fail_Cow_Count = sum(hh_owns_livestock == "YES" & is.na(hh_num_cows_bulls), na.rm=T)
  ) %>%
  pivot_longer(everything(), names_to = "Logic_Path", values_to = "Failure_Count") %>%
  filter(Failure_Count > 0)

if(nrow(omission_audit) == 0) {
  cat("✅ PASS: All Skip Logic gaps have been successfully resolved/filled.\n")
} else {
  cat("❌ FAIL: Found persistent NAs in conditional paths (Resolution Script missed these):\n")
  print(kable(omission_audit))
}
cat("\n")

# ==============================================================================
# TEST 3: IMPUTATION VOLUME REPORT
# ==============================================================================
# Information: How much "NO RESPONSE" / -99 did we actually inject?

cat("--- TEST 3: Imputation Volume Report (Info Only) ---\n")

imputation_stats <- df_audit %>%
  summarise(
    # Text Injections
    "Total 'NO RESPONSE'" = sum(across(where(is.character), ~ . == "NO RESPONSE"), na.rm = TRUE),
    
    # Specific Checks
    "Unknown Net Brands" = sum(prev_net_brand == "NO RESPONSE", na.rm=T),
    "Unknown Pregnancy Duration" = sum(women_pregnancy_duration_months == "NO RESPONSE", na.rm=T)
  ) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Count")

print(kable(imputation_stats))
cat("\n")

# ==============================================================================
# TEST 4: LOGIC FLAGS RE-RUN (COMMISSION AUDIT)
# ==============================================================================
# Expectation: This checks for INCONSISTENCIES (e.g., Male + Pregnant).
# If this fails, it means you have "Dirty" data that needs blanking, not filling.

cat("--- TEST 4: Commission Error Audit (Inconsistencies) ---\n")

commission_audit <- df_audit %>%
  summarise(
    # Male answering Women's Qs
    Error_Male_Preg = sum(demo_gender == "MALE" & !is.na(women_ever_given_birth), na.rm=T),
    
    # No Nets but has Brand
    Error_NoNets_Data = sum(prev_has_mosquito_nets == "NO" & !is.na(prev_net_brand), na.rm=T),
    
    # No Fever but has Blood Sample info
    Error_NoFever_Data = sum(treat_hh_fever_last_2weeks == "NO" & !is.na(treat_blood_sample_taken), na.rm=T),
    
    # No Livestock but has Counts
    Error_NoStock_Count = sum(hh_owns_livestock == "NO" & !is.na(hh_num_cows_bulls), na.rm=T)
  ) %>%
  pivot_longer(everything(), names_to = "Inconsistency", values_to = "Count") 

if(sum(commission_audit$Count) == 0) {
  cat("✅ PASS: No logical inconsistencies found.\n")
} else {
  cat("⚠️ WARNING: Logical inconsistencies exist (Commission Errors).\n")
  cat("   (This means respondents answered questions they should have skipped).\n")
  cat("   (Resolution script fills gaps; it does not delete extra data).\n")
  print(kable(commission_audit %>% filter(Count > 0)))
}

cat("\nAUDIT COMPLETE.\n")