# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  05_Derived_Variables.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    28 November 2025
#
# DESCRIPTION:
# 1. Calculates Wealth Index (PCA) using specific assets from the Recoded data.
# 2. Derives MIS-standard indicators (ITN coverage, ANC, etc.).
# 3. Formats variables for final tabulation.
#
# NOTE: No Weights available. Analysis is unweighted.
# ==============================================================================

# --- 1. SETUP ---
library(tidyverse)
library(haven)
library(labelled)
library(FactoMineR)

# Input/Output
INPUT_FILE  <- "Sproxil_Recoded.rds"
OUTPUT_FILE <- "Sproxil_Derived.rds"

if(!file.exists(INPUT_FILE)) stop("Recoded dataset not found. Run Script 04 first.")
df <- readRDS(INPUT_FILE)

message("Step 1: Loaded recoded dataset with ", nrow(df), " respondents.")

#==============================================================================
#  2. WEALTH INDEX (PCA)
#==============================================================================
  
message("Step 2: Calculating Wealth Index...")

# Select Asset Variables (Binary 0/1 from Script 04)
# We exclude 'hh_owns_livestock'/'agri_land' from PCA as they are often
# inverse indicators of wealth in urban/peri-urban settings (Sproxil users).
asset_vars <- c(
  "hh_has_electricity", "hh_has_radio", "hh_has_tv",
  "hh_has_non_mobile_phone", "hh_has_computer", "hh_has_refrigerator",
  "hh_has_table", "hh_has_chair", "hh_has_bed", "hh_has_sofa",
  "hh_has_cupboard", "hh_has_ac", "hh_has_electric_iron",
  "hh_has_generator", "hh_has_fan",
  "hh_own_watch", "hh_own_mobile_phone", "hh_own_bicycle",
  "hh_own_motorcycle", "hh_own_animal_cart", "hh_own_car_truck",
  "hh_own_motor_boat", "hh_own_canoe", "hh_own_keke_napep",
  "hh_has_bank_account"
)

# Prepare Data (Ensure no NAs; although Script 04 should have handled this)
df_pca_prep <- df %>%
  select(all_of(asset_vars)) %>%
  #Impute residual NAs to 0 (Not owned) for PCA safety
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

# Remove zero-variance columns (e.g., if nobody owns a canoe, it breaks PCA)
df_pca_prep <- df_pca_prep %>% select(where(~ n_distinct(.) > 1))

# Run PCA
pca_res <- PCA(df_pca_prep, graph = FALSE, ncp = 1)

# Extract Scores & Quintiles
df <- df %>%
  mutate(
    # The raw score (PC1)
    wealth_score = pca_res$ind$coord[,1],
    
    # Stratify into Quintiles
    wealth_quintile = ntile(wealth_score, 5)
  )
    
message("   ...Wealth Index created.")


#==============================================================================
#  3. DERIVED INDICATORS
#==============================================================================
  
message("Step 3: Deriving MIS Indicators...")

df_analysis <- df %>%
  mutate(
    
    # --- A. HOUSEHOLD CHARACTERISTICS (Drinking Water) ---
    
    # 1. Water Time Category (Required for Table 2.1)
    # Logic: If Location is 'On Premises' (1 or 2), time is 0. 
    # Otherwise check minutes.
    derived_water_time_cat = case_when(
      hh_water_location %in% c(1, 2) ~ "Water on premises",
      hh_water_time_trip >= 0 & hh_water_time_trip <= 30 ~ "30 minutes or less",
      hh_water_time_trip > 30 & hh_water_time_trip < 99 ~ "More than 30 minutes",
      hh_water_time_trip >= 98 ~ "Don't know", # Matches Recode DK/Missing
      TRUE ~ "Missing"
    ),
    
    # 2. Main Water Source Category 
    derived_water_category = case_when(
      hh_drinking_water_source %in% c(11, 12, 13, 21, 31, 41, 51, 61, 62) ~ 1,
      # Unimproved:
      hh_drinking_water_source %in% c(32, 42, 96) ~ 2, 
      # Surface:
      hh_drinking_water_source == 81 ~ 0,
      TRUE ~ 99
    ), 
    
    # --- A. HOUSEHOLD CHARACTERISTICS (SANITATION) ---
    
    # 1. Main Sanitation Category
    # Codes: 1=Improved, 2=Unimproved, 3=Open Defecation
    derived_sanitation_category = case_when(
      # Improved: Flush to Sewer/Septic/Pit, VIP, Slab, Compost
      hh_toilet_type %in% c(11, 12, 13, 21, 22, 31) ~ 1,
      
      # Unimproved: Flush elsewhere, Open Pit, Bucket, Hanging, Other
      hh_toilet_type %in% c(14, 15, 23, 41, 51, 96) ~ 2,
      
      # Open Defecation
      hh_toilet_type == 61 ~ 3,
      
      # Missing
      TRUE ~ 99
    ),
    
    # 2. Strict Improved Sanitation (Binary Indicator)
    # Note: Older MIS definition usually requires "Improved" AND "Not Shared"
    # derived_sanitation_improved = if_else(derived_sanitation_category == 1 & hh_toilet_shared == 0, 1, 0, missing=0),
    
    
    # --- A. HOUSEHOLD CHARACTERISTICS (COOKING) ---
    
    # 1. Main Cooking Tech Category
    # 1=Clean, 2=Other/Polluting, 3=None
    derived_cook_tech_cat = case_when(
      # Clean: Electric, Solar, LPG, Natural Gas, Biogas
      hh_cookstove_type %in% c(1, 2, 3, 4, 5) ~ 1,
      
      # Other: Liquid fuel, Solid, Three stone, Other
      hh_cookstove_type %in% c(6, 7, 8, 9, 96) ~ 2,
      
      # No Food
      hh_cookstove_type == 95 ~ 3,
      
      TRUE ~ 9
    ),
    
    # 2. Main Cooking Fuel Category
    # 1=Clean, 2=Solid, 3=Other (Liquid/Plastic), 4=None
    derived_cook_fuel_cat = case_when(
      # Solid: Coal, Charcoal, Wood, Straw, Crop, Dung, Pellets, Garbage Sawdust
      hh_cookstove_fuel %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12) ~ 1,
      
      # Other: Gasoline, Kerosene, Other
      hh_cookstove_fuel %in% c(2, 3, 96) ~ 2,
      
      # No Food
      hh_cookstove_fuel == 95 ~ 3,
      
      TRUE ~ 9
    ),
    
    # 3. Clean Fuel Indicator (Binary)
    # Used for simple "Percentage using clean fuel" stats
    derived_clean_fuel = if_else(derived_cook_tech_cat == 1, 1, 0, missing=0),
    
    
    # --- A. HOUSEHOLD CHARACTERISTICS (COMPOSITION) ---
    
    # Household Size Grouping (1-9+)
    # Note: Using 'hh_total_persons_usually_v3' per table note "de jure/usual members"
    derived_hh_size_cat = case_when(
      hh_total_persons_usually_v3 == 1 ~ 1,
      hh_total_persons_usually_v3 == 2 ~ 2,
      hh_total_persons_usually_v3 == 3 ~ 3,
      hh_total_persons_usually_v3 == 4 ~ 4,
      hh_total_persons_usually_v3 == 5 ~ 5,
      hh_total_persons_usually_v3 == 6 ~ 6,
      hh_total_persons_usually_v3 == 7 ~ 7,
      hh_total_persons_usually_v3 == 8 ~ 8,
      hh_total_persons_usually_v3 >= 9 ~ 9, # Groups 9 and above
      TRUE ~ 99
    ),
    
    # Stratifier for Table 5.3.1
    derived_has_child_u5 = if_else(demo_hh_children_under5 > 0, 1, 0, missing=0),
    
    # --- MEDIA EXPOSURE ---
    
    # 1. Watches TV at least once a week
    # Script 04 Code: 1="At least once a week" (or more often)
    derived_media_tv_weekly = if_else(bg_tv_frequency == 1, 1, 0, missing=0),
    
    # --- INTERNET USAGE  ---
    
    # 1. Used Internet in the last 12 months
    # Logic: If they provided a frequency (1, 2, or 3), they used it.
    # Note: 1=Every day, 2=Weekly, 3=Less often
    derived_internet_use_12m = if_else(bg_internet_frequency %in% c(1, 2, 3), 1, 0, missing=0),
    
    # 2. Internet Frequency Category (For the columns)
    # Mapping Script 04 numeric codes to the table headers
    derived_internet_freq_cat = case_when(
      bg_internet_frequency == 1 ~ 1, # Almost every day
      bg_internet_frequency == 2 ~ 2, # At least once a week
      bg_internet_frequency == 3 ~ 3, # Less than once a week
      # Note: 'Not at all' is usually captured by those who say "No" to ever using, 
      # or filtered out of this specific sub-section of the table.
      TRUE ~ 9
    ),            
    
    # --- B. MALARIA PREVENTION (ITN OWNERSHIP & ACCESS) ---
    
    # 1. Household has at least one ITN (Binary Indicator)
    # Script 04 Codes: 1-5 are verified ITN brands. 96 is 'Other'.
    derived_hh_has_itn = case_when(
      prev_has_mosquito_nets == 1 & prev_net_brand %in% c(1, 2, 3, 4, 5) ~ 1,
      TRUE ~ 0
    ),
    
    # 2. Number of ITNs (Numeric Count)
    # Logic: If the HH has nets and the brand is ITN, we use the total net count as ITN count.
    # Otherwise, ITN count is 0.
    derived_num_itns = if_else(derived_hh_has_itn == 1, prev_num_mosquito_nets, 0, missing=0),
    
    # 3. Access: At least 1 Net for every 2 persons (Binary Indicator)
    # Logic: If (Nets * 2) is greater than or equal to Total People, they have enough access.
    derived_access_any_net = case_when(
      hh_total_persons_v1 > 0 & (prev_num_mosquito_nets * 2 >= hh_total_persons_v1) ~ 1,
      TRUE ~ 0
    ),
    
    # 4. Access: At least 1 ITN for every 2 persons (Binary Indicator)
    # Logic: Same ratio, but using the derived ITN count.
    derived_access_itn = case_when(
      hh_total_persons_v1 > 0 & (derived_num_itns * 2 >= hh_total_persons_v1) ~ 1,
      TRUE ~ 0
    ),
    
    
    # --- B. MALARIA PREVENTION (POPULATION ACCESS) ---
    
    # 1. Theoretical Capacity of ITNs (How many people can the ITNs cover?)
    # Assumption: 1 ITN covers 2 people
    itn_capacity = derived_num_itns * 2,
    
    # 2. Number of People with Access (Numerator for Table 3.3.1)
    # Logic: The number of people covered cannot exceed the household size.
    # If Capacity > Size, everyone has access. If Capacity < Size, only Capacity has access.
    derived_pop_with_access = case_when(
      hh_total_persons_v1 > 0 & itn_capacity >= hh_total_persons_v1 ~ hh_total_persons_v1,
      hh_total_persons_v1 > 0 & itn_capacity <  hh_total_persons_v1 ~ itn_capacity,
      TRUE ~ 0
    ),
    
    
    # --- B. MALARIA PREVENTION (NET SOURCE) ---
    
    # 1. Composite Net Source (Matching Table 3.2.1 Columns)
    # Logic: Prioritize Method (Campaign/ANC) first. If "Other" method, look at Location.
    derived_net_source_composite = case_when(
      # --- Priority 1: The "HOW" (Method) ---
      prev_net_obtained_how == 1 ~ 1, # Mass distribution
      prev_net_obtained_how == 2 ~ 2, # ANC visit
      prev_net_obtained_how == 3 ~ 3, # Immunization visit
      
      # --- Priority 2: The "WHERE" (Location) ---
      # Only look here if they didn't specify a campaign method above
      prev_net_obtained_where == 1 ~ 4,  # Government health facility
      prev_net_obtained_where == 2 ~ 5,  # Private health facility
      prev_net_obtained_where == 3 ~ 6,  # Pharmacy
      prev_net_obtained_where == 4 ~ 7,  # Shop/market
      prev_net_obtained_where == 5 ~ 8,  # Community health worker (CHW)
      prev_net_obtained_where == 6 ~ 9,  # Religious institution
      prev_net_obtained_where == 7 ~ 10, # School
      
      # --- Others ---
      prev_net_obtained_where == 96 | prev_net_obtained_how == 96 ~ 96, # Other
      prev_net_obtained_where == 98 ~ 98, # Don't know
      
      # Default
      TRUE ~ 99 # Missing
    ),
    
    # 2. Net Type Category (For the Table Rows "ITN" vs "Other")
    # Derived from Brand earlier in the script
    derived_net_type = if_else(derived_hh_has_itn == 1, 1, 2, missing=99),
    
    
    # --- C. MATERNAL CARE (ANC PROVIDERS) ---
    
    # 1. Hierarchical ANC Provider (For Table Columns)
    # Logic: Classify by the highest qualification seen.
    # Hierarchy: Doctor > Nurse/Midwife > Aux Midwife > CHW > TBA > Other.
    # If women_anc_seen == 0, then "No ANC".
    derived_anc_provider_composite = case_when(
      women_anc_seen == 0 ~ 8, # Did not see anyone
      
      # 1. Skilled Providers
      anc_prov_doc == 1 ~ 1,   # Doctor
      anc_prov_nurse == 1 ~ 2, # Nurse/Midwife
      anc_prov_aux == 1 ~ 3,   # Auxiliary Midwife
      
      # 2. Unskilled Providers
      anc_prov_chew == 1 ~ 4,  # CHW / CHEW
      anc_prov_tba == 1 ~ 5,   # TBA
      anc_prov_field == 1 ~ 6,   # Fieldworker
      
      # 3. Other (Seen someone, but none of the above matched)
      anc_prov_other == 1 ~ 7, 
      
      TRUE ~ 99
    ),
    
    # 2. Skilled Provider Indicator (Binary for the summary column)
    # Skilled = Doctor, Nurse/Midwife, or Auxiliary Midwife
    derived_anc_skilled = if_else(derived_anc_provider_composite %in% c(1, 2, 3), 1, 0, missing=0),
    
    
    
    # --- C. MATERNAL CARE (ANC VISITS & TIMING) ---
    
    # 1. Number of ANC Visits (Grouped for Table 3.10)
    # Categories: None, 1, 2, 3, 4-7, 8+, Don't know
    derived_anc_visits_cat = case_when(
      women_anc_seen == 0 ~ 0,  # None
      women_anc_total_visits == 1 ~ 1,
      women_anc_total_visits == 2 ~ 2,
      women_anc_total_visits == 3 ~ 3,
      women_anc_total_visits >= 4 & women_anc_total_visits <= 7 ~ 4, # 4-7 visits
      women_anc_total_visits >= 8 & women_anc_total_visits < 98 ~ 5, # 8+ visits
      women_anc_total_visits == 98 ~ 98, # Don't know
      TRUE ~ 99
    ),
    
    # 2. Timing of First ANC Visit (Grouped for Table 3.10)
    # Categories: No ANC, <4 months, 4-7 months, 7+ months (Interpreted as >7)
    # Note: If month is '98' (Don't Know), it goes to separate category
    derived_anc_timing_cat = case_when(
      women_anc_seen == 0 ~ 0, # No ANC
      women_anc_first_visit_month < 4 ~ 1, # < 4 Months
      women_anc_first_visit_month >= 4 & women_anc_first_visit_month <= 7 ~ 2, # 4-7 Months
      women_anc_first_visit_month > 7 & women_anc_first_visit_month < 98 ~ 3, # 7+ Months (8, 9)
      women_anc_first_visit_month == 98 ~ 98, # Don't know
      TRUE ~ 99
    ),
    
    # 3. Clean Month for Median Calculation
    # For the median column, we need a version where "Don't Know" (98) is NA, 
    # so it doesn't skew the math.
    derived_anc_month_clean = if_else(women_anc_first_visit_month < 90, women_anc_first_visit_month, NA_real_),    
    
    
    # --- C. MATERNAL CARE (IPTP DOSES) ---
    
    # 1. Received 1 or more doses of SP/Fansidar
    # Logic: Woman said YES to taking SP AND reported 1 or more doses.
    # Note: Doses < 90 handles excluding "98/Don't Know" or "99/Missing" as 0 (Conservative estimate)
    derived_iptp_1plus = if_else(
      women_took_sp_fansidar == 1 & women_sp_fansidar_doses >= 1 & women_sp_fansidar_doses < 90, 
      1, 0, missing=0
    ),
    
    # 2. Received 2 or more doses
    derived_iptp_2plus = if_else(
      women_took_sp_fansidar == 1 & women_sp_fansidar_doses >= 2 & women_sp_fansidar_doses < 90, 
      1, 0, missing=0
    ),
    
    # 3. Received 3 or more doses
    derived_iptp_3plus = if_else(
      women_took_sp_fansidar == 1 & women_sp_fansidar_doses >= 3 & women_sp_fansidar_doses < 90, 
      1, 0, missing=0
    ),
    
    
    # --- D. CHILD FEVER MANAGEMENT ---
    
    # 1. Child had fever in last 2 weeks (Base Indicator)
    # Denominator: All women with a child under 5
    derived_child_has_fever = if_else(women_child_fever_2weeks == 1, 1, 0, missing=0),
    
    # 2. Sought Advice/Treatment (Numerator for Col 3)
    derived_fever_seek_advice = if_else(women_child_seek_advice == 1, 1, 0, missing=0),
    
    # 3. Prompt Care: Same or Next Day (Numerator for Col 4)
    # Logic: Sought advice AND delay was 0 (Same day) or 1 (Next day)
    derived_fever_prompt_care = if_else(
      women_child_seek_advice == 1 & women_child_advice_delay_days <= 1, 
      1, 0, missing=0
    ),
    
    # 4. Blood Taken for Testing (Numerator for Col 5)
    derived_fever_tested = if_else(women_child_blood_sample == 1, 1, 0, missing=0),
    
    # 5. Diagnosed with Malaria (Numerator for Col 6)
    derived_fever_diagnosed = if_else(women_child_malaria_diagnosis == 1, 1, 0, missing=0),
    
    # 6. Source of Care (Background variable for other tables)
    derived_fever_care_source = case_when(
      women_child_first_advice_location == 1 ~ "Public Sector",
      women_child_first_advice_location == 2 ~ "Private Hospital/Clinic",
      women_child_first_advice_location == 3 ~ "PPM/Pharmacy/Chemist",
      women_child_first_advice_location == 4 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    
    # --- D. CHILD FEVER (SECTOR GROUPING) ---
    
    # 1. Main Provider Sector (For Table Bold Headers)
    # Mapping YOUR Script 04 codes to the new Sector Structure
    # Codes: 1=Public, 2=Private Med, 3=NGO, 4=Other Pvt, 9=Other
    derived_fever_sector = case_when(
      # Public Sector: Government (1), Mobile (4), Community/CHW (6)
      women_child_first_advice_location %in% c(1, 4, 6) ~ 1, 
      
      # Private Medical Sector: Pvt Hospital (2), Private Doctor (5), Pharmacy (7)
      women_child_first_advice_location %in% c(2, 5, 7) ~ 2,         
      
      # NGO Medical Sector: NGO (3)
      women_child_first_advice_location == 3 ~ 3,
      
      # Other Private Sector: Chemist (8), Traditional (10), Religious (11)
      women_child_first_advice_location %in% c(8, 10, 11) ~ 4,         
      
      # Other: "Others"  
      women_child_first_advice_location %in% c(96) ~ 5,
      
      # No Response (99)
      TRUE ~ 9
    ),
    
    
    # --- D. CHILD FEVER (ANTIMALARIAL TYPES) ---
    
    # 1. Any ACT (Artemisinin Combination Therapy)
    derived_med_act = if_else(med_act == 1, 1, 0, missing=0),
    
    # 2. SP/Fansidar
    derived_med_sp = if_else(med_sp == 1, 1, 0, missing=0),
    
    # 3. Chloroquine
    derived_med_chloro = if_else(med_chloro == 1, 1, 0, missing=0),
    
    # 4. Amodiaquine
    derived_med_amod = if_else(med_amod == 1, 1, 0, missing=0),
    
    # 5. Quinine Pills (Since Injection is excluded, all Quinine is Pills)
    derived_med_quinine_pills = if_else(med_quinine == 1, 1, 0, missing=0),
    
    # 6. Artesunate Rectal
    derived_med_art_rectal = if_else(med_artesun == 1, 1, 0, missing=0),
    
    # 7. Artesunate Injection/IV
    # Logic: User Rule -> Generic "INJECTION" counts as Artesunate Injection
    derived_med_art_inj = if_else(
      med_inject == 1, 
      1, 0, missing=0
    ),
    
    # 8. Other Antimalarial
    derived_med_other_anti = if_else(med_other == 1, 1, 0, missing=0),
    
    # 9. DENOMINATOR FLAG: Took Any Antimalarial
    # This is crucial. This table is ONLY for children who took an antimalarial.
    derived_took_any_antimalarial = if_else(
      med_act==1 | med_sp==1 | med_chloro==1 | med_amod==1 | 
        med_artesun==1 | med_quinine==1 | med_inject==1 | med_other==1,
      1, 0, missing=0
    ),
    
    
    # --- D. CHILD FEVER (ACT OUTCOME) ---
    
    # 1. Fever went away after ACT (Numerator)
    # Logic: Mother answered YES to "Did the fever go away?"
    # Note: Denominator (Received ACT) is handled in the Analysis filter.
    derived_act_fever_resolved = if_else(women_child_act_effective == 1, 1, 0, missing=0),
    
    
    # --- F. MEDIA EXPOSURE (MALARIA MESSAGES) ---
    
    # 1. Seen/Heard message in last 6 months (Base Indicator)
    derived_msg_heard_6m = if_else(bg_heard_malaria_msg_6months == 1, 1, 0, missing=0),
    
    # 2. Specific Sources (Cleaning dummies from Script 04)
    # We explicitly convert NAs to 0 to ensure accurate percentages.
    derived_src_radio = if_else(msg_radio == 1, 1, 0, missing=0),
    derived_src_tv = if_else(msg_tv == 1, 1, 0, missing=0),
    derived_src_poster = if_else(msg_poster == 1, 1, 0, missing=0), # Covers Billboard
    derived_src_news = if_else(msg_news == 1, 1, 0, missing=0),        # Newspaper
    derived_src_leaf = if_else(msg_leaf == 1, 1, 0, missing=0),        # leaflet
    derived_src_hcp = if_else(msg_hcp == 1, 1, 0, missing=0),        # Health Care Provider
    derived_src_chw = if_else(msg_chw == 1, 1, 0, missing=0),        # Community Health Worker
    derived_src_social = if_else(msg_social == 1, 1, 0, missing=0),  # Social Media
    derived_src_town = if_else(msg_town == 1, 1, 0, missing=0),      # Town Announcer
    derived_src_ipc = if_else(msg_ipc == 1, 1, 0, missing=0),        # IPC/Friend/Family
    
    
    # --- E. KNOWLEDGE (AVOIDANCE METHODS) ---
    
    # 1. Stated there are ways to avoid malaria (Column 1)
    # Logic: User said Yes (1) to "Are you aware of any way...?"
    derived_know_any_way = if_else(bg_aware_avoidance == 1, 1, 0, missing=0),
    
    # 2. Specific Methods (Columns 3-11)
    # Mapping your specific Script 04 variables
    
    # Sleep under net OR ITN (Combined column in Table 3.11)
    derived_know_nets = if_else(know_net == 1 | know_itn == 1, 1, 0, missing=0),
    
    # Use Repellent
    derived_know_repellent = if_else(know_repel == 1, 1, 0, missing=0),
    
    # Preventive Meds
    derived_know_meds = if_else(know_meds == 1, 1, 0, missing=0),
    
    # Spray House
    derived_know_spray = if_else(know_spray == 1, 1, 0, missing=0),
    
    # Fill Stagnant Water
    derived_know_stagnant = if_else(know_stag == 1, 1, 0, missing=0),
    
    # Clean Surroundings
    derived_know_clean = if_else(know_clean == 1, 1, 0, missing=0),
    
    # Screens on windows
    derived_know_screens = if_else(know_screens == 1, 1, 0, missing=0),
    
    # Other 
    derived_know_other = if_else(know_other == 1, 1, 0, missing=0),
    
    # Don't Know 
    derived_know_dk = if_else(know_dont == 1, 1, 0, missing=0),
    
    
    # --- G. ATTITUDES & BELIEFS ---
    
    # 1. Perceived Susceptibility
    # Q: "People only get malaria in rainy season" -> Favorable: Disagree (2)
    derived_att_rainy_disagree = if_else(att_rainy_season_only == 2, 1, 0, missing=0),
    
    # Q: "When child has fever, worry it's malaria" -> Favorable: Agree (1)
    derived_att_worry_agree = if_else(att_fever_worry_malaria == 1, 1, 0, missing=0),
    
    # Composite: High Perceived Susceptibility (Union of the two above)
    derived_att_susceptibility_comp = if_else(
      derived_att_rainy_disagree == 1 | derived_att_worry_agree == 1, 
      1, 0, missing=0
    ),
    
    # 2. Perceived Severity
    # Q: "Malaria not a problem... easily treated" -> Favorable: Disagree (2)
    derived_att_treated_disagree = if_else(att_malaria_easily_treated == 2, 1, 0, missing=0),
    
    # Q: "Only weak children die from malaria" -> Favorable: Disagree (2)
    derived_att_weak_disagree = if_else(att_weak_children_die == 2, 1, 0, missing=0),
    
    # Composite: High Perceived Severity
    derived_att_severity_comp = if_else(
      derived_att_treated_disagree == 1 | derived_att_weak_disagree == 1, 
      1, 0, missing=0
    ),
    
    # 3. Perceived Self-Efficacy (Net Use)
    # Q: "Only need net when lots of mosquitoes" -> Favorable: Disagree (2)
    # (Implies they use it even when few mosquitoes)
    derived_att_density_disagree = if_else(att_net_use_mosquito_density == 2, 1, 0, missing=0),
    
    # Q: "Do not like net when weather is warm" -> Favorable: Disagree (2)
    # (Implies they use it even when warm)
    derived_att_warm_disagree = if_else(att_net_use_warm_weather == 2, 1, 0, missing=0),
    
    # Composite: High Self-Efficacy
    derived_att_efficacy_comp = if_else(
      derived_att_density_disagree == 1 | derived_att_warm_disagree == 1, 
      1, 0, missing=0
    ),
    
    
    # --- G. ATTITUDES & NORMS (TABLE 5.4.1) ---
    
    # 1. Attitude: Home Meds First (Table Col 2)
    # Question: "Best to start by giving medicine you have at home"
    # The table asks for % who AGREE (Code 1).
    derived_att_home_meds_agree = if_else(att_home_meds_first == 1, 1, 0, missing=0),
    
    # *Helper for Composite*: The favorable behavior is actually to DISAGREE (Code 2).
    att_home_meds_disagree = if_else(att_home_meds_first == 2, 1, 0, missing=0),
    
    # 2. Attitude: Full Dose Importance (Table Col 3)
    # Question: "Important children take full dose" -> Favorable: Agree (1)
    derived_att_full_dose_agree = if_else(att_full_dose_importance == 1, 1, 0, missing=0),
    
    # 3. Composite: Favorable Attitude towards Behaviors (Table Col 4)
    # Per Footnote 1: Disagree Warm Weather OR Disagree Home Meds OR Agree Full Dose
    derived_att_favorable_behavior_comp = if_else(
      derived_att_warm_disagree == 1 | att_home_meds_disagree == 1 | derived_att_full_dose_agree == 1,
      1, 0, missing=0
    ),
    
    # --- COMMUNITY NORMS ---
    
    # 4. Norm: Prompt Care Seeking (Table Col 5)
    # Question: "People in community usually take children... same/next day" -> Agree (1)
    derived_norm_care_agree = if_else(att_seek_care_immediate == 1, 1, 0, missing=0),
    
    # 5. Norm: Net Usage (Table Col 6)
    # Question: "People in community... usually sleep under net every night" -> Agree (1)
    derived_norm_net_agree = if_else(att_community_net_usage == 1, 1, 0, missing=0),
    
    # 6. Composite: Perceived Community Norms (Table Col 7)
    # Per Footnote 2: Agree Care Seeking OR Agree Net Usage
    derived_norm_comp = if_else(
      derived_norm_care_agree == 1 | derived_norm_net_agree == 1,
      1, 0, missing=0
    ),
    
    # --- H. PERCEPTIONS & POLICY (TABLE 6.1) ---
    
    # 1. Perception: Malaria Drugs are Affordable (Column 1)
    # Question: Rate affordability. Codes: 1=Very Aff, 2=Somewhat Aff.
    derived_percep_affordable = if_else(
      treat_drug_affordability %in% c(1, 2), 
      1, 0, missing=0
    ),
    
    # 2. Perception: Government Efforts are Effective (Column 3)
    # Question: Rate gov efforts. Codes: 5=Very Effective, 4=Somewhat Effective.
    derived_gov_effort_effective = if_else(
      feedback_gov_effort_rating %in% c(4, 5), 
      1, 0, missing=0
    ),
    
    # 3. Experience: Received Free Treatment (Proxy for Accessibility - Column 5)
    # Question: Received free malaria treatment at govt facility in last 6 months?
    derived_exp_free_tx = if_else(feedback_free_treatment_6months == 1, 1, 0, missing=0),
    
    # 4. Experience: Drug Stockouts (Negative Indicator of Control Effort)
    # Question: Experienced stockout in last 6 months?
    derived_exp_stockout = if_else(feedback_drug_stockout_6months == 1, 1, 0, missing=0),
    
    
    # --- I. COST & AFFORDABILITY (THE "ADVANTAGE" VARIABLES) ---
    
    # 1. Payment Incidence (Did they pay anything vs Free?)
    # Logic: 0 = Free, 1-16 = Paid, 98/99 = Missing/DK
    derived_paid_transport = case_when(
      treat_transport_cost == 0 ~ 0, # Free
      treat_transport_cost >= 1 & treat_transport_cost <= 16 ~ 1, # Paid
      TRUE ~ NA_real_
    ),
    
    derived_paid_test = case_when(
      treat_test_cost == 0 ~ 0,
      treat_test_cost >= 1 & treat_test_cost <= 16 ~ 1,
      TRUE ~ NA_real_
    ),
    
    derived_paid_drugs = case_when(
      treat_drug_cost == 0 ~ 0,
      treat_drug_cost >= 1 & treat_drug_cost <= 16 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # 2. High Cost Burden (Drugs)
    # Logic: Arbitrary threshold for "Expensive" (e.g., > N2,000)
    # Codes: 3 is N2000-N2999. So codes 3 and above are >N2000.
    derived_drug_cost_high = case_when(
      treat_drug_cost >= 3 & treat_drug_cost <= 16 ~ 1,
      treat_drug_cost < 3 ~ 0,
      TRUE ~ 0
    ),
    
    # 3. Perceived Affordability (Binary)
    # Logic: 1="Very Affordable", 2="Somewhat". 
    # Negative: 4="Somewhat Exp", 5="Very Exp".
    derived_perceived_expensive = if_else(treat_drug_affordability %in% c(4, 5), 1, 0, missing=0),
    
  ) %>%
  
#---------------------------------------------------------
#  REMOVE INTERMEDIATE HELPER COLUMNS
#---------------------------------------------------------
  
  select(-c(
    pot_users,                # From ITN Access calculation
    itn_capacity,             # From Pop Access calculation
    att_home_meds_disagree    # From Attitude Composite calculation
  ))

# ==============================================================================
# 4. FINAL FORMATTING & LABELLING
# ==============================================================================
message("Step 4: Final Formatting and Labelling...")

# ------------------------------------------------------------------------------
# 4.1 DEFINE VALUE LABEL LISTS
# ------------------------------------------------------------------------------
# Wealth quintile
val_lbl_wealth <- c(
  "Lowest"  = 1,
  "Second"  = 2,
  "Middle"  = 3,
  "Fourth"  = 4,
  "Highest" = 5
)

# Generic Binary
val_lbl_yesno <- c("No" = 0, "Yes" = 1)

# Water & Sanitation
val_lbl_water_time <- c(
  "Water on premises" = 1, 
  "30 minutes or less" = 2, 
  "More than 30 minutes" = 3, 
  "Don't know" = 98, 
  "Missing" = 99
)

val_lbl_water_source <- c(
  "Improved" = 1, 
  "Unimproved" = 2, 
  "Surface water" = 3, # Note: Script 05 logic used 3 for Surface, 0/2 for others
  "Missing" = 99
)

val_lbl_sanitation <- c(
  "Improved" = 1, 
  "Unimproved" = 2, 
  "Open defecation" = 3,
  "Missing" = 99
)

# Household
val_lbl_cook_tech <- c(
  "Clean fuels/tech" = 1, 
  "Other fuels/tech" = 2, 
  "No food cooked" = 3, 
  "Missing" = 9
)

val_lbl_cook_fuel <- c(
  "Clean" = 1, 
  "Solid fuels" = 2, 
  "Other" = 3, 
  "No food cooked" = 4, 
  "Missing" = 9
)

val_lbl_hh_size <- c(
  "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, 
  "6" = 6, "7" = 7, "8" = 8, "9+" = 9, "Missing" = 99
)

# Nets
val_lbl_net_src <- c(
  "Mass distribution" = 1, "ANC visit" = 2, "Immunization" = 3,
  "Govt facility" = 4, "Pvt facility" = 5, "Pharmacy" = 6,
  "Shop/market" = 7, "CHW" = 8, "Religious Inst" = 9, "School" = 10,
  "Other" = 96, "Don't know" = 98, "Missing" = 99
)

val_lbl_net_type <- c("ITN" = 1, "Other" = 2)

# Maternal
val_lbl_anc_prov <- c(
  "Doctor" = 1, "Nurse/midwife" = 2, "Aux midwife" = 3,
  "CHW" = 4, "TBA" = 5, "Other" = 6, "No ANC" = 7
)

val_lbl_anc_vis <- c(
  "None" = 0, "1" = 1, "2" = 2, "3" = 3, 
  "4-7" = 4, "8+" = 5, "Don't know" = 98
)

val_lbl_anc_time <- c(
  "No ANC" = 0, "< 4 months" = 1, "4-7 months" = 2, "7+ months" = 3, "Don't know" = 98
)

# Fever
val_lbl_fever_sec <- c(
  "Public sector" = 1, "Private medical" = 2, 
  "NGO" = 3, "Other private" = 4, "Other" = 5, "Missing" = 9
)

# Media
val_lbl_inet_freq <- c(
  "Almost every day" = 1, "At least once a week" = 2, 
  "Less than once a week" = 3, "Not at all" = 9
)

val_lbl_payment <- c(
  "Free" = 0, 
  "Paid" = 1
)

val_lbl_literacy <- c(
  "Not Literate" = 0,
  "Literate" = 1
)

# ------------------------------------------------------------------------------
# 4.2 APPLY LABELS
# ------------------------------------------------------------------------------

df_derived <- df_derive %>%
  set_value_labels(
    wealth_quintile = val_lbl_wealth,
    # Household
    derived_water_time_cat = val_lbl_water_time,
    derived_water_category = val_lbl_water_source,
    derived_sanitation_category = val_lbl_sanitation,
    derived_cook_tech_cat = val_lbl_cook_tech,
    derived_cook_fuel_cat = val_lbl_cook_fuel,
    derived_hh_size_cat = val_lbl_hh_size,
    
    # Nets
    derived_net_source_composite = val_lbl_net_src,
    derived_net_type = val_lbl_net_type,

    # Maternal
    derived_anc_provider_composite = val_lbl_anc_prov,
    derived_anc_visits_cat = val_lbl_anc_vis,
    derived_anc_timing_cat = val_lbl_anc_time,
    
    # Fever
    derived_fever_sector = val_lbl_fever_sec,
    
    # Media
    derived_internet_freq_cat = val_lbl_inet_freq,
    
    # Affordability
    derived_paid_transport = val_lbl_payment,
    derived_paid_test = val_lbl_payment,
    derived_paid_drugs = val_lbl_payment,

  ) %>%
  
  # Apply Yes/No to ALL binary indicators created
  set_value_labels(
    .labels = val_lbl_yesno,
    .strict = FALSE,
    derived_water_improved, derived_sanitation_improved, derived_clean_fuel,
    derived_hh_has_itn, derived_access_any_net, derived_access_itn,
    derived_anc_4plus, derived_anc_skilled, 
    derived_iptp_1plus, derived_iptp_2plus, derived_iptp_3plus,
    derived_child_has_fever, derived_fever_seek_advice, derived_fever_prompt_care,
    derived_fever_tested, derived_fever_diagnosed, derived_fever_referred,
    derived_took_any_antimalarial, derived_act_fever_resolved,
    derived_msg_heard_6m, derived_internet_use_12m,
    derived_media_tv_weekly, derived_drug_cost_high, derived_perceived_expensive,
    # Also apply to the specific drug/knowledge/source binaries
    starts_with("derived_med_"), 
    starts_with("derived_src_"),
    starts_with("derived_know_"),
    starts_with("derived_att_"),
    starts_with("derived_percep_"),
    starts_with("derived_gov_"),
    starts_with("derived_exp_")
  ) %>%
  
# ------------------------------------------------------------------------------
# 4.3 CONVERT TO FACTORS (Final Step)
# ------------------------------------------------------------------------------
# This ensures they export to SPSS/R as labelled factors, not just numbers
mutate(across(where(is.labelled), to_factor)) %>%
  
# ------------------------------------------------------------------------------
# 4.4 APPLY VARIABLE DESCRIPTIONS
# ------------------------------------------------------------------------------
set_variable_labels(
  # --- Wealth & Background ---
  wealth_score = "Wealth Index Score (PCA)",
  wealth_quintile = "Wealth Quintile (1=Poorest, 5=Richest)",
  derived_has_child_u5 = "Living children under age 5",
  
  # --- Household ---
  derived_water_time_cat = "Time to obtain drinking water (round trip)",
  derived_water_category = "Main Source of Drinking Water",
  derived_water_improved = "Uses improved drinking water source",
  derived_sanitation_category = "Main Sanitation Facility Type",
  derived_sanitation_improved = "Uses improved sanitation (non-shared)",
  derived_cook_tech_cat = "Main cooking technology Group",
  derived_cook_fuel_cat = "Cooking fuel Group",
  derived_clean_fuel = "Uses clean fuel for cooking",
  
  # --- ITN (Possession & Access) ---
  derived_hh_has_itn = "Household has at least one ITN",
  derived_num_itns = "Number of ITNs in household",
  derived_access_any_net = "Access: 1 net per 2 persons (Any Net)",
  derived_access_itn = "Access: 1 net per 2 persons (ITN)",
  derived_pop_with_access = "Number of household members with access to an ITN",
  derived_itn_use_pop_count = "Number of household members who slept under an ITN",
  derived_net_source_composite = "Source of mosquito net",
  derived_net_type = "Type of mosquito net",
  derived_net_no_use_reason = "Main reason mosquito net was not used",
  
  # --- Maternal Care ---
  derived_anc_provider_composite = "Provider of antenatal care (Hierarchy)",
  derived_anc_skilled = "Received ANC from skilled provider",
  derived_anc_visits_cat = "Number of ANC visits (Grouped)",
  derived_anc_timing_cat = "Months pregnant at first ANC visit (Grouped)",
  derived_iptp_1plus = "Received 1+ doses of SP/Fansidar",
  derived_iptp_2plus = "Received 2+ doses of SP/Fansidar",
  derived_iptp_3plus = "Received 3+ doses of SP/Fansidar",
  
  # --- Child Fever ---
  derived_child_has_fever = "Child had fever in last 2 weeks",
  derived_fever_seek_advice = "Sought advice or treatment",
  derived_fever_prompt_care = "Sought advice same or next day",
  derived_fever_tested = "Child had blood taken for testing",
  derived_fever_diagnosed = "Child diagnosed with malaria",
  derived_fever_referred = "Child referred to higher level of care",
  derived_fever_sector = "Sector of fever advice/treatment",
  derived_act_fever_resolved = "Fever went away after taking ACT",
  
  # --- Medications ---
  derived_took_any_antimalarial = "Child took any antimalarial drug",
  derived_med_act = "Took any ACT",
  derived_med_sp = "Took SP/Fansidar",
  derived_med_chloro = "Took Chloroquine",
  derived_med_amod = "Took Amodiaquine",
  derived_med_quinine_pills = "Took Quinine pills",
  derived_med_art_rectal = "Took Artesunate rectal",
  derived_med_art_inj = "Took Artesunate injection/IV",
  derived_med_other_anti = "Took other antimalarial",
  
  # --- Media & Knowledge ---
  derived_media_tv_weekly = "Watches TV at least once a week",
  derived_msg_heard_6m = "Heard malaria message in past 6 months",
  derived_internet_use_12m = "Used Internet in the last 12 months",
  derived_internet_freq_cat = "Frequency of Internet use (last month)",
  
  # --- Knowledge Sources ---
  derived_src_radio = "Source: Radio",
  derived_src_tv = "Source: Television",
  derived_src_poster = "Source: Poster/Billboard",
  derived_src_hcp = "Source: Health care provider",
  derived_src_chw = "Source: Community health worker",
  derived_src_social = "Source: Social media",
  derived_src_town = "Source: Town announcer",
  derived_src_ipc = "Source: IPC/Family/Friends",
  
  # --- Knowledge Methods ---
  derived_know_any_way = "States there are ways to avoid malaria",
  derived_know_nets = "Method: Sleep under net/ITN",
  derived_know_repellent = "Method: Use mosquito repellent",
  derived_know_meds = "Method: Take preventive medication",
  derived_know_spray = "Method: Spray house",
  derived_know_stagnant = "Method: Fill in stagnant water",
  derived_know_clean = "Method: Keep surroundings clean",
  derived_know_screens = "Method: Screen windows/doors",
  derived_know_dk = "Method: Don't know specific ways",
  
  # --- Attitudes ---
  derived_att_rainy_disagree = "Disagree: Malaria only in rainy season",
  derived_att_worry_agree = "Agree: Worry fever is malaria",
  derived_att_susceptibility_comp = "Composite: Perceived Susceptibility",
  derived_att_treated_disagree = "Disagree: Malaria easily treated",
  derived_att_weak_disagree = "Disagree: Only weak children die",
  derived_att_severity_comp = "Composite: Perceived Severity",
  derived_att_density_disagree = "Disagree: Net use only if many mosquitoes",
  derived_att_warm_disagree = "Disagree: Dislike net in warm weather",
  derived_att_efficacy_comp = "Composite: Perceived Self-Efficacy",
  
  # --- Norms ---
  derived_att_home_meds_agree = "Agree: Best to give home medicine first",
  derived_att_full_dose_agree = "Agree: Important to take full dose",
  derived_att_favorable_behavior_comp = "Composite: Favorable Attitude (Behaviors)",
  derived_norm_care_agree = "Agree: Community seeks prompt care",
  derived_norm_net_agree = "Agree: Community uses nets nightly",
  derived_norm_comp = "Composite: Positive Community Norms",
  
  # --- Perceptions ---
  derived_percep_affordable = "Perceives drugs as Affordable",
  derived_gov_effort_effective = "Rates Govt Malaria Effort as Effective",
  derived_exp_free_tx = "Received Free Treatment (Last 6m)",
  derived_exp_stockout = "Experienced Drug Stockout (Last 6m)"
  
)

#==============================================================================
#  5. EXPORT
#==============================================================================
  
saveRDS(df_derived, OUTPUT_FILE)
write_sav(df_derived, gsub("rds", "sav", OUTPUT_FILE))

cat("SUCCESS: Derived variables created. \n")
cat("Output saved to:", OUTPUT_FILE, "\n")
glimpse(df_derived)