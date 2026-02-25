# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  05_Derived_Variables.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    November 28, 2025
#
# DESCRIPTION:
# 1. Calculates Wealth Index (PCA) using DHS-compliant asset variables.
# 2. Derives core MIS indicators (ITN Access, IPTp, and Fever Management).
# 3. Applies eligibility logic and masks special codes (98/99) for scientific math.
#
# NOTE: Denominators are filtered by demographic 
#       eligibility (e.g., child-specific indicators only for HH with children <5).
# ==============================================================================

library(tidyverse)
library(haven)
library(FactoMineR)

# --- 1. SETUP ---
INPUT_FILE  <- "Sproxil_Recoded_Final.rds"
OUTPUT_FILE <- "Sproxil_Derived_Final.rds"

if(!file.exists(INPUT_FILE)) stop("Recoded dataset not found. Run Script 04 first.")
df <- readRDS(INPUT_FILE)

message("Step 1: Loaded recoded dataset with ", nrow(df), " respondents.")

# ==============================================================================
# 2. STRATIFIERS (WEALTH, RESIDENCE, ZONE, DEMOGRAPHICS)
# ==============================================================================
message("Step 2: Deriving Stratifiers...")

# --- 2A. WEALTH INDEX (Standard National PCA) ---
asset_vars <- c(
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone", 
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair", 
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", "hh_has_electric_iron",
  "hh_has_generator", "hh_has_fan", "hh_own_watch", "hh_own_mobile_phone", 
  "hh_own_bicycle", "hh_own_motorcycle", "hh_own_car_truck", "hh_has_bank_account"
)

df_pca_prep <- df %>%
  select(all_of(asset_vars)) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | . > 1, 0, .))) %>%
  select(where(~ n_distinct(.) > 1))

pca_res <- PCA(df_pca_prep, graph = FALSE, ncp = 1)

df <- df %>%
  mutate(
    derived_wealth_score = pca_res$ind$coord[,1],
    derived_wealth_quintile = ntile(derived_wealth_score, 5)
  )

# --- 2B. DEMOGRAPHICS & GEOGRAPHY ---
df_derive <- df %>% 
  mutate(
    # Geopolitical Zones
    derived_zone = case_when(
      demo_state %in% c(7, 15, 23, 24, 26, 27, 32) ~ 1, # North Central
      demo_state %in% c(2, 5, 8, 16, 35, 36) ~ 2,       # North East
      demo_state %in% c(18, 19, 20, 21, 22, 34, 37) ~ 3,# North West
      demo_state %in% c(1, 4, 11, 14, 17) ~ 4,          # South East
      demo_state %in% c(3, 6, 9, 10, 12, 33) ~ 5,       # South South
      demo_state %in% c(13, 25, 28, 29, 30, 31) ~ 6,    # South West
      TRUE ~ 9
    ),
    
    # Residence
    derived_residence = case_when(
      residency == 1 ~ 1, # Urban
      residency == 0 ~ 2, # Rural
      TRUE ~ 9
    ),
    
    # Official MIS 5-year Age Groups for Women
    derived_age_group = case_when(
      demo_age >= 15 & demo_age <= 19 ~ 1,
      demo_age >= 20 & demo_age <= 24 ~ 2,
      demo_age >= 25 & demo_age <= 29 ~ 3,
      demo_age >= 30 & demo_age <= 34 ~ 4,
      demo_age >= 35 & demo_age <= 39 ~ 5,
      demo_age >= 40 & demo_age <= 44 ~ 6,
      demo_age >= 45 & demo_age <= 49 ~ 7,
      TRUE ~ 9
    ),
    
    # Education Consolidation (4 Categories matching NMIS)
    derived_edu_cat = case_when(
      demo_edu_level == 0 ~ 1, # No Education
      demo_edu_level == 1 ~ 2, # Primary
      demo_edu_level == 2 ~ 3, # Secondary
      demo_edu_level == 3 ~ 4, # More than secondary
      TRUE ~ 9
    )
  )

# ==============================================================================
# 3. CORE MALARIA INDICATORS (Strict Denominator Enforcement)
# ==============================================================================
message("Step 3: Deriving Core Malaria Indicators...")

df_derive <- df_derive %>% mutate(
  
  # ------------------------------------------------------------------
  # SECTION 3A: PREVENTION (ITN OWNERSHIP & ACCESS)
  # ------------------------------------------------------------------
  derived_hh_has_any_net = ifelse(prev_has_mosquito_nets == 1, 1, 0),
  
  derived_hh_has_itn = case_when(
    prev_has_mosquito_nets == 1 & (prev_is_itn == 1 | prev_net_brand %in% 10:25) ~ 1,
    TRUE ~ 0
  ),
  
  derived_num_itns = ifelse(derived_hh_has_itn == 1, prev_num_mosquito_nets, 0),
  derived_access_itn = case_when(
    hh_total_persons_v1 > 0 & (derived_num_itns * 2 >= hh_total_persons_v1) ~ 1,
    TRUE ~ 0
  ),
  
  # ------------------------------------------------------------------
  # SECTION 3B: MALARIA IN PREGNANCY (ANC & IPTP)
  # ------------------------------------------------------------------
  flag_recent_birth = ifelse(women_births_2020_2025 > 0, 1, 0),
  
  derived_anc_skilled = case_when(
    flag_recent_birth != 1 ~ NA_real_,
    women_anc_seen == 1 & (anc_prov_doc == 1 | anc_prov_nurse == 1 | anc_prov_aux == 1) ~ 1,
    TRUE ~ 0
  ),
  
  derived_anc_4plus = case_when(
    flag_recent_birth != 1 ~ NA_real_, 
    women_anc_seen == 0 ~ 0,
    women_anc_total_visits >= 4 ~ 1,
    TRUE ~ 0
  ),
  
  derived_iptp_1plus = case_when(
    flag_recent_birth != 1 ~ NA_real_,
    women_took_sp_fansidar == 1 & women_sp_fansidar_doses >= 1 ~ 1,
    TRUE ~ 0
  ),
  derived_iptp_2plus = case_when(
    flag_recent_birth != 1 ~ NA_real_,
    women_took_sp_fansidar == 1 & women_sp_fansidar_doses >= 2 ~ 1,
    TRUE ~ 0
  ),
  derived_iptp_3plus = case_when(
    flag_recent_birth != 1 ~ NA_real_,
    women_took_sp_fansidar == 1 & women_sp_fansidar_doses >= 3 ~ 1,
    TRUE ~ 0
  ),
  
  # ------------------------------------------------------------------
  # SECTION 3C: CASE MANAGEMENT (FEVER IN CHILDREN < 5)
  # ------------------------------------------------------------------
  flag_fever = ifelse(women_child_fever_2weeks == 1, 1, 0),
  
  derived_fever_seek_advice = case_when(
    flag_fever != 1 ~ NA_real_,
    women_child_seek_advice == 1 ~ 1,
    TRUE ~ 0
  ),
  
  derived_fever_prompt_care = case_when(
    flag_fever != 1 ~ NA_real_,
    women_child_seek_advice == 1 & women_child_advice_delay_days <= 1 ~ 1,
    TRUE ~ 0
  ),
  
  derived_fever_tested = case_when(
    flag_fever != 1 ~ NA_real_,
    women_child_blood_sample == 1 ~ 1,
    TRUE ~ 0
  ),
  
  # Medications Taken (Table 4.4 Replication)
  derived_fever_took_anti = case_when(
    flag_fever != 1 ~ NA_real_,
    women_child_took_medicine == 1 & (med_act==1 | med_sp==1 | med_chloro==1 | med_amod==1 | med_artesun==1 | med_quinine==1 | med_inject==1 | med_other==1) ~ 1,
    TRUE ~ 0
  ),
  derived_fever_took_act    = case_when(flag_fever != 1 ~ NA_real_, women_child_took_medicine == 1 & med_act == 1 ~ 1, TRUE ~ 0),
  derived_fever_took_sp     = case_when(flag_fever != 1 ~ NA_real_, women_child_took_medicine == 1 & med_sp == 1 ~ 1, TRUE ~ 0),
  derived_fever_took_chloro = case_when(flag_fever != 1 ~ NA_real_, women_child_took_medicine == 1 & med_chloro == 1 ~ 1, TRUE ~ 0),
  derived_fever_took_amod   = case_when(flag_fever != 1 ~ NA_real_, women_child_took_medicine == 1 & med_amod == 1 ~ 1, TRUE ~ 0),
  derived_fever_took_quin   = case_when(flag_fever != 1 ~ NA_real_, women_child_took_medicine == 1 & med_quinine == 1 ~ 1, TRUE ~ 0),
  derived_fever_took_artes  = case_when(flag_fever != 1 ~ NA_real_, women_child_took_medicine == 1 & med_artesun == 1 ~ 1, TRUE ~ 0),
  
  # ------------------------------------------------------------------
  # SECTION 3D: PERCEPTIONS & FEEDBACK (CUSTOM TABLE 6.1)
  # ------------------------------------------------------------------
  derived_percep_affordable = ifelse(treat_drug_affordability %in% c(1, 2), 1, 0),
  derived_gov_effective     = ifelse(feedback_gov_effort_rating %in% c(4, 5), 1, 0),
  derived_exp_stockout      = ifelse(feedback_drug_stockout_6months == 1, 1, 0),
  derived_exp_free_tx       = ifelse(feedback_free_treatment_6months == 1, 1, 0)
)

# ==============================================================================
# 4. FINAL FORMATTING & LABELLING
# ==============================================================================
message("Step 4: Applying Variable and Value Labels...")

val_lbl_wealth <- c("Lowest" = 1, "Second" = 2, "Middle" = 3, "Fourth" = 4, "Highest" = 5)
val_lbl_zone   <- c("North Central" = 1, "North East" = 2, "North West" = 3, "South East" = 4, "South South" = 5, "South West" = 6)
val_lbl_residence <- c("Urban" = 1, "Rural" = 2, "Missing" = 9)
val_lbl_age <- c("15-19" = 1, "20-24" = 2, "25-29" = 3, "30-34" = 4, "35-39" = 5, "40-44" = 6, "45-49" = 7, "Missing" = 9)
val_lbl_edu <- c("No Education" = 1, "Primary" = 2, "Secondary" = 3, "More than secondary" = 4, "Missing" = 9)
val_lbl_yesno <- c("No" = 0, "Yes" = 1)

df_derived <- df_derive %>%
  mutate(
    derived_wealth_quintile = haven::labelled(derived_wealth_quintile, val_lbl_wealth),
    derived_zone            = haven::labelled(derived_zone, val_lbl_zone),
    derived_residence       = haven::labelled(derived_residence, val_lbl_residence),
    derived_age_group       = haven::labelled(derived_age_group, val_lbl_age),
    derived_edu_cat         = haven::labelled(derived_edu_cat, val_lbl_edu)
  ) %>%
  mutate(across(
    .cols = c(
      derived_hh_has_any_net, derived_hh_has_itn, derived_access_itn,
      derived_anc_skilled, derived_anc_4plus, derived_iptp_1plus, derived_iptp_2plus, derived_iptp_3plus,
      derived_fever_seek_advice, derived_fever_prompt_care, derived_fever_tested,
      derived_fever_took_anti, derived_fever_took_act, derived_fever_took_sp, 
      derived_fever_took_chloro, derived_fever_took_amod, derived_fever_took_quin, derived_fever_took_artes,
      derived_percep_affordable, derived_gov_effective, derived_exp_stockout, derived_exp_free_tx
    ),
    .fns = ~ haven::labelled(.x, val_lbl_yesno)
  ))

df_derived <- df_derived %>% 
  set_variable_labels(
   
    # --- Stratifiers ---
    derived_wealth_score    = "Wealth Index Score (PCA Continuous)",
    derived_wealth_quintile = "Wealth Quintile",
    derived_zone            = "Geopolitical Zone",
    derived_residence       = "Type of place of residence",
    derived_age_group       = "Age of woman (5-year groups)",
    derived_edu_cat         = "Highest educational level attended",
    
    # --- ITN ---
    derived_hh_has_any_net  = "Household owns any mosquito net",
    derived_hh_has_itn      = "Household owns at least one ITN",
    derived_num_itns        = "Number of ITNs owned by household",
    derived_access_itn      = "Household has at least one ITN for every two persons",
    
    # --- MIP ---
    flag_recent_birth       = "Denominator Flag: Woman gave birth in last 2 years",
    derived_anc_skilled     = "Received ANC from a skilled provider",
    derived_anc_4plus       = "Received 4+ ANC visits",
    derived_iptp_1plus      = "Received 1+ doses of SP/Fansidar",
    derived_iptp_2plus      = "Received 2+ doses of SP/Fansidar",
    derived_iptp_3plus      = "Received 3+ doses of SP/Fansidar",
    
    # --- Case Management ---
    flag_fever                = "Denominator Flag: Child <5 had fever in last 2 weeks",
    derived_fever_seek_advice = "Sought advice/treatment for fever",
    derived_fever_prompt_care = "Sought treatment same/next day",
    derived_fever_tested      = "Had blood taken for testing",
    derived_fever_took_anti   = "Took any antimalarial for fever",
    derived_fever_took_act    = "Took ACT for fever",
    derived_fever_took_sp     = "Took SP/Fansidar for fever",
    derived_fever_took_chloro = "Took Chloroquine for fever",
    derived_fever_took_amod   = "Took Amodiaquine for fever",
    derived_fever_took_quin   = "Took Quinine for fever",
    derived_fever_took_artes  = "Took Artesunate for fever",
    
    # --- Perceptions ---
    derived_percep_affordable = "Agrees malaria drugs are affordable",
    derived_gov_effective     = "Agrees government efforts are effective",
    derived_exp_stockout      = "Experienced drug stockout in last 6 months",
    derived_exp_free_tx       = "Received free treatment in last 6 months"
  ) %>%
  mutate(across(where(is.labelled), to_factor))

# ==============================================================================
# 5. EXPORT
# ==============================================================================
saveRDS(df_derived, OUTPUT_FILE)
haven::write_sav(df_derived, "Sproxil_Derived_Final.sav")
write.csv(df_derived, "Sproxil_Derived_Final.csv", row.names = FALSE)

cat("✅ SUCCESS: Sproxil_ADerived_Final.rds created.\n")