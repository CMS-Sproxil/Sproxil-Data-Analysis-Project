# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  04_Data_Recoding.R
# PURPOSE: Section 1 & 2 - Setup and Multi-Select Dummification
# ==============================================================================

library(tidyverse)
library(haven)
library(labelled)
library(stringr)

# --- 1. SETUP ---
cat("--- 1. Loading Resolved Data and Metadata ---\n")
# Using the output from Script 03
df_recode <- readRDS("Sproxil_Cleaned_Final.rds")
metadata <- readRDS("Sproxil_dictionary.rds")
mapping <- metadata$column_mapping

# --- 2. DUMMIFICATION (MULTI-SELECT VARIABLES) ---
cat("--- 2. Creating Dummy Variables for 10 Multi-Select Columns ---\n")

# Helper Function: Ensures "NO RESPONSE" and NA result in NA dummies
dummify <- function(column, pattern) {
  case_when(
    is.na(column) ~ NA_real_,
    column == "NO RESPONSE" ~ 9,
    str_detect(toupper(column), pattern) ~ 1,
    TRUE ~ 0
  )
}

df_dummies <- df_recode %>%
  mutate(
    # 1. Informal Education
    edu_inf_none     = dummify(demo_edu_informal, "NEVER|NONE"),
    edu_inf_adult    = dummify(demo_edu_informal, "ADULT"),
    edu_inf_tsangaya = dummify(demo_edu_informal, "TSANGAYA"),
    edu_inf_quranic  = dummify(demo_edu_informal, "QUARANIC|QUARAN"),
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
    anc_loc_govpost  = dummify(women_anc_location, "GOVERNMENT HEALTH POST"),
    anc_loc_pvt  = dummify(women_anc_location, "PRIVATE"),
    anc_loc_ngohos  = dummify(women_anc_location, "NGO HOSPITAL"),
    anc_loc_ngoclin  = dummify(women_anc_location, "NGO CLINIC"),
    anc_loc_herhome = dummify(women_anc_location, "HER HOME"),
    anc_loc_yourhome = dummify(women_anc_location, "YOUR HOME"),
    anc_loc_other = dummify(women_anc_location, "OTHERS"),
    
    # 5. SP/Fansidar Source
    sp_src_anc     = dummify(women_sp_fansidar_source, "ANTENATAL VISIT"),
    sp_src_nonanc  = dummify(women_sp_fansidar_source, "NON ANTENATAL"),
    sp_src_pharm   = dummify(women_sp_fansidar_source, "PHARMACY|CHEMIST"),
    sp_src_chew    = dummify(women_sp_fansidar_source, "COMMUNITY HEALTH"),
    sp_src_other   = dummify(women_sp_fansidar_source, "OTHERS|TRADITIONAL HEALER|GOVERNMENT HOSPITAL"),

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
    child_adv_other = dummify(women_child_advice_location, "OTHERS"),
        
    # 7. Child Medicine Type
    med_act     = dummify(women_child_medicine_type, "ARTEMISININ"),
    med_sp      = dummify(women_child_medicine_type, "SP/FANSIDAR"),
    med_chloro  = dummify(women_child_medicine_type, "CHLOROQUINE"),
    med_amod    = dummify(women_child_medicine_type, "AMODIAQUINE"),
    med_artesun = dummify(women_child_medicine_type, "ARTESUNATE"),
    med_quinine = dummify(women_child_medicine_type, "QUININE"),
    med_inject  = dummify(women_child_medicine_type, "INJECTION"),
    med_other   = dummify(women_child_medicine_type, "OTHERS|PARACETAMOL/PANADOL|IBUPROFEN|AMOXICILLIN|COTRIMOXAZOLE|ASPIRIN|ACETAMINOPHEN"),
    med_dont    = dummify(women_child_medicine_type, "I DON'T KNOW"),
    
    # 8. Languages
    lang_en = dummify(bg_languages, "ENGLISH"),
    lang_yo = dummify(bg_languages, "YORUBA"),
    lang_ha = dummify(bg_languages, "HAUSA"),
    lang_ig = dummify(bg_languages, "IGBO"),
    lang_ot = dummify(bg_languages, "OTHERS"),
    
    # 9. Malaria Message Source
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
    
    # 10. Prevention Knowledge
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

# --- 3. RECODING SINGLE-SELECT VARIABLES ---
cat("--- 3. Recoding Single-Select Variables to Numeric ---\n")

df_recoded <- df_dummies %>%
  mutate(
    # --- A. BINARY VARIABLES (1=YES, 0=NO, 8=DK, 9=MISSING/NO RESPONSE) ---
    across(c(
      prev_has_mosquito_nets, prev_slept_under_net_last_night, prev_home_sprayed_interior,
      treat_hh_fever_last_2weeks, treat_blood_sample_taken, treat_heard_smc,
      women_ever_given_birth, women_anc_seen, women_took_sp_fansidar,
      women_child_fever_2weeks, women_child_blood_sample, women_child_malaria_diagnosis,
      women_child_seek_advice, women_child_referral, women_child_took_medicine,
      women_child_act_effective, women_currently_pregnant,
      bg_own_smartphone, bg_internet_ever_used, bg_heard_malaria_msg_6months, bg_aware_avoidance, 
      hh_toilet_shared, hh_owns_livestock, hh_owns_agri_land,
      starts_with("hh_has_"), starts_with("hh_own_"), starts_with("hh_mobile_"), residency
    ), ~case_when(
      is.na(.) ~ NA_real_,
      . == "YES" ~ 1,
      . == "NO" ~ 0,
      str_detect(., "DON'T KNOW|NOT SURE") ~ 8,
      TRUE ~ 9 # Catch-all for "NO RESPONSE" or invalid text
    )),
    
    # --- Spatial Validation Variables ---
    across(c(
      gps_state_match, gps_lga_match
    ), ~case_when(
      is.na(.) ~ NA_real_,
      . == "MATCH" ~ 1,
      . == "MISMATCH" ~ 0,
      TRUE ~ 9
    )),
    
    # --- METADATA ---
    meta_status = case_when(
      is.na(meta_status) ~ NA_real_,
      meta_status == "COMPLETED" ~ 1, 
      TRUE ~ 9
    ),
    
    meta_channel = case_when(
      is.na(meta_channel) ~ NA_real_,
      meta_channel == "DIRECT" ~ 1,
      meta_channel == "OUTBOUND" ~ 2,
      TRUE ~ 9
    ),
    
    # --- B. DEMOGRAPHICS ---
    demo_state = case_when(
      is.na(demo_state) ~ NA_real_,
      str_detect(demo_state, "ABIA") ~ 1, str_detect(demo_state, "ADAMAWA") ~ 2,
      str_detect(demo_state, "AKWA IBOM") ~ 3, str_detect(demo_state, "ANAMBRA") ~ 4,
      str_detect(demo_state, "BAUCHI") ~ 5, str_detect(demo_state, "BAYELSA") ~ 6,
      str_detect(demo_state, "BENUE") ~ 7, str_detect(demo_state, "BORNO") ~ 8,
      str_detect(demo_state, "CROSS RIVER") ~ 9, str_detect(demo_state, "DELTA") ~ 10,
      str_detect(demo_state, "EBONYI") ~ 11, str_detect(demo_state, "EDO") ~ 12,
      str_detect(demo_state, "EKITI") ~ 13, str_detect(demo_state, "ENUGU") ~ 14,
      str_detect(demo_state, "FEDERAL CAPITAL TERRITORY") ~ 15, str_detect(demo_state, "GOMBE") ~ 16,
      str_detect(demo_state, "IMO") ~ 17, str_detect(demo_state, "JIGAWA") ~ 18,
      str_detect(demo_state, "KADUNA") ~ 19, str_detect(demo_state, "KANO") ~ 20,
      str_detect(demo_state, "KATSINA") ~ 21, str_detect(demo_state, "KEBBI") ~ 22,
      str_detect(demo_state, "KOGI") ~ 23, str_detect(demo_state, "KWARA") ~ 24,
      str_detect(demo_state, "LAGOS") ~ 25, str_detect(demo_state, "NASARAWA") ~ 26,
      str_detect(demo_state, "NIGER") ~ 27, str_detect(demo_state, "OGUN") ~ 28,
      str_detect(demo_state, "ONDO") ~ 29, str_detect(demo_state, "OSUN") ~ 30,
      str_detect(demo_state, "OYO") ~ 31, str_detect(demo_state, "PLATEAU") ~ 32,
      str_detect(demo_state, "RIVERS") ~ 33, str_detect(demo_state, "SOKOTO") ~ 34,
      str_detect(demo_state, "TARABA") ~ 35, str_detect(demo_state, "YOBE") ~ 36,
      str_detect(demo_state, "ZAMFARA") ~ 37, TRUE ~ 99
    ),
    
    demo_age = case_when(
      is.na(demo_age) ~ NA_real_,
      str_detect(demo_age, "UNDISCLOSED|GREATER THAN 15|>15") ~ 997,
      TRUE ~ as.numeric(str_extract(demo_age, "\\d+"))
    ),    
    
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
      str_detect(demo_edu_level, "HIGHER|POST") ~ 3,
      TRUE ~ 9
    ),
    
    # --- C. NETS & BRANDS ---
    prev_net_brand = case_when(
      is.na(prev_net_brand) ~ NA_real_,
      prev_net_brand == "LLIN" ~ 10, prev_net_brand == "OLYSET LLIN" ~ 11,
      prev_net_brand == "ICONLIFE LLIN" ~ 12, prev_net_brand == "DURANET LLIN" ~ 13,
      prev_net_brand == "NETPROTECT LLIN" ~ 14, prev_net_brand == "BASF INTERCEPTOR LLIN" ~ 15,
      prev_net_brand == "YORKOOL LLIN" ~ 16, prev_net_brand == "MAGNET LLIN" ~ 17,
      prev_net_brand == "DAWAPLUS 2.0 LLIN" ~ 18, prev_net_brand == "ROYAL SECURITY LLIN" ~ 19,
      prev_net_brand == "ROYAL SENTRY LLIN" ~ 20, prev_net_brand == "PERMANET 2.0 LLIN" ~ 21,
      prev_net_brand == "PERMANET 3.0 LLIN" ~ 22, prev_net_brand == "VEERALIN LLIN" ~ 23,
      prev_net_brand == "INTERCEPTOR G2 LLIN" ~ 24, prev_net_brand == "ROYAL GUARD LLIN" ~ 25,
      prev_net_brand == "OTHER/DON'T KNOW BRAND BUT (LLIN)" ~ 26, 
      prev_net_brand == "OTHER TYPE (NOT LLIN)" ~ 95, 
      str_detect(prev_net_brand, "DON'T KNOW") ~ 98, 
      TRUE ~ 99
    ),
    
    prev_is_itn = case_when(
      is.na(prev_is_itn) ~ NA_real_,
      str_detect(prev_is_itn, "YES") ~ 1,
      str_detect(prev_is_itn, "NO") ~ 0,
      str_detect(prev_is_itn, "DON'T KNOW") ~ 8,
      TRUE ~ 9
    ),
    
    
    prev_net_obtained_how = case_when(
      is.na(prev_net_obtained_how) ~ NA_real_,
      str_detect(prev_net_obtained_how, "MASS") ~ 1,
      str_detect(prev_net_obtained_how, "ANTE NATAL") ~ 2,
      str_detect(prev_net_obtained_how, "IMMUNIZATION") ~ 3,
      str_detect(prev_net_obtained_how, "OTHERS") ~ 96,
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
    
    prev_net_not_used_reason = case_when(
      is.na(prev_net_not_used_reason) ~ NA_real_,
      str_detect(prev_net_not_used_reason, "NO MOSQUITOES") ~ 1,
      str_detect(prev_net_not_used_reason, "NO MALARIA") ~ 2,
      str_detect(prev_net_not_used_reason, "TOO HOT") ~ 3,
      str_detect(prev_net_not_used_reason, "DON'T LIKE SMELL") ~ 4,
      str_detect(prev_net_not_used_reason, "CLOSED IN") ~ 5,
      str_detect(prev_net_not_used_reason, "OLD/TORN") ~ 6,
      str_detect(prev_net_not_used_reason, "DIRTY") ~ 7,
      str_detect(prev_net_not_used_reason, "WASHING") ~ 8,
      str_detect(prev_net_not_used_reason, "USUAL USERS") ~ 9,
      str_detect(prev_net_not_used_reason, "NOT NEEDED") ~ 10,
      str_detect(prev_net_not_used_reason, "BED BUGS") ~ 11,
      str_detect(prev_net_not_used_reason, "DON'T KNOW") ~ 98,
      str_detect(prev_net_not_used_reason, "OTHERS") ~ 96,
      TRUE ~ 99
    ),
    
    # --- D. TREATMENT & TIMING ---
    across(c(prev_first_treatment_location, women_child_first_advice_location), ~case_when(    
      is.na(.) ~ NA_real_,
      str_detect(., "GOVERNMENT") ~ 1,
      str_detect(., "PRIVATE HOSPITAL") ~ 2,
      str_detect(., "NGO") ~ 3,
      str_detect(., "MOBILE") ~ 4,
      str_detect(., "PRIVATE DOCTOR") ~ 5,
      str_detect(., "COMMUNITY") ~ 6,
      str_detect(., "PHARMACY") ~ 7,
      str_detect(., "CHEMIST") ~ 8,
      str_detect(., "TRADITIONAL") ~ 10,
      str_detect(., "RELIGIOUS") ~ 11,
      TRUE ~ 99
    )),
    
    prev_time_to_treatment_facility = case_when(
      is.na(prev_time_to_treatment_facility) ~ NA_real_,
      str_detect(prev_time_to_treatment_facility, "LESS") ~ 1,
      str_detect(prev_time_to_treatment_facility, "30 MINS") ~ 2,
      str_detect(prev_time_to_treatment_facility, "MORE") ~ 3,
      str_detect(prev_time_to_treatment_facility, "DON'T KNOW") ~ 8,
      TRUE ~ 9
    ),
    
    # --- E. COSTS & PURCHASES ---
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
      str_detect(., "DON'T KNOW") ~ 98,
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
      str_detect(treat_transport_cost, "DON'T KNOW") ~ 98,
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
    across(c(treat_children_received_smc, treat_children_received_vaccine), ~case_when(
      is.na(.) ~ NA_real_,
      . == "YES" ~ 1,
      str_detect(., "NO AND I HAVE A CHILD") ~ 0,
      str_detect(., "NO, I DO NOT HAVE A CHILD") ~ 6,
      str_detect(., "DON'T KNOW") ~ 8,
      TRUE ~ 9
    )),
    
    treat_know_smc_drug = case_when(
      is.na(treat_know_smc_drug) ~ NA_real_,
      str_detect(treat_know_smc_drug, "SULFADOXINE-PYRIMETHAMINE \\+ AMODIAQUINE") ~ 1,
      str_detect(treat_know_smc_drug, "SULFADOXINE-PYRIMETHAMIN") ~ 2,
      str_detect(treat_know_smc_drug, "ARTEMETHER") ~ 3,
      str_detect(treat_know_smc_drug, "ARTESUNATE") ~ 4,
      str_detect(treat_know_smc_drug, "DIHYDROARTEMISININ") ~ 5,
      str_detect(treat_know_smc_drug, "PROGUANIL") ~ 6,
      str_detect(treat_know_smc_drug, "OTHERS") ~ 96,
      str_detect(treat_know_smc_drug, "DON'T KNOW") ~ 98,
      TRUE ~ 99
    ),
    
    treat_vaccine_age_knowledge = case_when(
      is.na(treat_vaccine_age_knowledge) ~ NA_real_,
      str_detect(treat_vaccine_age_knowledge, "LESS THAN 12") ~ 1,
      str_detect(treat_vaccine_age_knowledge, "^1 YEAR") ~ 2, 
      str_detect(treat_vaccine_age_knowledge, "^2 YEAR") ~ 3, 
      str_detect(treat_vaccine_age_knowledge, "^3 YEAR") ~ 4, 
      str_detect(treat_vaccine_age_knowledge, "^4 YEAR") ~ 5, 
      str_detect(treat_vaccine_age_knowledge, "^5 YEAR") ~ 6, 
      str_detect(treat_vaccine_age_knowledge, "ABOVE 5") ~ 7,
      str_detect(treat_vaccine_age_knowledge, "DON'T KNOW") ~ 98,
      TRUE ~ 99
    ),
    
    # --- G. FEEDBACK & BACKGROUND ---
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
    
    bg_tv_frequency = case_when(
      is.na(bg_tv_frequency) ~ NA_real_,
      str_detect(bg_tv_frequency, "AT LEAST ONCE") ~ 1,
      str_detect(bg_tv_frequency, "WITHOUT WATCHNG") ~ 2,
      str_detect(bg_tv_frequency, "DON'T WATCH TV AT ALL") ~ 3,
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
    
    bg_ethnic_group = case_when(
      is.na(bg_ethnic_group) ~ NA_real_,
      str_detect(bg_ethnic_group, "HAUSA") ~ 1,
      str_detect(bg_ethnic_group, "YORUBA") ~ 2,
      str_detect(bg_ethnic_group, "IGBO") ~ 3,
      str_detect(bg_ethnic_group, "KANURI") ~ 4,
      str_detect(bg_ethnic_group, "IBIBIO") ~ 5,
      str_detect(bg_ethnic_group, "TIV") ~ 6,
      str_detect(bg_ethnic_group, "FULANI") ~ 7,
      str_detect(bg_ethnic_group, "EDO") ~ 8,
      str_detect(bg_ethnic_group, "IJAW") ~ 9,
      str_detect(bg_ethnic_group, "NUPE") ~ 10,
      str_detect(bg_ethnic_group, "EFIK") ~ 11,
      str_detect(bg_ethnic_group, "GWARI") ~ 12,
      str_detect(bg_ethnic_group, "ANANG") ~ 13,
      str_detect(bg_ethnic_group, "ITSEKIRI") ~ 14,
      str_detect(bg_ethnic_group, "JUKUN") ~ 15,
      str_detect(bg_ethnic_group, "BINI") ~ 16,
      str_detect(bg_ethnic_group, "ISOKO") ~ 17,
      str_detect(bg_ethnic_group, "OGONI") ~ 18,
      str_detect(bg_ethnic_group, "IDOMA") ~ 19,
      str_detect(bg_ethnic_group, "GWANDARA") ~ 20,
      str_detect(bg_ethnic_group, "EBIRA") ~ 21,
      str_detect(bg_ethnic_group, "IGBIRA") ~ 22,
      str_detect(bg_ethnic_group, "OTHERS") ~ 96,
      TRUE ~ 99
    ),
    
    # --- H. ATTITUDES (LIKERT) ---
    across(starts_with("att_"), ~case_when(
      is.na(.) ~ NA_real_,
      str_detect(., "DISAGREE") | str_detect(., "LESS THAN HALF") ~ 2,
      str_detect(., "AGREE")    | str_detect(., "MORE THAN HALF") ~ 1,
      str_detect(., "DON'T KNOW|UNCERTAIN") ~ 8,
      TRUE ~ 9
    )),
    
    # --- I. HOUSEHOLD INFRASTRUCTURE ---
    hh_relation_to_head = case_when(
      is.na(hh_relation_to_head) ~ NA_real_,
      str_detect(hh_relation_to_head, "HEAD") ~ 1,
      str_detect(hh_relation_to_head, "WIFE|HUSBAND") ~ 2,
      str_detect(hh_relation_to_head, "SON|DAUGHTER") ~ 3,
      str_detect(hh_relation_to_head, "SON-IN-LAW") ~ 4,
      str_detect(hh_relation_to_head, "GRANDCHILD") ~ 5,
      str_detect(hh_relation_to_head, "PARENT") ~ 6,
      str_detect(hh_relation_to_head, "PARENT-IN-LAW") ~ 7,
      str_detect(hh_relation_to_head, "BROTHER|SISTER") ~ 8,
      str_detect(hh_relation_to_head, "OTHER RELATIVE") ~ 9,
      str_detect(hh_relation_to_head, "ADOPTED") ~ 10,
      str_detect(hh_relation_to_head, "NOT RELATED|CO-WIFE") ~ 11,
      str_detect(hh_relation_to_head, "DON'T KNOW") ~ 98,      
      TRUE ~ 99
    ),
    
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
      str_detect(hh_drinking_water_source, "SATCHET") ~ 92,
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
      str_detect(hh_other_water_source, "SATCHET") ~ 92,
      str_detect(hh_other_water_source, "OTHER") ~ 96,
      TRUE ~ 99
    ),    
    
    hh_toilet_type = case_when(
      is.na(hh_toilet_type) ~ NA_real_,
      str_detect(hh_toilet_type, "FLUSH TO PIPED") ~ 11,
      str_detect(hh_toilet_type, "FLUSH TO SEPTIC") ~ 12,
      str_detect(hh_toilet_type, "FLUSH TO PIT") ~ 13,
      str_detect(hh_toilet_type, "FLUSH TO SOMEWHERE") ~ 14,
      str_detect(hh_toilet_type, "DON'T KNOW WHERE") ~ 15, 
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
    
    across(c(hh_water_location, hh_toilet_location), ~case_when(
      is.na(.) ~ NA_real_,
      str_detect(., "OWN DWELLING") ~ 1,
      str_detect(., "OWN YARD") ~ 2,
      str_detect(., "ELSEWHERE") ~ 3,
      TRUE ~ 9
    )),
    
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
    
    hh_roof_material = case_when(
      is.na(hh_roof_material) ~ NA_real_,
      str_detect(hh_roof_material, "NO ROOF") ~ 0,
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
      str_detect(hh_roof_material, "SHYNGLES") ~ 36,
      str_detect(hh_roof_material, "ASBESTOS") ~ 37,
      str_detect(hh_roof_material, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    hh_wall_material = case_when(
      is.na(hh_wall_material) ~ NA_real_,
      str_detect(hh_wall_material, "NO WALLS") ~ 11,
      str_detect(hh_wall_material, "CANE|PALM|TRUNKS") ~ 12,
      str_detect(hh_wall_material, "DIRT") ~ 13, 
      str_detect(hh_wall_material, "BAMBOO WITH MUD") ~ 21,
      str_detect(hh_wall_material, "STONE WITH MUD") ~ 22,
      str_detect(hh_wall_material, "UNCOVERED ADOBE") ~ 23,      
      str_detect(hh_wall_material, "PLYWOOD") ~ 24,
      str_detect(hh_wall_material, "CARDBOARD") ~ 25,
      str_detect(hh_wall_material, "REUSED WOOD") ~ 26,
      str_detect(hh_wall_material, "CEMENT") ~ 31, 
      str_detect(hh_wall_material, "STONE WITH LIME") ~ 32,
      str_detect(hh_wall_material, "BRICKS") ~ 33,
      str_detect(hh_wall_material, "CEMENT BLOCKS") ~ 34,       
      str_detect(hh_wall_material, "COVERED ADOBE") ~ 35,
      str_detect(hh_wall_material, "WOOD PLANKS/SHINGLES") ~ 36, 
      str_detect(hh_wall_material, "OTHER") ~ 96,
      TRUE ~ 99
    ),
    
    # --- J. NUMERIC PARSING (COUNTS) ---
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
    )),
    
    # --- K. YEAR OF BIRTH ---
    demo_year_of_birth = case_when(
      is.na(demo_year_of_birth) ~ NA_real_,
      str_detect(demo_year_of_birth, "^[0-9]{4}$") ~ as.numeric(demo_year_of_birth),
      str_detect(demo_year_of_birth, "OLDER THAN 15") ~ 9997,
      str_detect(demo_year_of_birth, "DON'T KNOW") ~ 9998,
      TRUE ~ 9999
    ),
    
    # --- L. PREGNANCY TIMING (CONSOLIDATION TO WEEKS) ---
    across(c(women_anc_first_visit_month, women_pregnancy_duration_months), ~case_when(
      is.na(.) | . == "NO RESPONSE" ~ NA_real_,
      str_detect(., "I DON'T KNOW") ~ 98,
      
      # 1. Convert Month strings to specific week midpoints
      str_detect(., "LESS THAN ONE MONTH") ~ 2,
      str_detect(., "1 MONTH")  ~ 4,
      str_detect(., "2 MONTHS") ~ 9,
      str_detect(., "3 MONTHS") ~ 13,
      str_detect(., "4 MONTHS") ~ 17,
      str_detect(., "5 MONTHS") ~ 22,
      str_detect(., "6 MONTHS") ~ 26,
      str_detect(., "7 MONTHS") ~ 30,
      str_detect(., "8 MONTHS") ~ 35,
      str_detect(., "9 MONTHS") ~ 39,
      str_detect(., "10 MONTHS OR MORE") ~ 43,
      str_detect(., "MORE THAN 42 WEEKS") ~ 43,
      
      # 3. If it says "Weeks", just extract the number
      str_detect(., "WEEKS") ~ as.numeric(str_extract(., "\\d+")),
      
      # 4. Catch-all for any remaining raw numbers
      TRUE ~ as.numeric(str_extract(., "\\d+"))
    )),
    
    # --- M. CHILD FEVER DELAY RECODING ---
    women_child_advice_delay_days = case_when(
      is.na(women_child_advice_delay_days) ~ NA_real_,
      str_detect(women_child_advice_delay_days, "SAME DAY") ~ 0,
      str_detect(women_child_advice_delay_days, "1 DAY")    ~ 1,
      str_detect(women_child_advice_delay_days, "2 DAYS")   ~ 2,
      str_detect(women_child_advice_delay_days, "3 DAYS")   ~ 3,
      str_detect(women_child_advice_delay_days, "4 DAYS")   ~ 4,
      str_detect(women_child_advice_delay_days, "5 DAYS")   ~ 5,
      str_detect(women_child_advice_delay_days, "6 DAYS")   ~ 6,
      str_detect(women_child_advice_delay_days, "OVER 7")   ~ 7,
      TRUE ~ 9
    ),
    
    women_child_act_delay = case_when(
      is.na(women_child_act_delay) ~ NA_real_,
      str_detect(women_child_act_delay, "SAME DAY") ~ 0,
      str_detect(women_child_act_delay, "NEXT DAY") ~ 1,
      str_detect(women_child_act_delay, "TWO DAYS") ~ 2,
      str_detect(women_child_act_delay, "THREE OR MORE") ~ 3,
      str_detect(women_child_act_delay, "DON'T KNOW") ~ 8,
      TRUE ~ 9
    )
  )

# Identify the variable type (class) for every column
var_types <- df_recoded %>%
  summarise(across(everything(), ~ paste(class(.), collapse = ", "))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "column_type")

print(var_types)

# 4. Apply Metadata (VALUE & VARIABLE LABELS)

cat("--- 4. Applying Exhaustive Metadata Labels (Variable & Value) ---\n")

# 4.1: Apply Variable Labels (The Question Descriptions)
# We convert the vector to a list so the labelled package understands the mapping
df_labelled <- df_recoded %>%
  set_variable_labels(.labels = as.list(variable_labels), .strict = FALSE)

# 4.2: Apply Specific Value Labels

cat("--- 5. Applying Value Labels to df_recoded ---\n")

# --- A. BATCH APPLY: STANDARD YES/NO (Scale: 0, 1, 8, 9) ---
# These are single-select binary questions
yesno_vars <- c(
  "prev_has_mosquito_nets", "prev_is_itn", "prev_slept_under_net_last_night" ,"prev_home_sprayed_interior",
  "treat_hh_fever_last_2weeks", "treat_blood_sample_taken", "treat_heard_smc",
  "women_ever_given_birth", "women_anc_seen", "women_took_sp_fansidar",
  "women_child_fever_2weeks", "women_child_blood_sample", "women_child_malaria_diagnosis",
  "women_child_seek_advice", "women_child_referral", "women_child_took_medicine",
  "women_child_act_effective", "women_currently_pregnant",
  "bg_own_smartphone", "bg_internet_ever_used", "bg_heard_malaria_msg_6months", 
  "bg_aware_avoidance", "hh_toilet_shared", "hh_owns_livestock", "hh_owns_agri_land",
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", "hh_has_electric_iron",
  "hh_has_generator", "hh_has_fan", "hh_own_watch", "hh_own_mobile_phone",
  "hh_own_bicycle", "hh_own_motorcycle", "hh_own_animal_cart", "hh_own_car_truck",
  "hh_own_motor_boat", "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account",
  "hh_mobile_money_usage, residency"
)

for (v in yesno_vars) {
  if (v %in% names(df_labelled)) val_labels(df_labelled[[v]]) <- value_labels$yesno
}

# --- B. BATCH APPLY: DUMMY VARIABLES (Scale: 0, 1, 9) ---
# Created during dummification (Informal edu, Repellents, Sources, etc.)
dummy_prefixes <- c("edu_inf_", "repel_", "anc_prov_", "anc_loc_", "sp_src_", 
                    "child_adv_", "med_", "lang_", "msg_", "know_")

for (pre in dummy_prefixes) {
  cols <- names(df_labelled)[grep(paste0("^", pre), names(df_labelled))]
  for (col in cols) {
    val_labels(df_labelled[[col]]) <- value_labels$dummy
  }
}

# --- C. METADATA & DEMOGRAPHICS ---
val_labels(df_labelled$meta_status)     <- value_labels$status
val_labels(df_labelled$meta_channel)    <- value_labels$channel
val_labels(df_labelled$gps_state_match) <- value_labels$match
val_labels(df_labelled$gps_lga_match)   <- value_labels$match
val_labels(df_labelled$demo_state)      <- value_labels$states
val_labels(df_labelled$demo_gender)     <- value_labels$gender
val_labels(df_labelled$demo_edu_level)  <- value_labels$education
val_labels(df_labelled$bg_religion)     <- value_labels$religion
val_labels(df_labelled$bg_ethnic_group) <- value_labels$ethnicity
val_labels(df_labelled$demo_age)        <- value_labels$age_special

# --- D. NETS & BRANDS ---
val_labels(df_labelled$prev_net_brand)           <- value_labels$net_brands


val_labels(df_labelled$prev_net_obtained_how)    <- value_labels$net_obtained_how
val_labels(df_labelled$prev_net_obtained_where)  <- value_labels$net_obtained_where
val_labels(df_labelled$prev_net_not_used_reason) <- value_labels$net_no_use

# --- E. TREATMENT, LOCATIONS & COSTS ---
# Locations
val_labels(df_labelled$prev_first_treatment_location)   <- value_labels$locations
val_labels(df_labelled$women_child_first_advice_location) <- value_labels$locations

# Costs (Note the split scale for transport)
val_labels(df_labelled$treat_test_cost)      <- value_labels$costs_standard
val_labels(df_labelled$treat_drug_cost)      <- value_labels$costs_standard
val_labels(df_labelled$treat_transport_cost) <- value_labels$costs_transport

# Timing & Delays
val_labels(df_labelled$prev_time_to_treatment_facility) <- value_labels$time_facility
val_labels(df_labelled$treat_drug_purchase_time)         <- value_labels$drug_purchase
val_labels(df_labelled$women_child_advice_delay_days)    <- value_labels$delays
val_labels(df_labelled$women_child_act_delay)            <- value_labels$delays
val_labels(df_labelled$women_anc_first_visit_month)     <- value_labels$weeks_special
val_labels(df_labelled$women_pregnancy_duration_months) <- value_labels$weeks_special

# --- F. SMC & VACCINE ---
val_labels(df_labelled$treat_children_received_smc)     <- value_labels$smc_eligibility
val_labels(df_labelled$treat_children_received_vaccine) <- value_labels$smc_eligibility
val_labels(df_labelled$treat_know_smc_drug)              <- value_labels$smc_drugs
val_labels(df_labelled$treat_vaccine_age_knowledge)     <- value_labels$vaccine_age

# Feedback (Note the 0/6 Logic)
val_labels(df_labelled$feedback_free_treatment_6months) <- value_labels$feedback_health
val_labels(df_labelled$feedback_drug_stockout_6months)  <- value_labels$feedback_health
val_labels(df_labelled$feedback_gov_effort_rating)      <- value_labels$gov_effort

# --- G. ATTITUDES & MEDIA ---
# Most attitudes use Agree/Disagree
att_agree_vars <- c(
  "att_rainy_season_only", "att_fever_worry_malaria", "att_malaria_easily_treated",
  "att_weak_children_die", "att_net_use_mosquito_density", "att_net_use_warm_weather",
  "att_home_meds_first", "att_full_dose_importance"
)
for (v in att_agree_vars) {
  if (v %in% names(df_labelled)) val_labels(df_labelled[[v]]) <- value_labels$attitudes
}

# Proportion-based attitudes
val_labels(df_labelled$att_seek_care_immediate) <- value_labels$proportions
val_labels(df_labelled$att_community_net_usage) <- value_labels$proportions

# Affordability & Media
val_labels(df_labelled$treat_drug_affordability) <- value_labels$affordability
val_labels(df_labelled$bg_tv_frequency)          <- value_labels$media_frequency
val_labels(df_labelled$bg_internet_frequency)    <- value_labels$media_frequency

# --- H. HOUSEHOLD INFRASTRUCTURE ---
val_labels(df_labelled$hh_relation_to_head)   <- value_labels$relation
val_labels(df_labelled$hh_drinking_water_source) <- value_labels$water_source
val_labels(df_labelled$hh_other_water_source)    <- value_labels$water_source
val_labels(df_labelled$hh_water_location)        <- value_labels$facility_loc
val_labels(df_labelled$hh_toilet_location)       <- value_labels$facility_loc
val_labels(df_labelled$hh_toilet_type)           <- value_labels$toilet_type
val_labels(df_labelled$hh_cookstove_type)        <- value_labels$cookstove_type
val_labels(df_labelled$hh_cookstove_fuel)        <- value_labels$fuel_type
val_labels(df_labelled$hh_floor_material)        <- value_labels$floor_mat
val_labels(df_labelled$hh_roof_material)         <- value_labels$roof_mat
val_labels(df_labelled$hh_wall_material)         <- value_labels$wall_mat

cat("--- Value labels successfully applied. ---\n")

# --- 5. CLEANUP & EXPORT ---
cat("--- 5. Exporting Final Datasets ---\n")

# Remove the metadata, geo-coordinates (for privacy), temporary, and original dummy text columns
df_export <- df_labelled %>%
  select(-any_of(c(
    "meta_latitude", "meta_longitude", "meta_survey_id", "meta_survey_code", "meta_survey_name", "meta_channel","demo_lga", "demo_town", "demo_edu_informal", "prev_repellent_methods", "women_anc_provider", 
    "women_anc_location", "women_sp_fansidar_source", "women_child_advice_location",
    "women_child_medicine_type", "bg_languages", "bg_malaria_msg_source", 
    "bg_prevention_knowledge", "detected_state", "detected_lga", "gps_outside_boundary", "latitude", "longitude" 
  ))) # Remove geo-coordinates columns for privacy

# Final Exports
saveRDS(df_export, "Sproxil_Recoded_Final.rds")
write_sav(df_export, "Sproxil_Recoded_Final.sav")
write_csv(df_export, "Sproxil_Recoded_Final.csv")

cat("✅ RECODING COMPLETE! SAV and RDS files generated.\n")