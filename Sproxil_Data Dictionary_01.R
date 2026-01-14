# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Data_Preparation.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    November 26, 2025
#
# DESCRIPTION:
# 1. Defines the "Data Dictionary" based on the Sproxil Questionnaire.
# 2. Defines the "Standard Names" to rename raw columns.
#
# OUTPUT: "Sproxil_mis_dictionary.rds"
# ==============================================================================

library(dplyr)
library(tidyr)
library(knitr)

# ==============================================================================
# 1. DATA DICTIONARY (VALID ANSWER KEY)
# ==============================================================================
# Converted to ALL CAPS to match the cleaning script standardization.

mis_data_dictionary <- list(
  
  # --- Meta Data ---
  meta_status = c("USED", "IN-PROGRESS"),
  
  # --- Section 1: Demographics ---
  demo_gender = c("MALE", "FEMALE"),
  
  # Page 2, Q4
  demo_edu_level = c(
    "I NEVER HAD ANY FORMAL EDUCATION", 
    "PRIMARY", 
    "SECONDARY", 
    "HIGHER (POST SECONDARY) EDUCATION", "POSTGRADUATE", "VOCATIONAL EDUCATION"
  ),
  
  # Page 2, Q5
  demo_edu_informal = c(
    "I NEVER HAD ANY FORMAL EDUCATION", 
    "ADULT EDUCATION", 
    "TSANGAYA", 
    "QUARANIC", 
    "OTHER"
  ),
  
  # --- Section 2: Malaria Prevention ---
  # Page 2, Q8
  prev_has_mosquito_nets = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 3, Q10 (Time)
  prev_months_since_net_obtained = c(
    "LESS THAN A MONTH", "1 MONTH AGO", "2 MONTHS AGO", "3 MONTHS AGO", 
    "4 MONTHS AGO", "5 MONTHS AGO", "6 MONTHS AGO", "7 MONTHS AGO", 
    "8 MONTHS AGO", "9 MONTHS AGO", "10 MONTHS AGO", "11 MONTHS AGO", 
    "12 MONTHS AGO", "13 MONTHS AGO", "14 MONTHS AGO", "15 MONTHS AGO", 
    "16 MONTHS AGO", "17 MONTHS AGO", "18 MONTHS AGO", "19 MONTHS AGO", 
    "20 MONTHS AGO", "21 MONTHS AGO", "22 MONTHS AGO", "23 MONTHS AGO", 
    "24 MONTHS AGO", "25 MONTHS AGO", "26 MONTHS AGO", "27 MONTHS AGO", 
    "28 MONTHS AGO", "29 MONTHS AGO", "30 MONTHS AGO", "31 MONTHS AGO", 
    "32 MONTHS AGO", "33 MONTHS AGO", "34 MONTHS AGO", "35 MONTHS AGO", 
    "36 MONTHS AGO", "MORE THAN 36 MONTHS AGO", "NOT SURE"
  ),
  
  # Page 4, Q11
  prev_net_brand = c(
    "LLIN", "OLYSET LLIN", "ICONLIFE LLIN", "DURANET LLIN", "NETPROTECT LLIN", 
    "BASF INTERCEPTOR LLIN", "YORKOOL LLIN", "MAGNET LLIN", "DAWAPLUS 2.0 LLIN", 
    "ROYAL SECURITY LLIN", "ROYAL SENTRY LLIN", "PERMANET 2.0 LLIN", 
    "PERMANET 3.0 LLIN", "VEERALIN LLIN", "INTERCEPTOR G2 LLIN", 
    "ROYAL GUARD LLIN", "OTHER/DON'T KNOW BRAND BUT (LLIN)", 
    "OTHER TYPE (NOT LLIN)", "DON'T KNOW TYPE (IF LLIN OR NOT)"
  ),
  
  # Page 4, Q12
  prev_net_obtained_how = c(
    "MASS DISTRIBUTION CAMPAIGN", "ANTE NATAL CARE VISIT", 
    "IMMUNIZATION VISIT", "OTHERS"
  ),
  
  # Page 4, Q13
  prev_net_obtained_where = c(
    "I DON'T KNOW", "GOVERNMENT HEALTH FACILITY", "PRIVATE HEALTH FACILITY", 
    "PHARMACY", "SHOP/MARKET", "CHW", "RELIGIOUS INSTITUTION", "SCHOOL", 
    "OTHER (SPECIFY)"
  ),
  
  # Page 5, Q17
  prev_home_sprayed_interior = c("YES", "NO", "I DON'T KNOW WHAT IT IS"),
  
  # Page 5, Q18
  prev_repellent_methods = c(
    "COILS", "SPRAYS/INSECTICIDES", "CREAMS", "ELECTRONIC DEVICES", 
    "OTHERS", "NONE"
  ),
  
  # Page 6, Q19
  prev_first_treatment_location = c(
    "GOVERNMENT HOSPITAL", "PRIVATE HOSPITAL/CLINIC", "NGO HOSPITAL", 
    "MOBILE CLINIC", "PRIVATE DOCTOR (NO HOSPITAL VISIT)", 
    "COMMUNITY NURSE/ HEALTH WORKER (NO HOSPITAL VISIT)", "PHARMACY", 
    "LOCAL DRUG STORE/CHEMIST", "MOBILE DRUG SELLER", "TRADITIONAL HEALER", 
    "RELIGIOUS HEALER"
  ),
  
  # Page 6, Q20
  prev_time_to_treatment_facility = c(
    "LESS THAN 30 MINS", "30 MINS - 1 HOUR", "MORE THAN 1 HOUR", "I DON'T KNOW"
  ),
  
  # --- Section 3: Malaria Treatment ---
  # Page 6, Q21 (Transport Cost)
  treat_transport_cost = c(
    "FREE", "N1 - N999", "N1,000 - N1,999", "N2,000 - N2,999", 
    "N3,000 - N3,999", "N4,000 - N4,999", "N5,000 - N5,999", 
    "N6,000 - N6,999", "N7000 - N7,999", "N8,000 - N8,999", 
    "N9,000 - N9,999", "ABOVE N10,000", "I DON'T KNOW"
  ),
  
  # Page 7, Q22
  treat_hh_fever_last_2weeks = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 7, Q23
  treat_blood_sample_taken = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 7, Q24 (Test Cost)
  treat_test_cost = c(
    "FREE", "N1 - N999", "N1,000 - N1,999", "N2000 - N2,999", 
    "N3,000 - N3,999", "N4,000 - N4,999", "N5,000 - N5,999", 
    "N6,000 - N6,999", "N7000 - N7,999", "N8,000 - N8,999", 
    "N9,000 - N9,999", "N10,000 - N10,999", "N11,000 - N11,999", 
    "N12,000 - N12,999", "N13,000 - N13,999", "N14,000 - N14,999", 
    "ABOVE N15,000", "I DON'T KNOW"
  ),
  
  # Page 8, Q25 (Drug Cost) - Same scale as above
  treat_drug_cost = c(
    "FREE", "N1 - N999", "N1,000 - N1,999", "N2000 - N2,999", 
    "N3,000 - N3,999", "N4,000 - N4,999", "N5,000 - N5,999", 
    "N6,000 - N6,999", "N7000 - N7,999", "N8,000 - N8,999", 
    "N9,000 - N9,999", "N10,000 - N10,999", "N11,000 - N11,999", 
    "N12,000 - N12,999", "N13,000 - N13,999", "N14,000 - N14,999", 
    "ABOVE N15,000", "I DON'T KNOW"
  ),
  
  # Page 8, Q26
  treat_drug_purchase_time = c(
    "0 – 4 WEEKS AGO", "1 – 5 MONTHS AGO", "6 – 12 MONTHS AGO", 
    "OVER ONE YEAR AGO", "I DON’T KNOW"
  ),
  
  # Page 8, Q27
  treat_drug_affordability = c(
    "VERY AFFORDABLE", "SOMEWHAT AFFORDABLE", 
    "NEUTRAL - NEITHER AFFORDABLE NOR EXPENSIVE", 
    "SOMEWHAT EXPENSIVE", "VERY EXPENSIVE"
  ),
  
  # Page 9, Q28
  treat_heard_smc = c("YES", "NO"),
  
  # Page 9, Q29
  treat_children_received_smc = c(
    "YES", 
    "NO AND I HAVE A CHILD OF 5 YEARS OR BELOW", 
    "NO, I DO NOT HAVE A CHILD OF 5 YEARS OR BELOW",
    "I DON'T KNOW WHAT IT IS"
  ),
  
  # Page 9, Q30
  treat_know_smc_drug = c(
    "SULFADOXINE-PYRIMETHAMINE + AMODIAQUINE", 
    "SULFADOXINE-PYRIMETHAMINE", 
    "ARTEMETHER-LUMEFANTRINE", 
    "ARTESUNATE + AMODIAQUINE", 
    "DIHYDROARTEMISININ-PIPERAQUINE", 
    "PROGUANIL", 
    "OTHERS I DON'T KNOW"
  ),
  
  # Page 9, Q31
  treat_vaccine_age_knowledge = c(
    "LESS THAN 12 MONTHS", "1 YEAR", "2 YEARS", "3 YEARS", "4 YEARS", 
    "5 YEARS", "ABOVE 5 YEARS", "I DON'T KNOW ABOUT MALARIA VACCINE"
  ),
  
  # Page 10, Q32
  treat_children_received_vaccine = c(
    "YES", 
    "NO AND I HAVE A CHILD OF 5 YEARS OR BELOW", 
    "NO, I DO NOT HAVE A CHILD OF 5 YEARS OR BELOW"
  ),
  
  # --- Feedback ---
  # Page 10, Q33 & Q34
  feedback_free_treatment_6months = c(
    "YES", 
    "NO AND I WENT FOR IN MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS", 
    "NO, I DID NOT GO FOR MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS"
  ),
  
  feedback_drug_stockout_6months = c(
    "YES", 
    "NO AND I WENT FOR IN MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS", 
    "NO, I DID NOT GO FOR MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS"
  ),
  
  # Page 10, Q35
  feedback_gov_effort_rating = c(
    "VERY EFFECTIVE", "SOMEWHAT EFFECTIVE", "NEUTRAL", 
    "SOMEWHAT INEFFECTIVE", "VERY INEFFECTIVE"
  ),
  
  # --- Section 5: Women Questionnaire ---
  # Page 11, Q1
  women_ever_given_birth = c("YES", "NO"),
  
  # Page 11, Q3
  women_anc_seen = c("YES", "NO"),
  
  # Page 11, Q4
  women_anc_provider = c(
    "DOCTOR", "NURSE/MIDWIFE", "AUXILIARY MIDWIFE", 
    "COMMUNITY EXTENSION HEALTH WORKER", "TRADITIONAL BIRTH ATTENDANT", 
    "COMMUNITY HEALTH WORKER/FIELD WORKER", "OTHERS"
  ),
  
  # Page 12, Q5
  women_anc_location = c(
    "GOVERNMENT HOSPITAL", "GOVERNMENT HEALTH CENTER", "GOVERNMENT HEALTH POST", 
    "PRIVATE HOSPITAL/CLINIC", "NGO HOSPITAL", "NGO CLINIC", 
    "YOUR HOME", "HER HOME", "OTHERS (SPECIFY)"
  ),
  
  # Page 13, Q8
  women_took_sp_fansidar = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 13, Q10
  women_sp_fansidar_source = c(
    "ANTENATAL VISIT TO A HEATH FACILITY", "NON ANTENATAL VISIT TO A HEALTH FACILITY", 
    "PHARMACY/LOCAL CHEMIST", "COMMUNITY HEALTH EXTENSION WORKER", "OTHERS"
  ),
  
  # Page 14, Q11
  women_child_fever_2weeks = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 14, Q12
  women_child_blood_sample = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 14, Q13
  women_child_malaria_diagnosis = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 14, Q14
  women_child_seek_advice = c("YES", "NO"),
  
  # Page 14, Q15 & Page 15, Q16
  women_child_first_advice_location = c(
    "GOVERNMENT HOSPITAL", "PRIVATE HOSPITAL/CLINIC", "NGO HOSPITAL", 
    "MOBILE CLINIC", "PRIVATE DOCTOR (NO HOSPITAL VISIT)", 
    "COMMUNITY NURSE/ HEALTH WORKER (NO HOSPITAL VISIT)", "PHARMACY", 
    "LOCAL DRUG STORE/CHEMIST", "MOBILE DRUG SELLER", 
    "TRADITIONAL HEALER", "RELIGIOUS HEALER", "OTHERS"
  ),
  
  # Page 15, Q17
  women_child_advice_delay_days = c(
    "SAME DAY", "1 DAY AFTER", "2 DAYS AFTER", "3 DAYS AFTER", 
    "4 DAYS AFTER", "5 DAYS AFTER", "6 DAYS AFTER", "OVER 7 DAYS AFTER"
  ),
  
  # Page 15, Q18
  women_child_referral = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 16, Q19
  women_child_took_medicine = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 16, Q20
  women_child_medicine_type = c(
    "ARTEMISININ COMBINATION THERAPY (ACT)", "SP/FANSIDAR", "CHLOROQUINE", 
    "AMODIAQUINE", "QUININE PILLS", "INJECTION", "ARTESUNATE RECTAL", 
    "AMOXICILLIN", "COTRIMOXAZOLE", "ASPIRIN", "PARACETAMOL/PANADOL/", 
    "ACETAMINOPHEN", "IBUPROFEN", "OTHERS", "I DONT KNOW"
  ),
  
  # Page 16, Q21
  women_child_act_delay = c(
    "SAME DAY", "NEXT DAY", "TWO DAYS AFTER FEVER", 
    "THREE OR MORE DAYS AFTER FEVER", "I DON'T KNOW"
  ),
  
  # Page 16, Q22
  women_child_act_effective = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 17, Q23
  women_currently_pregnant = c("YES", "NO", "NOT SURE"),
  
  # --- Section 6: Background Info Part A ---
  # Page 18, Q2
  bg_tv_frequency = c(
    "AT LEAST ONCE A WEEK", "I CAN GO OVER ONE WEEK WITHOUT WATCHNG TV", 
    "I DONT WATCH TV AT ALL"
  ),
  
  # Page 18, Q3
  bg_own_smartphone = c("YES", "NO", "I DONT KNOW WHAT A SMARTPHONE IS"),
  
  # Page 18, Q4
  bg_internet_ever_used = c("YES", "NO", "I DONT KNOW WHAT THE INTERNET IS"),
  
  # Page 18, Q5
  bg_internet_frequency = c(
    "ALMOST EVERY DAY", "AT LEAST ONCE A WEEK", 
    "I WENT MORE THAN ONCE WEEK WITHOUT USING THE INTERNET"
  ),
  
  # Page 18, Q6
  bg_religion = c("CHRISTIAN", "ISLAM", "TRADITIONALIST", "OTHERS"),
  
  # Page 19, Q8
  bg_heard_malaria_msg_6months = c("YES", "NO"),
  
  # Page 19, Q9
  bg_malaria_msg_source = c(
    "RADIO", "TELEVISION", "POSTER/BILLBOARD", "NEWSPAPER/MAGAZINE", 
    "LEAFLET/BROCHURE", "HEALTHCARE PROVIDER", "COMMUNITY HEALTH WORKER", 
    "SOCIAL MEDIA", "TOWN ANNOUNCER", "INTER-PERSONAL COMMUNICATION AGENT/ COMMUNITY VOLUNTEER", 
    "FAMILY/FRIENDS", "I DON’T REMEMBER", "OTHERS (SPECIFY)"
  ),
  
  # Page 20, Q10
  bg_aware_avoidance = c("YES", "NO"),
  
  # Page 20, Q11
  bg_prevention_knowledge = c(
    "I DON'T KNOW", 
    "SLEEP INSIDE A MOSQUITO NET", 
    "SLEEP INSIDE AN INSECTICIDE-TREATED MOSQUITO NET", 
    "USE MOSQUITO REPELANT OR COIL", 
    "TAKE PREVENTATIVE MEDICATIONS", 
    "SPRAY HOUSE WITH INSECTICIDE", 
    "FILL IN STAGNANT WATERS (PUDDLES)", 
    "KEEP SURROUNDINGS CLEAN", 
    "PUT MOSQUITO SCREEN ON WINDOWS", 
    "OTHERS (SPECIFY)"
  ),
  
  # --- Section 7: Attitudes ---
  # Page 21-22 (Q12 - Q19)
  att_scale_type_1 = c("AGREE", "DISAGREE", "DON'T KNOW/UNCERTAIN"),
  
  # Page 22-23 (Q20 - Q21)
  att_scale_type_2 = c("AGREE / MORE THAN HALF", "DISAGREE / LESS THAN HALF", "DON'T KNOW/UNCERTAIN"),
  
  # --- Section 8: Household Information ---
  # Page 23, Q5
  hh_relation_to_head = c(
    "HEAD", "WIFE OR HUSBAND", "SON OR DAUGHTER", "SON-IN-LAW OR DAUGHTER-IN-LAW", 
    "GRANDCHILD", "PARENT", "PARENT-IN-LAW", "BROTHER OR SISTER", 
    "OTHER RELATIVE", "ADOPTED/FOSTER/STEPCHILD", "NOT RELATED", 
    "CO-WIFE", "I DON'T KNOW"
  ),
  
  # Page 24, Q6
  hh_drinking_water_source = c(
    "PIPED IN DWELLING", "PIPED TO YARD/PLOT", "PIPED TO NEIGHBOR", 
    "PUBLIC TAP/STANDPIPE", "TUBEWELL OR BOREHOLE", "PROTECTED WELL", 
    "UNPROTECTED WELL", "RAINWATER", "TANKER TRUCK", "CART WITH SMALL TANK", 
    "SURFACE WATER (RIVER/DAM/POND/STREAM/CANAL/IRRIGATION CHANNEL)", 
    "BOTTLED WATER", "SACHET WATER"
  ),
  
  # Page 24, Q7 uses the same list as above
  hh_other_water_source = c(
    "PIPED IN DWELLING", "PIPED TO YARD/PLOT", "PIPED TO NEIGHBOR", 
    "PUBLIC TAP/STANDPIPE", "TUBEWELL OR BOREHOLE", "PROTECTED WELL", 
    "UNPROTECTED WELL", "RAINWATER", "TANKER TRUCK", "CART WITH SMALL TANK", 
    "SURFACE WATER (RIVER/DAM/POND/STREAM/CANAL/IRRIGATION CHANNEL)"
  ),
  
  # Page 25, Q8
  hh_water_location = c("IN OWN DWELLING", "IN OWN YARD/PLOT", "ELSEWHERE"),
  
  # Page 25, Q9
  hh_water_time_trip = c(
    "LESS THAN 1 MINUTE", "11 MINUTES", "12 MINUTES", "13 MINUTES", "14 MINUTES", "15 MINUTES", 
    "16 MINUTES", "17 MINUTES", "18 MINUTES", "19 MINUTES", "20 MINUTES", 
    "21 MINUTES", "22 MINUTES", "23 MINUTES", "24 MINUTES", "25 MINUTES", 
    "26 MINUTES", "27 MINUTES", "28 MINUTES", "29 MINUTES", "30 MINUTES", 
    "MORE THAN 30 MINUTES", "I DON'T KNOW"
  ),
  
  # Page 25, Q10
  hh_toilet_facility_type = c(
    "FLUSH TO PIPE SEWER SYSTEM", "FLUSH TO SEPTIC TANK", "FLUSH TO PIT LATRINE", 
    "FLUSH TO SOMEWHERE ELSE", "FLUSH DON'T KNOW WHERE", 
    "VENTILATED IMPROVED PIT LATRINE", "PIT LATRINE WITH SLAB", 
    "PIT LATRINE WITHOUT SLAB/OPEN PIT", "COMPOSTING TOILET", "BUCKET TOILET", 
    "HANGING TOILET/HANGING LATRINE", "NO FACILITY/BUSH/FIELD", 
    "OTHERS (SPECIFY)"
  ),
  
  # Page 26, Q11
  hh_toilet_shared = c("YES", "NO"),
  
  # Page 26, Q13
  hh_toilet_location = c("IN OWN DWELLING", "IN OWN YARD/PLOT", "ELSEWHERE"),
  
  # Page 26, Q14
  hh_cookstove_type = c(
    "ELECTRIC STOVE", "SOLAR COOKER", "LIQUIFIED PETROLEUM GAS (LPG)/COOKING GAS STOVE", 
    "PIPED NATURAL GAS STOVE", "BIOGAS STOVE", "KEROSENE STOVE", 
    "MANUFACTURED SOLID FUEL STOVE", "TRADITIONAL SOLID FUEL STOVE", 
    "THREE STONE STOVE"
  ),
  
  # Page 27, Q15
  hh_cookstove_fuel = c(
    "ALCOHOL/ETHANOL", "GASOLINE/DIESEL", "KEROSENE/PARAFFIN", "COAL/LIGNITE", 
    "CHARCOAL", "WOOD", "STRAW/SHRUBS/GRASS", "AGRICULTURAL CROP", 
    "ANIMAL DUNG/WASTE", "PROCESSED BIOMASS (PELLETS)/WOODCHIPS", 
    "GARBAGE/PLASTIC", "SAWDUST", "OTHERS (SPECIFY)", 
    "NO FOOD COOKED IN HOUSEHOLD"
  ),
  
  # Page 27, Q16
  hh_owns_livestock = c("YES", "NO"),
  
  # Page 30, Q25
  hh_owns_agri_land = c("YES", "NO"),
  
  # Page 31-33 (Assets)
  hh_has_electricity = c("YES", "NO", "I DON'T KNOW"),
  hh_has_radio = c("YES", "NO", "I DON'T KNOW"),
  hh_has_tv = c("YES", "NO", "I DON'T KNOW"),
  
  # Page 34, Q53
  hh_floor_material = c(
    "EARTH/SAND", "DUNG", "WOOD PLANKS", "PALM/BAMBOO", 
    "PARQUET/POLISHED WOOD", "VINYL/ASPHALT STRIPS", "CERAMIC TILES", 
    "CEMENT", "CARPET", "OTHER (SPECIFY)"
  ),
  
  # Page 34, Q54
  hh_roof_material = c(
    "NO ROOF", "THATCH/PALM LEAF", "GRASS", "RUSTIC MAT", "PALM/BAMBOO", 
    "WOOD PLANKS", "CARDBOARD", "METAL/ZINC", "WOOD", 
    "CELAMINE/CEMENT FIBER", "CERAMIC TILES", "CEMENT", "ROOFING SHINGLES", 
    "ASBESTOS", "OTHER (SPECIFY)"
  ),
  
  # Page 34, Q55
  hh_wall_material = c(
    "NO WALLS", "CANE/PALM/TRUNKS", "DIRT", "BAMBOO WITH MUD", 
    "UNCOVERED ADOBE", "PLYWOOD", "CARDBOARD", "REUSED WOOD", "CEMENT", 
    "STONE WITH LIME/CEMENT", "BRICKS", "CEMENT BLOCKS", "COVERED ADOBE", 
    "WOOD PLANKS/SHINGLES", "OTHER (SPECIFY)"
  )
)

# Save the dictionary object
saveRDS(mis_data_dictionary, file = "Sproxil_mis_dictionary.rds")
print("Dictionary saved successfully.")

