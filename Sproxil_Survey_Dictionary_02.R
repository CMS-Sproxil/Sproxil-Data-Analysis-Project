# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Data_Preparation.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    November 26, 2025
#
# DESCRIPTION:
# 1. Defines the "Data Dictionary" based on the Sproxil Questionnaire.
# 2. Defines the "Standard Names" to rename raw columns.
# 3. Defines the descriptive labels for variables and values.
#
# OUTPUT: "Sproxil_dictionary.rds"
# ==============================================================================

library(dplyr)
library(tidyr)
library(knitr)


# 1. Define the Column Mapping
column_mapping <- c(
  "meta_status"                     = "status",
  "meta_date_created"               = "date_created",
  "meta_respondent_id"              = "respondent_id",
  "meta_longitude"                  = "longitude",
  "meta_latitude"                   = "latitude",
  "meta_survey_code"                = "survey_code",
  "meta_survey_id"                  = "survey_id",
  "meta_survey_name"                = "survey_name",
  "demo_state"                      = "state",
  "demo_lga"                        = "lga",
  "demo_town"                       = "town",
  "demo_age"                        = "age",
  "demo_year_of_birth"              = "year_of_birth",
  "demo_gender"                     = "What is your gender?",
  "demo_edu_level"                  = "What is your highest level of education?",
  "demo_edu_informal"               = "What type of informal education have you attended?",
  "demo_hh_children_under5"         = "How many children under the age of 5 live in your household?",
  "demo_hh_sleeping_rooms"          = "How many rooms in your house are used for sleeping",
  "prev_has_mosquito_nets"          = "Does your household have any mosquito nets?",
  "prev_is_itn"                     = "Do you know if that net is an insecticide treated net?",
  "prev_num_mosquito_nets"          = "How many mosquito nets does your household have?",
  "prev_months_since_net_obtained"  = "How many months ago did your household get the mosquito net?",
  "prev_net_brand"                  = "What brand of mosquito nets do members of this household use?  (LLLNG means Long-lasting insecticide-treated net)",
  "prev_net_obtained_how"           = "How did you get the mosquito net?",
  "prev_net_obtained_where"         = "Where did you get the net?",
  "prev_slept_under_net_last_night" = "Did you or anyone in your household sleep under a mosquito net last night?",
  "prev_num_people_slept_net"       = "How many people slept inside this mosquito net last night?",
  "prev_net_not_used_reason"        = "What was the MAIN REASON this net was not used last night?",
  "prev_home_sprayed_interior"      = "In the past 12 months, has anyone entered your home to spray the interior walls for mosquito control?",
  "prev_repellent_methods"          = "Do you use any of these mosquito repellent methods?",
  "prev_first_treatment_location"   = "Where do you usually go first to seek malaria treatment?",
  "prev_time_to_treatment_facility" = "How long does it take to reach the nearest healthcare facility to seek malaria treatment?",
  "treat_transport_cost"            = "How much do you spend on transport cost (to and fro) to visit  the nearest healthcare facility to seek malaria treatment",
  "treat_hh_fever_last_2weeks"      = "Have you or anyone in your household had fever in the last 2 weeks?",
  "treat_blood_sample_taken"        = "Was a blood sample taken from any part of their body for testing before treatment was given?",
  "treat_test_cost"                 = "How much did the test cost you?",
  "treat_drug_cost"                 = "How much did your last malaria drugs cost?",
  "treat_drug_purchase_time"        = "When did you buy  your last malaria drug?",
  "treat_drug_affordability"        = "Rate the affordability of malaria drugs in your communityâ€™",
  "treat_heard_smc"                 = "Have you heard about Seasonal Malaria Chemoprevention (SMC)?",
  "treat_children_received_smc"     = "Have your children ever received Seasonal Malaria Chemoprevention (SMC) treatment?",
  "treat_know_smc_drug"             = "Do you know the drug that was administered to your children during  the Seasonal Malaria  Chemoprevention?",
  "treat_vaccine_age_knowledge"     = "At what age should a child receive the first dose of the malaria vaccine",
  "treat_children_received_vaccine" = "Have your children ever received a malaria vaccine dose?",
  "feedback_free_treatment_6months" = "Have you received free malaria treatment at a government facility in the last 6 months?",
  "feedback_drug_stockout_6months" = "Have you experienced any case where there was no malaria drugs available at a government health facilities in the last 6 months?",
  "feedback_gov_effort_rating"      = "How would you rate the government's efforts in combating malaria?",
  "women_ever_given_birth"          = "Have you ever given birth?",
  "women_births_2020_2025"          = "How many children have you given birth to between 2020 and 2025?",
  "women_anc_seen"                  = "While you were pregnant with your last child that you gave birth to alive, did you see anyone for antenatal care for this pregnancy?",
  "women_anc_provider"              = "Whom did you see for the antenatal care?",
  "women_anc_location"              = "Where did you receive the antenal care?",
  "women_anc_first_visit_month"     = "How many months/weeks pregnant were you when you first received antenatal care for this pregnancy?",
  "women_anc_total_visits"          = "How many times did you receive antenatal care during this pregnancy?",
  "women_took_sp_fansidar"          = "During this pregnancy, did you take SP/Fansidar to keep you from getting malaria?",
  "women_sp_fansidar_doses"         = "How many times did you take SP/Fansidar during this pregnancy?",
  "women_sp_fansidar_source"        = "How did you get the SP/Fansidar?",
  "women_child_fever_2weeks"        = "Has your youngest child (born in the last 5 years) been ill with a fever at any time in the last 2 weeks?",
  "women_child_blood_sample"        = "At any time during the illness, did the child have blood sample sample taken for testing?",
  "women_child_malaria_diagnosis"   = "Were you told by a healthcare provider that the child had malaria?",
  "women_child_seek_advice"         = "Did you seek advice or treatment for the illness from any source?",
  "women_child_advice_location"     = "Where did you seek advice or treatment for the fever?",
  "women_child_first_advice_location" = "Where did you first seek advice or treatment?",
  "women_child_advice_delay_days"   = "How many days after the illness began did you first seek advice or treatment?",
  "women_child_referral"            = "While your child was sick with this fever were you referred to go to a higher level of care?",
  "women_child_took_medicine"       = "At any time during the illness, did your child take any medicine for the illness?",
  "women_child_medicine_type"       = "What medicine did your child take during the illness?",
  "women_child_act_delay"           = "How long after the fever started did your child first take an artemisinin combination therapy?",
  "women_child_act_effective"       = "After your child took an artemisinin combination therapy, did the fever go away?",
  "women_currently_pregnant"        = "Are you pregnant now?",
  "women_pregnancy_duration_months" = "How many weeks/months pregnant are you?",
  "bg_languages"                    = "What languages do you speak?",
  "bg_tv_frequency"                 = "How often do you watch TV?",
  "bg_own_smartphone"               = "Do you own a smart phone?",
  "bg_internet_ever_used"           = "Have you ever used the Internet before?",
  "bg_internet_frequency"           = "How often did you use the Internet?",
  "bg_religion"                     = "What is your religion?",
  "bg_ethnic_group"                 = "What is your ethnic group?",
  "bg_heard_malaria_msg_6months"    = "In the past 6 months, have you seen or heard any messages about malaria?",
  "bg_malaria_msg_source"           = "Where did you see or hear the messages about malaria?",
  "bg_aware_avoidance"              = "Are you aware of any way to avoid malaria?",
  "bg_prevention_knowledge"         = "What are the things that people can do to prevent themselves from getting malaria?",
  "att_rainy_season_only"           = "People in this community only get malaria during the rainy season?",
  "att_fever_worry_malaria"         = "When a child has a fever, do you almost always worry it might be malaria?",
  "att_malaria_easily_treated"      = "Getting malaria is not a problem because it can be easily treated.",
  "att_weak_children_die"           = "Only weak children can die from malaria.",
  "att_net_use_mosquito_density"    = "You only need to sleep inside a mosquito net for the entire night when there are lots of mosquitoes.",
  "att_net_use_warm_weather"        = "You do not like sleeping inside a mosquito net when the weather is too warm.",
  "att_home_meds_first"             = "When a child has a fever, it is best to start by giving them any medicine you have at home.",
  "att_full_dose_importance"        = "It is important that children take the full dose of medicine that they are prescribed for malaria",
  "att_seek_care_immediate"         = "People in your community usually take their children to a health care provider on the same day or day after they develop a fever.",
  "att_community_net_usage"         = "People in your community who have a mosquito net usually sleep inside a mosquito net every night.",
  "hh_total_persons_v1"             = "How many persons live usually in your household - including children, domestic servants, lodgers and guests of the household?",
  "hh_total_persons_v2"             = "How many people live in your house hold?",
  "hh_members_under5"               = "How many people that live in this household are under 5 years old?",
  "hh_total_persons_usually_v3"     = "How many persons USUALLY live in your household - including children, domestic servants, lodgers and guests of the household?",
  "hh_relation_to_head"             = "What is your relationship with the head of the household?",
  "hh_drinking_water_source"        = "What is the MAIN source of drinking water for members of your household?",
  "hh_other_water_source"           = "What is the MAIN source of water used by your household for other purposes such as cooking and handwashing?",
  "hh_water_location"               = "Where is that water source located?",
  "hh_water_time_trip"              = "How long does it take to go there, get water, and come back?",
  "hh_toilet_type"                  = "What kind of toilet facility do members of your household usually use?",
  "hh_toilet_shared"                = "Do you share this toilet facility with other households?",
  "hh_toilet_share_count"           = "Including your own household, how many households use this toilet facility?",
  "hh_toilet_location"              = "Where is this toilet facility located?",
  "hh_cookstove_type"               = "In your household, what type of cookstove is MAINLY used for cooking?",
  "hh_cookstove_fuel"               = "What type of fuel or energy source is used in this cookstove?",
  "hh_owns_livestock"               = "Does this household own any livestock, herds, other farm animals, or poultry?",
  "hh_num_cows_bulls"               = "How many cow/bulls does this household own?",
  "hh_num_other_cattle"             = "How many other cattles does this household own?",
  "hh_num_horses_donkeys"           = "How many horses/donkeys/mules does this household own?",
  "hh_num_goats"                    = "How many goats does this household own?",
  "hh_num_sheep"                    = "How many sheep does this household own?",
  "hh_num_poultry"                  = "How many chickens/other poultry does this household own?",
  "hh_num_pigs"                     = "How many pigs does this household own?",
  "hh_num_camels"                   = "How many camels does this household own?",
  "hh_owns_agri_land"               = "Does any member of this household own any agricultural land?",
  "hh_num_agri_plots"               = "How many plots of agricultural land do members of this household own?",
  "hh_has_electricity"              = "Does your household have electricity?",
  "hh_has_radio"                    = "Does your household have a radio?",
  "hh_has_tv"                       = "Does your household have a television?",
  "hh_has_non_mobile_phone"         = "Does your household have a non-mobile phone?",
  "hh_has_computer"                 = "Does your household have a computer?",
  "hh_has_refrigerator"             = "Does your household have a refrigerator?",
  "hh_has_table"                    = "Does your household have a table?",
  "hh_has_chair"                    = "Does your household have a chair?",
  "hh_has_bed"                      = "Does your household have a bed?",
  "hh_has_sofa"                     = "Does your household have a sofa?",
  "hh_has_cupboard"                 = "Does your household have a cupboard?",
  "hh_has_ac"                       = "Does your household have an air conditioner?",
  "hh_has_electric_iron"            = "Does your household have an electric iron?",
  "hh_has_generator"                = "Does your household have a generator?",
  "hh_has_fan"                      = "Does your household have a fan?",
  "hh_own_watch"                    = "Does any member of this household own a watch?",
  "hh_own_mobile_phone"             = "Does any member of this household own a mobile phone?",
  "hh_own_bicycle"                  = "Does any member of this household own a bicycle?",
  "hh_own_motorcycle"               = "Does any member of this household own a motorcycle or motor scooter?",
  "hh_own_animal_cart"              = "Does any member of this household own an animal-drawn cart?",
  "hh_own_car_truck"                = "Does any member of this household own a car or truck?",
  "hh_own_motor_boat"               = "Does any member of this household own a boat with a motor?",
  "hh_own_canoe"                    = "Does any member of this household own a canoe?",
  "hh_own_keke_napep"               = "Does any member of this household own a keke napep?",
  "hh_has_bank_account"             = "Does any member of this household have an account in a bank or other financial institution?",
  "hh_mobile_money_usage"           = "Does any member of this household use a mobile phone to make financial transactions such as sending or receiving money, paying bills, purchasing goods or services, or receiving wages?",
  "hh_floor_material"               = "What is the MAIN material used for the FLOOR of the house you live in?",
  "hh_roof_material"                = "What is the MAIN material used for the ROOF of the house you live in?",
  "hh_wall_material"                = "What is the MAIN material used for the WALL of the house you live in?",
  "meta_response_date"              = "response_date",
  "meta_channel"                    = "channel"  
)

# 2. Define the Answer Key
mis_data_dictionary <- list(
  meta_status = c("COMPLETED", "IN-PROGRESS"),
  meta_channel = c("DIRECT", "OUTBOUND"),
  demo_gender = c("MALE", "FEMALE"),
  demo_year_of_birth = c(paste(1930:2010), "I DON'T KNOW BUT I AM OLDER THAN 15"),
  demo_state = c("ADAMAWA", "ABIA", "ANAMBRA", "AKWA IBOM", "BAUCHI", "BAYELSA", "BENUE", "BORNO", "CROSS RIVER", "DELTA", "EBONYI", "EDO", "EKITI", "ENUGU", "FEDERAL CAPITAL TERRITORY", "GOMBE", "IMO", "JIGAWA", "KADUNA", "KANO", "KATSINA", "KEBBI", "KOGI", "KWARA", "LAGOS", "NASARAWA", "NIGER", "OGUN", "ONDO", "OSUN", "OYO", "PLATEAU", "RIVERS", "SOKOTO", "TARABA", "YOBE", "ZAMFARA"),
  demo_edu_level = c("I NEVER HAD ANY FORMAL EDUCATION", "PRIMARY", "SECONDARY", "HIGHER (POST SECONDARY) EDUCATION", "POSTGRADUATE", "VOCATIONAL EDUCATION"),
  demo_edu_informal = c("I NEVER HAD ANY FORMAL EDUCATION", "ADULT EDUCATION", "TSANGAYA", "QUARANIC", "OTHER"),
  demo_hh_children_under5 = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "MORE THAN 8"),
  demo_hh_sleeping_rooms = c("OPEN FLOOR WITH NO SEPARATE BEDROOMS", "1", "2", "3", "4", "5", "MORE THAN 5"),
  prev_has_mosquito_nets = c("YES", "NO", "I DON'T KNOW"),
  prev_num_mosquito_nets = c("0", "1", "2", "3", "4", "5", "6", "7 OR MORE"),
  prev_is_itn = c("YES - IT IS", "NO - IT IS NOT", "I DON'T KNOW"),
  prev_months_since_net_obtained = c("LESS THAN A MONTH", paste(1, "MONTH AGO"), paste(2:36, "MONTHS AGO"), "MORE THAN 36 MONTHS AGO", "NOT SURE"),
  prev_net_brand = c("LLIN", "OLYSET LLIN", "ICONLIFE LLIN", "DURANET LLIN", "NETPROTECT LLIN", "BASF INTERCEPTOR LLIN", "YORKOOL LLIN", "MAGNET LLIN", "DAWAPLUS 2.0 LLIN", "ROYAL SECURITY LLIN", "ROYAL SENTRY LLIN", "PERMANET 2.0 LLIN", "PERMANET 3.0 LLIN", "VEERALIN LLIN", "INTERCEPTOR G2 LLIN", "ROYAL GUARD LLIN", "OTHER/DON'T KNOW BRAND BUT (LLIN)", "OTHER TYPE (NOT LLIN)", "DON'T KNOW TYPE (IF LLIN OR NOT)"),
  prev_net_obtained_how = c("MASS DISTRIBUTION CAMPAIGN", "ANTE NATAL CARE VISIT", "IMMUNIZATION VISIT", "OTHERS"),
  prev_net_obtained_where = c("I DON'T KNOW", "GOVERNMENT HEALTH FACILITY", "PRIVATE HEALTH FACILITY", "PHARMACY", "SHOP/MARKET", "CHW", "RELIGIOUS INSTITUTION", "SCHOOL", "OTHER (SPECIFY)"),
  prev_slept_under_net_last_night = c("YES", "NO", "I AM NOT SURE"),
  prev_num_people_slept_net = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 OR MORE", "I DON'T KNOW"),
  prev_net_not_used_reason = c("NO MOSQUITOES", "NO MALARIA", "TOO HOT", "DON'T LIKE SMELL", "FEEL 'CLOSED IN'", "NET TOO OLD/TORN", "NET TOO DIRTY", "NET NOT AVAILABLE LAST NIGHT (WASHING)", "USUAL USERS DID NOT SLEEP HERE LAST NIGHT", "NET NOT NEEDED LAST NIGHT", "BED BUGS", "DON'T KNOW", "OTHERS (SPECIFY)"),
  prev_home_sprayed_interior = c("YES", "NO", "I DON'T KNOW WHAT IT IS"),
  prev_repellent_methods = c("COILS", "SPRAYS/INSECTICIDES", "CREAMS", "ELECTRONIC DEVICES", "OTHERS", "NONE"),
  prev_first_treatment_location = c("GOVERNMENT HOSPITAL", "PRIVATE HOSPITAL/CLINIC", "NGO HOSPITAL", "MOBILE CLINIC", "PRIVATE DOCTOR (NO HOSPITAL VISIT)", "COMMUNITY NURSE/ HEALTH WORKER (NO HOSPITAL VISIT)", "PHARMACY", "LOCAL DRUG STORE/CHEMIST", "MOBILE DRUG SELLER", "TRADITIONAL HEALER", "RELIGIOUS HEALER"),
  prev_time_to_treatment_facility = c("LESS THAN 30 MINS", "30 MINS - 1 HOUR", "MORE THAN 1 HOUR", "I DON'T KNOW"),
  treat_transport_cost = c("FREE", "N1 - N999", "N1,000 - N1,999", "N2,000 - N2,999", "N3,000 - N3,999", "N4,000 - N4,999", "N5,000 - N5,999", "N6,000 - N6,999", "N7000 - N7,999", "N8,000 - N8,999", "N9,000 - N9,999", "ABOVE N10,000", "I DON'T KNOW"),
  treat_hh_fever_last_2weeks = c("YES", "NO", "I DON'T KNOW"),
  treat_blood_sample_taken = c("YES", "NO", "I DON'T KNOW"),
  treat_test_cost = c("FREE", "N1 - N999", "N1,000 - N1,999", "N2000 - N2,999", "N3,000 - N3,999", "N4,000 - N4,999", "N5,000 - N5,999", "N6,000 - N6,999", "N7000 - N7,999", "N8,000 - N8,999", "N9,000 - N9,999", "N10,000 - N10,999", "N11,000 - N11,999", "N12,000 - N12,999", "N13,000 - N13,999", "N14,000 - N14,999", "ABOVE N15,000", "I DON'T KNOW"),
  treat_drug_cost = c("FREE", "N1 - N999", "N1,000 - N1,999", "N2000 - N2,999", "N3,000 - N3,999", "N4,000 - N4,999", "N5,000 - N5,999", "N6,000 - N6,999", "N7000 - N7,999", "N8,000 - N8,999", "N9,000 - N9,999", "N10,000 - N10,999", "N11,000 - N11,999", "N12,000 - N12,999", "N13,000 - N13,999", "N14,000 - N14,999", "ABOVE N15,000", "I DON'T KNOW"),
  treat_drug_purchase_time = c("0- 4 WEEKS AGO", "1- 5 MONTHS AGO", "6- 12 MONTHS AGO", "OVER ONE YEAR AGO", "I DON'T KNOW"),
  treat_drug_affordability = c("VERY AFFORDABLE", "SOMEWHAT AFFORDABLE", "NEUTRAL - NEITHER AFFORDABLE NOR EXPENSIVE", "SOMEWHAT EXPENSIVE", "VERY EXPENSIVE"),
  treat_heard_smc = c("YES", "NO"),
  treat_children_received_smc = c("YES", "NO AND I HAVE A CHILD OF 5 YEARS OR BELOW", "NO, I DO NOT HAVE A CHILD OF 5 YEARS OR BELOW", "I DON'T KNOW WHAT IT IS"),
  treat_know_smc_drug = c("SULFADOXINE-PYRIMETHAMINE + AMODIAQUINE", "SULFADOXINE-PYRIMETHAMINE", "ARTEMETHER-LUMEFANTRINE", "ARTESUNATE + AMODIAQUINE", "DIHYDROARTEMISININ-PIPERAQUINE", "PROGUANIL", "OTHERS I DON'T KNOW"),
  treat_vaccine_age_knowledge = c("LESS THAN 12 MONTHS", "1 YEAR- 2 YEARS", "3 YEARS", "4 YEARS", "5 YEARS", "ABOVE 5 YEARS", "I DON'T KNOW ABOUT MALARIA VACCINE"),
  treat_children_received_vaccine = c("YES", "NO AND I HAVE A CHILD OF 5 YEARS OR BELOW", "NO, I DO NOT HAVE A CHILD OF 5 YEARS OR BELOW", "I DON'T KNOW WHAT IT IS"),
  feedback_free_treatment_6months = c("YES", "NO AND I WENT FOR IN MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS", "NO, I DID NOT GO FOR MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS"),
  feedback_drug_stockout_6months = c("YES", "NO AND I WENT FOR IN MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS", "NO, I DID NOT GO FOR MALARIA TREATMENT IN A GOVERNMENT HEALTH FACILITY IN THE LAST 6 MONTHS"),
  feedback_gov_effort_rating = c("VERY EFFECTIVE", "SOMEWHAT EFFECTIVE", "NEUTRAL", "SOMEWHAT INEFFECTIVE", "VERY INEFFECTIVE"),
  women_ever_given_birth = c("YES", "NO"),
  women_births_2020_2025 = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 OR MORE"),
  women_anc_seen = c("YES", "NO"),
  women_anc_provider = c("DOCTOR", "NURSE/MIDWIFE", "AUXILIARY MIDWIFE", "COMMUNITY EXTENSION HEALTH WORKER", "TRADITIONAL BIRTH ATTENDANT", "COMMUNITY HEALTH WORKER/FIELD WORKER", "OTHERS"),
  women_anc_location = c("GOVERNMENT HOSPITAL", "GOVERNMENT HEALTH CENTER", "GOVERNMENT HEALTH POST", "PRIVATE HOSPITAL/CLINIC", "NGO HOSPITAL", "NGO CLINIC", "YOUR HOME", "HER HOME", "OTHERS (SPECIFY)"),
  women_anc_total_visits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "MORE THAN 10", "I DON'T KNOW"),
  women_anc_first_visit_month = c(
    "I DON'T KNOW", "LESS THAN ONE MONTH", paste(1, "MONTH"), paste(2:9, "MONTHS"), "10 MONTHS OR MORE",
    paste(2:42, "WEEKS"), "MORE THAN 42 WEEKS"
  ),
  women_sp_fansidar_doses = c(paste(1:10), "MORE THAN 10", "I DON'T KNOW"),  
  women_took_sp_fansidar = c("YES", "NO", "I DON'T KNOW"),
  women_sp_fansidar_source = c("ANTENATAL VISIT TO A HEATH FACILITY", "NON ANTENATAL VISIT TO A HEALTH FACILITY", "PHARMACY/LOCAL CHEMIST", "COMMUNITY HEALTH EXTENSION WORKER", "OTHERS"),
  women_child_fever_2weeks = c("YES", "NO", "I DON'T KNOW"),
  women_child_blood_sample = c("YES", "NO", "I DON'T KNOW"),
  women_child_malaria_diagnosis = c("YES", "NO", "I DON'T KNOW"),
  women_child_seek_advice = c("YES", "NO"),
  women_child_advice_location = c("GOVERNMENT HOSPITAL", "PRIVATE HOSPITAL/CLINIC", "NGO HOSPITAL", "MOBILE CLINIC", "PRIVATE DOCTOR (NO HOSPITAL VISIT)", "COMMUNITY NURSE/ HEALTH WORKER (NO HOSPITAL VISIT)", "PHARMACY", "LOCAL DRUG STORE/CHEMIST", "MOBILE DRUG SELLER", "TRADITIONAL HEALER", "RELIGIOUS HEALER", "OTHERS"),
  women_child_first_advice_location = c("GOVERNMENT HOSPITAL", "PRIVATE HOSPITAL/CLINIC", "NGO HOSPITAL", "MOBILE CLINIC", "PRIVATE DOCTOR (NO HOSPITAL VISIT)", "COMMUNITY NURSE/ HEALTH WORKER (NO HOSPITAL VISIT)", "PHARMACY", "LOCAL DRUG STORE/CHEMIST", "MOBILE DRUG SELLER", "TRADITIONAL HEALER", "RELIGIOUS HEALER", "OTHERS"),
  women_child_advice_delay_days = c("SAME DAY", "1 DAY AFTER", "2 DAYS AFTER", "3 DAYS AFTER", "4 DAYS AFTER", "5 DAYS AFTER", "6 DAYS AFTER", "OVER 7 DAYS AFTER"),
  women_child_referral = c("YES", "NO", "I DON'T KNOW"),
  women_child_took_medicine = c("YES", "NO", "I DON'T KNOW"),
  women_child_medicine_type = c("ARTEMISININ COMBINATION THERAPY (ACT)", "SP/FANSIDAR", "CHLOROQUINE", "AMODIAQUINE", "QUININE PILLS", "INJECTION", "ARTESUNATE rectal", "AMOXICILLIN", "COTRIMOXAZOLE", "ASPIRIN", "PARACETAMOL/PANADOL/", "ACETAMINOPHEN", "IBUPROFEN", "OTHERS", "I DON'T KNOW"),
  women_child_act_delay = c("SAME DAY", "NEXT DAY", "TWO DAYS AFTER FEVER", "THREE OR MORE DAYS AFTER FEVER", "I DON'T KNOW"),
  women_child_act_effective = c("YES", "NO", "I DON'T KNOW"),
  women_currently_pregnant = c("YES", "NO", "I AM NOT SURE"),
  women_pregnancy_duration_months = c(
    "I DON'T KNOW", "LESS THAN ONE MONTH", paste(1, "MONTH"), paste(2:9, "MONTHS"), "10 MONTHS OR MORE",
    paste(2:42, "WEEKS"), "MORE THAN 42 WEEKS"
  ),
  bg_languages = c("ENGLISH", "YORUBA", "HAUSA", "IGBO", "OTHERS"),
  bg_tv_frequency = c("AT LEAST ONCE A WEEK", "I CAN GO OVER ONE WEEK WITHOUT WATCHNG TV", "I DON'T WATCH TV AT ALL"),
  bg_own_smartphone = c("YES", "NO", "I DON'T KNOW WHAT A SMARTPHONE IS"),
  bg_internet_ever_used = c("YES", "NO", "I DON'T KNOW WHAT THE INTERNET IS"),
  bg_internet_frequency = c("ALMOST EVERY DAY", "AT LEAST ONCE A WEEK", "I WENT MORE THAN ONCE A WEEK WITHOUT USING THE INTERNET"),
  bg_religion = c("CHRISTIAN", "CATHOLIC", "ISLAM", "TRADITIONALIST", "OTHERS"),
  bg_ethnic_group = c("HAUSA", "YORUBA", "IGBO", "KANURI", "IBIBIO", "TIV", "FULANI", "EDO", "IJAW", "NUPE", "EFIK", "GWARI", "ANANG", "ITSEKIRI", "JUKUN", "BINI", "ISOKO", "OGONI", "IDOMA", "GWANDARA", "EBIRA", "IGBIRA", "OTHERS"),
  bg_heard_malaria_msg_6months = c("YES", "NO"),
  bg_malaria_msg_source = c("RADIO", "TELEVISION", "POSTER/BILLBOARD", "NEWSPAPER/MAGAZINE", "LEAFLET/BROCHURE", "HEALTHCARE PROVIDER", "COMMUNITY HEALTH WORKER", "SOCIAL MEDIA", "TOWN ANNOUNCER", "INTER-PERSONAL COMMUNICATION AGENT/ COMMUNITY VOLUNTEER", "FAMILY/FRIENDS", "I DON'T REMEMBER", "OTHERS (SPECIFY)"),
  bg_aware_avoidance = c("YES", "NO"),
  bg_prevention_knowledge = c("I DON'T KNOW", "SLEEP INSIDE A MOSQUITO NET", "SLEEP INSIDE AN INSECTICIDE-TREATED MOSQUITO NET", "USE MOSQUITO REPELLANT OR COIL", "TAKE PREVENTATIVE MEDICATIONS", "SPRAY HOUSE WITH INSECTICIDE", "FILL IN STAGNANT WATERS (PUDDLES)", "KEEP SURROUNDINGS CLEAN", "PUT MOSQUITO SCREEN ON WINDOWS", "OTHERS (SPECIFY)"),
  att_rainy_season_only = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_fever_worry_malaria = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_malaria_easily_treated = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_weak_children_die = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_net_use_mosquito_density = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_net_use_warm_weather = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_home_meds_first = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_full_dose_importance = c("AGREE", "DISAGREE", "I DON'T KNOW/UNCERTAIN"),
  att_seek_care_immediate = c("AGREE / MORE THAN HALF", "DISAGREE / LESS THAN HALF", "I DON'T KNOW/UNCERTAIN"),
  att_community_net_usage = c("AGREE / MORE THAN HALF", "DISAGREE / LESS THAN HALF", "I DON'T KNOW/UNCERTAIN"),
  hh_total_persons_v1 = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "MORE THAN 10"),
  hh_total_persons_v2 = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "MORE THAN 10"),
  hh_members_under5 = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "MORE THAN 10"),
  hh_total_persons_usually_v3 = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "MORE THAN 10"),    
  hh_relation_to_head = c("HEAD", "WIFE OR HUSBAND", "SON OR DAUGHTER", "SON-IN-LAW OR DAUGHTER-IN-LAW", "GRANDCHILD", "PARENT", "PARENT-IN-LAW", "BROTHER OR SISTER", "OTHER RELATIVE", "ADOPTED/FOSTER/STEPCHILD", "NOT RELATED", "CO-WIFE", "I DON'T KNOW"),
  hh_drinking_water_source = c("PIPED IN DWELLING", "PIPED TO YARD/PLOT", "PIPED TO NEIGHBOR", "PUBLIC TAP/STANDPIPE", "TUBEWELL OR BOREHOLE", "PROTECTED WELL", "UNPROTECTED WELL", "RAINWATER", "TANKER TRUCK", "CART WITH SMALL TANK", "SURFACE WATER (RIVER/DAM/POND/STREAM/CANAL/IRRIGATION CHANNEL)", "BOTTLED WATER", "SATCHET WATER"),
  hh_other_water_source = c("PIPED IN DWELLING", "PIPED TO YARD/PLOT", "PIPED TO NEIGHBOR", "PUBLIC TAP/STANDPIPE", "TUBEWELL OR BOREHOLE", "PROTECTED WELL", "UNPROTECTED WELL", "RAINWATER", "TANKER TRUCK", "CART WITH SMALL TANK", "SURFACE WATER (RIVER/DAM/POND/STREAM/CANAL/IRRIGATION CHANNEL)", "BOTTLED WATER", "SATCHET WATER"),
  hh_water_location = c("IN OWN DWELLING", "IN OWN YARD/PLOT", "ELSEWHERE"),
  hh_water_time_trip = c("LESS THAN 1 MINUTE", paste(1:30, "MINUTES"), "MORE THAN 30 MINUTES", "I DON'T KNOW"),
  hh_toilet_type = c("FLUSH TO PIPE SEWER SYSTEM", "FLUSH TO SEPTIC TANK", "FLUSH TO PIT LATRINE", "FLUSH TO SOMEWHERE ELSE", "FLUSH DON'T KNOW WHERE", "VENTILATED IMPROVED PIT LATRINE", "PIT LATRINE WITH SLAB", "PIT LATRINE WITHOUT SLAB/OPEN PIT", "COMPOSTING TOILET", "BUCKET TOILET", "HANGING TOILET/HANGING LATRINE", "NO FACILITY/BUSH/FIELD", "OTHERS (SPECIFY)"),
  hh_toilet_shared = c("YES", "NO"),
  hh_toilet_share_count = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10 OR MORE", "I DON'T KNOW"),
  hh_toilet_location = c("IN OWN DWELLING", "IN OWN YARD/PLOT", "ELSEWHERE"),
  hh_cookstove_type = c("ELECTRIC STOVE", "SOLAR COOKER", "LIQUIFIED PETROLEUM GAS (LPG)/COOKING GAS STOVE", "PIPED NATURAL GAS STOVE", "BIOGAS STOVE", "KEROSENE STOVE", "MANUFACTURED SOLID FUEL STOVE", "TRADITIONAL SOLID FUEL STOVE", "THREE STONE STOVE", "OTHERS (SPECIFY)", "NO FOOD COOKED IN HOUSEHOLD"),
  hh_cookstove_fuel = c("ALCOHOL/ETHANOL", "GASOLINE/DIESEL", "KEROSENE/PARAFFIN", "COAL/LIGNITE", "CHARCOAL", "WOOD", "STRAW/SHRUBS/GRASS", "AGRICULTURAL CROP", "ANIMAL DUNG/WASTE", "PROCESSED BIOMASS (PELLETS)/WOODCHIPS", "GARBAGE/PLASTIC", "SAWDUST", "OTHERS (SPECIFY)", "NO FOOD COOKED IN HOUSEHOLD"),
  hh_owns_livestock = c("YES", "NO"),
  hh_num_cows_bulls = c("I DON'T KNOW", paste(0:95), "95 OR MORE"),
  hh_num_goats = c("I DON'T KNOW", paste(0:95), "95 OR MORE"),
  hh_owns_agri_land = c("YES", "NO"),
  hh_floor_material = c("EARTH/SAND", "DUNG", "WOOD PLANKS", "PALM/BAMBOO", "PARQUET/POLISHED WOOD", "VINYL/ASPHALT STRIPS", "CERAMIC TILES", "CEMENT", "CARPET", "OTHER (SPECIFY)"),
  hh_roof_material = c("NO ROOF", "THATCH/PALM LEAF", "GRASS", "RUSTIC MAT", "PALM/BAMBOO", "WOOD PLANKS", "CARDBOARD", "METAL/ZINC", "WOOD", "CELAMINE/CEMENT FIBER", "CERAMIC TILES", "CEMENT", "ROOFING SHYNGLES", "ASBESTOS", "OTHER (SPECIFY)"),
  hh_wall_material = c("NO WALLS", "CANE/PALM/TRUNKS", "DIRT", "BAMBOO WITH MUD", "STONE WITH MUD", "UNCOVERED ADOBE", "PLYWOOD", "CARDBOARD", "REUSED WOOD", "CEMENT", "STONE WITH LIME/CEMENT", "BRICKS", "CEMENT BLOCKS", "COVERED ADOBE", "WOOD PLANKS/SHYNGLES", "OTHER (SPECIFY)")
)

# --- PROGRAMMATICALLY ADD ASSETS (Individual Columns) ---
asset_cols <- c("hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone", 
                "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair", 
                "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", "hh_has_electric_iron", 
                "hh_has_generator", "hh_has_fan", "hh_own_watch", "hh_own_mobile_phone", 
                "hh_own_bicycle", "hh_own_motorcycle", "hh_own_animal_cart", "hh_own_car_truck", 
                "hh_own_motor_boat", "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account", 
                "hh_mobile_money_usage")

for(col in asset_cols) {
  mis_data_dictionary[[col]] <- c("YES", "NO", "I DON'T KNOW")
}

# --- PROGRAMMATICALLY ADD LIVESTOCK & AGRIC COUNTS ---
count_cols <- c("hh_num_other_cattle", "hh_num_horses_donkeys", "hh_num_sheep", 
                "hh_num_poultry", "hh_num_pigs", "hh_num_camels", "hh_num_agri_plots")

for(col in count_cols) {
  mis_data_dictionary[[col]] <- c("I DON'T KNOW", paste(0:95), "95 OR MORE")
}

# 3. Define Variable Labels (Short Name for Descriptive Question/Analysis)

variable_labels <- c(
  # --- Metadata & Demographics ---
  "meta_date_created"               = "Date record was created",
  "meta_respondent_id"              = "Unique Respondent ID",
  "meta_status"                     = "Interview Completion Status",
  "meta_response_date"              = "Date response was made",
  "meta_channel"                    = "Channel response was received",
  "meta_longitude"                  = "Longitude of the respondent's location",
  "meta_latitude"                   = "Latitude of the respondent's location",
  "meta_survey_code"                = "Code identifying the survey",
  "meta_survey_id"                  = "Unique survey ID",
  "meta_survey_name"                = "Name of the survey",
  "gps_state_match"                 = "Spatial validation for respondents' State",
  "gps_lga_match"                   = "Spatial validation for respondents' LGA",  
  "detected_state"                  = "State detected based on spatial or other data",
  "detected_lga"                    = "Local Government Area detected based on spatial or other data",
  "demo_state"                      = "State of residence",
  "demo_lga"                        = "Local Government Area",
  "demo_town"                       = "Town/Village name",
  "demo_age"                        = "Age of respondent",
  "demo_year_of_birth"              = "Year of birth (Numeric or 9997 for DK >15)",
  "demo_gender"                     = "Gender of respondent",
  "demo_edu_level"                  = "Highest level of formal education attended",
  "demo_edu_informal"               = "Informal education level of the respondent",
  "demo_hh_children_under5"         = "Number of children under age 5 in household",
  "demo_hh_sleeping_rooms"          = "Number of rooms used for sleeping in the house",
  "urban_status"                    = "Urban status of the geocoordinates polygon match",
  "lga_dist_error_km"               = "Spatial: Distance error from reported LGA (km)",
  
  # --- Malaria Prevention ---
  "prev_has_mosquito_nets"          = "Household has any mosquito nets",
  "prev_is_itn"                     = "Respondent knows if net is insecticide-treated (ITN)", 
  "prev_num_mosquito_nets"          = "Number of mosquito nets owned by household",
  "prev_months_since_net_obtained"  = "Months ago household obtained the mosquito net",
  "prev_net_brand"                  = "Brand of mosquito net used",
  "prev_net_obtained_how"           = "How the mosquito net was obtained",
  "prev_net_obtained_where"         = "Where the net was obtained from",
  "prev_slept_under_net_last_night" = "Respondent or anyone slept under net last night",
  "prev_num_people_slept_net"       = "Number of people who slept inside the net last night",
  "prev_net_not_used_reason"        = "Main reason net was not used last night",
  "prev_home_sprayed_interior"      = "Home interior walls sprayed for mosquitoes (last 12m)",
  "prev_first_treatment_location"   = "Location first visited for malaria treatment",
  "prev_time_to_treatment_facility" = "Travel time to reach nearest malaria treatment facility",
  "prev_repellent_methods"          = "Mosquito repellent methods used (Raw Text)",
  
  # --- Malaria Treatment ---
  "treat_transport_cost"            = "Cost of transport to/from health facility",
  "treat_hh_fever_last_2weeks"      = "Any household member had fever in last 2 weeks",
  "treat_blood_sample_taken"        = "Blood sample taken for testing before treatment",
  "treat_test_cost"                 = "Cost of the malaria test",
  "treat_drug_cost"                 = "Cost of the last malaria drugs purchased",
  "treat_drug_purchase_time"        = "Time since last malaria drug was bought",
  "treat_drug_affordability"        = "Rating of malaria drug affordability in community",
  "treat_heard_smc"                 = "Heard about Seasonal Malaria Chemoprevention (SMC)",
  "treat_children_received_smc"     = "Children ever received SMC treatment",
  "treat_know_smc_drug"             = "Knows drug administered during SMC",
  "treat_vaccine_age_knowledge"     = "Knowledge of age for first malaria vaccine dose",
  "treat_children_received_vaccine" = "Children ever received a malaria vaccine dose",
  
  # --- Feedback ---
  "feedback_free_treatment_6months" = "Received free malaria treatment at govt facility (last 6m)",
  "feedback_drug_stockout_6months"  = "Experienced drug stockout at govt facility (last 6m)",
  "feedback_gov_effort_rating"      = "Rating of government efforts in combating malaria",
  
  # --- Women's Health & Child Health ---
  "women_ever_given_birth"          = "Respondent has ever given birth",
  "women_births_2020_2025"          = "Number of children born between 2020 and 2025",
  "women_anc_seen"                  = "Received ANC during last pregnancy",
  "women_anc_provider"              = "Provider seen for ANC (Raw Text)",
  "women_anc_location"              = "Location where ANC was received (Raw Text)",
  "women_anc_first_visit_month"     = "Months/weeks pregnant at first ANC visit",
  "women_anc_total_visits"          = "Total number of ANC visits during pregnancy",
  "women_took_sp_fansidar"          = "Took SP/Fansidar for malaria prevention during pregnancy",
  "women_sp_fansidar_doses"         = "Number of times SP/Fansidar was taken",
  "women_sp_fansidar_source"        = "Source where SP/Fansidar was obtained (Raw Text)",
  "women_child_fever_2weeks"        = "Youngest child had fever in last 2 weeks",
  "women_child_blood_sample"        = "Child had blood sample taken for testing",
  "women_child_malaria_diagnosis"   = "Healthcare provider confirmed child had malaria",
  "women_child_seek_advice"         = "Sought advice/treatment for child's fever",
  "women_child_advice_location"     = "Location sought for advice/treatment (Raw Text)",
  "women_child_first_advice_location" = "First location sought for advice/treatment",
  "women_child_advice_delay_days"   = "Days after illness started before seeking advice",
  "women_child_referral"            = "Child referred to higher level of care",
  "women_child_took_medicine"       = "Child took any medicine for the illness",
  "women_child_medicine_type"       = "Type of medicine child took (Raw Text)",
  "women_child_act_delay"           = "Days after fever started before child took ACT",
  "women_child_act_effective"       = "Fever went away after child took ACT",
  "women_currently_pregnant"        = "Respondent is currently pregnant",
  "women_pregnancy_duration_months" = "Duration of current pregnancy (weeks/months)",
  
  # --- Background Information ---
  "bg_languages"                    = "Languages spoken by respondent (Raw Text)",
  "bg_tv_frequency"                 = "Frequency of watching television",
  "bg_own_smartphone"               = "Respondent owns a smartphone",
  "bg_internet_ever_used"           = "Respondent has ever used the internet",
  "bg_internet_frequency"           = "Frequency of internet use",
  "bg_religion"                     = "Religion of respondent",
  "bg_ethnic_group"                 = "Ethnic group of respondent",
  "bg_heard_malaria_msg_6months"    = "Heard/seen malaria messages in past 6 months",
  "bg_malaria_msg_source"           = "Source of malaria messages (Raw Text)",
  "bg_aware_avoidance"              = "Aware of ways to avoid malaria",
  "bg_prevention_knowledge"         = "Known methods of malaria prevention (Raw Text)",
  
  # --- Attitudes ---
  "att_rainy_season_only"           = "Attitude: Malaria only occurs during rainy season",
  "att_fever_worry_malaria"         = "Attitude: Almost always worry fever is malaria",
  "att_malaria_easily_treated"      = "Attitude: Malaria is not a problem; easily treated",
  "att_weak_children_die"           = "Attitude: Only weak children die from malaria",
  "att_net_use_mosquito_density"    = "Attitude: Only need net when mosquitoes are many",
  "att_net_use_warm_weather"        = "Attitude: Dislike net when weather is too warm",
  "att_home_meds_first"             = "Attitude: Best to start with home meds for fever",
  "att_full_dose_importance"        = "Attitude: Important for child to finish malaria dose",
  "att_seek_care_immediate"         = "Attitude: Community seeks care same/next day",
  "att_community_net_usage"         = "Attitude: Community net owners sleep in them nightly",
  
  # --- Household Information & Assets ---
  "hh_total_persons_v1"             = "Total persons in household (V1)",
  "hh_total_persons_v2"             = "Total persons in household (V2)",
  "hh_members_under5"               = "Number of household members under 5 years old",
  "hh_total_persons_usually_v3"     = "Total persons usually living in household (V3)",
  "hh_relation_to_head"             = "Respondent relationship to household head",
  "hh_drinking_water_source"        = "Main source of drinking water",
  "hh_other_water_source"           = "Main source of water for other purposes",
  "hh_water_location"               = "Location of the water source",
  "hh_water_time_trip"              = "Time taken to fetch water (round trip)",
  "hh_toilet_type"                  = "Type of toilet facility used",
  "hh_toilet_shared"                = "Toilet facility is shared with other households",
  "hh_toilet_share_count"           = "Number of households sharing the toilet",
  "hh_toilet_location"              = "Location of the toilet facility",
  "hh_cookstove_type"               = "Main type of cookstove used",
  "hh_cookstove_fuel"               = "Main energy source for the cookstove",
  "hh_owns_livestock"               = "Household owns livestock/animals/poultry",
  "hh_num_cows_bulls"               = "Number of cows or bulls owned",
  "hh_num_other_cattle"             = "Number of other cattle owned",
  "hh_num_horses_donkeys"           = "Number of horses, donkeys, or mules owned",
  "hh_num_goats"                    = "Number of goats owned",
  "hh_num_sheep"                    = "Number of sheep owned",
  "hh_num_poultry"                  = "Number of chickens or other poultry owned",
  "hh_num_pigs"                     = "Number of pigs owned",
  "hh_num_camels"                   = "Number of camels owned",
  "hh_owns_agri_land"               = "Household member owns agricultural land",
  "hh_num_agri_plots"               = "Number of agricultural land plots owned",
  "hh_has_electricity"              = "Household has electricity",
  "hh_has_radio"                    = "Household has a radio",
  "hh_has_tv"                       = "Household has a television",
  "hh_has_non_mobile_phone"         = "Household has a non-mobile phone",
  "hh_has_computer"                 = "Household has a computer",
  "hh_has_refrigerator"             = "Household has a refrigerator",
  "hh_has_table"                    = "Household has a table",
  "hh_has_chair"                    = "Household has a chair",
  "hh_has_bed"                      = "Household has a bed",
  "hh_has_sofa"                     = "Household has a sofa",
  "hh_has_cupboard"                 = "Household has a cupboard",
  "hh_has_ac"                       = "Household has an air conditioner",
  "hh_has_electric_iron"            = "Household has an electric iron",
  "hh_has_generator"                = "Household has a generator",
  "hh_has_fan"                      = "Household has a fan",
  "hh_own_watch"                    = "Member owns a watch",
  "hh_own_mobile_phone"             = "Member owns a mobile phone",
  "hh_own_bicycle"                  = "Member owns a bicycle",
  "hh_own_motorcycle"               = "Member owns a motorcycle or scooter",
  "hh_own_animal_cart"              = "Member owns an animal-drawn cart",
  "hh_own_car_truck"                = "Member owns a car or truck",
  "hh_own_motor_boat"               = "Member owns a boat with a motor",
  "hh_own_canoe"                    = "Member owns a canoe",
  "hh_own_keke_napep"               = "Member owns a keke napep",
  "hh_has_bank_account"             = "Member has a bank account",
  "hh_mobile_money_usage"           = "Member uses mobile phone for financial transactions",
  "hh_floor_material"               = "Main material of the floor",
  "hh_roof_material"                = "Main material of the roof",
  "hh_wall_material"                = "Main material of the walls",
  
  # --- Dummy Variables (Informal Education) ---
  "edu_inf_none"     = "Informal Edu: None",
  "edu_inf_adult"    = "Informal Edu: Adult literacy",
  "edu_inf_tsangaya" = "Informal Edu: Tsangaya",
  "edu_inf_quranic"  = "Informal Edu: Quranic",
  "edu_inf_other"    = "Informal Edu: Other",
  
  # --- Dummy Variables (Repellents) ---
  "repel_coils"    = "Repellent: Mosquito Coils",
  "repel_spray"    = "Repellent: Sprays/Insecticides",
  "repel_cream"    = "Repellent: Creams",
  "repel_electric" = "Repellent: Electronic devices",
  "repel_none"     = "Repellent: None",
  
  # --- Dummy Variables (ANC Providers) ---
  "anc_prov_doc"   = "ANC Provider: Doctor",
  "anc_prov_nurse" = "ANC Provider: Nurse/Midwife",
  "anc_prov_aux"   = "ANC Provider: Auxiliary Midwife",
  "anc_prov_chew"  = "ANC Provider: CHEW/Extension Worker",
  "anc_prov_tba"   = "ANC Provider: Trad Birth Attendant",
  "anc_prov_field" = "ANC Provider: Comm Health Worker",
  "anc_prov_other" = "ANC Provider: Other",
  
  # --- Dummy Variables (ANC Locations) ---
  "anc_loc_govhos"  = "ANC Loc: Govt Hospital",
  "anc_loc_govcen"  = "ANC Loc: Govt Health Center",
  "anc_loc_govpost" = "ANC Loc: Govt Health Post",
  "anc_loc_pvt"     = "ANC Loc: Private Facility",
  "anc_loc_ngohos"  = "ANC Loc: NGO Hospital",
  "anc_loc_ngoclin" = "ANC Loc: NGO Clinic",
  "anc_loc_herhome"    = "ANC Loc: Provider home",
  "anc_loc_yourhome"    = "ANC Loc: Respondent home",
  "anc_loc_other"   = "ANC Loc: Other",
  
  # --- Dummy Variables (SP Source) ---
  "sp_src_anc"      = "SP Source: ANC visit",
  "sp_src_nonanc"   = "SP Source: Other health facility visit",
  "sp_src_pharm"    = "SP Source: Pharmacy/Chemist",
  "sp_src_chew"     = "SP Source: Community health worker",
  "sp_src_other"    = "SP Source: Others",
  
  # --- Dummy Variables (Child Advice Location) ---
  "child_adv_gov"   = "Child Advice: Govt Health Facility",
  "child_adv_pvth"  = "Child Advice: Private Hospital",
  "child_adv_ngo"   = "Child Advice: NGO Facility",
  "child_adv_mob"   = "Child Advice: Mobile Clinic",
  "child_adv_pvtd"  = "Child Advice: Private Doctor",
  "child_adv_com"   = "Child Advice: Community health worker",
  "child_adv_pharm" = "Child Advice: Pharmacy",
  "child_adv_chem"  = "Child Advice: Chemist",
  "child_adv_trad"  = "Child Advice: Traditional Practitioner",
  "child_adv_rel"   = "Child Advice: Religious Institution",
  "child_adv_other" = "Child Advice: Other",
  
  # --- Dummy Variables (Child Medicine) ---
  "med_act"         = "Medicine: ACT",
  "med_sp"          = "Medicine: SP/Fansidar",
  "med_chloro"      = "Medicine: Chloroquine",
  "med_amod"        = "Medicine: Amodiaquine",
  "med_artesun"     = "Medicine: Artesunate",
  "med_quinine"     = "Medicine: Quinine",
  "med_inject"      = "Medicine: Injection",
  "med_other"       = "Medicine: Other",
  "med_dont"        = "Medicine: Don't Know",
  
  # --- Dummy Variables (Languages) ---
  "lang_en"         = "Language: English",
  "lang_yo"         = "Language: Yoruba",
  "lang_ha"         = "Language: Hausa",
  "lang_ig"         = "Language: Igbo",
  "lang_ot"         = "Language: Others",
  
  # --- Dummy Variables (Message Sources) ---
  "msg_radio"       = "Msg Source: Radio",
  "msg_tv"          = "Msg Source: Television",
  "msg_poster"      = "Msg Source: Poster/Billboard",
  "msg_news"        = "Msg Source: Newspaper/Magazine",
  "msg_leaf"        = "Msg Source: Leaflet/Brochure",
  "msg_hcp"         = "Msg Source: Healthcare provider",
  "msg_chw"         = "Msg Source: Community health worker",
  "msg_social"      = "Msg Source: Social Media",
  "msg_town"        = "Msg Source: Town Announcer",
  "msg_ipc"         = "Msg Source: Inter-personal communication",
  "msg_family"      = "Msg Source: Family/Friends",
  
  # --- Dummy Variables (Prevention Knowledge) ---
  "know_net"        = "Knowledge: Mosquito net",
  "know_itn"        = "Knowledge: ITN",
  "know_repel"      = "Knowledge: Repellent/Coil",
  "know_stag"       = "Knowledge: Clear stagnant water",
  "know_spray"      = "Knowledge: Insecticide spray",
  "know_meds"       = "Knowledge: Preventative meds",
  "know_clean"      = "Knowledge: Clean surroundings",
  "know_screens"    = "Knowledge: Window screens",
  "know_other"      = "Knowledge: Other",
  "know_dont"       = "Knowledge: Don't know",
  
  # --- Derived stratifiers / indices ---
  "derived_zone"                    = "Derived geopolitical zone (6 zones; 9=Missing)",
  "derived_residence"               = "Derived residence type (Urban/Rural; 9=Missing)",
  "derived_wealth_score"            = "Wealth score (PCA first component; complete cases only)",
  "derived_wealth_quintile"         = "Derived wealth quintile (1=Lowest ... 5=Highest)",
  "wealth_q"                        = "Wealth quintile (numeric copy of derived_wealth_quintile)",
  "derived_edu_cat"                 = "Derived education category (4-level consolidation)",
  "derived_age_group_w"             = "Derived women age group (15-49, 5-year bands; 9=Missing)",
  "zone_num"                        = "Zone code (numeric copy of derived_zone)",
  "residence_num"                   = "Residence code (numeric copy of derived_residence)",
  "edu_cat"                         = "Education category code (numeric copy of derived_edu_cat)",
  
  # --- Universe flags (denominator definitions) ---
  "u_all"                           = "Universe: all respondents",
  "u_household"                     = "Universe: household-level tables (respondent-as-household)",
  "u_women_15_49"                    = "Universe: women age 15-49",
  "births_2020_2025_num"             = "Births 2020-2025 (numeric, type-safe)",
  "u_recent_birth"                  = "Universe: women with recent birth proxy (births_2020_2025 > 0)",
  "hh_u5_num"                       = "Household children under 5 count (numeric, type-safe)",
  "u_hh_has_u5"                     = "Universe: household has child under 5 (1/0; NA if unknown)",
  "u_child_youngest"                = "Universe: youngest child module (follows household has under-5)",
  "u_child_fever"                   = "Universe: child fever module (child <5 in universe)",
  "u_msg"                           = "Universe: heard malaria message in last 6 months",
  "u_avoid"                         = "Universe: aware of malaria avoidance",
  "u_internet"                      = "Universe: ever used internet",
  
  # --- Derived indicators: Nets / ITNs ---
  "derived_hh_has_any_net"          = "Household owns any mosquito net (0/1; NA out-of-universe/DK)",
  "derived_hh_has_itn"              = "Household has ITN/LLIN (0/1; NA out-of-universe/DK)",
  "prev_is_itn_num"                 = "Net type ITN indicator (numeric, type-safe)",
  "prev_net_brand_num"              = "Net brand code (numeric, type-safe)",
  "derived_num_itns"                = "Number of ITNs in household (proxy; NA if missing)",
  "derived_access_itn"              = "ITN access proxy (>=1 ITN per 2 household members)",
  "derived_net_use_any_last_night"  = "Any household member slept under a net last night (0/1; NA DK)",
  "derived_net_use_people_count"    = "Count of people who slept under net (if any used; NA otherwise)",
  
  # --- Derived indicators: ANC / IPTp ---
  "derived_anc_any"                 = "Any ANC visit among women with recent birth proxy (0/1; NA out-of-universe)",
  "derived_anc_skilled"             = "Skilled ANC provider among women with recent birth proxy (0/1; NA out-of-universe)",
  "anc_visits_num"                  = "Total ANC visits (numeric, type-safe)",
  "derived_anc_4plus"               = "Four or more ANC visits among women with recent birth proxy (0/1; NA out-of-universe)",
  "sp_taken_num"                    = "Took SP/Fansidar during pregnancy (numeric, type-safe)",
  "sp_doses_num"                    = "Number of SP/Fansidar doses (numeric, type-safe)",
  "derived_iptp_1plus"              = "IPTp1+ (>=1 SP dose) among women with recent birth proxy (0/1; NA out-of-universe)",
  "derived_iptp_2plus"              = "IPTp2+ (>=2 SP doses) among women with recent birth proxy (0/1; NA out-of-universe)",
  "derived_iptp_3plus"              = "IPTp3+ (>=3 SP doses) among women with recent birth proxy (0/1; NA out-of-universe)",
  
  # --- Derived indicators: Child fever management ---
  "derived_child_fever"             = "Child had fever in last 2 weeks (youngest child <5; 0/1; NA out-of-universe)",
  "derived_fever_seek_advice"       = "Sought advice/treatment for child fever (0/1; NA out-of-universe)",
  "delay_num"                       = "Delay to seek care (days; numeric, type-safe)",
  "derived_fever_prompt_care"       = "Prompt care for child fever (same/next day; 0/1; NA out-of-universe)",
  "derived_fever_tested"            = "Child fever tested (blood sample) (0/1; NA out-of-universe)",
  "derived_fever_took_any_medicine" = "Child took any medicine for fever (0/1; NA out-of-universe)",
  "derived_fever_took_anti"         = "Child took any antimalarial for fever (0/1; NA out-of-universe)",
  "derived_fever_took_act"          = "Child took ACT for fever (0/1; NA out-of-universe)",
  "derived_fever_took_sp"           = "Child took SP for fever (0/1; NA out-of-universe)",
  "derived_fever_took_chloro"       = "Child took chloroquine for fever (0/1; NA out-of-universe)",
  "derived_fever_took_amod"         = "Child took amodiaquine for fever (0/1; NA out-of-universe)",
  "derived_fever_took_quin"         = "Child took quinine for fever (0/1; NA out-of-universe)",
  "derived_fever_took_artes"        = "Child took artesunate/artemisinin for fever (0/1; NA out-of-universe)",
  
  # --- ACT timing & effectiveness (among ACT takers) ---
  "act_delay_num"                   = "Delay to start ACT (numeric, type-safe)",
  "derived_act_prompt"              = "ACT started promptly (same/next day) among ACT takers (0/1; NA out-of-universe)",
  "derived_act_effective"           = "ACT perceived effective among ACT takers (0/1; NA out-of-universe)",
  
  # --- Perceptions & experiences ---
  "afford_num"                      = "Drug affordability rating (numeric, type-safe)",
  "derived_percep_affordable"       = "Perceived drugs affordable (1-2 vs 3-5) (0/1; NA missing)",
  "gov_num"                         = "Government effort rating (numeric, type-safe)",
  "derived_gov_effective"           = "Government perceived effective (ratings 4-5 vs 1-3) (0/1; NA missing)",
  "stockout_num"                    = "Experienced stockout in last 6 months at government facility (numeric, type-safe)",
  "free_num"                        = "Received free treatment in last 6 months at government facility (numeric, type-safe)",
  "u_govt_visit_6m_stockout"         = "Universe: visited government facility in last 6 months (stockout question applicable)",
  "u_govt_visit_6m_free"             = "Universe: visited government facility in last 6 months (free treatment question applicable)",
  "derived_exp_stockout"            = "Experienced stockout (among govt visitors) (0/1; NA out-of-universe)",
  "derived_exp_free_tx"             = "Received free treatment (among govt visitors) (0/1; NA out-of-universe)",
  "demo_gender_num"                 = "Numeric representation of respondent's gender (1=Male, 2=Female)",
  "demo_age_num"                    = "Numeric representation of respondent's age",
  "demo_state_num"                  = "Numeric code representing respondent's state of residence",
  "hh_nets_num"                     = "Number of mosquito nets in the household",
  "hh_size_num"                     = "Total number of people in the household",
  "ppl_slept_net_num"               = "Number of people who slept under the mosquito net last night",
  # --- Additional derived variables / weighting helpers / top-code flags ---
  "derived_age_group_4"              = "Derived age group (15-24, 25-34, 35-44, 45+)",
  "age_group_4"                      = "Age group 4-category code (numeric copy of derived_age_group_4)",
  
  "anc_first_month_num"              = "Months pregnant at first ANC visit (0 = less than one month, including 2-3 weeks)",
  "preg_month_num"                   = "Current pregnancy duration in months (0 = less than one month, including 2-3 weeks)",
  "derived_anc_first_trimester"      = "First ANC visit occurred in first trimester (0/1; 0 month includes less than one month, 2 weeks, and 3 weeks)",
  "derived_currently_pregnant"       = "Currently pregnant (0/1; NA if missing or uncertain)",
  "derived_pregnancy_first_trimester"= "Current pregnancy is in first trimester (0/1; 0 month includes less than one month, 2 weeks, and 3 weeks)",
  
  "derived_prop_hh_slept_under_net"  = "Proportion of household members who slept under a mosquito net last night",
  "u_affordability"                  = "Universe: affordability question observed",
  "u_recent_fever_hh"                = "Universe: household had fever in last 2 weeks",
  "test_cost_num"                    = "Cost of malaria test (numeric, type-safe)",
  "drug_cost_num"                    = "Cost of malaria drugs (numeric, type-safe)",
  "drug_buy_time_num"                = "Time since malaria drug purchase (numeric, type-safe)",
  "derived_test_paid"                = "Paid anything for malaria test among households with recent fever (0/1; NA out-of-universe)",
  "derived_drug_paid"                = "Paid anything for malaria drugs among households with recent fever (0/1; NA out-of-universe)",
  
  "tc_demo_hh_children_under5"       = "Top-code flag for number of children under age 5 in household",
  "tc_demo_hh_sleeping_rooms"        = "Top-code flag for number of rooms used for sleeping",
  "tc_prev_num_mosquito_nets"        = "Top-code flag for number of mosquito nets owned by household",
  "tc_prev_num_people_slept_net"     = "Top-code flag for number of people who slept under mosquito net last night",
  "tc_prev_months_since_net_obtained"= "Top-code flag for months since household obtained mosquito net",
  "tc_women_births_2020_2025"        = "Top-code flag for number of births between 2020 and 2025",
  "tc_women_anc_total_visits"        = "Top-code flag for total ANC visits during pregnancy",
  "tc_women_sp_fansidar_doses"       = "Top-code flag for number of SP/Fansidar doses taken during pregnancy",
  "tc_hh_total_persons_v1"           = "Top-code flag for total persons in household (V1)",
  "tc_hh_total_persons_v2"           = "Top-code flag for total persons in household (V2)",
  "tc_hh_members_under5"             = "Top-code flag for number of household members under 5 years old",
  "tc_hh_total_persons_usually_v3"   = "Top-code flag for total persons usually living in household (V3)",
  "tc_hh_water_time_trip"            = "Top-code flag for time taken to fetch water (round trip)",
  "tc_hh_toilet_share_count"         = "Top-code flag for number of households sharing the toilet facility",
  "tc_hh_num_cows_bulls"             = "Top-code flag for number of cows or bulls owned",
  "tc_hh_num_other_cattle"           = "Top-code flag for number of other cattle owned",
  "tc_hh_num_horses_donkeys"         = "Top-code flag for number of horses, donkeys, or mules owned",
  "tc_hh_num_goats"                  = "Top-code flag for number of goats owned",
  "tc_hh_num_sheep"                  = "Top-code flag for number of sheep owned",
  "tc_hh_num_poultry"                = "Top-code flag for number of chickens or other poultry owned",
  "tc_hh_num_pigs"                   = "Top-code flag for number of pigs owned",
  "tc_hh_num_camels"                 = "Top-code flag for number of camels owned",
  "tc_hh_num_agri_plots"             = "Top-code flag for number of agricultural land plots owned",
  
  "w_base"                           = "Base weight for calibration (set to 1 for all eligible records)",
  "w_calibrated_raw"                 = "Raw calibrated post-stratification weight before trimming",
  "w_calibrated_trim"                = "Trimmed and rescaled calibrated post-stratification weight",
  "calib_ok"                         = "Eligible for calibration weighting based on complete benchmark variables"
)


# 4. Define Value Labels (Merged, Updated & Verified)

value_labels <- list(
  # --- Common Scales & Dummies ---
  yesno = c("No" = 0, "Yes" = 1, "Don't Know" = 8, "Missing" = 9),
  
  # Standard label for all 0/1/9 dummy variables
  dummy = c("No" = 0, "Yes" = 1, "Missing" = 9),
  
  status = c("Completed" = 1, "Missing" = 9 ),
  
  match = c("Match" = 1, "Mismatch" = 0, "Outside Boundary" = 7, "Missing" = 9 ),
  
  channel = c("Direct" = 1, "Outbound" = 2, "Missing" = 9),
  
  gender = c("Male" = 1, "Female" = 2, "Missing" = 9),
  
  education = c("No education" = 0, "Primary" = 1, "Secondary" = 2, 
                "Higher (Post Secondary)" = 3, "Vocational" = 4, "Missing" = 9),
  
  # --- Geography & Demographics ---
  states = c("Abia"=1, "Adamawa"=2, "Akwa Ibom"=3, "Anambra"=4, "Bauchi"=5, "Bayelsa"=6, 
             "Benue"=7, "Borno"=8, "Cross River"=9, "Delta"=10, "Ebonyi"=11, "Edo"=12, 
             "Ekiti"=13, "Enugu"=14, "FCT Abuja"=15, "Gombe"=16, "Imo"=17, "Jigawa"=18, 
             "Kaduna"=19, "Kano"=20, "Katsina"=21, "Kebbi"=22, "Kogi"=23, "Kwara"=24, 
             "Lagos"=25, "Nasarawa"=26, "Niger"=27, "Ogun"=28, "Ondo"=29, "Osun"=30, 
             "Oyo"=31, "Plateau"=32, "Rivers"=33, "Sokoto"=34, "Taraba"=35, "Yobe"=36, 
             "Zamfara"=37, "Missing"=99),
  
  ethnicity = c("Hausa"=1, "Yoruba"=2, "Igbo"=3, "Kanuri"=4, "Ibibio"=5, "Tiv"=6, 
                "Fulani"=7, "Edo"=8, "Ijaw"=9, "Nupe"=10, "Efik"=11, "Gwari"=12, 
                "Anang"=13, "Itsekiri"=14, "Jukun"=15, "Bini"=16, "Isoko"=17, 
                "Ogoni"=18, "Idoma"=19, "Gwandara"=20, "Ebira"=21, "Igbira"=22, 
                "Others"=96, "Missing"=99),
  
  age_special = c("Undisclosed (>15)" = 997),
  
  urban_status = c("Urban" = 1, "Rural" = 0),
  
  # --- Locations (Treatment & Advice) ---
  locations = c("Government Hospital" = 1, "Private Hospital/Clinic" = 2, "NGO Hospital" = 3, 
                "Mobile Clinic" = 4, "Private Doctor" = 5, "Community Nurse/Health Worker" = 6, 
                "Pharmacy" = 7, "Local Drug Store/Chemist" = 8, "Itinerant drug seller" = 9, "Traditional Healer" = 10, 
                "Religious Healer" = 11, "Missing" = 99),
  
  # --- Financial Costs ---
  costs_standard = c("Free" = 0, "N1 - N999" = 1, "N1,000 - N1,999" = 2, "N2,000 - N2,999" = 3, 
                     "N3,000 - N3,999" = 4, "N4,000 - N4,999" = 5, "N5,000 - N5,999" = 6, 
                     "N6,000 - N6,999" = 7, "N7,000 - N7,999" = 8, "N8,000 - N8,999" = 9, 
                     "N9,000 - N9,999" = 10, "N10,000 - N10,999" = 11, "N11,000 - N11,999" = 12, 
                     "N12,000 - N12,999" = 13, "N13,000 - N13,999" = 14, "N14,000 - N14,999" = 15, 
                     "Above N15,000" = 16, "Don't Know" = 98, "Missing" = 99),
  
  costs_transport = c("Free" = 0, "N1 - N999" = 1, "N1,000 - N1,999" = 2, "N2,000 - N2,999" = 3, 
                      "N3,000 - N3,999" = 4, "N4,000 - N4,999" = 5, "N5,000 - N5,999" = 6, 
                      "N6,000 - N6,999" = 7, "N7,000 - N7,999" = 8, "N8,000 - N8,999" = 9, 
                      "N9,000 - N9,999" = 10, "Above N10,000" = 11, "Don't Know" = 98, "Missing" = 99),
  
  # --- Timing & Delays ---
  time_facility = c("Less than 30 mins" = 1, "30 mins to 1 hour" = 2, "More than 1 hour" = 3, 
                    "Don't Know" = 8, "Missing" = 9),
  
  delays = c("Same Day" = 0, "Next Day" = 1, "2 Days" = 2, "3+ Days" = 3, 
             "Don't Know" = 8, "Missing" = 9),
  
  drug_purchase = c("0-4 Weeks ago" = 1, "1-5 Months ago" = 2, 
                    "6-12 Months ago" = 3, "Over one year ago" = 4, 
                    "Don't Know" = 8, "Missing" = 9),
  
  months_special = c(
    "Less than one month" = 0,
    "10 months or More" = 10, 
    "Don't Know" = 98
  ),
  
  # --- Net Information ---
  net_brands = c("LLIN" = 10, "Olyset LLIN" = 11, "Iconlife LLIN" = 12, "Duranet LLIN" = 13,
                 "Netprotect LLIN" = 14, "BASF Interceptor LLIN" = 15, "Yorkool LLIN" = 16,
                 "Magnet LLIN" = 17, "Dawaplus 2.0 LLIN" = 18, "Royal Security LLIN" = 19,
                 "Royal Sentry LLIN" = 20, "PermaNet 2.0 LLIN" = 21, "PermaNet 3.0 LLIN" = 22,
                 "Veeralin LLIN" = 23, "Interceptor G2 LLIN" = 24, "Royal Guard LLIN" = 25,
                 "Other LLIN/DK Brand" = 26, "Other (Not LLIN)" = 95, "Other" = 96, 
                 "Don't Know" = 98, "Missing" = 99),
  
  net_obtained_how = c("Mass Campaign" = 1, "ANC" = 2, "Immunisation" = 3, "Other" = 96, "Missing" = 99),
  
  net_obtained_where = c("Government" = 1, "Private Sector" = 2, "Pharmacy" = 3, "Shop/Market" = 4,
                         "CHW" = 5, "Religious Inst" = 6, "School" = 7, "Other" = 96, 
                         "Don't Know" = 98, "Missing" = 99),
  
  net_no_use = c("No mosquitoes"=1, "No malaria"=2, "Too hot"=3, "Don't like smell"=4, 
                 "Feel Closed In"=5, "Net too old/torn"=6, "Net too dirty"=7, 
                 "Net not available (Washing)"=8, "Usual users away"=9, "Net not needed"=10, 
                 "Bed bugs"=11, "Don't Know"=98, "Others"=96, "Missing"=99),
  
  # --- SMC, Vaccine & Feedback (Fix B: Logic Alignment) ---
  smc_eligibility = c("Yes" = 1, "No (Has Child <5)" = 0, "No (No Child <5)" = 6, 
                      "Don't Know/Not Sure" = 8, "Missing" = 9),
  
  feedback_health = c("Yes" = 1, "No (Went to facility)" = 0, "No (Did not go)" = 6, "Missing" = 9),
  
  smc_drugs = c("SP + AQ" = 1, "SP Only" = 2, "AL (Coartem)" = 3, "AS + AQ" = 4, 
                "DHA-PPQ" = 5, "Proguanil" = 6, "Other" = 96, "Don't Know" = 98, "Missing" = 99),
  
  vaccine_age = c("Less than 12 months" = 1, "1 year" = 2, "2 years" = 3, "3 years" = 4, 
                  "4 years" = 5, "5 years" = 6, "Above 5 years" = 7, "Don't Know" = 98, "Missing" = 99),
  
  # --- Attitudes & Rating (Fix D: Likert vs Proportions) ---
  attitudes = c("Agree" = 1, "Disagree" = 2, "Don't Know/Uncertain" = 8, "Missing" = 9),
  
  proportions = c("More than half" = 1, "Less than half" = 2, "Don't Know" = 8, "Missing" = 9),
  
  affordability = c("Very Affordable" = 1, "Somewhat Affordable" = 2, "Neutral" = 3, 
                    "Somewhat Expensive" = 4, "Very Expensive" = 5, 
                    "Don't Know" = 8, "Missing" = 9),
  
  gov_effort = c("Very Effective" = 5, "Somewhat Effective" = 4, "Neutral" = 3, 
                 "Somewhat Ineffective" = 2, "Very Ineffective" = 1, "Missing" = 9),
  
  # --- Household Characteristics ---
  water_source = c("Piped in Dwelling/Yard/Plot" = 11, "Piped to Neighbor" = 12,
                   "Public Tap/Standpipe" = 13, "Tube Well or Borehole" = 21, "Protected Well" = 31,
                   "Unprotected Well" = 32, "Protected Spring" = 41, "Unprotected Spring" = 42, 
                   "Rainwater" = 51, "Tanker Truck" = 61, "Cart with Small Tank" = 71, 
                   "Surface Water" = 81, "Bottled Water" = 91, "Satchet Water" = 92, "Other" = 96, "Missing" = 99),
  
  toilet_type = c("Flush to Piped Sewer" = 11, "Flush to Septic Tank" = 12, "Flush to Pit Latrine" = 13,
                  "Flush to Somewhere Else" = 14, "Flush Don't Know" = 15, "VIP Latrine" = 21, 
                  "Pit Latrine with Slab" = 22, "Pit Latrine without Slab" = 23,
                  "Composting Toilet" = 31, "Bucket Toilet" = 41, "Hanging Toilet/Latrine" = 51,
                  "No Facility/Bush/Field" = 61, "Other" = 96, "Missing" = 99),
  
  facility_loc = c("In own dwelling" = 1, "In own yard/plot" = 2, "Elsewhere" = 3, "Missing" = 9),
  
  cookstove_type = c("Electric" = 1, "Solar" = 2, "LPG/Cooking Gas" = 3, "Piped Natural Gas" = 4,
                     "Biogas" = 5, "Kerosene" = 6, "Manufactured Solid Fuel" = 7, "Traditional Solid Fuel" = 8, 
                     "Three Stone/Open Fire" = 9, "No Food Cooked" = 95, "Other" = 96, "Missing" = 99),
  
  fuel_type = c("Alcohol" = 01, "Gasoline" = 02, "Kerosene" = 03, "Coal/Lignite" = 04,
                "Charcoal" = 05, "Wood" = 06, "Straw/Shrubs/Grass" = 07, "Agri Crop" = 08,
                "Animal Dung" = 09, "Biomass" = 10, "Garbage" = 11, "Sawdust" = 12,
                "Other" = 96, "Missing" = 99),
  
  floor_mat = c("Earth/Sand" = 11, "Dung" = 12, "Wood Planks" = 21, "Palm/Bamboo" = 22,
                "Parquet/Polished Wood" = 31, "Vinyl/Asphalt" = 32, "Ceramic Tiles" = 33, 
                "Cement" = 34, "Carpet" = 35, "Other" = 96, "Missing" = 99),
  
  roof_mat = c("No Roof" = 11, "Thatch/Palm Leaf" = 12, "Grass" = 13, "Rustic Mat" = 21, 
               "Palm/Bamboo" = 22, "Wood Planks" = 23, "Cardboard" = 24, "Metal/Zinc" = 31, "Wood" = 32, 
               "Calamine/Cement Fiber" = 33, "Ceramic Tiles" = 34, "Cement" = 35, 
               "Roofing Shingles" = 36, "Asbestos" = 37, "Other" = 96, "Missing" = 99),
  
  wall_mat = c("No Walls" = 11, "Cane/Palm/Trunks" = 12, "Dirt" = 13, "Bamboo with Mud" = 21, 
               "Stone with Mud" = 22, "Uncovered Adobe" = 23, "Plywood" = 24, "Cardboard" = 25, "Reused Wood" = 26, "Cement" = 31, 
               "Stone with Lime" = 32, "Bricks" = 33, "Cement Blocks" = 34, "Covered Adobe" = 35, 
               "Wood Planks/Shingles" = 36, "Other" = 96, "Missing" = 99),
  
  relation = c("Head" = 1, "Wife/Husband" = 2, "Son/Daughter" = 3, "Son-in-law/Daughter-in-law" = 4,
               "Grandchild" = 5, "Parent" = 6, "Parent-in-law" = 7, "Brother/Sister" = 8,
               "Other Relative" = 9, "Adopted/Stepchild" = 10, "Not Related" = 11, "Co-wife" = 12,
               "Don't Know" = 98, "Missing" = 99),
  
  religion = c("Catholic" = 1, "Christian" = 2, "Islam" = 3, "Traditional" = 4, "Other" = 6, "Missing" = 9),
  
  media_frequency = c("Daily/Often" = 1, "Weekly/Rarely" = 2, "Never" = 3, "Missing" = 9),
  
  # -- Derived Variables -- #
  
  # --- Converted from val_lbl_* to list style ---
  zone = c(
    "North Central" = 1, "North East" = 2, "North West" = 3,
    "South East" = 4, "South South" = 5, "South West" = 6,
    "Missing" = 9
  ),
  
  derived_residence = c("Urban" = 1, "Rural" = 2, "Missing" = 9),
  
  wealth_quintile = c(
    "Lowest" = 1, "Second" = 2, "Middle" = 3, "Fourth" = 4, "Highest" = 5,
    "Missing" = 9
  ),
  
  edu4 = c(
    "No Education" = 1, "Primary" = 2, "Secondary" = 3,
    "More than secondary" = 4, "Missing" = 9
  ),
  
  age_group_women = c(
    "15-19" = 1, "20-24" = 2, "25-29" = 3, "30-34" = 4,
    "35-39" = 5, "40-44" = 6, "45-49" = 7,
    "Missing" = 9
  ),
  
  # --- Additional value labels for newly added derived variables ---
  
  age_group_4 = c(
    "15-24" = 1,
    "25-34" = 2,
    "35-44" = 3,
    "45+"   = 4,
    "Missing" = 9
  ),
  
  trimester = c(
    "No" = 0,
    "Yes" = 1,
    "Missing" = 9
  ),
  
  topcode_flag = c(
    "No" = 0,
    "Yes" = 1
  ),
  
  yesno_simple = c("No" = 0, "Yes" = 1, "Missing" = 9)
)

# 3. Save as a Unified List
sproxil_metadata <- list(
  column_mapping = column_mapping,
  mis_data_dictionary = mis_data_dictionary,
  variable_labels = variable_labels,
  value_labels = value_labels
)

saveRDS(sproxil_metadata, file = "Sproxil_dictionary.rds")
print("✅ Dictionary and Mapping saved successfully.")
