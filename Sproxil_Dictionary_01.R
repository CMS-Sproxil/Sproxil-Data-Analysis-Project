#===============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Data_Dictionary
# AUTHOR:  Ikechukwu Onuko
# DATE:    23 November 2025
#
# DESCRIPTION:
# This script loads 
#
# OUTPUT: "Sproxil_Data_Dictionary.rds"
#===============================================================================
library(tidyverse)
library(writexl)

# ==============================================================================
# SPROXIL MIS: DATA DICTIONARY GENERATION SCRIPT
# ==============================================================================
# This script generates the reference file "Sproxil_Data_Dictionary.xlsx".
# Variable names have been updated to match the "standard_names_vector".

# --- 1. Define Common Value Label Strings ---
val_yes_no      <- "1=Yes; 0=No; 8=Don't Know; 9=Refused/Missing"
val_yes_no_cond <- "1=Yes; 0=No; 6=Not Applicable; 8=Don't Know; 9=Missing"
val_gender      <- "1=Male; 2=Female; 9=Refused"
val_edu         <- "0=None; 1=Primary; 2=Secondary; 3=Higher; 9=Missing"
# Note: State list kept for reference, though "State" column was missing in your last headers
val_states      <- "1=Abia; ... 37=Zamfara; 99=Missing" 
val_brands      <- "11=Permanet; 12=Olyset; 13=IconLife; 14=Duranet; 15=Netprotect; 16=BASF Interceptor; 17=Yorkool; 18=Magnet; 19=DawaPlus 2.0; 20=Royal Security; 21=Royal Sentry; 22=Permanet 2.0; 23=Permanet 3.0; 24=Veeralin; 25=Interceptor G2; 26=Royal Guard; 36=Other LLIN; 96=Other Non-LLIN; 98=Don't Know; 99=Missing"
val_loc_water   <- "1=In Own Dwelling; 2=In Own Yard/Plot; 3=Elsewhere; 9=Missing"
val_cost        <- "0=Free; 1=< N1,000; 2=N1,000-N1,999; 3=N2,000-N2,999; 4=N3,000-N3,999; 5=N4,000-N4,999; 6=N5,000-N5,999; 7=N6,000-N6,999; 8=N7,000-N7,999; 9=N8,000-N8,999; 10=N9,000-N9,999; 11=Above N10,000; 98=Don't Know; 99=Missing"
val_source      <- "1=Government; 2=Private; 3=NGO; 4=Community; 5=Pharmacy/Chemist; 6=Mobile Seller; 7=Traditional; 8=Religious; 9=Shop/Market; 96=Other; 98=Don't Know"
val_agree       <- "1=Agree; 2=Disagree; 8=Don't Know; 9=Missing"
val_freq_net    <- "1=Agree (Every Night); 2=Disagree (Not Every Night); 8=Don't Know"
val_rating      <- "1=Excellent; 2=Good; 3=Average; 4=Poor; 5=Very Poor; 8=Don't Know"

# --- 2. Define the Standard Variables (Mapped to Actual Headers) ---
dictionary_main <- tribble(
  ~variable, ~label, ~value_labels,
  
  # --- Meta Data ---
  "meta_respondent_id", "Unique Respondent Identifier", NA,
  "meta_status", "Survey Status", NA,
  
  # --- Section 1: Demographics ---
  "demo_gender", "What is your gender?", val_gender,
  "demo_edu_level", "Highest level of school attended", val_edu,
  "demo_edu_informal", "Type of informal education attended", "1=Quranic; 2=Adult Lit; 3=Vocational; 4=None; 96=Other",
  "demo_hh_children_under5", "Number of children under 5 in household", NA,
  "demo_hh_sleeping_rooms", "Number of rooms used for sleeping", NA,
  
  # --- Section 2: Malaria Prevention ---
  "prev_has_mosquito_nets", "Does household have any mosquito nets?", val_yes_no,
  "prev_num_mosquito_nets", "How many mosquito nets does household have?", "7=7 or more; 99=Missing",
  "prev_months_since_net_obtained", "How many months ago did you get the net?", "0=Less than 1 month; 95=More than 36 months; 98=Not Sure",
  "prev_net_brand", "Brand of mosquito net used", val_brands,
  "prev_net_obtained_how", "How did you get the mosquito net?", "1=Mass Dist; 2=ANC; 3=Immunization; 4=No; 5=Market; 6=Pharmacy; 96=Other",
  "prev_net_obtained_where", "Where did you get the net?", "1=Gov Facility; 2=Pvt Facility; 3=Pharmacy; 4=Shop/Market; 5=CHW; 6=Religious; 7=School; 96=Other; 98=Don't Know",
  
  # Note: "Slept in net" and "Reason not used" were MISSING in the actual headers provided, 
  # but "Number of people slept" WAS in the headers.
  "prev_num_people_slept_net", "How many people slept inside this mosquito net last night?", NA,
  
  "prev_home_sprayed_interior", "Has home been sprayed (IRS) in last 12 months?", val_yes_no,
  "prev_repellent_methods", "Mosquito repellent methods used", "Multi-Select (See Dummies)",
  "prev_first_treatment_location", "Where do you go first for malaria treatment?", val_source,
  "prev_time_to_treatment_facility", "Time to reach nearest health facility (Minutes)", NA,
  
  # --- Section 3: Malaria Treatment ---
  "treat_transport_cost", "Cost of transport to facility", val_cost,
  "treat_hh_fever_last_2weeks", "Has anyone had fever in last 2 weeks?", val_yes_no,
  "treat_blood_sample_taken", "Was blood sample taken for testing?", val_yes_no,
  "treat_test_cost", "Cost of malaria test", val_cost,
  "treat_drug_cost", "Cost of malaria drugs", val_cost,
  "treat_drug_purchase_time", "When did you buy last malaria drug?", "1=<1 month; 2=1-5 months; 3=6-11 months; 4=>1 year; 98=Don't Know",
  "treat_drug_affordability", "Rate affordability of malaria drugs", "1=Very Affordable; 2=Affordable; 3=Expensive; 4=Very Expensive; 8=Don't Know",
  
  "treat_heard_smc", "Heard about Seasonal Malaria Chemoprevention (SMC)?", val_yes_no,
  "treat_children_received_smc", "Have your children received SMC?", val_yes_no_cond,
  "treat_know_smc_drug", "Do you know the SMC drug name?", "1=SP+AQ; 2=SP; 3=AL; 4=ASAQ; 96=Other; 98=Don't Know",
  "treat_vaccine_age_knowledge", "Age child should receive first malaria vaccine", "1=<12 mths; 2=1 yr; 3=2 yrs; 4=3 yrs; 5=4 yrs; 6=5 yrs; 7=>5 yrs; 98=Don't Know",
  "treat_children_received_vaccine", "Have your children received malaria vaccine?", val_yes_no_cond,
  
  # --- Section 4: Feedback ---
  "feedback_free_treatment_6months", "Received free malaria treatment in last 6 months?", val_yes_no_cond,
  "feedback_drug_stockout_6months", "Experienced no drugs at gov facility in last 6 months?", val_yes_no_cond,
  "feedback_gov_effort_rating", "Rate gov efforts in combating malaria", val_rating,
  
  # --- Section 5: Women Questionnaire ---
  "women_ever_given_birth", "Have you ever given birth?", val_yes_no,
  "women_births_2020_2025", "Children born between 2020 and 2025", NA,
  "women_anc_seen", "Did you see anyone for ANC?", val_yes_no,
  "women_anc_provider", "Whom did you see for ANC?", "Multi-Select",
  "women_anc_location", "Where did you receive ANC?", "Multi-Select",
  "women_anc_first_visit_month", "Pregnancy duration at first ANC visit (Months)", NA, 
  "women_anc_total_visits", "Number of ANC visits", NA,
  "women_took_sp_fansidar", "Did you take SP/Fansidar during pregnancy?", val_yes_no,
  "women_sp_fansidar_doses", "Number of SP/Fansidar doses taken", NA,
  "women_sp_fansidar_source", "How did you get the SP/Fansidar?", "Multi-Select",
  
  "women_child_fever_2weeks", "Has youngest child had fever in last 2 weeks?", val_yes_no,
  "women_child_blood_sample", "Was blood sample taken from child?", val_yes_no,
  "women_child_malaria_diagnosis", "Were you told child had malaria?", val_yes_no,
  "women_child_seek_advice", "Did you seek advice/treatment?", val_yes_no,
  "women_child_advice_location", "Where did you seek treatment?", "Multi-Select",
  "women_child_first_advice_location", "Where did you go *first*?", val_source,
  "women_child_advice_delay_days", "Days after illness began before seeking care", NA,
  "women_child_referral", "Were you referred to higher level care?", val_yes_no,
  "women_child_took_medicine", "Did child take any medicine?", val_yes_no,
  "women_child_medicine_type", "What medicine did child take?", "Multi-Select",
  "women_child_act_delay", "How long after fever did child take ACT?", "0=Same Day; 1=Next Day; 2=2 Days; 3=3+ Days; 8=Don't Know",
  "women_child_act_effective", "Did fever go away after ACT?", val_yes_no,
  "women_currently_pregnant", "Are you pregnant now?", val_yes_no,
  "women_pregnancy_duration_months", "Duration of current pregnancy (Months)", NA,
  
  # --- Section 6: Background A ---
  "bg_tv_frequency", "How often do you watch TV?", "1=At least once/week; 2=< Once/week; 3=Not at all",
  "bg_own_smartphone", "Do you own a smartphone?", val_yes_no,
  "bg_internet_ever_used", "Have you used the Internet?", val_yes_no,
  "bg_internet_frequency", "Frequency of internet use", "1=Every Day; 2=At least once/week; 3=< Once/week; 4=Not at all",
  "bg_religion", "Religion", "1=Catholic; 2=Other Christian; 3=Islam; 4=Traditional; 96=Other",
  "bg_heard_malaria_msg_6months", "Heard malaria messages in last 6 months?", val_yes_no,
  "bg_malaria_msg_source", "Source of malaria messages", "Multi-Select",
  "bg_aware_avoidance", "Are you aware of ways to avoid malaria?", val_yes_no,
  "bg_prevention_knowledge", "Ways to prevent malaria", "Multi-Select",
  
  # --- Section 7: Background B (Attitudes) ---
  "att_rainy_season_only", "Malaria only happens in rainy season?", val_agree,
  "att_fever_worry_malaria", "When child has fever, worry it is malaria?", val_agree,
  "att_malaria_easily_treated", "Malaria not a problem because easily treated?", val_agree,
  "att_weak_children_die", "Only weak children die from malaria?", val_agree,
  "att_net_use_mosquito_density", "Need to sleep in net only when mosquitoes are many?", val_agree,
  "att_net_use_warm_weather", "Don't like net when weather is warm?", val_agree,
  "att_home_meds_first", "Best to start with home medicine for fever?", val_agree,
  "att_full_dose_importance", "Important to take full dose of malaria meds?", val_agree,
  "att_seek_care_immediate", "Community seeks care same/next day?", val_agree, 
  "att_community_net_usage", "Community sleeps in nets every night?", val_freq_net,
  
  # --- Section 8: Household ---
  "hh_total_persons_v1", "Total persons in household (V1)", NA,
  "hh_total_persons_v2", "Total persons in household (V2)", NA,
  "hh_members_under5", "Number of household members under 5 years", NA,
  "hh_total_persons_usually_v3", "Total persons usually in household (V3)", NA,
  "hh_relation_to_head", "Relationship to head of household", "1=Head; 2=Spouse; 3=Child; 4=In-law; 5=Grandchild; 6=Parent; 8=Sibling; 9=Other Relative; 10=Foster; 11=Not Related; 12=Co-Wife",
  
  "hh_drinking_water_source", "Main source of drinking water", val_loc_water,
  "hh_other_water_source", "Main source of water for other use", val_loc_water,
  "hh_water_location", "Location of water source", val_loc_water,
  "hh_water_time_trip", "Time to get water (minutes)", "998=Don't Know",
  "hh_toilet_type", "Toilet facility type", "11=Flush Sewer; 12=Flush Septic; 13=Flush Pit; 21=VIP; 22=Pit Slab; 23=Pit Open; 31=Compost; 41=Bucket; 51=Hanging; 61=Bush; 96=Other",
  "hh_toilet_shared", "Is toilet shared?", val_yes_no,
  "hh_toilet_share_count", "Number of households sharing toilet", NA,
  "hh_toilet_location", "Location of toilet", val_loc_water,
  "hh_cookstove_type", "Main cookstove type", "1=Electric; 2=Solar; 3=LPG; 4=Nat Gas; 5=Biogas; 6=Kerosene; 7=Solid Fuel; 9=Open Fire; 95=No Food Cooked; 96=Other",
  "hh_cookstove_fuel", "Fuel for cookstove", "1=Alcohol; 2=Gasoline; 3=Kerosene; 4=Coal; 5=Charcoal; 6=Wood; 9=Dung; 96=Other",
  
  "hh_owns_livestock", "Owns livestock?", val_yes_no,
  "hh_num_cows_bulls", "Number of cows/bulls owned", NA,
  "hh_num_other_cattle", "Number of other cattle owned", NA,
  "hh_num_horses_donkeys", "Number of horses/donkeys owned", NA,
  "hh_num_goats", "Number of goats owned", NA,
  "hh_num_sheep", "Number of sheep owned", NA,
  "hh_num_poultry", "Number of poultry owned", NA,
  "hh_num_pigs", "Number of pigs owned", NA,
  "hh_num_camels", "Number of camels owned", NA,
  "hh_owns_agri_land", "Owns agricultural land?", val_yes_no,
  "hh_num_agri_plots", "Number of agricultural plots owned", NA,
  
  # Assets
  "hh_has_electricity", "Has electricity?", val_yes_no,
  "hh_has_radio", "Has radio?", val_yes_no,
  "hh_has_tv", "Has TV?", val_yes_no,
  "hh_has_non_mobile_phone", "Has non-mobile phone?", val_yes_no,
  "hh_has_computer", "Has computer?", val_yes_no,
  "hh_has_refrigerator", "Has refrigerator?", val_yes_no,
  "hh_has_table", "Has table?", val_yes_no,
  "hh_has_chair", "Has chair?", val_yes_no,
  "hh_has_bed", "Has bed?", val_yes_no,
  "hh_has_sofa", "Has sofa?", val_yes_no,
  "hh_has_cupboard", "Has cupboard?", val_yes_no,
  "hh_has_ac", "Has AC?", val_yes_no,
  "hh_has_electric_iron", "Has electric iron?", val_yes_no,
  "hh_has_generator", "Has generator?", val_yes_no,
  "hh_has_fan", "Has fan?", val_yes_no,
  "hh_own_watch", "Owns watch?", val_yes_no,
  "hh_own_mobile_phone", "Owns mobile phone?", val_yes_no,
  "hh_own_bicycle", "Owns bicycle?", val_yes_no,
  "hh_own_motorcycle", "Owns motorcycle?", val_yes_no,
  "hh_own_animal_cart", "Owns animal cart?", val_yes_no,
  "hh_own_car_truck", "Owns car/truck?", val_yes_no,
  "hh_own_motor_boat", "Owns motor boat?", val_yes_no,
  "hh_own_canoe", "Owns canoe?", val_yes_no,
  "hh_own_keke_napep", "Owns Keke Napep?", val_yes_no,
  "hh_has_bank_account", "Has bank account?", val_yes_no,
  "hh_mobile_money_usage", "Uses mobile money?", val_yes_no,
  
  "hh_floor_material", "Main floor material", "11=Earth/Sand; 21=Wood; 34=Cement; 35=Carpet; 96=Other",
  "hh_roof_material", "Main roof material", "11=No Roof; 12=Thatch; 31=Zinc; 35=Cement; 96=Other",
  "hh_wall_material", "Main wall material", "11=No Walls; 13=Dirt; 31=Cement; 34=Cement Blocks; 96=Other"
)

# --- 3. Export ---
# This creates the Excel file that acts as the "Codebook" for the analysts.
write_xlsx(dictionary_main, "Sproxil_Data_Dictionary_Clean.xlsx")

message("Dictionary successfully created with STANDARD NAMES: Sproxil_Data_Dictionary_Clean.xlsx")