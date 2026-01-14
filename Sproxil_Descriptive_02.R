# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  06_Analysis_and_Tabulation.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    December 2025
# DESCRIPTION: 
# Generates descriptive tables (frequencies/means) disaggregated by Wealth Quintile.
# Exports results to a multi-sheet Excel workbook matching the NMIS structure.
# ==============================================================================

# --- 1. SETUP ---
library(tidyverse)
library(openxlsx)
library(haven)
library(labelled)

# Input/Output
INPUT_FILE  <- "Sproxil_Derived.rds"
OUTPUT_XLSX <- "Sproxil_Final_Tables.xlsx"

if(!file.exists(INPUT_FILE)) stop("Derived dataset not found. Run Script 05 first.")
df <- readRDS(INPUT_FILE)

# Ensure Wealth Quintile is a Factor for column headers
df <- df %>% 
  mutate(wealth_quintile = factor(wealth_quintile, 
                                  levels = 1:5, 
                                  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# ==============================================================================
# 2. HELPER FUNCTIONS (The Analysis Engine)
# ==============================================================================

# Function A: Crosstab (Categorical Row vs Wealth Column)
# Used for: Water Source, Toilet Type, Education Level, etc.
calc_crosstab <- function(data, var_name, filter_cond = NULL) {
  
  # Apply filter if provided (e.g., only women who gave birth)
  if(!is.null(filter_cond)) { data <- data %>% filter(!!rlang::parse_expr(filter_cond)) }
  
  # 1. By Quintile
  tab_quintile <- data %>%
    count(var = .data[[var_name]], wealth_quintile) %>%
    group_by(wealth_quintile) %>%
    mutate(pct = n / sum(n) * 100) %>%
    select(-n) %>%
    pivot_wider(names_from = wealth_quintile, values_from = pct, values_fill = 0)
  
  # 2. Total Column
  tab_total <- data %>%
    count(var = .data[[var_name]]) %>%
    mutate(Total = n / sum(n) * 100) %>%
    select(var, Total)
  
  # 3. Merge
  final_tab <- left_join(tab_quintile, tab_total, by = "var")
  return(final_tab)
}

# Function B: Binary Stack (List of Yes/No Variables)
# Used for: Assets, Antimalarial Drugs Taken, Knowledge Methods
calc_binary_stack <- function(data, var_list, filter_cond = NULL) {
  
  if(!is.null(filter_cond)) { data <- data %>% filter(!!rlang::parse_expr(filter_cond)) }
  
  results <- list()
  
  for(v in var_list) {
    # Calculate % "1" (Yes) by Quintile
    q_res <- data %>%
      group_by(wealth_quintile) %>%
      summarise(pct = mean(.data[[v]] == 1, na.rm = TRUE) * 100) %>%
      pivot_wider(names_from = wealth_quintile, values_from = pct)
    
    # Calculate % "1" Total
    t_res <- data %>%
      summarise(Total = mean(.data[[v]] == 1, na.rm = TRUE) * 100)
    
    # Combine
    row <- bind_cols(Variable = v, q_res, t_res)
    results[[v]] <- row
  }
  
  return(bind_rows(results))
}

# Function C: Mean Calculation
# Used for: Avg household size, Avg number of nets
calc_mean_tab <- function(data, var_name) {
  q_res <- data %>%
    group_by(wealth_quintile) %>%
    summarise(Mean = mean(.data[[var_name]], na.rm = TRUE)) %>%
    pivot_wider(names_from = wealth_quintile, values_from = Mean)
  
  t_res <- data %>% summarise(Total = mean(.data[[var_name]], na.rm = TRUE))
  
  bind_cols(Variable = var_name, q_res, t_res)
}

# ==============================================================================
# 3. GENERATE TABLES
# ==============================================================================
message("Generating Tables...")

# --- CHAPTER 2: HOUSING & RESPONDENTS ---

# Table 2.1: Drinking Water
t2_1_source <- calc_crosstab(df, "derived_water_category")
t2_1_time   <- calc_crosstab(df, "derived_water_time_cat")
t2_1 <- bind_rows(mutate(t2_1_source, Section="Source"), mutate(t2_1_time, Section="Time"))

# Table 2.2: Water Service Ladder
t2_2 <- calc_crosstab(df, "derived_water_service_ladder")

# Table 2.3: Sanitation
t2_3 <- calc_crosstab(df, "derived_sanitation_category")

# Table 2.5: Housing Materials
t2_5_floor <- calc_crosstab(df, "hh_floor_material")
t2_5_wall  <- calc_crosstab(df, "hh_wall_material")
t2_5_roof  <- calc_crosstab(df, "hh_roof_material")
t2_5 <- bind_rows(t2_5_floor, t2_5_wall, t2_5_roof)

# Table 2.6: Electricity & Cooking
t2_6_elec <- calc_binary_stack(df, "hh_has_electricity")
t2_6_tech <- calc_crosstab(df, "derived_cook_tech_cat")
t2_6_fuel <- calc_crosstab(df, "derived_cook_fuel_cat")
t2_6 <- list(Electricity=t2_6_elec, Tech=t2_6_tech, Fuel=t2_6_fuel)

# Table 2.7: Assets (Binary Stack)
assets_list <- c("hh_has_radio", "hh_has_tv", "hh_own_mobile_phone", "hh_has_refrigerator", 
                 "hh_own_bicycle", "hh_own_motorcycle", "hh_own_car_truck", "hh_owns_agri_land", "hh_owns_livestock")
t2_7 <- calc_binary_stack(df, assets_list)

# Table 2.10: Household Size
t2_10_dist <- calc_crosstab(df, "derived_hh_size_cat")
t2_10_mean <- calc_mean_tab(df, "hh_total_persons_v1")
t2_10 <- bind_rows(t2_10_dist, t2_10_mean)

# Table 2.11: Respondent Background
t2_11_edu <- calc_crosstab(df, "demo_edu_level")
t2_11_rel <- calc_crosstab(df, "bg_religion")
t2_11 <- bind_rows(t2_11_edu, t2_11_rel)

# Table 2.14.1: Literacy & Media
t2_14_lit <- calc_binary_stack(df, "derived_literate")
t2_14_med <- calc_binary_stack(df, "derived_media_tv_weekly") # Add other media vars if available
t2_14 <- bind_rows(t2_14_lit, t2_14_med)

# Table 2.16: Mobile & Internet
t2_16_mob <- calc_binary_stack(df, c("bg_own_smartphone", "derived_internet_use_12m"))
t2_16_frq <- calc_crosstab(df, "derived_internet_freq_cat", filter_cond = "derived_internet_use_12m == 1")
t2_16 <- list(Ownership=t2_16_mob, Frequency=t2_16_frq)


# --- CHAPTER 3: MALARIA PREVENTION ---

# Table 3.1.1: Ownership
t3_1_any  <- calc_binary_stack(df, "prev_has_mosquito_nets")
t3_1_itn  <- calc_binary_stack(df, "derived_hh_has_itn")
t3_1_avg  <- calc_mean_tab(df, "prev_num_mosquito_nets")
t3_1 <- bind_rows(t3_1_any, t3_1_itn, t3_1_avg)

# Table 3.2.1: Source of Nets (Filter: Households with nets)
t3_2 <- calc_crosstab(df, "derived_net_source_composite", filter_cond = "prev_has_mosquito_nets == 1")

# Table 3.3.1: Access (Any Net & ITN)
t3_3 <- calc_binary_stack(df, c("derived_access_any_net", "derived_access_itn"))

# Table 3.4.1: Usage (Population Proxy - Mean Proportion)
t3_4 <- calc_mean_tab(df, "derived_itn_usage_prop")

# Table 3.9: ANC Provider & Skilled
t3_9_prov <- calc_crosstab(df, "derived_anc_provider_composite", filter_cond = "women_ever_given_birth == 1")
t3_9_skil <- calc_binary_stack(df, "derived_anc_skilled", filter_cond = "women_ever_given_birth == 1")
t3_9 <- bind_rows(t3_9_prov, t3_9_skil)

# Table 3.10: ANC Timing & Visits
t3_10_num <- calc_crosstab(df, "derived_anc_visits_cat", filter_cond = "women_ever_given_birth == 1")
t3_10_tim <- calc_crosstab(df, "derived_anc_timing_cat", filter_cond = "women_ever_given_birth == 1")
t3_10_med <- calc_mean_tab(df %>% filter(women_anc_seen == 1), "derived_anc_month_clean")
t3_10 <- bind_rows(t3_10_num, t3_10_tim, t3_10_med)

# Table 3.11: IPTp
t3_11 <- calc_binary_stack(df, c("derived_iptp_1plus", "derived_iptp_2plus", "derived_iptp_3plus"), filter_cond = "women_ever_given_birth == 1")


# --- CHAPTER 4: FEVER & TREATMENT ---

# Table 4.1: Fever Prevalence & Care Seeking
t4_1_fev <- calc_binary_stack(df, "derived_child_has_fever", filter_cond = "demo_hh_children_under5 > 0")
t4_1_sek <- calc_binary_stack(df, "derived_fever_seek_advice", filter_cond = "derived_child_has_fever == 1")
t4_1_prm <- calc_binary_stack(df, "derived_fever_prompt_care", filter_cond = "derived_child_has_fever == 1")
t4_1_tst <- calc_binary_stack(df, "derived_fever_tested", filter_cond = "derived_child_has_fever == 1")
t4_1 <- bind_rows(t4_1_fev, t4_1_sek, t4_1_prm, t4_1_tst)

# Table 4.3: Source of Advice (Sector)
t4_3 <- calc_crosstab(df, "derived_fever_sector", filter_cond = "derived_fever_seek_advice == 1")

# Table 4.4: Antimalarials Taken
drugs_list <- c("derived_med_act", "derived_med_sp", "derived_med_chloro", "derived_med_amod", "derived_med_art_inj", "derived_med_other_anti")
t4_4 <- calc_binary_stack(df, drugs_list, filter_cond = "derived_took_any_antimalarial == 1")

# Table 4.5: ACT Effectiveness
t4_5 <- calc_binary_stack(df, "derived_act_fever_resolved", filter_cond = "derived_med_act == 1")


# --- CHAPTER 5: KNOWLEDGE & BELIEFS ---

# Table 5.1: Media Exposure
t5_1_any <- calc_binary_stack(df, "derived_msg_heard_6m")
sources_list <- c("derived_src_radio", "derived_src_tv", "derived_src_poster", "derived_src_hcp", "derived_src_chw", "derived_src_social")
t5_1_src <- calc_binary_stack(df, sources_list, filter_cond = "derived_msg_heard_6m == 1")
t5_1 <- bind_rows(t5_1_any, t5_1_src)

# Table 5.2: Avoidance Knowledge
t5_2_any <- calc_binary_stack(df, "derived_know_any_way")
methods_list <- c("derived_know_nets", "derived_know_repellent", "derived_know_meds", "derived_know_spray", "derived_know_clean")
t5_2_meth <- calc_binary_stack(df, methods_list, filter_cond = "derived_know_any_way == 1")
t5_2 <- bind_rows(t5_2_any, t5_2_meth)

# Table 5.3 & 5.4: Attitudes
attitudes_list <- c("derived_att_susceptibility_comp", "derived_att_severity_comp", "derived_att_efficacy_comp",
                    "derived_att_favorable_behavior_comp", "derived_norm_comp")
t5_3 <- calc_binary_stack(df, attitudes_list)


# --- CHAPTER 6: ADVANTAGE TABLE (Cost & Affordability) ---

message("Generating Advantage Table...")
advantage_list <- c(
  "derived_paid_transport",
  "derived_paid_test",
  "derived_paid_drugs",
  "derived_drug_cost_high",
  "derived_perceived_expensive",
  "derived_exp_stockout",
  "derived_exp_free_tx"
)

# Apply to all respondents (or filter to those who treated fever if desired)
t6_2 <- calc_binary_stack(df, advantage_list, filter_cond = "treat_hh_fever_last_2weeks == 1") 


# ==============================================================================
# 4. EXPORT TO EXCEL
# ==============================================================================
message("Exporting to Excel...")

wb <- createWorkbook()

addWorksheet(wb, "2.1_Water"); writeData(wb, "2.1_Water", t2_1)
addWorksheet(wb, "2.2_Water_Ladder"); writeData(wb, "2.2_Water_Ladder", t2_2)
addWorksheet(wb, "2.3_Sanitation"); writeData(wb, "2.3_Sanitation", t2_3)
addWorksheet(wb, "2.5_Housing"); writeData(wb, "2.5_Housing", t2_5)
addWorksheet(wb, "2.7_Assets"); writeData(wb, "2.7_Assets", t2_7)
addWorksheet(wb, "2.10_HH_Size"); writeData(wb, "2.10_HH_Size", t2_10)
addWorksheet(wb, "2.11_Background"); writeData(wb, "2.11_Background", t2_11)
addWorksheet(wb, "2.14_Media_Lit"); writeData(wb, "2.14_Media_Lit", t2_14)
addWorksheet(wb, "2.16_Internet"); writeData(wb, "2.16_Internet", bind_rows(t2_16))

addWorksheet(wb, "3.1_Net_Own"); writeData(wb, "3.1_Net_Own", t3_1)
addWorksheet(wb, "3.2_Net_Source"); writeData(wb, "3.2_Net_Source", t3_2)
addWorksheet(wb, "3.3_Net_Access"); writeData(wb, "3.3_Net_Access", t3_3)
addWorksheet(wb, "3.4_Net_Use_Prop"); writeData(wb, "3.4_Net_Use_Prop", t3_4)
addWorksheet(wb, "3.9_ANC_Prov"); writeData(wb, "3.9_ANC_Prov", t3_9)
addWorksheet(wb, "3.10_ANC_Visits"); writeData(wb, "3.10_ANC_Visits", t3_10)
addWorksheet(wb, "3.11_IPTp"); writeData(wb, "3.11_IPTp", t3_11)

addWorksheet(wb, "4.1_Fever_Care"); writeData(wb, "4.1_Fever_Care", t4_1)
addWorksheet(wb, "4.3_Care_Source"); writeData(wb, "4.3_Care_Source", t4_3)
addWorksheet(wb, "4.4_Drugs"); writeData(wb, "4.4_Drugs", t4_4)
addWorksheet(wb, "4.5_ACT_Effect"); writeData(wb, "4.5_ACT_Effect", t4_5)

addWorksheet(wb, "5.1_Media_Msg"); writeData(wb, "5.1_Media_Msg", t5_1)
addWorksheet(wb, "5.2_Knowledge"); writeData(wb, "5.2_Knowledge", t5_2)
addWorksheet(wb, "5.3_Attitudes"); writeData(wb, "5.3_Attitudes", t5_3)

addWorksheet(wb, "6.2_Advantage_Cost"); writeData(wb, "6.2_Advantage_Cost", t6_2)

saveWorkbook(wb, OUTPUT_XLSX, overwrite = TRUE)

cat("SUCCESS: All tables exported to", OUTPUT_XLSX, "\n")