# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Missingness decision support
# AUTHOR:  Corona Management Systems
# DATE:    23 January 2026
#
# PURPOSE:
# Assesses true respondent non-response using the cleaned dataset.
#
# This script:
# 1. loads the cleaned final dataset,
# 2. identifies universal and conditional variables,
# 3. defines module groupings for interpretation,
# 4. calculates variable-level non-response summaries,
# 5. aggregates module-level non-response patterns,
# 6. generates decision flags for reporting,
# 7. summarises record-level non-response distributions, and
# 8. exports decision-support tables and figures.
#
# INPUT:
#   - Sproxil_Cleaned_Final.rds
#
# OUTPUT:
#   - missingness_variable_summary.csv
#   - missingness_module_summary.csv
#   - missingness_universal_summary.csv
#   - missingness_conditional_summary.csv
#   - missingness_decision_flags.csv
#   - nonresponse_distribution.csv
#   - figure_nonresponse_by_module.png
#   - figure_nonresponse_distribution.png
#
# REPLICATION NOTE:
# This file uses the cleaned dataset, where "NO RESPONSE" represents true
# respondent non-response and NA represents structural blanks or values that are
# not applicable after logic resolution. Main non-response percentages are based
# on applicable denominators, while structural blanks are retained for internal
# interpretation but are not the main report-facing metric.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(tibble)
  library(stringr)
  library(purrr)
})

# ------------------------------------------------------------------------------
# 1. Load cleaned dataset
# ------------------------------------------------------------------------------
s08_df_cleaned <- readRDS("Sproxil_Cleaned_Final.rds")

# ------------------------------------------------------------------------------
# 2. Define universal variables
# ------------------------------------------------------------------------------
# These variables are expected across all respondents.
s08_universal_vars <- c(
  "meta_respondent_id", "demo_state", "demo_lga", "demo_town",
  "demo_year_of_birth", "demo_age", "demo_gender",
  "demo_edu_level",
  "demo_hh_children_under5", "demo_hh_sleeping_rooms",
  "hh_total_persons_v1", "hh_relation_to_head",
  "hh_drinking_water_source", "hh_other_water_source", "hh_water_location",
  "hh_toilet_type", "hh_cookstove_type",
  "hh_owns_livestock", "hh_owns_agri_land",
  "hh_floor_material", "hh_roof_material", "hh_wall_material",
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac",
  "hh_has_electric_iron", "hh_has_generator", "hh_has_fan", "hh_own_watch",
  "hh_own_mobile_phone", "hh_own_bicycle", "hh_own_motorcycle",
  "hh_own_animal_cart", "hh_own_car_truck", "hh_own_motor_boat",
  "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account", "hh_mobile_money_usage",
  "bg_languages", "bg_tv_frequency", "bg_own_smartphone", "bg_internet_ever_used",
  "bg_religion", "bg_ethnic_group", "bg_heard_malaria_msg_6months", "bg_aware_avoidance",
  "att_rainy_season_only", "att_fever_worry_malaria", "att_malaria_easily_treated",
  "att_weak_children_die", "att_net_use_mosquito_density", "att_net_use_warm_weather",
  "att_home_meds_first", "att_full_dose_importance", "att_seek_care_immediate",
  "att_community_net_usage",
  "prev_home_sprayed_interior",
  "prev_repellent_methods",
  "prev_first_treatment_location",
  "prev_time_to_treatment_facility",
  "treat_transport_cost",
  "treat_drug_affordability",
  "prev_has_mosquito_nets",
  "treat_hh_fever_last_2weeks",
  "treat_heard_smc",
  "treat_vaccine_age_knowledge",
  "feedback_free_treatment_6months",
  "feedback_drug_stockout_6months",
  "feedback_gov_effort_rating"
)

s08_universal_vars <- s08_universal_vars[s08_universal_vars %in% names(s08_df_cleaned)]

# ------------------------------------------------------------------------------
# 3. Define conditional variables and parent rules
# ------------------------------------------------------------------------------
s08_edge_rules <- tibble::tribble(
  ~parent, ~child, ~trigger_type, ~trigger_value,
  "demo_edu_level", "demo_edu_informal", "EQUALS", "I NEVER HAD ANY FORMAL EDUCATION",
  "prev_has_mosquito_nets", "prev_num_mosquito_nets", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_is_itn", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_months_since_net_obtained", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_net_brand", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_net_obtained_how", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_net_obtained_where", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_slept_under_net_last_night", "EQUALS", "YES",
  "prev_slept_under_net_last_night", "prev_num_people_slept_net", "EQUALS", "YES",
  "prev_slept_under_net_last_night", "prev_net_not_used_reason", "IN_SET", "NO|I AM NOT SURE",
  "treat_hh_fever_last_2weeks", "treat_blood_sample_taken", "EQUALS", "YES",
  "treat_hh_fever_last_2weeks", "treat_test_cost", "EQUALS", "YES",
  "treat_hh_fever_last_2weeks", "treat_drug_cost", "EQUALS", "YES",
  "treat_hh_fever_last_2weeks", "treat_drug_purchase_time", "EQUALS", "YES",
  "treat_heard_smc", "treat_children_received_smc", "EQUALS", "YES",
  "treat_children_received_smc", "treat_know_smc_drug", "STARTS_WITH", "YES",
  "treat_vaccine_age_knowledge", "treat_children_received_vaccine", "NOT_CONTAINS", "I DON'T KNOW",
  "demo_gender", "women_ever_given_birth", "EQUALS", "FEMALE",
  "demo_gender", "women_currently_pregnant", "EQUALS", "FEMALE",
  "women_ever_given_birth", "women_births_2020_2025", "EQUALS", "YES",
  "women_ever_given_birth", "women_anc_seen", "EQUALS", "YES",
  "women_anc_seen", "women_anc_provider", "EQUALS", "YES",
  "women_anc_seen", "women_anc_location", "EQUALS", "YES",
  "women_anc_seen", "women_anc_first_visit_month", "EQUALS", "YES",
  "women_anc_seen", "women_anc_total_visits", "EQUALS", "YES",
  "women_took_sp_fansidar", "women_sp_fansidar_doses", "EQUALS", "YES",
  "women_took_sp_fansidar", "women_sp_fansidar_source", "EQUALS", "YES",
  "women_currently_pregnant", "women_pregnancy_duration_months", "EQUALS", "YES",
  "women_child_fever_2weeks", "women_child_blood_sample", "EQUALS", "YES",
  "women_child_fever_2weeks", "women_child_malaria_diagnosis", "EQUALS", "YES",
  "women_child_fever_2weeks", "women_child_seek_advice", "EQUALS", "YES",
  "women_child_fever_2weeks", "women_child_took_medicine", "EQUALS", "YES",
  "women_child_seek_advice", "women_child_advice_location", "EQUALS", "YES",
  "women_child_seek_advice", "women_child_first_advice_location", "EQUALS", "YES",
  "women_child_seek_advice", "women_child_advice_delay_days", "EQUALS", "YES",
  "women_child_seek_advice", "women_child_referral", "EQUALS", "YES",
  "women_child_took_medicine", "women_child_medicine_type", "EQUALS", "YES",
  "women_child_medicine_type", "women_child_act_delay", "EQUALS", "ARTEMISININ COMBINATION THERAPY (ACT)",
  "women_child_medicine_type", "women_child_act_effective", "EQUALS", "ARTEMISININ COMBINATION THERAPY (ACT)",
  "bg_internet_ever_used", "bg_internet_frequency", "EQUALS", "YES",
  "bg_heard_malaria_msg_6months", "bg_malaria_msg_source", "EQUALS", "YES",
  "bg_aware_avoidance", "bg_prevention_knowledge", "EQUALS", "YES",
  "hh_toilet_type", "hh_toilet_shared", "NOT_EQUALS", "NO FACILITY/BUSH/FIELD",
  "hh_toilet_type", "hh_toilet_location", "NOT_EQUALS", "NO FACILITY/BUSH/FIELD",
  "hh_toilet_shared", "hh_toilet_share_count", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_cows_bulls", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_goats", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_other_cattle", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_sheep", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_poultry", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_horses_donkeys", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_pigs", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_camels", "EQUALS", "YES",
  "hh_owns_agri_land", "hh_num_agri_plots", "EQUALS", "YES"
) %>%
  filter(parent %in% names(s08_df_cleaned), child %in% names(s08_df_cleaned))

s08_conditional_vars <- unique(s08_edge_rules$child)

# ------------------------------------------------------------------------------
# 4. Define variable-to-module map
# ------------------------------------------------------------------------------
s08_module_map <- bind_rows(
  tibble(variable = c(
    "meta_respondent_id", "demo_state", "demo_lga", "demo_town", "demo_year_of_birth",
    "demo_age", "demo_gender", "demo_edu_level", "demo_edu_informal"
  ), module = "Core identifiers and stratifiers"),
  
  tibble(variable = c(
    "demo_hh_children_under5", "demo_hh_sleeping_rooms", "hh_total_persons_v1",
    "hh_total_persons_v2", "hh_total_persons_usually_v3", "hh_members_under5",
    "hh_relation_to_head", "hh_drinking_water_source", "hh_other_water_source",
    "hh_water_location", "hh_water_time_trip", "hh_toilet_type", "hh_toilet_shared",
    "hh_toilet_share_count", "hh_toilet_location", "hh_cookstove_type",
    "hh_cookstove_fuel", "hh_floor_material", "hh_roof_material", "hh_wall_material"
  ), module = "Household composition and living conditions"),
  
  tibble(variable = c(
    "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
    "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
    "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac", "hh_has_electric_iron",
    "hh_has_generator", "hh_has_fan", "hh_own_watch", "hh_own_mobile_phone",
    "hh_own_bicycle", "hh_own_motorcycle", "hh_own_animal_cart", "hh_own_car_truck",
    "hh_own_motor_boat", "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account",
    "hh_mobile_money_usage", "hh_owns_livestock", "hh_num_cows_bulls",
    "hh_num_other_cattle", "hh_num_horses_donkeys", "hh_num_goats", "hh_num_sheep",
    "hh_num_poultry", "hh_num_pigs", "hh_num_camels", "hh_owns_agri_land",
    "hh_num_agri_plots"
  ), module = "Household assets and livelihoods"),
  
  tibble(variable = c(
    "prev_has_mosquito_nets", "prev_num_mosquito_nets", "prev_is_itn",
    "prev_months_since_net_obtained", "prev_net_brand", "prev_net_obtained_how",
    "prev_net_obtained_where", "prev_slept_under_net_last_night",
    "prev_num_people_slept_net", "prev_net_not_used_reason",
    "prev_home_sprayed_interior", "prev_repellent_methods"
  ), module = "Malaria prevention and net use"),
  
  tibble(variable = c(
    "prev_first_treatment_location", "prev_time_to_treatment_facility",
    "treat_transport_cost", "treat_hh_fever_last_2weeks", "treat_blood_sample_taken",
    "treat_test_cost", "treat_drug_cost", "treat_drug_purchase_time",
    "treat_drug_affordability", "treat_heard_smc", "treat_children_received_smc",
    "treat_know_smc_drug", "treat_vaccine_age_knowledge",
    "treat_children_received_vaccine", "feedback_free_treatment_6months",
    "feedback_drug_stockout_6months", "feedback_gov_effort_rating"
  ), module = "Treatment, care seeking, and perceptions"),
  
  tibble(variable = c(
    "bg_languages", "bg_tv_frequency", "bg_own_smartphone", "bg_internet_ever_used",
    "bg_internet_frequency", "bg_religion", "bg_ethnic_group",
    "bg_heard_malaria_msg_6months", "bg_malaria_msg_source", "bg_aware_avoidance",
    "bg_prevention_knowledge", "att_rainy_season_only", "att_fever_worry_malaria",
    "att_malaria_easily_treated", "att_weak_children_die",
    "att_net_use_mosquito_density", "att_net_use_warm_weather",
    "att_home_meds_first", "att_full_dose_importance", "att_seek_care_immediate",
    "att_community_net_usage"
  ), module = "Background, knowledge, and attitudes"),
  
  tibble(variable = c(
    "women_ever_given_birth", "women_births_2020_2025", "women_anc_seen",
    "women_anc_provider", "women_anc_location", "women_anc_first_visit_month",
    "women_anc_total_visits", "women_took_sp_fansidar", "women_sp_fansidar_doses",
    "women_sp_fansidar_source", "women_currently_pregnant",
    "women_pregnancy_duration_months"
  ), module = "Women’s health, ANC, IPTp, and pregnancy"),
  
  tibble(variable = c(
    "women_child_fever_2weeks", "women_child_blood_sample",
    "women_child_malaria_diagnosis", "women_child_seek_advice",
    "women_child_advice_location", "women_child_first_advice_location",
    "women_child_advice_delay_days", "women_child_referral",
    "women_child_took_medicine", "women_child_medicine_type",
    "women_child_act_delay", "women_child_act_effective"
  ), module = "Child fever management")
) %>%
  filter(variable %in% names(s08_df_cleaned))

# ------------------------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------------------------
s08_is_no_response <- function(x) {
  x_chr <- as.character(x)
  !is.na(x_chr) & str_squish(str_to_upper(x_chr)) == "NO RESPONSE"
}

s08_is_observed_response <- function(x) {
  x_chr <- as.character(x)
  !is.na(x_chr) & str_squish(x_chr) != "" & str_squish(str_to_upper(x_chr)) != "NO RESPONSE"
}

s08_edge_triggered <- function(parent_vec, trigger_type, trigger_value) {
  parent_vec <- as.character(parent_vec)
  
  if (trigger_type == "EQUALS") {
    parent_vec == trigger_value
  } else if (trigger_type == "NOT_EQUALS") {
    parent_vec != trigger_value
  } else if (trigger_type == "STARTS_WITH") {
    str_detect(parent_vec, paste0("^", fixed(trigger_value)))
  } else if (trigger_type == "CONTAINS") {
    str_detect(parent_vec, fixed(trigger_value))
  } else if (trigger_type == "NOT_CONTAINS") {
    !str_detect(parent_vec, fixed(trigger_value))
  } else if (trigger_type == "IN_SET") {
    allowed <- str_split(trigger_value, "\\|", simplify = TRUE)
    parent_vec %in% allowed
  } else {
    rep(FALSE, length(parent_vec))
  }
}

# ------------------------------------------------------------------------------
# 6. Universal variable summary
# ------------------------------------------------------------------------------
s08_missingness_universal_summary <- tibble(variable = s08_universal_vars) %>%
  left_join(s08_module_map, by = c("variable")) %>%
  mutate(
    total_n = nrow(s08_df_cleaned),
    applicable_n = nrow(s08_df_cleaned),
    observed_n = map_int(variable, ~ sum(s08_is_observed_response(s08_df_cleaned[[.x]]), na.rm = TRUE)),
    nonresponse_n = map_int(variable, ~ sum(s08_is_no_response(s08_df_cleaned[[.x]]), na.rm = TRUE)),
    structural_blank_n = map_int(variable, ~ sum(is.na(s08_df_cleaned[[.x]]), na.rm = TRUE))
  ) %>%
  mutate(
    observed_pct = round(100 * observed_n / applicable_n, 1),
    nonresponse_pct = round(100 * nonresponse_n / applicable_n, 1),
    structural_blank_pct = round(100 * structural_blank_n / applicable_n, 1),
    variable_type = "Universal"
  )

# ------------------------------------------------------------------------------
# 7. Conditional variable summary
# ------------------------------------------------------------------------------
s08_missingness_conditional_summary <- s08_edge_rules %>%
  rowwise() %>%
  mutate(
    applicable_n = sum(s08_edge_triggered(s08_df_cleaned[[parent]], trigger_type, trigger_value), na.rm = TRUE),
    observed_n = {
      trig <- s08_edge_triggered(s08_df_cleaned[[parent]], trigger_type, trigger_value)
      sum(trig & s08_is_observed_response(s08_df_cleaned[[child]]), na.rm = TRUE)
    },
    nonresponse_n = {
      trig <- s08_edge_triggered(s08_df_cleaned[[parent]], trigger_type, trigger_value)
      sum(trig & s08_is_no_response(s08_df_cleaned[[child]]), na.rm = TRUE)
    },
    structural_blank_n = {
      trig <- s08_edge_triggered(s08_df_cleaned[[parent]], trigger_type, trigger_value)
      sum(trig & is.na(s08_df_cleaned[[child]]), na.rm = TRUE)
    },
    out_of_scope_n = {
      trig <- s08_edge_triggered(s08_df_cleaned[[parent]], trigger_type, trigger_value)
      sum(!trig, na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  left_join(s08_module_map, by = c("child" = "variable")) %>%
  mutate(
    total_n = nrow(s08_df_cleaned),
    observed_pct = round(100 * observed_n / applicable_n, 1),
    nonresponse_pct = round(100 * nonresponse_n / applicable_n, 1),
    structural_blank_pct = round(100 * structural_blank_n / applicable_n, 1),
    variable_type = "Conditional"
  ) %>%
  rename(variable = child)

# ------------------------------------------------------------------------------
# 8. Combined variable-level summary
# ------------------------------------------------------------------------------
s08_missingness_variable_summary <- bind_rows(
  s08_missingness_universal_summary %>%
    select(
      variable, module, variable_type, total_n, applicable_n,
      observed_n, nonresponse_n, structural_blank_n,
      observed_pct, nonresponse_pct, structural_blank_pct
    ),
  
  s08_missingness_conditional_summary %>%
    select(
      variable, module, variable_type, total_n, applicable_n,
      observed_n, nonresponse_n, structural_blank_n,
      observed_pct, nonresponse_pct, structural_blank_pct
    )
) %>%
  arrange(module, desc(nonresponse_pct), variable)

write_csv(s08_missingness_variable_summary, "missingness_variable_summary.csv")
write_csv(s08_missingness_universal_summary, "missingness_universal_summary.csv")
write_csv(s08_missingness_conditional_summary, "missingness_conditional_summary.csv")

# ------------------------------------------------------------------------------
# 9. Module-level summary
# ------------------------------------------------------------------------------
s08_missingness_module_summary <- s08_missingness_variable_summary %>%
  group_by(module, variable_type) %>%
  summarise(
    variables_in_module = n(),
    mean_nonresponse_pct = round(mean(nonresponse_pct, na.rm = TRUE), 1),
    median_nonresponse_pct = round(median(nonresponse_pct, na.rm = TRUE), 1),
    max_nonresponse_pct = round(max(nonresponse_pct, na.rm = TRUE), 1),
    mean_structural_blank_pct = round(mean(structural_blank_pct, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_nonresponse_pct))

write_csv(s08_missingness_module_summary, "missingness_module_summary.csv")

# ------------------------------------------------------------------------------
# 10. Decision flags for reporting
# ------------------------------------------------------------------------------
s08_missingness_decision_flags <- s08_missingness_variable_summary %>%
  mutate(
    report_flag = case_when(
      variable_type == "Universal" & nonresponse_pct >= 10 ~ "Discuss in main text",
      variable_type == "Conditional" & nonresponse_pct >= 10 ~ "Discuss in main text with applicable denominator",
      variable_type == "Universal" & nonresponse_pct >= 5 ~ "Include in annex and mention if relevant",
      variable_type == "Conditional" & nonresponse_pct >= 5 ~ "Include in annex; optional text mention",
      TRUE ~ "Annex only"
    )
  ) %>%
  arrange(desc(nonresponse_pct))

write_csv(s08_missingness_decision_flags, "missingness_decision_flags.csv")

# ------------------------------------------------------------------------------
# 11. Record-level distribution of respondent non-response
# ------------------------------------------------------------------------------
s08_row_nonresponse_distribution <- s08_df_cleaned %>%
  transmute(across(all_of(unique(s08_missingness_variable_summary$variable)), ~ s08_is_no_response(.x))) %>%
  mutate(
    n_nonresponse_in_row = rowSums(across(everything()))
  ) %>%
  count(n_nonresponse_in_row, name = "rows") %>%
  mutate(
    pct_rows = round(100 * rows / sum(rows), 1)
  ) %>%
  arrange(n_nonresponse_in_row)

write_csv(s08_row_nonresponse_distribution, "nonresponse_distribution.csv")

# ------------------------------------------------------------------------------
# 12. Export figures
# ------------------------------------------------------------------------------
s08_fig_module <- ggplot(
  s08_missingness_module_summary,
  aes(
    x = reorder(paste(module, variable_type, sep = " | "), mean_nonresponse_pct),
    y = mean_nonresponse_pct
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Respondent non-response by module",
    x = NULL,
    y = "Mean non-response (%)"
  ) +
  theme_minimal()

ggsave(
  "figure_nonresponse_by_module.png",
  s08_fig_module,
  width = 11,
  height = 7,
  dpi = 300
)

s08_fig_dist <- ggplot(
  s08_row_nonresponse_distribution,
  aes(x = n_nonresponse_in_row, y = rows)
) +
  geom_col() +
  labs(
    title = "Distribution of respondent non-response across records",
    x = "Number of NO RESPONSE items in record",
    y = "Number of records"
  ) +
  theme_minimal()

ggsave(
  "figure_nonresponse_distribution.png",
  s08_fig_dist,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 13. Print outputs for review
# ------------------------------------------------------------------------------
s08_missingness_module_summary
head(s08_missingness_variable_summary, 40)
head(s08_missingness_decision_flags, 40)
s08_row_nonresponse_distribution