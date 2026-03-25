# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  03_data_validation_and_logic_resolution.R
# AUTHOR:  Corona Management Systems
# DATE:    November 26, 2025
#
# PURPOSE:
# Carries out the validation stage for the prepared survey dataset.
#
# This script:
# 1. loads the prepared dataset and metadata,
# 2. defines universal variables and explicit skip-logic rules,
# 3. performs spatial validation of reported State and LGA,
# 4. audits variables for holes and ghost values,
# 5. checks categorical responses against the data dictionary,
# 6. applies targeted categorical repairs,
# 7. resolves skip-logic issues using the edge rules,
# 8. generates audit and resolution logs, and
# 9. exports the cleaned validation output for downstream processing.
#
# OUTPUT:
#   - Sproxil_Unique_Invalid_Values.csv
#   - Sproxil_Audit_BEFORE_Cleaning.csv
#   - Sproxil_Audit_AFTER_Cleaning.csv
#   - Sproxil_Resolution_Log.csv
#   - Sproxil_Cleaned_Final.rds
#
# REPLICATION NOTE:
# This file validates structure, geography, categories, and skip logic after the
# initial preparation stage. It also records all major logic repairs and exports
# audit files that can be reviewed alongside the cleaned final dataset.
# ==============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(sf)

# ------------------------------------------------------------------------------
# 1. Load prepared data and metadata
# ------------------------------------------------------------------------------
s03_df_prepared <- readRDS("Sproxil_Prepared.rds")
s03_metadata <- readRDS("Sproxil_dictionary.rds")
s03_dict <- s03_metadata$mis_data_dictionary
s03_mapping <- s03_metadata$column_mapping

# ------------------------------------------------------------------------------
# 2. Define universal variables
# ------------------------------------------------------------------------------
# These are the variables expected across all respondents in this tool.
s03_universal_vars <- c(
  # Meta, location, and demographics
  "meta_respondent_id", "demo_state", "demo_lga", "demo_town",
  "demo_year_of_birth", "demo_age", "demo_gender",
  "demo_edu_level",
  "demo_hh_children_under5", "demo_hh_sleeping_rooms",
  
  # Household core
  "hh_total_persons_v1", "hh_relation_to_head",
  "hh_drinking_water_source", "hh_other_water_source", "hh_water_location",
  "hh_toilet_type", "hh_cookstove_type",
  "hh_owns_livestock", "hh_owns_agri_land",
  "hh_floor_material", "hh_roof_material", "hh_wall_material",
  
  # Assets
  "hh_has_electricity", "hh_has_radio", "hh_has_tv", "hh_has_non_mobile_phone",
  "hh_has_computer", "hh_has_refrigerator", "hh_has_table", "hh_has_chair",
  "hh_has_bed", "hh_has_sofa", "hh_has_cupboard", "hh_has_ac",
  "hh_has_electric_iron", "hh_has_generator", "hh_has_fan", "hh_own_watch",
  "hh_own_mobile_phone", "hh_own_bicycle", "hh_own_motorcycle",
  "hh_own_animal_cart", "hh_own_car_truck", "hh_own_motor_boat",
  "hh_own_canoe", "hh_own_keke_napep", "hh_has_bank_account", "hh_mobile_money_usage",
  
  # Background and attitudes
  "bg_languages", "bg_tv_frequency", "bg_own_smartphone", "bg_internet_ever_used",
  "bg_religion", "bg_ethnic_group", "bg_heard_malaria_msg_6months", "bg_aware_avoidance",
  "att_rainy_season_only", "att_fever_worry_malaria", "att_malaria_easily_treated",
  "att_weak_children_die", "att_net_use_mosquito_density", "att_net_use_warm_weather",
  "att_home_meds_first", "att_full_dose_importance", "att_seek_care_immediate",
  "att_community_net_usage",
  
  # Prevention and access items
  "prev_home_sprayed_interior",
  "prev_repellent_methods",
  "prev_first_treatment_location",
  "prev_time_to_treatment_facility",
  "treat_transport_cost",
  "treat_drug_affordability",
  
  # Parent variables for conditional paths
  "prev_has_mosquito_nets",
  "treat_hh_fever_last_2weeks",
  "treat_heard_smc",
  "treat_vaccine_age_knowledge",
  
  # Health system experience
  "feedback_free_treatment_6months",
  "feedback_drug_stockout_6months",
  "feedback_gov_effort_rating"
)


# ------------------------------------------------------------------------------
# 3. Define explicit edge rules
# ------------------------------------------------------------------------------
# These rules control when child variables should be present or absent.
s03_edge_rules <- tibble::tribble(
  ~parent, ~child, ~trigger_type, ~trigger_value,
  
  # Education
  "demo_edu_level", "demo_edu_informal", "EQUALS", "I NEVER HAD ANY FORMAL EDUCATION",
  
  # Nets
  "prev_has_mosquito_nets", "prev_num_mosquito_nets", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_is_itn", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_months_since_net_obtained", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_net_brand", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_net_obtained_how", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_net_obtained_where", "EQUALS", "YES",
  "prev_has_mosquito_nets", "prev_slept_under_net_last_night", "EQUALS", "YES",
  
  # Net use branches
  "prev_slept_under_net_last_night", "prev_num_people_slept_net", "EQUALS", "YES",
  "prev_slept_under_net_last_night", "prev_net_not_used_reason", "IN_SET", "NO|I AM NOT SURE",
  
  # Household fever and treatment
  "treat_hh_fever_last_2weeks", "treat_blood_sample_taken", "EQUALS", "YES",
  "treat_hh_fever_last_2weeks", "treat_test_cost", "EQUALS", "YES",
  "treat_hh_fever_last_2weeks", "treat_drug_cost", "EQUALS", "YES",
  "treat_hh_fever_last_2weeks", "treat_drug_purchase_time", "EQUALS", "YES",
  
  # SMC
  "treat_heard_smc", "treat_children_received_smc", "EQUALS", "YES",
  "treat_children_received_smc", "treat_know_smc_drug", "STARTS_WITH", "YES",
  
  # Vaccine
  "treat_vaccine_age_knowledge", "treat_children_received_vaccine", "NOT_CONTAINS", "I DON'T KNOW",
  
  # Women module
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
  
  # Child fever module
  "women_child_fever_2weeks", "women_child_blood_sample", "EQUALS", "YES",
  "women_child_fever_2weeks", "women_child_malaria_diagnosis", "EQUALS", "YES",
  "women_child_fever_2weeks", "women_child_seek_advice", "EQUALS", "YES",
  "women_child_fever_2weeks", "women_child_took_medicine", "EQUALS", "YES",
  
  "women_child_seek_advice", "women_child_advice_location", "EQUALS", "YES",
  "women_child_seek_advice", "women_child_first_advice_location", "EQUALS", "YES",
  "women_child_seek_advice", "women_child_advice_delay_days", "EQUALS", "YES",
  "women_child_seek_advice", "women_child_referral", "EQUALS", "YES",
  
  "women_child_took_medicine", "women_child_medicine_type", "EQUALS", "YES",
  
  # ACT gating
  "women_child_medicine_type", "women_child_act_delay", "EQUALS", "ARTEMISININ COMBINATION THERAPY (ACT)",
  "women_child_medicine_type", "women_child_act_effective", "EQUALS", "ARTEMISININ COMBINATION THERAPY (ACT)",
  
  # Background
  "bg_internet_ever_used", "bg_internet_frequency", "EQUALS", "YES",
  "bg_heard_malaria_msg_6months", "bg_malaria_msg_source", "EQUALS", "YES",
  "bg_aware_avoidance", "bg_prevention_knowledge", "EQUALS", "YES",
  
  # Toilet
  "hh_toilet_type", "hh_toilet_shared", "NOT_EQUALS", "NO FACILITY/BUSH/FIELD",
  "hh_toilet_type", "hh_toilet_location", "NOT_EQUALS", "NO FACILITY/BUSH/FIELD",
  "hh_toilet_shared", "hh_toilet_share_count", "EQUALS", "YES",
  
  # Livestock and land
  "hh_owns_livestock", "hh_num_cows_bulls", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_goats", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_other_cattle", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_sheep", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_poultry", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_horses_donkeys", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_pigs", "EQUALS", "YES",
  "hh_owns_livestock", "hh_num_camels", "EQUALS", "YES",
  
  "hh_owns_agri_land", "hh_num_agri_plots", "EQUALS", "YES"
)

# ------------------------------------------------------------------------------
# 4. Spatial validation
# ------------------------------------------------------------------------------
# Standardises names before comparing survey records with the shapefile.
s03_clean_name_standard <- function(x) str_to_upper(str_squish(as.character(x)))

# Loads the boundary file and prepares the matching keys.
s03_admin_boundaries <- st_read(
  "shapefile/grid3_nga_boundary_vaccwards.shp",
  quiet = TRUE
) %>%
  mutate(
    clean_shp_state = s03_clean_name_standard(statename),
    clean_shp_lga   = s03_clean_name_standard(lganame),
    urban_status = str_to_upper(as.character(urban))
  )

cat("--- Running spatial join and distance verification ---\n")

# Prepares State and LGA keys to match known survey and shapefile differences.
s03_df_spatial_temp <- s03_df_prepared %>%
  mutate(
    state_shp_key = case_when(
      s03_clean_name_standard(demo_state) %in% c("ABUJA", "FEDERAL CAPITAL TERRITORY", "FCT ABUJA") ~ "FCT",
      s03_clean_name_standard(demo_state) == "NASSARAWA" ~ "NASARAWA",
      TRUE ~ s03_clean_name_standard(demo_state)
    ),
    
    lga_shp_key = case_match(
      s03_clean_name_standard(demo_lga),
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
      .default = s03_clean_name_standard(demo_lga)
    )
  )

# Converts valid coordinate records to sf format.
s03_survey_sf <- s03_df_spatial_temp %>%
  filter(!is.na(meta_longitude) & !is.na(meta_latitude)) %>%
  st_as_sf(
    coords = c("meta_longitude", "meta_latitude"),
    crs = 4326,
    remove = FALSE
  )

# Assigns each point to the intersecting polygon.
s03_joined_sf <- st_join(
  s03_survey_sf,
  s03_admin_boundaries %>% select(clean_shp_state, clean_shp_lga, urban_status),
  join = st_intersects,
  left = TRUE
)

# Extracts the detected State, LGA, and urban status.
s03_spatial_results <- s03_joined_sf %>%
  st_drop_geometry() %>%
  transmute(
    meta_respondent_id,
    detected_state = clean_shp_state,
    detected_lga   = clean_shp_lga,
    urban_status = urban_status
  ) %>%
  distinct(meta_respondent_id, .keep_all = TRUE)

# Calculates distance to the claimed LGA when the detected LGA does not match.
s03_survey_sf_temp <- s03_survey_sf %>%
  left_join(s03_spatial_results, by = "meta_respondent_id")

s03_lga_mismatch_indices <- which(
  !is.na(s03_survey_sf_temp$detected_lga) &
    (s03_survey_sf_temp$lga_shp_key != s03_survey_sf_temp$detected_lga)
)

s03_dist_km_vector <- rep(NA_real_, nrow(s03_survey_sf))

if (length(s03_lga_mismatch_indices) > 0) {
  s03_survey_key_claimed <- paste0(
    s03_survey_sf_temp$state_shp_key[s03_lga_mismatch_indices], "_",
    s03_survey_sf_temp$lga_shp_key[s03_lga_mismatch_indices]
  )
  
  s03_shape_key <- paste0(
    s03_admin_boundaries$clean_shp_state, "_",
    s03_admin_boundaries$clean_shp_lga
  )
  
  s03_target_shape_indices <- match(s03_survey_key_claimed, s03_shape_key)
  s03_valid_targets_mask <- !is.na(s03_target_shape_indices)
  
  if (any(s03_valid_targets_mask)) {
    s03_survey_m <- st_transform(s03_survey_sf, 3857)
    s03_admin_m  <- st_transform(s03_admin_boundaries, 3857)
    
    s03_dist_calc <- st_distance(
      s03_survey_m[s03_lga_mismatch_indices[s03_valid_targets_mask], ],
      st_geometry(s03_admin_m)[s03_target_shape_indices[s03_valid_targets_mask]],
      by_element = TRUE
    )
    
    s03_dist_km_vector[s03_lga_mismatch_indices[s03_valid_targets_mask]] <-
      as.numeric(s03_dist_calc) / 1000
  }
}

# Integrates spatial outputs and creates match flags.
s03_df_spatial <- s03_df_spatial_temp %>%
  left_join(s03_spatial_results, by = "meta_respondent_id") %>%
  left_join(
    tibble(
      meta_respondent_id = s03_survey_sf$meta_respondent_id,
      lga_dist_error_km = round(s03_dist_km_vector, 2)
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
  select(-lga_shp_key, -state_shp_key)

# ------------------------------------------------------------------------------
# 5. Audit helper functions
# ------------------------------------------------------------------------------
s03_edge_triggered <- function(parent_vec, trigger_type, trigger_value) {
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

s03_audit_universal <- function(df, universal_vars) {
  tibble(Variable = universal_vars) %>%
    mutate(
      Missing_Holes = ifelse(
        Variable %in% names(df),
        sapply(
          Variable[Variable %in% names(df)],
          \(v) sum(is.na(df[[v]]) | df[[v]] == "", na.rm = TRUE)
        ),
        NA_integer_
      ),
      Ghost_Values = 0
    )
}

s03_audit_edges <- function(df, edge_rules) {
  out <- vector("list", nrow(edge_rules))
  
  for (i in seq_len(nrow(edge_rules))) {
    p <- edge_rules$parent[i]
    c <- edge_rules$child[i]
    tt <- edge_rules$trigger_type[i]
    tv <- edge_rules$trigger_value[i]
    
    if (!(p %in% names(df) && c %in% names(df))) next
    
    trig <- s03_edge_triggered(df[[p]], tt, tv)
    child <- as.character(df[[c]])
    
    missing <- is.na(child) | child == ""
    present <- !missing & child != "NO RESPONSE"
    
    out[[i]] <- tibble(
      Variable = c,
      Parent = p,
      Trigger_Type = tt,
      Trigger_Value = tv,
      Missing_Holes = sum(trig & missing, na.rm = TRUE),
      Ghost_Values  = sum(!trig & present, na.rm = TRUE)
    )
  }
  
  bind_rows(out) %>%
    group_by(Variable) %>%
    summarise(
      Missing_Holes = sum(Missing_Holes, na.rm = TRUE),
      Ghost_Values  = sum(Ghost_Values, na.rm = TRUE),
      .groups = "drop"
    )
}

s03_generate_variable_audit <- function(df, universal_vars, edge_rules) {
  bind_rows(
    s03_audit_universal(df, universal_vars) %>% select(Variable, Missing_Holes, Ghost_Values),
    s03_audit_edges(df, edge_rules) %>% select(Variable, Missing_Holes, Ghost_Values)
  ) %>%
    group_by(Variable) %>%
    summarise(
      Missing_Holes = sum(Missing_Holes, na.rm = TRUE),
      Ghost_Values  = sum(Ghost_Values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Missing_Holes + Ghost_Values))
}

# ------------------------------------------------------------------------------
# 6. Dictionary audit
# ------------------------------------------------------------------------------
cat("--- Running dictionary audit (multi-select aware) ---\n")

# Lists variables allowed to contain multiple responses.
s03_multi_select_cols <- c(
  "demo_edu_informal", "prev_repellent_methods", "women_anc_provider", 
  "women_anc_location", "women_sp_fansidar_source", "women_child_advice_location",
  "women_child_medicine_type", "bg_languages", "bg_malaria_msg_source", "bg_prevention_knowledge"
)

# Selects variables found in both the data and the dictionary.
s03_audit_cols <- setdiff(
  intersect(names(s03_df_spatial), names(s03_dict)),
  c("demo_town", "demo_lga")
)

s03_invalid_values_report <- s03_df_spatial %>%
  select(any_of(s03_audit_cols)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "Variable_Name", values_to = "Found_Value") %>%
  filter(!is.na(Found_Value), Found_Value != "") %>%
  mutate(
    Found_Value = ifelse(
      Variable_Name %in% s03_multi_select_cols,
      str_split(Found_Value, "[;,]+"),
      Found_Value
    )
  ) %>%
  unnest(Found_Value) %>%
  mutate(Found_Value = str_trim(Found_Value)) %>%
  filter(Found_Value != "") %>%
  distinct(Variable_Name, Found_Value) %>%
  rowwise() %>%
  filter(!(toupper(Found_Value) %in% toupper(s03_dict[[Variable_Name]]))) %>%
  ungroup() %>%
  arrange(Variable_Name)

write.csv(
  s03_invalid_values_report,
  "Sproxil_Unique_Invalid_Values.csv",
  row.names = FALSE
)

cat("✅ Dictionary audit complete. Multi-select answers handled correctly.\n")

# ------------------------------------------------------------------------------
# 7. Targeted categorical repairs
# ------------------------------------------------------------------------------
cat("--- 2.5. Repairing categorical discrepancies ---\n")

s03_df_repair <- s03_df_spatial %>%
  mutate(
    bg_internet_frequency = ifelse(
      bg_internet_frequency == "CHRISTIAN",
      "NO RESPONSE",
      bg_internet_frequency
    ),
    
    hh_cookstove_type = case_match(
      hh_cookstove_type,
      "WOOD"              ~ "NO RESPONSE",
      "ANIMAL DUNG/WASTE" ~ "NO RESPONSE",
      .default = hh_cookstove_type
    ),
    
    women_child_advice_location = ifelse(
      women_child_advice_location == "ANTENATAL VISIT TO A HEATH FACILITY",
      "NO RESPONSE",
      women_child_advice_location
    )
  )

cat("✅ Categorical repair block applied successfully.\n")

# ------------------------------------------------------------------------------
# 8. Resolve holes and ghost values using the edge rules
# ------------------------------------------------------------------------------
s03_apply_edge_resolution <- function(df, edge_rules) {
  out <- df %>% mutate(across(everything(), as.character))
  
  for (i in seq_len(nrow(edge_rules))) {
    p <- edge_rules$parent[i]
    c <- edge_rules$child[i]
    tt <- edge_rules$trigger_type[i]
    tv <- edge_rules$trigger_value[i]
    
    if (!(p %in% names(out) && c %in% names(out))) next
    
    trig <- s03_edge_triggered(out[[p]], tt, tv)
    
    # Fills holes where the child was expected but missing.
    out[[c]] <- ifelse(
      trig & (is.na(out[[c]]) | out[[c]] == ""),
      "NO RESPONSE",
      out[[c]]
    )
    
    # Removes ghost values where the child was not expected.
    out[[c]] <- ifelse(
      !trig & !is.na(out[[c]]) & out[[c]] != "",
      NA_character_,
      out[[c]]
    )
  }
  
  out
}

cat("--- Generating 'BEFORE' audit report ---\n")

s03_audit_before <- s03_generate_variable_audit(
  s03_df_spatial,
  s03_universal_vars,
  s03_edge_rules
)

write.csv(
  s03_audit_before,
  "Sproxil_Audit_BEFORE_Cleaning.csv",
  row.names = FALSE
)

cat("--- Running exhaustive resolution (holes and ghosts) ---\n")

s03_df_original <- s03_df_repair
s03_df_resolved <- s03_apply_edge_resolution(s03_df_repair, s03_edge_rules)

# ------------------------------------------------------------------------------
# 9. Demographic consistency checks
# ------------------------------------------------------------------------------
s03_df_resolved <- s03_df_resolved %>%
  mutate(
    across(
      c(treat_children_received_smc, treat_children_received_vaccine, women_child_fever_2weeks),
      ~ case_when(
        (demo_hh_children_under5 == "0" | demo_hh_children_under5 == 0) & . == "YES" ~ NA_character_,
        TRUE ~ as.character(.)
      )
    )
  )

# ------------------------------------------------------------------------------
# 10. Patch universal variables and create the resolution log
# ------------------------------------------------------------------------------
cat("--- 4. Patching universal variables and generating resolution log ---\n")

# Fills remaining missing values in universal variables with NO RESPONSE.
s03_df_resolved <- s03_df_resolved %>%
  mutate(across(any_of(s03_universal_vars), ~ ifelse(is.na(.), "NO RESPONSE", .)))

# Compares the repaired and resolved versions to produce a change log.
cat("   - Comparing original data to resolved data... (null-safe comparison)\n")

s03_res_log <- s03_df_original %>%
  select(meta_respondent_id, everything()) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-meta_respondent_id, names_to = "Variable", values_to = "Original_Value") %>%
  left_join(
    s03_df_resolved %>%
      select(meta_respondent_id, everything()) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(-meta_respondent_id, names_to = "Variable", values_to = "Resolved_Value"),
    by = c("meta_respondent_id", "Variable")
  ) %>%
  mutate(
    temp_orig = replace_na(Original_Value, "[[EMPTY]]"),
    temp_res  = replace_na(Resolved_Value, "[[EMPTY]]")
  ) %>%
  filter(temp_orig != temp_res) %>%
  select(-temp_orig, -temp_res) %>%
  mutate(
    Resolution_Type = case_when(
      (is.na(Original_Value) | Original_Value == "") & Resolved_Value == "NO RESPONSE" ~
        "HOLE: Missing value filled with NO RESPONSE",
      !is.na(Original_Value) & is.na(Resolved_Value) ~
        "GHOST: Skip-logic violation purged to BLANK (NA)",
      TRUE ~
        "DATA_PATCH: Categorical or logic correction"
    )
  )

write.csv(s03_res_log, "Sproxil_Resolution_Log.csv", row.names = FALSE)

cat("--- Generating 'AFTER' audit report ---\n")

s03_audit_after <- s03_generate_variable_audit(
  s03_df_resolved,
  s03_universal_vars,
  s03_edge_rules
)

write.csv(
  s03_audit_after,
  "Sproxil_Audit_AFTER_Cleaning.csv",
  row.names = FALSE
)

# Prints a simple before-and-after check.
cat("\nVerification Summary:\n")
cat("Issues Before:", sum(s03_audit_before$Missing_Holes + s03_audit_before$Ghost_Values), "\n")
cat("Issues After: ", sum(s03_audit_after$Missing_Holes + s03_audit_after$Ghost_Values), "\n")

# ------------------------------------------------------------------------------
# 11. Export final validation outputs
# ------------------------------------------------------------------------------
cat("--- 5. Exporting final datasets ---\n")

# Re-applies variable labels for downstream usability.
for (col_name in names(s03_df_resolved)) {
  full_question <- s03_mapping[col_name]
  
  if (!is.na(full_question)) {
    attr(s03_df_resolved[[col_name]], "label") <- as.character(full_question)
  }
}

# Saves the final validated dataset.
saveRDS(s03_df_resolved, "Sproxil_Cleaned_Final.rds")
cat("✅ Final RDS file saved: Sproxil_Cleaned_Final.rds\n")


# Prints a final validation summary.
cat("\n==================================================================\n")
cat("DATA VALIDATION SUMMARY\n")
cat("------------------------------------------------------------------\n")
cat("Total Records Processed:  ", nrow(s03_df_resolved), "\n")
cat("Total Logical Corrections:", nrow(s03_res_log), "\n")
cat("Logic Violations Purged:  ", sum(s03_res_log$Resolution_Type == "GHOST: Skip-logic violation purged to BLANK (NA)"), "\n")
cat("Missing Gaps Resolved:    ", sum(s03_res_log$Resolution_Type == "HOLE: Missing value filled with NO RESPONSE"), "\n")
cat("==================================================================\n")