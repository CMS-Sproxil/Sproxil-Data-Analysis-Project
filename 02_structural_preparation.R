# ==============================================================================
# PROJECT: Sproxil Malaria Survey
# SCRIPT:  02_structural_preparation.R
# AUTHOR:  Corona Management Systems
# DATE:    26 February 2026
#
# PURPOSE:
# Carries out the first structural cleaning stage for the raw survey export.
#
# This script:
# 1. loads the raw survey data and metadata,
# 2. standardises text fields,
# 3. keeps completed interviews only,
# 4. applies the standard variable names,
# 5. parses submission date and time,
# 6. removes duplicate records by respondent ID,
# 7. checks categorical values against the data dictionary,
# 8. produces missingness audit outputs, and
# 9. saves the prepared dataset for downstream recoding.
#
# OUTPUT:
#   - Sproxil_Prepared.rds
#   - Sproxil_BadDateCreated_Audit.csv
#   - Sproxil_Duplicates_ByRespondentID_Audit.csv
#   - Sproxil_Invalid_Values_Log.csv
#   - missing_drop_summary.csv
#   - missing_by_variable.csv
#   - drop_pattern_summary.csv
#   - missing_pattern_table_only.csv
#   - rows_to_drop.csv
#   - rows_to_keep.csv
#
# REPLICATION NOTE:
# This file prepares the raw survey export for recoding by enforcing a
# consistent structure, removing duplicate respondent records, and generating
# audit files for date parsing, dictionary mismatches, and missingness review.
# ==============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(knitr)
library(lubridate)

# ------------------------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------------------------
INPUT_FILE <- "Sproxil_Survey_Data_030226.xlsx"
METADATA_RDS <- "Sproxil_dictionary.rds"
OUTPUT_RDS <- "Sproxil_Prepared.rds"

# ------------------------------------------------------------------------------
# 2. Load raw data and metadata
# ------------------------------------------------------------------------------
cat("--- 1. Loading metadata and raw data ---\n")

if (!file.exists(METADATA_RDS)) stop("Metadata RDS not found!")

s02_metadata <- readRDS(METADATA_RDS)

s02_raw_data <- read_excel(
  INPUT_FILE,
  sheet = "Survey Data",
  col_types = "text"
)

# ------------------------------------------------------------------------------
# 3. Standardise text and apply core structural cleaning
# ------------------------------------------------------------------------------
cat("--- 2. Standardising text content ---\n")

# Converts text to upper case, trims extra spaces, keeps completed interviews,
# applies the standard names, and converts GPS fields to numeric.
s02_prepared <- s02_raw_data %>%
  mutate(across(where(is.character), ~ str_squish(toupper(.)))) %>%
  filter(status == "COMPLETED") %>%
  rename(any_of(s02_metadata$column_mapping)) %>%
  mutate(
    meta_latitude = as.numeric(meta_latitude),
    meta_longitude = as.numeric(meta_longitude)
  )

# Confirms that the standard names were applied as expected.
print(names(s02_prepared))

# ------------------------------------------------------------------------------
# 4. Parse submission date and time
# ------------------------------------------------------------------------------
cat("--- 3. Identifying potential duplicates ---\n")

# Parses the submission timestamp using the expected format:
# dd/mm/yyyy HH:MM
s02_prepared <- s02_prepared %>%
  mutate(
    dt_created = suppressWarnings(
      lubridate::dmy_hm(meta_date_created, tz = "Africa/Lagos")
    )
  )

# Stops for review if date parsing fails for any records.
s02_n_dt_na <- sum(is.na(s02_prepared$dt_created))

if (s02_n_dt_na > 0) {
  warning(
    "dt_created parsing produced NA for ",
    s02_n_dt_na,
    " rows. Check meta_date_created format."
  )
  
  write.csv(
    s02_prepared %>%
      filter(is.na(dt_created)) %>%
      select(meta_respondent_id, meta_date_created),
    "Sproxil_BadDateCreated_Audit.csv",
    row.names = FALSE
  )
}

# ------------------------------------------------------------------------------
# 5. Remove duplicates by respondent ID
# ------------------------------------------------------------------------------
cat("--- 3. Identifying potential duplicates (improved) ---\n")

# Confirms that the parsed datetime field exists before deduplication.
if (!"dt_created" %in% names(s02_prepared)) {
  stop("dt_created missing. Date parsing step failed.")
}

# Keeps the earliest submission for each respondent ID.
s02_dup_by_id <- s02_prepared %>%
  filter(!is.na(meta_respondent_id), meta_respondent_id != "") %>%
  group_by(meta_respondent_id) %>%
  arrange(dt_created, .by_group = TRUE) %>%
  mutate(
    dup_id_seq = row_number(),
    is_dup_id  = dup_id_seq > 1
  ) %>%
  ungroup()

# Saves duplicate records removed at this step for audit.
s02_discard_id <- s02_dup_by_id %>%
  filter(is_dup_id) %>%
  select(
    meta_respondent_id,
    dt_created,
    meta_date_created,
    meta_channel,
    demo_state,
    demo_lga,
    everything()
  )

if (nrow(s02_discard_id) > 0) {
  write.csv(
    s02_discard_id,
    "Sproxil_Duplicates_ByRespondentID_Audit.csv",
    row.names = FALSE
  )
  cat("⚠️ Found and removed", nrow(s02_discard_id), "duplicate rows by respondent_id.\n")
} else {
  cat("✅ No duplicates found by respondent_id.\n")
}

# Keeps the non-duplicate respondent ID records.
s02_keep_id <- s02_dup_by_id %>%
  filter(!is_dup_id) %>%
  select(-dup_id_seq, -is_dup_id)

# Keeps records with missing respondent ID unchanged at this stage.
s02_no_id <- s02_prepared %>%
  filter(is.na(meta_respondent_id) | meta_respondent_id == "")

# Rebuilds the prepared dataset after respondent ID deduplication.
s02_prepared <- bind_rows(s02_keep_id, s02_no_id)

# Removes the helper datetime field after deduplication.
s02_prepared <- s02_prepared %>%
  select(-dt_created)

# ------------------------------------------------------------------------------
# 6. Validate categorical values against the data dictionary
# ------------------------------------------------------------------------------
cat("--- 4. Validating content against data dictionary ---\n")

# Lists the multi-select variables that need splitting during validation.
s02_multi_select_cols <- c(
  "demo_edu_informal",
  "prev_repellent_methods",
  "women_anc_provider",
  "women_anc_location",
  "women_sp_fansidar_source",
  "women_child_advice_location",
  "women_child_medicine_type",
  "bg_languages",
  "bg_malaria_msg_source",
  "bg_prevention_knowledge"
)

if ("mis_data_dictionary" %in% names(s02_metadata)) {
  
  s02_mis_dict <- s02_metadata$mis_data_dictionary
  s02_invalid_report <- list()
  
  # Checks only variables found in both the dataset and the dictionary.
  s02_cols_to_check <- intersect(names(s02_prepared), names(s02_mis_dict))
  
  for (col in s02_cols_to_check) {
    
    # Standardises the allowed values from the dictionary.
    s02_allowed <- toupper(as.character(s02_mis_dict[[col]]))
    
    # Extracts the observed values from the dataset.
    s02_raw_vals <- na.omit(s02_prepared[[col]])
    s02_raw_vals <- s02_raw_vals[s02_raw_vals != ""]
    
    if (length(s02_raw_vals) == 0) next
    
    if (col %in% s02_multi_select_cols) {
      s02_actual_items <- unlist(stringr::str_split(s02_raw_vals, "[;,]+"))
      s02_actual <- unique(stringr::str_trim(s02_actual_items))
    } else {
      s02_actual <- unique(s02_raw_vals)
    }
    
    # Flags values not found in the dictionary.
    s02_bad_vals <- setdiff(s02_actual, s02_allowed)
    
    if (length(s02_bad_vals) > 0) {
      s02_invalid_report[[col]] <- paste(s02_bad_vals, collapse = "; ")
    }
  }
  
  if (length(s02_invalid_report) > 0) {
    s02_invalid_df <- tibble(
      Variable = names(s02_invalid_report),
      `Invalid Values Found` = unlist(s02_invalid_report)
    )
    
    cat("⚠️ WARNING: Dictionary validation found mismatches:\n")
    print(kable(s02_invalid_df))
    
    write.csv(
      s02_invalid_df,
      "Sproxil_Invalid_Values_Log.csv",
      row.names = FALSE
    )
  } else {
    cat("✅ SUCCESS: All categorical values match the data dictionary.\n")
  }
}

# ------------------------------------------------------------------------------
# 7. Print an audit summary
# ------------------------------------------------------------------------------
s02_n_raw <- nrow(s02_raw_data)
s02_n_completed <- s02_raw_data %>%
  filter(status == "Completed") %>%
  nrow()
s02_n_final <- nrow(s02_prepared)

cat("N raw:", s02_n_raw, "\n")
cat("N completed:", s02_n_completed, "\n")
cat("N final after dedup:", s02_n_final, "\n")

# ------------------------------------------------------------------------------
# 8. Define variables expected for missingness review
# ------------------------------------------------------------------------------
# These are the core variables expected across all respondents in this tool.
s02_universal_vars <- c(
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
  
  # Parent items for conditional paths
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
# 9. Build missingness audit outputs
# ------------------------------------------------------------------------------
# Keeps only variables that are present in the prepared dataset.
s02_vars_present <- s02_universal_vars[s02_universal_vars %in% names(s02_prepared)]

# Marks rows that are complete across the selected variables.
s02_row_is_complete <- complete.cases(s02_prepared[s02_vars_present])

# Summarises overall retention after applying the completeness check.
s02_missing_drop_summary <- data.frame(
  total_rows    = nrow(s02_prepared),
  rows_retained = sum(s02_row_is_complete),
  rows_dropped  = sum(!s02_row_is_complete),
  pct_retained  = round(100 * sum(s02_row_is_complete) / nrow(s02_prepared), 1),
  pct_dropped   = round(100 * sum(!s02_row_is_complete) / nrow(s02_prepared), 1)
)

# Counts missing values by variable.
s02_missing_by_variable <- data.frame(
  variable  = s02_vars_present,
  missing_n = sapply(s02_prepared[s02_vars_present], function(x) sum(is.na(x)))
) |>
  mutate(
    total_n = nrow(s02_prepared),
    missing_pct = round(100 * missing_n / total_n, 1)
  ) |>
  arrange(desc(missing_n))

# Splits the dataset into rows retained and rows dropped.
s02_rows_to_drop <- s02_prepared[!s02_row_is_complete, ]
s02_rows_to_keep <- s02_prepared[s02_row_is_complete, ]

# Summarises the number of missing values per dropped row.
s02_drop_pattern_summary <- s02_prepared |>
  select(all_of(s02_vars_present)) |>
  mutate(
    n_missing_in_row = rowSums(is.na(across(everything())))
  ) |>
  filter(n_missing_in_row > 0) |>
  count(n_missing_in_row, name = "rows") |>
  arrange(n_missing_in_row)

# Builds a table of common missingness patterns.
s02_missing_df <- s02_prepared |>
  select(all_of(s02_vars_present))

s02_missing_pattern_table <- s02_missing_df |>
  mutate(
    missing_pattern = apply(
      is.na(s02_missing_df),
      1,
      function(x) {
        vars <- s02_vars_present[x]
        if (length(vars) == 0) "No missing"
        else paste(vars, collapse = ", ")
      }
    )
  ) |>
  count(missing_pattern, name = "rows") |>
  arrange(desc(rows))

s02_missing_pattern_table_only <- s02_missing_pattern_table |>
  filter(missing_pattern != "No missing")

# ------------------------------------------------------------------------------
# 10. Export missingness audit files
# ------------------------------------------------------------------------------
write.csv(s02_missing_drop_summary, "missing_drop_summary.csv", row.names = FALSE)
write.csv(s02_missing_by_variable, "missing_by_variable.csv", row.names = FALSE)
write.csv(s02_drop_pattern_summary, "drop_pattern_summary.csv", row.names = FALSE)
write.csv(s02_missing_pattern_table_only, "missing_pattern_table_only.csv", row.names = FALSE)
write.csv(s02_rows_to_drop, "rows_to_drop.csv", row.names = FALSE)
write.csv(s02_rows_to_keep, "rows_to_keep.csv", row.names = FALSE)

# Prints the key outputs for review in the console.
s02_missing_drop_summary
s02_missing_by_variable
s02_drop_pattern_summary
head(s02_missing_pattern_table_only, 20)

# ------------------------------------------------------------------------------
# 11. Save prepared dataset
# ------------------------------------------------------------------------------
saveRDS(s02_prepared, OUTPUT_RDS)
cat("✅ Script 02 complete. Prepared data saved to:", OUTPUT_RDS, "\n")