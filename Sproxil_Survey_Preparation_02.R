# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  02_Data_Cleaning.R
# PURPOSE: Structural Preparation & Deduplication
# ==============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(knitr)
library(lubridate)

# --- 1. CONFIGURATION ---
INPUT_FILE <- "Sproxil_Survey_Data_030226.xlsx"
METADATA_RDS <- "Sproxil_dictionary.rds"
OUTPUT_RDS <- "Sproxil_Prepared.rds"

# --- 2. LOAD DATA & METADATA ---
cat("--- 1. Loading Metadata and Raw Data ---\n")
if (!file.exists(METADATA_RDS)) stop("Metadata RDS not found!")
metadata <- readRDS(METADATA_RDS)

raw_data <- read_excel(INPUT_FILE, sheet = "Survey Data", col_types = "text")

# --- 3. TEXT STANDARDIZATION ---
cat("--- 2. Standardizing Text Content ---\n")
# Convert all character columns to UPPERCASE and remove extra whitespace
sproxil_clean <- raw_data %>%
  mutate(across(where(is.character), ~ str_squish(toupper(.)))) %>%
  # Filter for Completed surveys and apply the mapping
  filter(status == "COMPLETED") %>%
  rename(any_of(metadata$column_mapping)) %>%
  # Clean GPS Columns
  mutate(meta_latitude = as.numeric(meta_latitude), meta_longitude = as.numeric(meta_longitude))

# Verify the renaming worked
print(names(sproxil_clean))


# --- 4. DUPLICATE DETECTION (TIME-AWARE FINGERPRINTING) ---
cat("--- 3. Identifying Potential Duplicates ---\n")

# 4.1. Parse the date_created column
# This converts the date to an R datetime object
# 4.1 Parse meta_date_created safely (expected format: dd/mm/yyyy HH:MM)
sproxil_clean <- sproxil_clean %>%
  mutate(
    dt_created = suppressWarnings(lubridate::dmy_hm(meta_date_created, tz = "Africa/Lagos"))
  )

# Hard checks (fail fast if parsing breaks)
n_dt_na <- sum(is.na(sproxil_clean$dt_created))
if (n_dt_na > 0) {
  warning("dt_created parsing produced NA for ", n_dt_na, " rows. Check meta_date_created format.")
  # Optional: write the bad rows for audit
  write.csv(
    sproxil_clean %>% filter(is.na(dt_created)) %>% select(meta_respondent_id, meta_date_created),
    "Sproxil_BadDateCreated_Audit.csv",
    row.names = FALSE
  )
}

# --- 4. DUPLICATE DETECTION (RESPONDENT-ID FIRST, THEN FINGERPRINT) ---
cat("--- 3. Identifying Potential Duplicates (Improved) ---\n")


# Ensure dt_created exists (from the parsing fix above)
if (!"dt_created" %in% names(sproxil_clean)) stop("dt_created missing. Date parsing step failed.")

# 4A) DEDUPE BY RESPONDENT ID (strongest key)
# Keep earliest submission per respondent_id (change to slice_max if you prefer latest)
dup_by_id <- sproxil_clean %>%
  filter(!is.na(meta_respondent_id), meta_respondent_id != "") %>%
  group_by(meta_respondent_id) %>%
  arrange(dt_created, .by_group = TRUE) %>%
  mutate(dup_id_seq = row_number(),
         is_dup_id  = dup_id_seq > 1) %>%
  ungroup()

discard_id <- dup_by_id %>%
  filter(is_dup_id) %>%
  select(meta_respondent_id, dt_created, meta_date_created, meta_channel, demo_state, demo_lga, everything())

if (nrow(discard_id) > 0) {
  write.csv(discard_id, "Sproxil_Duplicates_ByRespondentID_Audit.csv", row.names = FALSE)
  cat("⚠️ Found and removed", nrow(discard_id), "duplicate rows by respondent_id.\n")
} else {
  cat("✅ No duplicates found by respondent_id.\n")
}

keep_id <- dup_by_id %>%
  filter(!is_dup_id) %>%
  select(-dup_id_seq, -is_dup_id)

# Bring back records with missing respondent_id untouched (for now)
no_id <- sproxil_clean %>%
  filter(is.na(meta_respondent_id) | meta_respondent_id == "")

sproxil_clean <- bind_rows(keep_id, no_id)

sproxil_clean <- sproxil_clean %>% select(-dt_created)

# --- 5. CONTENT VALIDATION (Dictionary Check) ---
cat("--- 4. Validating Content Against Data Dictionary ---\n")

# Multi-select columns to split during check
multi_select_cols <- c(
  "demo_edu_informal", "prev_repellent_methods", "women_anc_provider", 
  "women_anc_location", "women_sp_fansidar_source", "women_child_advice_location",
  "women_child_medicine_type", "bg_languages", "bg_malaria_msg_source", "bg_prevention_knowledge"
)

if ("mis_data_dictionary" %in% names(metadata)) {
  
  mis_dict <- metadata$mis_data_dictionary
  invalid_report <- list()
  
  # Check columns that exist in both Data and Dictionary
  cols_to_check <- intersect(names(sproxil_clean), names(mis_dict))
  
  for (col in cols_to_check) {
    # Get allowed values from metadata and force to Upper
    allowed <- toupper(as.character(mis_dict[[col]]))
    
    # Get actual values from data
    raw_vals <- na.omit(sproxil_clean[[col]])
    raw_vals <- raw_vals[raw_vals != ""]
    
    if (length(raw_vals) == 0) next
    
    if (col %in% multi_select_cols) {
      actual_items <- unlist(stringr::str_split(raw_vals, "[;,]+"))
      actual <- unique(stringr::str_trim(actual_items))
    } else {
      actual <- unique(raw_vals)
    }
    
    # Identify discrepancies
    bad_vals <- setdiff(actual, allowed)
    
    if (length(bad_vals) > 0) {
      invalid_report[[col]] <- paste(bad_vals, collapse = "; ")
    }
  }
  
  if (length(invalid_report) > 0) {
    invalid_df <- tibble(
      Variable = names(invalid_report),
      `Invalid Values Found` = unlist(invalid_report)
    )
    cat("⚠️ WARNING: Dictionary validation found mismatches:\n")
    print(kable(invalid_df))
    write.csv(invalid_df, "Sproxil_Invalid_Values_Log.csv", row.names = FALSE)
  } else {
    cat("✅ SUCCESS: All categorical values match the Data Dictionary.\n")
  }
}

# Audit-grade processing log
n_raw <- nrow(raw_data)
n_completed <- raw_data %>% filter(status == "Completed") %>% nrow()
n_final <- nrow(sproxil_clean)

cat("N raw:", n_raw, "\n")
cat("N completed:", n_completed, "\n")
cat("N final after dedup:", n_final, "\n")


# --- 5. EXPORT PREPARED DATA ---
saveRDS(sproxil_clean, OUTPUT_RDS)
cat("✅ Script 02 Complete. Prepared data saved to:", OUTPUT_RDS, "\n")