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
  mutate(latitude = as.numeric(meta_latitude), longitude = as.numeric(meta_longitude))

# Verify the renaming worked
print(names(sproxil_clean))


# --- 4. DUPLICATE DETECTION (TIME-AWARE FINGERPRINTING) ---
cat("--- 3. Identifying Potential Duplicates ---\n")

# 4.1. Parse the date_created column
# This converts the date to an R datetime object
sproxil_clean <- sproxil_clean %>%
  mutate(dt_created = as_datetime(meta_date_created))

# 4.2. Create the demographic fingerprint
# We exclude the date from the key itself so we can group identical profiles
sproxil_fingerprinted <- sproxil_clean %>%
  unite("master_key", 
        any_of(c("demo_year_of_birth", "demo_gender", "bg_ethnic_group", 
                 "hh_total_persons_v1", "hh_floor_material", "hh_roof_material")), 
        starts_with("hh_has_"), starts_with("hh_own_"),
        sep = "|", remove = FALSE)

# 4.3. Detect duplicates within a time threshold (e.g., 60 minutes)
# Logic: If two identical profiles are submitted within 60 mins, they are duplicates.
TIME_THRESHOLD_MINS <- 60 

sproxil_deduped <- sproxil_fingerprinted %>%
  group_by(master_key) %>%
  arrange(dt_created) %>%
  mutate(
    time_since_prev = as.numeric(difftime(dt_created, lag(dt_created), units = "mins")),
    is_duplicate = ifelse(!is.na(time_since_prev) & time_since_prev <= TIME_THRESHOLD_MINS, TRUE, FALSE)
  ) %>%
  ungroup()

# 4.4. Audit and Discard
discarded_duplicates <- sproxil_deduped %>% filter(is_duplicate == TRUE)

if(nrow(discarded_duplicates) > 0) {
  write.csv(discarded_duplicates, "Sproxil_Duplicates_Audit.csv", row.names = FALSE)
  cat("⚠️ Found and removed", nrow(discarded_duplicates), 
      "duplicates submitted within", TIME_THRESHOLD_MINS, "mins of an identical profile.\n")
  
  # Keep only the first entry of a duplicate group
  sproxil_clean <- sproxil_deduped %>%
    filter(is_duplicate == FALSE) %>%
    select(-master_key, -dt_created, -time_since_prev, -is_duplicate)
} else {
  cat("✅ No duplicates found within the time threshold.\n")
  sproxil_clean <- sproxil_clean %>% select(-dt_created)
}


# --- 5. CONTENT VALIDATION (Dictionary Check) ---
cat("--- 4. Validating Content Against Data Dictionary ---\n")

# Multi-select columns to split during check
multi_select_cols <- c(
  "demo_edu_informal", "prev_repellent_methods", "women_anc_provider", 
  "women_anc_location", "women_sp_fansidar_source", "women_child_advice_location",
  "women_child_medicine_type", "bg_languages", "bg_malaria_msg_source", "bg_prevention_knowledge"
)

if ("mis_data_dictionary" %in% names(metadata)) {
  
  metadata$value_labels <- lapply(metadata$value_labels, toupper)
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


# --- 5. EXPORT PREPARED DATA ---
saveRDS(sproxil_clean, OUTPUT_RDS)
cat("✅ Script 02 Complete. Prepared data saved to:", OUTPUT_RDS, "\n")