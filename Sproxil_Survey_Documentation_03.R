# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  07_Master_Project_Documentation.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    February 24, 2026
#
# DESCRIPTION:
# 1. Generates a Technical Data Dictionary (Codebook) with Universe definitions.
# 2. Produces a Row-Level Audit Trail (Cleaning Log) comparing Raw vs. Final data.
# 3. Exports technical notes on methodology, weighting, and data limitations.
#
# NOTE: This script provides the "Chain of Custody" required for high-level 
#       reproducibility and professional scientific audit.
# ==============================================================================

library(tidyverse)
library(openxlsx)
library(labelled)
library(readxl)

# --- 1. CONFIGURATION & LOAD DATASETS ---
cat("--- 1. Loading Datasets ---\n")

RAW_FILE    <- "Sproxil_Survey_Data_030226.xlsx"
FINAL_FILE  <- "Sproxil_Derived_Final.rds"
METADATA    <- "Sproxil_dictionary.rds"

raw_df   <- read_excel(RAW_FILE, sheet = "Survey Data", col_types = "text")
final_df <- readRDS(FINAL_FILE)
meta     <- readRDS(METADATA)

# --- 2. VARIABLE RENAMING LOG ---
# This captures the "Chain of Custody" for column names (Old Header -> New Variable)
cat("--- 2. Generating Variable Renaming Log ---\n")

# We create a map of how columns were renamed in Script 02
renaming_log <- tibble(
  Original_Excel_Header = names(raw_df),
  Renamed_Variable_Name = names(raw_df) # Default to same
) %>%
  # Apply the mapping logic used in the project
  mutate(Renamed_Variable_Name = recode(Renamed_Variable_Name, !!!meta$column_mapping)) %>%
  filter(Original_Excel_Header != Renamed_Variable_Name) %>%
  mutate(
    Action_Description = "Renamed raw survey header to standard variable name",
    Audit_Date = Sys.Date()
  )

# --- 3. PREPARE VALUE AUDIT (Row-Level Cleaning Log) ---
cat("--- 3. Preparing Value-Level Cleaning Log ---\n")

# Prepare raw data with standard names for side-by-side comparison
raw_renamed <- raw_df %>% 
  rename(any_of(meta$column_mapping)) %>%
  mutate(across(everything(), as.character))

final_char <- final_df %>% 
  mutate(across(everything(), as.character))

# Pivot to long format for comparison
raw_long <- raw_renamed %>%
  select(any_of(names(final_char))) %>% 
  pivot_longer(-meta_respondent_id, names_to = "Variable", values_to = "Original_Value")

clean_long <- final_char %>%
  select(any_of(names(raw_renamed))) %>% 
  pivot_longer(-meta_respondent_id, names_to = "Variable", values_to = "Cleaned_Value")

# Generate the Differentiated Audit Trail
audit_trail <- inner_join(raw_long, clean_long, by = c("meta_respondent_id", "Variable")) %>%
  filter(Original_Value != Cleaned_Value | (is.na(Original_Value) != is.na(Cleaned_Value))) %>%
  mutate(
    Change_Type = case_when(
      is.na(Original_Value) & !is.na(Cleaned_Value) ~ "HOLE PATCHED (Script 03: Missing data resolved)",
      !is.na(Original_Value) & is.na(Cleaned_Value) ~ "LOGIC PURGE (Script 03: Ghost/Invalid response removed)",
      
      # LOGIC: If the result is a number (0-9) but started as text, it's a Recode (Script 04)
      # Otherwise, it's a Standardization fix (Script 02/03)
      str_detect(Cleaned_Value, "^[0-9]+$") & !str_detect(Original_Value, "^[0-9]+$") ~ "RECODED (Script 04: Mapped string to numeric value label)",
      
      TRUE ~ "STANDARDIZED (Script 02: Typo fix / Case correction / Whitespace)"
    ),
    Audit_Date = Sys.Date()
  ) %>%
  arrange(meta_respondent_id, Variable)

# --- 4. GENERATE THE DATA DICTIONARY ---
cat("--- 4. Generating Technical Data Dictionary ---\n")

get_value_codes <- function(x) {
  if (is.factor(x)) {
    paste(levels(x), collapse = " | ")
  } else if (!is.null(attr(x, "labels"))) {
    lbls <- attr(x, "labels")
    paste(names(lbls), lbls, sep = "=", collapse = " | ")
  } else {
    "Numeric/Continuous"
  }
}

data_dictionary <- tibble(
  Variable = names(final_df),
  Label = unname(map_chr(final_df, ~ {
    lbl <- var_label(.x)
    if(is.null(lbl)) "" else as.character(lbl)
  }))
) %>%
  rowwise() %>%
  mutate(
    Data_Type   = class(final_df[[Variable]])[1],
    Value_Codes = get_value_codes(final_df[[Variable]]),
    Universe = case_when(
      str_detect(Variable, "fever|med_|child_took") ~ "Children < 5 with recent fever",
      str_detect(Variable, "anc|iptp")              ~ "Women 15-49 with a live birth in last 2 years",
      str_detect(Variable, "^hh_|^prev_|^derived_water|^derived_sanitation|^derived_hh") ~ "Household Unit",
      TRUE ~ "All Women Age 15-49"
    ),
    Calculation_Note = case_when(
      str_starts(Variable, "derived_") ~ "Calculated Indicator. Enforces strict NA logic.",
      str_starts(Variable, "flag_") ~ "Analytical Denominator Flag (0/1).",
      TRUE ~ "Standard Survey Variable."
    )
  ) %>%
  ungroup()

# --- 5. TECHNICAL NOTES ---
tech_notes <- tibble(
  `Area` = c("Sampling", "Weighting", "Denominators"),
  `Rationale` = c(
    "Non-probability pharmacy intercept survey.",
    "Unweighted due to extreme urban/educated skew (DEff > 2.0).",
    "Used strict NA logic to prevent denominator contamination."
  )
)

# --- 6. EXPORT (The "Formula-Trigger" Scrubber) ---
cat("--- 5. Scrubbing and Exporting to Excel ---\n")

# Aggressive scrubber to fix the Excel Repair Error
clean_for_excel <- function(df) {
  df %>% mutate(across(where(is.character), ~ {
    x <- iconv(., to = "UTF-8", sub = "") # Fix encoding
    x <- str_replace_all(x, "[[:cntrl:]]", "") # Remove invisible controls
    # NEW FIX: If cell starts with formula triggers (=, +, -, @), add a space
    # This is a major cause of "Repair" errors in openxlsx
    x <- ifelse(str_detect(x, "^[=+@\\-]"), paste0(" ", x), x)
    x <- str_sub(x, 1, 32000) # Cell length limit
    return(x)
  }))
}

# Apply scrubber to ALL dataframes
data_dictionary <- clean_for_excel(data_dictionary)
audit_trail     <- clean_for_excel(audit_trail)
renaming_log    <- clean_for_excel(renaming_log)
tech_notes      <- clean_for_excel(tech_notes)

# Create Workbook
wb <- createWorkbook()
header_style <- createStyle(fgFill = "#1F4E78", fontColour = "white", textDecoration = "bold", halign = "center")

addWorksheet(wb, "1. Dictionary"); writeData(wb, 1, data_dictionary)
addWorksheet(wb, "2. Renaming Log"); writeData(wb, 2, renaming_log)
addWorksheet(wb, "3. Cleaning Audit"); writeData(wb, 3, audit_trail)
addWorksheet(wb, "4. Tech Notes"); writeData(wb, 4, tech_notes)

# Final Save
DOC_NAME <- paste0("Sproxil_Malaria_Full_Documentation_", format(Sys.Date(), "%d%b%Y"), ".xlsx")
saveWorkbook(wb, DOC_NAME, overwrite = TRUE)

cat(sprintf("✅ SUCCESS: Documentation saved as '%s'\n", DOC_NAME))