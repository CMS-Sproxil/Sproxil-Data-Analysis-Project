# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  07_Master_Project_Documentation_ClientFriendly.R
# PURPOSE: Client-friendly documentation workbook + full audit trail
# OUTPUT:  Sproxil_Survey_Documentation.xlsx + Sproxil_Full_Cleaning_Log.csv
# SHEETS:  A. Summary | B. Dictionary | B. Value Labels (optional)
#          C. Cleaning Summary | D. Cleaning Log (chunked)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
  library(labelled)
  library(readxl)
  library(haven)
  library(scales)
  library(readr)
})

# --- 1. LOAD DATASETS ---
cat("--- 1. Loading Datasets ---\n")

RAW_FILE       <- "Sproxil_Survey_Data_030226.xlsx"
PREPARED_FILE  <- "Sproxil_Prepared.rds"
CLEANED_FILE   <- "Sproxil_Cleaned_Final.rds"
ANALYSIS_FILE  <- "Sproxil_Analysis_Ready.rds"
METADATA       <- "Sproxil_dictionary.rds"
RES_LOG_FILE   <- "Sproxil_Resolution_Log.csv"   # optional for Dictionary only

raw_df     <- read_excel(RAW_FILE, sheet = "Survey Data", col_types = "text")
df_prepared <- readRDS(PREPARED_FILE)
df_cleaned  <- readRDS(CLEANED_FILE)
df_analysis <- readRDS(ANALYSIS_FILE)
meta       <- readRDS(METADATA)

res_log <- NULL
if (file.exists(RES_LOG_FILE)) {
  res_log <- read_csv(RES_LOG_FILE, show_col_types = FALSE)
}

# -----------------------------
# Helpers
# -----------------------------
clean_txt <- function(x) str_squish(gsub("\u00A0", " ", as.character(x)))

strip_nul <- function(x) {
  x <- as.character(x)
  iconv(x, from = "", to = "UTF-8", sub = "")
}

clean_controls_keep_newlines <- function(x) {
  x <- strip_nul(x)
  gsub("[\x01-\x08\x0B\x0C\x0E-\x1F]", " ", x, useBytes = TRUE)
}

is_blankish <- function(x) {
  is.na(x) | clean_txt(x) == ""
}

is_numeric_like <- function(x) {
  # allow integers/decimals, optional leading - sign
  !is_blankish(x) & str_detect(clean_txt(x), "^-?[0-9]+(\\.[0-9]+)?$")
}

# Value code formatter (dictionary)
get_value_codes <- function(x) {
  if (is.factor(x)) {
    paste(levels(x), collapse = " | ")
  } else if (!is.null(attr(x, "labels"))) {
    lbls <- attr(x, "labels")
    paste(names(lbls), lbls, sep = "=", collapse = " | ")
  } else {
    ""
  }
}

# Excel chunk writer (no truncation)
EXCEL_MAX_ROWS <- 1048576
CHUNK_SIZE <- EXCEL_MAX_ROWS - 1

add_log_sheets <- function(wb, df_log, base_name = "D. Cleaning Log") {
  n <- nrow(df_log)
  
  if (n == 0) {
    addWorksheet(wb, base_name)
    writeData(wb, base_name, df_log)
    return(invisible(NULL))
  }
  
  n_chunks <- ceiling(n / CHUNK_SIZE)
  
  for (k in seq_len(n_chunks)) {
    nm <- if (n_chunks == 1) base_name else paste0(base_name, " (", k, ")")
    addWorksheet(wb, nm)
    i1 <- (k - 1) * CHUNK_SIZE + 1
    i2 <- min(k * CHUNK_SIZE, n)
    writeData(wb, nm, df_log[i1:i2, , drop = FALSE])
  }
}

# ------------------------------------------------------------------------------
# Dictionary field classification (as previously agreed)
# ------------------------------------------------------------------------------
multi_select_cols <- c(
  "demo_edu_informal", "prev_repellent_methods", "women_anc_provider",
  "women_anc_location", "women_sp_fansidar_source", "women_child_advice_location",
  "women_child_medicine_type", "bg_languages", "bg_malaria_msg_source", "bg_prevention_knowledge"
)

datetime_cols <- c("meta_date_created", "meta_response_date")  # all Datetime
free_text_cols <- c("demo_town")                              # only free text

is_derived_like <- function(v) {
  str_detect(v, "^(derived_|u_|.*_num$|wealth_q$|zone_num$|residence_num$)")
}

spatial_derived_vars <- c(
  "detected_state", "detected_lga", "gps_state_match", "gps_lga_match",
  "lga_dist_error_km", "urban_status"  # urban treated as N/A
)

field_class <- function(v, stored_type) {
  if (v %in% free_text_cols) return("Free text")
  if (v %in% multi_select_cols) return("Multi-select")
  if (v %in% datetime_cols) return("Datetime")
  if (v %in% c("meta_latitude", "meta_longitude")) return("Geo-coordinate")
  if (stored_type %in% c("numeric", "integer")) return("Numeric (continuous)")
  if (stored_type %in% c("character")) return("Categorical / Metadata (text)")
  stored_type
}

value_format <- function(v, stored_type, value_codes_from_labels) {
  if (!is.na(value_codes_from_labels) && value_codes_from_labels != "") return(value_codes_from_labels)
  if (v %in% free_text_cols) return("Free text")
  if (v %in% multi_select_cols) return("Multi-select")
  if (v %in% datetime_cols) return("Datetime")
  if (v %in% c("meta_latitude", "meta_longitude")) return("Decimal degrees")
  if (stored_type %in% c("numeric", "integer")) return("Numeric/Continuous")
  "Text"
}

# Exclusion from holes/ghosts columns in Dictionary
is_excluded_var <- function(v, original_raw_name) {
  is_derived_like(v) ||
    v %in% spatial_derived_vars ||
    identical(original_raw_name, "N/A")
}

# -----------------------------
# 2. PREPARE DATASETS FOR LOGGING (standardise to comparable character forms)
# -----------------------------
cat("--- 2. Preparing datasets for comparisons ---\n")

# Raw Excel renamed to internal variable names
raw_renamed <- raw_df %>%
  rename(any_of(meta$column_mapping)) %>%
  mutate(across(everything(), ~ clean_txt(.)))

# Strip/prep RDS dataframes to character (for comparisons)
prepared_stripped <- df_prepared %>%
  mutate(across(everything(), ~ clean_txt(as.character(zap_labels(.)))))

cleaned_stripped <- df_cleaned %>%
  mutate(across(everything(), ~ clean_txt(as.character(zap_labels(.)))))

analysis_stripped <- df_analysis %>%
  mutate(across(everything(), ~ clean_txt(as.character(zap_labels(.)))))

# Guard: ID required
for (d in list(raw_renamed = raw_renamed, prepared_stripped = prepared_stripped,
               cleaned_stripped = cleaned_stripped, analysis_stripped = analysis_stripped)) {
  if (!"meta_respondent_id" %in% names(d)) {
    stop("meta_respondent_id missing in one of the datasets. Ensure mapping + RDS outputs include meta_respondent_id.")
  }
}

# -----------------------------
# 3. BUILD CLEANING LOGS (three-stage, then bind)
#   A) EXCLUSIONS: raw (all) -> cleaned (completed-only)
#   B) VALIDATION: prepared -> cleaned (flag holes + purge ghosts + standardise)
#   C) RECODE: cleaned -> analysis (recoded to numeric + other changes)
# -----------------------------
cat("--- 3. Building Cleaning Logs (Exclusions, Validation, Recode) ---\n")

# Helper to make long df
to_long <- function(df, id = "meta_respondent_id", cols) {
  df %>%
    select(all_of(c(id, cols))) %>%
    pivot_longer(-all_of(id), names_to = "Variable", values_to = "Value")
}

# Common columns for each comparison
cols_raw_cleaned <- intersect(names(raw_renamed), names(cleaned_stripped))
cols_prepared_cleaned <- intersect(names(prepared_stripped), names(cleaned_stripped))
cols_cleaned_analysis <- intersect(names(cleaned_stripped), names(analysis_stripped))

cols_raw_cleaned <- setdiff(cols_raw_cleaned, "meta_respondent_id")
cols_prepared_cleaned <- setdiff(cols_prepared_cleaned, "meta_respondent_id")
cols_cleaned_analysis <- setdiff(cols_cleaned_analysis, "meta_respondent_id")


# -----------------------------
# 3B) VALIDATION LOG (prepared -> cleaned)
# -----------------------------
prepared_long <- to_long(prepared_stripped, cols = cols_prepared_cleaned) %>%
  rename(Original = Value)

cleaned_long_pc <- to_long(cleaned_stripped, cols = cols_prepared_cleaned) %>%
  rename(Cleaned = Value)

validation_log <- full_join(prepared_long, cleaned_long_pc,
                            by = c("meta_respondent_id", "Variable")) %>%
  mutate(
    Original = clean_controls_keep_newlines(Original),
    Cleaned  = clean_controls_keep_newlines(Cleaned),
    Original_norm = toupper(clean_txt(Original)),
    Cleaned_norm  = toupper(clean_txt(Cleaned)),
    Record_Status = case_when(
      is.na(Original) & !is.na(Cleaned) ~ "Present in cleaned only",
      !is.na(Original) & is.na(Cleaned) ~ "Present in prepared only",
      TRUE ~ "Present in both"
    )
  ) %>%
  filter(
    (Original_norm != Cleaned_norm) |
      (is_blankish(Original) != is_blankish(Cleaned))
  ) %>%
  mutate(
    Change_Type = case_when(
      # Flag holes: missing/blank -> NO RESPONSE
      (is_blankish(Original) & !is_blankish(Cleaned) & toupper(clean_txt(Cleaned)) == "NO RESPONSE") ~
        "Flagged missing response",
      
      # True fill (imputation placeholder): missing/blank -> some other value
      (is_blankish(Original) & !is_blankish(Cleaned) & toupper(clean_txt(Cleaned)) != "NO RESPONSE") ~
        "Filled missing response using Imputation",
      
      # Purge: present -> blank/NA
      (!is_blankish(Original) & is_blankish(Cleaned)) ~
        "Implausible value purged to blank during validation (Logic violation)",
      
      # Default: standardisation / correction
      TRUE ~ "Purged mismatched entries"
    ),
    Audit_Date = Sys.Date()
  ) %>%
  select(meta_respondent_id, Variable, Original, Cleaned, Record_Status, Change_Type, Audit_Date)

# -----------------------------
# 3C) RECODE LOG (cleaned -> analysis)
# -----------------------------
cat("   - Building recode log (Cleaned -> Analysis Ready) ...\n")

common_vars_recode <- setdiff(intersect(names(cleaned_stripped), names(analysis_stripped)), "meta_respondent_id")

cleaned_long_ca <- to_long(cleaned_stripped, cols = common_vars_recode) %>%
  rename(Original = Value)

analysis_long_ca <- to_long(analysis_stripped, cols = common_vars_recode) %>%
  rename(Cleaned = Value)

recode_log <- full_join(cleaned_long_ca, analysis_long_ca,
                        by = c("meta_respondent_id", "Variable")) %>%
  mutate(
    Original = clean_controls_keep_newlines(Original),
    Cleaned  = clean_controls_keep_newlines(Cleaned),
    Original_norm = toupper(clean_txt(Original)),
    Cleaned_norm  = toupper(clean_txt(Cleaned))
  ) %>%
  # focus ONLY on numeric recodes: non-numeric -> numeric
  filter((!is_numeric_like(Original)) & is_numeric_like(Cleaned)) %>%
  mutate(
    Record_Status = "Present in both",
    Change_Type   = "Recoded to numeric category",
    Audit_Date    = Sys.Date()
  ) %>%
  select(meta_respondent_id, Variable, Original, Cleaned, Record_Status, Change_Type, Audit_Date)

n_recoded_total <- nrow(recode_log)

# -----------------------------
# Combine logs
# -----------------------------
cleaning_log <- bind_rows(
  validation_log,
  recode_log
) %>%
  arrange(Change_Type, Variable, meta_respondent_id)

write_csv(cleaning_log, "Sproxil_Full_Cleaning_Log.csv")

# -----------------------------
# 4. DICTIONARY (based on CLEANED FINAL; holes/ghost columns from resolution log)
# -----------------------------
cat("--- 4. Building Dictionary ---\n")

final_df_for_dict <- df_analysis

data_dictionary <- tibble(
  Variable = names(final_df_for_dict),
  Original_Raw_Name = unname(meta$column_mapping[names(final_df_for_dict)]),
  Label = unname(map_chr(final_df_for_dict, ~ {
    lbl <- var_label(.x)
    if (is.null(lbl)) "" else as.character(lbl)
  }))
) %>%
  mutate(
    Original_Raw_Name = ifelse(Variable %in% names(meta$column_mapping), Original_Raw_Name, "N/A"),
    Stored_Data_Type = map_chr(Variable, ~ class(zap_labels(final_df_for_dict[[.x]]))[1]),
    Value_Codes_Raw = map_chr(Variable, ~ get_value_codes(final_df_for_dict[[.x]])),
    Field_Class = map2_chr(Variable, Stored_Data_Type, field_class),
    Value_Codes = pmap_chr(list(Variable, Stored_Data_Type, Value_Codes_Raw), ~ value_format(..1, ..2, ..3))
  )

# Holes/Ghosts from resolution log only (dictionary columns stay, excluded vars show "N/A")
holes_fixed_tbl <- tibble()
ghosts_purged_tbl <- tibble()

if (!is.null(res_log) && nrow(res_log) > 0 && all(c("Variable", "Resolution_Type") %in% names(res_log))) {
  holes_fixed_tbl <- res_log %>%
    filter(str_detect(Resolution_Type, "^HOLE:")) %>%
    count(Variable, name = "Holes_Fixed_N")
  
  ghosts_purged_tbl <- res_log %>%
    filter(str_detect(Resolution_Type, "^GHOST:")) %>%
    count(Variable, name = "Ghosts_Purged_N")
}

qc_counts <- tibble(Variable = names(final_df_for_dict)) %>%
  left_join(holes_fixed_tbl, by = "Variable") %>%
  left_join(ghosts_purged_tbl, by = "Variable") %>%
  mutate(
    Holes_Fixed_N = replace_na(Holes_Fixed_N, 0L),
    Ghosts_Purged_N = replace_na(Ghosts_Purged_N, 0L)
  )

data_dictionary <- data_dictionary %>%
  left_join(qc_counts, by = "Variable") %>%
  mutate(
    Exclude_QC = map2_lgl(Variable, Original_Raw_Name, is_excluded_var),
    Holes_Fixed = ifelse(Exclude_QC, "N/A", as.character(Holes_Fixed_N)),
    Ghosts_Purged = ifelse(Exclude_QC, "N/A", as.character(Ghosts_Purged_N))
  ) %>%
  select(
    Variable, Original_Raw_Name, Label,
    Stored_Data_Type, Field_Class, Value_Codes,
    Holes_Fixed, Ghosts_Purged
  ) %>%
  arrange(Variable)

# Optional: Value labels
value_labels_long <- map_dfr(names(final_df_for_dict), function(v) {
  lbls <- attr(final_df_for_dict[[v]], "labels")
  if (is.null(lbls)) return(NULL)
  tibble(Variable = v, Value = as.numeric(lbls), Label = names(lbls)) %>%
    arrange(Value)
})

# -----------------------------
# 5. CLEANING SUMMARY
# -----------------------------
cat("--- 5. Building Cleaning Summary ---\n")

n_raw <- nrow(raw_df)
n_raw_completed <- raw_df %>% filter(toupper(str_squish(status)) == "COMPLETED") %>% nrow()
n_final <- nrow(df_cleaned)

# Changes by type (from combined logs)
# --- Changes by type from (Exclusions + Validation) ---
change_type_levels <- c(
  "Excluded record (non-completed / out-of-scope)",
  "Flagged missing response",
  "Filled missing response using Imputation",
  "Implausible value purged to blank during validation (Logic violation)",
  "Purged mismatched entries",
  "Recoded to numeric category"
)

# Exclusions should be counted as ROWS (respondents), not value-level entries
n_excluded_rows <- length(excluded_ids)

# Validation changes should remain value-level (entries)
cleaning_by_type_base <- cleaning_log %>%
  filter(Change_Type != "Excluded record (non-completed / out-of-scope)") %>%
  count(Change_Type, name = "N")

# --- Cleaning changes by type (A. Summary): keep ONLY true cleaning actions ---
change_type_levels_A <- c(
  "Filled missing response using Imputation",
  "Implausible value purged to blank during validation (Logic violation)",
  "Purged mismatched entries",
  "Recoded to numeric category"
)

cleaning_by_type <- cleaning_log %>%
  # keep: recode + true cleaning actions; drop only exclusions/flagging if you still want
  filter(!Change_Type %in% c(
    "Excluded record (non-completed / out-of-scope)",
    "Flagged missing response"
  )) %>%
  count(Change_Type, name = "N") %>%
  complete(Change_Type = change_type_levels_A, fill = list(N = 0)) %>%
  mutate(Pct = percent(N / sum(N))) %>%
  arrange(match(Change_Type, change_type_levels_A))

# Changes by variable and type
cleaning_by_variable <- cleaning_log %>%
  filter(Change_Type != "Excluded record (non-completed / out-of-scope)") %>%
  count(Variable, Change_Type, name="N") %>%
  group_by(Variable) %>%
  mutate(
    Pct_within_variable = percent(N / sum(N)),
    Pct_of_records      = percent(N / n_final)
  ) %>%
  ungroup() %>%
  arrange(desc(N))

# -----------------------------
# 6. SUMMARY SHEET TABLES (client-facing)
# -----------------------------
cat("--- 6. Building Executive Summary ---\n")

n_raw <- nrow(raw_df)
n_raw_completed <- raw_df %>% filter(toupper(str_squish(status)) == "COMPLETED") %>% nrow()
n_final <- nrow(df_cleaned)

n_changes <- nrow(cleaning_log)
n_ids_raw <- raw_renamed %>% summarise(n = n_distinct(meta_respondent_id)) %>% pull(n)
n_ids_final <- cleaned_stripped %>% summarise(n = n_distinct(meta_respondent_id)) %>% pull(n)

summary_kpis <- tibble(
  Metric = c(
    "Raw records (all)",
    "Raw records (completed)",
    "Final records (cleaned/validated)",
    "Distinct respondent IDs (raw)",
    "Distinct respondent IDs (final)",
    "Total value-level changes logged (cleaning only)"
  ),
  Value = c(n_raw, n_raw_completed, n_final, n_ids_raw, n_ids_final,
            nrow(cleaning_log),
            n_recoded_total)
)

# Top variables by MISSING VALUES IMPUTED (not NO RESPONSE flags)
top_imputed <- cleaning_log %>%
  filter(Change_Type == "Filled missing response using Imputation") %>%
  count(Variable, name = "N_Imputed") %>%
  mutate(Pct_Imputed = percent(N_Imputed / n_final)) %>%
  left_join(data_dictionary %>% select(Variable, Label), by = "Variable") %>%
  select(Variable, Label, N_Imputed, Pct_Imputed) %>%
  arrange(desc(N_Imputed)) %>%
  slice_head(n = 25)

# Top variables by PURGED to NA in validation (prepared -> cleaned)
top_purged <- cleaning_log %>%
  filter(Change_Type == "Implausible value purged to blank during validation (Logic violation)") %>%
  count(Variable, name = "N_Purged") %>%
  mutate(Pct_Purged = percent(N_Purged / n_final)) %>%
  left_join(data_dictionary %>% select(Variable, Label), by = "Variable") %>%
  select(Variable, Label, N_Purged, Pct_Purged) %>%
  arrange(desc(N_Purged)) %>%
  slice_head(n = 25)

# -----------------------------
# 7. EXPORT WORKBOOK
# -----------------------------
cat("--- 7. Writing Client-Friendly Excel Documentation ---\n")

wb <- createWorkbook()

header_style <- createStyle(fgFill = "#1F4E78", fontColour = "white", textDecoration = "bold")
subhdr_style <- createStyle(textDecoration = "bold")
wrap_style   <- createStyle(wrapText = TRUE)

# A. Summary
addWorksheet(wb, "A. Summary")
writeData(wb, "A. Summary", "Sproxil Survey Documentation (Client Summary)", startRow = 1, startCol = 1)
addStyle(wb, "A. Summary", subhdr_style, rows = 1, cols = 1, stack = TRUE)

writeData(wb, "A. Summary", "Key counts", startRow = 3, startCol = 1)
addStyle(wb, "A. Summary", subhdr_style, rows = 3, cols = 1, stack = TRUE)
writeData(wb, "A. Summary", summary_kpis, startRow = 4, startCol = 1)
addStyle(wb, "A. Summary", header_style, rows = 4, cols = 1:ncol(summary_kpis), gridExpand = TRUE, stack = TRUE)

writeData(wb, "A. Summary", "Cleaning changes by type", startRow = 12, startCol = 1)
addStyle(wb, "A. Summary", subhdr_style, rows = 12, cols = 1, stack = TRUE)
writeData(wb, "A. Summary", cleaning_by_type, startRow = 13, startCol = 1)
addStyle(wb, "A. Summary", header_style, rows = 13, cols = 1:ncol(cleaning_by_type), gridExpand = TRUE, stack = TRUE)

writeData(wb, "A. Summary", "Top variables by missing values imputed", startRow = 20, startCol = 1)
addStyle(wb, "A. Summary", subhdr_style, rows = 20, cols = 1, stack = TRUE)
writeData(wb, "A. Summary", top_imputed, startRow = 21, startCol = 1)
addStyle(wb, "A. Summary", header_style, rows = 21, cols = 1:ncol(top_imputed), gridExpand = TRUE, stack = TRUE)

writeData(wb, "A. Summary", "Top variables by implausible values purged (set to NA)", startRow = 40, startCol = 1)
addStyle(wb, "A. Summary", subhdr_style, rows = 40, cols = 1, stack = TRUE)
writeData(wb, "A. Summary", top_purged, startRow = 41, startCol = 1)
addStyle(wb, "A. Summary", header_style, rows = 41, cols = 1:ncol(top_purged), gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "A. Summary", cols = 1:6, widths = c(34, 55, 18, 18, 18, 18))

# B. Dictionary
addWorksheet(wb, "B. Dictionary")

dict_out <- data_dictionary %>%
  transmute(
    `Variable Name` = Variable,
    `Original Column Name` = Original_Raw_Name,
    `Description` = Label,
    `Stored Data Type` = Stored_Data_Type,
    `Field Class` = Field_Class,
    `Value Codes / Format` = Value_Codes,
    `Holes Fixed (N)` = Holes_Fixed,
    `Ghosts Purged (N)` = Ghosts_Purged
  )

writeData(wb, "B. Dictionary", dict_out)
addStyle(wb, "B. Dictionary", header_style, rows = 1, cols = 1:ncol(dict_out), gridExpand = TRUE)
setColWidths(wb, "B. Dictionary", cols = 1:ncol(dict_out),
             widths = c(28, 30, 55, 18, 20, 70, 16, 16))
addStyle(wb, "B. Dictionary", wrap_style, rows = 1:(nrow(dict_out)+1), cols = 3, gridExpand = TRUE)

# Optional: Value labels sheet
if (!is.null(value_labels_long) && nrow(value_labels_long) > 0) {
  addWorksheet(wb, "B. Value Labels")
  writeData(wb, "B. Value Labels", value_labels_long)
  addStyle(wb, "B. Value Labels", header_style, rows = 1, cols = 1:ncol(value_labels_long), gridExpand = TRUE)
  setColWidths(wb, "B. Value Labels", cols = 1:3, widths = c(28, 12, 70))
}

# C. Cleaning Summary
addWorksheet(wb, "C. Cleaning Summary")

writeData(wb, "C. Cleaning Summary", "Changes by variable and change type", startRow = 1, startCol = 1)
addStyle(wb, "C. Cleaning Summary", subhdr_style, rows = 1, cols = 1, stack = TRUE)
writeData(wb, "C. Cleaning Summary", cleaning_by_variable, startRow = 2, startCol = 1)
addStyle(wb, "C. Cleaning Summary", header_style, rows = 2, cols = 1:ncol(cleaning_by_variable), gridExpand = TRUE)
setColWidths(wb, "C. Cleaning Summary", cols = 1:4, widths = c(32, 40, 16, 18))

# D. Cleaning Log (chunked)
add_log_sheets(wb, cleaning_log, base_name = "D. Cleaning Log")

log_sheets <- names(wb)[grepl("^D\\. Cleaning Log", names(wb))]
for (sh in log_sheets) {
  addStyle(wb, sh, header_style, rows = 1, cols = 1:ncol(cleaning_log), gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:ncol(cleaning_log), widths = c(18, 30, 30, 30, 34, 40, 14))
  addStyle(wb, sh, wrap_style, rows = 1:min(nrow(cleaning_log)+1, 5000), cols = 3:4, gridExpand = TRUE)
}

saveWorkbook(wb, "Sproxil_Survey_Documentation.xlsx", overwrite = TRUE)

cat("✅ SUCCESS: Client documentation saved to Sproxil_Survey_Documentation.xlsx\n")
cat("✅ Full cleaning log saved to Sproxil_Full_Cleaning_Log.csv\n")