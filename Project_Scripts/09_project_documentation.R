# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  07_project_documentation.R
# AUTHOR:  Corona Management Systems
# DATE:    23 January 2026
#
# PURPOSE:
# Creates a client-friendly documentation workbook and a full value-level data
# change log for the Sproxil survey pipeline.
#
# This script:
# 1. loads the raw, prepared, cleaned, and analysis-ready datasets,
# 2. standardises comparison fields across pipeline stages,
# 3. identifies exclusions used for KPI reporting,
# 4. builds value-level validation and recode change logs,
# 5. reapplies variable labels to the analysis-ready dataset,
# 6. builds a final data dictionary from the analysis-ready file,
# 7. prepares summary tables for client review, and
# 8. exports the workbook and full change log.
#
# OUTPUT:
#   - Sproxil_Survey_Documentation.xlsx
#   - Sproxil_Full_Change_Log.csv
#
# WORKBOOK SHEETS:
#   - A. Summary
#   - B. Dictionary
#   - B. Value Labels
#   - C. Change Summary
#
# REPLICATION NOTE:
# The dictionary is built from the analysis-ready dataset so that final and
# derived variables are captured. KPI exclusions are not included in the
# value-level change log. NO RESPONSE flags are excluded from the change log and
# summary tables. The change log captures validation-stage repairs and same-
# variable recoding changes, but excludes text harmonisation and new derived
# variable creation.
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

# ------------------------------------------------------------------------------
# 1. File paths
# ------------------------------------------------------------------------------
cat("--- 1. Loading files ---\n")

S07_RAW_FILE      <- "Sproxil_Survey_Data_030226.xlsx"
S07_PREPARED_FILE <- "Sproxil_Prepared.rds"
S07_CLEANED_FILE  <- "Sproxil_Cleaned_Final.rds"
S07_ANALYSIS_FILE <- "Sproxil_Analysis_Ready.rds"
S07_METADATA_FILE <- "Sproxil_dictionary.rds"
S07_RES_LOG_FILE  <- "Sproxil_Resolution_Log.csv"
S07_OUT_XLSX      <- "Sproxil_Survey_Documentation.xlsx"
S07_OUT_LOG_CSV   <- "Sproxil_Full_Change_Log.csv"

s07_required_files <- c(
  S07_RAW_FILE,
  S07_PREPARED_FILE,
  S07_CLEANED_FILE,
  S07_ANALYSIS_FILE,
  S07_METADATA_FILE
)

s07_missing_files <- s07_required_files[!file.exists(s07_required_files)]

if (length(s07_missing_files) > 0) {
  stop("Missing required file(s): ", paste(s07_missing_files, collapse = ", "))
}

# ------------------------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------------------------
s07_clean_txt <- function(x) {
  stringr::str_squish(gsub("\u00A0", " ", as.character(x)))
}

s07_strip_nul <- function(x) {
  x <- as.character(x)
  iconv(x, from = "", to = "UTF-8", sub = "")
}

s07_clean_controls_keep_newlines <- function(x) {
  x <- s07_strip_nul(x)
  gsub("[\x01-\x08\x0B\x0C\x0E-\x1F]", " ", x, useBytes = TRUE)
}

s07_is_blankish <- function(x) {
  is.na(x) | s07_clean_txt(x) == ""
}

s07_is_numeric_like <- function(x) {
  !s07_is_blankish(x) & stringr::str_detect(s07_clean_txt(x), "^-?[0-9]+(\\.[0-9]+)?$")
}

s07_is_text_like <- function(x) {
  !s07_is_blankish(x) & !s07_is_numeric_like(x)
}

s07_norm_txt <- function(x) {
  toupper(s07_clean_txt(x))
}

s07_get_value_codes <- function(x) {
  if (is.factor(x)) {
    paste(levels(x), collapse = " | ")
  } else if (!is.null(attr(x, "labels"))) {
    lbls <- attr(x, "labels")
    paste(names(lbls), lbls, sep = "=", collapse = " | ")
  } else {
    ""
  }
}

# ------------------------------------------------------------------------------
# 3. Dictionary field classification
# ------------------------------------------------------------------------------
s07_multi_select_cols <- c(
  "demo_edu_informal", "prev_repellent_methods", "women_anc_provider",
  "women_anc_location", "women_sp_fansidar_source", "women_child_advice_location",
  "women_child_medicine_type", "bg_languages", "bg_malaria_msg_source", "bg_prevention_knowledge"
)

s07_datetime_cols <- c("meta_date_created", "meta_response_date")
s07_free_text_cols <- c("demo_town")

s07_is_derived_like <- function(v) {
  stringr::str_detect(v, "^(derived_|u_|.*_num$|wealth_q$|zone_num$|residence_num$)")
}

s07_spatial_derived_vars <- c(
  "detected_state", "detected_lga", "gps_state_match", "gps_lga_match",
  "lga_dist_error_km", "urban_status"
)

s07_field_class <- function(v, stored_type) {
  if (v %in% s07_free_text_cols) return("Free text")
  if (v %in% s07_multi_select_cols) return("Multi-select")
  if (v %in% s07_datetime_cols) return("Datetime")
  if (v %in% c("meta_latitude", "meta_longitude")) return("Geo-coordinate")
  if (stored_type %in% c("numeric", "integer")) return("Numeric (continuous)")
  if (stored_type %in% c("character")) return("Categorical / Metadata (text)")
  stored_type
}

s07_value_format <- function(v, stored_type, value_codes_from_labels) {
  if (!is.na(value_codes_from_labels) && value_codes_from_labels != "") return(value_codes_from_labels)
  if (v %in% s07_free_text_cols) return("Free text")
  if (v %in% s07_multi_select_cols) return("Multi-select")
  if (v %in% s07_datetime_cols) return("Datetime")
  if (v %in% c("meta_latitude", "meta_longitude")) return("Decimal degrees")
  if (stored_type %in% c("numeric", "integer")) return("Numeric/Continuous")
  "Text"
}

s07_is_excluded_var <- function(v, original_raw_name) {
  s07_is_derived_like(v) ||
    v %in% s07_spatial_derived_vars ||
    identical(original_raw_name, "N/A")
}

# ------------------------------------------------------------------------------
# 4. Load data
# ------------------------------------------------------------------------------
s07_raw_df <- read_excel(S07_RAW_FILE, sheet = "Survey Data", col_types = "text")
s07_df_prepared <- readRDS(S07_PREPARED_FILE)
s07_df_cleaned  <- readRDS(S07_CLEANED_FILE)
s07_df_analysis <- readRDS(S07_ANALYSIS_FILE)
s07_meta <- readRDS(S07_METADATA_FILE)

s07_res_log <- NULL
if (file.exists(S07_RES_LOG_FILE)) {
  s07_res_log <- read_csv(S07_RES_LOG_FILE, show_col_types = FALSE)
}

if (!"column_mapping" %in% names(s07_meta)) {
  stop("Metadata object does not contain 'column_mapping'.")
}
if (is.null(names(s07_meta$column_mapping))) {
  stop("s07_meta$column_mapping must be a named vector: names = internal variable names, values = raw column names.")
}

# ------------------------------------------------------------------------------
# 5. Standardise data for comparison
# ------------------------------------------------------------------------------
cat("--- 2. Preparing datasets for comparisons ---\n")

s07_raw_renamed <- s07_raw_df %>%
  rename(any_of(s07_meta$column_mapping)) %>%
  mutate(across(everything(), ~ s07_clean_txt(.)))

s07_prepared_stripped <- s07_df_prepared %>%
  mutate(across(everything(), ~ s07_clean_txt(as.character(zap_labels(.)))))

s07_cleaned_stripped <- s07_df_cleaned %>%
  mutate(across(everything(), ~ s07_clean_txt(as.character(zap_labels(.)))))

s07_analysis_stripped <- s07_df_analysis %>%
  mutate(across(everything(), ~ s07_clean_txt(as.character(zap_labels(.)))))

S07_REQUIRED_ID <- "meta_respondent_id"

for (nm in c("s07_raw_renamed", "s07_prepared_stripped", "s07_cleaned_stripped", "s07_analysis_stripped")) {
  obj <- get(nm)
  if (!S07_REQUIRED_ID %in% names(obj)) {
    stop(S07_REQUIRED_ID, " missing in ", nm, ".")
  }
}

if (!"status" %in% names(s07_raw_df)) {
  stop("'status' column not found in s07_raw_df. Needed for KPI exclusion count.")
}

# ------------------------------------------------------------------------------
# 6. Define exclusions for KPI reporting
# ------------------------------------------------------------------------------
cat("--- 3. Defining exclusions for KPI ---\n")

s07_status_clean <- s07_norm_txt(s07_raw_df$status)

s07_excluded_ids <- s07_raw_df %>%
  mutate(status_clean = s07_norm_txt(status)) %>%
  filter(is.na(status) | status_clean != "COMPLETED") %>%
  pull(respondent_id) %>%
  unique()

s07_n_excluded_rows <- length(s07_excluded_ids)

# ------------------------------------------------------------------------------
# 7. Build value-level data change logs
# ------------------------------------------------------------------------------
cat("--- 4. Building value-level data change logs ---\n")

s07_vars_prepared_cleaned <- setdiff(
  intersect(names(s07_prepared_stripped), names(s07_cleaned_stripped)),
  S07_REQUIRED_ID
)

s07_vars_cleaned_analysis <- setdiff(
  intersect(names(s07_cleaned_stripped), names(s07_analysis_stripped)),
  S07_REQUIRED_ID
)

# ------------------------------------------------------------------------------
# 7A. Validation log: prepared -> cleaned
# ------------------------------------------------------------------------------
s07_build_validation_log_var <- function(v, df_old, df_new, id_col = "meta_respondent_id") {
  x_old <- df_old[[v]]
  x_new <- df_new[[v]]
  
  old_norm <- s07_norm_txt(x_old)
  new_norm <- s07_norm_txt(x_new)
  
  changed <- (old_norm != new_norm) | (s07_is_blankish(x_old) != s07_is_blankish(x_new))
  changed[is.na(changed)] <- TRUE
  
  if (!any(changed)) return(NULL)
  
  out <- tibble(
    meta_respondent_id = df_old[[id_col]][changed],
    Variable = v,
    Original = s07_clean_controls_keep_newlines(x_old[changed]),
    Cleaned  = s07_clean_controls_keep_newlines(x_new[changed])
  ) %>%
    mutate(
      Record_Status = "Present in both",
      Change_Type = case_when(
        (s07_is_blankish(Original) & !s07_is_blankish(Cleaned) & s07_norm_txt(Cleaned) == "NO RESPONSE") ~
          "DROP_NO_RESPONSE_FLAG",
        (s07_is_blankish(Original) & !s07_is_blankish(Cleaned) & s07_norm_txt(Cleaned) != "NO RESPONSE") ~
          "Filled missing response using Imputation",
        (!s07_is_blankish(Original) & s07_is_blankish(Cleaned)) ~
          "Logic violation purged to blank during validation",
        (!s07_is_blankish(Original) & !s07_is_blankish(Cleaned) & s07_norm_txt(Cleaned) == "NO RESPONSE") ~
          "Misaligned response set to missing",
        TRUE ~ "Standardised or corrected response"
      ),
      Audit_Date = Sys.Date()
    ) %>%
    filter(Change_Type %in% c(
      "Filled missing response using Imputation",
      "Logic violation purged to blank during validation",
      "Misaligned response set to missing"
    )) %>%
    select(meta_respondent_id, Variable, Original, Cleaned, Record_Status, Change_Type, Audit_Date)
  
  if (nrow(out) == 0) return(NULL)
  out
}

s07_validation_log_list <- vector("list", length(s07_vars_prepared_cleaned))
for (i in seq_along(s07_vars_prepared_cleaned)) {
  s07_validation_log_list[[i]] <- s07_build_validation_log_var(
    s07_vars_prepared_cleaned[i],
    df_old = s07_prepared_stripped,
    df_new = s07_cleaned_stripped,
    id_col = S07_REQUIRED_ID
  )
}
s07_validation_log <- bind_rows(s07_validation_log_list)

# ------------------------------------------------------------------------------
# 7B. Recode log: cleaned -> analysis
# ------------------------------------------------------------------------------
s07_build_recode_log_var <- function(v, df_old, df_new, id_col = "meta_respondent_id") {
  x_old <- df_old[[v]]
  x_new <- df_new[[v]]
  
  old_norm <- s07_norm_txt(x_old)
  new_norm <- s07_norm_txt(x_new)
  
  changed <- (old_norm != new_norm) | (s07_is_blankish(x_old) != s07_is_blankish(x_new))
  changed[is.na(changed)] <- TRUE
  
  if (!any(changed)) return(NULL)
  
  tmp <- tibble(
    meta_respondent_id = df_old[[id_col]][changed],
    Variable = v,
    Original = s07_clean_controls_keep_newlines(x_old[changed]),
    Cleaned  = s07_clean_controls_keep_newlines(x_new[changed])
  ) %>%
    mutate(
      old_is_num = s07_is_numeric_like(Original),
      new_is_num = s07_is_numeric_like(Cleaned),
      old_is_text = s07_is_text_like(Original),
      new_is_text = s07_is_text_like(Cleaned)
    ) %>%
    filter(
      new_is_num,
      !(old_is_text & new_is_text),
      !(s07_is_blankish(Original) & s07_is_blankish(Cleaned))
    ) %>%
    mutate(
      Record_Status = "Present in both",
      Change_Type   = "Recoded to numeric category",
      Audit_Date    = Sys.Date()
    ) %>%
    select(meta_respondent_id, Variable, Original, Cleaned, Record_Status, Change_Type, Audit_Date)
  
  if (nrow(tmp) == 0) return(NULL)
  tmp
}

s07_recode_log_list <- vector("list", length(s07_vars_cleaned_analysis))
for (i in seq_along(s07_vars_cleaned_analysis)) {
  s07_recode_log_list[[i]] <- s07_build_recode_log_var(
    s07_vars_cleaned_analysis[i],
    df_old = s07_cleaned_stripped,
    df_new = s07_analysis_stripped,
    id_col = S07_REQUIRED_ID
  )
}
s07_recode_log <- bind_rows(s07_recode_log_list)

s07_change_log <- bind_rows(
  s07_validation_log,
  s07_recode_log
) %>%
  arrange(Change_Type, Variable, meta_respondent_id)

write_csv(s07_change_log, S07_OUT_LOG_CSV)

# ------------------------------------------------------------------------------
# 7C. Reapply variable labels to the analysis-ready dataset
# ------------------------------------------------------------------------------
cat("--- 4B. Reapplying variable labels to analysis-ready dataset ---\n")

for (v in names(s07_df_analysis)) {
  labelled::var_label(s07_df_analysis[[v]]) <- NULL
}

s07_matched_label_vars <- intersect(names(s07_meta$variable_labels), names(s07_df_analysis))

for (v in s07_matched_label_vars) {
  labelled::var_label(s07_df_analysis[[v]]) <- s07_meta$variable_labels[[v]]
}

cat("   Labels reapplied for ", length(s07_matched_label_vars), " variables.\n", sep = "")

# ------------------------------------------------------------------------------
# 8. Build dictionary from the analysis-ready dataset
# ------------------------------------------------------------------------------
cat("--- 5. Building dictionary ---\n")

s07_final_df_for_dict <- s07_df_analysis

s07_data_dictionary <- tibble(
  Variable = names(s07_final_df_for_dict),
  Original_Raw_Name = unname(s07_meta$column_mapping[names(s07_final_df_for_dict)]),
  Label = unname(map_chr(s07_final_df_for_dict, ~ {
    lbl <- var_label(.x)
    if (is.null(lbl)) "" else as.character(lbl)
  }))
) %>%
  mutate(
    Original_Raw_Name = ifelse(Variable %in% names(s07_meta$column_mapping), Original_Raw_Name, "N/A"),
    Stored_Data_Type = map_chr(Variable, ~ class(zap_labels(s07_final_df_for_dict[[.x]]))[1]),
    Value_Codes_Raw = map_chr(Variable, ~ s07_get_value_codes(s07_final_df_for_dict[[.x]])),
    Field_Class = map2_chr(Variable, Stored_Data_Type, s07_field_class),
    Value_Codes = pmap_chr(list(Variable, Stored_Data_Type, Value_Codes_Raw), ~ s07_value_format(..1, ..2, ..3))
  )

s07_holes_fixed_tbl <- tibble()
s07_ghosts_purged_tbl <- tibble()

if (!is.null(s07_res_log) &&
    nrow(s07_res_log) > 0 &&
    all(c("Variable", "Resolution_Type") %in% names(s07_res_log))) {
  
  s07_holes_fixed_tbl <- s07_res_log %>%
    filter(str_detect(Resolution_Type, "^HOLE:")) %>%
    count(Variable, name = "Holes_Fixed_N")
  
  s07_ghosts_purged_tbl <- s07_res_log %>%
    filter(str_detect(Resolution_Type, "^GHOST:")) %>%
    count(Variable, name = "Ghosts_Purged_N")
}

s07_qc_counts <- tibble(Variable = names(s07_final_df_for_dict)) %>%
  left_join(s07_holes_fixed_tbl, by = "Variable") %>%
  left_join(s07_ghosts_purged_tbl, by = "Variable") %>%
  mutate(
    Holes_Fixed_N = replace_na(Holes_Fixed_N, 0L),
    Ghosts_Purged_N = replace_na(Ghosts_Purged_N, 0L)
  )

s07_data_dictionary <- s07_data_dictionary %>%
  left_join(s07_qc_counts, by = "Variable") %>%
  mutate(
    Exclude_QC = map2_lgl(Variable, Original_Raw_Name, s07_is_excluded_var),
    Holes_Fixed = ifelse(Exclude_QC, "N/A", as.character(Holes_Fixed_N)),
    Ghosts_Purged = ifelse(Exclude_QC, "N/A", as.character(Ghosts_Purged_N))
  ) %>%
  select(
    Variable, Original_Raw_Name, Label,
    Stored_Data_Type, Field_Class, Value_Codes,
    Holes_Fixed, Ghosts_Purged
  ) %>%
  arrange(Variable)

s07_value_labels_long <- map_dfr(names(s07_final_df_for_dict), function(v) {
  lbls <- attr(s07_final_df_for_dict[[v]], "labels")
  if (is.null(lbls)) return(NULL)
  tibble(Variable = v, Value = as.numeric(lbls), Label = names(lbls)) %>%
    arrange(Value)
})

# ------------------------------------------------------------------------------
# 9. Build summary tables
# ------------------------------------------------------------------------------
cat("--- 6. Building summary tables ---\n")

s07_n_raw <- nrow(s07_raw_df)
s07_n_raw_completed <- s07_raw_df %>%
  filter(s07_norm_txt(status) == "COMPLETED") %>%
  nrow()
s07_n_final <- nrow(s07_df_analysis)

s07_n_ids_raw <- s07_raw_renamed %>%
  summarise(n = n_distinct(meta_respondent_id)) %>%
  pull(n)

s07_n_ids_final <- s07_analysis_stripped %>%
  summarise(n = n_distinct(meta_respondent_id)) %>%
  pull(n)

s07_summary_kpis <- tibble(
  Metric = c(
    "Raw records (all)",
    "Valid records (Respondent completed survey)",
    "Final records (Cleaned and Processed)",
    "Distinct respondent IDs (raw)",
    "Distinct respondent IDs (final)",
    "Excluded records (non-completed survey)",
    "Total value-level changes logged"
  ),
  Value = c(
    s07_n_raw,
    s07_n_raw_completed,
    s07_n_final,
    s07_n_ids_raw,
    s07_n_ids_final,
    s07_n_excluded_rows,
    nrow(s07_change_log)
  )
)

s07_change_type_levels_summary <- c(
  "Filled missing response using Imputation",
  "Logic violation purged to blank during validation",
  "Misaligned response set to missing",
  "Recoded to numeric category"
)

s07_change_by_type <- s07_change_log %>%
  count(Change_Type, name = "N") %>%
  complete(Change_Type = s07_change_type_levels_summary, fill = list(N = 0)) %>%
  mutate(
    Pct_num = if (sum(N) == 0) 0 else N / sum(N),
    Pct = percent(Pct_num, accuracy = 0.1)
  ) %>%
  select(Change_Type, N, Pct) %>%
  arrange(match(Change_Type, s07_change_type_levels_summary))

s07_change_by_variable <- s07_change_log %>%
  count(Variable, Change_Type, name = "N") %>%
  group_by(Variable) %>%
  mutate(
    Pct_within_variable_num = if (sum(N) == 0) 0 else N / sum(N),
    Pct_relative_to_final_records_num = N / s07_n_final
  ) %>%
  ungroup() %>%
  mutate(
    Pct_within_variable = percent(Pct_within_variable_num, accuracy = 0.1),
    Pct_relative_to_final_records = percent(Pct_relative_to_final_records_num, accuracy = 0.1)
  ) %>%
  select(Variable, Change_Type, N, Pct_within_variable, Pct_relative_to_final_records) %>%
  arrange(desc(N), Variable)

s07_top_imputed <- s07_change_log %>%
  filter(Change_Type == "Filled missing response using Imputation") %>%
  count(Variable, name = "N_Imputed") %>%
  mutate(Pct_Imputed = percent(N_Imputed / s07_n_final)) %>%
  left_join(s07_data_dictionary %>% select(Variable, Label), by = "Variable") %>%
  select(Variable, Label, N_Imputed, Pct_Imputed) %>%
  arrange(desc(N_Imputed)) %>%
  slice_head(n = 25)

s07_top_purged <- s07_change_log %>%
  filter(Change_Type == "Logic violation purged to blank during validation") %>%
  count(Variable, name = "N_Purged") %>%
  mutate(Pct_Purged = percent(N_Purged / s07_n_final)) %>%
  left_join(s07_data_dictionary %>% select(Variable, Label), by = "Variable") %>%
  select(Variable, Label, N_Purged, Pct_Purged) %>%
  arrange(desc(N_Purged)) %>%
  slice_head(n = 25)

# ------------------------------------------------------------------------------
# 10. Export workbook
# ------------------------------------------------------------------------------
cat("--- 7. Writing Excel documentation workbook ---\n")

s07_wb <- createWorkbook()

s07_header_style <- createStyle(
  fgFill = "#1F4E78",
  fontColour = "white",
  textDecoration = "bold"
)
s07_subhdr_style <- createStyle(textDecoration = "bold")
s07_wrap_style   <- createStyle(wrapText = TRUE)

# ------------------------------------------------------------------------------
# A. Summary
# ------------------------------------------------------------------------------
addWorksheet(s07_wb, "A. Summary")
freezePane(s07_wb, "A. Summary", firstActiveRow = 4, firstActiveCol = 1)

writeData(s07_wb, "A. Summary", "Sproxil Survey Documentation (Client Summary)", startRow = 1, startCol = 1)
addStyle(s07_wb, "A. Summary", s07_subhdr_style, rows = 1, cols = 1, stack = TRUE)

s07_r_kpi_title <- 3
s07_r_kpi_table <- 4

writeData(s07_wb, "A. Summary", "Key counts", startRow = s07_r_kpi_title, startCol = 1)
addStyle(s07_wb, "A. Summary", s07_subhdr_style, rows = s07_r_kpi_title, cols = 1, stack = TRUE)
writeData(s07_wb, "A. Summary", s07_summary_kpis, startRow = s07_r_kpi_table, startCol = 1)
addStyle(
  s07_wb, "A. Summary", s07_header_style,
  rows = s07_r_kpi_table,
  cols = 1:ncol(s07_summary_kpis),
  gridExpand = TRUE,
  stack = TRUE
)

s07_r_type_title <- s07_r_kpi_table + nrow(s07_summary_kpis) + 3
s07_r_type_table <- s07_r_type_title + 1

writeData(s07_wb, "A. Summary", "Value-level changes by type", startRow = s07_r_type_title, startCol = 1)
addStyle(s07_wb, "A. Summary", s07_subhdr_style, rows = s07_r_type_title, cols = 1, stack = TRUE)
writeData(s07_wb, "A. Summary", s07_change_by_type, startRow = s07_r_type_table, startCol = 1)
addStyle(
  s07_wb, "A. Summary", s07_header_style,
  rows = s07_r_type_table,
  cols = 1:ncol(s07_change_by_type),
  gridExpand = TRUE,
  stack = TRUE
)

s07_r_imp_title <- s07_r_type_table + nrow(s07_change_by_type) + 3
s07_r_imp_table <- s07_r_imp_title + 1

writeData(s07_wb, "A. Summary", "Top variables by missing values imputed", startRow = s07_r_imp_title, startCol = 1)
addStyle(s07_wb, "A. Summary", s07_subhdr_style, rows = s07_r_imp_title, cols = 1, stack = TRUE)
writeData(s07_wb, "A. Summary", s07_top_imputed, startRow = s07_r_imp_table, startCol = 1)
if (ncol(s07_top_imputed) > 0) {
  addStyle(
    s07_wb, "A. Summary", s07_header_style,
    rows = s07_r_imp_table,
    cols = 1:ncol(s07_top_imputed),
    gridExpand = TRUE,
    stack = TRUE
  )
}

s07_r_purge_title <- s07_r_imp_table + nrow(s07_top_imputed) + 3
s07_r_purge_table <- s07_r_purge_title + 1

writeData(s07_wb, "A. Summary", "Top variables by logic violation purged", startRow = s07_r_purge_title, startCol = 1)
addStyle(s07_wb, "A. Summary", s07_subhdr_style, rows = s07_r_purge_title, cols = 1, stack = TRUE)
writeData(s07_wb, "A. Summary", s07_top_purged, startRow = s07_r_purge_table, startCol = 1)
if (ncol(s07_top_purged) > 0) {
  addStyle(
    s07_wb, "A. Summary", s07_header_style,
    rows = s07_r_purge_table,
    cols = 1:ncol(s07_top_purged),
    gridExpand = TRUE,
    stack = TRUE
  )
}

setColWidths(s07_wb, "A. Summary", cols = 1:6, widths = c(38, 55, 18, 18, 18, 18))

# ------------------------------------------------------------------------------
# B. Dictionary
# ------------------------------------------------------------------------------
addWorksheet(s07_wb, "B. Dictionary")
freezePane(s07_wb, "B. Dictionary", firstActiveRow = 2, firstActiveCol = 1)

s07_dict_out <- s07_data_dictionary %>%
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

writeData(s07_wb, "B. Dictionary", s07_dict_out, withFilter = TRUE)
addStyle(s07_wb, "B. Dictionary", s07_header_style, rows = 1, cols = 1:ncol(s07_dict_out), gridExpand = TRUE)
setColWidths(
  s07_wb, "B. Dictionary",
  cols = 1:ncol(s07_dict_out),
  widths = c(28, 30, 55, 18, 20, 70, 16, 16)
)
addStyle(
  s07_wb, "B. Dictionary", s07_wrap_style,
  rows = 1:(nrow(s07_dict_out) + 1),
  cols = 3,
  gridExpand = TRUE
)

# ------------------------------------------------------------------------------
# B. Value Labels
# ------------------------------------------------------------------------------
if (!is.null(s07_value_labels_long) && nrow(s07_value_labels_long) > 0) {
  addWorksheet(s07_wb, "B. Value Labels")
  freezePane(s07_wb, "B. Value Labels", firstActiveRow = 2, firstActiveCol = 1)
  writeData(s07_wb, "B. Value Labels", s07_value_labels_long, withFilter = TRUE)
  addStyle(s07_wb, "B. Value Labels", s07_header_style, rows = 1, cols = 1:ncol(s07_value_labels_long), gridExpand = TRUE)
  setColWidths(s07_wb, "B. Value Labels", cols = 1:3, widths = c(28, 12, 70))
}

# ------------------------------------------------------------------------------
# C. Change Summary
# ------------------------------------------------------------------------------
addWorksheet(s07_wb, "C. Change Summary")
freezePane(s07_wb, "C. Change Summary", firstActiveRow = 2, firstActiveCol = 1)

writeData(s07_wb, "C. Change Summary", "Changes by variable and change type", startRow = 1, startCol = 1)
addStyle(s07_wb, "C. Change Summary", s07_subhdr_style, rows = 1, cols = 1, stack = TRUE)
writeData(s07_wb, "C. Change Summary", s07_change_by_variable, startRow = 2, startCol = 1, withFilter = TRUE)
addStyle(
  s07_wb, "C. Change Summary", s07_header_style,
  rows = 2,
  cols = 1:ncol(s07_change_by_variable),
  gridExpand = TRUE
)
setColWidths(
  s07_wb, "C. Change Summary",
  cols = 1:ncol(s07_change_by_variable),
  widths = c(32, 45, 16, 20, 22)
)

saveWorkbook(s07_wb, S07_OUT_XLSX, overwrite = TRUE)

cat("✅ SUCCESS: Documentation workbook saved to ", S07_OUT_XLSX, "\n", sep = "")
cat("✅ SUCCESS: Full data change log saved to ", S07_OUT_LOG_CSV, "\n", sep = "")