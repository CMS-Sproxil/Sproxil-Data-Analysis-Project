# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Missingness report tables
# AUTHOR:  Corona Management Systems
# DATE:    23 January 2026
#
# PURPOSE: Create Annex Table A1 and Annex Table A2 with variable descriptions
#
# INPUTS:
#   - missingness_universal_summary.csv
#   - missingness_conditional_summary.csv
#   - Sproxil_dictionary.rds
#
# OUTPUTS:
#   - Annex_Table_A1_Universal_Item_Nonresponse.csv
#   - Annex_Table_A2_Conditional_Item_Nonresponse.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# ------------------------------------------------------------------------------
# 1. Load missingness outputs and metadata
# ------------------------------------------------------------------------------
a1_universal <- read_csv("missingness_universal_summary.csv", show_col_types = FALSE)
a2_conditional <- read_csv("missingness_conditional_summary.csv", show_col_types = FALSE)

metadata <- readRDS("Sproxil_dictionary.rds")
variable_labels <- metadata$variable_labels

# ------------------------------------------------------------------------------
# 2. Prepare variable description lookup
# ------------------------------------------------------------------------------
var_desc_lookup <- tibble(
  variable = names(variable_labels),
  variable_description = unname(variable_labels)
)

# ------------------------------------------------------------------------------
# 3. Build Annex Table A1
# ------------------------------------------------------------------------------
annex_table_a1 <- a1_universal %>%
  left_join(var_desc_lookup, by = "variable") %>%
  mutate(
    variable_description = ifelse(
      is.na(variable_description),
      str_replace_all(variable, "_", " "),
      variable_description
    )
  ) %>%
  select(
    module,
    variable,
    variable_description,
    total_n,
    observed_n,
    nonresponse_n,
    nonresponse_pct
  ) %>%
  rename(
    `Module` = module,
    `Variable` = variable,
    `Variable description` = variable_description,
    `Total eligible records` = total_n,
    `Substantive responses` = observed_n,
    `Non-response count` = nonresponse_n,
    `Non-response (%)` = nonresponse_pct
  ) %>%
  arrange(`Module`, desc(`Non-response (%)`), `Variable`)

# ------------------------------------------------------------------------------
# 4. Build Annex Table A2
# ------------------------------------------------------------------------------
annex_table_a2 <- a2_conditional %>%
  left_join(var_desc_lookup, by = "variable") %>%
  mutate(
    variable_description = ifelse(
      is.na(variable_description),
      str_replace_all(variable, "_", " "),
      variable_description
    ),
    trigger_rule = case_when(
      trigger_type == "EQUALS" ~ paste0(parent, " = ", trigger_value),
      trigger_type == "NOT_EQUALS" ~ paste0(parent, " != ", trigger_value),
      trigger_type == "STARTS_WITH" ~ paste0(parent, " starts with ", trigger_value),
      trigger_type == "CONTAINS" ~ paste0(parent, " contains ", trigger_value),
      trigger_type == "NOT_CONTAINS" ~ paste0(parent, " does not contain ", trigger_value),
      trigger_type == "IN_SET" ~ paste0(parent, " in {", trigger_value, "}"),
      TRUE ~ paste(parent, trigger_type, trigger_value)
    )
  ) %>%
  select(
    module,
    variable,
    variable_description,
    parent,
    trigger_type,
    trigger_value,
    trigger_rule,
    applicable_n,
    observed_n,
    nonresponse_n,
    nonresponse_pct
  ) %>%
  rename(
    `Module` = module,
    `Variable` = variable,
    `Variable description` = variable_description,
    `Parent variable` = parent,
    `Trigger type` = trigger_type,
    `Trigger value` = trigger_value,
    `Routing condition` = trigger_rule,
    `Applicable denominator` = applicable_n,
    `Substantive responses` = observed_n,
    `Non-response count` = nonresponse_n,
    `Non-response (%)` = nonresponse_pct
  ) %>%
  arrange(`Module`, desc(`Non-response (%)`), `Variable`)

# ------------------------------------------------------------------------------
# 5. Export annex tables
# ------------------------------------------------------------------------------
write_csv(annex_table_a1, "Annex_Table_A1_Universal_Item_Nonresponse.csv")
write_csv(annex_table_a2, "Annex_Table_A2_Conditional_Item_Nonresponse.csv")

# ------------------------------------------------------------------------------
# 6. Print previews
# ------------------------------------------------------------------------------
annex_table_a1
annex_table_a2