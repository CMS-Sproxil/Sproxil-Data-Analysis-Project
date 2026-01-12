# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT: Sproxil_Flag
# AUTHOR: Ikechukwu Onuko
# DATE: 25 November 2025
#
# DESCRIPTION:
# This script performs automated data validation using the STANDARDISED column names.
# It detects inconsistencies, missing logic skips, and incorrect responses.
# ==============================================================================

# -------------------------------------------------------------------
# 1. INITIAL SETUP
# -------------------------------------------------------------------
library(dplyr)
library(stringr)
library(labelled)
library(knitr)
library(visdat)
library(ggplot2)
library(tidyr)
library(readr)

# Load the cleaned/standardized dataset
# Ensure this file exists from the previous cleaning step
if(file.exists("Sproxil_Prepared.rds")) {
  flag_df <- readRDS("Sproxil_Prepared.rds")
} else {
  stop("Please run the Cleaning Script first to generate the prepared dataset")
}

# -------------------------------------------------------------------
# 2. DATA PRE-PROCESSING & INITIAL INSPECTION
# -------------------------------------------------------------------
# Display structure
cat("Data structure:\n")
glimpse(flag_df)

# -------------------------------------------------------------------
# 3. UNIQUE VALUE INSPECTION
# -------------------------------------------------------------------
cat("\n--- 3. Writing detailed column analysis to 'column_summary_report.txt' ---\n")

sink("column_summary_report.txt")
cat("--- Analysis of Unique Values in Each Column ---\n")

for (col_name in names(flag_df)) {
  col_vector <- flag_df %>% pull(col_name)
  n_unique <- n_distinct(col_vector, na.rm = TRUE)
  freq_table <- table(col_vector, useNA = "ifany")
  
  cat(paste("\n----- Column:", col_name, "-----\n"))
  cat(paste("Number of Unique Values:", n_unique, "\n\n"))
  
  if (n_unique < 20) {
    print(kable(freq_table, col.names = c("Value", "Count")))
  } else {
    print(kable(head(sort(freq_table, decreasing = TRUE), 5), col.names = c("Value", "Count")))
  }
}
sink()
cat("Report saved.\n")


# -------------------------------------------------------------------
# 4. CHECK POTENTIALLY MISCODED NUMERIC COLUMNS
# -------------------------------------------------------------------
cat("\n--- 4. Checking for Numeric Data stored as Text ---\n")

potential_numeric_cols <- sapply(flag_df, function(x) {
  if (!is.character(x)) return(1.0)
  original_na_count <- sum(is.na(x))
  numeric_vals <- suppressWarnings(as.numeric(x))
  coerced_na_count <- sum(is.na(numeric_vals))
  original_non_na_count <- sum(!is.na(x))
  if (original_non_na_count == 0) return(0)
  failed_conversions <- coerced_na_count - original_na_count
  return(failed_conversions / original_non_na_count)
})

cat("\nColumns that look numeric (≥90% convertible):\n")
print(names(which(potential_numeric_cols < 0.1)))


# -------------------------------------------------------------------
# 5. FLAG INITIALISATION
# -------------------------------------------------------------------
cat("\n--- 5. Initialising Flags and Performing Logical Checks ---\n")

# List of flag variables
flag_vars <- c(
  "flag_demo_gender_wqskip", 
  "flag_demo_edulevel_informal",
  "flag_prev_hasnets_logic", 
  "flag_treat_fever_logic", 
  "flag_treat_bsample_logic",
  "flag_treat_smc_heard_logic", 
  "flag_treat_smc_rec_logic", 
  "flag_treat_vaccine_logic",
  "flag_women_birth_logic", 
  "flag_women_anc_logic", 
  "flag_women_sp_logic",
  "flag_women_childfever_logic", 
  "flag_women_childmed_logic", 
  "flag_women_pregnant_logic", 
  "flag_bg_internet_logic", 
  "flag_bg_malmsg_logic",
  "flag_bg_malavoid_logic", 
  "flag_hh_toilet_logic", 
  "flag_hh_livestock_logic", 
  "flag_hh_agriland_logic", 
  "flag_hh_children_vs_total"
)

# Initialize all flags to 0
flag_df[flag_vars] <- 0


# -------------------------------------------------------------------
# 6. LOGICAL CONSISTENCY RULES
# -------------------------------------------------------------------
# Using Standardized Variable Names from the previous script

## 1. DEMOGRAPHICS
flag_df <- flag_df %>%
  mutate(
    # Gender is Male, but Women's Questionnaire columns are filled
    flag_demo_gender_wqskip = ifelse(!is.na(demo_gender) & demo_gender == "MALE" &
                                       rowSums(!is.na(select(., starts_with("women_")))) > 0, 1, 0),
    
    # Says "No Education" but filled "Informal Education" details
    flag_demo_edulevel_informal = ifelse(!is.na(demo_edu_level) &
                                           demo_edu_level == "NO EDUCATION" &
                                           !is.na(demo_edu_informal), 1, 0)
  )

## 2. MALARIA PREVENTION
flag_df <- flag_df %>%
  mutate(
    # Says NO nets, but filled number, brand, or acquisition details
    flag_prev_hasnets_logic = ifelse(!is.na(prev_has_mosquito_nets) &
                                       prev_has_mosquito_nets == "NO" &
                                       rowSums(!is.na(select(., prev_num_mosquito_nets, prev_net_brand, prev_net_obtained_how))) > 0, 1, 0)
    
    # Note: Logic for "SleptInNetLastNight" removed because that question 
    # was missing in the raw headers provided.
  )

## 3. MALARIA TREATMENT
flag_df <- flag_df %>%
  mutate(
    # Says NO Fever, but filled blood sample, costs, or drug details
    flag_treat_fever_logic = ifelse(!is.na(treat_hh_fever_last_2weeks) &
                                      treat_hh_fever_last_2weeks == "NO" &
                                      rowSums(!is.na(select(., treat_blood_sample_taken, treat_drug_cost))) > 0, 1, 0),
    
    # Says NO Blood Sample, but filled test cost
    flag_treat_bsample_logic = ifelse(!is.na(treat_blood_sample_taken) &
                                        treat_blood_sample_taken == "NO" &
                                        !is.na(treat_test_cost), 1, 0)
  )

## 4. GOVERNMENT CONTROL / SMC (Note: SMC vars are prefixed 'treat_' in standardization)
flag_df <- flag_df %>%
  mutate(
    # Heard SMC = NO, but says children received it
    flag_treat_smc_heard_logic = ifelse(!is.na(treat_heard_smc) &
                                          treat_heard_smc == "NO" &
                                          (!is.na(treat_children_received_smc) | !is.na(treat_know_smc_drug)), 1, 0),
    
    # Children Received SMC = NO, but knows the drug name
    flag_treat_smc_rec_logic = ifelse(!is.na(treat_children_received_smc) &
                                        treat_children_received_smc == "NO" &
                                        !is.na(treat_know_smc_drug), 1, 0),
    
    # Doesn't know vaccine, but says children received it
    flag_treat_vaccine_logic = ifelse(!is.na(treat_vaccine_age_knowledge) &
                                        str_detect(treat_vaccine_age_knowledge, "DON'T KNOW") &
                                        !is.na(treat_children_received_vaccine), 1, 0)
  )

## 5. WOMEN QUESTIONNAIRE
flag_df <- flag_df %>%
  mutate(
    # Says Never Given Birth, but filled children count/ANC
    flag_women_birth_logic = ifelse(!is.na(women_ever_given_birth) &
                                      women_ever_given_birth == "NO" &
                                      rowSums(!is.na(select(., women_births_2020_2025, women_anc_seen))) > 0, 1, 0),
    
    # Says NO ANC, but filled provider/location
    flag_women_anc_logic = ifelse(!is.na(women_anc_seen) &
                                    women_anc_seen == "NO" &
                                    rowSums(!is.na(select(., women_anc_provider, women_anc_location))) > 0, 1, 0),
    
    # Says NO SP/Fansidar, but filled doses
    flag_women_sp_logic = ifelse(!is.na(women_took_sp_fansidar) &
                                   women_took_sp_fansidar == "NO" &
                                   (!is.na(women_sp_fansidar_doses) | !is.na(women_sp_fansidar_source)), 1, 0),
    
    # Says Child NO Fever, but filled diagnosis/treatment
    flag_women_childfever_logic = ifelse(!is.na(women_child_fever_2weeks) &
                                           women_child_fever_2weeks == "NO" &
                                           rowSums(!is.na(select(., women_child_blood_sample, women_child_malaria_diagnosis))) > 0, 1, 0),
    
    # Says Child Took NO Medicine, but filled medicine type
    flag_women_childmed_logic = ifelse(!is.na(women_child_took_medicine) &
                                         women_child_took_medicine == "NO" &
                                         !is.na(women_child_medicine_type), 1, 0),
    
    # Says Not Pregnant, but filled duration
    flag_women_pregnant_logic = ifelse(!is.na(women_currently_pregnant) &
                                         women_currently_pregnant == "NO" &
                                         !is.na(women_pregnancy_duration_months), 1, 0)
  )

## 6. BACKGROUND
flag_df <- flag_df %>%
  mutate(
    # Says Never Used Internet, but filled frequency
    flag_bg_internet_logic = ifelse(!is.na(bg_internet_ever_used) &
                                      bg_internet_ever_used == "NO" &
                                      !is.na(bg_internet_frequency), 1, 0),
    
    # Says Never Heard Malaria Msg, but filled source
    flag_bg_malmsg_logic = ifelse(!is.na(bg_heard_malaria_msg_6months) &
                                    bg_heard_malaria_msg_6months == "NO" &
                                    !is.na(bg_malaria_msg_source), 1, 0),
    
    # Says Not Aware of Avoidance, but listed methods
    flag_bg_malavoid_logic = ifelse(!is.na(bg_aware_avoidance) &
                                      bg_aware_avoidance == "NO" &
                                      !is.na(bg_prevention_knowledge), 1, 0)
  )

## 7. HOUSEHOLD
flag_df <- flag_df %>%
  mutate(
    # Says No Toilet/Bush, but filled shared/location
    flag_hh_toilet_logic = ifelse(!is.na(hh_toilet_type) &
                                    str_detect(hh_toilet_type, "NO FACILITY") &
                                    rowSums(!is.na(select(., hh_toilet_shared, hh_toilet_location))) > 0, 1, 0),
    
    # Says No Livestock, but filled counts
    flag_hh_livestock_logic = ifelse(!is.na(hh_owns_livestock) &
                                       hh_owns_livestock == "NO" &
                                       rowSums(!is.na(select(., hh_num_cows_bulls, hh_num_goats))) > 0, 1, 0),
    
    # Says No Agri Land, but filled plots
    flag_hh_agriland_logic = ifelse(!is.na(hh_owns_agri_land) &
                                      hh_owns_agri_land == "NO" &
                                      !is.na(hh_num_agri_plots), 1, 0),
    
    # Logic: Children under 5 > Total Persons in Household
    hh_u5_numeric = as.numeric(str_extract(hh_members_under5, "\\d+")),
    hh_total_numeric = as.numeric(str_extract(hh_total_persons_v1, "\\d+")),
    
    flag_hh_children_vs_total = ifelse(!is.na(hh_u5_numeric) & 
                                         !is.na(hh_total_numeric) & 
                                         hh_u5_numeric > hh_total_numeric, 1, 0)
  ) %>%
  select(-hh_u5_numeric, -hh_total_numeric) # Clean up temp columns

cat("Logical flagging complete.\n")


# -------------------------------------------------------------------
# 7. APPLY LABELS AND SUMMARIZE
# -------------------------------------------------------------------
cat("\n--- 7. Applying Labels and Generating Summary ---\n")

var_labels <- c(
  flag_demo_gender_wqskip = 'Male respondent answered Women Qs',
  flag_demo_edulevel_informal = 'No Education but Informal details filled',
  flag_prev_hasnets_logic = 'No Nets but Net details filled',
  flag_treat_fever_logic = 'No Fever but Treatment details filled',
  flag_treat_bsample_logic = 'No Blood Sample but Test Cost filled',
  flag_treat_smc_heard_logic = 'Not Heard SMC but Child Received SMC details filled',
  flag_treat_smc_rec_logic = 'Child did not Rec SMC but Drug Known',
  flag_treat_vaccine_logic = 'Does not know Vaccine but Child Received vaccine details filled',
  flag_women_birth_logic = 'Never Given Birth but Children details filled',
  flag_women_anc_logic = 'No ANC but Provider/Location filled',
  flag_women_sp_logic = 'No SP/Fansidar but Dose details filled',
  flag_women_childfever_logic = 'Child No Fever but Diagnosis filled',
  flag_women_childmed_logic = 'Child No Meds but Med Type filled',
  flag_women_pregnant_logic = 'Not Pregnant but Pregnancy Duration filled',
  flag_bg_internet_logic = 'Never used Internet but Internet Frequency filled',
  flag_bg_malmsg_logic = 'Never heard Malaria Msg but Msg Source filled',
  flag_bg_malavoid_logic = 'Unaware of Malaria Avoidance but Methods filled',
  flag_hh_toilet_logic = 'No Toilet but Shared/Location filled',
  flag_hh_livestock_logic = 'No Livestock but Counts filled',
  flag_hh_agriland_logic = 'No Agri Land but Plots filled',
  flag_hh_children_vs_total = 'Children U5 > Total Household Members'
)

# Apply variable labels
var_label(flag_df[, names(var_labels)]) <- as.list(var_labels)

# Convert flags to Factors
flag_df <- flag_df %>%
  mutate(across(all_of(flag_vars),
                ~factor(., levels = c(0, 1),
                        labels = c('Consistent', 'Inconsistent'))))

# Create Summary Table
summary_table <- bind_rows(
  lapply(flag_vars, function(v) {
    as.data.frame(table(flag_df[[v]])) %>%
      mutate(FlagName = v)
  })
) %>%
  rename(Status = Var1, Count = Freq) %>%
  pivot_wider(names_from = Status, values_from = Count, values_fill = 0) %>%
  mutate(Total = Consistent + Inconsistent) %>%
  arrange(desc(Inconsistent))

# 4. Map Descriptions to the Summary Table and Reorder
# Create a lookup table from the named vector var_labels
label_lookup <- tibble(
  FlagName = names(var_labels),
  Description = unname(var_labels)
)

# Join and arrange columns so Description is FIRST
summary_table <- summary_table %>%
  left_join(label_lookup, by = "FlagName") %>%
  select(FlagName, Description, Consistent, Inconsistent, Total) %>%
  arrange(desc(Inconsistent))


print(kable(summary_table, caption = "Summary of Logic Flags"))

# -------------------------------------------------------------------
# 8. EXPORT
# -------------------------------------------------------------------

# Save the flagged dataset
saveRDS(flag_df, "Sproxil_flagged.rds")

# Save the Summary Table
write_csv(summary_table, "Flag_Summary_Report.csv")

cat("\nDone. Flagged dataset saved as 'Sproxil_flagged.rds'.\n")