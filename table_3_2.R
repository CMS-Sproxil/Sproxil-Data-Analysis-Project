suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Table_3_2_Source_of_Mosquito_Nets.R
# PURPOSE:
#   Create:
#     - Table 3.2.1 Source of mosquito nets: National
#     - Table 3.2.2 Source of mosquito nets: States (ITNs only)
#
# IMPORTANT MODIFICATION NOTES
# - Built from Sproxil household respondent data, not DHS household roster data
# - Source categories are harmonised from:
#     * prev_net_obtained_how
#     * prev_net_obtained_where
# - National table uses households with at least one mosquito net
# - State table uses households with an ITN
# - Percentage columns are weighted if WVAR exists; count column is unweighted
# - "Number of mosquito nets" is based on reported household net counts:
#     * national rows: hh_nets_num
#     * ITN/state rows: derived_num_itns
# ==============================================================================

# -----------------------------
# CONFIG
# -----------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
SHEET_NAT <- "Table 3.2.1"
SHEET_ST  <- "Table 3.2.2"
WVAR      <- "w_calibrated_trim"

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 3.2 weighting active: ", use_weight)

# -----------------------------
# Helpers
# -----------------------------
source_levels <- c(
  "Mass distribution campaign",
  "ANC visit",
  "Immunization visit",
  "Government health facility",
  "Private health facility",
  "Pharmacy",
  "Shop/market",
  "Community health worker",
  "Religious institution",
  "School",
  "Other",
  "Don't know"
)

state_names <- c(
  `1`="Abia", `2`="Adamawa", `3`="Akwa Ibom", `4`="Anambra", `5`="Bauchi", `6`="Bayelsa",
  `7`="Benue", `8`="Borno", `9`="Cross River", `10`="Delta", `11`="Ebonyi", `12`="Edo",
  `13`="Ekiti", `14`="Enugu", `15`="FCT-Abuja", `16`="Gombe", `17`="Imo", `18`="Jigawa",
  `19`="Kaduna", `20`="Kano", `21`="Katsina", `22`="Kebbi", `23`="Kogi", `24`="Kwara",
  `25`="Lagos", `26`="Nasarawa", `27`="Niger", `28`="Ogun", `29`="Ondo", `30`="Osun",
  `31`="Oyo", `32`="Plateau", `33`="Rivers", `34`="Sokoto", `35`="Taraba", `36`="Yobe",
  `37`="Zamfara"
)

zone_names <- c(
  `1`="North Central", `2`="North East", `3`="North West",
  `4`="South East", `5`="South South", `6`="South West"
)

zone_to_states <- list(
  "North Central" = c("FCT-Abuja","Benue","Kogi","Kwara","Nasarawa","Niger","Plateau"),
  "North East"    = c("Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe"),
  "North West"    = c("Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara"),
  "South East"    = c("Abia","Anambra","Ebonyi","Enugu","Imo"),
  "South South"   = c("Akwa Ibom","Bayelsa","Cross River","Delta","Edo","Rivers"),
  "South West"    = c("Ekiti","Lagos","Ogun","Ondo","Osun","Oyo")
)

row_of <- function(rows_vec, label, start_row) {
  i <- match(label, rows_vec)
  if (is.na(i)) return(NA_integer_)
  start_row + i - 1
}

write_source_row <- function(wb, sheet, r, vec, n_nets,
                             col_start = 3, col_n = 16,
                             pct_style = NULL, int_style = NULL) {
  if (is.na(r)) return(invisible(FALSE))
  writeData(wb, sheet, as.list(vec), startRow = r, startCol = col_start, colNames = FALSE)
  writeData(wb, sheet, n_nets, startRow = r, startCol = col_n, colNames = FALSE)
  if (!is.null(pct_style)) addStyle(wb, sheet, pct_style, rows = r, cols = col_start:(col_n - 1),
                                    gridExpand = TRUE, stack = TRUE)
  if (!is.null(int_style)) addStyle(wb, sheet, int_style, rows = r, cols = col_n,
                                    stack = TRUE)
  invisible(TRUE)
}

source_dist <- function(dat, source_var = "source_cat", wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[source_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  out <- setNames(rep(NA_real_, length(source_levels)), source_levels)
  
  if (nrow(d) == 0) return(out)
  
  if (is.null(wvar)) {
    tab <- d %>%
      count(.data[[source_var]], name = "n") %>%
      mutate(p = 100 * n / sum(n))
  } else {
    tab <- d %>%
      group_by(.data[[source_var]]) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      mutate(p = 100 * w / sum(w))
  }
  
  for (i in seq_len(nrow(tab))) {
    out[tab[[source_var]][i]] <- round(tab$p[i], 1)
  }
  
  out["Total"] <- 100.0
  out
}

safe_sum_nets <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(0L)
  as.integer(round(sum(x, na.rm = TRUE), 0))
}

# -----------------------------
# Build analysis frame
# -----------------------------
req_vars <- c(
  "u_household", "derived_hh_has_any_net", "derived_hh_has_itn",
  "derived_residence", "derived_zone", "derived_wealth_quintile",
  "demo_state_num", "prev_net_obtained_how", "prev_net_obtained_where",
  "hh_nets_num", "derived_num_itns"
)
miss <- setdiff(req_vars, names(df))
if (length(miss) > 0) stop("Missing required variables: ", paste(miss, collapse = ", "))

df_t <- df %>%
  filter(u_household == 1) %>%
  mutate(
    residence = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    zone = unname(zone_names[as.character(as.numeric(derived_zone))]),
    wealth = case_when(
      as.numeric(derived_wealth_quintile) == 1 ~ "Lowest",
      as.numeric(derived_wealth_quintile) == 2 ~ "Second",
      as.numeric(derived_wealth_quintile) == 3 ~ "Middle",
      as.numeric(derived_wealth_quintile) == 4 ~ "Fourth",
      as.numeric(derived_wealth_quintile) == 5 ~ "Highest",
      TRUE ~ NA_character_
    ),
    state_name = unname(state_names[as.character(as.numeric(demo_state_num))]),
    how = suppressWarnings(as.numeric(prev_net_obtained_how)),
    where = suppressWarnings(as.numeric(prev_net_obtained_where)),
    hh_nets_num = suppressWarnings(as.numeric(hh_nets_num)),
    derived_num_itns = suppressWarnings(as.numeric(derived_num_itns)),
    net_type = case_when(
      as.numeric(derived_hh_has_itn) == 1 ~ "ITN1",
      as.numeric(derived_hh_has_itn) == 0 & as.numeric(derived_hh_has_any_net) == 1 ~ "Other2",
      TRUE ~ NA_character_
    ),
    source_cat = case_when(
      how == 1 ~ "Mass distribution campaign",
      how == 2 ~ "ANC visit",
      how == 3 ~ "Immunization visit",
      where == 1 ~ "Government health facility",
      where == 2 ~ "Private health facility",
      where == 3 ~ "Pharmacy",
      where == 4 ~ "Shop/market",
      where == 5 ~ "Community health worker",
      where == 6 ~ "Religious institution",
      where == 7 ~ "School",
      how == 96 | where == 96 ~ "Other",
      where == 98 ~ "Don't know",
      TRUE ~ NA_character_
    )
  )

# National table universe: households with any net and source category
df_nat <- df_t %>%
  filter(as.numeric(derived_hh_has_any_net) == 1)

# State table universe: households with ITN and source category
df_st <- df_t %>%
  filter(as.numeric(derived_hh_has_itn) == 1)

# -----------------------------
# Workbook setup
# -----------------------------
if (file.exists(OUT_XLSX)) {
  wb <- loadWorkbook(OUT_XLSX)
} else {
  wb <- createWorkbook()
}

if (SHEET_NAT %in% names(wb)) removeWorksheet(wb, SHEET_NAT)
if (SHEET_ST  %in% names(wb)) removeWorksheet(wb, SHEET_ST)

addWorksheet(wb, SHEET_NAT, gridLines = FALSE)
addWorksheet(wb, SHEET_ST,  gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
hdrStyle   <- createStyle(textDecoration = "bold")
pctStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

# ==============================================================================
# TABLE 3.2.1 NATIONAL
# ==============================================================================
rows_nat <- c(
  "Background characteristic",
  "Type of net",
  "ITN1",
  "Other2",
  "",
  "Residence",
  "Urban",
  "Rural",
  "",
  "Region",
  "North Central",
  "North East",
  "North West",
  "South East",
  "South South",
  "South West",
  "",
  "Wealth quintile",
  "Lowest",
  "Second",
  "Middle",
  "Fourth",
  "Highest",
  "",
  "Total",
  "ANC = Antenatal care",
  "1 An insecticide-treated net (ITN) is a factory-treated net reported or identified as an ITN in the Sproxil respondent survey.",
  "2 Any net that is not an ITN."
)

START_ROW_NAT <- 9
COL_LABEL     <- 2
COL_FIRST_SRC <- 3
COL_N_NETS    <- 16

writeData(wb, SHEET_NAT,
          "Table 3.2.1  Source of mosquito nets: National (Sproxil respondents)",
          startRow = 2, startCol = 2)
addStyle(wb, SHEET_NAT, titleStyle, rows = 2, cols = 2, stack = TRUE)

note_txt_nat <- if (use_weight) {
  "NOTE: Percentage columns weighted using calibrated trimmed weights; net counts are unweighted. Source distribution is based on households with at least one mosquito net and a non-missing reported source."
} else {
  "NOTE: Unweighted percentages; net counts are unweighted. Source distribution is based on households with at least one mosquito net and a non-missing reported source."
}
writeData(wb, SHEET_NAT, note_txt_nat, startRow = 3, startCol = 2)
addStyle(wb, SHEET_NAT, noteStyle, rows = 3, cols = 2, stack = TRUE)

hdr_nat <- c(
  "Background characteristic",
  "Mass distribution campaign",
  "ANC visit",
  "Immunization visit",
  "Government health facility",
  "Private health facility",
  "Pharmacy",
  "Shop/market",
  "Community health worker",
  "Religious institution",
  "School",
  "Other",
  "Don't know",
  "Total",
  "Number of mosquito nets"
)
writeData(wb, SHEET_NAT, hdr_nat, startRow = START_ROW_NAT - 1, startCol = COL_LABEL, colNames = FALSE)
addStyle(wb, SHEET_NAT, hdrStyle, rows = START_ROW_NAT - 1, cols = COL_LABEL:COL_N_NETS,
         gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET_NAT, rows_nat, startRow = START_ROW_NAT, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET_NAT, cols = COL_LABEL, widths = 24)
setColWidths(wb, SHEET_NAT, cols = COL_FIRST_SRC:(COL_N_NETS - 1), widths = 12)
setColWidths(wb, SHEET_NAT, cols = COL_N_NETS, widths = 14)

# Type of net rows
for (lab in c("ITN1", "Other2")) {
  dsub <- df_nat %>% filter(net_type == lab)
  vec <- source_dist(dsub, "source_cat", wvar_use)
  n_nets <- if (lab == "ITN1") safe_sum_nets(dsub$derived_num_itns) else safe_sum_nets(dsub$hh_nets_num)
  write_source_row(
    wb, SHEET_NAT, row_of(rows_nat, lab, START_ROW_NAT),
    vec, n_nets, COL_FIRST_SRC, COL_N_NETS, pctStyle, intStyle
  )
}

# Residence rows
for (lab in c("Urban", "Rural")) {
  dsub <- df_nat %>% filter(residence == lab)
  vec <- source_dist(dsub, "source_cat", wvar_use)
  n_nets <- safe_sum_nets(dsub$hh_nets_num)
  write_source_row(
    wb, SHEET_NAT, row_of(rows_nat, lab, START_ROW_NAT),
    vec, n_nets, COL_FIRST_SRC, COL_N_NETS, pctStyle, intStyle
  )
}

# Zone rows
for (lab in c("North Central","North East","North West","South East","South South","South West")) {
  dsub <- df_nat %>% filter(zone == lab)
  vec <- source_dist(dsub, "source_cat", wvar_use)
  n_nets <- safe_sum_nets(dsub$hh_nets_num)
  write_source_row(
    wb, SHEET_NAT, row_of(rows_nat, lab, START_ROW_NAT),
    vec, n_nets, COL_FIRST_SRC, COL_N_NETS, pctStyle, intStyle
  )
}

# Wealth quintile rows
for (lab in c("Lowest","Second","Middle","Fourth","Highest")) {
  dsub <- df_nat %>% filter(wealth == lab)
  vec <- source_dist(dsub, "source_cat", wvar_use)
  n_nets <- safe_sum_nets(dsub$hh_nets_num)
  write_source_row(
    wb, SHEET_NAT, row_of(rows_nat, lab, START_ROW_NAT),
    vec, n_nets, COL_FIRST_SRC, COL_N_NETS, pctStyle, intStyle
  )
}

# Total row
vec_tot_nat <- source_dist(df_nat, "source_cat", wvar_use)
n_tot_nat <- safe_sum_nets(df_nat$hh_nets_num)
write_source_row(
  wb, SHEET_NAT, row_of(rows_nat, "Total", START_ROW_NAT),
  vec_tot_nat, n_tot_nat, COL_FIRST_SRC, COL_N_NETS, pctStyle, intStyle
)

# ==============================================================================
# TABLE 3.2.2 STATES (ITNs only)
# ==============================================================================
rows_st <- c(
  "State",
  "North Central",
  "FCT-Abuja","Benue","Kogi","Kwara","Nasarawa","Niger","Plateau",
  "",
  "North East",
  "Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe",
  "",
  "North West",
  "Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara",
  "",
  "South East",
  "Abia","Anambra","Ebonyi","Enugu","Imo",
  "",
  "South South",
  "Akwa Ibom","Bayelsa","Cross River","Delta","Edo","Rivers",
  "",
  "South West",
  "Ekiti","Lagos","Ogun","Ondo","Osun","Oyo",
  "",
  "Total",
  "ANC = Antenatal care",
  "1 An insecticide-treated net (ITN) is a factory-treated net reported or identified as an ITN in the Sproxil respondent survey.",
  "2 State estimates are based on the Sproxil respondent sample and should be interpreted with caution."
)

START_ROW_ST <- 9

writeData(wb, SHEET_ST,
          "Table 3.2.2  Source of mosquito nets: States (ITNs only; Sproxil respondents)",
          startRow = 2, startCol = 2)
addStyle(wb, SHEET_ST, titleStyle, rows = 2, cols = 2, stack = TRUE)

note_txt_st <- if (use_weight) {
  "NOTE: Percentage columns weighted using calibrated trimmed weights; ITN counts are unweighted. State rows are based on households with an ITN and a non-missing reported source."
} else {
  "NOTE: Unweighted percentages; ITN counts are unweighted. State rows are based on households with an ITN and a non-missing reported source."
}
writeData(wb, SHEET_ST, note_txt_st, startRow = 3, startCol = 2)
addStyle(wb, SHEET_ST, noteStyle, rows = 3, cols = 2, stack = TRUE)

hdr_st <- c(
  "State",
  "Mass distribution campaign",
  "ANC visit",
  "Immunization visit",
  "Government health facility",
  "Private health facility",
  "Pharmacy",
  "Shop/market",
  "Community health worker",
  "Religious institution",
  "School",
  "Other",
  "Don't know",
  "Total",
  "Number of mosquito nets"
)
writeData(wb, SHEET_ST, hdr_st, startRow = START_ROW_ST - 1, startCol = COL_LABEL, colNames = FALSE)
addStyle(wb, SHEET_ST, hdrStyle, rows = START_ROW_ST - 1, cols = COL_LABEL:COL_N_NETS,
         gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET_ST, rows_st, startRow = START_ROW_ST, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET_ST, cols = COL_LABEL, widths = 18)
setColWidths(wb, SHEET_ST, cols = COL_FIRST_SRC:(COL_N_NETS - 1), widths = 12)
setColWidths(wb, SHEET_ST, cols = COL_N_NETS, widths = 14)

for (zn in names(zone_to_states)) {
  for (st in zone_to_states[[zn]]) {
    dsub <- df_st %>% filter(state_name == st)
    vec <- source_dist(dsub, "source_cat", wvar_use)
    n_nets <- safe_sum_nets(dsub$derived_num_itns)
    write_source_row(
      wb, SHEET_ST, row_of(rows_st, st, START_ROW_ST),
      vec, n_nets, COL_FIRST_SRC, COL_N_NETS, pctStyle, intStyle
    )
  }
}

vec_tot_st <- source_dist(df_st, "source_cat", wvar_use)
n_tot_st <- safe_sum_nets(df_st$derived_num_itns)
write_source_row(
  wb, SHEET_ST, row_of(rows_st, "Total", START_ROW_ST),
  vec_tot_st, n_tot_st, COL_FIRST_SRC, COL_N_NETS, pctStyle, intStyle
)

# -----------------------------
# Save
# -----------------------------
saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 3.2.1 and Table 3.2.2 written to: ", OUT_XLSX)