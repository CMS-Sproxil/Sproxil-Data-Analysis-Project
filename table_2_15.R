suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
})

# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  Table_2_15_Exposure_to_Mass_Media_TV.R
# PURPOSE: Create Table 2.15.1 and Table 2.15.2 for weekly TV exposure
# NOTE:
# - Modified for Sproxil respondents
# - Keeps TV frequency column only
# - Region 1-6 replaced with Nigeria geopolitical zones
# - Women 15-49 universe only
# - Percentages may be weighted; counts remain unweighted
# ==============================================================================

# -----------------------------
# CONFIG
# -----------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
SHEET_NAT <- "Table 2.15.1"
SHEET_ST  <- "Table 2.15.2"
WVAR      <- "w_calibrated_trim"

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.15 weighting active: ", use_weight)

# -----------------------------
# Helpers
# -----------------------------
pct_yes <- function(dat, x, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[x]]), .data[[x]] %in% c(0, 1))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (nrow(d) == 0) return(NA_real_)
  
  if (is.null(wvar)) {
    round(100 * mean(d[[x]] == 1), 1)
  } else {
    round(100 * sum(d[[wvar]] * (d[[x]] == 1), na.rm = TRUE) /
            sum(d[[wvar]], na.rm = TRUE), 1)
  }
}

n_unw <- function(dat) nrow(dat)

write_pn <- function(wb, sheet, r, p, n, pct_col, n_col, pct_style = NULL, int_style = NULL) {
  if (is.na(r)) return(invisible(FALSE))
  writeData(wb, sheet, p, startRow = r, startCol = pct_col, colNames = FALSE)
  writeData(wb, sheet, n, startRow = r, startCol = n_col, colNames = FALSE)
  if (!is.null(pct_style)) addStyle(wb, sheet, pct_style, rows = r, cols = pct_col, stack = TRUE)
  if (!is.null(int_style)) addStyle(wb, sheet, int_style, rows = r, cols = n_col, stack = TRUE)
  invisible(TRUE)
}

row_of <- function(rows_vec, label, start_row) {
  i <- match(label, rows_vec)
  if (is.na(i)) return(NA_integer_)
  start_row + i - 1
}

# -----------------------------
# Build working frame
# -----------------------------
# bg_tv_frequency:
# 1 = at least once a week
# 2 = less frequent than weekly
# 3 = never
# Table indicator = watches television at least once a week

state_names <- c(
  `1`="Abia", `2`="Adamawa", `3`="Akwa Ibom", `4`="Anambra", `5`="Bauchi", `6`="Bayelsa",
  `7`="Benue", `8`="Borno", `9`="Cross River", `10`="Delta", `11`="Ebonyi", `12`="Edo",
  `13`="Ekiti", `14`="Enugu", `15`="FCT-Abuja", `16`="Gombe", `17`="Imo", `18`="Jigawa",
  `19`="Kaduna", `20`="Kano", `21`="Katsina", `22`="Kebbi", `23`="Kogi", `24`="Kwara",
  `25`="Lagos", `26`="Nasarawa", `27`="Niger", `28`="Ogun", `29`="Ondo", `30`="Osun",
  `31`="Oyo", `32`="Plateau", `33`="Rivers", `34`="Sokoto", `35`="Taraba", `36`="Yobe",
  `37`="Zamfara"
)

df_w <- df %>%
  filter(u_women_15_49 == 1) %>%
  mutate(
    tv_weekly = case_when(
      as.numeric(bg_tv_frequency) == 1 ~ 1,
      as.numeric(bg_tv_frequency) %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    agegrp = case_when(
      as.numeric(demo_age_num) >= 15 & as.numeric(demo_age_num) <= 19 ~ "15-19",
      as.numeric(demo_age_num) >= 20 & as.numeric(demo_age_num) <= 24 ~ "20-24",
      as.numeric(demo_age_num) >= 25 & as.numeric(demo_age_num) <= 29 ~ "25-29",
      as.numeric(demo_age_num) >= 30 & as.numeric(demo_age_num) <= 34 ~ "30-34",
      as.numeric(demo_age_num) >= 35 & as.numeric(demo_age_num) <= 39 ~ "35-39",
      as.numeric(demo_age_num) >= 40 & as.numeric(demo_age_num) <= 44 ~ "40-44",
      as.numeric(demo_age_num) >= 45 & as.numeric(demo_age_num) <= 49 ~ "45-49",
      TRUE ~ NA_character_
    ),
    residence = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    zone = case_when(
      as.numeric(derived_zone) == 1 ~ "North Central",
      as.numeric(derived_zone) == 2 ~ "North East",
      as.numeric(derived_zone) == 3 ~ "North West",
      as.numeric(derived_zone) == 4 ~ "South East",
      as.numeric(derived_zone) == 5 ~ "South South",
      as.numeric(derived_zone) == 6 ~ "South West",
      TRUE ~ NA_character_
    ),
    edu = case_when(
      as.numeric(derived_edu_cat) == 1 ~ "No education",
      as.numeric(derived_edu_cat) == 2 ~ "Primary",
      as.numeric(derived_edu_cat) == 3 ~ "Secondary",
      as.numeric(derived_edu_cat) == 4 ~ "More than secondary",
      TRUE ~ NA_character_
    ),
    wq = case_when(
      as.numeric(derived_wealth_quintile) == 1 ~ "Lowest",
      as.numeric(derived_wealth_quintile) == 2 ~ "Second",
      as.numeric(derived_wealth_quintile) == 3 ~ "Middle",
      as.numeric(derived_wealth_quintile) == 4 ~ "Fourth",
      as.numeric(derived_wealth_quintile) == 5 ~ "Highest",
      TRUE ~ NA_character_
    ),
    state_name = unname(state_names[as.character(as.numeric(demo_state_num))])
  )

# -----------------------------
# Table 2.15.1 rows
# -----------------------------
rows_nat <- c(
  "Background characteristic",
  "Age",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
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
  "Education",
  "No education",
  "Primary",
  "Secondary",
  "More than secondary",
  "",
  "Wealth quintile",
  "Lowest",
  "Second",
  "Middle",
  "Fourth",
  "Highest",
  "",
  "Total"
)

# -----------------------------
# Table 2.15.2 rows
# -----------------------------
rows_st <- c(
  "State",
  "North Central",
  "FCT-Abuja",
  "Benue",
  "Kogi",
  "Kwara",
  "Nasarawa",
  "Niger",
  "Plateau",
  "",
  "North East",
  "Adamawa",
  "Bauchi",
  "Borno",
  "Gombe",
  "Taraba",
  "Yobe",
  "",
  "North West",
  "Jigawa",
  "Kaduna",
  "Kano",
  "Katsina",
  "Kebbi",
  "Sokoto",
  "Zamfara",
  "",
  "South East",
  "Abia",
  "Anambra",
  "Ebonyi",
  "Enugu",
  "Imo",
  "",
  "South South",
  "Akwa Ibom",
  "Bayelsa",
  "Cross River",
  "Delta",
  "Edo",
  "Rivers",
  "",
  "South West",
  "Ekiti",
  "Lagos",
  "Ogun",
  "Ondo",
  "Osun",
  "Oyo",
  "",
  "Total"
)

# -----------------------------
# Create/open workbook
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
subStyle   <- createStyle(textDecoration = "bold", fgFill = "#F4B183")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
hdrStyle   <- createStyle(textDecoration = "bold")
pctStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

# ==============================================================================
# TABLE 2.15.1 NATIONAL
# ==============================================================================
START_ROW_NAT <- 8
COL_LABEL_NAT <- 2
COL_PCT_NAT   <- 3
COL_N_NAT     <- 4

writeData(wb, SHEET_NAT,
          "Table 2.15.1  Exposure to mass media: National [Bias risk: HIGH]",
          startRow = 2, startCol = 2)
addStyle(wb, SHEET_NAT, titleStyle, rows = 2, cols = 2, stack = TRUE)

writeData(wb, SHEET_NAT,
          "STATUS: Modified. Keep TV frequency column. (Sproxil respondents)",
          startRow = 3, startCol = 2)
addStyle(wb, SHEET_NAT, subStyle, rows = 3, cols = 2, stack = TRUE)

note_txt_nat <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; number of women is unweighted. This table describes women respondents age 15-49 in the sample and should not be interpreted as a population distribution."
} else {
  "NOTE: Unweighted percentages; number of women is unweighted. This table describes women respondents age 15-49 in the sample and should not be interpreted as a population distribution."
}
writeData(wb, SHEET_NAT, note_txt_nat, startRow = 4, startCol = 2)
addStyle(wb, SHEET_NAT, noteStyle, rows = 4, cols = 2, stack = TRUE)

writeData(wb, SHEET_NAT,
          c("Background characteristic", "Watches television at least once a week", "Number of women"),
          startRow = START_ROW_NAT - 1, startCol = COL_LABEL_NAT, colNames = FALSE)
addStyle(wb, SHEET_NAT, hdrStyle, rows = START_ROW_NAT - 1, cols = COL_LABEL_NAT:COL_N_NAT,
         gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET_NAT, rows_nat, startRow = START_ROW_NAT, startCol = COL_LABEL_NAT, colNames = FALSE)

setColWidths(wb, SHEET_NAT, cols = COL_LABEL_NAT, widths = 28)
setColWidths(wb, SHEET_NAT, cols = COL_PCT_NAT, widths = 18)
setColWidths(wb, SHEET_NAT, cols = COL_N_NAT, widths = 14)

# Age
for (lab in c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")) {
  dsub <- df_w %>% filter(agegrp == lab)
  r <- row_of(rows_nat, lab, START_ROW_NAT)
  write_pn(
    wb, SHEET_NAT, r,
    pct_yes(dsub, "tv_weekly", wvar_use),
    n_unw(dsub),
    COL_PCT_NAT, COL_N_NAT, pctStyle, intStyle
  )
}

# Residence
for (lab in c("Urban","Rural")) {
  dsub <- df_w %>% filter(residence == lab)
  r <- row_of(rows_nat, lab, START_ROW_NAT)
  write_pn(
    wb, SHEET_NAT, r,
    pct_yes(dsub, "tv_weekly", wvar_use),
    n_unw(dsub),
    COL_PCT_NAT, COL_N_NAT, pctStyle, intStyle
  )
}

# Zones
for (lab in c("North Central","North East","North West","South East","South South","South West")) {
  dsub <- df_w %>% filter(zone == lab)
  r <- row_of(rows_nat, lab, START_ROW_NAT)
  write_pn(
    wb, SHEET_NAT, r,
    pct_yes(dsub, "tv_weekly", wvar_use),
    n_unw(dsub),
    COL_PCT_NAT, COL_N_NAT, pctStyle, intStyle
  )
}

# Education
for (lab in c("No education","Primary","Secondary","More than secondary")) {
  dsub <- df_w %>% filter(edu == lab)
  r <- row_of(rows_nat, lab, START_ROW_NAT)
  write_pn(
    wb, SHEET_NAT, r,
    pct_yes(dsub, "tv_weekly", wvar_use),
    n_unw(dsub),
    COL_PCT_NAT, COL_N_NAT, pctStyle, intStyle
  )
}

# Wealth quintile
for (lab in c("Lowest","Second","Middle","Fourth","Highest")) {
  dsub <- df_w %>% filter(wq == lab)
  r <- row_of(rows_nat, lab, START_ROW_NAT)
  write_pn(
    wb, SHEET_NAT, r,
    pct_yes(dsub, "tv_weekly", wvar_use),
    n_unw(dsub),
    COL_PCT_NAT, COL_N_NAT, pctStyle, intStyle
  )
}

# Total
r_tot_nat <- row_of(rows_nat, "Total", START_ROW_NAT)
write_pn(
  wb, SHEET_NAT, r_tot_nat,
  pct_yes(df_w, "tv_weekly", wvar_use),
  n_unw(df_w),
  COL_PCT_NAT, COL_N_NAT, pctStyle, intStyle
)

# ==============================================================================
# TABLE 2.15.2 STATES
# ==============================================================================
START_ROW_ST <- 9
COL_LABEL_ST <- 2
COL_PCT_ST   <- 3
COL_N_ST     <- 4

writeData(wb, SHEET_ST,
          "Table 2.15.2  Exposure to mass media: State",
          startRow = 2, startCol = 2)
addStyle(wb, SHEET_ST, titleStyle, rows = 2, cols = 2, stack = TRUE)

writeData(wb, SHEET_ST,
          "Percentage of women age 15-49 who are exposed to television at least once a week, according to background characteristics",
          startRow = 3, startCol = 2)
addStyle(wb, SHEET_ST, noteStyle, rows = 3, cols = 2, stack = TRUE)

note_txt_st <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; number of women is unweighted. State estimates are sample-based and should be interpreted with caution."
} else {
  "NOTE: Unweighted percentages; number of women is unweighted. State estimates are sample-based and should be interpreted with caution."
}
writeData(wb, SHEET_ST, note_txt_st, startRow = 4, startCol = 2)
addStyle(wb, SHEET_ST, noteStyle, rows = 4, cols = 2, stack = TRUE)

writeData(wb, SHEET_ST,
          c("State", "Watches television at least once a week", "Number of women"),
          startRow = START_ROW_ST - 1, startCol = COL_LABEL_ST, colNames = FALSE)
addStyle(wb, SHEET_ST, hdrStyle, rows = START_ROW_ST - 1, cols = COL_LABEL_ST:COL_N_ST,
         gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET_ST, rows_st, startRow = START_ROW_ST, startCol = COL_LABEL_ST, colNames = FALSE)

setColWidths(wb, SHEET_ST, cols = COL_LABEL_ST, widths = 22)
setColWidths(wb, SHEET_ST, cols = COL_PCT_ST, widths = 18)
setColWidths(wb, SHEET_ST, cols = COL_N_ST, widths = 14)

state_order <- c(
  "FCT-Abuja","Benue","Kogi","Kwara","Nasarawa","Niger","Plateau",
  "Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe",
  "Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara",
  "Abia","Anambra","Ebonyi","Enugu","Imo",
  "Akwa Ibom","Bayelsa","Cross River","Delta","Edo","Rivers",
  "Ekiti","Lagos","Ogun","Ondo","Osun","Oyo"
)

for (lab in state_order) {
  dsub <- df_w %>% filter(state_name == lab)
  r <- row_of(rows_st, lab, START_ROW_ST)
  write_pn(
    wb, SHEET_ST, r,
    pct_yes(dsub, "tv_weekly", wvar_use),
    n_unw(dsub),
    COL_PCT_ST, COL_N_ST, pctStyle, intStyle
  )
}

r_tot_st <- row_of(rows_st, "Total", START_ROW_ST)
write_pn(
  wb, SHEET_ST, r_tot_st,
  pct_yes(df_w, "tv_weekly", wvar_use),
  n_unw(df_w),
  COL_PCT_ST, COL_N_ST, pctStyle, intStyle
)

# -----------------------------
# Save
# -----------------------------
saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.15.1 and Table 2.15.2 written to: ", OUT_XLSX)