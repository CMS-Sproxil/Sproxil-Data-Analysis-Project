suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

# -----------------------------
# CONFIG
# -----------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
SHEET     <- "Table 2.9"
WVAR      <- "w_calibrated_trim"

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.9 weighting active: ", use_weight)

# -----------------------------
# Helpers
# -----------------------------
age_5yr_15_64 <- function(a) {
  case_when(
    is.na(a) ~ NA_character_,
    a < 15 | a > 64 ~ NA_character_,
    a >= 15 & a <= 19 ~ "15-19",
    a >= 20 & a <= 24 ~ "20-24",
    a >= 25 & a <= 29 ~ "25-29",
    a >= 30 & a <= 34 ~ "30-34",
    a >= 35 & a <= 39 ~ "35-39",
    a >= 40 & a <= 44 ~ "40-44",
    a >= 45 & a <= 49 ~ "45-49",
    a >= 50 & a <= 54 ~ "50-54",
    a >= 55 & a <= 59 ~ "55-59",
    a >= 60 & a <= 64 ~ "60-64",
    TRUE ~ NA_character_
  )
}

pct_dist <- function(dat, age_var, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[age_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (nrow(d) == 0) return(tibble(age = character(), p = numeric()))
  
  if (is.null(wvar)) {
    d %>%
      count(.data[[age_var]], name = "n") %>%
      mutate(p = 100 * n / sum(n)) %>%
      transmute(age = as.character(.data[[age_var]]), p = round(p, 1))
  } else {
    d %>%
      group_by(.data[[age_var]]) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      mutate(p = 100 * w / sum(w)) %>%
      transmute(age = as.character(.data[[age_var]]), p = round(p, 1))
  }
}

# -----------------------------
# Build analysis frame
# -----------------------------
df_t29 <- df %>%
  filter(u_all == 1) %>%
  mutate(
    res = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    sex = case_when(
      as.numeric(demo_gender_num) == 1 ~ "Male",
      as.numeric(demo_gender_num) == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    agegrp = age_5yr_15_64(as.numeric(demo_age_num))
  ) %>%
  filter(!is.na(res), !is.na(sex), !is.na(agegrp))

age_levels <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")

# Create 6 column groups
get_col <- function(residence, sexlab) {
  d <- df_t29 %>% filter(res == residence)
  if (sexlab %in% c("Male", "Female")) d <- d %>% filter(sex == sexlab)
  
  pct_dist(d, "agegrp", wvar = wvar_use) %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    tidyr::complete(age, fill = list(p = 0.0)) %>%
    arrange(age) %>%
    mutate(age = as.character(age))
}

U_M <- get_col("Urban", "Male")
U_F <- get_col("Urban", "Female")
U_T <- get_col("Urban", "Total")

R_M <- get_col("Rural", "Male")
R_F <- get_col("Rural", "Female")
R_T <- get_col("Rural", "Total")

# N persons (unweighted counts)
N_unw <- function(residence, sexlab) {
  d <- df_t29 %>% filter(res == residence)
  if (sexlab %in% c("Male", "Female")) d <- d %>% filter(sex == sexlab)
  nrow(d)
}

Nm <- c(
  U_M = N_unw("Urban", "Male"),
  U_F = N_unw("Urban", "Female"),
  U_T = N_unw("Urban", "Total"),
  R_M = N_unw("Rural", "Male"),
  R_F = N_unw("Rural", "Female"),
  R_T = N_unw("Rural", "Total")
)

# -----------------------------
# Skeleton
# -----------------------------
rows <- c(
  "Age",
  "15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64",
  "",
  "Total",
  "",
  "Number of persons"
)

COL_LABEL <- 2
COL_UM <- 3; COL_UF <- 4; COL_UT <- 5
COL_RM <- 7; COL_RF <- 8; COL_RT <- 9
START_ROW <- 8

row_of <- function(label) {
  i <- match(label, rows)
  if (is.na(i)) return(NA_integer_)
  START_ROW + i - 1
}

# -----------------------------
# Workbook
# -----------------------------
if (file.exists(OUT_XLSX)) wb <- loadWorkbook(OUT_XLSX) else wb <- createWorkbook()
if (SHEET %in% names(wb)) removeWorksheet(wb, SHEET)
addWorksheet(wb, SHEET, gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
hdrStyle   <- createStyle(textDecoration = "bold")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
numStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(wb, SHEET, "Table 2.9  Respondent sample by age, sex, and residence (Sproxil respondents)", startRow = 2, startCol = 2)
addStyle(wb, SHEET, titleStyle, rows = 2, cols = 2, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; counts are unweighted. This table describes the respondent sample and should not be interpreted as a population age-sex distribution."
} else {
  "NOTE: Unweighted percentages; counts are unweighted. This table describes the respondent sample and should not be interpreted as a population age-sex distribution."
}
writeData(wb, SHEET, note_txt, startRow = 3, startCol = 2)
addStyle(wb, SHEET, noteStyle, rows = 3, cols = 2, stack = TRUE)

# Headers
writeData(wb, SHEET, "Age", startRow = START_ROW - 2, startCol = 2)
writeData(wb, SHEET, "Urban", startRow = START_ROW - 2, startCol = COL_UM)
writeData(wb, SHEET, c("Male", "Female", "Total"), startRow = START_ROW - 1, startCol = COL_UM, colNames = FALSE)
writeData(wb, SHEET, "Rural", startRow = START_ROW - 2, startCol = COL_RM)
writeData(wb, SHEET, c("Male", "Female", "Total"), startRow = START_ROW - 1, startCol = COL_RM, colNames = FALSE)
addStyle(wb, SHEET, hdrStyle, rows = (START_ROW - 2):(START_ROW - 1), cols = COL_UM:COL_UT, gridExpand = TRUE, stack = TRUE)
addStyle(wb, SHEET, hdrStyle, rows = (START_ROW - 2):(START_ROW - 1), cols = COL_RM:COL_RT, gridExpand = TRUE, stack = TRUE)

# Row labels
writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET, cols = COL_LABEL, widths = 14)
setColWidths(wb, SHEET, cols = c(COL_UM:COL_UT, COL_RM:COL_RT), widths = 11)

# Fill age rows
for (lab in age_levels) {
  r <- row_of(lab)
  writeData(wb, SHEET, U_M$p[U_M$age == lab], startRow = r, startCol = COL_UM, colNames = FALSE)
  writeData(wb, SHEET, U_F$p[U_F$age == lab], startRow = r, startCol = COL_UF, colNames = FALSE)
  writeData(wb, SHEET, U_T$p[U_T$age == lab], startRow = r, startCol = COL_UT, colNames = FALSE)
  writeData(wb, SHEET, R_M$p[R_M$age == lab], startRow = r, startCol = COL_RM, colNames = FALSE)
  writeData(wb, SHEET, R_F$p[R_F$age == lab], startRow = r, startCol = COL_RF, colNames = FALSE)
  writeData(wb, SHEET, R_T$p[R_T$age == lab], startRow = r, startCol = COL_RT, colNames = FALSE)
  addStyle(wb, SHEET, numStyle, rows = r, cols = c(COL_UM:COL_UT, COL_RM:COL_RT), gridExpand = TRUE, stack = TRUE)
}

# Total row = 100
rTot <- row_of("Total")
for (cc in c(COL_UM:COL_UT, COL_RM:COL_RT)) {
  writeData(wb, SHEET, 100.0, startRow = rTot, startCol = cc, colNames = FALSE)
}
addStyle(wb, SHEET, numStyle, rows = rTot, cols = c(COL_UM:COL_UT, COL_RM:COL_RT), gridExpand = TRUE, stack = TRUE)

# Number of persons row (unweighted)
rN <- row_of("Number of persons")
valsN <- c(Nm["U_M"], Nm["U_F"], Nm["U_T"], Nm["R_M"], Nm["R_F"], Nm["R_T"])
colsN <- c(COL_UM, COL_UF, COL_UT, COL_RM, COL_RF, COL_RT)
for (k in seq_along(colsN)) {
  writeData(wb, SHEET, as.integer(valsN[k]), startRow = rN, startCol = colsN[k], colNames = FALSE)
}
addStyle(wb, SHEET, intStyle, rows = rN, cols = colsN, gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.9 written to: ", OUT_XLSX)