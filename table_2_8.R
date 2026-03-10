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
SHEET     <- "Table 2.8"
WVAR      <- "w_calibrated_trim"

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.8 weighting active: ", use_weight)

# ---- State code -> name
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

# ---- Distribution across wealth quintiles 1..5
dist_quint <- function(dat, row_var, row_val, wvar = NULL) {
  d <- dat %>%
    filter(.data[[row_var]] == row_val, !is.na(wealth_q), wealth_q %in% 1:5)
  
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (nrow(d) == 0) {
    return(tibble(
      Lowest = NA_real_, Second = NA_real_, Middle = NA_real_,
      Fourth = NA_real_, Highest = NA_real_, Total = 100.0,
      Number_of_persons = 0
    ))
  }
  
  if (is.null(wvar)) {
    sh <- d %>%
      count(wealth_q, name = "n") %>%
      mutate(p = 100 * n / sum(n))
  } else {
    sh <- d %>%
      group_by(wealth_q) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      mutate(p = 100 * w / sum(w))
  }
  
  vec <- rep(0, 5)
  names(vec) <- 1:5
  vec[as.character(sh$wealth_q)] <- round(sh$p, 1)
  
  tibble(
    Lowest = vec["1"],
    Second = vec["2"],
    Middle = vec["3"],
    Fourth = vec["4"],
    Highest = vec["5"],
    Total = 100.0,
    Number_of_persons = nrow(d)
  )
}

# ---- Build working frame
df_w <- df %>%
  filter(u_household == 1) %>%
  mutate(
    residence = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    zone = as.numeric(derived_zone),
    state = as.numeric(demo_state_num),
    wealth_q = as.numeric(derived_wealth_quintile)
  )

# ---- Skeleton rows
rows <- c(
  "Residence/region",
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
  "North Central states",
  "FCT-Abuja","Benue","Kogi","Kwara","Nasarawa","Niger","Plateau",
  "",
  "North East states",
  "Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe",
  "",
  "North West states",
  "Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara",
  "",
  "South East states",
  "Abia","Anambra","Ebonyi","Enugu","Imo",
  "",
  "South South states",
  "Akwa Ibom","Bayelsa","Cross River","Delta","Edo","Rivers",
  "",
  "South West states",
  "Ekiti","Lagos","Ogun","Ondo","Osun","Oyo",
  "",
  "Total"
)

# Columns
COL_LABEL <- 2
COL_L1 <- 3; COL_L2 <- 4; COL_L3 <- 5; COL_L4 <- 6; COL_L5 <- 7
COL_TOT <- 8
COL_N <- 9
START_ROW <- 7

row_of_exact <- function(label) {
  i <- match(label, rows)
  if (is.na(i)) return(NA_integer_)
  START_ROW + i - 1
}

write_row <- function(wb, sheet, r, vals) {
  if (is.na(r)) return(invisible(FALSE))
  writeData(wb, sheet, vals$Lowest,  startRow = r, startCol = COL_L1, colNames = FALSE)
  writeData(wb, sheet, vals$Second,  startRow = r, startCol = COL_L2, colNames = FALSE)
  writeData(wb, sheet, vals$Middle,  startRow = r, startCol = COL_L3, colNames = FALSE)
  writeData(wb, sheet, vals$Fourth,  startRow = r, startCol = COL_L4, colNames = FALSE)
  writeData(wb, sheet, vals$Highest, startRow = r, startCol = COL_L5, colNames = FALSE)
  writeData(wb, sheet, vals$Total,   startRow = r, startCol = COL_TOT, colNames = FALSE)
  writeData(wb, sheet, vals$Number_of_persons, startRow = r, startCol = COL_N, colNames = FALSE)
  invisible(TRUE)
}

# Workbook
if (file.exists(OUT_XLSX)) wb <- loadWorkbook(OUT_XLSX) else wb <- createWorkbook()
if (SHEET %in% names(wb)) removeWorksheet(wb, SHEET)
addWorksheet(wb, SHEET, gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
hdrStyle   <- createStyle(textDecoration = "bold")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
numStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(
  wb, SHEET,
  "Table 2.8  Distribution across sample-derived wealth quintiles (Sproxil respondents)",
  startRow = 1, startCol = 1
)
addStyle(wb, SHEET, titleStyle, rows = 1, cols = 1, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; counts are unweighted. Wealth quintiles are derived from a sample-based asset index and reflect relative socioeconomic position within this dataset."
} else {
  "NOTE: Unweighted percentages; counts are unweighted. Wealth quintiles are derived from a sample-based asset index and reflect relative socioeconomic position within this dataset."
}
writeData(wb, SHEET, note_txt, startRow = 2, startCol = 1)
addStyle(wb, SHEET, noteStyle, rows = 2, cols = 1, stack = TRUE)

# header row
writeData(
  wb, SHEET,
  c("Wealth quintile", "Lowest", "Second", "Middle", "Fourth", "Highest", "Total", "Number of persons"),
  startRow = 6, startCol = 2, colNames = FALSE
)
addStyle(wb, SHEET, hdrStyle, rows = 6, cols = 2:9, gridExpand = TRUE, stack = TRUE)

# labels
writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET, cols = COL_LABEL, widths = 22)
setColWidths(wb, SHEET, cols = COL_L1:COL_N, widths = c(10,10,10,10,10,10,16))

# ---- Residence rows
for (lab in c("Urban", "Rural")) {
  r <- row_of_exact(lab)
  vals <- dist_quint(df_w, "residence", lab, wvar = wvar_use)
  write_row(wb, SHEET, r, vals)
  addStyle(wb, SHEET, numStyle, rows = r, cols = COL_L1:COL_TOT, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, SHEET, intStyle, rows = r, cols = COL_N, stack = TRUE)
}

# ---- Zone rows
for (k in 1:6) {
  nm <- unname(zone_names[as.character(k)])
  r <- row_of_exact(nm)
  vals <- dist_quint(df_w, "zone", k, wvar = wvar_use)
  write_row(wb, SHEET, r, vals)
  addStyle(wb, SHEET, numStyle, rows = r, cols = COL_L1:COL_TOT, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, SHEET, intStyle, rows = r, cols = COL_N, stack = TRUE)
}

# ---- State rows grouped by zone
zone_to_states <- list(
  `1` = c(15,7,23,24,26,27,32),
  `2` = c(2,5,8,16,35,36),
  `3` = c(18,19,20,21,22,34,37),
  `4` = c(1,4,11,14,17),
  `5` = c(3,6,9,10,12,33),
  `6` = c(13,25,28,29,30,31)
)

for (z in 1:6) {
  for (st in zone_to_states[[as.character(z)]]) {
    nm <- unname(state_names[as.character(st)])
    r <- row_of_exact(nm)
    vals <- dist_quint(df_w, "state", st, wvar = wvar_use)
    write_row(wb, SHEET, r, vals)
    addStyle(wb, SHEET, numStyle, rows = r, cols = COL_L1:COL_TOT, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, SHEET, intStyle, rows = r, cols = COL_N, stack = TRUE)
  }
}

# ---- Total row
rTot <- row_of_exact("Total")
d <- df_w %>% filter(!is.na(wealth_q), wealth_q %in% 1:5)
if (!is.null(wvar_use)) d <- d %>% filter(!is.na(.data[[wvar_use]]))

if (nrow(d) == 0) {
  valsTot <- tibble(
    Lowest = NA_real_, Second = NA_real_, Middle = NA_real_,
    Fourth = NA_real_, Highest = NA_real_, Total = 100.0,
    Number_of_persons = 0
  )
} else {
  if (is.null(wvar_use)) {
    sh <- d %>% count(wealth_q, name = "n") %>% mutate(p = 100 * n / sum(n))
  } else {
    sh <- d %>%
      group_by(wealth_q) %>%
      summarise(w = sum(.data[[wvar_use]], na.rm = TRUE), .groups = "drop") %>%
      mutate(p = 100 * w / sum(w))
  }
  vec <- rep(0, 5); names(vec) <- 1:5
  vec[as.character(sh$wealth_q)] <- round(sh$p, 1)
  valsTot <- tibble(
    Lowest = vec["1"], Second = vec["2"], Middle = vec["3"],
    Fourth = vec["4"], Highest = vec["5"], Total = 100.0,
    Number_of_persons = nrow(d)
  )
}

write_row(wb, SHEET, rTot, valsTot)
addStyle(wb, SHEET, numStyle, rows = rTot, cols = COL_L1:COL_TOT, gridExpand = TRUE, stack = TRUE)
addStyle(wb, SHEET, intStyle, rows = rTot, cols = COL_N, stack = TRUE)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.8 written to: ", OUT_XLSX)