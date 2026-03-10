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
SHEET     <- "Table 2.11"
WVAR      <- "w_calibrated_trim"

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Replacement Table 2.11 weighting active: ", use_weight)

# -----------------------------
# Helpers
# -----------------------------
pct_table <- function(dat, var, levels_expected, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (nrow(d) == 0) {
    return(tibble(level = levels_expected, pct = NA_real_, un = 0L))
  }
  
  if (is.null(wvar)) {
    pct_tab <- d %>%
      count(.data[[var]], name = "n") %>%
      transmute(level = as.character(.data[[var]]),
                pct = round(100 * n / sum(n), 1))
  } else {
    pct_tab <- d %>%
      group_by(.data[[var]]) %>%
      summarise(wt = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      transmute(level = as.character(.data[[var]]),
                pct = round(100 * wt / sum(wt), 1))
  }
  
  un_tab <- d %>%
    count(.data[[var]], name = "un") %>%
    transmute(level = as.character(.data[[var]]), un = as.integer(un))
  
  full_join(pct_tab, un_tab, by = "level") %>%
    mutate(level = factor(level, levels = levels_expected)) %>%
    tidyr::complete(level, fill = list(pct = 0.0, un = 0L)) %>%
    arrange(level) %>%
    mutate(level = as.character(level))
}

write_pu <- function(wb, sheet, r, p, u, pct_col, un_col, pct_style = NULL, int_style = NULL) {
  if (is.na(r)) return(invisible(FALSE))
  writeData(wb, sheet, p, startRow = r, startCol = pct_col, colNames = FALSE)
  writeData(wb, sheet, u, startRow = r, startCol = un_col, colNames = FALSE)
  if (!is.null(pct_style)) addStyle(wb, sheet, pct_style, rows = r, cols = pct_col, stack = TRUE)
  if (!is.null(int_style)) addStyle(wb, sheet, int_style, rows = r, cols = un_col, stack = TRUE)
  invisible(TRUE)
}

# -----------------------------
# Build women 15-49 sample-profile frame
# -----------------------------
df_w <- df %>%
  filter(u_women_15_49 == 1) %>%
  mutate(
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
    religion = case_when(
      as.numeric(bg_religion) %in% c(1, 2) ~ "Christian",
      as.numeric(bg_religion) == 3 ~ "Islam",
      as.numeric(bg_religion) == 4 ~ "Traditionalist",
      as.numeric(bg_religion) == 6 ~ "Others",
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
    )
  )

age_levels <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
rel_levels <- c("Christian","Islam","Traditionalist","Others")
res_levels <- c("Urban","Rural")
zone_levels <- c("North Central","North East","North West","South East","South South","South West")
edu_levels <- c("No education","Primary","Secondary","More than secondary")
wq_levels  <- c("Lowest","Second","Middle","Fourth","Highest")

age_tab  <- pct_table(df_w, "agegrp", age_levels, wvar = wvar_use)
rel_tab  <- pct_table(df_w, "religion", rel_levels, wvar = wvar_use)
res_tab  <- pct_table(df_w, "residence", res_levels, wvar = wvar_use)
zone_tab <- pct_table(df_w, "zone", zone_levels, wvar = wvar_use)
edu_tab  <- pct_table(df_w, "edu", edu_levels, wvar = wvar_use)
wq_tab   <- pct_table(df_w, "wq", wq_levels, wvar = wvar_use)

U_total <- nrow(df_w)

# -----------------------------
# Skeleton
# -----------------------------
rows <- c(
  "Background characteristic",
  "Age",
  "15-19","20-24","25-29","30-34","35-39","40-44","45-49",
  "",
  "Religion",
  "Christian","Islam","Traditionalist","Others",
  "",
  "Residence",
  "Urban","Rural",
  "",
  "Zone",
  "North Central","North East","North West","South East","South South","South West",
  "",
  "Education",
  "No education","Primary","Secondary","More than secondary",
  "",
  "Wealth quintile",
  "Lowest","Second","Middle","Fourth","Highest",
  "",
  "Total",
  "",
  "Note: This table describes women respondents age 15-49 in the sample. Percentages may be weighted if calibrated weights are available, but the table should not be interpreted as a population distribution."
)

COL_LABEL <- 2
COL_PCT   <- 3
COL_UN    <- 4
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
pctStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(wb, SHEET,
          "Table 2.11  Background characteristics of women respondents age 15-49 (sample profile)",
          startRow = 2, startCol = 2)
addStyle(wb, SHEET, titleStyle, rows = 2, cols = 2, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; counts are unweighted. This table describes women respondents age 15-49 in the sample and should not be interpreted as a population distribution."
} else {
  "NOTE: Unweighted percentages; counts are unweighted. This table describes women respondents age 15-49 in the sample and should not be interpreted as a population distribution."
}
writeData(wb, SHEET, note_txt, startRow = 3, startCol = 2)
addStyle(wb, SHEET, noteStyle, rows = 3, cols = 2, stack = TRUE)

writeData(wb, SHEET,
          c("Women age 15-49", "Percent", "Unweighted number"),
          startRow = 6, startCol = 2, colNames = FALSE)
addStyle(wb, SHEET, hdrStyle, rows = 6, cols = 2:4, gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET, cols = COL_LABEL, widths = 40)
setColWidths(wb, SHEET, cols = COL_PCT:COL_UN, widths = c(14, 18))

fill_block <- function(tab) {
  for (i in seq_len(nrow(tab))) {
    r <- row_of(tab$level[i])
    write_pu(wb, SHEET, r, tab$pct[i], tab$un[i], COL_PCT, COL_UN, pctStyle, intStyle)
  }
}

fill_block(age_tab)
fill_block(rel_tab)
fill_block(res_tab)
fill_block(zone_tab)
fill_block(edu_tab)
fill_block(wq_tab)

rT <- row_of("Total")
write_pu(wb, SHEET, rT, 100.0, U_total, COL_PCT, COL_UN, pctStyle, intStyle)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Replacement Table 2.11 written to: ", OUT_XLSX)