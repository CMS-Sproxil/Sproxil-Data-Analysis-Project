suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
})

INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
WVAR      <- "w_calibrated_trim"

df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
w <- if (use_weight) WVAR else NULL
message("3.3/3.4 weighting active: ", use_weight)

res_lab  <- c(`1`="Urban", `2`="Rural")
zone_lab <- c(
  `1`="North Central", `2`="North East", `3`="North West",
  `4`="South East", `5`="South South", `6`="South West"
)

state_names <- c(
  `1`="Abia",`2`="Adamawa",`3`="Akwa Ibom",`4`="Anambra",`5`="Bauchi",`6`="Bayelsa",
  `7`="Benue",`8`="Borno",`9`="Cross River",`10`="Delta",`11`="Ebonyi",`12`="Edo",
  `13`="Ekiti",`14`="Enugu",`15`="FCT-Abuja",`16`="Gombe",`17`="Imo",`18`="Jigawa",
  `19`="Kaduna",`20`="Kano",`21`="Katsina",`22`="Kebbi",`23`="Kogi",`24`="Kwara",
  `25`="Lagos",`26`="Nasarawa",`27`="Niger",`28`="Ogun",`29`="Ondo",`30`="Osun",
  `31`="Oyo",`32`="Plateau",`33`="Rivers",`34`="Sokoto",`35`="Taraba",`36`="Yobe",
  `37`="Zamfara"
)

zone_header <- c(
  `1`="North Central",`2`="North East",`3`="North West",
  `4`="South East",`5`="South South",`6`="South West"
)

zone_to_states <- list(
  `1` = c(15, 7, 23, 24, 26, 27, 32),
  `2` = c(2, 5, 8, 16, 35, 36),
  `3` = c(18, 19, 20, 21, 22, 34, 37),
  `4` = c(1, 4, 11, 14, 17),
  `5` = c(3, 6, 9, 10, 12, 33),
  `6` = c(13, 25, 28, 29, 30, 31)
)

w_mean <- function(x, wts = NULL) {
  if (is.null(wts)) return(mean(x, na.rm = TRUE))
  ok <- !is.na(x) & !is.na(wts)
  if (!any(ok)) return(NA_real_)
  sum(wts[ok] * x[ok]) / sum(wts[ok])
}

d <- df %>%
  mutate(
    access_itn = case_when(
      is.na(derived_access_itn) ~ NA_real_,
      TRUE ~ as.numeric(derived_access_itn)
    )
  ) %>%
  filter(u_household == 1)

calc <- function(dat) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  tibble(
    `Percentage of households with ITN access proxy (≥1 ITN per two usual members)` =
      round(100 * w_mean(dat$access_itn, wts), 1),
    `Number of households` = nrow(dat)
  )
}

blank_nat <- function(label = "") {
  tibble(
    `Background characteristic` = label,
    `Percentage of households with ITN access proxy (≥1 ITN per two usual members)` = NA_real_,
    `Number of households` = NA_real_
  )
}

blank_state <- function(label = "") {
  tibble(
    State = label,
    `Percentage of households with ITN access proxy (≥1 ITN per two usual members)` = NA_real_,
    `Number of households` = NA_real_
  )
}

make_block <- function(dat, gvar, levels_vec, label_map, header) {
  out <- lapply(levels_vec, function(k) {
    dd <- dat %>% filter(!is.na(.data[[gvar]]), .data[[gvar]] == k)
    tibble(`Background characteristic` = unname(label_map[as.character(k)])) %>%
      bind_cols(calc(dd))
  }) %>% bind_rows()
  
  bind_rows(
    blank_nat(header),
    out,
    blank_nat("")
  )
}

# ---- Table 3.3.1 National ----
tab331 <- bind_rows(
  make_block(d, "derived_residence", 1:2, res_lab, "Residence"),
  make_block(d, "derived_zone", 1:6, zone_lab, "Zone")
)
tab331 <- bind_rows(tab331, tibble(`Background characteristic` = "Total") %>% bind_cols(calc(d)))

# ---- Table 3.4 States ----
d_state <- d %>%
  filter(!is.na(demo_state_num), demo_state_num %in% 1:37,
         !is.na(derived_zone), derived_zone %in% 1:6)

tab34 <- tibble()
for (z in 1:6) {
  tab34 <- bind_rows(
    tab34,
    blank_state(zone_header[as.character(z)])
  )
  
  for (s in zone_to_states[[as.character(z)]]) {
    dd <- d_state %>% filter(demo_state_num == s)
    tab34 <- bind_rows(
      tab34,
      tibble(State = state_names[as.character(s)]) %>% bind_cols(calc(dd))
    )
  }
  
  tab34 <- bind_rows(tab34, blank_state(""))
}
tab34 <- bind_rows(tab34, tibble(State = "Total") %>% bind_cols(calc(d_state)))

# ---- write ----
wb <- if (file.exists(OUT_XLSX)) loadWorkbook(OUT_XLSX) else createWorkbook()

write_sheet <- function(sheet, title, dfout, footnote, cautious_note = NULL) {
  if (sheet %in% names(wb)) removeWorksheet(wb, sheet)
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
  noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
  hdrStyle   <- createStyle(textDecoration = "bold")
  numStyle   <- createStyle(numFmt = "0.0")
  intStyle   <- createStyle(numFmt = "0")
  
  writeData(wb, sheet, title, startRow = 2, startCol = 2)
  addStyle(wb, sheet, titleStyle, rows = 2, cols = 2, stack = TRUE)
  
  note_txt <- if (use_weight) {
    "NOTE: Percentages weighted using calibrated trimmed weights; household counts are unweighted."
  } else {
    "NOTE: Unweighted percentages; household counts are unweighted."
  }
  writeData(wb, sheet, note_txt, startRow = 3, startCol = 2)
  addStyle(wb, sheet, noteStyle, rows = 3, cols = 2, stack = TRUE)
  
  if (!is.null(cautious_note)) {
    writeData(wb, sheet, cautious_note, startRow = 4, startCol = 2)
    addStyle(wb, sheet, noteStyle, rows = 4, cols = 2, stack = TRUE)
    data_start <- 6
  } else {
    data_start <- 5
  }
  
  writeData(wb, sheet, dfout, startRow = data_start, startCol = 2, colNames = TRUE)
  addStyle(wb, sheet, hdrStyle, rows = data_start, cols = 2:(ncol(dfout) + 1), gridExpand = TRUE, stack = TRUE)
  
  addStyle(
    wb, sheet, numStyle,
    rows = (data_start + 1):(data_start + nrow(dfout)),
    cols = 3,
    gridExpand = TRUE, stack = TRUE
  )
  addStyle(
    wb, sheet, intStyle,
    rows = (data_start + 1):(data_start + nrow(dfout)),
    cols = 4,
    gridExpand = TRUE, stack = TRUE
  )
  
  writeData(wb, sheet, footnote, startRow = data_start + nrow(dfout) + 2, startCol = 2)
  addStyle(wb, sheet, noteStyle, rows = data_start + nrow(dfout) + 2, cols = 2, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 40)
  setColWidths(wb, sheet, cols = 3:4, widths = 20)
}

write_sheet(
  "Table 3.3.1",
  "Table 3.3.1 Access to an insecticide-treated net (ITN): Household access proxy (Sproxil respondents)",
  tab331,
  "¹ ITN includes LLIN. Proxy: household has at least one ITN for every two usual household members."
)

write_sheet(
  "Table 3.4",
  "Table 3.4 Access to an insecticide-treated net (ITN): Household access proxy by state (Sproxil respondents)",
  tab34,
  "¹ ITN includes LLIN. Proxy: household has at least one ITN for every two usual household members.",
  cautious_note = "State-level results are descriptive of interviewed households in the sample and should be interpreted cautiously."
)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Wrote Table 3.3.1 and 3.4 to: ", OUT_XLSX)