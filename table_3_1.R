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
message("3.1 weighting active: ", use_weight)

# ---- labels ----
res_lab  <- c(`1`="Urban", `2`="Rural")
zone_lab <- c(
  `1`="North Central", `2`="North East", `3`="North West",
  `4`="South East", `5`="South South", `6`="South West"
)
wq_lab   <- c(`1`="Lowest",`2`="Second",`3`="Middle",`4`="Fourth",`5`="Highest")

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

# ---- helpers ----
w_mean <- function(x, wts = NULL) {
  if (is.null(wts)) return(mean(x, na.rm = TRUE))
  ok <- !is.na(x) & !is.na(wts)
  if (!any(ok)) return(NA_real_)
  sum(wts[ok] * x[ok]) / sum(wts[ok])
}

pct_yes <- function(x, wts = NULL) round(100 * w_mean(as.numeric(x == 1), wts), 1)
avg_val <- function(x, wts = NULL) round(w_mean(x, wts), 1)

# ---- build analysis columns (household level) ----
d <- df %>%
  mutate(
    any_net = case_when(
      derived_hh_has_any_net == 1 ~ 1,
      derived_hh_has_any_net == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    itn_net = case_when(
      derived_hh_has_itn == 1 ~ 1,
      derived_hh_has_itn == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    n_nets = suppressWarnings(as.numeric(prev_num_mosquito_nets)),
    n_itn  = suppressWarnings(as.numeric(derived_num_itns)),
    hh_size = suppressWarnings(as.numeric(hh_total_persons_v1)),
    
    access_any = case_when(
      is.na(n_nets) | is.na(hh_size) | hh_size <= 0 ~ NA_real_,
      n_nets %in% c(98, 99) | hh_size %in% c(98, 99) ~ NA_real_,
      TRUE ~ as.numeric(n_nets * 2 >= hh_size)
    ),
    
    access_itn = case_when(
      is.na(derived_access_itn) ~ NA_real_,
      TRUE ~ as.numeric(derived_access_itn)
    )
  ) %>%
  filter(u_household == 1)

# protect averages
d <- d %>%
  mutate(
    n_nets = ifelse(n_nets %in% c(98, 99), NA_real_, n_nets),
    n_itn  = ifelse(n_itn  %in% c(98, 99), NA_real_, n_itn),
    hh_size = ifelse(hh_size %in% c(98, 99), NA_real_, hh_size)
  )

# ---- row calculator ----
calc_row <- function(dat) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  
  tibble(
    `Any mosquito net` = pct_yes(dat$any_net, wts),
    `Insecticide-treated mosquito net (ITN)¹` = pct_yes(dat$itn_net, wts),
    
    `Any mosquito net (avg)` = avg_val(dat$n_nets, wts),
    `Insecticide-treated mosquito net (ITN)¹ (avg)` = avg_val(dat$n_itn, wts),
    
    `Access proxy (Any net)` = round(100 * w_mean(dat$access_any, wts), 1),
    `Access proxy (ITN)¹`    = round(100 * w_mean(dat$access_itn, wts), 1),
    
    `Number of households` = nrow(dat)
  )
}

make_block <- function(dat, gvar, levels_vec, label_map, header) {
  out <- lapply(levels_vec, function(k) {
    dd <- dat %>% filter(!is.na(.data[[gvar]]), .data[[gvar]] == k)
    tibble(`Background characteristic` = unname(label_map[as.character(k)])) %>%
      bind_cols(calc_row(dd))
  }) %>% bind_rows()
  
  bind_rows(
    tibble(`Background characteristic` = header),
    out,
    tibble(`Background characteristic` = "")
  )
}

# =========================
# Table 3.1.1 National
# =========================
t311 <- bind_rows(
  make_block(d, "derived_residence", 1:2, res_lab, "Residence"),
  make_block(d, "derived_zone", 1:6, zone_lab, "Zone"),
  make_block(d, "derived_wealth_quintile", 1:5, wq_lab, "Wealth quintile"),
  tibble(`Background characteristic` = "Total") %>% bind_cols(calc_row(d))
)

# =========================
# Table 3.1.2 States
# =========================
d_state <- d %>%
  filter(!is.na(demo_state_num), demo_state_num %in% 1:37,
         !is.na(derived_zone), derived_zone %in% 1:6)

row_state <- function(s) {
  dd <- d_state %>% filter(demo_state_num == s)
  tibble(State = state_names[as.character(s)]) %>%
    bind_cols(calc_row(dd))
}

t312 <- tibble()
for (z in 1:6) {
  t312 <- bind_rows(t312, tibble(State = zone_header[as.character(z)]))
  for (s in zone_to_states[[as.character(z)]]) {
    t312 <- bind_rows(t312, row_state(s))
  }
  t312 <- bind_rows(t312, tibble(State = ""))
}
t312 <- bind_rows(t312, tibble(State = "Total") %>% bind_cols(calc_row(d_state)))

# ---- write workbook ----
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
    "NOTE: Percentages and averages weighted using calibrated trimmed weights; household counts are unweighted."
  } else {
    "NOTE: Unweighted percentages and averages; household counts are unweighted."
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
  
  count_cols <- which(names(dfout) %in% c("Number of households"))
  num_cols <- setdiff(2:ncol(dfout), count_cols)
  
  if (length(num_cols) > 0) {
    addStyle(
      wb, sheet, numStyle,
      rows = (data_start + 1):(data_start + nrow(dfout)),
      cols = (1 + num_cols),
      gridExpand = TRUE, stack = TRUE
    )
  }
  if (length(count_cols) > 0) {
    addStyle(
      wb, sheet, intStyle,
      rows = (data_start + 1):(data_start + nrow(dfout)),
      cols = (1 + count_cols),
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  writeData(wb, sheet, footnote, startRow = data_start + nrow(dfout) + 2, startCol = 2)
  addStyle(wb, sheet, noteStyle, rows = data_start + nrow(dfout) + 2, cols = 2, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 36)
  setColWidths(wb, sheet, cols = 3:(ncol(dfout) + 1), widths = 18)
}

write_sheet(
  "Table 3.1.1",
  "Table 3.1.1 Household possession of mosquito nets (Sproxil respondents)",
  t311,
  "¹ An insecticide-treated net (ITN) is a factory-treated net that does not require any further treatment (LLIN). Access proxy uses usual household members."
)

write_sheet(
  "Table 3.1.2",
  "Table 3.1.2 Household possession of mosquito nets: States (Sproxil respondents)",
  t312,
  "¹ An insecticide-treated net (ITN) is a factory-treated net that does not require any further treatment (LLIN). Access proxy uses usual household members.",
  cautious_note = "State-level results are descriptive of interviewed households in the sample and should be interpreted cautiously."
)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Wrote Table 3.1.1 and 3.1.2 to: ", OUT_XLSX)