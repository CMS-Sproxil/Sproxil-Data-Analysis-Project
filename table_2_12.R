suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
  library(stringr)
})

INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
WVAR      <- "w_calibrated_trim"

df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("2.12 weighting active: ", use_weight)

# ---- labels ----
age_lab <- c(`1`="15-19",`2`="20-24",`3`="25-29",`4`="30-34",`5`="35-39",`6`="40-44",`7`="45-49")
res_lab <- c(`1`="Urban",`2`="Rural")
zone_lab <- c(
  `1`="North Central",
  `2`="North East",
  `3`="North West",
  `4`="South East",
  `5`="South South",
  `6`="South West"
)
wq_lab <- c(`1`="Lowest",`2`="Second",`3`="Middle",`4`="Fourth",`5`="Highest")
edu_lab <- c(`1`="No education¹",`2`="Completed primary",`3`="Completed secondary",`4`="More than secondary")

# State code mapping
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
  `1`="North Central",
  `2`="North East",
  `3`="North West",
  `4`="South East",
  `5`="South South",
  `6`="South West"
)

zone_to_states <- list(
  `1` = c(15, 7, 23, 24, 26, 27, 32),
  `2` = c(2, 5, 8, 16, 35, 36),
  `3` = c(18, 19, 20, 21, 22, 34, 37),
  `4` = c(1, 4, 11, 14, 17),
  `5` = c(3, 6, 9, 10, 12, 33),
  `6` = c(13, 25, 28, 29, 30, 31)
)

# ---- helper: row-wise education distribution ----
edu_dist <- function(dat, row_var, row_levels, wvar = NULL) {
  d <- dat %>%
    filter(
      !is.na(.data[[row_var]]),
      .data[[row_var]] %in% row_levels,
      !is.na(derived_edu_cat),
      derived_edu_cat %in% 1:4
    )
  
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (nrow(d) == 0) {
    return(
      tibble(row = as.character(row_levels)) %>%
        mutate(
          `No education¹`       = 0.0,
          `Completed primary`   = 0.0,
          `Completed secondary` = 0.0,
          `More than secondary` = 0.0,
          Total = 100.0
        )
    )
  }
  
  if (is.null(wvar)) {
    out <- d %>%
      count(.data[[row_var]], derived_edu_cat, name = "n") %>%
      group_by(.data[[row_var]]) %>%
      mutate(p = 100 * n / sum(n)) %>%
      ungroup()
  } else {
    out <- d %>%
      group_by(.data[[row_var]], derived_edu_cat) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[row_var]]) %>%
      mutate(p = 100 * w / sum(w)) %>%
      ungroup()
  }
  
  out %>%
    mutate(
      row = as.character(.data[[row_var]]),
      col = as.character(derived_edu_cat),
      p = round(p, 1)
    ) %>%
    select(row, col, p) %>%
    complete(row = as.character(row_levels), col = as.character(1:4), fill = list(p = 0.0)) %>%
    pivot_wider(names_from = col, values_from = p) %>%
    transmute(
      row,
      `No education¹`       = `1`,
      `Completed primary`   = `2`,
      `Completed secondary` = `3`,
      `More than secondary` = `4`,
      Total = 100.0
    )
}

n_by <- function(dat, row_var, row_levels) {
  dat %>%
    filter(!is.na(.data[[row_var]]), .data[[row_var]] %in% row_levels) %>%
    group_by(.data[[row_var]]) %>%
    summarise(N = n(), .groups = "drop") %>%
    mutate(row = as.character(.data[[row_var]])) %>%
    select(row, N) %>%
    right_join(tibble(row = as.character(row_levels)), by = "row") %>%
    mutate(N = replace_na(N, 0L))
}

combine_block <- function(tblock, nblock) {
  tblock %>%
    left_join(nblock, by = "row") %>%
    select(
      Background, Label,
      `No education¹`, `Completed primary`, `Completed secondary`, `More than secondary`,
      Total,
      `Number of persons` = N
    )
}

women <- df %>% filter(u_women_15_49 == 1)

# ============================
# Table 2.12.1 (National)
# ============================
t_age <- edu_dist(women, "derived_age_group_w", 1:7, wvar_use) %>%
  mutate(Background = "Age", Label = age_lab[row])
n_age <- n_by(women, "derived_age_group_w", 1:7)

t_res <- edu_dist(women, "derived_residence", 1:2, wvar_use) %>%
  mutate(Background = "Residence", Label = res_lab[row])
n_res <- n_by(women, "derived_residence", 1:2)

t_zone <- edu_dist(women, "derived_zone", 1:6, wvar_use) %>%
  mutate(Background = "Zone", Label = zone_lab[row])
n_zone <- n_by(women, "derived_zone", 1:6)

t_wq <- edu_dist(women, "derived_wealth_quintile", 1:5, wvar_use) %>%
  mutate(Background = "Wealth quintile", Label = wq_lab[row])
n_wq <- n_by(women, "derived_wealth_quintile", 1:5)

# total row
total_dist <- edu_dist(women %>% mutate(TotalRow = 1), "TotalRow", 1, wvar_use)
total_n    <- nrow(women)

tab2121 <- bind_rows(
  tibble(Background = "Age", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  combine_block(t_age, n_age),
  
  tibble(Background = "", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  
  tibble(Background = "Residence", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  combine_block(t_res, n_res),
  
  tibble(Background = "", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  
  tibble(Background = "Zone", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  combine_block(t_zone, n_zone),
  
  tibble(Background = "", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  
  tibble(Background = "Wealth quintile", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  combine_block(t_wq, n_wq),
  
  tibble(Background = "", Label = "", `No education¹` = NA, `Completed primary` = NA,
         `Completed secondary` = NA, `More than secondary` = NA, Total = NA, `Number of persons` = NA),
  
  tibble(
    Background = "",
    Label = "Total",
    `No education¹`       = total_dist$`No education¹`[1],
    `Completed primary`   = total_dist$`Completed primary`[1],
    `Completed secondary` = total_dist$`Completed secondary`[1],
    `More than secondary` = total_dist$`More than secondary`[1],
    Total = 100.0,
    `Number of persons` = total_n
  )
)

# ============================
# Table 2.12.2 (States)
# ============================
women_state <- women %>%
  filter(
    !is.na(demo_state_num), demo_state_num %in% 1:37,
    !is.na(derived_zone), derived_zone %in% 1:6
  )

state_dist <- function(dat, wvar = NULL) {
  d <- dat %>%
    filter(
      !is.na(demo_state_num), demo_state_num %in% 1:37,
      !is.na(derived_edu_cat), derived_edu_cat %in% 1:4
    )
  
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (nrow(d) == 0) {
    return(
      tibble(state = as.character(1:37)) %>%
        mutate(
          `No education¹`       = 0.0,
          `Completed primary`   = 0.0,
          `Completed secondary` = 0.0,
          `More than secondary` = 0.0,
          Total = 100.0,
          N = 0L
        )
    )
  }
  
  if (is.null(wvar)) {
    out <- d %>%
      count(demo_state_num, derived_edu_cat, name = "n") %>%
      group_by(demo_state_num) %>%
      mutate(p = 100 * n / sum(n)) %>%
      ungroup()
  } else {
    out <- d %>%
      group_by(demo_state_num, derived_edu_cat) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      group_by(demo_state_num) %>%
      mutate(p = 100 * w / sum(w)) %>%
      ungroup()
  }
  
  wide <- out %>%
    mutate(state = as.character(demo_state_num), col = as.character(derived_edu_cat), p = round(p, 1)) %>%
    select(state, col, p) %>%
    complete(state = as.character(1:37), col = as.character(1:4), fill = list(p = 0.0)) %>%
    pivot_wider(names_from = col, values_from = p) %>%
    transmute(
      state,
      `No education¹`       = `1`,
      `Completed primary`   = `2`,
      `Completed secondary` = `3`,
      `More than secondary` = `4`,
      Total = 100.0
    )
  
  nst <- d %>%
    count(demo_state_num, name = "N") %>%
    mutate(state = as.character(demo_state_num)) %>%
    select(state, N)
  
  wide %>%
    left_join(nst, by = "state") %>%
    mutate(N = replace_na(N, 0L))
}

st_tbl <- state_dist(women_state, wvar_use)

tab2122 <- tibble()
for (z in 1:6) {
  tab2122 <- bind_rows(
    tab2122,
    tibble(
      State = zone_header[as.character(z)],
      `No education¹` = NA, `Completed primary` = NA,
      `Completed secondary` = NA, `More than secondary` = NA,
      Total = NA, `Number of persons` = NA
    )
  )
  
  for (s in zone_to_states[[as.character(z)]]) {
    r <- st_tbl %>% filter(state == as.character(s))
    tab2122 <- bind_rows(
      tab2122,
      tibble(
        State = state_names[as.character(s)],
        `No education¹`       = r$`No education¹`,
        `Completed primary`   = r$`Completed primary`,
        `Completed secondary` = r$`Completed secondary`,
        `More than secondary` = r$`More than secondary`,
        Total = 100.0,
        `Number of persons` = r$N
      )
    )
  }
  
  tab2122 <- bind_rows(
    tab2122,
    tibble(
      State = "",
      `No education¹` = NA, `Completed primary` = NA,
      `Completed secondary` = NA, `More than secondary` = NA,
      Total = NA, `Number of persons` = NA
    )
  )
}

# national total row for states section
tab2122 <- bind_rows(
  tab2122,
  tibble(
    State = "Total",
    `No education¹`       = total_dist$`No education¹`[1],
    `Completed primary`   = total_dist$`Completed primary`[1],
    `Completed secondary` = total_dist$`Completed secondary`[1],
    `More than secondary` = total_dist$`More than secondary`[1],
    Total = 100.0,
    `Number of persons` = total_n
  )
)

# ---- write workbook ----
wb <- if (file.exists(OUT_XLSX)) loadWorkbook(OUT_XLSX) else createWorkbook()

write_sheet <- function(sheetname, title, dfout, footnote = NULL, cautious_note = NULL) {
  if (sheetname %in% names(wb)) removeWorksheet(wb, sheetname)
  addWorksheet(wb, sheetname, gridLines = FALSE)
  
  titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
  noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
  hdrStyle   <- createStyle(textDecoration = "bold")
  numStyle   <- createStyle(numFmt = "0.0")
  intStyle   <- createStyle(numFmt = "0")
  
  writeData(wb, sheetname, title, startRow = 2, startCol = 2)
  addStyle(wb, sheetname, titleStyle, rows = 2, cols = 2, stack = TRUE)
  
  note_txt <- if (use_weight) {
    "NOTE: Percentages weighted using calibrated trimmed weights; number of women is unweighted."
  } else {
    "NOTE: Unweighted percentages; number of women is unweighted."
  }
  writeData(wb, sheetname, note_txt, startRow = 3, startCol = 2)
  addStyle(wb, sheetname, noteStyle, rows = 3, cols = 2, stack = TRUE)
  
  if (!is.null(cautious_note)) {
    writeData(wb, sheetname, cautious_note, startRow = 4, startCol = 2)
    addStyle(wb, sheetname, noteStyle, rows = 4, cols = 2, stack = TRUE)
    data_start <- 6
  } else {
    data_start <- 5
  }
  
  writeData(wb, sheetname, dfout, startRow = data_start, startCol = 2, colNames = TRUE)
  addStyle(wb, sheetname, hdrStyle, rows = data_start, cols = 2:(ncol(dfout) + 1), gridExpand = TRUE, stack = TRUE)
  
  num_cols <- which(!names(dfout) %in% c("Background", "Label", "State", "Number of persons"))
  if (length(num_cols) > 0) {
    addStyle(
      wb, sheetname, numStyle,
      rows = (data_start + 1):(data_start + nrow(dfout)),
      cols = (1 + num_cols),
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  ncol_idx <- which(names(dfout) %in% c("Number of persons"))
  if (length(ncol_idx) > 0) {
    addStyle(
      wb, sheetname, intStyle,
      rows = (data_start + 1):(data_start + nrow(dfout)),
      cols = (1 + ncol_idx),
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  setColWidths(wb, sheetname, cols = 2, widths = 22)
  setColWidths(wb, sheetname, cols = 3:(ncol(dfout) + 1), widths = 16)
  
  if (!is.null(footnote)) {
    writeData(wb, sheetname, footnote, startRow = data_start + nrow(dfout) + 2, startCol = 2)
    addStyle(wb, sheetname, noteStyle, rows = data_start + nrow(dfout) + 2, cols = 2, stack = TRUE)
  }
}

write_sheet(
  "Table 2.12.1",
  "Table 2.12.1 Formal educational attainment of interviewed women (Sproxil respondents)",
  tab2121,
  footnote = "¹ No education includes informal education (adult education, Tsangaya, or Quranic)."
)

write_sheet(
  "Table 2.12.2",
  "Table 2.12.2 Formal educational attainment of interviewed women: States (Sproxil respondents)",
  tab2122,
  footnote = "¹ No education includes informal education (adult education, Tsangaya, or Quranic).",
  cautious_note = "State-level results are descriptive of interviewed women in the sample and should be interpreted cautiously."
)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Wrote Table 2.12.1 and 2.12.2 to: ", OUT_XLSX)