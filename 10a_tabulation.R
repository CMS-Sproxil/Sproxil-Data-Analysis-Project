# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  10_Sproxil_Tabulation_Engine_Phase1.R
# AUTHOR:  Corona Management Systems
# DATE:    23 January 2026
# PURPOSE:
#   Generate the first implemented block of Sproxil MIS-style descriptive tables
#   from the final weighted analysis-ready dataset using a single, publishable
#   engine-style script.
#
# INCLUDED IN THIS PHASE:
#   - Table 2.1
#   - Table 2.3
#   - Table 2.5
#   - Table 2.6
#   - Table 2.7
#   - Table 2.8
#   - Table 2.9
#   - Table 2.10
#   - Table 2.11 and 2.11.2
#   - Table 2.12 and 2.12.2
#
# DESIGN PRINCIPLES:
#   - One input dataset, one workbook, one weighting rule.
#   - Percentages use the locked final production weight.
#   - Count columns remain unweighted.
#   - Builders return plain data frames and metadata; sheet writers handle the
#     Excel formatting.
#   - Header logic is handled through a small family of reusable writers.
#   - Proxy/derived constructs are documented through footnotes where needed.
#   - Low-base display rule:
#       *  10-24 unweighted observations -> show estimate with *
#       *  <10 unweighted observations    -> show "-"
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(openxlsx)
})

# ------------------------------------------------------------------------------
# 1. Locked production configuration
# ------------------------------------------------------------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready_Weighted.rds"
OUT_XLSX  <- "Sproxil_Tables_Output_Final.xlsx"

USE_WEIGHTS <- TRUE
FINAL_WEIGHT_VAR <- "w_calibrated_trim"
REPLACE_EXISTING_SHEETS <- TRUE

if (!file.exists(INPUT_RDS)) stop("Missing weighted analysis-ready dataset: ", INPUT_RDS)
df <- readRDS(INPUT_RDS)

if (USE_WEIGHTS && !(FINAL_WEIGHT_VAR %in% names(df))) {
  stop("Weight variable not found in dataset: ", FINAL_WEIGHT_VAR)
}

DISPLAY_DIGITS <- 2

# ------------------------------------------------------------------------------
# 2. Common labels and footnotes
# ------------------------------------------------------------------------------
STATE_LAB <- c(
  `1`="Abia", `2`="Adamawa", `3`="Akwa Ibom", `4`="Anambra", `5`="Bauchi",
  `6`="Bayelsa", `7`="Benue", `8`="Borno", `9`="Cross River", `10`="Delta",
  `11`="Ebonyi", `12`="Edo", `13`="Ekiti", `14`="Enugu", `15`="FCT-Abuja",
  `16`="Gombe", `17`="Imo", `18`="Jigawa", `19`="Kaduna", `20`="Kano",
  `21`="Katsina", `22`="Kebbi", `23`="Kogi", `24`="Kwara", `25`="Lagos",
  `26`="Nasarawa", `27`="Niger", `28`="Ogun", `29`="Ondo", `30`="Osun",
  `31`="Oyo", `32`="Plateau", `33`="Rivers", `34`="Sokoto", `35`="Taraba",
  `36`="Yobe", `37`="Zamfara"
)

AGE_W_LAB <- c(
  `1` = "15-19", `2` = "20-24", `3` = "25-29", `4` = "30-34",
  `5` = "35-39", `6` = "40-44", `7` = "45-49"
)

EDU4_LAB <- c(
  `1` = "No education",
  `2` = "Primary",
  `3` = "Secondary",
  `4` = "More than secondary"
)

EDU_COL_LAB <- c(
  `1` = "No education",
  `2` = "Completed primary",
  `3` = "Completed secondary",
  `4` = "More than secondary"
)

WEALTH_LAB <- c(
  `1` = "Lowest",
  `2` = "Second",
  `3` = "Middle",
  `4` = "Fourth",
  `5` = "Highest"
)

ZONE_LAB <- c(
  "North Central", "North East", "North West",
  "South East", "South South", "South West"
)

FOOTNOTE_BANK <- list(
  weighted_counts = "Percentages are weighted using the final calibrated trimmed weight (w_calibrated_trim); sample counts are unweighted.",
  wealth_sample_derived = "Wealth quintiles are based on a sample-derived household living-standards index and reflect relative socioeconomic position within the Sproxil sample.",
  sample_composition_note = "This table describes the respondent sample and should not be interpreted as a population age-sex distribution.",
  itn_access_proxy = "ITN access is a household proxy based on confirmed mosquito net information and usual household members; it is not a direct DHS de facto household roster estimate.",
  state_sample_caution = "State-level estimates are weighted descriptive estimates for the survey sample and should be interpreted cautiously.",
  low_base_flag = "* Estimate based on 10-24 unweighted observations; interpret with caution.",
  low_base_suppress = "- Estimate suppressed because the unweighted denominator is less than 10."
)

# ------------------------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

need_cols <- function(cols, dat = df) {
  miss <- setdiff(cols, names(dat))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))
}

get_wvar <- function(dat = df) {
  if (!USE_WEIGHTS) return(NULL)
  if (!FINAL_WEIGHT_VAR %in% names(dat)) return(NULL)
  if (all(is.na(dat[[FINAL_WEIGHT_VAR]]))) return(NULL)
  FINAL_WEIGHT_VAR
}

weighted_mean_safe <- function(x, w = NULL) {
  if (is.null(w)) return(if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
  ok <- !is.na(x) & !is.na(w) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sw <- sum(w[ok])
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  sum(w[ok] * x[ok]) / sw
}

pct_from_binary <- function(x, w = NULL) round(100 * weighted_mean_safe(x, w), DISPLAY_DIGITS)
mean_from_numeric <- function(x, w = NULL) round(weighted_mean_safe(x, w), DISPLAY_DIGITS)

get_base_status <- function(n) {
  case_when(
    is.na(n) ~ "ok",
    n < 10 ~ "suppress",
    n < 25 ~ "flag",
    TRUE ~ "ok"
  )
}

format_estimate_display <- function(value, n, digits = 1) {
  status <- get_base_status(n)
  
  if (status == "suppress") return("-")
  if (is.na(value)) return("")
  
  v <- formatC(round(value, digits), format = "f", digits = digits)
  
  if (status == "flag") {
    return(paste0(v, "*"))
  }
  
  v
}

format_count_display <- function(n) {
  ifelse(is.na(n), "", as.character(n))
}

collapse_base_notes <- function(status_vec) {
  status_vec <- unique(na.omit(status_vec))
  notes <- character(0)
  
  if ("flag" %in% status_vec) {
    notes <- c(notes, FOOTNOTE_BANK$low_base_flag)
  }
  if ("suppress" %in% status_vec) {
    notes <- c(notes, FOOTNOTE_BANK$low_base_suppress)
  }
  
  if (length(notes) == 0) return(NULL)
  paste(notes, collapse = " ")
}

collapse_footnotes <- function(keys) {
  keys <- strsplit(keys %||% "", ";", fixed = TRUE)[[1]]
  keys <- trimws(keys)
  keys <- keys[nzchar(keys)]
  if (length(keys) == 0) return(NULL)
  vals <- unname(FOOTNOTE_BANK[keys])
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NULL)
  paste(vals, collapse = " ")
}

pct_yes_by_group <- function(dat, group_var, x_var, wvar = NULL, levels = NULL) {
  d <- dat %>%
    filter(!is.na(.data[[group_var]]), !is.na(.data[[x_var]]), .data[[x_var]] %in% c(0, 1))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  if (nrow(d) == 0) return(tibble())
  
  out <- if (is.null(wvar)) {
    d %>%
      group_by(.data[[group_var]]) %>%
      summarise(value = 100 * mean(.data[[x_var]] == 1), .groups = "drop")
  } else {
    d %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        value = 100 * sum(.data[[wvar]] * (.data[[x_var]] == 1), na.rm = TRUE) /
          sum(.data[[wvar]], na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  out <- out %>% transmute(group = as.character(.data[[group_var]]), value = round(value, 1))
  
  if (!is.null(levels)) {
    out <- tibble(group = levels) %>% left_join(out, by = "group")
  }
  out
}

dist_wide <- function(dat, group_var, cat_var, expected_levels = NULL, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[group_var]]), !is.na(.data[[cat_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  if (nrow(d) == 0) return(tibble())
  
  out <- if (is.null(wvar)) {
    d %>%
      count(.data[[group_var]], .data[[cat_var]], name = "n") %>%
      group_by(.data[[group_var]]) %>%
      mutate(p = 100 * n / sum(n)) %>%
      ungroup()
  } else {
    d %>%
      group_by(.data[[group_var]], .data[[cat_var]]) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[group_var]]) %>%
      mutate(p = 100 * w / sum(w)) %>%
      ungroup()
  }
  
  out <- out %>%
    transmute(
      group = as.character(.data[[group_var]]),
      cat = as.character(.data[[cat_var]]),
      p = round(p, 1)
    )
  
  if (!is.null(expected_levels)) {
    out <- out %>%
      mutate(cat = factor(cat, levels = expected_levels)) %>%
      complete(group, cat, fill = list(p = 0.0)) %>%
      mutate(cat = as.character(cat))
  }
  
  out %>%
    pivot_wider(names_from = group, values_from = p, values_fill = 0.0)
}

n_by_group <- function(dat, group_var, levels = NULL) {
  out <- dat %>%
    count(.data[[group_var]], name = "N") %>%
    transmute(group = as.character(.data[[group_var]]), N)
  
  if (!is.null(levels)) out <- tibble(group = levels) %>% left_join(out, by = "group")
  out$N[is.na(out$N)] <- 0L
  out
}

make_urt_frame <- function(dat, res_var = "derived_residence") {
  out <- dat %>%
    mutate(res_grp = case_when(
      to_num(.data[[res_var]]) == 1 ~ "Urban",
      to_num(.data[[res_var]]) == 2 ~ "Rural",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(res_grp))
  bind_rows(out, out %>% mutate(res_grp = "Total"))
}

make_state_frame <- function(dat, state_var = "demo_state_num") {
  dat %>%
    mutate(state_lbl = STATE_LAB[as.character(to_num(.data[[state_var]]))]) %>%
    filter(!is.na(state_lbl))
}

standardize_zone <- function(x) {
  z <- as.character(x)
  z <- trimws(z)
  
  case_when(
    z %in% c("1", "North Central", "NORTH CENTRAL", "north central") ~ "North Central",
    z %in% c("2", "North East", "NORTH EAST", "north east") ~ "North East",
    z %in% c("3", "North West", "NORTH WEST", "north west") ~ "North West",
    z %in% c("4", "South East", "SOUTH EAST", "south east") ~ "South East",
    z %in% c("5", "South South", "SOUTH SOUTH", "south south") ~ "South South",
    z %in% c("6", "South West", "SOUTH WEST", "south west") ~ "South West",
    TRUE ~ NA_character_
  )
}

get_respondent_frame <- function(dat) {
  if ("u_all" %in% names(dat)) {
    dat %>% filter(u_all == 1)
  } else {
    dat
  }
}

weighted_dist_1way <- function(dat, cat_var, levels, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[cat_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  if (nrow(d) == 0) return(setNames(rep(NA_real_, length(levels)), levels))
  
  if (is.null(wvar)) {
    out <- d %>%
      count(.data[[cat_var]], name = "n") %>%
      mutate(p = 100 * n / sum(n))
  } else {
    out <- d %>%
      group_by(.data[[cat_var]]) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      mutate(p = 100 * w / sum(w))
  }
  
  vals <- setNames(rep(0, length(levels)), levels)
  tmp <- out$p
  names(tmp) <- as.character(out[[cat_var]])
  vals[names(tmp)] <- round(tmp, 1)
  vals
}

weighted_row_dist <- function(dat, row_var, col_var, row_levels, col_levels, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[row_var]]), !is.na(.data[[col_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  
  if (nrow(d) == 0) {
    out <- expand.grid(row = row_levels, col = col_levels, stringsAsFactors = FALSE)
    out$p <- NA_real_
    return(out)
  }
  
  if (is.null(wvar)) {
    out <- d %>%
      count(.data[[row_var]], .data[[col_var]], name = "n") %>%
      group_by(.data[[row_var]]) %>%
      mutate(p = 100 * n / sum(n)) %>%
      ungroup()
  } else {
    out <- d %>%
      group_by(.data[[row_var]], .data[[col_var]]) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[row_var]]) %>%
      mutate(p = 100 * w / sum(w)) %>%
      ungroup()
  }
  
  out <- out %>%
    transmute(
      row = as.character(.data[[row_var]]),
      col = as.character(.data[[col_var]]),
      p = round(p, 1)
    )
  
  expand.grid(row = row_levels, col = col_levels, stringsAsFactors = FALSE) %>%
    left_join(out, by = c("row", "col"))
}

style_bank <- function() {
  list(
    title = createStyle(textDecoration = "bold", fontSize = 12),
    header = createStyle(
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      wrapText = TRUE,
      border = "TopBottomLeftRight"
    ),
    subhead = createStyle(textDecoration = "bold"),
    stub = createStyle(wrapText = TRUE),
    note = createStyle(textDecoration = "italic", fontSize = 9),
    pct = createStyle(numFmt = "0.0"),
    int = createStyle(numFmt = "0")
  )
}

wb_init <- function(path) {
  if (file.exists(path)) loadWorkbook(path) else createWorkbook()
}

wb_replace_sheet <- function(wb, sheet) {
  if (sheet %in% names(wb) && REPLACE_EXISTING_SHEETS) removeWorksheet(wb, sheet)
  addWorksheet(wb, sheet, gridLines = FALSE)
}

blank_na_for_excel <- function(x) {
  x2 <- x
  for (j in seq_along(x2)) {
    x2[[j]][is.na(x2[[j]])] <- ""
  }
  x2
}

write_table_A <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  stub_col <- 2
  urb_col <- 4
  rur_col <- 5
  tot_col <- 6
  
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, "Characteristic", startRow = 6, startCol = 2, colNames = FALSE)
  mergeCells(wb, sheet, cols = 4:6, rows = 6)
  writeData(wb, sheet, "Population", startRow = 6, startCol = 4, colNames = FALSE)
  writeData(wb, sheet, c("Urban", "Rural", "Total"), startRow = 7, startCol = 4, colNames = FALSE)
  addStyle(wb, sheet, s$header, rows = 6:7, cols = c(2, 4:6), gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = stub_col, colNames = FALSE)
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = stub_col, gridExpand = TRUE, stack = TRUE)
  
  num_rows <- which(grepl("^Number", body[[1]]))
  if (length(num_rows) > 0) {
    addStyle(wb, sheet, s$int, rows = start_row + num_rows - 1, cols = c(urb_col:rur_col, tot_col), gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 60)
  setColWidths(wb, sheet, cols = 4:6, widths = 12)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_B <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 6
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = TRUE)
  addStyle(wb, sheet, s$header, rows = start_row, cols = 2:(ncol(body) + 1), gridExpand = TRUE, stack = TRUE)
  
  int_cols <- which(grepl("^Number|Unweighted number", names(body))) + 1
  if (length(int_cols) > 0) {
    addStyle(wb, sheet, s$int, rows = (start_row + 1):(start_row + nrow(body)), cols = int_cols, gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 40)
  if (ncol(body) > 1) setColWidths(wb, sheet, cols = 3:(ncol(body) + 1), widths = 14)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_D <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, "Age", startRow = 6, startCol = 2, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 3:5, rows = 6)
  mergeCells(wb, sheet, cols = 6:8, rows = 6)
  mergeCells(wb, sheet, cols = 9:11, rows = 6)
  
  writeData(wb, sheet, "Urban", startRow = 6, startCol = 3, colNames = FALSE)
  writeData(wb, sheet, "Rural", startRow = 6, startCol = 6, colNames = FALSE)
  writeData(wb, sheet, "Total", startRow = 6, startCol = 9, colNames = FALSE)
  
  writeData(
    wb, sheet,
    c("Male", "Female", "Total", "Male", "Female", "Total", "Male", "Female", "Total"),
    startRow = 7, startCol = 3, colNames = FALSE
  )
  
  addStyle(wb, sheet, s$header, rows = 6:7, cols = 2:11, gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = FALSE)
  
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  
  num_rows <- which(body[[1]] == "Number of persons")
  if (length(num_rows) > 0) {
    addStyle(wb, sheet, s$int, rows = start_row + num_rows - 1, cols = 3:11, gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 18)
  setColWidths(wb, sheet, cols = 3:11, widths = 11)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_E <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, "Characteristic", startRow = 6, startCol = 2, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 3:4, rows = 6)
  mergeCells(wb, sheet, cols = 5, rows = 6:7)
  
  writeData(wb, sheet, "Residence", startRow = 6, startCol = 3, colNames = FALSE)
  writeData(wb, sheet, "Total", startRow = 6, startCol = 5, colNames = FALSE)
  writeData(wb, sheet, c("Urban", "Rural"), startRow = 7, startCol = 3, colNames = FALSE)
  
  addStyle(wb, sheet, s$header, rows = 6:7, cols = 2:5, gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = FALSE)
  
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  
  int_rows <- which(body[[1]] == "Number of households")
  if (length(int_rows) > 0) {
    addStyle(wb, sheet, s$int, rows = start_row + int_rows - 1, cols = 3:5, gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 32)
  setColWidths(wb, sheet, cols = 3:5, widths = 12)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_F <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, "Background characteristic", startRow = 6, startCol = 2, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 3:6, rows = 6)
  mergeCells(wb, sheet, cols = 7, rows = 6:7)
  mergeCells(wb, sheet, cols = 8, rows = 6:7)
  
  writeData(wb, sheet, "Highest level of schooling", startRow = 6, startCol = 3, colNames = FALSE)
  writeData(wb, sheet, "Total", startRow = 6, startCol = 7, colNames = FALSE)
  writeData(wb, sheet, "Number of women", startRow = 6, startCol = 8, colNames = FALSE)
  
  writeData(
    wb, sheet,
    c("No education", "Completed primary", "Completed secondary", "More than secondary"),
    startRow = 7, startCol = 3, colNames = FALSE
  )
  
  addStyle(wb, sheet, s$header, rows = 6:7, cols = 2:8, gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = FALSE)
  
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, s$int, rows = start_row:(start_row + nrow(body) - 1), cols = 8, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 24)
  setColWidths(wb, sheet, cols = 3:8, widths = 14)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_G <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, "Residence/region", startRow = 6, startCol = 2, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 3:7, rows = 6)
  writeData(wb, sheet, "Wealth quintile", startRow = 6, startCol = 3, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 8, rows = 6:7)
  writeData(wb, sheet, "Total", startRow = 6, startCol = 8, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 9, rows = 6:7)
  writeData(wb, sheet, "Number of persons", startRow = 6, startCol = 9, colNames = FALSE)
  
  writeData(
    wb, sheet,
    c("Lowest", "Second", "Middle", "Fourth", "Highest"),
    startRow = 7, startCol = 3, colNames = FALSE
  )
  
  addStyle(wb, sheet, s$header, rows = 6:7, cols = 2:9, gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = FALSE)
  
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, s$int, rows = start_row:(start_row + nrow(body) - 1), cols = 9, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 22)
  setColWidths(wb, sheet, cols = 3:9, widths = 12)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

# ------------------------------------------------------------------------------
# 4. Builder functions: phase 1 implementation
# ------------------------------------------------------------------------------
build_table_2_1 <- function(dat) {
  need_cols(c("u_household","derived_residence","hh_drinking_water_source","hh_other_water_source","hh_water_location","hh_water_time_trip"), dat)
  wvar <- get_wvar(dat)
  
  d0 <- dat %>% filter(u_household == 1) %>% mutate(
    wsrc = to_num(hh_drinking_water_source),
    owsrc = to_num(hh_other_water_source),
    wloc = to_num(hh_water_location),
    wtime = to_num(hh_water_time_trip)
  )
  
  d <- make_urt_frame(d0) %>% mutate(
    other_source_improved = case_when(
      owsrc %in% c(11, 12, 13, 21, 31, 41, 51, 61, 71, 92) ~ 1,
      owsrc %in% c(32, 42, 81, 96) ~ 0,
      TRUE ~ NA_real_
    ),
    water_cat = case_when(
      wsrc == 11 ~ "Piped into dwelling/yard/plot",
      wsrc == 12 ~ "Piped to neighbor",
      wsrc == 13 ~ "Public tap/standpipe",
      wsrc == 21 ~ "Tubewell/borehole",
      wsrc == 31 ~ "Protected dug well",
      wsrc == 41 ~ "Protected spring",
      wsrc == 51 ~ "Rainwater",
      wsrc %in% c(61,71) ~ "Tanker truck/cart with small tank",
      wsrc == 91 ~ "Bottled water",
      wsrc == 92 ~ "Sachet water",
      wsrc == 32 ~ "Unprotected dug well",
      wsrc == 42 ~ "Unprotected spring",
      wsrc == 96 ~ "Other",
      wsrc == 81 ~ "Surface water",
      TRUE ~ NA_character_
    ),
    water_group = case_when(
      wsrc %in% c(11,12,13,21,31,41,51,61,71,92) ~ "Improved source",
      wsrc == 91 & other_source_improved == 1 ~ "Improved source",
      wsrc == 91 & other_source_improved == 0 ~ "Unimproved source",
      wsrc %in% c(32,42,96) ~ "Unimproved source",
      wsrc == 81 ~ "Surface water",
      TRUE ~ NA_character_
    ),
    time_cat = case_when(
      wsrc == 12 ~ "Water on premises¹",
      wloc %in% c(1,2) ~ "Water on premises¹",
      wtime == 0 ~ "Water on premises¹",
      wtime %in% c(98,99) ~ "Don't know",
      is.na(wtime) ~ NA_character_,
      wtime <= 30 ~ "30 minutes or less",
      wtime > 30 ~ "More than 30 minutes",
      TRUE ~ NA_character_
    )
  )
  
  group_w <- dist_wide(d, "res_grp", "water_group", wvar = wvar)
  detail_w <- dist_wide(d, "res_grp", "water_cat", wvar = wvar)
  time_w <- dist_wide(d, "res_grp", "time_cat", wvar = wvar)
  
  n_all <- n_by_group(d %>% filter(res_grp %in% c("Urban","Rural")), "res_grp", c("Urban","Rural"))
  n_tot <- tibble(group = "Total", N = sum(n_all$N))
  n_counts <- bind_rows(n_all, n_tot)
  
  body <- tribble(
    ~Characteristic, ~Urban, ~Rural, ~Total,
    "Source of drinking water", NA, NA, NA,
    "Improved source", group_w$Urban[group_w$cat == "Improved source"], group_w$Rural[group_w$cat == "Improved source"], group_w$Total[group_w$cat == "Improved source"],
    "  Piped into dwelling/yard/plot", detail_w$Urban[detail_w$cat == "Piped into dwelling/yard/plot"], detail_w$Rural[detail_w$cat == "Piped into dwelling/yard/plot"], detail_w$Total[detail_w$cat == "Piped into dwelling/yard/plot"],
    "  Piped to neighbor", detail_w$Urban[detail_w$cat == "Piped to neighbor"], detail_w$Rural[detail_w$cat == "Piped to neighbor"], detail_w$Total[detail_w$cat == "Piped to neighbor"],
    "  Public tap/standpipe", detail_w$Urban[detail_w$cat == "Public tap/standpipe"], detail_w$Rural[detail_w$cat == "Public tap/standpipe"], detail_w$Total[detail_w$cat == "Public tap/standpipe"],
    "  Tubewell/borehole", detail_w$Urban[detail_w$cat == "Tubewell/borehole"], detail_w$Rural[detail_w$cat == "Tubewell/borehole"], detail_w$Total[detail_w$cat == "Tubewell/borehole"],
    "  Protected dug well", detail_w$Urban[detail_w$cat == "Protected dug well"], detail_w$Rural[detail_w$cat == "Protected dug well"], detail_w$Total[detail_w$cat == "Protected dug well"],
    "  Protected spring", detail_w$Urban[detail_w$cat == "Protected spring"], detail_w$Rural[detail_w$cat == "Protected spring"], detail_w$Total[detail_w$cat == "Protected spring"],
    "  Rainwater", detail_w$Urban[detail_w$cat == "Rainwater"], detail_w$Rural[detail_w$cat == "Rainwater"], detail_w$Total[detail_w$cat == "Rainwater"],
    "  Tanker truck/cart with small tank", detail_w$Urban[detail_w$cat == "Tanker truck/cart with small tank"], detail_w$Rural[detail_w$cat == "Tanker truck/cart with small tank"], detail_w$Total[detail_w$cat == "Tanker truck/cart with small tank"],
    "  Bottled water", detail_w$Urban[detail_w$cat == "Bottled water"], detail_w$Rural[detail_w$cat == "Bottled water"], detail_w$Total[detail_w$cat == "Bottled water"],
    "  Sachet water", detail_w$Urban[detail_w$cat == "Sachet water"], detail_w$Rural[detail_w$cat == "Sachet water"], detail_w$Total[detail_w$cat == "Sachet water"],
    "Unimproved source", group_w$Urban[group_w$cat == "Unimproved source"], group_w$Rural[group_w$cat == "Unimproved source"], group_w$Total[group_w$cat == "Unimproved source"],
    "  Unprotected dug well", detail_w$Urban[detail_w$cat == "Unprotected dug well"], detail_w$Rural[detail_w$cat == "Unprotected dug well"], detail_w$Total[detail_w$cat == "Unprotected dug well"],
    "  Unprotected spring", detail_w$Urban[detail_w$cat == "Unprotected spring"], detail_w$Rural[detail_w$cat == "Unprotected spring"], detail_w$Total[detail_w$cat == "Unprotected spring"],
    "  Other", detail_w$Urban[detail_w$cat == "Other"], detail_w$Rural[detail_w$cat == "Other"], detail_w$Total[detail_w$cat == "Other"],
    "Surface water", group_w$Urban[group_w$cat == "Surface water"], group_w$Rural[group_w$cat == "Surface water"], group_w$Total[group_w$cat == "Surface water"],
    "Total", 100, 100, 100,
    "Time to obtain drinking water (round trip)", NA, NA, NA,
    "  Water on premises¹", time_w$Urban[time_w$cat == "Water on premises¹"], time_w$Rural[time_w$cat == "Water on premises¹"], time_w$Total[time_w$cat == "Water on premises¹"],
    "  30 minutes or less", time_w$Urban[time_w$cat == "30 minutes or less"], time_w$Rural[time_w$cat == "30 minutes or less"], time_w$Total[time_w$cat == "30 minutes or less"],
    "  More than 30 minutes", time_w$Urban[time_w$cat == "More than 30 minutes"], time_w$Rural[time_w$cat == "More than 30 minutes"], time_w$Total[time_w$cat == "More than 30 minutes"],
    "  Don't know", time_w$Urban[time_w$cat == "Don't know"], time_w$Rural[time_w$cat == "Don't know"], time_w$Total[time_w$cat == "Don't know"],
    "Total", 100, 100, 100,
    "Number of respondents", n_counts$N[n_counts$group == "Urban"], n_counts$N[n_counts$group == "Rural"], n_counts$N[n_counts$group == "Total"]
  )
  
  list(
    type = "A",
    body = body,
    footnote = paste(
      collapse_footnotes("weighted_counts"),
      "¹Includes water piped to a neighbour and those reporting a round-trip collection time of zero minutes"
    )
  )
}

build_table_2_3 <- function(dat) {
  need_cols(c("u_household","derived_residence","hh_toilet_type","hh_toilet_location","hh_toilet_shared"), dat)
  wvar <- get_wvar(dat)
  
  d0 <- dat %>% filter(u_household == 1) %>% mutate(
    toilet = to_num(hh_toilet_type),
    tloc = to_num(hh_toilet_location)
  )
  
  d <- make_urt_frame(d0) %>% mutate(
    toilet_group = case_when(
      toilet %in% c(11,12,13,21,22,31) ~ "Improved sanitation facility",
      toilet %in% c(14,15,23,41,51,96) ~ "Unimproved sanitation facility",
      toilet == 61 ~ "Open defecation (no facility/bush/field)",
      TRUE ~ NA_character_
    ),
    toilet_detail = case_when(
      toilet == 11 ~ "Flush/pour flush to piped sewer system",
      toilet == 12 ~ "Flush/pour flush to septic tank",
      toilet == 13 ~ "Flush/pour flush to a pit latrine",
      toilet == 21 ~ "Ventilated improved pit (VIP) latrine",
      toilet == 22 ~ "Pit latrine with a slab",
      toilet == 31 ~ "Composting toilet",
      toilet %in% c(14,15) ~ "Flush/pour flush not to sewer/ septic tank/pit latrine",
      toilet == 23 ~ "Pit latrine without slab/open pit",
      toilet == 41 ~ "Bucket",
      toilet == 51 ~ "Hanging toilet/hanging latrine",
      toilet == 96 ~ "Other",
      toilet == 61 ~ "Open defecation (no facility/bush/field)",
      TRUE ~ NA_character_
    ),
    loc_cat = case_when(
      tloc == 1 ~ "In own dwelling",
      tloc == 2 ~ "In own yard/plot",
      tloc == 3 ~ "Elsewhere",
      TRUE ~ NA_character_
    ),
    has_facility = case_when(
      toilet == 61 ~ 0,
      toilet %in% c(11,12,13,14,15,21,22,23,31,41,51,96) ~ 1,
      TRUE ~ NA_real_
    )
  )
  
  loc_df <- d %>% filter(has_facility == 1, !is.na(loc_cat))
  toilet_group_levels <- c(
    "Improved sanitation facility",
    "Unimproved sanitation facility",
    "Open defecation (no facility/bush/field)"
  )
  
  toilet_detail_levels <- c(
    "Flush/pour flush to piped sewer system",
    "Flush/pour flush to septic tank",
    "Flush/pour flush to a pit latrine",
    "Ventilated improved pit (VIP) latrine",
    "Pit latrine with a slab",
    "Composting toilet",
    "Flush/pour flush not to sewer/ septic tank/pit latrine",
    "Pit latrine without slab/open pit",
    "Bucket",
    "Hanging toilet/hanging latrine",
    "Other",
    "Open defecation (no facility/bush/field)"
  )
  
  loc_levels <- c(
    "In own dwelling",
    "In own yard/plot",
    "Elsewhere"
  )
  
  g   <- dist_wide(d, "res_grp", "toilet_group", toilet_group_levels, wvar)
  det <- dist_wide(d, "res_grp", "toilet_detail", toilet_detail_levels, wvar)
  loc <- dist_wide(loc_df, "res_grp", "loc_cat", loc_levels, wvar)
  
  n_all <- n_by_group(d %>% filter(res_grp %in% c("Urban","Rural")), "res_grp", c("Urban","Rural"))
  n_all <- bind_rows(n_all, tibble(group = "Total", N = sum(n_all$N)))
  
  n_fac <- n_by_group(loc_df %>% filter(res_grp %in% c("Urban","Rural")), "res_grp", c("Urban","Rural"))
  n_fac <- bind_rows(n_fac, tibble(group = "Total", N = sum(n_fac$N)))
  
  body <- tribble(
    ~Characteristic, ~Urban, ~Rural, ~Total,
    "Type and location of toilet/latrine facility", NA, NA, NA,
    "Improved sanitation facility", g$Urban[g$cat=="Improved sanitation facility"], g$Rural[g$cat=="Improved sanitation facility"], g$Total[g$cat=="Improved sanitation facility"],
    "   Flush/pour flush to piped sewer system", det$Urban[det$cat=="Flush/pour flush to piped sewer system"], det$Rural[det$cat=="Flush/pour flush to piped sewer system"], det$Total[det$cat=="Flush/pour flush to piped sewer system"],
    "   Flush/pour flush to septic tank", det$Urban[det$cat=="Flush/pour flush to septic tank"], det$Rural[det$cat=="Flush/pour flush to septic tank"], det$Total[det$cat=="Flush/pour flush to septic tank"],
    "   Flush/pour flush to a pit latrine", det$Urban[det$cat=="Flush/pour flush to a pit latrine"], det$Rural[det$cat=="Flush/pour flush to a pit latrine"], det$Total[det$cat=="Flush/pour flush to a pit latrine"],
    "   Ventilated improved pit (VIP) latrine", det$Urban[det$cat=="Ventilated improved pit (VIP) latrine"], det$Rural[det$cat=="Ventilated improved pit (VIP) latrine"], det$Total[det$cat=="Ventilated improved pit (VIP) latrine"],
    "   Pit latrine with a slab", det$Urban[det$cat=="Pit latrine with a slab"], det$Rural[det$cat=="Pit latrine with a slab"], det$Total[det$cat=="Pit latrine with a slab"],
    "   Composting toilet", det$Urban[det$cat=="Composting toilet"], det$Rural[det$cat=="Composting toilet"], det$Total[det$cat=="Composting toilet"],
    "Unimproved sanitation facility", g$Urban[g$cat=="Unimproved sanitation facility"], g$Rural[g$cat=="Unimproved sanitation facility"], g$Total[g$cat=="Unimproved sanitation facility"],
    "   Flush/pour flush not to sewer/ septic tank/pit latrine", det$Urban[det$cat=="Flush/pour flush not to sewer/ septic tank/pit latrine"], det$Rural[det$cat=="Flush/pour flush not to sewer/ septic tank/pit latrine"], det$Total[det$cat=="Flush/pour flush not to sewer/ septic tank/pit latrine"],
    "   Pit latrine without slab/open pit", det$Urban[det$cat=="Pit latrine without slab/open pit"], det$Rural[det$cat=="Pit latrine without slab/open pit"], det$Total[det$cat=="Pit latrine without slab/open pit"],
    "   Bucket", det$Urban[det$cat=="Bucket"], det$Rural[det$cat=="Bucket"], det$Total[det$cat=="Bucket"],
    "   Hanging toilet/hanging latrine", det$Urban[det$cat=="Hanging toilet/hanging latrine"], det$Rural[det$cat=="Hanging toilet/hanging latrine"], det$Total[det$cat=="Hanging toilet/hanging latrine"],
    "   Other", det$Urban[det$cat=="Other"], det$Rural[det$cat=="Other"], det$Total[det$cat=="Other"],
    "Open defecation (no facility/bush/field)", g$Urban[g$cat=="Open defecation (no facility/bush/field)"], g$Rural[g$cat=="Open defecation (no facility/bush/field)"], g$Total[g$cat=="Open defecation (no facility/bush/field)"],
    "Total", 100, 100, 100,
    "Number of respondents", n_all$N[n_all$group=="Urban"], n_all$N[n_all$group=="Rural"], n_all$N[n_all$group=="Total"],
    "Location of toilet facility", NA, NA, NA,
    "In own dwelling", loc$Urban[loc$cat=="In own dwelling"], loc$Rural[loc$cat=="In own dwelling"], loc$Total[loc$cat=="In own dwelling"],
    "In own yard/plot", loc$Urban[loc$cat=="In own yard/plot"], loc$Rural[loc$cat=="In own yard/plot"], loc$Total[loc$cat=="In own yard/plot"],
    "Elsewhere", loc$Urban[loc$cat=="Elsewhere"], loc$Rural[loc$cat=="Elsewhere"], loc$Total[loc$cat=="Elsewhere"],
    "Total", 100, 100, 100,
    "Number of respondents with a toilet/latrine facility", n_fac$N[n_fac$group=="Urban"], n_fac$N[n_fac$group=="Rural"], n_fac$N[n_fac$group=="Total"]
  )
  
  list(type = "A", body = body, footnote = paste(collapse_footnotes("weighted_counts"), collapse_footnotes("sanitation_note")))
}

build_table_2_5 <- function(dat) {
  need_cols(c("u_household","derived_residence","hh_floor_material","hh_roof_material","hh_wall_material","demo_hh_sleeping_rooms"), dat)
  wvar <- get_wvar(dat)
  
  d <- make_urt_frame(dat %>% filter(u_household == 1)) %>% mutate(
    floor = to_num(hh_floor_material),
    roof = to_num(hh_roof_material),
    wall = to_num(hh_wall_material),
    rooms_num = to_num(demo_hh_sleeping_rooms),
    floor_cat = case_when(
      floor == 11 ~ "Earth/sand",
      floor == 12 ~ "Dung",
      floor == 21 ~ "Wood planks",
      floor == 22 ~ "Palm/bamboo",
      floor == 31 ~ "Parquet or polished wood",
      floor == 32 ~ "Vinyl or asphalt strips",
      floor == 33 ~ "Ceramic tiles",
      floor == 34 ~ "Cement",
      floor == 35 ~ "Carpet",
      floor == 96 ~ "Other",
      TRUE ~ NA_character_
    ),
    roof_cat = case_when(
      roof == 11 ~ "No roof",
      roof == 12 ~ "Thatch / Palm leaf",
      roof == 13 ~ "Grass",
      roof == 21 ~ "Rustic mat",
      roof == 22 ~ "Palm/bamboo",
      roof == 23 ~ "Wood planks",
      roof == 24 ~ "Cardboard",
      roof == 31 ~ "Metal/zinc",
      roof == 32 ~ "Wood",
      roof == 33 ~ "Calamine/cement fibre",
      roof == 34 ~ "Ceramic tiles",
      roof == 35 ~ "Cement",
      roof == 36 ~ "Roofing shingles",
      roof == 37 ~ "Asbestos",
      roof == 96 ~ "Other",
      TRUE ~ NA_character_
    ),
    wall_cat = case_when(
      wall == 11 ~ "No walls",
      wall == 12 ~ "Cane/palm/trunks",
      wall == 13 ~ "Dirt",
      wall == 21 ~ "Bamboo with mud",
      wall == 22 ~ "Stone with mud",
      wall == 23 ~ "Uncovered adobe",
      wall == 24 ~ "Plywood",
      wall == 25 ~ "Cardboard",
      wall == 26 ~ "Reused wood",
      wall == 31 ~ "Cement",
      wall == 32 ~ "Stone with lime/cement",
      wall == 33 ~ "Bricks",
      wall == 34 ~ "Cement blocks",
      wall == 35 ~ "Covered adobe",
      wall == 36 ~ "Wood planks/shingles",
      wall == 96 ~ "Other",
      TRUE ~ NA_character_
    ),
    rooms_cat = case_when(
      rooms_num == 1 ~ "One",
      rooms_num == 2 ~ "Two",
      rooms_num >= 3 ~ "Three or more",
      TRUE ~ NA_character_
    )
  )
  
  floor_levels <- c("Earth/sand","Dung","Wood planks","Palm/bamboo","Parquet or polished wood","Vinyl or asphalt strips","Ceramic tiles","Cement","Carpet","Other")
  roof_levels <- c("No roof","Thatch / Palm leaf","Grass","Rustic mat","Palm/bamboo","Wood planks","Cardboard","Metal/zinc","Wood","Calamine/cement fibre","Ceramic tiles","Cement","Roofing shingles","Asbestos","Other")
  wall_levels <- c("No walls","Cane/palm/trunks","Dirt","Bamboo with mud","Stone with mud","Uncovered adobe","Plywood","Cardboard","Reused wood","Cement","Stone with lime/cement","Bricks","Cement blocks","Covered adobe","Wood planks/shingles","Other")
  rooms_levels <- c("One","Two","Three or more")
  
  fw <- dist_wide(d, "res_grp", "floor_cat", floor_levels, wvar)
  rw <- dist_wide(d, "res_grp", "roof_cat", roof_levels, wvar)
  ww <- dist_wide(d, "res_grp", "wall_cat", wall_levels, wvar)
  rmw <- dist_wide(d, "res_grp", "rooms_cat", rooms_levels, wvar)
  
  nn <- n_by_group(d %>% filter(res_grp %in% c("Urban","Rural")), "res_grp", c("Urban","Rural"))
  nn <- bind_rows(nn, tibble(group = "Total", N = sum(nn$N)))
  
  block_rows <- function(labels, wide, indent = "") {
    tibble(
      Characteristic = paste0(indent, labels),
      Urban = map_dbl(labels, ~ wide$Urban[wide$cat == .x][1] %||% NA_real_),
      Rural = map_dbl(labels, ~ wide$Rural[wide$cat == .x][1] %||% NA_real_),
      Total = map_dbl(labels, ~ wide$Total[wide$cat == .x][1] %||% NA_real_)
    )
  }
  
  body <- bind_rows(
    tibble(Characteristic = "Flooring material", Urban = NA, Rural = NA, Total = NA),
    block_rows(floor_levels, fw),
    tibble(Characteristic = "Roof material", Urban = NA, Rural = NA, Total = NA),
    block_rows(roof_levels, rw),
    tibble(Characteristic = "Exterior wall material", Urban = NA, Rural = NA, Total = NA),
    block_rows(wall_levels, ww),
    tibble(Characteristic = "Rooms used for sleeping", Urban = NA, Rural = NA, Total = NA),
    block_rows(rooms_levels, rmw),
    tibble(Characteristic = "Number of respondents", Urban = nn$N[nn$group=="Urban"], Rural = nn$N[nn$group=="Rural"], Total = nn$N[nn$group=="Total"])
  )
  
  list(type = "A", body = body, footnote = collapse_footnotes("weighted_counts"))
}

build_table_2_6 <- function(dat) {
  need_cols(c("u_household","derived_residence","hh_has_electricity","hh_cookstove_type","hh_cookstove_fuel"), dat)
  wvar <- get_wvar(dat)
  
  d <- make_urt_frame(dat %>% filter(u_household == 1)) %>% mutate(
    elec_cat = case_when(
      to_num(hh_has_electricity) == 1 ~ "Yes",
      to_num(hh_has_electricity) == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    stove = to_num(hh_cookstove_type),
    fuel = to_num(hh_cookstove_fuel),
    tech_cat = case_when(
      stove == 1 ~ "Electric stove",
      stove == 2 ~ "Solar cooker",
      stove == 3 ~ "LPG/cooking gas stove",
      stove == 4 ~ "Piped natural gas stove",
      stove == 5 ~ "Biogas stove",
      stove == 6 ~ "Kerosene stove/liquid fuel stove not using alcohol/ethanol",
      stove == 7 ~ "Manufactured solid fuel stove",
      stove == 8 ~ "Traditional solid fuel stove",
      stove == 9 ~ "Three stone stove/open fire",
      stove == 96 ~ "Other fuel",
      stove == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    ),
    tech_group = case_when(
      stove %in% c(1,2,3,4,5) ~ "Clean fuels and technologies",
      stove %in% c(6,7,8,9,96) ~ "Other fuels and technologies",
      stove == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    ),
    fuel_cat = case_when(
      fuel == 4 ~ "Coal/lignite",
      fuel == 5 ~ "Charcoal",
      fuel == 6 ~ "Wood",
      fuel == 7 ~ "Straw/shrubs/grass",
      fuel == 8 ~ "Agricultural crop",
      fuel == 9 ~ "Animal dung/waste",
      fuel == 10 ~ "Processed biomass (pellets) or woodchips",
      fuel == 11 ~ "Garbage/plastic",
      fuel == 12 ~ "Sawdust",
      fuel == 2 ~ "Gasoline/diesel",
      fuel == 3 ~ "Kerosene",
      fuel == 96 ~ "Other fuel",
      stove == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    ),
    fuel_group = case_when(
      stove %in% c(1,2,3,4,5) | fuel == 1 ~ "Clean fuels and technologies",
      fuel %in% c(4,5,6,7,8,9,10,11,12) ~ "Solid fuels for cooking",
      fuel %in% c(2,3,96) ~ "Other fuel",
      stove == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    )
  )
  
  elec_w <- dist_wide(d, "res_grp", "elec_cat", c("Yes","No"), wvar)
  tech_group_w <- dist_wide(d, "res_grp", "tech_group", c("Clean fuels and technologies","Other fuels and technologies","No food cooked in household"), wvar)
  tech_detail_w <- dist_wide(d, "res_grp", "tech_cat", c("Electric stove","Solar cooker","LPG/cooking gas stove","Piped natural gas stove","Biogas stove","Kerosene stove/liquid fuel stove not using alcohol/ethanol","Manufactured solid fuel stove","Traditional solid fuel stove","Three stone stove/open fire","Other fuel","No food cooked in household"), wvar)
  fuel_group_w <- dist_wide(d, "res_grp", "fuel_group", c("Clean fuels and technologies","Solid fuels for cooking","Other fuel","No food cooked in household"), wvar)
  fuel_detail_w <- dist_wide(d, "res_grp", "fuel_cat", c("Coal/lignite","Charcoal","Wood","Straw/shrubs/grass","Agricultural crop","Animal dung/waste","Processed biomass (pellets) or woodchips","Garbage/plastic","Sawdust","Gasoline/diesel","Kerosene","Other fuel","No food cooked in household"), wvar)
  
  nn <- n_by_group(d %>% filter(res_grp %in% c("Urban","Rural")), "res_grp", c("Urban","Rural"))
  nn <- bind_rows(nn, tibble(group = "Total", N = sum(nn$N)))
  
  pick3 <- function(wide, cat) c(
    wide$Urban[wide$cat == cat][1] %||% NA_real_,
    wide$Rural[wide$cat == cat][1] %||% NA_real_,
    wide$Total[wide$cat == cat][1] %||% NA_real_
  )
  
  row3 <- function(label, vals) tibble(
    Characteristic = label,
    Urban = vals[1],
    Rural = vals[2],
    Total = vals[3]
  )
  
  body <- bind_rows(
    row3("Electricity", c(NA,NA,NA)),
    row3("   Yes", pick3(elec_w, "Yes")),
    row3("   No", pick3(elec_w, "No")),
    row3("Total", c(100,100,100)),
    row3("Main cooking technology", c(NA,NA,NA)),
    row3("   Clean fuels and technologies", pick3(tech_group_w, "Clean fuels and technologies")),
    row3("      Electric stove", pick3(tech_detail_w, "Electric stove")),
    row3("      Solar cooker", pick3(tech_detail_w, "Solar cooker")),
    row3("      LPG/cooking gas stove", pick3(tech_detail_w, "LPG/cooking gas stove")),
    row3("      Piped natural gas stove", pick3(tech_detail_w, "Piped natural gas stove")),
    row3("      Biogas stove", pick3(tech_detail_w, "Biogas stove")),
    row3("   Other fuels and technologies", pick3(tech_group_w, "Other fuels and technologies")),
    row3("      Kerosene stove/liquid fuel stove not using alcohol/ethanol", pick3(tech_detail_w, "Kerosene stove/liquid fuel stove not using alcohol/ethanol")),
    row3("      Manufactured solid fuel stove", pick3(tech_detail_w, "Manufactured solid fuel stove")),
    row3("      Traditional solid fuel stove", pick3(tech_detail_w, "Traditional solid fuel stove")),
    row3("      Three stone stove/open fire", pick3(tech_detail_w, "Three stone stove/open fire")),
    row3("      Other fuel", pick3(tech_detail_w, "Other fuel")),
    row3("   No food cooked in household", pick3(tech_group_w, "No food cooked in household")),
    row3("Total", c(100,100,100)),
    row3("Cooking fuel", c(NA,NA,NA)),
    row3("   Clean fuels and technologies", pick3(fuel_group_w, "Clean fuels and technologies")),
    row3("   Solid fuels for cooking", pick3(fuel_group_w, "Solid fuels for cooking")),
    row3("      Coal/lignite", pick3(fuel_detail_w, "Coal/lignite")),
    row3("      Charcoal", pick3(fuel_detail_w, "Charcoal")),
    row3("      Wood", pick3(fuel_detail_w, "Wood")),
    row3("      Straw/shrubs/grass", pick3(fuel_detail_w, "Straw/shrubs/grass")),
    row3("      Agricultural crop", pick3(fuel_detail_w, "Agricultural crop")),
    row3("      Animal dung/waste", pick3(fuel_detail_w, "Animal dung/waste")),
    row3("      Processed biomass (pellets) or woodchips", pick3(fuel_detail_w, "Processed biomass (pellets) or woodchips")),
    row3("      Garbage/plastic", pick3(fuel_detail_w, "Garbage/plastic")),
    row3("      Sawdust", pick3(fuel_detail_w, "Sawdust")),
    row3("   Other fuel", pick3(fuel_group_w, "Other fuel")),
    row3("      Gasoline/diesel", pick3(fuel_detail_w, "Gasoline/diesel")),
    row3("      Kerosene", pick3(fuel_detail_w, "Kerosene")),
    row3("      Other fuel", pick3(fuel_detail_w, "Other fuel")),
    row3("   No food cooked in household", pick3(fuel_group_w, "No food cooked in household")),
    row3("Total", c(100,100,100)),
    row3("Number of respondents", c(nn$N[nn$group=="Urban"], nn$N[nn$group=="Rural"], nn$N[nn$group=="Total"]))
  )
  
  list(type = "A", body = body, footnote = paste(collapse_footnotes("weighted_counts"), "LPG = Liquefied petroleum gas."))
}

build_table_2_7 <- function(dat) {
  need_cols(c("u_household","derived_residence","hh_has_radio","hh_has_tv","hh_own_mobile_phone","hh_has_non_mobile_phone","hh_has_computer","hh_has_refrigerator","hh_has_table","hh_has_chair","hh_has_bed","hh_has_sofa","hh_has_cupboard","hh_has_ac","hh_has_electric_iron","hh_has_generator","hh_has_fan","hh_own_bicycle","hh_own_animal_cart","hh_own_motorcycle","hh_own_car_truck","hh_own_motor_boat","hh_own_keke_napep","hh_owns_agri_land","hh_owns_livestock"), dat)
  wvar <- get_wvar(dat)
  
  d <- make_urt_frame(dat %>% filter(u_household == 1))
  
  items <- tribble(
    ~label, ~var,
    "Radio", "hh_has_radio",
    "Television", "hh_has_tv",
    "Mobile phone", "hh_own_mobile_phone",
    "Non-mobile telephone", "hh_has_non_mobile_phone",
    "Computer", "hh_has_computer",
    "Refrigerator", "hh_has_refrigerator",
    "Table", "hh_has_table",
    "Chair", "hh_has_chair",
    "Bed", "hh_has_bed",
    "Sofa", "hh_has_sofa",
    "Cupboard", "hh_has_cupboard",
    "Air conditioner", "hh_has_ac",
    "Electric iron", "hh_has_electric_iron",
    "Generator", "hh_has_generator",
    "Fan", "hh_has_fan",
    "Bicycle", "hh_own_bicycle",
    "Animal drawn cart", "hh_own_animal_cart",
    "Motorcycle/scooter", "hh_own_motorcycle",
    "Car/truck", "hh_own_car_truck",
    "Boat with a motor", "hh_own_motor_boat",
    "Keke napep", "hh_own_keke_napep",
    "Ownership of agricultural land", "hh_owns_agri_land",
    "Ownership of farm animals¹", "hh_owns_livestock"
  )
  
  out <- pmap_dfr(items, function(label, var) {
    tmp <- pct_yes_by_group(d, "res_grp", var, wvar, c("Urban","Rural","Total"))
    tibble(
      Characteristic = label,
      Urban = tmp$value[tmp$group=="Urban"],
      Rural = tmp$value[tmp$group=="Rural"],
      Total = tmp$value[tmp$group=="Total"]
    )
  })
  
  nn <- n_by_group(d %>% filter(res_grp %in% c("Urban","Rural")), "res_grp", c("Urban","Rural"))
  nn <- bind_rows(nn, tibble(group = "Total", N = sum(nn$N)))
  
  body <- bind_rows(
    tibble(Characteristic = "Household effects", Urban = NA, Rural = NA, Total = NA),
    out[1:15,],
    tibble(Characteristic = "Means of transport", Urban = NA, Rural = NA, Total = NA),
    out[16:21,],
    out[22:23,],
    tibble(Characteristic = "Number", Urban = nn$N[nn$group=="Urban"], Rural = nn$N[nn$group=="Rural"], Total = nn$N[nn$group=="Total"])
  )
  
  list(type = "A", body = body, footnote = paste(collapse_footnotes("weighted_counts"), "¹Cows, bulls, other cattle, horses, donkeys, mules, goats, sheep, chickens, or other poultry"))
}

build_table_2_8 <- function(dat) {
  need_cols(c("u_all", "derived_residence", "derived_zone", "demo_state_num", "derived_wealth_quintile"), dat)
  wvar <- get_wvar(dat)
  
  wealth_levels <- unname(WEALTH_LAB)
  
  d <- get_respondent_frame(dat) %>%
    filter(
      u_all == 1,
      !is.na(derived_wealth_quintile),
      to_num(derived_wealth_quintile) %in% 1:5
    ) %>%
    mutate(
      residence = case_when(
        to_num(derived_residence) == 1 ~ "Urban",
        to_num(derived_residence) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      zone = standardize_zone(derived_zone),
      state_lbl = STATE_LAB[as.character(to_num(demo_state_num))],
      wealth = WEALTH_LAB[as.character(to_num(derived_wealth_quintile))]
    )
  
  row_from_group <- function(dd, label) {
    n_unw <- nrow(dd)
    dist <- weighted_dist_1way(dd, "wealth", wealth_levels, wvar)
    
    tibble(
      Characteristic = label,
      Lowest = format_estimate_display(dist["Lowest"], n_unw, DISPLAY_DIGITS),
      Second = format_estimate_display(dist["Second"], n_unw, DISPLAY_DIGITS),
      Middle = format_estimate_display(dist["Middle"], n_unw, DISPLAY_DIGITS),
      Fourth = format_estimate_display(dist["Fourth"], n_unw, DISPLAY_DIGITS),
      Highest = format_estimate_display(dist["Highest"], n_unw, DISPLAY_DIGITS),
      Total = format_estimate_display(100, n_unw, DISPLAY_DIGITS),
      `Number of persons` = format_count_display(n_unw),
      base_status = get_base_status(n_unw)
    )
  }
  
  zone_states <- list(
    "North Central" = c("FCT-Abuja", "Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"),
    "North East"    = c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
    "North West"    = c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"),
    "South East"    = c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
    "South South"   = c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers"),
    "South West"    = c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")
  )
  
  zone_block <- function(z) {
    st <- zone_states[[z]]
    bind_rows(
      tibble(
        Characteristic = z,
        Lowest = "", Second = "", Middle = "",
        Fourth = "", Highest = "", Total = "",
        `Number of persons` = "",
        base_status = "ok"
      ),
      map_dfr(st, ~ row_from_group(d %>% filter(state_lbl == .x), .x))
    )
  }
  
  body <- bind_rows(
    tibble(
      Characteristic = "Residence",
      Lowest = "", Second = "", Middle = "",
      Fourth = "", Highest = "", Total = "",
      `Number of persons` = "",
      base_status = "ok"
    ),
    row_from_group(d %>% filter(residence == "Urban"), "Urban"),
    row_from_group(d %>% filter(residence == "Rural"), "Rural"),
    
    tibble(
      Characteristic = "Zone",
      Lowest = "", Second = "", Middle = "",
      Fourth = "", Highest = "", Total = "",
      `Number of persons` = "",
      base_status = "ok"
    ),
    map_dfr(ZONE_LAB, ~ row_from_group(d %>% filter(zone == .x), .x)),
    
    map_dfr(ZONE_LAB, zone_block),
    
    row_from_group(d, "Total")
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("wealth_sample_derived"),
    collapse_base_notes(body$base_status)
  )
  
  list(
    type = "G",
    body = body %>% select(-base_status),
    footnote = footnote
  )
}

build_table_2_9 <- function(dat) {
  need_cols(c("u_all", "demo_age_num", "demo_gender_num", "derived_residence"), dat)
  wvar <- get_wvar(dat)
  
  age_levels <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","Don't know")
  
  d <- get_respondent_frame(dat) %>%
    filter(u_all == 1) %>%
    mutate(
      age_num = to_num(demo_age_num),
      age_cat = case_when(
        age_num >= 15 & age_num <= 19 ~ "15-19",
        age_num >= 20 & age_num <= 24 ~ "20-24",
        age_num >= 25 & age_num <= 29 ~ "25-29",
        age_num >= 30 & age_num <= 34 ~ "30-34",
        age_num >= 35 & age_num <= 39 ~ "35-39",
        age_num >= 40 & age_num <= 44 ~ "40-44",
        age_num >= 45 & age_num <= 49 ~ "45-49",
        age_num >= 50 & age_num <= 54 ~ "50-54",
        age_num >= 55 & age_num <= 59 ~ "55-59",
        age_num >= 60 & age_num <= 64 ~ "60-64",
        age_num == 997 ~ "Don't know", 
        TRUE ~ NA_character_
      ),
      sex_grp = case_when(
        to_num(demo_gender_num) == 1 ~ "Male",
        to_num(demo_gender_num) == 2 ~ "Female",
        TRUE ~ NA_character_
      ),
      res_grp = case_when(
        to_num(derived_residence) == 1 ~ "Urban",
        to_num(derived_residence) == 2 ~ "Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_cat), !is.na(sex_grp), !is.na(res_grp))
  
  subgroup_dist <- function(dd) {
    vals <- weighted_dist_1way(dd, "age_cat", age_levels, wvar)
    as.numeric(vals[age_levels])
  }
  
  subgroup_n <- function(dd) nrow(dd)
  
  fmt_subgroup <- function(dd) {
    n_unw <- subgroup_n(dd)
    vals <- subgroup_dist(dd)
    setNames(as.list(map_chr(vals, ~ format_estimate_display(.x, n_unw, DISPLAY_DIGITS))), age_levels)
  }
  
  u_m <- d %>% filter(res_grp == "Urban", sex_grp == "Male")
  u_f <- d %>% filter(res_grp == "Urban", sex_grp == "Female")
  u_t <- d %>% filter(res_grp == "Urban")
  r_m <- d %>% filter(res_grp == "Rural", sex_grp == "Male")
  r_f <- d %>% filter(res_grp == "Rural", sex_grp == "Female")
  r_t <- d %>% filter(res_grp == "Rural")
  t_m <- d %>% filter(sex_grp == "Male")
  t_f <- d %>% filter(sex_grp == "Female")
  t_t <- d
  
  um <- fmt_subgroup(u_m); uf <- fmt_subgroup(u_f); ut <- fmt_subgroup(u_t)
  rm <- fmt_subgroup(r_m); rf <- fmt_subgroup(r_f); rt <- fmt_subgroup(r_t)
  tm <- fmt_subgroup(t_m); tf <- fmt_subgroup(t_f); tt <- fmt_subgroup(t_t)
  
  body <- tibble(
    Age = age_levels,
    Urban_Male   = unlist(um[age_levels]),
    Urban_Female = unlist(uf[age_levels]),
    Urban_Total  = unlist(ut[age_levels]),
    Rural_Male   = unlist(rm[age_levels]),
    Rural_Female = unlist(rf[age_levels]),
    Rural_Total  = unlist(rt[age_levels]),
    Total_Male   = unlist(tm[age_levels]),
    Total_Female = unlist(tf[age_levels]),
    Total_Total  = unlist(tt[age_levels])
  )
  
  body <- bind_rows(
    body,
    tibble(
      Age = "Total",
      Urban_Male = format_estimate_display(100, subgroup_n(u_m), 1),
      Urban_Female = format_estimate_display(100, subgroup_n(u_f), 1),
      Urban_Total = format_estimate_display(100, subgroup_n(u_t), 1),
      Rural_Male = format_estimate_display(100, subgroup_n(r_m), 1),
      Rural_Female = format_estimate_display(100, subgroup_n(r_f), 1),
      Rural_Total = format_estimate_display(100, subgroup_n(r_t), 1),
      Total_Male = format_estimate_display(100, subgroup_n(t_m), 1),
      Total_Female = format_estimate_display(100, subgroup_n(t_f), 1),
      Total_Total = format_estimate_display(100, subgroup_n(t_t), 1)
    ),
    tibble(
      Age = "Number of persons",
      Urban_Male = format_count_display(subgroup_n(u_m)),
      Urban_Female = format_count_display(subgroup_n(u_f)),
      Urban_Total = format_count_display(subgroup_n(u_t)),
      Rural_Male = format_count_display(subgroup_n(r_m)),
      Rural_Female = format_count_display(subgroup_n(r_f)),
      Rural_Total = format_count_display(subgroup_n(r_t)),
      Total_Male = format_count_display(subgroup_n(t_m)),
      Total_Female = format_count_display(subgroup_n(t_f)),
      Total_Total = format_count_display(subgroup_n(t_t))
    )
  )
  
  statuses <- c(
    get_base_status(subgroup_n(u_m)), get_base_status(subgroup_n(u_f)), get_base_status(subgroup_n(u_t)),
    get_base_status(subgroup_n(r_m)), get_base_status(subgroup_n(r_f)), get_base_status(subgroup_n(r_t)),
    get_base_status(subgroup_n(t_m)), get_base_status(subgroup_n(t_f)), get_base_status(subgroup_n(t_t))
  )
  
  list(
    type = "D",
    body = body,
    footnote = paste(
      collapse_footnotes("weighted_counts"),
      collapse_footnotes("sample_composition_note"),
      collapse_base_notes(statuses)
    )
  )
}

build_table_2_10 <- function(dat) {
  need_cols(c("u_household", "derived_residence", "hh_total_persons_usually_v3"), dat)
  wvar <- get_wvar(dat)
  
  size_levels <- c("1","2","3","4","5","6","7","8","9+")
  
  d <- dat %>%
    filter(u_household == 1) %>%
    mutate(
      res_grp = case_when(
        to_num(derived_residence) == 1 ~ "Urban",
        to_num(derived_residence) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      hh_size = to_num(hh_total_persons_usually_v3),
      hh_size_cat = case_when(
        hh_size == 1 ~ "1",
        hh_size == 2 ~ "2",
        hh_size == 3 ~ "3",
        hh_size == 4 ~ "4",
        hh_size == 5 ~ "5",
        hh_size == 6 ~ "6",
        hh_size == 7 ~ "7",
        hh_size == 8 ~ "8",
        hh_size >= 9 ~ "9+",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(res_grp), !is.na(hh_size_cat))
  
  d_all <- bind_rows(d, d %>% mutate(res_grp = "Total"))
  
  dist <- weighted_row_dist(
    d_all,
    row_var = "res_grp",
    col_var = "hh_size_cat",
    row_levels = c("Urban", "Rural", "Total"),
    col_levels = size_levels,
    wvar = wvar
  )
  
  pick <- function(grp, cat) {
    dist$p[dist$row == grp & dist$col == cat][1] %||% NA_real_
  }
  
  n_grp <- d_all %>%
    filter(res_grp %in% c("Urban", "Rural", "Total")) %>%
    count(res_grp, name = "N")
  
  n_pick <- function(grp) n_grp$N[n_grp$res_grp == grp][1] %||% 0
  
  mean_grp <- function(grp) {
    dd <- d_all %>% filter(res_grp == grp)
    ww <- if (!is.null(wvar)) dd[[wvar]] else NULL
    mean_from_numeric(dd$hh_size, ww)
  }
  
  body <- bind_rows(
    tibble(Characteristic = "Number of usual members", Urban = "", Rural = "", Total = ""),
    tibble(
      Characteristic = size_levels,
      Urban = map_chr(size_levels, ~ format_estimate_display(pick("Urban", .x), n_pick("Urban"), 1)),
      Rural = map_chr(size_levels, ~ format_estimate_display(pick("Rural", .x), n_pick("Rural"), 1)),
      Total = map_chr(size_levels, ~ format_estimate_display(pick("Total", .x), n_pick("Total"), 1))
    ),
    tibble(
      Characteristic = "Total",
      Urban = format_estimate_display(100, n_pick("Urban"), 1),
      Rural = format_estimate_display(100, n_pick("Rural"), 1),
      Total = format_estimate_display(100, n_pick("Total"), 1)
    ),
    tibble(
      Characteristic = "Mean size of households",
      Urban = format_estimate_display(mean_grp("Urban"), n_pick("Urban"), 1),
      Rural = format_estimate_display(mean_grp("Rural"), n_pick("Rural"), 1),
      Total = format_estimate_display(mean_grp("Total"), n_pick("Total"), 1)
    ),
    tibble(
      Characteristic = "Number of households",
      Urban = format_count_display(n_pick("Urban")),
      Rural = format_count_display(n_pick("Rural")),
      Total = format_count_display(n_pick("Total"))
    )
  )
  
  statuses <- c(get_base_status(n_pick("Urban")), get_base_status(n_pick("Rural")), get_base_status(n_pick("Total")))
  
  list(
    type = "E",
    body = body,
    footnote = paste(
      collapse_footnotes("weighted_counts"),
      collapse_base_notes(statuses)
    )
  )
}

build_table_2_11 <- function(dat, by_state = FALSE) {
  need_cols(c(
    "u_women_15_49", "derived_age_group_w", "bg_religion", "derived_residence",
    "derived_zone", "derived_wealth_quintile", "derived_edu_cat", "demo_state_num"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_respondent_frame(dat) %>%
    filter(u_women_15_49 == 1) %>%
    mutate(
      age_grp = AGE_W_LAB[as.character(to_num(derived_age_group_w))],
      religion = case_when(
        to_num(bg_religion) == 1 ~ "Catholic",
        to_num(bg_religion) == 2 ~ "Christian",
        to_num(bg_religion) == 3 ~ "Islam",
        to_num(bg_religion) == 4 ~ "Traditional",
        to_num(bg_religion) == 6 ~ "Other",
        TRUE ~ NA_character_
      ),
      residence = case_when(
        to_num(derived_residence) == 1 ~ "Urban",
        to_num(derived_residence) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      zone = standardize_zone(derived_zone),
      wealth = WEALTH_LAB[as.character(to_num(derived_wealth_quintile))],
      edu = EDU4_LAB[as.character(to_num(derived_edu_cat))],
      state_lbl = STATE_LAB[as.character(to_num(demo_state_num))]
    )
  
  if (by_state) {
    states <- sort(unique(d$state_lbl))
    states <- states[!is.na(states)]
    
    body <- map_dfr(states, function(st) {
      dd <- d %>% filter(state_lbl == st)
      tibble(
        Characteristic = st,
        `Weighted percent` = format_estimate_display(100, nrow(dd), 1),
        `Unweighted number` = format_count_display(nrow(dd)),
        base_status = get_base_status(nrow(dd))
      )
    })
    
    return(list(
      type = "B",
      body = body %>% select(-base_status),
      footnote = paste(
        collapse_footnotes("weighted_counts"),
        collapse_footnotes("state_sample_caution"),
        collapse_base_notes(body$base_status)
      )
    ))
  }
  
  add_block <- function(label, var, levels) {
    dd <- d %>% filter(!is.na(.data[[var]]))
    dist <- weighted_dist_1way(dd, var, levels, wvar)
    
    cnt <- dd %>% count(.data[[var]], name = "n")
    nvec <- setNames(rep(0, length(levels)), levels)
    nvec[as.character(cnt[[var]])] <- cnt$n
    
    bind_rows(
      tibble(Characteristic = label, `Weighted percent` = "", `Unweighted number` = "", base_status = "ok"),
      tibble(
        Characteristic = levels,
        `Weighted percent` = map_chr(levels, ~ format_estimate_display(dist[.x], nvec[.x], 1)),
        `Unweighted number` = map_chr(levels, ~ format_count_display(nvec[.x])),
        base_status = map_chr(levels, ~ get_base_status(nvec[.x]))
      )
    )
  }
  
  body <- bind_rows(
    add_block("Age", "age_grp", unname(AGE_W_LAB)),
    add_block("Religion", "religion", c("Catholic", "Christian", "Islam", "Traditional", "Other")),
    add_block("Residence", "residence", c("Urban", "Rural")),
    add_block("Zone", "zone", ZONE_LAB),
    add_block("Wealth quintile", "wealth", unname(WEALTH_LAB)),
    add_block("Education", "edu", unname(EDU4_LAB)),
    tibble(Characteristic = "Number of women", `Weighted percent` = "", `Unweighted number` = format_count_display(nrow(d)), base_status = "ok")
  )
  
  list(
    type = "B",
    body = body %>% select(-base_status),
    footnote = paste(
      collapse_footnotes("weighted_counts"),
      collapse_base_notes(body$base_status)
    )
  )
}

build_table_2_12 <- function(dat, by_state = FALSE) {
  need_cols(c(
    "u_women_15_49", "derived_age_group_w", "derived_residence",
    "derived_zone", "derived_wealth_quintile", "derived_edu_cat", "demo_state_num"
  ), dat)
  
  wvar <- get_wvar(dat)
  edu_levels <- unname(EDU_COL_LAB)
  
  d <- get_respondent_frame(dat) %>%
    filter(u_women_15_49 == 1) %>%
    mutate(
      age_grp = AGE_W_LAB[as.character(to_num(derived_age_group_w))],
      residence = case_when(
        to_num(derived_residence) == 1 ~ "Urban",
        to_num(derived_residence) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      zone = standardize_zone(derived_zone),
      wealth = WEALTH_LAB[as.character(to_num(derived_wealth_quintile))],
      edu = EDU_COL_LAB[as.character(to_num(derived_edu_cat))],
      state_lbl = STATE_LAB[as.character(to_num(demo_state_num))]
    )
  
  row_from_group <- function(dd, label) {
    n_unw <- nrow(dd)
    dist <- weighted_dist_1way(dd, "edu", edu_levels, wvar)
    tibble(
      Characteristic = label,
      `No education` = format_estimate_display(dist["No education"], n_unw, DISPLAY_DIGITS),
      `Completed primary` = format_estimate_display(dist["Completed primary"], n_unw, DISPLAY_DIGITS),
      `Completed secondary` = format_estimate_display(dist["Completed secondary"], n_unw, DISPLAY_DIGITS),
      `More than secondary` = format_estimate_display(dist["More than secondary"], n_unw, DISPLAY_DIGITS),
      Total = format_estimate_display(100, n_unw, DISPLAY_DIGITS),
      `Number of women` = format_count_display(n_unw),
      base_status = get_base_status(n_unw)
    )
  }
  
  if (by_state) {
    states <- sort(unique(d$state_lbl))
    states <- states[!is.na(states)]
    
    body <- map_dfr(states, function(st) {
      row_from_group(d %>% filter(state_lbl == st), st)
    })
    
    return(list(
      type = "F",
      body = body %>% select(-base_status),
      footnote = paste(
        collapse_footnotes("weighted_counts"),
        "1 No education includes informal education (adult education, Tsangaya, or Quranic).",
        collapse_footnotes("state_sample_caution"),
        collapse_base_notes(body$base_status)
      )
    ))
  }
  
  body <- bind_rows(
    tibble(Characteristic = "Age", `No education` = "", `Completed primary` = "", `Completed secondary` = "", `More than secondary` = "", Total = "", `Number of women` = "", base_status = "ok"),
    map_dfr(unname(AGE_W_LAB), ~ row_from_group(d %>% filter(age_grp == .x), .x)),
    tibble(Characteristic = "Residence", `No education` = "", `Completed primary` = "", `Completed secondary` = "", `More than secondary` = "", Total = "", `Number of women` = "", base_status = "ok"),
    map_dfr(c("Urban", "Rural"), ~ row_from_group(d %>% filter(residence == .x), .x)),
    tibble(Characteristic = "Zone", `No education` = "", `Completed primary` = "", `Completed secondary` = "", `More than secondary` = "", Total = "", `Number of women` = "", base_status = "ok"),
    map_dfr(ZONE_LAB, ~ row_from_group(d %>% filter(zone == .x), .x)),
    tibble(Characteristic = "Wealth quintile", `No education` = "", `Completed primary` = "", `Completed secondary` = "", `More than secondary` = "", Total = "", `Number of women` = "", base_status = "ok"),
    map_dfr(unname(WEALTH_LAB), ~ row_from_group(d %>% filter(wealth == .x), .x)),
    row_from_group(d, "Total")
  )
  
  list(
    type = "F",
    body = body %>% select(-base_status),
    footnote = paste(
      collapse_footnotes("weighted_counts"),
      "1 No education includes informal education (adult education, Tsangaya, or Quranic).",
      collapse_base_notes(body$base_status)
    )
  )
}

# ------------------------------------------------------------------------------
# 5. Driver for phase 1 tables
# ------------------------------------------------------------------------------
run_phase1 <- function(dat, out_xlsx = OUT_XLSX) {
  wb <- wb_init(out_xlsx)
  
  specs <- list(
    list(sheet = "Table 2.1",    title = "Table 2.1 Household drinking water", build = function() build_table_2_1(dat)),
    list(sheet = "Table 2.3",    title = "Table 2.3 Household sanitation facilities", build = function() build_table_2_3(dat)),
    list(sheet = "Table 2.5",    title = "Table 2.5 Household characteristics: Construction materials and rooms used for sleeping", build = function() build_table_2_5(dat)),
    list(sheet = "Table 2.6",    title = "Table 2.6 Household energy and cooking characteristics", build = function() build_table_2_6(dat)),
    list(sheet = "Table 2.7",    title = "Table 2.7 Household possessions", build = function() build_table_2_7(dat)),
    list(sheet = "Table 2.8",    title = "Table 2.8 Distribution of respondents by wealth quintile", build = function() build_table_2_8(dat)),
    list(sheet = "Table 2.9",    title = "Table 2.9 Distribution of respondents by age, sex, and residence", build = function() build_table_2_9(dat)),
    list(sheet = "Table 2.10",   title = "Table 2.10 Household composition indicators", build = function() build_table_2_10(dat)),
    list(sheet = "Table 2.11",   title = "Table 2.11 Background characteristics of women age 15-49", build = function() build_table_2_11(dat, FALSE)),
    list(sheet = "Table 2.11.2", title = "Table 2.11.2 Background characteristics of women age 15-49 by state", build = function() build_table_2_11(dat, TRUE)),
    list(sheet = "Table 2.12",   title = "Table 2.12 Educational attainment of women age 15-49", build = function() build_table_2_12(dat, FALSE)),
    list(sheet = "Table 2.12.2", title = "Table 2.12.2 Educational attainment of women age 15-49 by state", build = function() build_table_2_12(dat, TRUE))
  )
  
  for (sp in specs) {
    message("Building ", sp$sheet, " ...")
    obj <- sp$build()
    
    writer <- switch(
      obj$type,
      A = write_table_A,
      B = write_table_B,
      D = write_table_D,
      E = write_table_E,
      F = write_table_F,
      G = write_table_G,
      stop("Unsupported writer type: ", obj$type)
    )
    
    writer(wb, sp$sheet, sp$title, obj$body, obj$footnote)
  }
  
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  message("✅ Phase 1 tables written to: ", out_xlsx)
}


# ------------------------------------------------------------------------------
# 6. Execute phase 1
# ------------------------------------------------------------------------------
run_phase1(df, OUT_XLSX)