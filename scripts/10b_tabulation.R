# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  11_Sproxil_Master_Tabulation_Engine_Phase2.R
# AUTHOR:  Corona Management Systems
# DATE:    November 26, 2025
#
# PURPOSE:
#   Generate the next implemented block of Sproxil MIS-style descriptive tables
#   from the final weighted analysis-ready dataset using a single engine-style
#   script that extends the Phase 1 design.
#
# INCLUDED IN THIS PHASE:
#   - Table 2.13
#   - Table 2.15
#   - Table 3.1
#   - Table 3.2
#   - Table 3.8
#
# DESIGN PRINCIPLES:
#   - One input dataset, one workbook, one locked production weight.
#   - Percentages use the final calibrated trimmed weight where applied.
#   - Count columns remain unweighted.
#   - Low-base handling:
#       * show estimate with * if unweighted denominator is 10-24
#       * show "-" if unweighted denominator is <10
#   - Builders return plain data frames and metadata; writers handle formatting.
#   - Household mosquito-net tables are implemented as Sproxil household proxy
#     tabulations rather than full DHS de facto net-roster estimates.
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

WEALTH_LAB <- c(
  `1` = "Lowest",
  `2` = "Second",
  `3` = "Middle",
  `4` = "Fourth",
  `5` = "Highest"
)

EDU4_LAB <- c(
  `1` = "No education",
  `2` = "Primary",
  `3` = "Secondary",
  `4` = "More than secondary"
)

AGE_W_LAB <- c(
  `1` = "15-19", `2` = "20-24", `3` = "25-29", `4` = "30-34",
  `5` = "35-39", `6` = "40-44", `7` = "45-49"
)

ZONE_LAB <- c(
  "North Central", "North East", "North West",
  "South East", "South South", "South West"
)

FOOTNOTE_BANK <- list(
  weighted_counts = "Percentages are weighted using the final calibrated trimmed weight (w_calibrated_trim); count columns are unweighted.",
  subgroup_caution = "Weight diagnostics showed strongest performance for national and state estimation; subgroup distributions shown here should be interpreted as descriptive weighted tabulations.",
  state_sample_caution = "State-level estimates are weighted descriptive estimates for the survey sample and should be interpreted cautiously.",
  wealth_sample_derived = "Wealth quintiles are based on a sample-derived household living-standards index and reflect relative socioeconomic position within the Sproxil sample.",
  low_base_flag = "* Estimate based on 10-24 unweighted observations; interpret with caution.",
  low_base_suppress = "- Estimate suppressed because the unweighted denominator is less than 10.",
  net_proxy = "Mosquito-net indicators are based on the Sproxil household-report structure and should be interpreted as household proxy estimates rather than full DHS de facto net-roster estimates.",
  itn_access_proxy = "ITN access is a conservative household proxy based on confirmed ITN information and usual household members.",
  women_only = "This table is restricted to interviewed women age 15-49.",
  informal_edu_note = "Informal schooling categories are based on reported attendance among interviewed women.",
  tv_note = "Television exposure is defined as watching television at least once a week."
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

get_state_label <- function(x) {
  STATE_LAB[as.character(to_num(x))]
}

get_respondent_frame <- function(dat) {
  if ("u_all" %in% names(dat)) dat %>% filter(u_all == 1) else dat
}

get_household_frame <- function(dat) {
  dat %>%
    filter(u_household == 1) %>%
    mutate(
      residence = case_when(
        to_num(derived_residence) == 1 ~ "Urban",
        to_num(derived_residence) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      zone = standardize_zone(derived_zone),
      state_lbl = get_state_label(demo_state_num),
      wealth = WEALTH_LAB[as.character(to_num(derived_wealth_quintile))]
    )
}

get_women_frame <- function(dat) {
  dat %>%
    filter(u_women_15_49 == 1) %>%
    mutate(
      age_grp = AGE_W_LAB[as.character(to_num(derived_age_group_w))],
      residence = case_when(
        to_num(derived_residence) == 1 ~ "Urban",
        to_num(derived_residence) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      zone = standardize_zone(derived_zone),
      state_lbl = get_state_label(demo_state_num),
      wealth = WEALTH_LAB[as.character(to_num(derived_wealth_quintile))],
      edu = EDU4_LAB[as.character(to_num(derived_edu_cat))]
    )
}

weighted_pct_binary <- function(x, w = NULL) {
  if (is.null(w)) {
    ok <- !is.na(x)
    if (!any(ok)) return(NA_real_)
    return(100 * mean(x[ok] == 1))
  }
  ok <- !is.na(x) & !is.na(w) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sw <- sum(w[ok])
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  100 * sum(w[ok] * (x[ok] == 1), na.rm = TRUE) / sw
}

weighted_mean_num <- function(x, w = NULL) {
  if (is.null(w)) {
    ok <- !is.na(x)
    if (!any(ok)) return(NA_real_)
    return(mean(x[ok]))
  }
  ok <- !is.na(x) & !is.na(w) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sw <- sum(w[ok])
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  sum(w[ok] * x[ok], na.rm = TRUE) / sw
}

weighted_dist_1way <- function(dat, cat_var, levels, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[cat_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  if (nrow(d) == 0) return(setNames(rep(NA_real_, length(levels)), levels))
  
  out <- if (is.null(wvar)) {
    d %>% count(.data[[cat_var]], name = "n") %>% mutate(p = 100 * n / sum(n))
  } else {
    d %>% group_by(.data[[cat_var]]) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      mutate(p = 100 * w / sum(w))
  }
  
  vals <- setNames(rep(0, length(levels)), levels)
  tmp <- out$p
  names(tmp) <- as.character(out[[cat_var]])
  vals[names(tmp)] <- round(tmp, DISPLAY_DIGITS)
  vals
}

get_base_status <- function(n) {
  case_when(
    is.na(n) ~ "ok",
    n < 10 ~ "suppress",
    n < 25 ~ "flag",
    TRUE ~ "ok"
  )
}

apply_base_rule_scalar <- function(value, n, digits = 1) {
  status <- get_base_status(n)
  if (status == "suppress") return(NA_real_)
  round(value, digits)
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

base_note_suffix <- function(dat, base_var) {
  if (all(is.na(dat[[base_var]]))) return(NULL)
  status <- unique(na.omit(dat[[base_var]]))
  bits <- c()
  if ("flag" %in% status) bits <- c(bits, collapse_footnotes("low_base_flag"))
  if ("suppress" %in% status) bits <- c(bits, collapse_footnotes("low_base_suppress"))
  if (length(bits) == 0) return(NULL)
  paste(bits, collapse = " ")
}

make_base_fields_binary <- function(dat, x_var, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[x_var]]), .data[[x_var]] %in% c(0, 1))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  d
}

make_base_fields_numeric <- function(dat, x_var, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[x_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  d
}

summ_binary_row <- function(dat, label, x_var, wvar = NULL) {
  d <- make_base_fields_binary(dat, x_var, wvar)
  n_unw <- nrow(d)
  pct <- weighted_pct_binary(d[[x_var]], if (!is.null(wvar)) d[[wvar]] else NULL)
  tibble(
    Characteristic = label,
    Percent = format_estimate_display(pct, n_unw, DISPLAY_DIGITS),
    `Unweighted number` = format_count_display(n_unw),
    base_status = get_base_status(n_unw)
  )
}

summ_mean_row <- function(dat, label, x_var, wvar = NULL) {
  d <- make_base_fields_numeric(dat, x_var, wvar)
  n_unw <- nrow(d)
  est <- weighted_mean_num(d[[x_var]], if (!is.null(wvar)) d[[wvar]] else NULL)
  tibble(
    Characteristic = label,
    Percent = format_estimate_display(est, n_unw, DISPLAY_DIGITS),
    `Unweighted number` = format_count_display(n_unw),
    base_status = get_base_status(n_unw)
  )
}

style_bank <- function() {
  list(
    title  = createStyle(textDecoration = "bold", fontSize = 12),
    header = createStyle(textDecoration = "bold", halign = "center", valign = "center",
                         wrapText = TRUE, border = "TopBottomLeftRight"),
    stub   = createStyle(wrapText = TRUE),
    note   = createStyle(textDecoration = "italic", fontSize = 9),
    pct    = createStyle(numFmt = "0.0"),
    int    = createStyle(numFmt = "0")
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

summ_dist_row <- function(dat, cat_var, levels, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[cat_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  n_unw <- nrow(d)
  dist <- weighted_dist_1way(d, cat_var, levels, wvar)
  list(
    n = n_unw,
    dist = dist,
    stat = get_base_status(n_unw)
  )
}

table_2_13_template_row <- function(label, stub_name = "Background characteristic") {
  nm <- if (stub_name == "State") "State" else "Background characteristic"
  out <- tibble::tibble(
    x1 = label,
    x2 = "",
    x3 = "",
    x4 = "",
    x5 = "",
    x6 = "",
    x7 = "",
    x8 = "",
    base_status = "ok"
  )
  names(out) <- c(
    nm,
    "Percentage of women who attended informal schooling",
    "Number of women with no formal education",
    "Adult education",
    "Tsangaya",
    "Quranic",
    "Total",
    "Number of women who attended informal schooling",
    "base_status"
  )
  out
}

women_state_zone_blocks <- function(d, maker_fun, template_row_fun) {
  bind_rows(
    template_row_fun("North Central", "State"),
    map_dfr(c("FCT-Abuja", "Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"),
            ~ maker_fun(d %>% filter(state_lbl == .x), .x)),
    
    template_row_fun("North East", "State"),
    map_dfr(c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
            ~ maker_fun(d %>% filter(state_lbl == .x), .x)),
    
    template_row_fun("North West", "State"),
    map_dfr(c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"),
            ~ maker_fun(d %>% filter(state_lbl == .x), .x)),
    
    template_row_fun("South East", "State"),
    map_dfr(c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
            ~ maker_fun(d %>% filter(state_lbl == .x), .x)),
    
    template_row_fun("South South", "State"),
    map_dfr(c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers"),
            ~ maker_fun(d %>% filter(state_lbl == .x), .x)),
    
    template_row_fun("South West", "State"),
    map_dfr(c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"),
            ~ maker_fun(d %>% filter(state_lbl == .x), .x)),
    
    maker_fun(d, "Total")
  )
}

# ------------------------------------------------------------------------------
# 4. Writer functions
# ------------------------------------------------------------------------------
write_table_B <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 6
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = TRUE)
  addStyle(wb, sheet, s$header, rows = start_row, cols = 2:(ncol(body)+1), gridExpand = TRUE, stack = TRUE)
  
  int_cols <- which(grepl("number|Number", names(body))) + 1
  if (length(int_cols) > 0) addStyle(wb, sheet, s$int, rows = (start_row+1):(start_row+nrow(body)), cols = int_cols, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 36)
  if (ncol(body) > 1) setColWidths(wb, sheet, cols = 3:(ncol(body)+1), widths = 14)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_H <- function(wb, sheet, title, body, footnote = NULL,
                          left_header = "Background characteristic",
                          middle_header = "Indicators",
                          middle_cols = 3:8,
                          right_headers = c("Total", "Unweighted number")) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, left_header, startRow = 6, startCol = 2, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = min(middle_cols):max(middle_cols), rows = 6)
  writeData(wb, sheet, middle_header, startRow = 6, startCol = min(middle_cols), colNames = FALSE)
  
  body_names <- names(body)
  mid_names <- body_names[(min(middle_cols)-1):(max(middle_cols)-1)]
  writeData(wb, sheet, mid_names, startRow = 7, startCol = min(middle_cols), colNames = FALSE)
  
  extra_start <- max(middle_cols) + 1
  for (i in seq_along(right_headers)) {
    mergeCells(wb, sheet, cols = extra_start + i - 1, rows = 6:7)
    writeData(wb, sheet, right_headers[i], startRow = 6, startCol = extra_start + i - 1, colNames = FALSE)
  }
  
  addStyle(wb, sheet, s$header, rows = 6:7, cols = 2:(ncol(body)+1), gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = FALSE)
  
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  
  num_like <- which(grepl("number|Number", names(body), ignore.case = TRUE)) + 1
  if (length(num_like) > 0) addStyle(wb, sheet, s$int, rows = start_row:(start_row + nrow(body) - 1), cols = num_like, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 30)
  setColWidths(wb, sheet, cols = 3:(ncol(body)+1), widths = 13)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_2_13 <- function(wb, sheet, title, body, footnote = NULL, stub_header = "Background characteristic") {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, stub_header, startRow = 6, startCol = 2, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 3, rows = 6:7)
  writeData(wb, sheet, "Percentage of women who attended informal schooling", startRow = 6, startCol = 3, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 4, rows = 6:7)
  writeData(wb, sheet, "Number of women with no formal education", startRow = 6, startCol = 4, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 5:7, rows = 6)
  writeData(wb, sheet, "Type of informal schooling attended", startRow = 6, startCol = 5, colNames = FALSE)
  
  writeData(wb, sheet, c("Adult education", "Tsangaya", "Quranic"), startRow = 7, startCol = 5, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 8, rows = 6:7)
  writeData(wb, sheet, "Total", startRow = 6, startCol = 8, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 9, rows = 6:7)
  writeData(wb, sheet, "Number of women who attended informal schooling", startRow = 6, startCol = 9, colNames = FALSE)
  
  addStyle(wb, sheet, s$header, rows = 6:7, cols = 2:9, gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = FALSE)
  
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, s$int, rows = start_row:(start_row + nrow(body) - 1), cols = c(4, 9), gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 28)
  setColWidths(wb, sheet, cols = 3:9, widths = 14)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

# ------------------------------------------------------------------------------
# 5. Builder functions
# ------------------------------------------------------------------------------

# ---- Table 2.13 ---------------------------------------------------------------
build_table_2_13_1 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "demo_edu_level",
    "edu_inf_adult", "edu_inf_tsangaya", "edu_inf_quranic",
    "derived_age_group_w", "derived_residence", "derived_zone", "derived_wealth_quintile"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_women_frame(dat) %>%
    mutate(
      no_formal = case_when(
        to_num(demo_edu_level) == 0 ~ 1,
        to_num(demo_edu_level) %in% 1:4 ~ 0,
        TRUE ~ NA_real_
      ),
      informal_any = case_when(
        no_formal != 1 ~ NA_real_,
        edu_inf_adult == 1 | edu_inf_tsangaya == 1 | edu_inf_quranic == 1 ~ 1,
        edu_inf_adult == 0 & edu_inf_tsangaya == 0 & edu_inf_quranic == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      informal_type = case_when(
        no_formal != 1 ~ NA_character_,
        edu_inf_adult == 1 ~ "Adult education",
        edu_inf_tsangaya == 1 ~ "Tsangaya",
        edu_inf_quranic == 1 ~ "Quranic",
        TRUE ~ NA_character_
      )
    )
  
  make_row <- function(dd, label) {
    d_nf <- dd %>%
      filter(no_formal == 1, !is.na(informal_any))
    if (!is.null(wvar)) d_nf <- d_nf %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    
    n_nf <- nrow(d_nf)
    pct_any <- weighted_pct_binary(d_nf$informal_any, if (!is.null(wvar)) d_nf[[wvar]] else NULL)
    
    d_inf <- d_nf %>% filter(informal_any == 1, !is.na(informal_type))
    n_inf <- nrow(d_inf)
    dist <- weighted_dist_1way(
      d_inf,
      "informal_type",
      c("Adult education", "Tsangaya", "Quranic"),
      wvar
    )
    
    tibble(
      `Background characteristic` = label,
      `Percentage of women who attended informal schooling` = format_estimate_display(pct_any, n_nf, DISPLAY_DIGITS),
      `Number of women with no formal education` = format_count_display(n_nf),
      `Adult education` = format_estimate_display(dist["Adult education"], n_inf, DISPLAY_DIGITS),
      `Tsangaya` = format_estimate_display(dist["Tsangaya"], n_inf, DISPLAY_DIGITS),
      `Quranic` = format_estimate_display(dist["Quranic"], n_inf, DISPLAY_DIGITS),
      Total = format_estimate_display(ifelse(n_inf > 0, 100, NA_real_), n_inf, DISPLAY_DIGITS),
      `Number of women who attended informal schooling` = format_count_display(n_inf),
      base_status = case_when(
        get_base_status(n_nf) == "suppress" | get_base_status(n_inf) == "suppress" ~ "suppress",
        get_base_status(n_nf) == "flag" | get_base_status(n_inf) == "flag" ~ "flag",
        TRUE ~ "ok"
      )
    )
  }
  
  body <- bind_rows(
    table_2_13_template_row("Age"),
    map_dfr(unname(AGE_W_LAB), ~ make_row(d %>% filter(age_grp == .x), .x)),
    
    table_2_13_template_row("Residence"),
    map_dfr(c("Urban", "Rural"), ~ make_row(d %>% filter(residence == .x), .x)),
    
    table_2_13_template_row("Zone"),
    map_dfr(ZONE_LAB, ~ make_row(d %>% filter(zone == .x), .x)),
    
    table_2_13_template_row("Wealth quintile"),
    map_dfr(unname(WEALTH_LAB), ~ make_row(d %>% filter(wealth == .x), .x)),
    
    make_row(d, "Total")
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("informal_edu_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "2.13_custom", body = body %>% select(-base_status), footnote = footnote, stub_header = "Background characteristic")
}

build_table_2_13_2 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "demo_edu_level", "demo_state_num", "derived_zone",
    "edu_inf_adult", "edu_inf_tsangaya", "edu_inf_quranic"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_women_frame(dat) %>%
    mutate(
      no_formal = case_when(
        to_num(demo_edu_level) == 0 ~ 1,
        to_num(demo_edu_level) %in% 1:4 ~ 0,
        TRUE ~ NA_real_
      ),
      informal_any = case_when(
        no_formal != 1 ~ NA_real_,
        edu_inf_adult == 1 | edu_inf_tsangaya == 1 | edu_inf_quranic == 1 ~ 1,
        edu_inf_adult == 0 & edu_inf_tsangaya == 0 & edu_inf_quranic == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      informal_type = case_when(
        no_formal != 1 ~ NA_character_,
        edu_inf_adult == 1 ~ "Adult education",
        edu_inf_tsangaya == 1 ~ "Tsangaya",
        edu_inf_quranic == 1 ~ "Quranic",
        TRUE ~ NA_character_
      )
    )
  
  make_row <- function(dd, label) {
    d_nf <- dd %>%
      filter(no_formal == 1, !is.na(informal_any))
    if (!is.null(wvar)) d_nf <- d_nf %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    
    n_nf <- nrow(d_nf)
    pct_any <- weighted_pct_binary(d_nf$informal_any, if (!is.null(wvar)) d_nf[[wvar]] else NULL)
    
    d_inf <- d_nf %>% filter(informal_any == 1, !is.na(informal_type))
    n_inf <- nrow(d_inf)
    dist <- weighted_dist_1way(
      d_inf,
      "informal_type",
      c("Adult education", "Tsangaya", "Quranic"),
      wvar
    )
    
    tibble(
      State = label,
      `Percentage of women who attended informal schooling` = format_estimate_display(pct_any, n_nf, DISPLAY_DIGITS),
      `Number of women with no formal education` = format_count_display(n_nf),
      `Adult education` = format_estimate_display(dist["Adult education"], n_inf, DISPLAY_DIGITS),
      `Tsangaya` = format_estimate_display(dist["Tsangaya"], n_inf, DISPLAY_DIGITS),
      `Quranic` = format_estimate_display(dist["Quranic"], n_inf, DISPLAY_DIGITS),
      Total = format_estimate_display(ifelse(n_inf > 0, 100, NA_real_), n_inf, DISPLAY_DIGITS),
      `Number of women who attended informal schooling` = format_count_display(n_inf),
      base_status = case_when(
        get_base_status(n_nf) == "suppress" | get_base_status(n_inf) == "suppress" ~ "suppress",
        get_base_status(n_nf) == "flag" | get_base_status(n_inf) == "flag" ~ "flag",
        TRUE ~ "ok"
      )
    )
  }
  
  body <- women_state_zone_blocks(d, make_row, table_2_13_template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("informal_edu_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("state_sample_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "2.13_custom", body = body %>% select(-base_status), footnote = footnote, stub_header = "State")
}
# ---- Table 2.15 ---------------------------------------------------------------
build_table_2_15 <- function(dat) {
  need_cols(c("u_women_15_49", "bg_tv_frequency", "derived_age_group_w", "derived_residence",
              "derived_zone", "derived_wealth_quintile", "derived_edu_cat"), dat)
  wvar <- get_wvar(dat)
  
  d <- get_women_frame(dat) %>%
    mutate(
      tv_weekly = case_when(
        to_num(bg_tv_frequency) == 1 ~ 1,
        to_num(bg_tv_frequency) %in% c(2, 3) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  make_row <- function(dd, label) summ_binary_row(dd, label, "tv_weekly", wvar)
  
  body <- bind_rows(
    tibble(Characteristic = "Age", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(unname(AGE_W_LAB), ~ make_row(d %>% filter(age_grp == .x), .x)),
    tibble(Characteristic = "Residence", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(c("Urban", "Rural"), ~ make_row(d %>% filter(residence == .x), .x)),
    tibble(Characteristic = "Zone", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(ZONE_LAB, ~ make_row(d %>% filter(zone == .x), .x)),
    tibble(Characteristic = "Wealth quintile", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(unname(WEALTH_LAB), ~ make_row(d %>% filter(wealth == .x), .x)),
    tibble(Characteristic = "Education", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(unname(EDU4_LAB), ~ make_row(d %>% filter(edu == .x), .x)),
    make_row(d, "Total")
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("tv_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "B", body = body %>% select(-base_status), footnote = footnote)
}

# ---- Table 3.1 ---------------------------------------------------------------
build_table_3_1 <- function(dat) {
  need_cols(c("u_household", "derived_hh_has_any_net", "hh_nets_num", "derived_access_itn",
              "derived_residence", "derived_zone", "demo_state_num", "derived_wealth_quintile"), dat)
  wvar <- get_wvar(dat)
  
  d <- get_household_frame(dat)
  
  make_row <- function(dd, label) {
    d_net <- dd %>% filter(!is.na(derived_hh_has_any_net))
    n1 <- nrow(d_net)
    p_any <- weighted_pct_binary(d_net$derived_hh_has_any_net, if (!is.null(wvar)) d_net[[wvar]] else NULL)
    
    d_mean <- dd %>% filter(!is.na(hh_nets_num))
    n2 <- nrow(d_mean)
    mn <- weighted_mean_num(d_mean$hh_nets_num, if (!is.null(wvar)) d_mean[[wvar]] else NULL)
    
    d_acc <- dd %>% filter(!is.na(derived_access_itn))
    n3 <- nrow(d_acc)
    p_acc <- weighted_pct_binary(d_acc$derived_access_itn, if (!is.null(wvar)) d_acc[[wvar]] else NULL)
    
    tibble(
      Characteristic = label,
      `Households with at least one mosquito net` = format_estimate_display(p_any, n1, DISPLAY_DIGITS),
      `Mean number of mosquito nets per household` = format_estimate_display(mn, n2, DISPLAY_DIGITS),
      `Households with at least one mosquito net per two usual household members` = format_estimate_display(p_acc, n3, DISPLAY_DIGITS),
      `Number of households` = format_count_display(nrow(dd)),
      base_status = case_when(
        get_base_status(n1) == "suppress" | get_base_status(n2) == "suppress" | get_base_status(n3) == "suppress" ~ "suppress",
        get_base_status(n1) == "flag" | get_base_status(n2) == "flag" | get_base_status(n3) == "flag" ~ "flag",
        TRUE ~ "ok"
      )
    )
  }
  
  body <- bind_rows(
    tibble(
      Characteristic = "Residence",
      `Households with at least one mosquito net` = "",
      `Mean number of mosquito nets per household` = "",
      `Households with at least one mosquito net per two usual household members` = "",
      `Number of households` = "",
      base_status = "ok"
    ),
    map_dfr(c("Urban", "Rural"), ~ make_row(d %>% filter(residence == .x), .x)),
    tibble(
      Characteristic = "Zone",
      `Households with at least one mosquito net` = "",
      `Mean number of mosquito nets per household` = "",
      `Households with at least one mosquito net per two usual household members` = "",
      `Number of households` = "",
      base_status = "ok"
    ),
    map_dfr(ZONE_LAB, ~ make_row(d %>% filter(zone == .x), .x)),
    tibble(
      Characteristic = "State",
      `Households with at least one mosquito net` = "",
      `Mean number of mosquito nets per household` = "",
      `Households with at least one mosquito net per two usual household members` = "",
      `Number of households` = "",
      base_status = "ok"
    ),
    map_dfr(na.omit(sort(unique(d$state_lbl))), ~ make_row(d %>% filter(state_lbl == .x), .x)),
    tibble(
      Characteristic = "Wealth quintile",
      `Households with at least one mosquito net` = "",
      `Mean number of mosquito nets per household` = "",
      `Households with at least one mosquito net per two usual household members` = "",
      `Number of households` = "",
      base_status = "ok"
    ),
    map_dfr(unname(WEALTH_LAB), ~ make_row(d %>% filter(wealth == .x), .x)),
    make_row(d, "Total")
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("net_proxy"),
    collapse_footnotes("itn_access_proxy"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Residence/zone/state/wealth",
      middle_header = "Mosquito-net ownership indicators",
      middle_cols = 3:5,
      right_headers = c("Number of households")
    )
  )
}

# ---- Table 3.2 ---------------------------------------------------------------
build_table_3_2 <- function(dat) {
  need_cols(c(
    "u_household", "derived_hh_has_any_net",
    "prev_net_obtained_how", "prev_net_obtained_where",
    "derived_residence", "derived_zone", "demo_state_num", "derived_wealth_quintile"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_household_frame(dat) %>%
    filter(derived_hh_has_any_net == 1) %>%
    mutate(
      how   = to_num(prev_net_obtained_how),
      where = to_num(prev_net_obtained_where),
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
  
  make_row <- function(dd, label) {
    d0 <- dd %>% filter(!is.na(source_cat))
    n0 <- nrow(d0)
    dist <- weighted_dist_1way(d0, "source_cat", source_levels, wvar)
    
    tibble(
      Characteristic = label,
      `Mass distribution campaign` = format_estimate_display(dist["Mass distribution campaign"], n0, DISPLAY_DIGITS),
      `ANC visit` = format_estimate_display(dist["ANC visit"], n0, DISPLAY_DIGITS),
      `Immunization visit` = format_estimate_display(dist["Immunization visit"], n0, DISPLAY_DIGITS),
      `Government health facility` = format_estimate_display(dist["Government health facility"], n0, DISPLAY_DIGITS),
      `Private health facility` = format_estimate_display(dist["Private health facility"], n0, DISPLAY_DIGITS),
      Pharmacy = format_estimate_display(dist["Pharmacy"], n0, DISPLAY_DIGITS),
      `Shop/market` = format_estimate_display(dist["Shop/market"], n0, DISPLAY_DIGITS),
      `Community health worker` = format_estimate_display(dist["Community health worker"], n0, DISPLAY_DIGITS),
      `Religious institution` = format_estimate_display(dist["Religious institution"], n0, DISPLAY_DIGITS),
      School = format_estimate_display(dist["School"], n0, DISPLAY_DIGITS),
      Other = format_estimate_display(dist["Other"], n0, DISPLAY_DIGITS),
      `Don't know` = format_estimate_display(dist["Don't know"], n0, DISPLAY_DIGITS),
      Total = format_estimate_display(ifelse(n0 > 0, 100, NA_real_), n0, DISPLAY_DIGITS),
      `Number of households with any mosquito net` = format_count_display(n0),
      base_status = get_base_status(n0)
    )
  }
  
  body <- bind_rows(
    tibble(
      Characteristic = "Residence",
      `Mass distribution campaign` = "", `ANC visit` = "", `Immunization visit` = "",
      `Government health facility` = "", `Private health facility` = "", Pharmacy = "",
      `Shop/market` = "", `Community health worker` = "", `Religious institution` = "",
      School = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(c("Urban", "Rural"), ~ make_row(d %>% filter(residence == .x), .x)),
    
    tibble(
      Characteristic = "Zone",
      `Mass distribution campaign` = "", `ANC visit` = "", `Immunization visit` = "",
      `Government health facility` = "", `Private health facility` = "", Pharmacy = "",
      `Shop/market` = "", `Community health worker` = "", `Religious institution` = "",
      School = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(ZONE_LAB, ~ make_row(d %>% filter(zone == .x), .x)),
    
    tibble(
      Characteristic = "State",
      `Mass distribution campaign` = "", `ANC visit` = "", `Immunization visit` = "",
      `Government health facility` = "", `Private health facility` = "", Pharmacy = "",
      `Shop/market` = "", `Community health worker` = "", `Religious institution` = "",
      School = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(na.omit(sort(unique(d$state_lbl))), ~ make_row(d %>% filter(state_lbl == .x), .x)),
    
    tibble(
      Characteristic = "Wealth quintile",
      `Mass distribution campaign` = "", `ANC visit` = "", `Immunization visit` = "",
      `Government health facility` = "", `Private health facility` = "", Pharmacy = "",
      `Shop/market` = "", `Community health worker` = "", `Religious institution` = "",
      School = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(unname(WEALTH_LAB), ~ make_row(d %>% filter(wealth == .x), .x)),
    make_row(d, "Total")
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    "The denominator is households reporting any mosquito net and is applied consistently across national and state tabulations.",
    collapse_footnotes("net_proxy"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Residence/zone/state/wealth",
      middle_header = "Source of mosquito net",
      middle_cols = 3:14,
      right_headers = c("Total", "Number of households with any mosquito net")
    )
  )
}

# ---- Table 3.8 ---------------------------------------------------------------
build_table_3_8 <- function(dat) {
  need_cols(c(
    "u_household", "derived_hh_has_any_net", "derived_net_use_any_last_night",
    "prev_net_not_used_reason", "derived_residence", "derived_zone",
    "demo_state_num", "derived_wealth_quintile"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_household_frame(dat) %>%
    filter(derived_hh_has_any_net == 1) %>%
    mutate(
      net_not_used = case_when(
        derived_net_use_any_last_night == 0 ~ 1,
        derived_net_use_any_last_night == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      main_reason = case_when(
        to_num(prev_net_not_used_reason) == 1 ~ "No mosquitoes",
        to_num(prev_net_not_used_reason) == 2 ~ "No malaria",
        to_num(prev_net_not_used_reason) == 3 ~ "Too hot",
        to_num(prev_net_not_used_reason) == 4 ~ "Don't like smell",
        to_num(prev_net_not_used_reason) == 5 ~ "Feel closed in",
        to_num(prev_net_not_used_reason) == 6 ~ "Net too old/torn",
        to_num(prev_net_not_used_reason) == 7 ~ "Net too dirty",
        to_num(prev_net_not_used_reason) == 8 ~ "Net not available last night (washing)",
        to_num(prev_net_not_used_reason) == 9 ~ "Usual users did not sleep here last night",
        to_num(prev_net_not_used_reason) == 10 ~ "Net not needed last night",
        to_num(prev_net_not_used_reason) == 11 ~ "Bed bugs",
        to_num(prev_net_not_used_reason) == 96 ~ "Other",
        to_num(prev_net_not_used_reason) == 98 ~ "Don't know",
        TRUE ~ NA_character_
      )
    )
  
  reason_levels <- c(
    "No mosquitoes",
    "No malaria",
    "Too hot",
    "Don't like smell",
    "Feel closed in",
    "Net too old/torn",
    "Net too dirty",
    "Net not available last night (washing)",
    "Usual users did not sleep here last night",
    "Net not needed last night",
    "Bed bugs",
    "Other",
    "Don't know"
  )
  
  make_row <- function(dd, label) {
    d0 <- dd %>% filter(!is.na(net_not_used))
    n0 <- nrow(d0)
    p_not_used <- weighted_pct_binary(d0$net_not_used, if (!is.null(wvar)) d0[[wvar]] else NULL)
    
    d1 <- dd %>% filter(net_not_used == 1, !is.na(main_reason))
    n1 <- nrow(d1)
    dist <- weighted_dist_1way(d1, "main_reason", reason_levels, wvar)
    
    tibble(
      Characteristic = label,
      `Mosquito net not used previous night` = format_estimate_display(p_not_used, n0, DISPLAY_DIGITS),
      `No mosquitoes` = format_estimate_display(dist["No mosquitoes"], n1, DISPLAY_DIGITS),
      `No malaria` = format_estimate_display(dist["No malaria"], n1, DISPLAY_DIGITS),
      `Too hot` = format_estimate_display(dist["Too hot"], n1, DISPLAY_DIGITS),
      `Don't like smell` = format_estimate_display(dist["Don't like smell"], n1, DISPLAY_DIGITS),
      `Feel closed in` = format_estimate_display(dist["Feel closed in"], n1, DISPLAY_DIGITS),
      `Net too old/torn` = format_estimate_display(dist["Net too old/torn"], n1, DISPLAY_DIGITS),
      `Net too dirty` = format_estimate_display(dist["Net too dirty"], n1, DISPLAY_DIGITS),
      `Net not available last night (washing)` = format_estimate_display(dist["Net not available last night (washing)"], n1, DISPLAY_DIGITS),
      `Usual users did not sleep here last night` = format_estimate_display(dist["Usual users did not sleep here last night"], n1, DISPLAY_DIGITS),
      `Net not needed last night` = format_estimate_display(dist["Net not needed last night"], n1, DISPLAY_DIGITS),
      `Bed bugs` = format_estimate_display(dist["Bed bugs"], n1, DISPLAY_DIGITS),
      Other = format_estimate_display(dist["Other"], n1, DISPLAY_DIGITS),
      `Don't know` = format_estimate_display(dist["Don't know"], n1, DISPLAY_DIGITS),
      Total = format_estimate_display(ifelse(n1 > 0, 100, NA_real_), n1, DISPLAY_DIGITS),
      `Number of households with any mosquito net` = format_count_display(n0),
      base_status = case_when(
        get_base_status(n0) == "suppress" | get_base_status(n1) == "suppress" ~ "suppress",
        get_base_status(n0) == "flag" | get_base_status(n1) == "flag" ~ "flag",
        TRUE ~ "ok"
      )
    )
  }
  
  body <- bind_rows(
    tibble(
      Characteristic = "Residence",
      `Mosquito net not used previous night` = "",
      `No mosquitoes` = "", `No malaria` = "", `Too hot` = "",
      `Don't like smell` = "", `Feel closed in` = "", `Net too old/torn` = "",
      `Net too dirty` = "", `Net not available last night (washing)` = "",
      `Usual users did not sleep here last night` = "", `Net not needed last night` = "",
      `Bed bugs` = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(c("Urban", "Rural"), ~ make_row(d %>% filter(residence == .x), .x)),
    
    tibble(
      Characteristic = "Zone",
      `Mosquito net not used previous night` = "",
      `No mosquitoes` = "", `No malaria` = "", `Too hot` = "",
      `Don't like smell` = "", `Feel closed in` = "", `Net too old/torn` = "",
      `Net too dirty` = "", `Net not available last night (washing)` = "",
      `Usual users did not sleep here last night` = "", `Net not needed last night` = "",
      `Bed bugs` = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(ZONE_LAB, ~ make_row(d %>% filter(zone == .x), .x)),
    
    tibble(
      Characteristic = "State",
      `Mosquito net not used previous night` = "",
      `No mosquitoes` = "", `No malaria` = "", `Too hot` = "",
      `Don't like smell` = "", `Feel closed in` = "", `Net too old/torn` = "",
      `Net too dirty` = "", `Net not available last night (washing)` = "",
      `Usual users did not sleep here last night` = "", `Net not needed last night` = "",
      `Bed bugs` = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(na.omit(sort(unique(d$state_lbl))), ~ make_row(d %>% filter(state_lbl == .x), .x)),
    
    tibble(
      Characteristic = "Wealth quintile",
      `Mosquito net not used previous night` = "",
      `No mosquitoes` = "", `No malaria` = "", `Too hot` = "",
      `Don't like smell` = "", `Feel closed in` = "", `Net too old/torn` = "",
      `Net too dirty` = "", `Net not available last night (washing)` = "",
      `Usual users did not sleep here last night` = "", `Net not needed last night` = "",
      `Bed bugs` = "", Other = "", `Don't know` = "", Total = "",
      `Number of households with any mosquito net` = "", base_status = "ok"
    ),
    map_dfr(unname(WEALTH_LAB), ~ make_row(d %>% filter(wealth == .x), .x)),
    make_row(d, "Total")
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("net_proxy"),
    "The main reason reflects the household-reported primary reason for mosquito-net non-use.",
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Residence/zone/state/wealth",
      middle_header = "Main reason mosquito net was not used",
      middle_cols = 3:16,
      right_headers = c("Total", "Number of households with any mosquito net")
    )
  )
}

# ------------------------------------------------------------------------------
# 6. Driver
# ------------------------------------------------------------------------------
run_phase2a <- function(dat, out_xlsx = OUT_XLSX) {
  wb <- wb_init(out_xlsx)
  
  specs <- list(
    list(
      sheet = "Table 2.13.1",
      title = "Table 2.13.1 Informal schooling attendance of interviewed women: National (Sproxil respondents)",
      build = function() build_table_2_13_1(dat)
    ),
    list(
      sheet = "Table 2.13.2",
      title = "Table 2.13.2 Informal schooling attendance of interviewed women: States",
      build = function() build_table_2_13_2(dat)
    ),    
    list(sheet = "Table 2.15", title = "Table 2.15 Exposure to television among women age 15-49", build = function() build_table_2_15(dat)),
    list(sheet = "Table 3.1",  title = "Table 3.1 Household possession of mosquito nets", build = function() build_table_3_1(dat)),
    list(sheet = "Table 3.2",  title = "Table 3.2 Source of mosquito nets", build = function() build_table_3_2(dat)),
    list(sheet = "Table 3.8",  title = "Table 3.8 Main reason mosquito net was not used", build = function() build_table_3_8(dat))
  )
  
  for (sp in specs) {
    message("Building ", sp$sheet, " ...")
    obj <- sp$build()
    
    if (obj$type == "B") {
      write_table_B(wb, sp$sheet, sp$title, obj$body, obj$footnote)
    } else if (obj$type == "H") {
      do.call(
        write_table_H,
        c(
          list(wb = wb, sheet = sp$sheet, title = sp$title, body = obj$body, footnote = obj$footnote),
          obj$writer_args
        )
      )
    } else if (obj$type == "2.13_custom") {
      write_table_2_13(
        wb = wb,
        sheet = sp$sheet,
        title = sp$title,
        body = obj$body,
        footnote = obj$footnote,
        stub_header = obj$stub_header
      )
    } else {
      stop("Unsupported writer type: ", obj$type)
    }
  }
  
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  message("✅ Phase 2A tables written to: ", out_xlsx)
}

# ------------------------------------------------------------------------------
# 7. Execute
# ------------------------------------------------------------------------------
run_phase2a(df, OUT_XLSX)