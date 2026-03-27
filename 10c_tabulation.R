# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  12_Sproxil_Master_Tabulation_Engine_Phase2.R
# AUTHOR:  Corona Management Systems
# DATE:    23 January 2026
# PURPOSE:
#   Generate the next block of Sproxil MIS-style descriptive tables using the
#   same engine design and low-base display convention approved for Phase 2A.
#
# INCLUDED IN THIS PHASE:
#   - Table 4.1
#   - Table 4.2
#   - Table 4.3
#   - Table 4.4
#   - Table 4.5
#   - Table 5.1
#   - Table 5.2
#   - Table 5.3
#   - Table 5.4
#   - Table 6.1
#
# DESIGN PRINCIPLES:
#   - One input dataset, one workbook, one locked production weight.
#   - Percentages use the final calibrated trimmed weight where applied.
#   - Count columns remain unweighted.
#   - Low-base display:
#       * estimate + "*" if unweighted denominator is 10-24
#       * "-" if unweighted denominator is <10
#   - Child tables use the selected-youngest-child proxy already accepted.
#   - Recent-birth tables use live birth in 2020-2025 proxy already accepted.
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
  weighted_counts = "Percentages are weighted using the final calibrated trimmed weight (w_calibrated_trim); count columns are unweighted.",
  subgroup_caution = "Weight diagnostics showed strongest performance for national and state estimation; subgroup distributions shown here should be interpreted as descriptive weighted tabulations.",
  state_sample_caution = "State-level estimates are weighted descriptive estimates for the survey sample and should be interpreted cautiously.",
  low_base_flag = "* Estimate based on 10-24 unweighted observations; interpret with caution.",
  low_base_suppress = "- Estimate suppressed because the unweighted denominator is less than 10.",
  women_only = "This table is restricted to interviewed women age 15-49.",
  child_proxy = "Child-health indicators are based on the selected-youngest-child proxy available in the Sproxil survey and therefore should not be interpreted as full under-5 roster estimates.",
  recent_birth_proxy = "Maternal care indicators use the available live-birth-in-2020-2025 proxy as the recent-birth denominator.",
  current_preg_note = "First-trimester estimates are calculated among women currently pregnant.",
  act_note = "ACT = artemisinin-based combination therapy.",
  awareness_note = "Awareness indicators are based on respondent self-report.",
  diagnosis_note = "Malaria diagnosis is based on respondent report of being told by a healthcare provider that the selected youngest child had malaria.",
  source_note = "Source categories reflect the Sproxil child-fever advice/treatment response options available in the dataset rather than the full DHS/MIS under-5 source roster.",
  other_anti_note = "The 'Other antimalarial' column is a cautious proxy based on the residual medicine category in the Sproxil child medicine responses and may include medicines not separately classified as standard antimalarial groups.",
  multiple_response_note = "More than one response may have been reported; percentages across response categories may sum to more than 100.",
  itn_note = "ITN = insecticide-treated net.",
  noedu_note = "No education includes informal education (adult education, Tsangaya, or Quranic).",
  captured_only_note = "Only perception or attitude items explicitly captured in the Sproxil questionnaire are shown; unsupported composite indicators have been omitted.",
  stockout_free_note = "Stockout and free-treatment indicators are based on the applicable government-facility-visit universe available in the Sproxil questionnaire."
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

format_estimate_display <- function(value, n, digits = 1) {
  status <- get_base_status(n)
  
  if (status == "suppress") return("-")
  if (is.na(value)) return("")
  
  v <- formatC(round(value, digits), format = "f", digits = digits)
  
  if (status == "flag") return(paste0(v, "*"))
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

summ_binary_row <- function(dat, label, x_var, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[x_var]]), .data[[x_var]] %in% c(0, 1))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
  n_unw <- nrow(d)
  pct <- weighted_pct_binary(d[[x_var]], if (!is.null(wvar)) d[[wvar]] else NULL)
  
  tibble(
    Characteristic = label,
    Percent = format_estimate_display(pct, n_unw, DISPLAY_DIGITS),
    `Unweighted number` = format_count_display(n_unw),
    base_status = get_base_status(n_unw)
  )
}

# ------------------------------------------------------------------------------
# 3b. Child-table helper functions
# ------------------------------------------------------------------------------
get_child_frame <- function(dat) {
  get_women_frame(dat) %>%
    filter(u_child_youngest == 1)
}

child_bg_blocks_restricted <- function(d, maker_fun, template_row_fun) {
  bind_rows(
    template_row_fun("Residence"),
    map_dfr(c("Urban", "Rural"), ~ maker_fun(d %>% filter(residence == .x), .x)),
    template_row_fun("Zone"),
    map_dfr(ZONE_LAB, ~ maker_fun(d %>% filter(zone == .x), .x)),
    template_row_fun("Mother's education"),
    map_dfr(unname(EDU4_LAB), ~ maker_fun(d %>% filter(edu == .x), .x)),
    template_row_fun("Wealth quintile"),
    map_dfr(unname(WEALTH_LAB), ~ maker_fun(d %>% filter(wealth == .x), .x)),
    maker_fun(d, "Total")
  )
}

valid_w_subset <- function(d, wvar = NULL) {
  if (is.null(wvar)) return(d)
  d %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
}

combine_base_status <- function(...) {
  vals <- c(...)
  vals <- vals[!is.na(vals)]
  case_when(
    "suppress" %in% vals ~ "suppress",
    "flag" %in% vals ~ "flag",
    TRUE ~ "ok"
  )
}

table_4_1_template_row <- function(label) {
  tibble(
    Characteristic = label,
    `Percentage with fever in the 2 weeks preceding the survey` = "",
    `Number of children` = "",
    `Percentage for whom advice or treatment was sought` = "",
    `Percentage for whom advice or treatment was sought the same day or next day` = "",
    `Percentage who had blood taken for testing` = "",
    `Percentage who were diagnosed with malaria by a healthcare provider` = "",
    `Number of children with fever` = "",
    base_status = "ok"
  )
}

table_4_2_template_row <- function(label) {
  tibble(
    Characteristic = label,
    `Percentage who were referred to a higher level of care` = "",
    `Number of children with fever for whom advice or treatment was sought` = "",
    base_status = "ok"
  )
}

table_4_4_template_row <- function(label) {
  tibble(
    Characteristic = label,
    `Any ACT` = "",
    `SP/Fansidar` = "",
    `Chloroquine` = "",
    `Amodiaquine` = "",
    `Quinine pills` = "",
    `Artesunate rectal` = "",
    `Injection` = "",
    `Other antimalarial` = "",
    `Number of children with fever who took any antimalarial drug` = "",
    base_status = "ok"
  )
}

table_4_5_template_row <- function(label) {
  tibble(
    Characteristic = label,
    `Percentage of children whose fever went away after they received ACT` = "",
    `Number of children with fever who received ACT` = "",
    base_status = "ok"
  )
}

style_bank <- function() {
  list(
    title  = createStyle(textDecoration = "bold", fontSize = 12),
    header = createStyle(
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      wrapText = TRUE,
      border = "TopBottomLeftRight"
    ),
    stub   = createStyle(wrapText = TRUE),
    note   = createStyle(textDecoration = "italic", fontSize = 9),
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
  for (j in seq_along(x2)) x2[[j]][is.na(x2[[j]])] <- ""
  x2
}

# ------------------------------------------------------------------------------
# 3c. Attitude-table helper functions
# ------------------------------------------------------------------------------

get_living_child_u5_lab <- function(x) {
  case_when(
    x == 1 ~ "One or more",
    x == 0 ~ "None",
    TRUE ~ NA_character_
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
      edu = EDU4_LAB[as.character(to_num(derived_edu_cat))],
      living_child_u5 = get_living_child_u5_lab(u_hh_has_u5)
    )
}

women_bg_blocks_national <- function(d, maker_fun, template_row_fun = NULL,
                                     include_age = TRUE,
                                     include_living_u5 = FALSE,
                                     include_residence = TRUE,
                                     include_zone = TRUE,
                                     include_education = TRUE,
                                     include_wealth = TRUE) {
  out <- list()
  
  if (include_age) {
    out <- append(out, list(
      if (is.null(template_row_fun)) tibble(Characteristic = "Age") else template_row_fun("Age"),
      map_dfr(unname(AGE_W_LAB), ~ maker_fun(d %>% filter(age_grp == .x), .x))
    ))
  }
  
  if (include_living_u5) {
    out <- append(out, list(
      if (is.null(template_row_fun)) tibble(Characteristic = "Living children under age 5") else template_row_fun("Living children under age 5"),
      map_dfr(c("One or more", "None"), ~ maker_fun(d %>% filter(living_child_u5 == .x), .x))
    ))
  }
  
  if (include_residence) {
    out <- append(out, list(
      if (is.null(template_row_fun)) tibble(Characteristic = "Residence") else template_row_fun("Residence"),
      map_dfr(c("Urban", "Rural"), ~ maker_fun(d %>% filter(residence == .x), .x))
    ))
  }
  
  if (include_zone) {
    out <- append(out, list(
      if (is.null(template_row_fun)) tibble(Characteristic = "Zone") else template_row_fun("Zone"),
      map_dfr(ZONE_LAB, ~ maker_fun(d %>% filter(zone == .x), .x))
    ))
  }
  
  if (include_education) {
    out <- append(out, list(
      if (is.null(template_row_fun)) tibble(Characteristic = "Education") else template_row_fun("Education"),
      map_dfr(unname(EDU4_LAB), ~ maker_fun(d %>% filter(edu == .x), .x))
    ))
  }
  
  if (include_wealth) {
    out <- append(out, list(
      if (is.null(template_row_fun)) tibble(Characteristic = "Wealth quintile") else template_row_fun("Wealth quintile"),
      map_dfr(unname(WEALTH_LAB), ~ maker_fun(d %>% filter(wealth == .x), .x))
    ))
  }
  
  bind_rows(!!!out, maker_fun(d, "Total"))
}

women_state_zone_blocks <- function(d, maker_fun, template_row_fun = NULL) {
  out <- list()
  
  for (z in ZONE_LAB) {
    dz <- d %>% filter(zone == z)
    st <- dz %>% filter(!is.na(state_lbl)) %>% distinct(state_lbl) %>% arrange(state_lbl) %>% pull(state_lbl)
    
    if (length(st) == 0) next
    
    out <- append(out, list(
      if (is.null(template_row_fun)) tibble(State = z) else template_row_fun(z),
      map_dfr(st, ~ maker_fun(dz %>% filter(state_lbl == .x), .x))
    ))
  }
  
  bind_rows(!!!out, maker_fun(d, "Total"))
}

summ_multi_binary <- function(dd, vars, wvar = NULL) {
  lapply(vars, function(v) {
    d0 <- dd %>% filter(!is.na(.data[[v]]), .data[[v]] %in% c(0, 1))
    if (!is.null(wvar)) d0 <- d0 %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n0 <- nrow(d0)
    p0 <- weighted_pct_binary(d0[[v]], if (!is.null(wvar)) d0[[wvar]] else NULL)
    list(n = n0, p = p0, stat = get_base_status(n0))
  })
}

write_table_state_grouped <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 6
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = TRUE)
  addStyle(wb, sheet, s$header, rows = start_row, cols = 2:(ncol(body) + 1), gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet, s$stub, rows = (start_row + 1):(start_row + nrow(body)), cols = 2, gridExpand = TRUE, stack = TRUE)
  
  int_cols <- which(grepl("number|Number", names(body), ignore.case = TRUE)) + 1
  if (length(int_cols) > 0) {
    addStyle(wb, sheet, s$int, rows = (start_row + 1):(start_row + nrow(body)), cols = int_cols, gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 24)
  setColWidths(wb, sheet, cols = 3:(ncol(body) + 1), widths = 14)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
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
  if (length(int_cols) > 0) {
    addStyle(wb, sheet, s$int, rows = (start_row+1):(start_row+nrow(body)), cols = int_cols, gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 36)
  if (ncol(body) > 1) setColWidths(wb, sheet, cols = 3:(ncol(body)+1), widths = 16)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_H <- function(wb, sheet, title, body, footnote = NULL,
                          left_header = "Background characteristic",
                          middle_header = "Indicators",
                          middle_cols = 3:6,
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
  
  mid_names <- names(body)[(min(middle_cols)-1):(max(middle_cols)-1)]
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
  
  int_cols <- which(grepl("number|Number", names(body), ignore.case = TRUE)) + 1
  if (length(int_cols) > 0) {
    addStyle(wb, sheet, s$int, rows = start_row:(start_row + nrow(body) - 1), cols = int_cols, gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 30)
  setColWidths(wb, sheet, cols = 3:(ncol(body)+1), widths = 14)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

write_table_4_1_custom <- function(wb, sheet, title, body, footnote = NULL) {
  s <- style_bank()
  wb_replace_sheet(wb, sheet)
  
  start_row <- 8
  writeData(wb, sheet, title, startRow = 2, startCol = 2, colNames = FALSE)
  addStyle(wb, sheet, s$title, rows = 2, cols = 2, stack = TRUE)
  
  mergeCells(wb, sheet, cols = 2, rows = 6:7)
  writeData(wb, sheet, "Background characteristic", startRow = 6, startCol = 2, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 3:4, rows = 6)
  writeData(wb, sheet, "Youngest child under age 5", startRow = 6, startCol = 3, colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 5:9, rows = 6)
  writeData(wb, sheet, "Youngest child under age 5 with fever", startRow = 6, startCol = 5, colNames = FALSE)
  
  hdrs <- names(body)[-1]
  writeData(wb, sheet, hdrs, startRow = 7, startCol = 3, colNames = FALSE)
  
  addStyle(wb, sheet, s$header, rows = 6:7, cols = 2:(ncol(body) + 1), gridExpand = TRUE, stack = TRUE)
  
  body_out <- blank_na_for_excel(body)
  writeData(wb, sheet, body_out, startRow = start_row, startCol = 2, colNames = FALSE)
  
  addStyle(wb, sheet, s$stub, rows = start_row:(start_row + nrow(body) - 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  
  int_cols <- c(4, 9)
  addStyle(wb, sheet, s$int, rows = start_row:(start_row + nrow(body) - 1), cols = int_cols, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 2, widths = 30)
  setColWidths(wb, sheet, cols = 3:(ncol(body) + 1), widths = 16)
  
  if (!is.null(footnote) && nzchar(footnote)) {
    writeData(wb, sheet, footnote, startRow = start_row + nrow(body) + 2, startCol = 2, colNames = FALSE)
    addStyle(wb, sheet, s$note, rows = start_row + nrow(body) + 2, cols = 2, stack = TRUE)
  }
}

# ------------------------------------------------------------------------------
# 5. Common row builders for women tables
# ------------------------------------------------------------------------------
women_bg_blocks_simple <- function(d, maker_fun) {
  bind_rows(
    tibble(Characteristic = "Age", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(unname(AGE_W_LAB), ~ maker_fun(d %>% filter(age_grp == .x), .x)),
    tibble(Characteristic = "Residence", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(c("Urban", "Rural"), ~ maker_fun(d %>% filter(residence == .x), .x)),
    tibble(Characteristic = "Zone", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(ZONE_LAB, ~ maker_fun(d %>% filter(zone == .x), .x)),
    tibble(Characteristic = "State", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(na.omit(sort(unique(d$state_lbl))), ~ maker_fun(d %>% filter(state_lbl == .x), .x)),
    tibble(Characteristic = "Wealth quintile", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(unname(WEALTH_LAB), ~ maker_fun(d %>% filter(wealth == .x), .x)),
    tibble(Characteristic = "Education", Percent = "", `Unweighted number` = "", base_status = "ok"),
    map_dfr(unname(EDU4_LAB), ~ maker_fun(d %>% filter(edu == .x), .x)),
    maker_fun(d, "Total")
  )
}

# ------------------------------------------------------------------------------
# 6. Builder functions
# ------------------------------------------------------------------------------

# ---- Table 4.1 ---------------------------------------------------------------
build_table_4_1 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_child_youngest", "derived_child_fever",
    "derived_fever_seek_advice", "derived_fever_prompt_care", "derived_fever_tested",
    "women_child_malaria_diagnosis",
    "derived_residence", "derived_zone", "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_child_frame(dat)
  
  make_row <- function(dd, label) {
    d_child <- dd %>%
      filter(!is.na(derived_child_fever), derived_child_fever %in% c(0, 1)) %>%
      valid_w_subset(wvar)
    
    n_child <- nrow(d_child)
    p_fever <- weighted_pct_binary(d_child$derived_child_fever, if (!is.null(wvar)) d_child[[wvar]] else NULL)
    
    d_fever <- dd %>%
      filter(derived_child_fever == 1) %>%
      valid_w_subset(wvar)
    
    n_fever <- nrow(d_fever)
    
    d_seek <- d_fever %>%
      filter(!is.na(derived_fever_seek_advice), derived_fever_seek_advice %in% c(0, 1))
    p_seek <- weighted_pct_binary(d_seek$derived_fever_seek_advice, if (!is.null(wvar)) d_seek[[wvar]] else NULL)
    n_seek <- nrow(d_seek)
    
    d_prompt <- d_fever %>%
      filter(!is.na(derived_fever_prompt_care), derived_fever_prompt_care %in% c(0, 1))
    p_prompt <- weighted_pct_binary(d_prompt$derived_fever_prompt_care, if (!is.null(wvar)) d_prompt[[wvar]] else NULL)
    n_prompt <- nrow(d_prompt)
    
    d_test <- d_fever %>%
      filter(!is.na(derived_fever_tested), derived_fever_tested %in% c(0, 1))
    p_test <- weighted_pct_binary(d_test$derived_fever_tested, if (!is.null(wvar)) d_test[[wvar]] else NULL)
    n_test <- nrow(d_test)
    
    d_diag <- d_fever %>%
      filter(!is.na(women_child_malaria_diagnosis), women_child_malaria_diagnosis %in% c(0, 1))
    p_diag <- weighted_pct_binary(d_diag$women_child_malaria_diagnosis, if (!is.null(wvar)) d_diag[[wvar]] else NULL)
    n_diag <- nrow(d_diag)
    
    tibble(
      Characteristic = label,
      `Percentage with fever in the 2 weeks preceding the survey` = format_estimate_display(p_fever, n_child, DISPLAY_DIGITS),
      `Number of children` = format_count_display(n_child),
      `Percentage for whom advice or treatment was sought` = format_estimate_display(p_seek, n_fever, DISPLAY_DIGITS),
      `Percentage for whom advice or treatment was sought the same day or next day` = format_estimate_display(p_prompt, n_prompt, DISPLAY_DIGITS),
      `Percentage who had blood taken for testing` = format_estimate_display(p_test, n_test, DISPLAY_DIGITS),
      `Percentage who were diagnosed with malaria by a healthcare provider` = format_estimate_display(p_diag, n_diag, DISPLAY_DIGITS),
      `Number of children with fever` = format_count_display(n_fever),
      base_status = combine_base_status(
        get_base_status(n_child),
        get_base_status(n_fever),
        get_base_status(n_seek),
        get_base_status(n_prompt),
        get_base_status(n_test),
        get_base_status(n_diag)
      )
    )
  }
  
  body <- child_bg_blocks_restricted(d, make_row, table_4_1_template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("child_proxy"),
    collapse_footnotes("diagnosis_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "4.1_custom", body = body %>% select(-base_status), footnote = footnote)
}

# ---- Table 4.2 ---------------------------------------------------------------
build_table_4_2 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_child_youngest", "derived_child_fever", "derived_fever_seek_advice",
    "women_child_referral",
    "derived_residence", "derived_zone", "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_child_frame(dat)
  
  make_row <- function(dd, label) {
    d_base <- dd %>%
      filter(derived_child_fever == 1, derived_fever_seek_advice == 1) %>%
      filter(!is.na(women_child_referral), women_child_referral %in% c(0, 1)) %>%
      valid_w_subset(wvar)
    
    n0 <- nrow(d_base)
    p0 <- weighted_pct_binary(d_base$women_child_referral, if (!is.null(wvar)) d_base[[wvar]] else NULL)
    
    tibble(
      Characteristic = label,
      `Percentage who were referred to a higher level of care` = format_estimate_display(p0, n0, DISPLAY_DIGITS),
      `Number of children with fever for whom advice or treatment was sought` = format_count_display(n0),
      base_status = get_base_status(n0)
    )
  }
  
  body <- child_bg_blocks_restricted(d, make_row, table_4_2_template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("child_proxy"),
    "The denominator is children whose selected youngest child had fever in the two weeks preceding the survey and for whom advice or treatment was sought.",
    "Includes advice or treatment from the following sources: public sector, private medical sector, NGO medical sector, chemist shop/patent and proprietary medicine vendor (PPMV), market, and itinerant drug seller. Excludes advice or treatment from a traditional practitioner.",
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "B", body = body %>% select(-base_status), footnote = footnote)
}

# ---- Table 4.3 ---------------------------------------------------------------
build_table_4_3 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_child_youngest", "derived_child_fever", "derived_fever_seek_advice",
    "child_adv_gov", "child_adv_pvth", "child_adv_ngo", "child_adv_mob", "child_adv_pvtd",
    "child_adv_com", "child_adv_pharm", "child_adv_chem", "child_adv_trad", "child_adv_rel",
    "child_adv_other"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_child_frame(dat)
  
  make_source_indicator_all_fever <- function(dd, src_var) {
    case_when(
      dd$derived_child_fever != 1 ~ NA_real_,
      dd$derived_fever_seek_advice == 0 ~ 0,
      dd$derived_fever_seek_advice == 1 & dd[[src_var]] %in% c(0, 1) ~ as.numeric(dd[[src_var]]),
      TRUE ~ NA_real_
    )
  }
  
  make_source_indicator_seekers <- function(dd, src_var) {
    case_when(
      dd$derived_child_fever != 1 ~ NA_real_,
      dd$derived_fever_seek_advice != 1 ~ NA_real_,
      dd[[src_var]] %in% c(0, 1) ~ as.numeric(dd[[src_var]]),
      TRUE ~ NA_real_
    )
  }
  
  source_specs <- tribble(
    ~Source, ~var,
    "Public sector", NA_character_,
    "Government facility", "child_adv_gov",
    "", NA_character_,
    "Private medical sector", NA_character_,
    "Private hospital/clinic", "child_adv_pvth",
    "Private doctor", "child_adv_pvtd",
    "Mobile clinic", "child_adv_mob",
    "Community nurse/health worker", "child_adv_com",
    "", NA_character_,
    "Private medical sector (NGO)", NA_character_,
    "NGO hospital/clinic", "child_adv_ngo",
    "", NA_character_,
    "Other private sector", NA_character_,
    "Pharmacy", "child_adv_pharm",
    "Chemist/local drug store", "child_adv_chem",
    "Traditional practitioner", "child_adv_trad",
    "Religious healer", "child_adv_rel",
    "Other", "child_adv_other"
  )
  
  rows <- pmap_dfr(source_specs, function(Source, var) {
    if (is.na(var)) {
      return(tibble(
        Source = Source,
        `Among children with fever` = "",
        `Among children with fever for whom advice or treatment was sought` = "",
        base_status = "ok"
      ))
    }
    
    x1 <- make_source_indicator_all_fever(d, var)
    d1 <- d %>%
      mutate(.x = x1) %>%
      filter(!is.na(.x), .x %in% c(0, 1)) %>%
      valid_w_subset(wvar)
    n1 <- nrow(d1)
    p1 <- weighted_pct_binary(d1$.x, if (!is.null(wvar)) d1[[wvar]] else NULL)
    
    x2 <- make_source_indicator_seekers(d, var)
    d2 <- d %>%
      mutate(.x = x2) %>%
      filter(!is.na(.x), .x %in% c(0, 1)) %>%
      valid_w_subset(wvar)
    n2 <- nrow(d2)
    p2 <- weighted_pct_binary(d2$.x, if (!is.null(wvar)) d2[[wvar]] else NULL)
    
    tibble(
      Source = Source,
      `Among children with fever` = format_estimate_display(p1, n1, DISPLAY_DIGITS),
      `Among children with fever for whom advice or treatment was sought` = format_estimate_display(p2, n2, DISPLAY_DIGITS),
      base_status = combine_base_status(get_base_status(n1), get_base_status(n2))
    )
  })
  
  n_fever <- d %>%
    filter(derived_child_fever == 1) %>%
    nrow()
  
  n_seek <- d %>%
    filter(derived_child_fever == 1, derived_fever_seek_advice == 1) %>%
    nrow()
  
  body <- bind_rows(
    rows,
    tibble(
      Source = "Number of children",
      `Among children with fever` = format_count_display(n_fever),
      `Among children with fever for whom advice or treatment was sought` = format_count_display(n_seek),
      base_status = combine_base_status(get_base_status(n_fever), get_base_status(n_seek))
    )
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("child_proxy"),
    collapse_footnotes("source_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "B", body = body %>% select(-base_status), footnote = footnote)
}

# ---- Table 4.4 ---------------------------------------------------------------
build_table_4_4 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_child_youngest", "derived_child_fever", "derived_fever_took_anti",
    "derived_fever_took_act", "derived_fever_took_sp", "derived_fever_took_chloro",
    "derived_fever_took_amod", "derived_fever_took_quin", "derived_fever_took_artes",
    "med_inject", "med_other",
    "derived_residence", "derived_zone", "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_child_frame(dat)
  
  make_row <- function(dd, label) {
    d_base <- dd %>%
      filter(derived_child_fever == 1, derived_fever_took_anti == 1) %>%
      valid_w_subset(wvar)
    
    calc_pct <- function(v) {
      d0 <- d_base %>% filter(!is.na(.data[[v]]), .data[[v]] %in% c(0, 1))
      list(
        n = nrow(d0),
        p = weighted_pct_binary(d0[[v]], if (!is.null(wvar)) d0[[wvar]] else NULL),
        stat = get_base_status(nrow(d0))
      )
    }
    
    out_act    <- calc_pct("derived_fever_took_act")
    out_sp     <- calc_pct("derived_fever_took_sp")
    out_chloro <- calc_pct("derived_fever_took_chloro")
    out_amod   <- calc_pct("derived_fever_took_amod")
    out_quin   <- calc_pct("derived_fever_took_quin")
    out_artes  <- calc_pct("derived_fever_took_artes")
    out_inj    <- calc_pct("med_inject")
    out_other  <- calc_pct("med_other")
    
    tibble(
      Characteristic = label,
      `Any ACT` = format_estimate_display(out_act$p, out_act$n, DISPLAY_DIGITS),
      `SP/Fansidar` = format_estimate_display(out_sp$p, out_sp$n, DISPLAY_DIGITS),
      `Chloroquine` = format_estimate_display(out_chloro$p, out_chloro$n, DISPLAY_DIGITS),
      `Amodiaquine` = format_estimate_display(out_amod$p, out_amod$n, DISPLAY_DIGITS),
      `Quinine pills` = format_estimate_display(out_quin$p, out_quin$n, DISPLAY_DIGITS),
      `Artesunate rectal` = format_estimate_display(out_artes$p, out_artes$n, DISPLAY_DIGITS),
      `Injection` = format_estimate_display(out_inj$p, out_inj$n, DISPLAY_DIGITS),
      `Other antimalarial` = format_estimate_display(out_other$p, out_other$n, DISPLAY_DIGITS),
      `Number of children with fever who took any antimalarial drug` = format_count_display(nrow(d_base)),
      base_status = combine_base_status(
        get_base_status(nrow(d_base)),
        out_act$stat, out_sp$stat, out_chloro$stat, out_amod$stat,
        out_quin$stat, out_artes$stat, out_inj$stat, out_other$stat
      )
    )
  }
  
  body <- child_bg_blocks_restricted(d, make_row, table_4_4_template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("child_proxy"),
    collapse_footnotes("act_note"),
    collapse_footnotes("other_anti_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Background characteristic",
      middle_header = "Percentage of children who took",
      middle_cols = 3:10,
      right_headers = c("Number of children with fever who took any antimalarial drug")
    )
  )
}

# ---- Table 4.5 ---------------------------------------------------------------
build_table_4_5 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_child_youngest", "derived_child_fever",
    "derived_fever_took_act", "derived_act_effective",
    "derived_residence", "derived_zone", "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_child_frame(dat)
  
  make_row <- function(dd, label) {
    d_base <- dd %>%
      filter(derived_child_fever == 1, derived_fever_took_act == 1) %>%
      filter(!is.na(derived_act_effective), derived_act_effective %in% c(0, 1)) %>%
      valid_w_subset(wvar)
    
    n0 <- nrow(d_base)
    p0 <- weighted_pct_binary(d_base$derived_act_effective, if (!is.null(wvar)) d_base[[wvar]] else NULL)
    
    tibble(
      Characteristic = label,
      `Percentage of children whose fever went away after they received ACT` = format_estimate_display(p0, n0, DISPLAY_DIGITS),
      `Number of children with fever who received ACT` = format_count_display(n0),
      base_status = get_base_status(n0)
    )
  }
  
  body <- child_bg_blocks_restricted(d, make_row, table_4_5_template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("child_proxy"),
    collapse_footnotes("act_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Background characteristic",
      middle_header = "Indicators",
      middle_cols = 3:3,
      right_headers = c("Number of children with fever who received ACT")
    )
  )
}

# ---- Table 5.1 ---------------------------------------------------------------
build_table_5_1_1 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "bg_heard_malaria_msg_6months",
    "msg_radio", "msg_tv", "msg_poster", "msg_news", "msg_leaf", "msg_hcp",
    "msg_chw", "msg_social", "msg_town", "msg_ipc", "msg_family",
    "msg_other", "msg_dontremember",
    "derived_age_group_w", "derived_residence", "derived_zone",
    "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_women_frame(dat) %>%
    mutate(
      heard_msg = case_when(
        to_num(bg_heard_malaria_msg_6months) == 1 ~ 1,
        to_num(bg_heard_malaria_msg_6months) == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  template_row <- function(label) {
    tibble(
      Characteristic = label,
      `Percentage who have seen or heard a malaria message in the past 6 months` = "",
      `Number of women` = "",
      Radio = "", Television = "", `Poster/Billboard` = "", `Newspaper/magazine` = "",
      `Leaflet/brochure` = "", `Health care provider` = "", `Community health worker` = "",
      `Social media` = "", `Town announcer` = "",
      `Inter-personal communication agent/community volunteer` = "",
      `Family/Friends` = "", Other = "", `Don't remember` = "",
      `Number of women who have seen or heard a message` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    d_all <- dd %>% filter(!is.na(heard_msg), heard_msg %in% c(0, 1))
    if (!is.null(wvar)) d_all <- d_all %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_all <- nrow(d_all)
    p_heard <- weighted_pct_binary(d_all$heard_msg, if (!is.null(wvar)) d_all[[wvar]] else NULL)
    
    d_heard <- dd %>% filter(heard_msg == 1)
    if (!is.null(wvar)) d_heard <- d_heard %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_heard <- nrow(d_heard)
    
    src_vars <- c("msg_radio","msg_tv","msg_poster","msg_news","msg_leaf","msg_hcp",
                  "msg_chw","msg_social","msg_town","msg_ipc","msg_family",
                  "msg_other","msg_dontremember")
    
    out <- summ_multi_binary(d_heard, src_vars, wvar)
    
    tibble(
      Characteristic = label,
      `Percentage who have seen or heard a malaria message in the past 6 months` = format_estimate_display(p_heard, n_all, DISPLAY_DIGITS),
      `Number of women` = format_count_display(n_all),
      Radio = format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      Television = format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Poster/Billboard` = format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Newspaper/magazine` = format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Leaflet/brochure` = format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Health care provider` = format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Community health worker` = format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      `Social media` = format_estimate_display(out[[8]]$p, out[[8]]$n, DISPLAY_DIGITS),
      `Town announcer` = format_estimate_display(out[[9]]$p, out[[9]]$n, DISPLAY_DIGITS),
      `Inter-personal communication agent/community volunteer` = format_estimate_display(out[[10]]$p, out[[10]]$n, DISPLAY_DIGITS),
      `Family/Friends` = format_estimate_display(out[[11]]$p, out[[11]]$n, DISPLAY_DIGITS),
      Other = format_estimate_display(out[[12]]$p, out[[12]]$n, DISPLAY_DIGITS),
      `Don't remember` = format_estimate_display(out[[13]]$p, out[[13]]$n, DISPLAY_DIGITS),
      `Number of women who have seen or heard a message` = format_count_display(n_heard),
      base_status = combine_base_status(get_base_status(n_all), get_base_status(n_heard))
    )
  }
  
  body <- women_bg_blocks_national(
    d, make_row, template_row_fun = template_row,
    include_age = TRUE, include_living_u5 = FALSE,
    include_residence = TRUE, include_zone = TRUE,
    include_education = TRUE, include_wealth = TRUE
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("multiple_response_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "H", body = body %>% select(-base_status), footnote = footnote,
       writer_args = list(
         left_header = "Background characteristic",
         middle_header = "Source of exposure to malaria messages in the past 6 months",
         middle_cols = 5:17,
         right_headers = c("Number of women who have seen or heard a message")
       ))
}

build_table_5_1_2 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "bg_heard_malaria_msg_6months",
    "msg_radio", "msg_tv", "msg_poster", "msg_news", "msg_leaf", "msg_hcp",
    "msg_chw", "msg_social", "msg_town", "msg_ipc", "msg_family",
    "msg_other", "msg_dontremember",
    "derived_age_group_w", "derived_residence", "derived_zone",
    "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_women_frame(dat) %>%
    mutate(
      heard_msg = case_when(
        to_num(bg_heard_malaria_msg_6months) == 1 ~ 1,
        to_num(bg_heard_malaria_msg_6months) == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  template_row <- function(label) {
    tibble(
      State = label,
      `Percentage who have seen or heard a malaria message in the past 6 months` = "",
      `Number of women` = "",
      Radio = "", Television = "", `Poster/Billboard` = "", `Newspaper/magazine` = "",
      `Leaflet/brochure` = "", `Health care provider` = "", `Community health worker` = "",
      `Social media` = "", `Town announcer` = "",
      `Inter-personal communication agent/community volunteer` = "",
      `Family/Friends` = "", Other = "", `Don't remember` = "",
      `Number of women who have seen or heard a message` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    d_all <- dd %>% filter(!is.na(heard_msg), heard_msg %in% c(0, 1))
    if (!is.null(wvar)) d_all <- d_all %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_all <- nrow(d_all)
    p_heard <- weighted_pct_binary(d_all$heard_msg, if (!is.null(wvar)) d_all[[wvar]] else NULL)
    
    d_heard <- dd %>% filter(heard_msg == 1)
    if (!is.null(wvar)) d_heard <- d_heard %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_heard <- nrow(d_heard)
    
    src_vars <- c("msg_radio","msg_tv","msg_poster","msg_news","msg_leaf","msg_hcp",
                  "msg_chw","msg_social","msg_town","msg_ipc","msg_family",
                  "msg_other","msg_dontremember")
    out <- summ_multi_binary(d_heard, src_vars, wvar)
    
    tibble(
      State = label,
      `Percentage who have seen or heard a malaria message in the past 6 months` = format_estimate_display(p_heard, n_all, DISPLAY_DIGITS),
      `Number of women` = format_count_display(n_all),
      Radio = format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      Television = format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Poster/Billboard` = format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Newspaper/magazine` = format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Leaflet/brochure` = format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Health care provider` = format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Community health worker` = format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      `Social media` = format_estimate_display(out[[8]]$p, out[[8]]$n, DISPLAY_DIGITS),
      `Town announcer` = format_estimate_display(out[[9]]$p, out[[9]]$n, DISPLAY_DIGITS),
      `Inter-personal communication agent/community volunteer` = format_estimate_display(out[[10]]$p, out[[10]]$n, DISPLAY_DIGITS),
      `Family/Friends` = format_estimate_display(out[[11]]$p, out[[11]]$n, DISPLAY_DIGITS),
      Other = format_estimate_display(out[[12]]$p, out[[12]]$n, DISPLAY_DIGITS),
      `Don't remember` = format_estimate_display(out[[13]]$p, out[[13]]$n, DISPLAY_DIGITS),
      `Number of women who have seen or heard a message` = format_count_display(n_heard),
      base_status = combine_base_status(get_base_status(n_all), get_base_status(n_heard))
    )
  }
  
  body <- women_state_zone_blocks(d, make_row, template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("multiple_response_note"),
    collapse_footnotes("state_sample_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "STATE", body = body %>% select(-base_status), footnote = footnote)
}


build_table_5_2_1 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "bg_aware_avoidance",
    "know_net", "know_repel", "know_meds", "know_spray",
    "know_stag", "know_clean", "know_screens", "know_other", "know_dont",
    "derived_age_group_w", "derived_residence", "derived_zone",
    "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_women_frame(dat) %>%
    mutate(
      aware_avoid = case_when(
        to_num(bg_aware_avoidance) == 1 ~ 1,
        to_num(bg_aware_avoidance) == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      know_other = to_num(know_other),
      know_dont  = to_num(know_dont)
    )
  
  template_row <- function(label) {
    tibble(
      Characteristic = label,
      `Percentage who state there are ways to avoid getting malaria` = "",
      `Number of women` = "",
      `Sleep under mosquito net or ITN` = "",
      `Use mosquito repellent` = "",
      `Take preventive medications` = "",
      `Spray house with insecticide` = "",
      `Fill in stagnant water (puddles)` = "",
      `Keep surroundings clean` = "",
      `Put mosquito screen on windows` = "",
      Other = "",
      `Don't know` = "",
      `Number of women who state there are ways to avoid getting malaria` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    d_all <- dd %>% filter(!is.na(aware_avoid), aware_avoid %in% c(0, 1))
    if (!is.null(wvar)) d_all <- d_all %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_all <- nrow(d_all)
    p_aware <- weighted_pct_binary(d_all$aware_avoid, if (!is.null(wvar)) d_all[[wvar]] else NULL)
    
    d_aware <- dd %>% filter(aware_avoid == 1)
    if (!is.null(wvar)) d_aware <- d_aware %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_aware <- nrow(d_aware)
    
    vars <- c(
      "know_net", "know_repel", "know_meds", "know_spray",
      "know_stag", "know_clean", "know_screens", "know_other", "know_dont"
    )
    out <- summ_multi_binary(d_aware, vars, wvar)
    
    tibble(
      Characteristic = label,
      `Percentage who state there are ways to avoid getting malaria` = format_estimate_display(p_aware, n_all, DISPLAY_DIGITS),
      `Number of women` = format_count_display(n_all),
      `Sleep under mosquito net or ITN` = format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Use mosquito repellent` = format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Take preventive medications` = format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Spray house with insecticide` = format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Fill in stagnant water (puddles)` = format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Keep surroundings clean` = format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Put mosquito screen on windows` = format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      Other = format_estimate_display(out[[8]]$p, out[[8]]$n, DISPLAY_DIGITS),
      `Don't know` = format_estimate_display(out[[9]]$p, out[[9]]$n, DISPLAY_DIGITS),
      `Number of women who state there are ways to avoid getting malaria` = format_count_display(n_aware),
      base_status = combine_base_status(get_base_status(n_all), get_base_status(n_aware))
    )
  }
  
  body <- women_bg_blocks_national(
    d, make_row, template_row_fun = template_row,
    include_age = TRUE, include_living_u5 = FALSE,
    include_residence = TRUE, include_zone = TRUE,
    include_education = TRUE, include_wealth = TRUE
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("multiple_response_note"),
    collapse_footnotes("itn_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Background characteristic",
      middle_header = "Ways to avoid getting malaria",
      middle_cols = 5:13,
      right_headers = c("Number of women who state there are ways to avoid getting malaria")
    )
  )
}


build_table_5_2_2 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "bg_aware_avoidance",
    "know_net", "know_repel", "know_meds", "know_spray",
    "know_stag", "know_clean", "know_screens", "know_other", "know_dont",
    "derived_zone", "demo_state_num"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_women_frame(dat) %>%
    mutate(
      aware_avoid = case_when(
        to_num(bg_aware_avoidance) == 1 ~ 1,
        to_num(bg_aware_avoidance) == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      know_other = to_num(know_other),
      know_dont  = to_num(know_dont)
    )
  
  template_row <- function(label) {
    tibble(
      State = label,
      `Percentage who state there are ways to avoid getting malaria` = "",
      `Number of women` = "",
      `Sleep under mosquito net or ITN` = "",
      `Use mosquito repellent` = "",
      `Take preventive medications` = "",
      `Spray house with insecticide` = "",
      `Fill in stagnant water (puddles)` = "",
      `Keep surroundings clean` = "",
      `Put mosquito screen on windows` = "",
      Other = "",
      `Don't know` = "",
      `Number of women who state there are ways to avoid getting malaria` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    d_all <- dd %>% filter(!is.na(aware_avoid), aware_avoid %in% c(0, 1))
    if (!is.null(wvar)) d_all <- d_all %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_all <- nrow(d_all)
    p_aware <- weighted_pct_binary(d_all$aware_avoid, if (!is.null(wvar)) d_all[[wvar]] else NULL)
    
    d_aware <- dd %>% filter(aware_avoid == 1)
    if (!is.null(wvar)) d_aware <- d_aware %>% filter(!is.na(.data[[wvar]]), is.finite(.data[[wvar]]))
    n_aware <- nrow(d_aware)
    
    vars <- c(
      "know_net", "know_repel", "know_meds", "know_spray",
      "know_stag", "know_clean", "know_screens", "know_other", "know_dont"
    )
    out <- summ_multi_binary(d_aware, vars, wvar)
    
    tibble(
      State = label,
      `Percentage who state there are ways to avoid getting malaria` = format_estimate_display(p_aware, n_all, DISPLAY_DIGITS),
      `Number of women` = format_count_display(n_all),
      `Sleep under mosquito net or ITN` = format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Use mosquito repellent` = format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Take preventive medications` = format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Spray house with insecticide` = format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Fill in stagnant water (puddles)` = format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Keep surroundings clean` = format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Put mosquito screen on windows` = format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      Other = format_estimate_display(out[[8]]$p, out[[8]]$n, DISPLAY_DIGITS),
      `Don't know` = format_estimate_display(out[[9]]$p, out[[9]]$n, DISPLAY_DIGITS),
      `Number of women who state there are ways to avoid getting malaria` = format_count_display(n_aware),
      base_status = combine_base_status(get_base_status(n_all), get_base_status(n_aware))
    )
  }
  
  body <- women_state_zone_blocks(d, make_row, template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("multiple_response_note"),
    collapse_footnotes("itn_note"),
    collapse_footnotes("state_sample_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "STATE", body = body %>% select(-base_status), footnote = footnote)
}


build_table_5_3_1 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_hh_has_u5",
    "att_rainy_season_only", "att_fever_worry_malaria",
    "att_malaria_easily_treated", "att_weak_children_die",
    "att_net_use_mosquito_density", "att_net_use_warm_weather",
    "derived_age_group_w", "derived_residence", "derived_zone",
    "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_women_frame(dat) %>%
    mutate(
      rainy_disagree = ifelse(
        to_num(att_rainy_season_only) %in% c(1, 2),
        ifelse(to_num(att_rainy_season_only) == 2, 1, 0),
        NA_real_
      ),
      fever_worry_agree = ifelse(
        to_num(att_fever_worry_malaria) %in% c(1, 2),
        ifelse(to_num(att_fever_worry_malaria) == 1, 1, 0),
        NA_real_
      ),
      easy_treat_disagree = ifelse(
        to_num(att_malaria_easily_treated) %in% c(1, 2),
        ifelse(to_num(att_malaria_easily_treated) == 2, 1, 0),
        NA_real_
      ),
      weakchild_disagree = ifelse(
        to_num(att_weak_children_die) %in% c(1, 2),
        ifelse(to_num(att_weak_children_die) == 2, 1, 0),
        NA_real_
      ),
      sleep_net_lots = ifelse(
        to_num(att_net_use_mosquito_density) %in% c(1, 2),
        ifelse(to_num(att_net_use_mosquito_density) == 2, 1, 0),
        NA_real_
      ),
      sleep_net_warm = ifelse(
        to_num(att_net_use_warm_weather) %in% c(1, 2),
        ifelse(to_num(att_net_use_warm_weather) == 2, 1, 0),
        NA_real_
      ),
      susceptibility_any = case_when(
        rowSums(!is.na(cbind(rainy_disagree, fever_worry_agree))) == 0 ~ NA_real_,
        rowSums(cbind(rainy_disagree == 1, fever_worry_agree == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      ),
      selfeff_any = case_when(
        rowSums(!is.na(cbind(sleep_net_lots, sleep_net_warm))) == 0 ~ NA_real_,
        rowSums(cbind(sleep_net_lots == 1, sleep_net_warm == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      )
    )
  
  template_row <- function(label) {
    tibble(
      Characteristic = label,
      `Percentage who disagree that people in the community get malaria only during the rainy season` = "",
      `Percentage who agree that when a child has a fever, they almost always worry it might be malaria` = "",
      `Percentage who perceive that their families and communities are at risk from malaria` = "",
      `Percentage who disagree that getting malaria is not a problem because it can be easily treated` = "",
      `Percentage who disagree that only weak children can die from malaria` = "",
      `Percentage who agree that they can sleep under a mosquito net for the entire night when there are lots of mosquitoes` = "",
      `Percentage who agree that they can sleep under a mosquito net for the entire night when the weather is too warm` = "",
      `Percentage who are confident in their ability to perform specific malaria-related behaviours captured in Sproxil` = "",
      `Number of women` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    vars <- c(
      "rainy_disagree",
      "fever_worry_agree",
      "susceptibility_any",
      "easy_treat_disagree",
      "weakchild_disagree",
      "sleep_net_lots",
      "sleep_net_warm",
      "selfeff_any"
    )
    out <- summ_multi_binary(dd, vars, wvar)
    
    tibble(
      Characteristic = label,
      `Percentage who disagree that people in the community get malaria only during the rainy season` =
        format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Percentage who agree that when a child has a fever, they almost always worry it might be malaria` =
        format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Percentage who perceive that their families and communities are at risk from malaria` =
        format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Percentage who disagree that getting malaria is not a problem because it can be easily treated` =
        format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Percentage who disagree that only weak children can die from malaria` =
        format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Percentage who agree that they can sleep under a mosquito net for the entire night when there are lots of mosquitoes` =
        format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Percentage who agree that they can sleep under a mosquito net for the entire night when the weather is too warm` =
        format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      `Percentage who are confident in their ability to perform specific malaria-related behaviours captured in Sproxil` =
        format_estimate_display(out[[8]]$p, out[[8]]$n, DISPLAY_DIGITS),
      `Number of women` = format_count_display(nrow(dd)),
      base_status = combine_base_status(
        out[[1]]$stat, out[[2]]$stat, out[[3]]$stat, out[[4]]$stat,
        out[[5]]$stat, out[[6]]$stat, out[[7]]$stat, out[[8]]$stat
      )
    )
  }
  
  body <- women_bg_blocks_national(
    d, make_row, template_row_fun = template_row,
    include_age = TRUE, include_living_u5 = TRUE,
    include_residence = TRUE, include_zone = TRUE,
    include_education = TRUE, include_wealth = TRUE
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    "The susceptibility summary includes women who disagree that people in the community get malaria only during the rainy season or agree that when a child has a fever, they almost always worry it might be malaria.",
    "The self-efficacy summary includes women who agree that they can sleep under a mosquito net for the entire night when there are lots of mosquitoes or when the weather is too warm.",
    collapse_footnotes("captured_only_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Background characteristics",
      middle_header = "Perceptions and self-efficacy items captured in Sproxil",
      middle_cols = 3:10,
      right_headers = c("Number of women")
    )
  )
}

build_table_5_3_2 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_hh_has_u5",
    "att_rainy_season_only", "att_fever_worry_malaria",
    "att_malaria_easily_treated", "att_weak_children_die",
    "att_net_use_mosquito_density", "att_net_use_warm_weather",
    "derived_zone", "demo_state_num"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_women_frame(dat) %>%
    mutate(
      rainy_disagree = ifelse(
        to_num(att_rainy_season_only) %in% c(1, 2),
        ifelse(to_num(att_rainy_season_only) == 2, 1, 0),
        NA_real_
      ),
      fever_worry_agree = ifelse(
        to_num(att_fever_worry_malaria) %in% c(1, 2),
        ifelse(to_num(att_fever_worry_malaria) == 1, 1, 0),
        NA_real_
      ),
      easy_treat_disagree = ifelse(
        to_num(att_malaria_easily_treated) %in% c(1, 2),
        ifelse(to_num(att_malaria_easily_treated) == 2, 1, 0),
        NA_real_
      ),
      weakchild_disagree = ifelse(
        to_num(att_weak_children_die) %in% c(1, 2),
        ifelse(to_num(att_weak_children_die) == 2, 1, 0),
        NA_real_
      ),
      sleep_net_lots = ifelse(
        to_num(att_net_use_mosquito_density) %in% c(1, 2),
        ifelse(to_num(att_net_use_mosquito_density) == 2, 1, 0),
        NA_real_
      ),
      sleep_net_warm = ifelse(
        to_num(att_net_use_warm_weather) %in% c(1, 2),
        ifelse(to_num(att_net_use_warm_weather) == 2, 1, 0),
        NA_real_
      ),
      susceptibility_any = case_when(
        rowSums(!is.na(cbind(rainy_disagree, fever_worry_agree))) == 0 ~ NA_real_,
        rowSums(cbind(rainy_disagree == 1, fever_worry_agree == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      ),
      selfeff_any = case_when(
        rowSums(!is.na(cbind(sleep_net_lots, sleep_net_warm))) == 0 ~ NA_real_,
        rowSums(cbind(sleep_net_lots == 1, sleep_net_warm == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      )
    )
  
  template_row <- function(label) {
    tibble(
      State = label,
      `Percentage who disagree that people in the community get malaria only during the rainy season` = "",
      `Percentage who agree that when a child has a fever, they almost always worry it might be malaria` = "",
      `Percentage who perceive that their families and communities are at risk from malaria` = "",
      `Percentage who disagree that getting malaria is not a problem because it can be easily treated` = "",
      `Percentage who disagree that only weak children can die from malaria` = "",
      `Percentage who agree that they can sleep under a mosquito net for the entire night when there are lots of mosquitoes` = "",
      `Percentage who agree that they can sleep under a mosquito net for the entire night when the weather is too warm` = "",
      `Percentage who are confident in their ability to perform specific malaria-related behaviours captured in Sproxil` = "",
      `Number of women` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    vars <- c(
      "rainy_disagree",
      "fever_worry_agree",
      "susceptibility_any",
      "easy_treat_disagree",
      "weakchild_disagree",
      "sleep_net_lots",
      "sleep_net_warm",
      "selfeff_any"
    )
    out <- summ_multi_binary(dd, vars, wvar)
    
    tibble(
      State = label,
      `Percentage who disagree that people in the community get malaria only during the rainy season` =
        format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Percentage who agree that when a child has a fever, they almost always worry it might be malaria` =
        format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Percentage who perceive that their families and communities are at risk from malaria` =
        format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Percentage who disagree that getting malaria is not a problem because it can be easily treated` =
        format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Percentage who disagree that only weak children can die from malaria` =
        format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Percentage who agree that they can sleep under a mosquito net for the entire night when there are lots of mosquitoes` =
        format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Percentage who agree that they can sleep under a mosquito net for the entire night when the weather is too warm` =
        format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      `Percentage who are confident in their ability to perform specific malaria-related behaviours captured in Sproxil` =
        format_estimate_display(out[[8]]$p, out[[8]]$n, DISPLAY_DIGITS),
      `Number of women` = format_count_display(nrow(dd)),
      base_status = combine_base_status(
        out[[1]]$stat, out[[2]]$stat, out[[3]]$stat, out[[4]]$stat,
        out[[5]]$stat, out[[6]]$stat, out[[7]]$stat, out[[8]]$stat
        
      )
    )
  }
  
  body <- women_state_zone_blocks(d, make_row, template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("captured_only_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("state_sample_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "STATE", body = body %>% select(-base_status), footnote = footnote)
}


build_table_5_4_1 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_hh_has_u5",
    "att_net_use_warm_weather", "att_home_meds_first", "att_full_dose_importance",
    "att_seek_care_immediate", "att_community_net_usage",
    "derived_age_group_w", "derived_residence", "derived_zone",
    "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_women_frame(dat) %>%
    mutate(
      warm_disagree = ifelse(
        to_num(att_net_use_warm_weather) %in% c(1, 2),
        ifelse(to_num(att_net_use_warm_weather) == 2, 1, 0),
        NA_real_
      ),
      homemed_disagree = ifelse(
        to_num(att_home_meds_first) %in% c(1, 2),
        ifelse(to_num(att_home_meds_first) == 2, 1, 0),
        NA_real_
      ),
      fulldose_agree = ifelse(
        to_num(att_full_dose_importance) %in% c(1, 2),
        ifelse(to_num(att_full_dose_importance) == 1, 1, 0),
        NA_real_
      ),
      seekcare_agree = ifelse(
        to_num(att_seek_care_immediate) %in% c(1, 2),
        ifelse(to_num(att_seek_care_immediate) == 1, 1, 0),
        NA_real_
      ),
      netnorm_agree = ifelse(
        to_num(att_community_net_usage) %in% c(1, 2),
        ifelse(to_num(att_community_net_usage) == 1, 1, 0),
        NA_real_
      ),
      attitude_any = case_when(
        rowSums(!is.na(cbind(warm_disagree, homemed_disagree, fulldose_agree))) == 0 ~ NA_real_,
        rowSums(cbind(warm_disagree == 1, homemed_disagree == 1, fulldose_agree == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      ),
      norms_any = case_when(
        rowSums(!is.na(cbind(seekcare_agree, netnorm_agree))) == 0 ~ NA_real_,
        rowSums(cbind(seekcare_agree == 1, netnorm_agree == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      )
    )
  
  template_row <- function(label) {
    tibble(
      Characteristic = label,
      `Percentage who disagree that they do not like sleeping under a mosquito net when the weather is too warm` = "",
      `Percentage who disagree that when a child has a fever, it is best to start by giving the child any medicine they have at home` = "",
      `Percentage who agree that children take the full dose of medicine that they are prescribed for malaria` = "",
      `Percentage who have a favourable attitude towards specific malaria-related behaviours captured in Sproxil` = "",
      `Percentage who agree that people in the community usually take their children to a health care provider on the same day or day after they develop a fever` = "",
      `Percentage who agree that people in the community who have a mosquito net usually sleep under a mosquito net every night` = "",
      `Percentage who believe the majority of people in their community currently practise specific malaria-related behaviours captured in Sproxil` = "",
      `Number of women` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    vars <- c(
      "warm_disagree",
      "homemed_disagree",
      "fulldose_agree",
      "attitude_any",
      "seekcare_agree",
      "netnorm_agree",
      "norms_any"
    )
    out <- summ_multi_binary(dd, vars, wvar)
    
    tibble(
      Characteristic = label,
      `Percentage who disagree that they do not like sleeping under a mosquito net when the weather is too warm` =
        format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Percentage who disagree that when a child has a fever, it is best to start by giving the child any medicine they have at home` =
        format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Percentage who agree that children take the full dose of medicine that they are prescribed for malaria` =
        format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Percentage who have a favourable attitude towards specific malaria-related behaviours captured in Sproxil` =
        format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Percentage who agree that people in the community usually take their children to a health care provider on the same day or day after they develop a fever` =
        format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Percentage who agree that people in the community who have a mosquito net usually sleep under a mosquito net every night` =
        format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Percentage who believe the majority of people in their community currently practise specific malaria-related behaviours captured in Sproxil` =
        format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      `Number of women` = format_count_display(nrow(dd)),
      base_status = combine_base_status(
        out[[1]]$stat, out[[2]]$stat, out[[3]]$stat, out[[4]]$stat,
        out[[5]]$stat, out[[6]]$stat, out[[7]]$stat
      )
    )
  }
  
  body <- women_bg_blocks_national(
    d, make_row, template_row_fun = template_row,
    include_age = TRUE, include_living_u5 = TRUE,
    include_residence = TRUE, include_zone = TRUE,
    include_education = TRUE, include_wealth = TRUE
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    "The favourable-attitude summary includes women who disagree that they do not like sleeping under a mosquito net when the weather is too warm, disagree that when a child has a fever it is best to start by giving the child any medicine they have at home, or agree that children should take the full dose of medicine prescribed for malaria.",
    "The community-practice summary includes women who agree that people in the community usually take their children to a health care provider on the same day or day after fever develops or agree that people in the community who have a mosquito net usually sleep under it every night.",
    collapse_footnotes("captured_only_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(
    type = "H",
    body = body %>% select(-base_status),
    footnote = footnote,
    writer_args = list(
      left_header = "Background characteristic",
      middle_header = "Attitudes and perceived norms captured in Sproxil",
      middle_cols = 3:9,
      right_headers = c("Number of women")
    )
  )
}


build_table_5_4_2 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_hh_has_u5",
    "att_net_use_warm_weather", "att_home_meds_first", "att_full_dose_importance",
    "att_seek_care_immediate", "att_community_net_usage",
    "derived_zone", "demo_state_num"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_women_frame(dat) %>%
    mutate(
      warm_disagree = ifelse(
        to_num(att_net_use_warm_weather) %in% c(1, 2),
        ifelse(to_num(att_net_use_warm_weather) == 2, 1, 0),
        NA_real_
      ),
      homemed_disagree = ifelse(
        to_num(att_home_meds_first) %in% c(1, 2),
        ifelse(to_num(att_home_meds_first) == 2, 1, 0),
        NA_real_
      ),
      fulldose_agree = ifelse(
        to_num(att_full_dose_importance) %in% c(1, 2),
        ifelse(to_num(att_full_dose_importance) == 1, 1, 0),
        NA_real_
      ),
      seekcare_agree = ifelse(
        to_num(att_seek_care_immediate) %in% c(1, 2),
        ifelse(to_num(att_seek_care_immediate) == 1, 1, 0),
        NA_real_
      ),
      netnorm_agree = ifelse(
        to_num(att_community_net_usage) %in% c(1, 2),
        ifelse(to_num(att_community_net_usage) == 1, 1, 0),
        NA_real_
      ),
      attitude_any = case_when(
        rowSums(!is.na(cbind(warm_disagree, homemed_disagree, fulldose_agree))) == 0 ~ NA_real_,
        rowSums(cbind(warm_disagree == 1, homemed_disagree == 1, fulldose_agree == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      ),
      norms_any = case_when(
        rowSums(!is.na(cbind(seekcare_agree, netnorm_agree))) == 0 ~ NA_real_,
        rowSums(cbind(seekcare_agree == 1, netnorm_agree == 1), na.rm = TRUE) > 0 ~ 1,
        TRUE ~ 0
      )
    )
  
  template_row <- function(label) {
    tibble(
      State = label,
      `Percentage who disagree that they do not like sleeping under a mosquito net when the weather is too warm` = "",
      `Percentage who disagree that when a child has a fever, it is best to start by giving the child any medicine they have at home` = "",
      `Percentage who agree that children take the full dose of medicine that they are prescribed for malaria` = "",
      `Percentage who have a favourable attitude towards specific malaria-related behaviours captured in Sproxil` = "",
      `Percentage who agree that people in the community usually take their children to a health care provider on the same day or day after they develop a fever` = "",
      `Percentage who agree that people in the community who have a mosquito net usually sleep under a mosquito net every night` = "",
      `Percentage who believe the majority of people in their community currently practise specific malaria-related behaviours captured in Sproxil` = "",
      `Number of women` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    vars <- c(
      "warm_disagree",
      "homemed_disagree",
      "fulldose_agree",
      "attitude_any",
      "seekcare_agree",
      "netnorm_agree",
      "norms_any"
    )
    
    out <- summ_multi_binary(dd, vars, wvar)
    
    tibble(
      State = label,
      `Percentage who disagree that they do not like sleeping under a mosquito net when the weather is too warm` =
        format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Percentage who disagree that when a child has a fever, it is best to start by giving the child any medicine they have at home` =
        format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Percentage who agree that children take the full dose of medicine that they are prescribed for malaria` =
        format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Percentage who have a favourable attitude towards specific malaria-related behaviours captured in Sproxil` =
        format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Percentage who agree that people in the community usually take their children to a health care provider on the same day or day after they develop a fever` =
        format_estimate_display(out[[5]]$p, out[[5]]$n, DISPLAY_DIGITS),
      `Percentage who agree that people in the community who have a mosquito net usually sleep under a mosquito net every night` =
        format_estimate_display(out[[6]]$p, out[[6]]$n, DISPLAY_DIGITS),
      `Percentage who believe the majority of people in their community currently practise specific malaria-related behaviours captured in Sproxil` =
        format_estimate_display(out[[7]]$p, out[[7]]$n, DISPLAY_DIGITS),
      `Number of women` = format_count_display(nrow(dd)),
      base_status = combine_base_status(
        out[[1]]$stat, out[[2]]$stat, out[[3]]$stat, out[[4]]$stat,
        out[[5]]$stat, out[[6]]$stat, out[[7]]$stat
      )
    )
  }
  
  body <- women_state_zone_blocks(d, make_row, template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    "The favourable-attitude summary includes women who disagree that they do not like sleeping under a mosquito net when the weather is too warm, disagree that when a child has a fever it is best to start by giving the child any medicine they have at home, or agree that children should take the full dose of medicine prescribed for malaria.",
    "The community-practice summary includes women who agree that people in the community usually take their children to a health care provider on the same day or day after fever develops or agree that people in the community who have a mosquito net usually sleep under it every night.",
    collapse_footnotes("captured_only_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("state_sample_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "STATE", body = body %>% select(-base_status), footnote = footnote)
}


build_table_6_1_1 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_hh_has_u5",
    "derived_percep_affordable", "derived_exp_stockout", "derived_exp_free_tx", "derived_gov_effective",
    "derived_age_group_w", "derived_residence", "derived_zone",
    "derived_wealth_quintile", "derived_edu_cat"
  ), dat)
  
  wvar <- get_wvar(dat)
  d <- get_women_frame(dat)
  
  template_row <- function(label) {
    tibble(
      Characteristic = label,
      `Percentage who agree that cost of malaria drugs at a health facility is affordable` = "",
      `Percentage who experienced malaria drug stockout at a government facility` = "",
      `Percentage who received free malaria treatment at a government facility` = "",
      `Percentage who rate government's control efforts effective` = "",
      `Number of respondents` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    vars <- c("derived_percep_affordable","derived_exp_stockout","derived_exp_free_tx","derived_gov_effective")
    out <- summ_multi_binary(dd, vars, wvar)
    
    tibble(
      Characteristic = label,
      `Percentage who agree that cost of malaria drugs at a health facility is affordable` = format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Percentage who experienced malaria drug stockout at a government facility` = format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Percentage who received free malaria treatment at a government facility` = format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Percentage who rate government's control efforts effective` = format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Number of respondents` = format_count_display(nrow(dd)),
      base_status = combine_base_status(out[[1]]$stat,out[[2]]$stat,out[[3]]$stat,out[[4]]$stat)
    )
  }
  
  body <- women_bg_blocks_national(
    d, make_row, template_row_fun = template_row,
    include_age = TRUE, include_living_u5 = TRUE,
    include_residence = TRUE, include_zone = TRUE,
    include_education = TRUE, include_wealth = TRUE
  )
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("stockout_free_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("subgroup_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "B", body = body %>% select(-base_status), footnote = footnote)
}

build_table_6_1_2 <- function(dat) {
  need_cols(c(
    "u_women_15_49", "u_hh_has_u5",
    "derived_percep_affordable", "derived_exp_stockout", "derived_exp_free_tx", "derived_gov_effective",
    "derived_zone", "demo_state_num"
  ), dat)
  
  wvar <- get_wvar(dat)
  
  d <- get_women_frame(dat)
  
  template_row <- function(label) {
    tibble(
      State = label,
      `Percentage who agree that cost of malaria drugs at a health facility is affordable` = "",
      `Percentage who experienced malaria drug stockout at a government facility` = "",
      `Percentage who received free malaria treatment at a government facility` = "",
      `Percentage who rate government's control efforts effective` = "",
      `Number of respondents` = "",
      base_status = "ok"
    )
  }
  
  make_row <- function(dd, label) {
    vars <- c(
      "derived_percep_affordable",
      "derived_exp_stockout",
      "derived_exp_free_tx",
      "derived_gov_effective"
    )
    
    out <- summ_multi_binary(dd, vars, wvar)
    
    tibble(
      State = label,
      `Percentage who agree that cost of malaria drugs at a health facility is affordable` =
        format_estimate_display(out[[1]]$p, out[[1]]$n, DISPLAY_DIGITS),
      `Percentage who experienced malaria drug stockout at a government facility` =
        format_estimate_display(out[[2]]$p, out[[2]]$n, DISPLAY_DIGITS),
      `Percentage who received free malaria treatment at a government facility` =
        format_estimate_display(out[[3]]$p, out[[3]]$n, DISPLAY_DIGITS),
      `Percentage who rate government's control efforts effective` =
        format_estimate_display(out[[4]]$p, out[[4]]$n, DISPLAY_DIGITS),
      `Number of respondents` = format_count_display(nrow(dd)),
      base_status = combine_base_status(
        out[[1]]$stat, out[[2]]$stat, out[[3]]$stat, out[[4]]$stat
      )
    )
  }
  
  body <- women_state_zone_blocks(d, make_row, template_row)
  
  footnote <- paste(
    collapse_footnotes("weighted_counts"),
    collapse_footnotes("women_only"),
    collapse_footnotes("stockout_free_note"),
    collapse_footnotes("noedu_note"),
    collapse_footnotes("state_sample_caution"),
    base_note_suffix(body, "base_status")
  )
  
  list(type = "STATE", body = body %>% select(-base_status), footnote = footnote)
}


# ------------------------------------------------------------------------------
# 7. Driver
# ------------------------------------------------------------------------------
run_phase2b <- function(dat, out_xlsx = OUT_XLSX) {
  wb <- wb_init(out_xlsx)
  
  specs <- list(
    list(
      sheet = "Table 4.1",
      title = "Table 4.1 Fever, care-seeking, testing, and malaria diagnosis among selected youngest children under age 5",
      build = function() build_table_4_1(dat)
    ),
    list(
      sheet = "Table 4.2",
      title = "Table 4.2 Referral to higher level of care (Sproxil respondents)",
      build = function() build_table_4_2(dat)
    ),
    list(
      sheet = "Table 4.3",
      title = "Table 4.3 Source of advice or treatment for children with fever (Sproxil respondents)",
      build = function() build_table_4_3(dat)
    ),
    list(
      sheet = "Table 4.4",
      title = "Table 4.4 Type of antimalarial drugs used (Sproxil respondents)",
      build = function() build_table_4_4(dat)
    ),
    list(
      sheet = "Table 4.5",
      title = "Table 4.5 ACT use and fever (Sproxil respondents)",
      build = function() build_table_4_5(dat)
    ),
    list(
      sheet = "Table 5.1.1",
      title = "Table 5.1.1 Media exposure to malaria messages (Sproxil respondents)",
      build = function() build_table_5_1_1(dat)
    ),
    list(
      sheet = "Table 5.1.2",
      title = "Table 5.1.2 Media exposure to malaria messages: States",
      build = function() build_table_5_1_2(dat)
    ),
    list(
      sheet = "Table 5.2.1",
      title = "Table 5.2.1 Knowledge of ways to avoid malaria: National",
      build = function() build_table_5_2_1(dat)
    ),
    list(
      sheet = "Table 5.2.2",
      title = "Table 5.2.2 Knowledge of ways to avoid malaria: States",
      build = function() build_table_5_2_2(dat)
    ),
    list(
      sheet = "Table 5.3.1",
      title = "Table 5.3.1 Malaria susceptibility, severity, and self-efficacy: National",
      build = function() build_table_5_3_1(dat)
    ),
    list(
      sheet = "Table 5.3.2",
      title = "Table 5.3.2 Malaria susceptibility, severity, and self-efficacy: States",
      build = function() build_table_5_3_2(dat)
    ),
    list(
      sheet = "Table 5.4.1",
      title = "Table 5.4.1 Attitudes towards malaria-related behaviours and malaria norms: National",
      build = function() build_table_5_4_1(dat)
    ),
    list(
      sheet = "Table 5.4.2",
      title = "Table 5.4.2 Attitudes towards malaria-related behaviours and malaria norms: States",
      build = function() build_table_5_4_2(dat)
    ),
    list(
      sheet = "Table 6.1.1",
      title = "Table 6.1.1 Perceptions of Malaria Care Affordability and Control Efforts: National",
      build = function() build_table_6_1_1(dat)
    ),
    list(
      sheet = "Table 6.1.2",
      title = "Table 6.1.2 Perceptions of Malaria Care Affordability and Control Efforts: States",
      build = function() build_table_6_1_2(dat)
    )
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
    } else if (obj$type == "4.1_custom") {
      write_table_4_1_custom(wb, sp$sheet, sp$title, obj$body, obj$footnote)
    } else if (obj$type == "STATE") {
      write_table_state_grouped(wb, sp$sheet, sp$title, obj$body, obj$footnote)
    } else {
      stop("Unsupported writer type: ", obj$type)
    }
  }  
  
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  message("✅ Phase 2B tables written to: ", out_xlsx)
}


# ------------------------------------------------------------------------------
# 8. Execute
# ------------------------------------------------------------------------------
run_phase2b(df, OUT_XLSX)
