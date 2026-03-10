suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(openxlsx)
})

INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
WVAR      <- "w_calibrated_trim"

df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
w <- if (use_weight) WVAR else NULL
message("Table 2.13 weighting active: ", use_weight)

# -----------------------------
# Labels
# -----------------------------
res_lab  <- c(`1`="Urban", `2`="Rural")
zone_lab <- c(`1`="Region 1",`2`="Region 2",`3`="Region 3",`4`="Region 4",`5`="Region 5",`6`="Region 6")
wq_lab   <- c(`1`="Lowest",`2`="Second",`3`="Middle",`4`="Fourth",`5`="Highest")
agew_lab <- c(`1`="15-19",`2`="20-24",`3`="25-29",`4`="30-34",`5`="35-39",`6`="40-44",`7`="45-49")

state_names <- c(
  `1`="Abia",`2`="Adamawa",`3`="Akwa Ibom",`4`="Anambra",`5`="Bauchi",`6`="Bayelsa",
  `7`="Benue",`8`="Borno",`9`="Cross River",`10`="Delta",`11`="Ebonyi",`12`="Edo",
  `13`="Ekiti",`14`="Enugu",`15`="FCT-Abuja",`16`="Gombe",`17`="Imo",`18`="Jigawa",
  `19`="Kaduna",`20`="Kano",`21`="Katsina",`22`="Kebbi",`23`="Kogi",`24`="Kwara",
  `25`="Lagos",`26`="Nasarawa",`27`="Niger",`28`="Ogun",`29`="Ondo",`30`="Osun",
  `31`="Oyo",`32`="Plateau",`33`="Rivers",`34`="Sokoto",`35`="Taraba",`36`="Yobe",
  `37`="Zamfara"
)
zone_header <- c(`1`="North Central",`2`="North East",`3`="North West",`4`="South East",`5`="South South",`6`="South West")

# -----------------------------
# Helpers
# -----------------------------
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

w_mean <- function(x, wts=NULL) {
  if (is.null(wts)) return(mean(x, na.rm=TRUE))
  ok <- !is.na(x) & !is.na(wts)
  if (!any(ok)) return(NA_real_)
  sum(wts[ok]*x[ok]) / sum(wts[ok])
}
pct <- function(x, wts=NULL) round(100*w_mean(x, wts), 1)

note_txt <- if (use_weight) "NOTE: Weighted using calibrated weights (trimmed)" else "NOTE: Unweighted"

# -----------------------------
# Required vars check
# -----------------------------
req <- c("u_women_15_49","derived_age_group_w","derived_residence","derived_zone","derived_wealth_quintile",
         "derived_edu_cat","demo_state_num","edu_inf_adult","edu_inf_tsangaya","edu_inf_quranic","edu_inf_other")
miss <- setdiff(req, names(df))
if (length(miss) > 0) {
  stop("Missing required variables in Analysis_Ready: ", paste(miss, collapse=", "))
}

# -----------------------------
# Build analysis frame
# -----------------------------
d <- df %>%
  mutate(
    u_women_15_49 = to_num(u_women_15_49),
    agew = to_num(derived_age_group_w),
    res  = to_num(derived_residence),
    zone = to_num(derived_zone),
    wq   = to_num(derived_wealth_quintile),
    edu4 = to_num(derived_edu_cat),
    st   = to_num(demo_state_num),
    
    # No formal education (your derived 4-cat education)
    no_formal = ifelse(edu4 == 1, 1, 0),
    
    # Attended informal schooling: any of the three main types
    inf_any = case_when(
      no_formal != 1 ~ NA_real_,
      to_num(edu_inf_adult)==1 | to_num(edu_inf_tsangaya)==1 | to_num(edu_inf_quranic)==1 ~ 1,
      to_num(edu_inf_other)==1 ~ 1,   # they still attended something
      TRUE ~ 0
    ),
    
    # Type classification for distribution (template only has Adult/Tsangaya/Quranic)
    inf_type = case_when(
      to_num(edu_inf_adult)==1 ~ "Adult education",
      to_num(edu_inf_tsangaya)==1 ~ "Tsangaya",
      to_num(edu_inf_quranic)==1 ~ "Quranic",
      TRUE ~ "Other/unspecified"
    )
  ) %>%
  filter(u_women_15_49 == 1)

# -----------------------------
# Core calculator for a group subset
# -----------------------------
calc_row <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  
  # Denominator: women with no formal education
  nof <- dat %>% filter(no_formal == 1)
  wts_nof <- if (!is.null(w)) nof[[w]] else NULL
  
  # % attended informal schooling (among no-formal-edu)
  p_att <- pct(as.numeric(nof$inf_any == 1), wts_nof)
  
  N_nof <- nrow(nof) # unweighted count for "Number of women with no formal education"
  
  # Among those who attended, distribution by type
  att <- nof %>% filter(inf_any == 1)
  wts_att <- if (!is.null(w)) att[[w]] else NULL
  
  # Exclude "Other/unspecified" from type shares because template has no "Other" column
  att3 <- att %>% filter(inf_type %in% c("Adult education","Tsangaya","Quranic"))
  wts_att3 <- if (!is.null(w)) att3[[w]] else NULL
  
  p_adult <- pct(as.numeric(att3$inf_type == "Adult education"), wts_att3)
  p_tsang <- pct(as.numeric(att3$inf_type == "Tsangaya"), wts_att3)
  p_qura  <- pct(as.numeric(att3$inf_type == "Quranic"), wts_att3)
  
  # Total column for type distribution: 100 (only for the restricted 3-type denom)
  p_total <- if (nrow(att3) > 0) 100.0 else NA_real_
  
  N_att <- nrow(att) # unweighted "Number of women who attended informal schooling"
  
  tibble(
    `Background characteristic` = rowlab,
    `Percentage of women who attended informal schooling` = p_att,
    `Number of women with no formal education` = N_nof,
    `Adult education` = p_adult,
    `Tsangaya` = p_tsang,
    `Quranic` = p_qura,
    `Total` = p_total,
    `Number of women who attended informal schooling` = N_att
  )
}

# -----------------------------
# Build Table 2.13.1 (National)
# -----------------------------
block <- function(header, dat, var, levels, lab) {
  out <- tibble(`Background characteristic` = header)
  for (k in levels) {
    dd <- dat %>% filter(.data[[var]] == k)
    out <- bind_rows(out, calc_row(dd, unname(lab[as.character(k)])))
  }
  bind_rows(out, tibble(`Background characteristic` = ""))
}

t2131 <- bind_rows(
  block("Age", d, "agew", 1:7, agew_lab),
  block("Residence", d, "res", 1:2, res_lab),
  block("Zone", d, "zone", 1:6, zone_lab),
  block("Wealth quintile", d, "wq", 1:5, wq_lab),
  calc_row(d, "Total")
)

# -----------------------------
# Build Table 2.13.2 (States)
# -----------------------------
make_state_table <- function(dat) {
  out <- tibble()
  dat2 <- dat %>%
    filter(!is.na(st), st %in% 1:37, !is.na(zone), zone %in% 1:6)
  
  for (z in 1:6) {
    out <- bind_rows(out, tibble(State = zone_header[as.character(z)]))
    st_in_z <- dat2 %>% filter(zone==z) %>% distinct(st) %>% pull(st) %>% sort()
    
    for (s in st_in_z) {
      dd <- dat2 %>% filter(st==s)
      row <- calc_row(dd, state_names[as.character(s)]) %>%
        rename(State = `Background characteristic`)
      out <- bind_rows(out, row)
    }
    out <- bind_rows(out, tibble(State = ""))
  }
  
  out <- bind_rows(out, calc_row(dat2, "Total") %>% rename(State=`Background characteristic`))
  out
}

t2132 <- make_state_table(d)

# -----------------------------
# Write to Excel
# -----------------------------
wb <- if (file.exists(OUT_XLSX)) loadWorkbook(OUT_XLSX) else createWorkbook()

write_sheet <- function(wb, sheet, title, note, tab, footnote) {
  if (sheet %in% names(wb)) removeWorksheet(wb, sheet)
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  titleStyle <- createStyle(textDecoration="bold", fontSize=12)
  noteStyle  <- createStyle(textDecoration="italic", fontSize=9)
  hdrStyle   <- createStyle(textDecoration="bold")
  num1Style  <- createStyle(numFmt="0.0")
  intStyle   <- createStyle(numFmt="0")
  
  writeData(wb, sheet, title, startRow=2, startCol=2)
  addStyle(wb, sheet, titleStyle, rows=2, cols=2, stack=TRUE)
  
  writeData(wb, sheet, note, startRow=3, startCol=2)
  addStyle(wb, sheet, noteStyle, rows=3, cols=2, stack=TRUE)
  
  writeData(wb, sheet, tab, startRow=5, startCol=2, colNames=TRUE)
  addStyle(wb, sheet, hdrStyle, rows=5, cols=2:(ncol(tab)+1), gridExpand=TRUE, stack=TRUE)
  
  # formatting
  num_cols <- which(sapply(tab, is.numeric))
  if (length(num_cols)>0) addStyle(wb, sheet, num1Style, rows=6:(6+nrow(tab)-1), cols=(1+num_cols), gridExpand=TRUE, stack=TRUE)
  int_cols <- which(grepl("^Number", names(tab)))
  if (length(int_cols)>0) addStyle(wb, sheet, intStyle, rows=6:(6+nrow(tab)-1), cols=(1+int_cols), gridExpand=TRUE, stack=TRUE)
  
  writeData(wb, sheet, footnote, startRow=6+nrow(tab)+2, startCol=2)
  addStyle(wb, sheet, noteStyle, rows=6+nrow(tab)+2, cols=2, stack=TRUE)
  
  setColWidths(wb, sheet, cols=2, widths=34)
  setColWidths(wb, sheet, cols=3:(ncol(tab)+1), widths=18)
}

foot <- "1 No education includes informal education (adult education, Tsangaya, or Quranic). Type distribution excludes 'Other/unspecified' because the template has no 'Other' column."

write_sheet(
  wb,
  "Table 2.13.1",
  "Table 2.13.1 Informal schooling attendance of interviewed women: National (Sproxil respondents)",
  note_txt,
  t2131,
  foot
)

write_sheet(
  wb,
  "Table 2.13.2",
  "Table 2.13.2 Informal schooling attendance of interviewed women: States (Sproxil respondents)",
  note_txt,
  t2132,
  foot
)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Wrote Table 2.13.1 and 2.13.2 to: ", OUT_XLSX)