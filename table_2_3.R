suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
  library(stringr)
})

# -----------------------------
# CONFIG
# -----------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
SHEET     <- "Table 2.3"
WVAR      <- "w_calibrated_trim"   # used only if present & non-missing

# -----------------------------
# Load data
# -----------------------------
df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.3 weighting active: ", use_weight)

# -----------------------------
# Helpers
# -----------------------------
dist_wide <- function(dat, group_var, cat_var, expected_levels = NULL, wvar = NULL) {
  d <- dat %>% filter(!is.na(.data[[group_var]]), !is.na(.data[[cat_var]]))
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (is.null(wvar)) {
    out <- d %>%
      count(.data[[group_var]], .data[[cat_var]], name = "n") %>%
      group_by(.data[[group_var]]) %>%
      mutate(p = 100 * n / sum(n)) %>%
      ungroup()
  } else {
    out <- d %>%
      group_by(.data[[group_var]], .data[[cat_var]]) %>%
      summarise(w = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[group_var]]) %>%
      mutate(p = 100 * w / sum(w)) %>%
      ungroup()
  }
  
  out <- out %>%
    transmute(group = as.character(.data[[group_var]]),
              cat   = as.character(.data[[cat_var]]),
              p     = round(p, 1))
  
  if (!is.null(expected_levels)) {
    out <- out %>%
      mutate(cat = factor(cat, levels = expected_levels)) %>%
      tidyr::complete(group, cat, fill = list(p = 0.0)) %>%
      mutate(cat = as.character(cat))
  }
  
  out_wide <- out %>%
    pivot_wider(names_from = group, values_from = p, values_fill = 0.0)
  
  for (nm in c("Urban", "Rural", "Total")) {
    if (!nm %in% names(out_wide)) out_wide[[nm]] <- 0.0
  }
  
  out_wide %>%
    select(cat, Urban, Rural, Total)
}

# -----------------------------
# Build Table 2.3 analysis frame (Analysis_Ready codes)
# -----------------------------
df_t23 <- df %>%
  filter(u_household == 1) %>%
  mutate(
    res_grp = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    toilet = as.numeric(hh_toilet_type),
    tloc   = as.numeric(hh_toilet_location),
    
    # --- Toilet facility grouping + detail rows
    toilet_group = case_when(
      toilet %in% c(11, 12, 13, 21, 22, 31) ~ "Improved sanitation facility",
      toilet %in% c(14, 15, 23, 41, 51, 96) ~ "Unimproved sanitation facility",
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
      
      toilet %in% c(14, 15) ~ "Flush/pour flush not to sewer/ septic tank/pit latrine",
      toilet == 23 ~ "Pit latrine without slab/open pit",
      toilet == 41 ~ "Bucket",
      toilet == 51 ~ "Hanging toilet/hanging latrine",
      toilet == 96 ~ "Other",
      
      toilet == 61 ~ "Open defecation (no facility/bush/field)",
      TRUE ~ NA_character_
    ),
    
    # Location of toilet facility
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
  ) %>%
  filter(!is.na(res_grp))

# Add Total stratum
df_t23_tot <- df_t23 %>% mutate(res_grp = "Total")

# Location section dataset: only respondents with facility
df_loc <- bind_rows(
  df_t23 %>% filter(has_facility == 1, !is.na(loc_cat)),
  df_t23 %>% filter(has_facility == 1, !is.na(loc_cat)) %>% mutate(res_grp = "Total")
)

# -----------------------------
# Distributions
# -----------------------------
group_levels  <- c(
  "Improved sanitation facility",
  "Unimproved sanitation facility",
  "Open defecation (no facility/bush/field)"
)

detail_levels <- c(
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

loc_levels <- c("In own dwelling", "In own yard/plot", "Elsewhere")

group_wide  <- dist_wide(bind_rows(df_t23, df_t23_tot), "res_grp", "toilet_group",
                         expected_levels = group_levels, wvar = wvar_use)

detail_wide <- dist_wide(bind_rows(df_t23, df_t23_tot), "res_grp", "toilet_detail",
                         expected_levels = detail_levels, wvar = wvar_use)

loc_wide    <- dist_wide(df_loc, "res_grp", "loc_cat",
                         expected_levels = loc_levels, wvar = wvar_use)

# Ns (unweighted)
n_all <- df_t23 %>%
  filter(!is.na(toilet_group)) %>%
  count(res_grp, name = "N") %>%
  bind_rows(tibble(res_grp = "Total", N = sum(.$N)))

n_fac <- df_t23 %>%
  filter(has_facility == 1) %>%
  count(res_grp, name = "N") %>%
  bind_rows(tibble(res_grp = "Total", N = sum(.$N)))

# -----------------------------
# Sheet skeleton
# -----------------------------
rows <- c(
  "Characteristic",
  "Type and location of toilet/latrine facility",
  "Improved sanitation facility",
  "   Flush/pour flush to piped sewer system",
  "   Flush/pour flush to septic tank",
  "   Flush/pour flush to a pit latrine",
  "   Ventilated improved pit (VIP) latrine",
  "   Pit latrine with a slab",
  "   Composting toilet",
  "",
  "Unimproved sanitation facility",
  "   Flush/pour flush not to sewer/ \n      septic tank/pit latrine",
  "",
  "   Pit latrine without slab/open pit",
  "   Bucket",
  "   Hanging toilet/hanging latrine",
  "   Other",
  "",
  "Open defecation (no facility/bush/field)",
  "",
  "Total ",
  "Number of respondents",
  "",
  "Location of toilet facility",
  "In own dwelling",
  "In own yard/plot",
  "Elsewhere",
  "",
  "Total ",
  "",
  "Number of respondents with a \n   toilet/latrine facility"
)

COL_LABEL <- 2
COL_URB   <- 4
COL_RUR   <- 5
COL_TOT   <- 6
START_ROW <- 7

row_of <- function(label) {
  i <- match(label, rows)
  if (is.na(i)) return(NA_integer_)
  START_ROW + i - 1
}

write_urt <- function(wb, sheet, r, u, rr, t) {
  if (is.na(r)) return(invisible(FALSE))
  writeData(wb, sheet, u,  startRow = r, startCol = COL_URB, colNames = FALSE)
  writeData(wb, sheet, rr, startRow = r, startCol = COL_RUR, colNames = FALSE)
  writeData(wb, sheet, t,  startRow = r, startCol = COL_TOT, colNames = FALSE)
  invisible(TRUE)
}

# -----------------------------
# Create / overwrite workbook sheet
# -----------------------------
if (file.exists(OUT_XLSX)) {
  wb <- loadWorkbook(OUT_XLSX)
} else {
  wb <- createWorkbook()
}

if (SHEET %in% names(wb)) removeWorksheet(wb, SHEET)
addWorksheet(wb, SHEET, gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
hdrStyle   <- createStyle(textDecoration = "bold")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
numStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(wb, SHEET, "Table 2.3  Household sanitation facilities (Sproxil respondents)", startRow = 3, startCol = 2)
addStyle(wb, SHEET, titleStyle, rows = 3, cols = 2, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; respondent counts are unweighted."
} else {
  "NOTE: Unweighted percentages; respondent counts are unweighted."
}
writeData(wb, SHEET, note_txt, startRow = 4, startCol = 2)
addStyle(wb, SHEET, noteStyle, rows = 4, cols = 2, stack = TRUE)

# headers
writeData(wb, SHEET, "Population", startRow = 6, startCol = 4)
writeData(wb, SHEET, "Urban", startRow = 7, startCol = 4)
writeData(wb, SHEET, "Rural", startRow = 7, startCol = 5)
writeData(wb, SHEET, "Total", startRow = 7, startCol = 6)
addStyle(wb, SHEET, hdrStyle, rows = 6:7, cols = 4:6, gridExpand = TRUE, stack = TRUE)

# labels
writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)

setColWidths(wb, SHEET, cols = 1, widths = 2)
setColWidths(wb, SHEET, cols = 2, widths = 60)
setColWidths(wb, SHEET, cols = 4:6, widths = 12)

# -----------------------------
# Fill: Group rows
# -----------------------------
for (lab in c("Improved sanitation facility", "Unimproved sanitation facility",
              "Open defecation (no facility/bush/field)")) {
  r <- row_of(lab)
  ww <- group_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET, numStyle, rows = r, cols = 4:6, gridExpand = TRUE, stack = TRUE)
}

# -----------------------------
# Fill: Detail rows
# -----------------------------
detail_map <- c(
  "   Flush/pour flush to piped sewer system" = "Flush/pour flush to piped sewer system",
  "   Flush/pour flush to septic tank"        = "Flush/pour flush to septic tank",
  "   Flush/pour flush to a pit latrine"      = "Flush/pour flush to a pit latrine",
  "   Ventilated improved pit (VIP) latrine"  = "Ventilated improved pit (VIP) latrine",
  "   Pit latrine with a slab"                = "Pit latrine with a slab",
  "   Composting toilet"                      = "Composting toilet",
  "   Flush/pour flush not to sewer/ \n      septic tank/pit latrine" =
    "Flush/pour flush not to sewer/ septic tank/pit latrine",
  "   Pit latrine without slab/open pit"      = "Pit latrine without slab/open pit",
  "   Bucket"                                 = "Bucket",
  "   Hanging toilet/hanging latrine"         = "Hanging toilet/hanging latrine",
  "   Other"                                  = "Other"
)

for (print_lab in names(detail_map)) {
  r <- row_of(print_lab)
  key <- unname(detail_map[print_lab])
  ww <- detail_wide %>% filter(cat == key)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET, numStyle, rows = r, cols = 4:6, gridExpand = TRUE, stack = TRUE)
}

# -----------------------------
# Totals
# -----------------------------
for (i in which(rows == "Total ")) {
  r <- START_ROW + i - 1
  write_urt(wb, SHEET, r, 100.0, 100.0, 100.0)
  addStyle(wb, SHEET, numStyle, rows = r, cols = 4:6, gridExpand = TRUE, stack = TRUE)
}

# -----------------------------
# Number of respondents (section 1)
# -----------------------------
rN1 <- row_of("Number of respondents")
n_u <- n_all %>% filter(res_grp == "Urban") %>% pull(N)
n_r <- n_all %>% filter(res_grp == "Rural") %>% pull(N)
n_t <- n_all %>% filter(res_grp == "Total") %>% pull(N)
write_urt(wb, SHEET, rN1, n_u, n_r, n_t)
addStyle(wb, SHEET, intStyle, rows = rN1, cols = 4:6, gridExpand = TRUE, stack = TRUE)

# -----------------------------
# Location of toilet facility (among those with facility)
# -----------------------------
for (lab in c("In own dwelling", "In own yard/plot", "Elsewhere")) {
  r <- row_of(lab)
  ww <- loc_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET, numStyle, rows = r, cols = 4:6, gridExpand = TRUE, stack = TRUE)
}

# -----------------------------
# Number of respondents with a toilet/latrine facility (section 2)
# -----------------------------
rN2 <- row_of("Number of respondents with a \n   toilet/latrine facility")
n2_u <- n_fac %>% filter(res_grp == "Urban") %>% pull(N)
n2_r <- n_fac %>% filter(res_grp == "Rural") %>% pull(N)
n2_t <- n_fac %>% filter(res_grp == "Total") %>% pull(N)
write_urt(wb, SHEET, rN2, n2_u, n2_r, n2_t)
addStyle(wb, SHEET, intStyle, rows = rN2, cols = 4:6, gridExpand = TRUE, stack = TRUE)

# Save
saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.3 written to: ", OUT_XLSX)