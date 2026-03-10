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
SHEET_T21 <- "Table 2.1"
WVAR      <- "w_calibrated_trim"   # if present and non-missing, use it for percentages

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.1 weighting active: ", use_weight)

# -----------------------------
# Helpers
# -----------------------------
dist_wide <- function(dat, group_var, cat_var, wvar = NULL) {
  d <- dat %>%
    filter(!is.na(.data[[group_var]]), !is.na(.data[[cat_var]]))
  
  if (!is.null(wvar)) {
    d <- d %>% filter(!is.na(.data[[wvar]]))
  }
  
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
  
  out_wide <- out %>%
    transmute(
      group = as.character(.data[[group_var]]),
      cat   = as.character(.data[[cat_var]]),
      p     = round(p, 1)
    ) %>%
    pivot_wider(names_from = group, values_from = p)
  
  for (nm in c("Urban", "Rural", "Total")) {
    if (!nm %in% names(out_wide)) out_wide[[nm]] <- NA_real_
  }
  
  out_wide %>%
    select(cat, Urban, Rural, Total)
}

# -----------------------------
# Build Table 2.1 working frame from Analysis_Ready codes
# -----------------------------
# derived_residence: 1=Urban, 2=Rural, 9=Missing
# hh_drinking_water_source: 11,12,13,21,31,32,41,42,51,61,71,81,91,92,96,99
# hh_other_water_source: same coding as above
# hh_water_location: 1 in dwelling, 2 in yard/plot, 3 elsewhere, 9 missing
# hh_water_time_trip: numeric-ish; 98/99 DK/missing; 0 includes on-premises style cases in your recode

df_t21 <- df %>%
  filter(u_household == 1) %>%
  mutate(
    res_grp = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    
    wsrc  = as.numeric(hh_drinking_water_source),
    owsrc = as.numeric(hh_other_water_source),
    wloc  = as.numeric(hh_water_location),
    wtime = as.numeric(hh_water_time_trip),
    
    # For bottled water classification, use other water source for comparability
    other_source_improved = case_when(
      owsrc %in% c(11, 12, 13, 21, 31, 41, 51, 61, 71, 92) ~ 1,
      owsrc %in% c(32, 42, 81, 96) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ---- Water detail labels
    water_cat = case_when(
      wsrc == 11 ~ "Piped into dwelling/yard/plot",
      wsrc == 12 ~ "Piped to neighbor",
      wsrc == 13 ~ "Public tap/standpipe",
      wsrc == 21 ~ "Tubewell/borehole",
      wsrc == 31 ~ "Protected dug well",
      wsrc == 41 ~ "Protected spring",
      wsrc == 51 ~ "Rainwater",
      wsrc %in% c(61, 71) ~ "Tanker truck/cart with small tank",
      wsrc == 91 ~ "Bottled water",
      wsrc == 92 ~ "Sachet water",
      wsrc == 32 ~ "Unprotected dug well",
      wsrc == 42 ~ "Unprotected spring",
      wsrc == 96 ~ "Other",
      wsrc == 81 ~ "Surface water",
      TRUE ~ NA_character_
    ),
    
    # ---- Water group labels
    water_group = case_when(
      wsrc %in% c(11, 12, 13, 21, 31, 41, 51, 61, 71, 92) ~ "Improved source",
      wsrc == 91 & other_source_improved == 1 ~ "Improved source",
      wsrc == 91 & other_source_improved == 0 ~ "Unimproved source",
      wsrc %in% c(32, 42, 96) ~ "Unimproved source",
      wsrc == 81 ~ "Surface water",
      TRUE ~ NA_character_
    ),
    
    # ---- Time to obtain drinking water (round trip)
    # Footnote-aligned: includes piped to neighbour and zero-minute round trip
    time_cat = case_when(
      wsrc == 12 ~ "Water on premises¹",
      wloc %in% c(1, 2) ~ "Water on premises¹",
      wtime == 0 ~ "Water on premises¹",
      wtime %in% c(98, 99) ~ "Don't know",
      is.na(wtime) ~ NA_character_,
      wtime <= 30 ~ "30 minutes or less",
      wtime > 30  ~ "More than 30 minutes",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(res_grp))

# Total group duplication
df_t21_tot <- df_t21 %>% mutate(res_grp = "Total")

# -----------------------------
# Compute wide distributions
# -----------------------------
water_group_wide  <- dist_wide(bind_rows(df_t21, df_t21_tot), "res_grp", "water_group", wvar = wvar_use)
water_detail_wide <- dist_wide(bind_rows(df_t21, df_t21_tot), "res_grp", "water_cat",   wvar = wvar_use)
time_wide         <- dist_wide(bind_rows(df_t21, df_t21_tot), "res_grp", "time_cat",    wvar = wvar_use)

# Unweighted Ns
n_counts <- df_t21 %>%
  count(res_grp, name = "N") %>%
  bind_rows(tibble(res_grp = "Total", N = sum(.$N)))

# -----------------------------
# Build the output sheet skeleton
# -----------------------------
rows <- c(
  "Characteristic",
  "Source of drinking water",
  "",
  "Improved source",
  "  Piped into dwelling/yard/plot",
  "  Piped to neighbor",
  "  Public tap/standpipe",
  "  Tubewell/borehole",
  "  Protected dug well",
  "  Protected spring",
  "  Rainwater",
  "  Tanker truck/cart with small tank",
  "  Bottled water",
  "  Sachet water",
  "",
  "Unimproved source",
  "  Unprotected dug well",
  "  Unprotected spring",
  "  Other",
  "",
  "Surface water",
  "",
  "Total",
  "",
  "Time to obtain drinking water (round trip)",
  "  Water on premises¹",
  "  30 minutes or less",
  "  More than 30 minutes",
  "  Don't know",
  "",
  "Total",
  "",
  "Number of respondents",
  "",
  "¹Includes water piped to a neighbour and those reporting a round-trip collection time of zero minutes"
)

COL_LABEL <- 2; COL_URB <- 3; COL_RUR <- 4; COL_TOT <- 5
START_ROW <- 5

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
# Create workbook
# -----------------------------
wb <- createWorkbook()
addWorksheet(wb, SHEET_T21, gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
hdrStyle   <- createStyle(textDecoration = "bold")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
numStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(wb, SHEET_T21, "Table 2.1 Household drinking water (Sproxil respondents)", startRow = 2, startCol = 2)
addStyle(wb, SHEET_T21, titleStyle, rows = 2, cols = 2, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; respondent counts are unweighted."
} else {
  "NOTE: Unweighted percentages; respondent counts are unweighted."
}
writeData(wb, SHEET_T21, note_txt, startRow = 2, startCol = 7)
addStyle(wb, SHEET_T21, noteStyle, rows = 2, cols = 7, stack = TRUE)

writeData(wb, SHEET_T21, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)

writeData(wb, SHEET_T21, "Urban", startRow = START_ROW, startCol = COL_URB)
writeData(wb, SHEET_T21, "Rural", startRow = START_ROW, startCol = COL_RUR)
writeData(wb, SHEET_T21, "Total", startRow = START_ROW, startCol = COL_TOT)
addStyle(wb, SHEET_T21, hdrStyle, rows = START_ROW, cols = COL_LABEL:COL_TOT, gridExpand = TRUE, stack = TRUE)

setColWidths(wb, SHEET_T21, cols = 1, widths = 2)
setColWidths(wb, SHEET_T21, cols = COL_LABEL, widths = 55)
setColWidths(wb, SHEET_T21, cols = COL_URB:COL_TOT, widths = 12)

# -----------------------------
# Write values
# -----------------------------
for (lab in c("Improved source", "Unimproved source", "Surface water")) {
  r <- row_of(lab)
  ww <- water_group_wide %>% filter(cat == lab)
  write_urt(wb, SHEET_T21, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET_T21, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

water_detail_printed <- c(
  "Piped into dwelling/yard/plot", "Piped to neighbor", "Public tap/standpipe", "Tubewell/borehole",
  "Protected dug well", "Protected spring", "Rainwater", "Tanker truck/cart with small tank",
  "Bottled water", "Sachet water", "Unprotected dug well", "Unprotected spring", "Other", "Surface water"
)

for (lab in water_detail_printed) {
  r <- row_of(paste0("  ", lab))
  ww <- water_detail_wide %>% filter(cat == lab)
  write_urt(wb, SHEET_T21, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET_T21, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

for (i in which(rows == "Total")) {
  r <- START_ROW + i - 1
  write_urt(wb, SHEET_T21, r, 100.0, 100.0, 100.0)
  addStyle(wb, SHEET_T21, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

time_printed <- c("Water on premises¹", "30 minutes or less", "More than 30 minutes", "Don't know")
for (lab in time_printed) {
  r <- row_of(paste0("  ", lab))
  ww <- time_wide %>% filter(cat == lab)
  write_urt(wb, SHEET_T21, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET_T21, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

rN <- row_of("Number of respondents")
n_u <- n_counts %>% filter(res_grp == "Urban") %>% pull(N)
n_r <- n_counts %>% filter(res_grp == "Rural") %>% pull(N)
n_t <- n_counts %>% filter(res_grp == "Total") %>% pull(N)
write_urt(wb, SHEET_T21, rN, n_u, n_r, n_t)
addStyle(wb, SHEET_T21, intStyle, rows = rN, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.1 written correctly to: ", OUT_XLSX)