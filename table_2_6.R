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
SHEET     <- "Table 2.6"
WVAR      <- "w_calibrated_trim"

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.6 weighting active: ", use_weight)

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
    transmute(
      group = as.character(.data[[group_var]]),
      cat   = as.character(.data[[cat_var]]),
      p     = round(p, 1)
    )
  
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
# Build analysis frame
# -----------------------------
df_t26 <- df %>%
  filter(u_household == 1) %>%
  mutate(
    res_grp = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    elec = as.numeric(hh_has_electricity),
    stove = as.numeric(hh_cookstove_type),
    fuel  = as.numeric(hh_cookstove_fuel),
    
    # Electricity yes/no labels
    elec_cat = case_when(
      elec == 1 ~ "Yes",
      elec == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    
    # --- Cooking technology
    tech_cat = case_when(
      stove == 1  ~ "Electric stove",
      stove == 2  ~ "Solar cooker",
      stove == 3  ~ "LPG/cooking gas stove",
      stove == 4  ~ "Piped natural gas stove",
      stove == 5  ~ "Biogas stove",
      stove == 6  ~ "Kerosene stove/liquid fuel stove not using alcohol/ethanol",
      stove == 7  ~ "Manufactured solid fuel stove",
      stove == 8  ~ "Traditional solid fuel stove",
      stove == 9  ~ "Three stone stove/open fire",
      stove == 96 ~ "Other fuel",
      stove == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    ),
    
    tech_group = case_when(
      stove %in% c(1, 2, 3, 4, 5) ~ "Clean fuels and technologies",
      stove %in% c(6, 7, 8, 9, 96) ~ "Other fuels and technologies",
      stove == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    ),
    
    # --- Cooking fuel detail rows shown in your table
    fuel_cat = case_when(
      fuel == 4  ~ "Coal/lignite",
      fuel == 5  ~ "Charcoal",
      fuel == 6  ~ "Wood",
      fuel == 7  ~ "Straw/shrubs/grass",
      fuel == 8  ~ "Agricultural crop",
      fuel == 9  ~ "Animal dung/waste",
      fuel == 10 ~ "Processed biomass (pellets) or woodchips",
      fuel == 11 ~ "Garbage/plastic",
      fuel == 12 ~ "Sawdust",
      fuel == 2  ~ "Gasoline/diesel",
      fuel == 3  ~ "Kerosene",
      fuel == 96 ~ "Other fuel",
      fuel == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    ),
    
    # Group rows for the fuel section
    fuel_group = case_when(
      stove %in% c(1, 2, 3, 4, 5) | fuel == 1 ~ "Clean fuels and technologies",
      fuel %in% c(4,5,6,7,8,9,10,11,12) ~ "Solid fuels for cooking",
      fuel %in% c(2,3,96) ~ "Other fuel",
      fuel == 95 ~ "No food cooked in household",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(res_grp))

df_tot <- df_t26 %>% mutate(res_grp = "Total")

# -----------------------------
# Expected levels
# -----------------------------
elec_levels <- c("Yes", "No")

tech_group_levels <- c(
  "Clean fuels and technologies",
  "Other fuels and technologies",
  "No food cooked in household"
)

tech_detail_levels <- c(
  "Electric stove",
  "Solar cooker",
  "LPG/cooking gas stove",
  "Piped natural gas stove",
  "Biogas stove",
  "Kerosene stove/liquid fuel stove not using alcohol/ethanol",
  "Manufactured solid fuel stove",
  "Traditional solid fuel stove",
  "Three stone stove/open fire",
  "Other fuel",
  "No food cooked in household"
)

fuel_group_levels <- c(
  "Clean fuels and technologies",
  "Solid fuels for cooking",
  "Other fuel",
  "No food cooked in household"
)

fuel_solid_levels <- c(
  "Coal/lignite",
  "Charcoal",
  "Wood",
  "Straw/shrubs/grass",
  "Agricultural crop",
  "Animal dung/waste",
  "Processed biomass (pellets) or woodchips",
  "Garbage/plastic",
  "Sawdust"
)

fuel_other_levels <- c("Gasoline/diesel", "Kerosene", "Other fuel")

fuel_detail_levels <- c(fuel_solid_levels, fuel_other_levels, "No food cooked in household")

# -----------------------------
# Distributions
# -----------------------------
elec_wide <- dist_wide(
  bind_rows(df_t26, df_tot), "res_grp", "elec_cat",
  expected_levels = elec_levels, wvar = wvar_use
)

tech_group_wide <- dist_wide(
  bind_rows(df_t26, df_tot), "res_grp", "tech_group",
  expected_levels = tech_group_levels, wvar = wvar_use
)

tech_detail_wide <- dist_wide(
  bind_rows(df_t26, df_tot), "res_grp", "tech_cat",
  expected_levels = tech_detail_levels, wvar = wvar_use
)

fuel_group_wide <- dist_wide(
  bind_rows(df_t26, df_tot), "res_grp", "fuel_group",
  expected_levels = fuel_group_levels, wvar = wvar_use
)

fuel_detail_wide <- dist_wide(
  bind_rows(df_t26, df_tot), "res_grp", "fuel_cat",
  expected_levels = fuel_detail_levels, wvar = wvar_use
)

n_counts <- df_t26 %>%
  count(res_grp, name = "N") %>%
  bind_rows(tibble(res_grp = "Total", N = sum(.$N)))

# -----------------------------
# Skeleton
# -----------------------------
rows <- c(
  "Housing characteristic",
  "Electricity",
  "   Yes",
  "   No",
  "",
  "Total",
  "",
  "Main cooking technology",
  "   Clean fuels and technologies",
  "      Electric stove",
  "      Solar cooker",
  "      LPG/cooking gas stove",
  "      Piped natural gas stove",
  "      Biogas stove",
  "   Other fuels and technologies",
  "      Kerosene stove/liquid fuel stove not using alcohol/ethanol",
  "      Manufactured solid fuel stove",
  "      Traditional solid fuel stove",
  "      Three stone stove/open fire",
  "      Other fuel",
  "   No food cooked in household",
  "",
  "Total ",
  "",
  "Cooking fuel",
  "   Clean fuels and technologies",
  "   Solid fuels for cooking",
  "      Coal/lignite",
  "      Charcoal",
  "      Wood",
  "      Straw/shrubs/grass",
  "      Agricultural crop",
  "      Animal dung/waste",
  "      Processed biomass (pellets) or woodchips",
  "      Garbage/plastic",
  "      Sawdust",
  "   Other fuel",
  "      Gasoline/diesel",
  "      Kerosene",
  "      Other fuel",
  "   No food cooked in household",
  "",
  "Total  ",
  "",
  "Number of respondents",
  "",
  "LPG = Liquefied petroleum gas",
  "1Includes stoves/cookers using electricity, LPG/natural gas/biogas, solar, and alcohol/ethanol"
)

COL_LABEL <- 2
COL_URB   <- 4
COL_RUR   <- 5
COL_TOT   <- 6
START_ROW <- 7

row_of_in_section <- function(label, section_start, section_end) {
  s <- match(section_start, rows)
  e <- match(section_end, rows)
  idx <- which(rows == label & seq_along(rows) > s & seq_along(rows) < e)
  if (length(idx) == 0) return(NA_integer_)
  START_ROW + idx[1] - 1
}

row_of_exact <- function(label) {
  i <- match(label, rows)
  if (is.na(i)) return(NA_integer_)
  START_ROW + i - 1
}

write_urt <- function(wb, sheet, r, u, rr, t, style = NULL) {
  if (is.na(r)) return(invisible(FALSE))
  writeData(wb, sheet, u,  startRow = r, startCol = COL_URB, colNames = FALSE)
  writeData(wb, sheet, rr, startRow = r, startCol = COL_RUR, colNames = FALSE)
  writeData(wb, sheet, t,  startRow = r, startCol = COL_TOT, colNames = FALSE)
  if (!is.null(style)) {
    addStyle(wb, sheet, style, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
  }
  invisible(TRUE)
}

# -----------------------------
# Workbook write
# -----------------------------
if (file.exists(OUT_XLSX)) wb <- loadWorkbook(OUT_XLSX) else wb <- createWorkbook()
if (SHEET %in% names(wb)) removeWorksheet(wb, SHEET)
addWorksheet(wb, SHEET, gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
hdrStyle   <- createStyle(textDecoration = "bold")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
numStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(wb, SHEET, "Table 2.6  Household characteristics: Electricity, cooking (Sproxil respondents)", startRow = 1, startCol = 1)
addStyle(wb, SHEET, titleStyle, rows = 1, cols = 1, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; respondent counts are unweighted."
} else {
  "NOTE: Unweighted percentages; respondent counts are unweighted."
}
writeData(wb, SHEET, note_txt, startRow = 2, startCol = 1)
addStyle(wb, SHEET, noteStyle, rows = 2, cols = 1, stack = TRUE)

writeData(wb, SHEET, "Population", startRow = 6, startCol = COL_URB)
writeData(wb, SHEET, "Urban", startRow = 7, startCol = COL_URB)
writeData(wb, SHEET, "Rural", startRow = 7, startCol = COL_RUR)
writeData(wb, SHEET, "Total", startRow = 7, startCol = COL_TOT)
addStyle(wb, SHEET, hdrStyle, rows = 6:7, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET, cols = COL_LABEL, widths = 65)
setColWidths(wb, SHEET, cols = COL_URB:COL_TOT, widths = 12)

# ---- Electricity
for (lab in elec_levels) {
  r <- row_of_in_section(paste0("   ", lab), "Electricity", "Main cooking technology")
  ww <- elec_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total, numStyle)
}
rTot_e <- row_of_in_section("Total", "Electricity", "Main cooking technology")
write_urt(wb, SHEET, rTot_e, 100.0, 100.0, 100.0, numStyle)

# ---- Main cooking technology: group rows
for (g in tech_group_levels) {
  r <- row_of_in_section(paste0("   ", g), "Main cooking technology", "Cooking fuel")
  ww <- tech_group_wide %>% filter(cat == g)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total, numStyle)
}

# ---- Main cooking technology: detail rows
tech_detail_print_map <- c(
  "      Electric stove" = "Electric stove",
  "      Solar cooker" = "Solar cooker",
  "      LPG/cooking gas stove" = "LPG/cooking gas stove",
  "      Piped natural gas stove" = "Piped natural gas stove",
  "      Biogas stove" = "Biogas stove",
  "      Kerosene stove/liquid fuel stove not using alcohol/ethanol" = "Kerosene stove/liquid fuel stove not using alcohol/ethanol",
  "      Manufactured solid fuel stove" = "Manufactured solid fuel stove",
  "      Traditional solid fuel stove" = "Traditional solid fuel stove",
  "      Three stone stove/open fire" = "Three stone stove/open fire",
  "      Other fuel" = "Other fuel"
)

for (print_lab in names(tech_detail_print_map)) {
  r <- row_of_in_section(print_lab, "Main cooking technology", "Cooking fuel")
  key <- unname(tech_detail_print_map[print_lab])
  ww <- tech_detail_wide %>% filter(cat == key)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total, numStyle)
}

rTot_t <- row_of_in_section("Total ", "Main cooking technology", "Cooking fuel")
write_urt(wb, SHEET, rTot_t, 100.0, 100.0, 100.0, numStyle)

# ---- Cooking fuel: group rows
for (g in fuel_group_levels) {
  r <- row_of_in_section(paste0("   ", g), "Cooking fuel", "Number of respondents")
  ww <- fuel_group_wide %>% filter(cat == g)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total, numStyle)
}

# ---- Cooking fuel: detail rows
for (lab in fuel_solid_levels) {
  r <- row_of_in_section(paste0("      ", lab), "Cooking fuel", "Number of respondents")
  ww <- fuel_detail_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total, numStyle)
}

for (lab in fuel_other_levels) {
  r <- row_of_in_section(paste0("      ", lab), "Cooking fuel", "Number of respondents")
  ww <- fuel_detail_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total, numStyle)
}

# No food cooked row in fuel section
r_nfc <- row_of_in_section("   No food cooked in household", "Cooking fuel", "Number of respondents")
ww_nfc <- fuel_group_wide %>% filter(cat == "No food cooked in household")
write_urt(wb, SHEET, r_nfc, ww_nfc$Urban, ww_nfc$Rural, ww_nfc$Total, numStyle)

rTot_f <- row_of_in_section("Total  ", "Cooking fuel", "Number of respondents")
write_urt(wb, SHEET, rTot_f, 100.0, 100.0, 100.0, numStyle)

# ---- Number of respondents
rN <- row_of_exact("Number of respondents")
n_u <- n_counts %>% filter(res_grp == "Urban") %>% pull(N)
n_r <- n_counts %>% filter(res_grp == "Rural") %>% pull(N)
n_t <- n_counts %>% filter(res_grp == "Total") %>% pull(N)
write_urt(wb, SHEET, rN, n_u, n_r, n_t, intStyle)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.6 written to: ", OUT_XLSX)