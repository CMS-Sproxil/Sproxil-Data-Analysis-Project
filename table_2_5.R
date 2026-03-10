suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

# -----------------------------
# CONFIG
# -----------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
SHEET     <- "Table 2.5"
WVAR      <- "w_calibrated_trim"   # used only if present & non-missing

# -----------------------------
# Load
# -----------------------------
df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.5 weighting active: ", use_weight)

# -----------------------------
# Helper: wide % distribution with optional expected levels
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
    tidyr::pivot_wider(names_from = group, values_from = p, values_fill = 0.0)
  
  for (nm in c("Urban", "Rural", "Total")) {
    if (!nm %in% names(out_wide)) out_wide[[nm]] <- 0.0
  }
  
  out_wide %>%
    select(cat, Urban, Rural, Total)
}

# -----------------------------
# Build analysis frame
# -----------------------------
df_t25 <- df %>%
  filter(u_household == 1) %>%
  mutate(
    res_grp = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    
    floor = as.numeric(hh_floor_material),
    roof  = as.numeric(hh_roof_material),
    wall  = as.numeric(hh_wall_material),
    
    # Script 04 already parsed this to numeric
    rooms_num = suppressWarnings(as.numeric(demo_hh_sleeping_rooms)),
    
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
      rooms_num == 1 ~ "One ",
      rooms_num == 2 ~ "Two ",
      rooms_num >= 3 ~ "Three or more ",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(res_grp))

# Total stratum
df_t25_tot <- df_t25 %>% mutate(res_grp = "Total")

# -----------------------------
# Expected row levels
# -----------------------------
floor_levels <- c(
  "Earth/sand","Dung","Wood planks","Palm/bamboo","Parquet or polished wood",
  "Vinyl or asphalt strips","Ceramic tiles","Cement","Carpet","Other"
)

roof_levels <- c(
  "No roof","Thatch / Palm leaf","Grass","Rustic mat","Palm/bamboo","Wood planks",
  "Cardboard","Metal/zinc","Wood","Calamine/cement fibre","Ceramic tiles","Cement",
  "Roofing shingles","Asbestos","Other"
)

wall_levels <- c(
  "No walls","Cane/palm/trunks","Dirt","Bamboo with mud","Stone with mud",
  "Uncovered adobe","Plywood","Cardboard","Reused wood","Cement",
  "Stone with lime/cement","Bricks","Cement blocks","Covered adobe",
  "Wood planks/shingles","Other"
)

rooms_levels <- c("One ","Two ","Three or more ")

# -----------------------------
# Distributions
# -----------------------------
floor_wide <- dist_wide(bind_rows(df_t25, df_t25_tot), "res_grp", "floor_cat", expected_levels = floor_levels, wvar = wvar_use)
roof_wide  <- dist_wide(bind_rows(df_t25, df_t25_tot), "res_grp", "roof_cat",  expected_levels = roof_levels,  wvar = wvar_use)
wall_wide  <- dist_wide(bind_rows(df_t25, df_t25_tot), "res_grp", "wall_cat",  expected_levels = wall_levels,  wvar = wvar_use)
rooms_wide <- dist_wide(bind_rows(df_t25, df_t25_tot), "res_grp", "rooms_cat", expected_levels = rooms_levels, wvar = wvar_use)

# Ns (unweighted)
n_counts <- df_t25 %>%
  count(res_grp, name = "N") %>%
  bind_rows(tibble(res_grp = "Total", N = sum(.$N)))

# -----------------------------
# Build sheet skeleton
# -----------------------------
rows <- c(
  "Housing characteristic",
  "",
  "Flooring material",
  "Earth/sand",
  "Dung",
  "Wood planks",
  "Palm/bamboo",
  "Parquet or polished wood",
  "Vinyl or asphalt strips",
  "Ceramic tiles",
  "Cement",
  "Carpet",
  "Other",
  "",
  "Total ",
  "",
  "Roof material",
  "No roof",
  "Thatch / Palm leaf",
  "Grass",
  "Rustic mat",
  "Palm/bamboo",
  "Wood planks",
  "Cardboard",
  "Metal/zinc",
  "Wood",
  "Calamine/cement fibre",
  "Ceramic tiles",
  "Cement",
  "Roofing shingles",
  "Asbestos",
  "Other",
  "",
  "Total ",
  "",
  "Exterior wall material",
  "No walls",
  "Cane/palm/trunks",
  "Dirt",
  "Bamboo with mud",
  "Stone with mud",
  "Uncovered adobe",
  "Plywood",
  "Cardboard",
  "Reused wood",
  "Cement",
  "Stone with lime/cement",
  "Bricks",
  "Cement blocks",
  "Covered adobe",
  "Wood planks/shingles",
  "Other",
  "",
  "Total ",
  "",
  "Rooms used for sleeping",
  "One ",
  "Two ",
  "Three or more ",
  "",
  "Total ",
  "",
  "Number of respondents"
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

row_of_in_section <- function(label, section_start_label, section_end_label, occurrence = 1L) {
  s <- match(section_start_label, rows)
  e <- match(section_end_label, rows)
  if (is.na(s) || is.na(e) || e <= s) stop("Bad section bounds for: ", section_start_label)
  
  idx <- which(rows == label & seq_along(rows) > s & seq_along(rows) < e)
  if (length(idx) < occurrence) {
    warning("Label not found in section: ", label, " (", section_start_label, " -> ", section_end_label, ")")
    return(NA_integer_)
  }
  START_ROW + idx[occurrence] - 1
}

write_urt <- function(wb, sheet, r, u, rr, t) {
  if (is.na(r)) return(invisible(FALSE))
  writeData(wb, sheet, u,  startRow = r, startCol = COL_URB, colNames = FALSE)
  writeData(wb, sheet, rr, startRow = r, startCol = COL_RUR, colNames = FALSE)
  writeData(wb, sheet, t,  startRow = r, startCol = COL_TOT, colNames = FALSE)
  invisible(TRUE)
}

# -----------------------------
# Create/overwrite sheet
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

writeData(wb, SHEET, "Table 2.5  Household characteristics: Construction materials and rooms used for sleeping (Sproxil respondents)", startRow = 3, startCol = 2)
addStyle(wb, SHEET, titleStyle, rows = 3, cols = 2, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; respondent counts are unweighted."
} else {
  "NOTE: Unweighted percentages; respondent counts are unweighted."
}
writeData(wb, SHEET, note_txt, startRow = 4, startCol = 2)
addStyle(wb, SHEET, noteStyle, rows = 4, cols = 2, stack = TRUE)

# headers
writeData(wb, SHEET, "Population", startRow = 6, startCol = COL_URB)
writeData(wb, SHEET, "Urban", startRow = 7, startCol = COL_URB)
writeData(wb, SHEET, "Rural", startRow = 7, startCol = COL_RUR)
writeData(wb, SHEET, "Total", startRow = 7, startCol = COL_TOT)
addStyle(wb, SHEET, hdrStyle, rows = 6:7, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)

# row labels
writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)

setColWidths(wb, SHEET, cols = 1, widths = 2)
setColWidths(wb, SHEET, cols = COL_LABEL, widths = 60)
setColWidths(wb, SHEET, cols = COL_URB:COL_TOT, widths = 12)

# -----------------------------
# Write blocks
# -----------------------------
# Flooring
for (lab in floor_levels) {
  r <- row_of_in_section(lab, "Flooring material", "Roof material")
  ww <- floor_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

# Roof
for (lab in roof_levels) {
  r <- row_of_in_section(lab, "Roof material", "Exterior wall material")
  ww <- roof_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

# Wall
for (lab in wall_levels) {
  r <- row_of_in_section(lab, "Exterior wall material", "Rooms used for sleeping")
  ww <- wall_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

# Rooms
for (lab in rooms_levels) {
  r <- row_of_in_section(lab, "Rooms used for sleeping", "Number of respondents")
  ww <- rooms_wide %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total)
  addStyle(wb, SHEET, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

# All "Total " rows = 100.0
for (i in which(rows == "Total ")) {
  r <- START_ROW + i - 1
  write_urt(wb, SHEET, r, 100.0, 100.0, 100.0)
  addStyle(wb, SHEET, numStyle, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
}

# Number of respondents
rN <- row_of("Number of respondents")
n_u <- n_counts %>% filter(res_grp == "Urban") %>% pull(N)
n_r <- n_counts %>% filter(res_grp == "Rural") %>% pull(N)
n_t <- n_counts %>% filter(res_grp == "Total") %>% pull(N)

write_urt(wb, SHEET, rN, n_u, n_r, n_t)
addStyle(wb, SHEET, intStyle, rows = rN, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)

# Save
saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.5 written to: ", OUT_XLSX)