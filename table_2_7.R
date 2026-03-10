suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
})

INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
SHEET     <- "Table 2.7"
WVAR      <- "w_calibrated_trim"

df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.7 weighting active: ", use_weight)

# %Yes among valid 0/1 only
pct_yes <- function(dat, group_var, x, wvar = NULL) {
  d <- dat %>%
    filter(!is.na(.data[[group_var]]), !is.na(.data[[x]]), .data[[x]] %in% c(0, 1))
  
  if (!is.null(wvar)) d <- d %>% filter(!is.na(.data[[wvar]]))
  
  if (nrow(d) == 0) {
    return(tibble(!!group_var := character(), p = numeric()))
  }
  
  if (is.null(wvar)) {
    d %>%
      group_by(.data[[group_var]]) %>%
      summarise(p = 100 * mean(.data[[x]] == 1), .groups = "drop")
  } else {
    d %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        p = 100 * sum(.data[[wvar]] * (.data[[x]] == 1), na.rm = TRUE) /
          sum(.data[[wvar]], na.rm = TRUE),
        .groups = "drop"
      )
  }
}

# Build frame
df_t27 <- df %>%
  filter(u_household == 1) %>%
  mutate(
    res_grp = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(res_grp))

df_tot <- df_t27 %>% mutate(res_grp = "Total")

# Items in tabulation-plan order
items <- tibble::tribble(
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

out <- bind_rows(lapply(seq_len(nrow(items)), function(i) {
  lab <- items$label[i]
  v   <- items$var[i]
  
  tmp <- pct_yes(bind_rows(df_t27, df_tot), "res_grp", v, wvar = wvar_use) %>%
    tidyr::pivot_wider(names_from = res_grp, values_from = p)
  
  for (nm in c("Urban", "Rural", "Total")) {
    if (!nm %in% names(tmp)) tmp[[nm]] <- NA_real_
  }
  
  tmp %>%
    mutate(
      Urban = round(Urban, 1),
      Rural = round(Rural, 1),
      Total = round(Total, 1),
      Possession = lab
    ) %>%
    select(Possession, Urban, Rural, Total)
}))

n_counts <- df_t27 %>%
  count(res_grp, name = "N") %>%
  bind_rows(tibble(res_grp = "Total", N = sum(.$N)))

# Skeleton
rows <- c(
  "Possession",
  "Household effects",
  "   Radio",
  "   Television",
  "   Mobile phone",
  "   Non-mobile telephone",
  "   Computer",
  "   Refrigerator",
  "   Table",
  "   Chair",
  "   Bed",
  "   Sofa",
  "   Cupboard",
  "   Air conditioner",
  "   Electric iron",
  "   Generator",
  "   Fan",
  "",
  "Means of transport",
  "   Bicycle",
  "   Animal drawn cart",
  "   Motorcycle/scooter",
  "   Car/truck",
  "   Boat with a motor",
  "   Keke napep",
  "",
  "Ownership of agricultural land",
  "",
  "Ownership of farm animals¹",
  "",
  "Number",
  "",
  "¹Cows, bulls, other cattle, horses, donkeys, mules, goats, sheep, chickens, or other poultry"
)

COL_LABEL <- 2
COL_URB   <- 4
COL_RUR   <- 5
COL_TOT   <- 6
START_ROW <- 7

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
  if (!is.null(style)) addStyle(wb, sheet, style, rows = r, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)
  invisible(TRUE)
}

# Workbook
if (file.exists(OUT_XLSX)) wb <- loadWorkbook(OUT_XLSX) else wb <- createWorkbook()
if (SHEET %in% names(wb)) removeWorksheet(wb, SHEET)
addWorksheet(wb, SHEET, gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
hdrStyle   <- createStyle(textDecoration = "bold")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
numStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(wb, SHEET, "Table 2.7  Household possessions (Sproxil respondents)", startRow = 1, startCol = 1)
addStyle(wb, SHEET, titleStyle, rows = 1, cols = 1, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages weighted using calibrated trimmed weights; counts are unweighted."
} else {
  "NOTE: Unweighted percentages; counts are unweighted."
}
writeData(wb, SHEET, note_txt, startRow = 2, startCol = 1)
addStyle(wb, SHEET, noteStyle, rows = 2, cols = 1, stack = TRUE)

writeData(wb, SHEET, "Residence", startRow = 6, startCol = COL_URB)
writeData(wb, SHEET, "Urban", startRow = 7, startCol = COL_URB)
writeData(wb, SHEET, "Rural", startRow = 7, startCol = COL_RUR)
writeData(wb, SHEET, "Total", startRow = 7, startCol = COL_TOT)
addStyle(wb, SHEET, hdrStyle, rows = 6:7, cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET, cols = COL_LABEL, widths = 55)
setColWidths(wb, SHEET, cols = COL_URB:COL_TOT, widths = 12)

# Fill possession rows
for (i in seq_len(nrow(out))) {
  lab <- out$Possession[i]
  r <- row_of_exact(paste0("   ", lab))
  if (lab %in% c("Ownership of agricultural land", "Ownership of farm animals¹")) {
    r <- row_of_exact(lab)
  }
  write_urt(wb, SHEET, r, out$Urban[i], out$Rural[i], out$Total[i], numStyle)
}

# Number row
rN <- row_of_exact("Number")
n_u <- n_counts %>% filter(res_grp == "Urban") %>% pull(N)
n_r <- n_counts %>% filter(res_grp == "Rural") %>% pull(N)
n_t <- n_counts %>% filter(res_grp == "Total") %>% pull(N)
write_urt(wb, SHEET, rN, n_u, n_r, n_t, intStyle)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.7 written to: ", OUT_XLSX)