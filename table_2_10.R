suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
SHEET     <- "Table 2.10"
WVAR      <- "w_calibrated_trim"

df <- readRDS(INPUT_RDS)
use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
wvar_use <- if (use_weight) WVAR else NULL
message("Table 2.10 weighting active: ", use_weight)

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

wmean <- function(x, w = NULL) {
  ok <- !is.na(x)
  if (!is.null(w)) ok <- ok & !is.na(w)
  x <- x[ok]
  if (length(x) == 0) return(NA_real_)
  if (is.null(w)) return(mean(x))
  w <- w[ok]
  sum(w * x) / sum(w)
}

df_t210 <- df %>%
  filter(u_household == 1) %>%
  mutate(
    res_grp = case_when(
      as.numeric(derived_residence) == 1 ~ "Urban",
      as.numeric(derived_residence) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    hhsize = dplyr::coalesce(
      suppressWarnings(as.numeric(hh_total_persons_usually_v3)),
      suppressWarnings(as.numeric(hh_total_persons_v1)),
      suppressWarnings(as.numeric(hh_total_persons_v2))
    ),
    hhsize_cat = case_when(
      is.na(hhsize) ~ NA_character_,
      hhsize >= 9 ~ "9+",
      hhsize %in% 1:8 ~ as.character(hhsize),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(res_grp), !is.na(hhsize_cat), !is.na(hhsize))

df_tot <- df_t210 %>% mutate(res_grp = "Total")

levels_hh <- c(as.character(1:8), "9+")

dist <- dist_wide(
  bind_rows(df_t210, df_tot),
  "res_grp",
  "hhsize_cat",
  expected_levels = levels_hh,
  wvar = wvar_use
)

mean_u <- wmean(
  df_t210$hhsize[df_t210$res_grp == "Urban"],
  if (!is.null(wvar_use)) df_t210[[wvar_use]][df_t210$res_grp == "Urban"] else NULL
)
mean_r <- wmean(
  df_t210$hhsize[df_t210$res_grp == "Rural"],
  if (!is.null(wvar_use)) df_t210[[wvar_use]][df_t210$res_grp == "Rural"] else NULL
)
mean_t <- wmean(
  df_t210$hhsize,
  if (!is.null(wvar_use)) df_t210[[wvar_use]] else NULL
)

# Unweighted household counts
n_u <- sum(df_t210$res_grp == "Urban", na.rm = TRUE)
n_r <- sum(df_t210$res_grp == "Rural", na.rm = TRUE)
n_t <- nrow(df_t210)

rows <- c(
  "Characteristic",
  "",
  "Number of usual members",
  "1","2","3","4","5","6","7","8","9+",
  "",
  "Total",
  "Mean size of households",
  "",
  "Number of households",
  "",
  "Note: Table is based on de jure household members, i.e., usual residents."
)

COL_LABEL <- 2
COL_URB <- 3
COL_RUR <- 4
COL_TOT <- 5
START_ROW <- 9

row_of <- function(label) {
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

if (file.exists(OUT_XLSX)) wb <- loadWorkbook(OUT_XLSX) else wb <- createWorkbook()
if (SHEET %in% names(wb)) removeWorksheet(wb, SHEET)
addWorksheet(wb, SHEET, gridLines = FALSE)

titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
hdrStyle   <- createStyle(textDecoration = "bold")
noteStyle  <- createStyle(fontSize = 9, textDecoration = "italic")
numStyle   <- createStyle(numFmt = "0.0")
intStyle   <- createStyle(numFmt = "0")

writeData(wb, SHEET, "Table 2.10  Household composition (Sproxil respondents)", startRow = 2, startCol = 2)
addStyle(wb, SHEET, titleStyle, rows = 2, cols = 2, stack = TRUE)

note_txt <- if (use_weight) {
  "NOTE: Percentages and mean household size weighted using calibrated trimmed weights; number of households is unweighted."
} else {
  "NOTE: Unweighted percentages and mean household size; number of households is unweighted."
}
writeData(wb, SHEET, note_txt, startRow = 3, startCol = 2)
addStyle(wb, SHEET, noteStyle, rows = 3, cols = 2, stack = TRUE)

writeData(wb, SHEET, "Residence", startRow = START_ROW - 2, startCol = COL_URB)
writeData(wb, SHEET, c("Urban", "Rural", "Total"), startRow = START_ROW - 1, startCol = COL_URB, colNames = FALSE)
addStyle(wb, SHEET, hdrStyle, rows = (START_ROW - 2):(START_ROW - 1), cols = COL_URB:COL_TOT, gridExpand = TRUE, stack = TRUE)

writeData(wb, SHEET, rows, startRow = START_ROW, startCol = COL_LABEL, colNames = FALSE)
setColWidths(wb, SHEET, cols = COL_LABEL, widths = 45)
setColWidths(wb, SHEET, cols = COL_URB:COL_TOT, widths = 12)

for (lab in levels_hh) {
  r <- row_of(lab)
  ww <- dist %>% filter(cat == lab)
  write_urt(wb, SHEET, r, ww$Urban, ww$Rural, ww$Total, numStyle)
}

rTot <- row_of("Total")
write_urt(wb, SHEET, rTot, 100.0, 100.0, 100.0, numStyle)

rM <- row_of("Mean size of households")
write_urt(wb, SHEET, rM, round(mean_u, 2), round(mean_r, 2), round(mean_t, 2), numStyle)

rN <- row_of("Number of households")
write_urt(wb, SHEET, rN, n_u, n_r, n_t, intStyle)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ Table 2.10 written to: ", OUT_XLSX)