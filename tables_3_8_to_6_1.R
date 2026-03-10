# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  09_Tables_3_8_to_6_1_Tabulation_Engine_REVISED.R
# PURPOSE: Generate Tables 3.8 through 6.1 into Excel output workbook
# INPUT:   Sproxil_Analysis_Ready.rds
# OUTPUT:  Sproxil_Descriptive_Tables_Output.xlsx
# NOTES:
# - Percentages use calibrated weights if WVAR exists and is non-missing
# - Count columns are unweighted unless explicitly stated otherwise
# - Multi-response source/knowledge/drug tables do NOT sum to 100
# - Table 6.1 is respondent-level (NOT women-only)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(openxlsx)
  library(tibble)
})

# -----------------------------
# CONFIG
# -----------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready.rds"
OUT_XLSX  <- "Sproxil_Descriptive_Tables_Output.xlsx"
WVAR      <- "w_calibrated_trim"

# -----------------------------
# LOAD
# -----------------------------
df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
w <- if (use_weight) WVAR else NULL
message("Weighting active: ", use_weight)

note_txt <- if (use_weight) {
  "NOTE: Weighted percentages use calibrated trimmed weights; count columns are unweighted."
} else {
  "NOTE: Unweighted."
}

# -----------------------------
# LABELS / MAPPINGS
# -----------------------------
res_lab  <- c(`1`="Urban", `2`="Rural")
zone_lab <- c(`1`="Region 1", `2`="Region 2", `3`="Region 3", `4`="Region 4", `5`="Region 5", `6`="Region 6")
wq_lab   <- c(`1`="Lowest", `2`="Second", `3`="Middle", `4`="Fourth", `5`="Highest")
edu_lab  <- c(`1`="No education", `2`="Primary", `3`="Secondary", `4`="More than secondary")
agew_lab <- c(`1`="15-19", `2`="20-24", `3`="25-29", `4`="30-34", `5`="35-39", `6`="40-44", `7`="45-49")

state_names <- c(
  `1`="Abia", `2`="Adamawa", `3`="Akwa Ibom", `4`="Anambra", `5`="Bauchi", `6`="Bayelsa",
  `7`="Benue", `8`="Borno", `9`="Cross River", `10`="Delta", `11`="Ebonyi", `12`="Edo",
  `13`="Ekiti", `14`="Enugu", `15`="FCT-Abuja", `16`="Gombe", `17`="Imo", `18`="Jigawa",
  `19`="Kaduna", `20`="Kano", `21`="Katsina", `22`="Kebbi", `23`="Kogi", `24`="Kwara",
  `25`="Lagos", `26`="Nasarawa", `27`="Niger", `28`="Ogun", `29`="Ondo", `30`="Osun",
  `31`="Oyo", `32`="Plateau", `33`="Rivers", `34`="Sokoto", `35`="Taraba", `36`="Yobe",
  `37`="Zamfara"
)

zone_header <- c(
  `1`="North Central", `2`="North East", `3`="North West",
  `4`="South East", `5`="South South", `6`="South West"
)

sex_lab <- c(`1`="Male", `2`="Female")

# -----------------------------
# HELPERS
# -----------------------------
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

need_cols <- function(cols, dat = df) {
  miss <- setdiff(cols, names(dat))
  if (length(miss) > 0) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }
}

weighted_mean_safe <- function(x, wts = NULL) {
  if (is.null(wts)) {
    if (all(is.na(x))) return(NA_real_)
    return(mean(x, na.rm = TRUE))
  }
  ok <- !is.na(x) & !is.na(wts)
  if (!any(ok)) return(NA_real_)
  if (sum(wts[ok]) <= 0) return(NA_real_)
  sum(wts[ok] * x[ok]) / sum(wts[ok])
}

pct_from_binary <- function(x, wts = NULL, digits = 1) {
  out <- 100 * weighted_mean_safe(x, wts)
  round(out, digits)
}

pct_yes01 <- function(x, wts = NULL, digits = 1) {
  xx <- case_when(
    is.na(x) ~ NA_real_,
    x == 1 ~ 1,
    x == 0 ~ 0,
    TRUE ~ NA_real_
  )
  pct_from_binary(xx, wts, digits)
}

unw_n <- function(dat) nrow(dat)

add_blank_row <- function(dfout) {
  bind_rows(dfout, as_tibble(setNames(as.list(rep(NA, ncol(dfout))), names(dfout))))
}

make_block <- function(dat, header, gvar, levels_vec, lab_map, calc_fun) {
  rows <- lapply(levels_vec, function(k) {
    dd <- dat %>% filter(!is.na(.data[[gvar]]), .data[[gvar]] == k)
    calc_fun(dd, unname(lab_map[as.character(k)]))
  }) %>% bind_rows()
  
  out <- bind_rows(
    tibble(`Background characteristic` = header),
    rows
  )
  add_blank_row(out)
}

make_block_char <- function(dat, header, gvar, levels_vec, lab_map, calc_fun) {
  rows <- lapply(levels_vec, function(k) {
    dd <- dat %>% filter(!is.na(.data[[gvar]]), .data[[gvar]] == k)
    calc_fun(dd, unname(lab_map[as.character(k)]))
  }) %>% bind_rows()
  
  out <- bind_rows(
    tibble(`Background characteristic` = header),
    rows
  )
  add_blank_row(out)
}

make_state_table <- function(dat, calc_fun, row_name = "State") {
  dat2 <- dat %>%
    filter(!is.na(demo_state_num), demo_state_num %in% 1:37,
           !is.na(derived_zone), derived_zone %in% 1:6)
  
  out <- tibble()
  
  for (z in 1:6) {
    out <- bind_rows(out, tibble(!!row_name := zone_header[as.character(z)]))
    st_in_z <- dat2 %>%
      filter(derived_zone == z) %>%
      distinct(demo_state_num) %>%
      pull(demo_state_num) %>%
      sort()
    
    for (s in st_in_z) {
      dd <- dat2 %>% filter(demo_state_num == s)
      out <- bind_rows(out, calc_fun(dd, state_names[as.character(s)], row_name = row_name))
    }
    
    out <- bind_rows(out, tibble(!!row_name := ""))
  }
  
  out
}

write_table_sheet <- function(wb, sheet, title, note, dfout, footnote = NULL) {
  if (sheet %in% names(wb)) removeWorksheet(wb, sheet)
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  titleStyle <- createStyle(textDecoration = "bold", fontSize = 12)
  noteStyle  <- createStyle(textDecoration = "italic", fontSize = 9)
  hdrStyle   <- createStyle(textDecoration = "bold")
  num1Style  <- createStyle(numFmt = "0.0")
  intStyle   <- createStyle(numFmt = "0")
  
  writeData(wb, sheet, title, startRow = 2, startCol = 2)
  addStyle(wb, sheet, titleStyle, rows = 2, cols = 2, stack = TRUE)
  
  writeData(wb, sheet, note, startRow = 3, startCol = 2)
  addStyle(wb, sheet, noteStyle, rows = 3, cols = 2, stack = TRUE)
  
  writeData(wb, sheet, dfout, startRow = 5, startCol = 2, colNames = TRUE)
  addStyle(wb, sheet, hdrStyle, rows = 5, cols = 2:(ncol(dfout) + 1), gridExpand = TRUE, stack = TRUE)
  
  num_cols <- which(sapply(dfout, is.numeric))
  if (length(num_cols) > 0) {
    addStyle(
      wb, sheet, num1Style,
      rows = 6:(6 + nrow(dfout) - 1),
      cols = 1 + num_cols,
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  int_cols <- which(grepl("^Number", names(dfout)))
  if (length(int_cols) > 0) {
    addStyle(
      wb, sheet, intStyle,
      rows = 6:(6 + nrow(dfout) - 1),
      cols = 1 + int_cols,
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  if (!is.null(footnote)) {
    writeData(wb, sheet, footnote, startRow = 6 + nrow(dfout) + 2, startCol = 2)
    addStyle(wb, sheet, noteStyle, rows = 6 + nrow(dfout) + 2, cols = 2, stack = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 2, widths = 36)
  setColWidths(wb, sheet, cols = 3:(ncol(dfout) + 1), widths = 18)
}

# -----------------------------
# STANDARDISE KEY STRATIFIERS
# -----------------------------
df <- df %>%
  mutate(
    demo_state_num          = if ("demo_state_num" %in% names(.)) to_num(demo_state_num) else to_num(demo_state),
    derived_zone            = to_num(derived_zone),
    derived_residence       = to_num(derived_residence),
    derived_wealth_quintile = to_num(derived_wealth_quintile),
    derived_edu_cat         = to_num(derived_edu_cat),
    derived_age_group_w     = to_num(derived_age_group_w),
    demo_gender_num         = if ("demo_gender_num" %in% names(.)) to_num(demo_gender_num) else to_num(demo_gender),
    u_women_15_49           = to_num(u_women_15_49),
    u_recent_birth          = to_num(u_recent_birth),
    u_child_youngest        = to_num(u_child_youngest),
    u_child_fever           = to_num(u_child_fever)
  )

# ============================================================
# TABLE 3.8 — Main reason mosquito net was not used last night
# ============================================================
need_cols(c(
  "u_household", "derived_hh_has_any_net", "derived_net_use_any_last_night",
  "prev_net_not_used_reason", "derived_hh_has_itn", "prev_num_mosquito_nets"
))

net_reason_map <- function(code) {
  case_when(
    code == 3 ~ "Too hot",
    code == 4 ~ "Don't like smell",
    code == 9 ~ "Usual users did not sleep here last night",
    code %in% c(1, 2) ~ "No mosquitoes / no malaria",
    code %in% c(6, 7, 8, 10, 11, 96, 98, 99) ~ "Other / don't know",
    TRUE ~ "Other / don't know"
  )
}

t38_dat <- df %>%
  filter(to_num(u_household) == 1, to_num(derived_hh_has_any_net) == 1) %>%
  mutate(
    used_last = to_num(derived_net_use_any_last_night),
    reason_code = to_num(prev_net_not_used_reason),
    reason = case_when(
      used_last == 0 ~ net_reason_map(reason_code),
      TRUE ~ NA_character_
    ),
    net_type = case_when(
      to_num(derived_hh_has_itn) == 1 ~ "ITN households",
      to_num(derived_hh_has_itn) == 0 ~ "Other net households",
      TRUE ~ "Unknown net type"
    ),
    nets_n = to_num(prev_num_mosquito_nets),
    nets_n = ifelse(nets_n %in% c(98, 99), NA_real_, nets_n)
  )

calc_t38 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  
  den <- dat %>% filter(used_last == 0)
  wts_den <- if (!is.null(w)) den[[w]] else NULL
  
  tibble(
    `Background characteristic` = rowlab,
    `% households with nets not used last night` = pct_from_binary(as.numeric(dat$used_last == 0), wts),
    `Too hot` = pct_from_binary(as.numeric(den$reason == "Too hot"), wts_den),
    `Don't like smell` = pct_from_binary(as.numeric(den$reason == "Don't like smell"), wts_den),
    `Usual users did not sleep here last night` = pct_from_binary(as.numeric(den$reason == "Usual users did not sleep here last night"), wts_den),
    `No mosquitoes / no malaria` = pct_from_binary(as.numeric(den$reason == "No mosquitoes / no malaria"), wts_den),
    `Other / don't know` = pct_from_binary(as.numeric(den$reason == "Other / don't know"), wts_den),
    `Total` = ifelse(nrow(den) > 0, 100.0, NA_real_),
    `Number of net-owning households` = unw_n(dat)
  )
}

t38 <- bind_rows(
  tibble(`Background characteristic` = "Type of net"),
  calc_t38(t38_dat %>% filter(net_type == "ITN households"), "ITN households"),
  calc_t38(t38_dat %>% filter(net_type == "Other net households"), "Other net households"),
  calc_t38(t38_dat, "All net-owning households"),
  tibble(`Background characteristic` = ""),
  make_block(t38_dat, "Residence", "derived_residence", 1:2, res_lab, calc_t38),
  make_block(t38_dat, "Region", "derived_zone", 1:6, zone_lab, calc_t38),
  make_block(t38_dat, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t38),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t38(t38_dat, "Total") %>% select(-`Background characteristic`))
)

# ============================================================
# TABLE 3.9 — Antenatal care among women with recent birth proxy
# ============================================================
need_cols(c(
  "u_recent_birth", "women_anc_seen", "anc_prov_doc", "anc_prov_nurse", "anc_prov_aux",
  "anc_prov_chew", "anc_prov_tba", "anc_prov_other", "derived_anc_skilled"
))

t39_dat <- df %>% filter(u_recent_birth == 1)

provider_hi <- function(dat) {
  doc  <- to_num(dat$anc_prov_doc) == 1
  nur  <- to_num(dat$anc_prov_nurse) == 1
  aux  <- to_num(dat$anc_prov_aux) == 1
  chew <- to_num(dat$anc_prov_chew) == 1
  tba  <- to_num(dat$anc_prov_tba) == 1
  oth  <- to_num(dat$anc_prov_other) == 1
  
  case_when(
    doc  ~ "Doctor",
    nur  ~ "Nurse/midwife",
    aux  ~ "Auxiliary midwife",
    chew ~ "Community health worker",
    tba  ~ "Traditional birth attendant",
    oth  ~ "Other",
    TRUE ~ "Unknown / not stated"
  )
}

calc_t39 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  
  anc_seen <- to_num(dat$women_anc_seen)
  any_anc  <- pct_yes01(anc_seen, wts)
  skilled  <- pct_from_binary(as.numeric(to_num(dat$derived_anc_skilled) == 1), wts)
  
  anc_seek <- dat %>% filter(to_num(women_anc_seen) == 1)
  wts2 <- if (!is.null(w)) anc_seek[[w]] else NULL
  prov <- provider_hi(anc_seek)
  
  tibble(
    `Background characteristic` = rowlab,
    `% received ANC` = any_anc,
    `% received ANC from skilled provider` = skilled,
    `Doctor` = pct_from_binary(as.numeric(prov == "Doctor"), wts2),
    `Nurse/midwife` = pct_from_binary(as.numeric(prov == "Nurse/midwife"), wts2),
    `Auxiliary midwife` = pct_from_binary(as.numeric(prov == "Auxiliary midwife"), wts2),
    `Community health worker` = pct_from_binary(as.numeric(prov == "Community health worker"), wts2),
    `Traditional birth attendant` = pct_from_binary(as.numeric(prov == "Traditional birth attendant"), wts2),
    `Other` = pct_from_binary(as.numeric(prov == "Other"), wts2),
    `Number of women with recent birth proxy` = unw_n(dat)
  )
}

t39 <- bind_rows(
  make_block(t39_dat, "Residence", "derived_residence", 1:2, res_lab, calc_t39),
  make_block(t39_dat, "Region", "derived_zone", 1:6, zone_lab, calc_t39),
  make_block(t39_dat, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t39),
  make_block(t39_dat, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t39),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t39(t39_dat, "Total") %>% select(-`Background characteristic`))
)

t39_states <- make_state_table(
  t39_dat,
  function(dd, lab, row_name = "State") {
    calc_t39(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t39(t39_dat, "Total") %>% select(-`Background characteristic`))
  )

# ============================================================
# TABLE 3.10 — ANC visits and timing of first visit
# ============================================================
need_cols(c("u_recent_birth", "women_anc_total_visits", "women_anc_first_visit_month", "women_anc_seen", "derived_anc_4plus"))

t310_dat <- t39_dat %>%
  mutate(
    anc_vis = to_num(women_anc_total_visits),
    anc_first = to_num(women_anc_first_visit_month),
    anc_seen = to_num(women_anc_seen),
    
    anc_vis_cat = case_when(
      anc_seen == 0 ~ "None",
      anc_vis %in% c(98, 99) ~ "Don't know",
      anc_vis == 1 ~ "1",
      anc_vis == 2 ~ "2",
      anc_vis == 3 ~ "3",
      anc_vis >= 4 & anc_vis <= 7 ~ "4-7",
      anc_vis >= 8 ~ "8+",
      TRUE ~ "Don't know"
    ),
    
    anc_first_cat = case_when(
      anc_seen == 0 ~ "No ANC",
      anc_first %in% c(98, 99) ~ "Don't know",
      anc_first >= 0 & anc_first <= 3 ~ "<4 months",
      anc_first >= 4 & anc_first <= 7 ~ "4-7 months",
      anc_first >= 8 ~ "8+ months",
      TRUE ~ "Don't know"
    )
  )

calc_t310 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  sh <- function(var, lvl) pct_from_binary(as.numeric(dat[[var]] == lvl), wts)
  
  tibble(
    `Background characteristic` = rowlab,
    `None` = sh("anc_vis_cat", "None"),
    `1` = sh("anc_vis_cat", "1"),
    `2` = sh("anc_vis_cat", "2"),
    `3` = sh("anc_vis_cat", "3"),
    `4-7` = sh("anc_vis_cat", "4-7"),
    `8+` = sh("anc_vis_cat", "8+"),
    `Don't know (visits)` = sh("anc_vis_cat", "Don't know"),
    `4+ ANC visits` = pct_from_binary(as.numeric(to_num(dat$derived_anc_4plus) == 1), wts),
    `No ANC` = sh("anc_first_cat", "No ANC"),
    `<4 months` = sh("anc_first_cat", "<4 months"),
    `4-7 months` = sh("anc_first_cat", "4-7 months"),
    `8+ months` = sh("anc_first_cat", "8+ months"),
    `Don't know (timing)` = sh("anc_first_cat", "Don't know"),
    `Number of women with recent birth proxy` = unw_n(dat)
  )
}

t310 <- bind_rows(
  make_block(t310_dat, "Residence", "derived_residence", 1:2, res_lab, calc_t310),
  make_block(t310_dat, "Region", "derived_zone", 1:6, zone_lab, calc_t310),
  make_block(t310_dat, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t310),
  make_block(t310_dat, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t310),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t310(t310_dat, "Total") %>% select(-`Background characteristic`))
)

t310_states <- make_state_table(
  t310_dat,
  function(dd, lab, row_name = "State") {
    calc_t310(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t310(t310_dat, "Total") %>% select(-`Background characteristic`))
  )

# ============================================================
# TABLE 3.11 — IPTp doses
# ============================================================
need_cols(c("u_recent_birth", "derived_iptp_1plus", "derived_iptp_2plus", "derived_iptp_3plus"))

calc_t311 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  tibble(
    `Background characteristic` = rowlab,
    `IPTp 1+ (SP/Fansidar)` = pct_from_binary(as.numeric(to_num(dat$derived_iptp_1plus) == 1), wts),
    `IPTp 2+ (SP/Fansidar)` = pct_from_binary(as.numeric(to_num(dat$derived_iptp_2plus) == 1), wts),
    `IPTp 3+ (SP/Fansidar)` = pct_from_binary(as.numeric(to_num(dat$derived_iptp_3plus) == 1), wts),
    `Number of women with recent birth proxy` = unw_n(dat)
  )
}

t311 <- bind_rows(
  make_block(t39_dat, "Residence", "derived_residence", 1:2, res_lab, calc_t311),
  make_block(t39_dat, "Region", "derived_zone", 1:6, zone_lab, calc_t311),
  make_block(t39_dat, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t311),
  make_block(t39_dat, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t311),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t311(t39_dat, "Total") %>% select(-`Background characteristic`))
)

t311_states <- make_state_table(
  t39_dat,
  function(dd, lab, row_name = "State") {
    calc_t311(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t311(t39_dat, "Total") %>% select(-`Background characteristic`))
  )

# ============================================================
# TABLE 4.1 — Fever, care seeking, prompt care, testing, diagnosis
# ============================================================
need_cols(c(
  "u_child_youngest", "u_child_fever", "women_child_fever_2weeks",
  "derived_fever_seek_advice", "derived_fever_prompt_care",
  "derived_fever_tested", "women_child_malaria_diagnosis"
))

t41_base <- df %>% filter(u_child_youngest == 1)

calc_t41 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  
  fever <- pct_from_binary(as.numeric(to_num(dat$women_child_fever_2weeks) == 1), wts)
  
  fever_dat <- dat %>% filter(to_num(u_child_fever) == 1)
  wts2 <- if (!is.null(w)) fever_dat[[w]] else NULL
  
  tibble(
    `Background characteristic` = rowlab,
    `% with fever in last 2 weeks` = fever,
    `Number of children in youngest-child universe` = unw_n(dat),
    `% advice/treatment sought (among fever)` = pct_from_binary(as.numeric(to_num(fever_dat$derived_fever_seek_advice) == 1), wts2),
    `% sought same day/next day (among fever)` = pct_from_binary(as.numeric(to_num(fever_dat$derived_fever_prompt_care) == 1), wts2),
    `% blood taken for testing (among fever)` = pct_from_binary(as.numeric(to_num(fever_dat$derived_fever_tested) == 1), wts2),
    `% diagnosed with malaria by provider (among fever)` = pct_from_binary(as.numeric(to_num(fever_dat$women_child_malaria_diagnosis) == 1), wts2),
    `Number of children with fever` = unw_n(fever_dat)
  )
}

t41 <- bind_rows(
  make_block(t41_base, "Residence", "derived_residence", 1:2, res_lab, calc_t41),
  make_block(t41_base, "Region", "derived_zone", 1:6, zone_lab, calc_t41),
  make_block(t41_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t41),
  make_block(t41_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t41),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t41(t41_base, "Total") %>% select(-`Background characteristic`))
)

# ============================================================
# TABLE 4.2 — Referral among fever cases where care was sought
# ============================================================
need_cols(c("women_child_referral", "derived_fever_seek_advice"))

t42_base <- df %>% filter(to_num(u_child_fever) == 1, to_num(derived_fever_seek_advice) == 1)

calc_t42 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  tibble(
    `Background characteristic` = rowlab,
    `% referred to higher level of care` = pct_from_binary(as.numeric(to_num(dat$women_child_referral) == 1), wts),
    `Number of children with fever and care sought` = unw_n(dat)
  )
}

t42 <- bind_rows(
  make_block(t42_base, "Residence", "derived_residence", 1:2, res_lab, calc_t42),
  make_block(t42_base, "Region", "derived_zone", 1:6, zone_lab, calc_t42),
  make_block(t42_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t42),
  make_block(t42_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t42),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t42(t42_base, "Total") %>% select(-`Background characteristic`))
)

# ============================================================
# TABLE 4.3 — Source of advice/treatment for fever (multi-response)
# ============================================================
need_cols(c(
  "u_child_fever", "derived_fever_seek_advice", "child_adv_gov", "child_adv_pvth",
  "child_adv_ngo", "child_adv_mob", "child_adv_pvtd", "child_adv_com",
  "child_adv_pharm", "child_adv_chem", "child_adv_trad", "child_adv_rel", "child_adv_other"
))

t43_base <- df %>% filter(to_num(u_child_fever) == 1, to_num(derived_fever_seek_advice) == 1)

src_cols <- c("child_adv_gov","child_adv_pvth","child_adv_ngo","child_adv_mob","child_adv_pvtd","child_adv_com",
              "child_adv_pharm","child_adv_chem","child_adv_trad","child_adv_rel","child_adv_other")

src_labels <- c(
  child_adv_gov   = "Government facility",
  child_adv_pvth  = "Private facility",
  child_adv_ngo   = "NGO facility",
  child_adv_mob   = "Mobile clinic",
  child_adv_pvtd  = "Private doctor",
  child_adv_com   = "Community health worker",
  child_adv_pharm = "Pharmacy",
  child_adv_chem  = "Chemist",
  child_adv_trad  = "Traditional healer",
  child_adv_rel   = "Religious healer",
  child_adv_other = "Other"
)

calc_t43 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  vals <- sapply(src_cols, function(v) pct_from_binary(as.numeric(to_num(dat[[v]]) == 1), wts))
  out <- tibble(`Background characteristic` = rowlab) %>% bind_cols(as_tibble_row(vals))
  out$`Number of children with fever and care sought` <- unw_n(dat)
  out
}

t43 <- bind_rows(
  make_block(t43_base, "Residence", "derived_residence", 1:2, res_lab, calc_t43),
  make_block(t43_base, "Region", "derived_zone", 1:6, zone_lab, calc_t43),
  make_block(t43_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t43),
  make_block(t43_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t43),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t43(t43_base, "Total") %>% select(-`Background characteristic`))
)

names(t43) <- ifelse(names(t43) %in% names(src_labels), unname(src_labels[names(t43)]), names(t43))

# ============================================================
# TABLE 4.4 — Antimalarial drugs taken for fever (multi-response)
# ============================================================
need_cols(c(
  "u_child_fever", "derived_fever_took_anti",
  "med_act", "med_sp", "med_chloro", "med_amod", "med_quinine", "med_artesun", "med_inject", "med_other"
))

t44_base <- df %>% filter(to_num(u_child_fever) == 1, to_num(derived_fever_took_anti) == 1)

drug_cols <- c("med_act","med_sp","med_chloro","med_amod","med_quinine","med_artesun","med_inject","med_other")
drug_labels <- c(
  med_act     = "Any ACT",
  med_sp      = "SP/Fansidar",
  med_chloro  = "Chloroquine",
  med_amod    = "Amodiaquine",
  med_quinine = "Quinine",
  med_artesun = "Artesunate",
  med_inject  = "Injection",
  med_other   = "Other (non-ACT)"
)

calc_t44 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  vals <- sapply(drug_cols, function(v) pct_from_binary(as.numeric(to_num(dat[[v]]) == 1), wts))
  out <- tibble(`Background characteristic` = rowlab) %>% bind_cols(as_tibble_row(vals))
  out$`Number of children with fever who took any antimalarial` <- unw_n(dat)
  out
}

t44 <- bind_rows(
  make_block(t44_base, "Residence", "derived_residence", 1:2, res_lab, calc_t44),
  make_block(t44_base, "Region", "derived_zone", 1:6, zone_lab, calc_t44),
  make_block(t44_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t44),
  make_block(t44_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t44),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t44(t44_base, "Total") %>% select(-`Background characteristic`))
)

names(t44) <- ifelse(names(t44) %in% names(drug_labels), unname(drug_labels[names(t44)]), names(t44))

# ============================================================
# TABLE 4.5 — ACT effectiveness among ACT users
# ============================================================
need_cols(c("u_child_fever", "derived_fever_took_act", "derived_act_effective"))

t45_base <- df %>% filter(to_num(u_child_fever) == 1, to_num(derived_fever_took_act) == 1)

calc_t45 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  tibble(
    `Background characteristic` = rowlab,
    `% fever went away after ACT` = pct_from_binary(as.numeric(to_num(dat$derived_act_effective) == 1), wts),
    `Number of children with fever who received ACT` = unw_n(dat)
  )
}

t45 <- bind_rows(
  make_block(t45_base, "Residence", "derived_residence", 1:2, res_lab, calc_t45),
  make_block(t45_base, "Region", "derived_zone", 1:6, zone_lab, calc_t45),
  make_block(t45_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t45),
  make_block(t45_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t45),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t45(t45_base, "Total") %>% select(-`Background characteristic`))
)

# ============================================================
# TABLE 5.1.1 — Exposure to malaria messages and sources
# ============================================================
need_cols(c(
  "u_women_15_49", "bg_heard_malaria_msg_6months", "msg_radio", "msg_tv", "msg_poster", "msg_news",
  "msg_leaf", "msg_hcp", "msg_chw", "msg_social", "msg_town", "msg_ipc", "msg_family"
))

t511_base <- df %>% filter(u_women_15_49 == 1)

msg_cols <- c("msg_radio","msg_tv","msg_poster","msg_news","msg_leaf","msg_hcp","msg_chw","msg_social","msg_town","msg_ipc","msg_family")
msg_labels <- c(
  msg_radio  = "Radio",
  msg_tv     = "Television",
  msg_poster = "Poster/Billboard",
  msg_news   = "Newspaper/Magazine",
  msg_leaf   = "Leaflet/Brochure",
  msg_hcp    = "Healthcare provider",
  msg_chw    = "Community health worker",
  msg_social = "Social media",
  msg_town   = "Town announcer",
  msg_ipc    = "IPC agent/community volunteer",
  msg_family = "Family/Friends"
)

calc_t511 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  heard <- pct_from_binary(as.numeric(to_num(dat$bg_heard_malaria_msg_6months) == 1), wts)
  
  sub <- dat %>% filter(to_num(bg_heard_malaria_msg_6months) == 1)
  wts2 <- if (!is.null(w)) sub[[w]] else NULL
  
  src <- sapply(msg_cols, function(v) pct_from_binary(as.numeric(to_num(sub[[v]]) == 1), wts2))
  
  out <- tibble(
    `Background characteristic` = rowlab,
    `% seen/heard malaria message in past 6 months` = heard,
    `Number of women` = unw_n(dat)
  ) %>% bind_cols(as_tibble_row(src))
  
  out
}

t511 <- bind_rows(
  make_block(t511_base, "Age", "derived_age_group_w", 1:7, agew_lab, calc_t511),
  make_block(t511_base, "Residence", "derived_residence", 1:2, res_lab, calc_t511),
  make_block(t511_base, "Region", "derived_zone", 1:6, zone_lab, calc_t511),
  make_block(t511_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t511),
  make_block(t511_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t511),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t511(t511_base, "Total") %>% select(-`Background characteristic`))
)

names(t511) <- ifelse(names(t511) %in% names(msg_labels), unname(msg_labels[names(t511)]), names(t511))

t511_states <- make_state_table(
  t511_base,
  function(dd, lab, row_name = "State") {
    calc_t511(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t511(t511_base, "Total") %>% select(-`Background characteristic`))
  )

names(t511_states) <- ifelse(names(t511_states) %in% names(msg_labels), unname(msg_labels[names(t511_states)]), names(t511_states))

# ============================================================
# TABLE 5.2.1 — Knowledge of ways to avoid malaria
# ============================================================
need_cols(c(
  "u_women_15_49", "bg_aware_avoidance", "know_net", "know_itn", "know_repel",
  "know_meds", "know_spray", "know_stag", "know_clean", "know_screens", "know_other", "know_dont"
))

t521_base <- t511_base

know_cols <- c("know_net","know_itn","know_repel","know_meds","know_spray","know_stag","know_clean","know_screens","know_other","know_dont")
know_labels <- c(
  know_net     = "Sleep under mosquito net",
  know_itn     = "Sleep under ITN",
  know_repel   = "Use repellent/coil",
  know_meds    = "Take preventive medications",
  know_spray   = "Spray house with insecticide",
  know_stag    = "Fill in stagnant waters",
  know_clean   = "Keep surroundings clean",
  know_screens = "Put mosquito screens on windows",
  know_other   = "Other",
  know_dont    = "Don't know"
)

calc_t521 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  aware <- pct_from_binary(as.numeric(to_num(dat$bg_aware_avoidance) == 1), wts)
  
  sub <- dat %>% filter(to_num(bg_aware_avoidance) == 1)
  wts2 <- if (!is.null(w)) sub[[w]] else NULL
  
  ways <- sapply(know_cols, function(v) pct_from_binary(as.numeric(to_num(sub[[v]]) == 1), wts2))
  
  out <- tibble(
    `Background characteristic` = rowlab,
    `% aware of any way to avoid malaria` = aware,
    `Number of women` = unw_n(dat)
  ) %>% bind_cols(as_tibble_row(ways))
  
  out
}

t521 <- bind_rows(
  make_block(t521_base, "Age", "derived_age_group_w", 1:7, agew_lab, calc_t521),
  make_block(t521_base, "Residence", "derived_residence", 1:2, res_lab, calc_t521),
  make_block(t521_base, "Region", "derived_zone", 1:6, zone_lab, calc_t521),
  make_block(t521_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t521),
  make_block(t521_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t521),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t521(t521_base, "Total") %>% select(-`Background characteristic`))
)

names(t521) <- ifelse(names(t521) %in% names(know_labels), unname(know_labels[names(t521)]), names(t521))

t521_states <- make_state_table(
  t521_base,
  function(dd, lab, row_name = "State") {
    calc_t521(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t521(t521_base, "Total") %>% select(-`Background characteristic`))
  )

names(t521_states) <- ifelse(names(t521_states) %in% names(know_labels), unname(know_labels[names(t521_states)]), names(t521_states))

# ============================================================
# TABLE 5.3.1 — Susceptibility / severity / self-efficacy items
# ============================================================
need_cols(c(
  "u_women_15_49", "att_rainy_season_only", "att_fever_worry_malaria",
  "att_malaria_easily_treated", "att_weak_children_die", "att_net_use_mosquito_density", "u_hh_has_u5"
))

favourable_att <- function(x, favourable_is = c("AGREE", "DISAGREE")) {
  favourable_is <- match.arg(favourable_is)
  xx <- to_num(x)
  case_when(
    is.na(xx) ~ NA_real_,
    favourable_is == "AGREE" & xx == 1 ~ 1,
    favourable_is == "AGREE" & xx == 2 ~ 0,
    favourable_is == "DISAGREE" & xx == 2 ~ 1,
    favourable_is == "DISAGREE" & xx == 1 ~ 0,
    TRUE ~ NA_real_
  )
}

t531_base <- t511_base %>%
  mutate(
    has_u5 = case_when(
      to_num(u_hh_has_u5) == 1 ~ "One or more",
      to_num(u_hh_has_u5) == 0 ~ "None",
      TRUE ~ NA_character_
    )
  )

calc_t531 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  tibble(
    `Background characteristic` = rowlab,
    `% disagree malaria occurs only in rainy season` = pct_from_binary(favourable_att(dat$att_rainy_season_only, "DISAGREE"), wts),
    `% agree worry fever might be malaria` = pct_from_binary(favourable_att(dat$att_fever_worry_malaria, "AGREE"), wts),
    `% disagree malaria is not a problem because easily treated` = pct_from_binary(favourable_att(dat$att_malaria_easily_treated, "DISAGREE"), wts),
    `% disagree only weak children die of malaria` = pct_from_binary(favourable_att(dat$att_weak_children_die, "DISAGREE"), wts),
    `% agree need net all night when mosquitoes are many` = pct_from_binary(favourable_att(dat$att_net_use_mosquito_density, "AGREE"), wts),
    `Number of women` = unw_n(dat)
  )
}

t531 <- bind_rows(
  make_block(t531_base, "Age", "derived_age_group_w", 1:7, agew_lab, calc_t531),
  make_block_char(t531_base, "Living children under 5", "has_u5", c("One or more", "None"),
                  c("One or more" = "One or more", "None" = "None"), calc_t531),
  make_block(t531_base, "Residence", "derived_residence", 1:2, res_lab, calc_t531),
  make_block(t531_base, "Region", "derived_zone", 1:6, zone_lab, calc_t531),
  make_block(t531_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t531),
  make_block(t531_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t531),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t531(t531_base, "Total") %>% select(-`Background characteristic`))
)

t531_states <- make_state_table(
  t531_base,
  function(dd, lab, row_name = "State") {
    calc_t531(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t531(t531_base, "Total") %>% select(-`Background characteristic`))
  )

# ============================================================
# TABLE 5.4.1 — Behaviour attitudes and norms
# ============================================================
need_cols(c(
  "u_women_15_49", "att_net_use_warm_weather", "att_home_meds_first", "att_full_dose_importance",
  "att_seek_care_immediate", "att_community_net_usage", "u_hh_has_u5"
))

calc_t541 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  tibble(
    `Background characteristic` = rowlab,
    `% disagree dislike using net when too warm` = pct_from_binary(favourable_att(dat$att_net_use_warm_weather, "DISAGREE"), wts),
    `% agree start fever treatment with home medicine` = pct_from_binary(favourable_att(dat$att_home_meds_first, "AGREE"), wts),
    `% agree it is important to complete malaria dose` = pct_from_binary(favourable_att(dat$att_full_dose_importance, "AGREE"), wts),
    `% agree community seeks care same day / next day` = pct_from_binary(favourable_att(dat$att_seek_care_immediate, "AGREE"), wts),
    `% agree net owners sleep under net nightly` = pct_from_binary(favourable_att(dat$att_community_net_usage, "AGREE"), wts),
    `Number of women` = unw_n(dat)
  )
}

t541 <- bind_rows(
  make_block(t531_base, "Age", "derived_age_group_w", 1:7, agew_lab, calc_t541),
  make_block_char(t531_base, "Living children under 5", "has_u5", c("One or more", "None"),
                  c("One or more" = "One or more", "None" = "None"), calc_t541),
  make_block(t531_base, "Residence", "derived_residence", 1:2, res_lab, calc_t541),
  make_block(t531_base, "Region", "derived_zone", 1:6, zone_lab, calc_t541),
  make_block(t531_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t541),
  make_block(t531_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t541),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t541(t531_base, "Total") %>% select(-`Background characteristic`))
)

t541_states <- make_state_table(
  t531_base,
  function(dd, lab, row_name = "State") {
    calc_t541(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t541(t531_base, "Total") %>% select(-`Background characteristic`))
  )

# ============================================================
# TABLE 6.1 — Respondent perceptions and recent experiences
# Respondent-level, not women-only
# ============================================================
need_cols(c(
  "derived_percep_affordable", "derived_gov_effective",
  "derived_exp_free_tx", "derived_exp_stockout",
  "u_govt_visit_6m_free", "u_govt_visit_6m_stockout",
  "demo_gender_num", "derived_residence", "derived_zone", "derived_edu_cat", "derived_wealth_quintile"
))

t61_base <- df %>%
  mutate(
    sex_grp = case_when(
      to_num(demo_gender_num) == 1 ~ "Male",
      to_num(demo_gender_num) == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  )

calc_t61 <- function(dat, rowlab) {
  wts <- if (!is.null(w)) dat[[w]] else NULL
  
  dat_free <- dat %>% filter(to_num(u_govt_visit_6m_free) == 1)
  dat_stock <- dat %>% filter(to_num(u_govt_visit_6m_stockout) == 1)
  
  wts_free <- if (!is.null(w)) dat_free[[w]] else NULL
  wts_stock <- if (!is.null(w)) dat_stock[[w]] else NULL
  
  tibble(
    `Background characteristic` = rowlab,
    `% consider malaria drugs affordable` =
      pct_from_binary(as.numeric(to_num(dat$derived_percep_affordable) == 1), wts),
    `% rate government efforts effective` =
      pct_from_binary(as.numeric(to_num(dat$derived_gov_effective) == 1), wts),
    `% received free malaria treatment at government facility (among govt-facility visitors)` =
      pct_from_binary(as.numeric(to_num(dat_free$derived_exp_free_tx) == 1), wts_free),
    `% experienced malaria drug stockout at government facility (among govt-facility visitors)` =
      pct_from_binary(as.numeric(to_num(dat_stock$derived_exp_stockout) == 1), wts_stock),
    `Number of respondents` = unw_n(dat),
    `Number visiting govt facility for free-treatment item` = unw_n(dat_free),
    `Number visiting govt facility for stockout item` = unw_n(dat_stock)
  )
}

t61 <- bind_rows(
  make_block_char(t61_base, "Sex", "sex_grp", c("Male", "Female"),
                  c("Male" = "Male", "Female" = "Female"), calc_t61),
  make_block(t61_base, "Residence", "derived_residence", 1:2, res_lab, calc_t61),
  make_block(t61_base, "Region", "derived_zone", 1:6, zone_lab, calc_t61),
  make_block(t61_base, "Education", "derived_edu_cat", 1:4, edu_lab, calc_t61),
  make_block(t61_base, "Wealth quintile", "derived_wealth_quintile", 1:5, wq_lab, calc_t61),
  tibble(`Background characteristic` = "Total") %>%
    bind_cols(calc_t61(t61_base, "Total") %>% select(-`Background characteristic`))
)

t61_states <- make_state_table(
  t61_base,
  function(dd, lab, row_name = "State") {
    calc_t61(dd, lab) %>% rename(!!row_name := `Background characteristic`)
  },
  row_name = "State"
) %>%
  bind_rows(
    tibble(State = "Total") %>%
      bind_cols(calc_t61(t61_base, "Total") %>% select(-`Background characteristic`))
  )

# ============================================================
# WRITE WORKBOOK
# ============================================================
wb <- if (file.exists(OUT_XLSX)) loadWorkbook(OUT_XLSX) else createWorkbook()

write_table_sheet(
  wb, "Table 3.8",
  "Table 3.8 Main reason mosquito net was not used last night (Sproxil respondents)",
  note_txt,
  t38,
  "Household-level proxy. Reason shares are among households with a net that was not used last night."
)

write_table_sheet(
  wb, "Table 3.9",
  "Table 3.9 Antenatal care among women with recent birth proxy (Sproxil respondents)",
  note_txt,
  t39,
  "Universe: women 15-49 with births_2020_2025 > 0 (recent birth proxy). Provider distribution is among ANC users."
)

write_table_sheet(
  wb, "Table 3.9 States",
  "Table 3.9 Antenatal care: States (Sproxil respondents)",
  note_txt,
  t39_states,
  "Universe: women 15-49 with births_2020_2025 > 0 (recent birth proxy)."
)

write_table_sheet(
  wb, "Table 3.10",
  "Table 3.10 Number of ANC visits and timing of first ANC visit (Sproxil respondents)",
  note_txt,
  t310,
  "Universe: women 15-49 with births_2020_2025 > 0. In your recode, less than one month / 2 weeks / 3 weeks are coded as 0."
)

write_table_sheet(
  wb, "Table 3.10 States",
  "Table 3.10 Number of ANC visits and timing of first ANC visit: States (Sproxil respondents)",
  note_txt,
  t310_states,
  "Universe: women 15-49 with births_2020_2025 > 0."
)

write_table_sheet(
  wb, "Table 3.11",
  "Table 3.11 Use of IPTp (SP/Fansidar) among women with recent birth proxy (Sproxil respondents)",
  note_txt,
  t311,
  "Universe: women 15-49 with births_2020_2025 > 0."
)

write_table_sheet(
  wb, "Table 3.11 States",
  "Table 3.11 IPTp (SP/Fansidar): States (Sproxil respondents)",
  note_txt,
  t311_states,
  "Universe: women 15-49 with births_2020_2025 > 0."
)

write_table_sheet(
  wb, "Table 4.1",
  "Table 4.1 Fever and care seeking (youngest child proxy) (Sproxil respondents)",
  note_txt,
  t41,
  "Universe: households with child under 5 based on the youngest-child proxy."
)

write_table_sheet(
  wb, "Table 4.2",
  "Table 4.2 Referral among fever cases with care sought (youngest child proxy) (Sproxil respondents)",
  note_txt,
  t42,
  "Universe: children with fever in the youngest-child proxy where advice or treatment was sought."
)

write_table_sheet(
  wb, "Table 4.3",
  "Table 4.3 Source of advice or treatment for fever (multi-response; youngest child proxy) (Sproxil respondents)",
  note_txt,
  t43,
  "Multi-response table: source columns do not sum to 100. Universe: fever cases with care sought."
)

write_table_sheet(
  wb, "Table 4.4",
  "Table 4.4 Antimalarial drugs taken for fever (multi-response; youngest child proxy) (Sproxil respondents)",
  note_txt,
  t44,
  "Multi-response table: drug columns do not sum to 100. Universe: fever cases who took any antimalarial."
)

write_table_sheet(
  wb, "Table 4.5",
  "Table 4.5 Fever resolved after ACT among ACT users (youngest child proxy) (Sproxil respondents)",
  note_txt,
  t45,
  "Universe: fever cases in the youngest-child proxy who received ACT."
)

write_table_sheet(
  wb, "Table 5.11",
  "Table 5.1.1 Exposure to malaria messages and sources (Sproxil respondents)",
  note_txt,
  t511,
  "Source columns are multi-response and do not sum to 100. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 5.11 States",
  "Table 5.1.1 Exposure to malaria messages: States (Sproxil respondents)",
  note_txt,
  t511_states,
  "Source columns are multi-response and do not sum to 100. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 5.2.1",
  "Table 5.2.1 Knowledge of ways to avoid malaria (Sproxil respondents)",
  note_txt,
  t521,
  "Method columns are multi-response and do not sum to 100. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 5.2.1 States",
  "Table 5.2.1 Knowledge of ways to avoid malaria: States (Sproxil respondents)",
  note_txt,
  t521_states,
  "Method columns are multi-response and do not sum to 100. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 5.3.1",
  "Table 5.3.1 Malaria susceptibility, severity and self-efficacy (supported items only) (Sproxil respondents)",
  note_txt,
  t531,
  "Modified custom table using only supported Sproxil items. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 5.3.1 States",
  "Table 5.3.1 Malaria susceptibility, severity and self-efficacy: States (Sproxil respondents)",
  note_txt,
  t531_states,
  "Modified custom table using only supported Sproxil items. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 5.4.1",
  "Table 5.4.1 Attitudes toward behaviours and community norms (supported items only) (Sproxil respondents)",
  note_txt,
  t541,
  "Modified custom table using only supported Sproxil items. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 5.4.1 States",
  "Table 5.4.1 Attitudes toward behaviours and community norms: States (Sproxil respondents)",
  note_txt,
  t541_states,
  "Modified custom table using only supported Sproxil items. Universe: women 15-49."
)

write_table_sheet(
  wb, "Table 6.1",
  "Table 6.1 Respondent perceptions and recent experiences related to malaria treatment affordability, government efforts, and public-facility service delivery (Sproxil respondents)",
  note_txt,
  t61,
  "Affordability and government-effort indicators use all respondents. Free-treatment and stockout indicators are among respondents who visited a government facility in the last 6 months."
)

write_table_sheet(
  wb, "Table 6.1 States",
  "Table 6.1 Respondent perceptions and recent experiences: States (Sproxil respondents)",
  note_txt,
  t61_states,
  "Affordability and government-effort indicators use all respondents. Free-treatment and stockout indicators are among respondents who visited a government facility in the last 6 months."
)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("✅ All revised tables 3.8 through 6.1 written to: ", OUT_XLSX)