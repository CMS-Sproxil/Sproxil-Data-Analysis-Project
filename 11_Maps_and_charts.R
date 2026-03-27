# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  10_State_Maps.R
# AUTHOR:  Corona Management Systems
# PURPOSE: Generate state choropleth maps from Sproxil_Analysis_Ready.rds
#          with dynamic integer ranges and darker shades indicating worse results
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(forcats)
  library(tibble)
})

# ------------------------------------------------------------------------------
# 1) CONFIG
# ------------------------------------------------------------------------------
INPUT_RDS <- "Sproxil_Analysis_Ready_Weighted.rds"
SHAPEFILE_PATH <- "shapefile_states/grid3_nga_boundary_vaccstates.shp"
OUT_DIR <- "state_maps"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

WVAR <- "w_calibrated_trim"

# State code -> state name mapping based on your recode
state_lookup <- tibble::tribble(
  ~demo_state_num, ~state_name,
  1, "ABIA",
  2, "ADAMAWA",
  3, "AKWA IBOM",
  4, "ANAMBRA",
  5, "BAUCHI",
  6, "BAYELSA",
  7, "BENUE",
  8, "BORNO",
  9, "CROSS RIVER",
  10, "DELTA",
  11, "EBONYI",
  12, "EDO",
  13, "EKITI",
  14, "ENUGU",
  15, "FEDERAL CAPITAL TERRITORY",
  16, "GOMBE",
  17, "IMO",
  18, "JIGAWA",
  19, "KADUNA",
  20, "KANO",
  21, "KATSINA",
  22, "KEBBI",
  23, "KOGI",
  24, "KWARA",
  25, "LAGOS",
  26, "NASARAWA",
  27, "NIGER",
  28, "OGUN",
  29, "ONDO",
  30, "OSUN",
  31, "OYO",
  32, "PLATEAU",
  33, "RIVERS",
  34, "SOKOTO",
  35, "TARABA",
  36, "YOBE",
  37, "ZAMFARA"
)

# ------------------------------------------------------------------------------
# 2) LOAD DATA
# ------------------------------------------------------------------------------
df <- readRDS(INPUT_RDS)

use_weight <- (WVAR %in% names(df)) && any(!is.na(df[[WVAR]]))
message("Using weights: ", use_weight)

nga_states <- st_read(SHAPEFILE_PATH, quiet = TRUE)

# ------------------------------------------------------------------------------
# 3) SHAPEFILE STATE NAME STANDARDISATION
# ------------------------------------------------------------------------------
state_name_col <- names(nga_states)[names(nga_states) %in% c(
  "statename", "STATE_NAME", "NAME_1", "admin1Name", "shapeName"
)][1]

if (is.na(state_name_col)) {
  stop(
    "Could not identify state name column in shapefile. Available columns: ",
    paste(names(nga_states), collapse = ", ")
  )
}

clean_state <- function(x) {
  x <- toupper(str_squish(as.character(x)))
  x <- case_when(
    x %in% c("FCT", "ABUJA", "FCT ABUJA") ~ "FEDERAL CAPITAL TERRITORY",
    x == "AKWA-IBOM" ~ "AKWA IBOM",
    TRUE ~ x
  )
  x
}

nga_states <- nga_states %>%
  mutate(state_name = clean_state(.data[[state_name_col]]))

# ------------------------------------------------------------------------------
# 4) HELPERS
# ------------------------------------------------------------------------------
weighted_mean_01 <- function(x, w = NULL) {
  ok <- !is.na(x)
  if (!is.null(w)) ok <- ok & !is.na(w)
  if (!any(ok)) return(NA_real_)
  if (is.null(w)) return(mean(x[ok]) * 100)
  weighted.mean(x[ok], w[ok]) * 100
}

state_indicator <- function(data, indicator, universe = NULL, wvar = NULL) {
  d <- data %>%
    mutate(
      demo_state_num = as.integer(as.numeric(as.character(demo_state_num))),
      ind = as.numeric(.data[[indicator]])
    )
  
  if (!is.null(universe)) {
    d <- d %>% filter(.data[[universe]] == 1)
  }
  
  d <- d %>%
    filter(demo_state_num %in% 1:37)
  
  if (is.null(wvar)) {
    out <- d %>%
      group_by(demo_state_num) %>%
      summarise(
        estimate = weighted_mean_01(ind, NULL),
        n_unweighted = sum(!is.na(ind)),
        .groups = "drop"
      )
  } else {
    out <- d %>%
      group_by(demo_state_num) %>%
      summarise(
        estimate = weighted_mean_01(ind, .data[[wvar]]),
        n_unweighted = sum(!is.na(ind)),
        .groups = "drop"
      )
  }
  
  out %>%
    left_join(state_lookup, by = "demo_state_num")
}

# ------------------------------------------------------------------------------
# 5) DYNAMIC INTEGER BINNING
# ------------------------------------------------------------------------------
make_dynamic_bins <- function(x, n_bins = 5) {
  x_nonmiss <- x[!is.na(x)]
  
  if (length(x_nonmiss) == 0) {
    return(list(
      band = factor(rep(NA_character_, length(x))),
      levels = character(0),
      labels = character(0)
    ))
  }
  
  xmin <- floor(min(x_nonmiss))
  xmax <- ceiling(max(x_nonmiss))
  
  # If all values are the same, create one class only
  if (xmin == xmax) {
    lbl <- paste0(xmin, "-", xmax)
    band <- ifelse(is.na(x), NA_character_, lbl)
    return(list(
      band = factor(band, levels = lbl, ordered = TRUE),
      levels = lbl,
      labels = lbl
    ))
  }
  
  raw_breaks <- seq(xmin, xmax, length.out = n_bins + 1)
  breaks <- round(raw_breaks)
  
  # ensure monotone increasing unique breaks
  breaks[1] <- xmin
  breaks[length(breaks)] <- xmax
  
  for (i in 2:length(breaks)) {
    if (breaks[i] <= breaks[i - 1]) {
      breaks[i] <- breaks[i - 1] + 1
    }
  }
  
  # If last break overshoots, compress safely
  if (breaks[length(breaks)] < xmax) {
    breaks[length(breaks)] <- xmax
  }
  
  # Build integer labels with no decimals
  labels <- character(length(breaks) - 1)
  for (i in seq_along(labels)) {
    lower <- breaks[i]
    upper <- breaks[i + 1]
    if (i < length(labels)) {
      upper_lab <- upper - 1
    } else {
      upper_lab <- upper
    }
    labels[i] <- paste0(lower, "-", upper_lab)
  }
  
  # Need final cut breaks to fully include maximum
  cut_breaks <- breaks
  cut_breaks[length(cut_breaks)] <- cut_breaks[length(cut_breaks)] + 1e-8
  
  band <- cut(
    x,
    breaks = cut_breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE
  )
  
  list(
    band = band,
    levels = labels,
    labels = labels
  )
}

# ------------------------------------------------------------------------------
# 6) PALETTE DIRECTION
# ------------------------------------------------------------------------------
# "negative_high = TRUE" means high value is worse -> darkest at high end
# "negative_high = FALSE" means low value is worse -> darkest at low end
get_fill_values <- function(levels_vec, negative_high = TRUE) {
  base_cols <- c(
    "#FEE5D9",
    "#FCAE91",
    "#FB6A4A",
    "#DE2D26",
    "#A50F15"
  )
  
  # Trim or expand to length needed
  if (length(levels_vec) == 1) {
    cols <- "#DE2D26"
  } else {
    cols <- colorRampPalette(base_cols)(length(levels_vec))
  }
  
  if (!negative_high) {
    cols <- rev(cols)
  }
  
  stats::setNames(cols, levels_vec)
}

# ------------------------------------------------------------------------------
# 7) PLOTTING
# ------------------------------------------------------------------------------
plot_state_map <- function(sf_data, title_txt, file_out, negative_high = TRUE) {
  
  # create dynamic bins for this indicator
  bin_obj <- make_dynamic_bins(sf_data$estimate, n_bins = 5)
  
  sf_data <- sf_data %>%
    mutate(
      band = factor(as.character(bin_obj$band), levels = bin_obj$levels, ordered = TRUE)
    )
  
  fill_values <- get_fill_values(bin_obj$levels, negative_high = negative_high)
  
  label_pts <- sf::st_point_on_surface(sf_data) %>%
    mutate(
      label_txt = case_when(
        is.na(estimate) ~ state_name,
        TRUE ~ paste0(state_name, "\n", round(estimate, 0), "%")
      )
    )
  
  p <- ggplot(sf_data) +
    geom_sf(
      aes(fill = band),
      color = "white",
      linewidth = 0.25
    ) +
    geom_sf_text(
      data = label_pts,
      aes(label = label_txt),
      size = 3.0,
      lineheight = 0.9,
      colour = "black"
    ) +
    scale_fill_manual(
      values = fill_values,
      na.value = "grey80",
      drop = FALSE,
      name = "%"
    ) +
    labs(
      title = title_txt,
      subtitle = "State-level percentage among Sproxil respondents",
      caption = "Opt-in convenience sample; darker shades indicate worse result."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file_out, p, width = 14, height = 11, dpi = 600)
  p
}

# ------------------------------------------------------------------------------
# 8) MAP SPECIFICATIONS
# ------------------------------------------------------------------------------
# negative_high:
#   TRUE  = higher value is worse
#   FALSE = lower value is worse
map_specs <- tibble::tribble(
  ~indicator,                       ~universe,           ~map_title,                                                 ~file_stub,                ~negative_high,
  "derived_access_itn",             "u_household",       "State-level ITN access (%)",                               "map_itn_access",          FALSE,
  "derived_hh_has_any_net",         "u_household",       "State-level households with any mosquito net (%)",         "map_any_net",             FALSE,
  "derived_hh_has_itn",             "u_household",       "State-level households with any ITN (%)",                  "map_any_itn",             FALSE,
  "derived_net_use_any_last_night", "u_household",       "State-level any household net use last night (%)",         "map_net_use_any",         FALSE,
  "derived_anc_any",                "u_recent_birth",    "State-level women with any ANC (%)",                       "map_anc_any",             FALSE,
  "derived_anc_4plus",              "u_recent_birth",    "State-level women with 4+ ANC visits (%)",                 "map_anc_4plus",           FALSE,
  "derived_iptp_1plus",             "u_recent_birth",    "State-level IPTp1+ among recent-birth women (%)",          "map_iptp1plus",           FALSE,
  "derived_iptp_2plus",             "u_recent_birth",    "State-level IPTp2+ among recent-birth women (%)",          "map_iptp2plus",           FALSE,
  "derived_iptp_3plus",             "u_recent_birth",    "State-level IPTp3+ among recent-birth women (%)",          "map_iptp3plus",           FALSE,
  "derived_child_fever",            "u_child_youngest",  "State-level youngest child fever in last 2 weeks (%)",     "map_child_fever",         TRUE,
  "derived_fever_seek_advice",      "u_child_fever",     "State-level advice/treatment sought for child fever (%)",  "map_fever_seek_advice",   FALSE,
  "derived_fever_tested",           "u_child_fever",     "State-level child fever tested (%)",                       "map_fever_tested",        FALSE,
  "derived_fever_took_act",         "u_child_fever",     "State-level ACT use for child fever (%)",                  "map_fever_act",           FALSE
)

# ------------------------------------------------------------------------------
# 9) GENERATE STATE ESTIMATES + MAPS
# ------------------------------------------------------------------------------
all_state_estimates <- list()

for (i in seq_len(nrow(map_specs))) {
  ind  <- map_specs$indicator[i]
  uni  <- map_specs$universe[i]
  ttl  <- map_specs$map_title[i]
  stub <- map_specs$file_stub[i]
  neg_hi <- map_specs$negative_high[i]
  
  if (!ind %in% names(df)) {
    message("Skipping missing indicator: ", ind)
    next
  }
  if (!is.null(uni) && !uni %in% names(df)) {
    message("Skipping due to missing universe: ", uni)
    next
  }
  
  est_df <- state_indicator(
    data = df,
    indicator = ind,
    universe = uni,
    wvar = if (use_weight) WVAR else NULL
  ) %>%
    mutate(
      indicator = ind,
      universe = uni
    )
  
  all_state_estimates[[ind]] <- est_df
  
  map_sf <- nga_states %>%
    left_join(est_df, by = "state_name")
  
  plot_state_map(
    sf_data = map_sf,
    title_txt = ttl,
    file_out = file.path(OUT_DIR, paste0(stub, ".png")),
    negative_high = neg_hi
  )
}

# ------------------------------------------------------------------------------
# 10) EXPORT ESTIMATE TABLE
# ------------------------------------------------------------------------------
state_estimates_long <- bind_rows(all_state_estimates)
write_csv(state_estimates_long, file.path(OUT_DIR, "state_map_estimates.csv"))

message("✅ State maps written to: ", OUT_DIR)
message("✅ State estimates CSV written to: ", file.path(OUT_DIR, "state_map_estimates.csv"))