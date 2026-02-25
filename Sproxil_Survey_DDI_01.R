# ==============================================================================
# PROJECT: Sproxil Malaria Survey Analysis
# SCRIPT:  08_Generate_DDI_XML.R
# AUTHOR:  Ikechukwu Onuko
# DATE:    February 24, 2026
#
# DESCRIPTION:
# 1. Imports the final labelled analytic dataset.
# 2. Attaches Study-Level Metadata (Citation, Methodology, Abstract).
# 3. Exports a DDI 2.5 Codebook (XML) for data archival (NADA/IHSN compliant).
# ==============================================================================

# Install DDIwR if you haven't yet
if(!require(DDIwR)) install.packages("DDIwR")

library(tidyverse)
library(DDIwR)
library(haven)

# --- 1. LOAD FINAL DATASET ---
cat("--- 1. Loading Final Data ---\n")
INPUT_FILE <- "Sproxil_Derived_Final.rds"

if(!file.exists(INPUT_FILE)) stop("Final RDS file not found!")
df <- readRDS(INPUT_FILE)

# --- 2. DEFINE STUDY-LEVEL METADATA ---
cat("--- 2. Attaching Study-Level Metadata ---\n")

# DDI requires information about the survey itself, not just the variables.
# We attach this information as a list.

study_metadata <- list(
  "file_name"        = "Sproxil_Malaria_Survey_2026.xml",
  "study_title"      = "Sproxil Malaria Care & Prevention Survey (Pharmacy Intercept)",
  "study_id"         = "NGA-SPROXIL-MIS-2026-V1",
  "author"           = "Ikechukwu Onuko",
  "producers"        = "Sproxil; National Malaria Elimination Programme (NMEP)",
  "production_date"  = as.character(Sys.Date()),
  "software_version" = "Generated using R package DDIwR",
  
  "description" = paste(
    "This survey collected data on malaria knowledge, attitudes, and practices (KAP),",
    "ITN ownership, and malaria treatment seeking behavior.",
    "Data was collected via a digital intercept survey of patrons verifying",
    "antimalarial drugs using the Sproxil mobile authentication platform."
  ),
  
  "sampling_procedure" = paste(
    "Non-probability convenience sampling (Intercept).",
    "Respondents were recruited digitally at the point of drug verification.",
    "Sample is skewed toward urban, educated populations with mobile access."
  ),
  
  "weighting" = "Data is unweighted due to significant design effects (DEff > 2.0).",
  "country"   = "Nigeria",
  "version"   = "1.0"
)

# --- 3. PREPARE VARIABLE METADATA ---
cat("--- 3. Mapping Variable Metadata for XML ---\n")

# The DDIwR package automatically reads the 'label' and 'labels' attributes 
# we created using the 'haven' and 'labelled' packages in Script 05.
# However, we ensure the dataframe is a standard data.frame for export.
df_export <- as.data.frame(df)

# --- 4. EXPORT TO DDI XML ---
cat("--- 4. Generating DDI 2.5 XML File ---\n")

OUTPUT_XML <- "Sproxil_Survey_DDI.xml"

# Use the specific export function instead of convert() to be safer
# OR use convert with to = "DDI"
convert(
  df_export,
  to = "DDI", # <--- CHANGED FROM "DDI-xml" to "DDI"
  file = OUTPUT_XML,
  study_info = study_metadata,
  monolang = FALSE
)

cat(sprintf("✅ SUCCESS: DDI Codebook saved as '%s'.\n", OUTPUT_XML))