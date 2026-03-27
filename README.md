# 🦟 Sproxil Malaria Insight Survey Analysis

![R](https://img.shields.io/badge/R-4.2%2B-blue)
![Status](https://img.shields.io/badge/status-production--ready-brightgreen)
![License](https://img.shields.io/badge/license-internal--use-lightgrey)
![Reproducibility](https://img.shields.io/badge/reproducible-yes-success)

---

## 📌 Overview

This repository contains the analytical code used to produce outputs for the **Sproxil Malaria Insight Survey**. It implements a structured, reproducible pipeline from raw survey data to final report-ready tables and outputs.

The workflow follows a **DHS/MIS-inspired analytical framework**, adapted to the realities of the Sproxil dataset.

---

## 🎯 Objectives

This repository supports:

- Transformation of raw survey data into an analysis-ready dataset
- Construction of standardised indicators and derived variables
- Application of survey weighting and calibration
- Generation of MIS-style tabulations
- Quality assurance and consistency checks
- Transparent and reproducible analytical workflows

---

## 🗂️ Repository Structure

```
sproxil-malaria-insight-survey/
│
├── README.md
├── .gitignore
├── LICENSE
│
├── data/
│   ├── raw/
│   ├── interim/
│   ├── processed/
│   └── external/
│
├── scripts/
│   ├── 01_metadata_setup.R
│   ├── 02_data_preparation.R
│   ├── 03_data_cleaning_recode.R
│   ├── 04_variable_derivation.R
│   ├── 05_calibration_weighting.R
│   ├── 06_stratification_setup.R
│   ├── 07_missingness_QA.R
│   ├── 08_change_log_documentation.R
│   ├── 09_analysis_ready_dataset_export.R
│   ├── 10_Sproxil_Master_Tabulation_Engine_Phase1.R
│   ├── 11_Sproxil_Master_Tabulation_Engine_Phase2A.R
│   ├── 12_Sproxil_Master_Tabulation_Engine_Phase2B.R
│   └── 99_run_pipeline.R
│
├── outputs/
│   ├── logs/
│   ├── qa/
│   ├── tables/
│   ├── figures/
│   └── workbooks/
│
├── docs/
│   ├── data_dictionary/
│   ├── methodology/
│   ├── table_shells/
│   └── change_logs/
│
└── archive/
```

---

## ⚙️ Analytical Pipeline

| Stage | Script | Description |
|------|--------|-------------|
| 1 | `01_metadata_setup.R` | Define labels, mappings, and metadata |
| 2 | `02_data_preparation.R` | Import and structure raw data |
| 3 | `03_data_cleaning_recode.R` | Clean and recode variables |
| 4 | `04_variable_derivation.R` | Create analytical indicators |
| 5 | `05_calibration_weighting.R` | Apply survey weights and diagnostics |
| 6 | `06_stratification_setup.R` | Define subgroups |
| 7 | `07_missingness_QA.R` | Data quality checks |
| 8 | `08_change_log_documentation.R` | Track analytical decisions |
| 9 | `09_analysis_ready_dataset_export.R` | Export final dataset |
| 10 | `10-12_tabulation_*.R` | Generate tables |
| 11 | `99_run_pipeline.R` | Run full pipeline |

---

## 📦 Requirements

### Software

- R (>= 4.2)
- RStudio (recommended)

### Packages

```r
install.packages(c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "readr", "readxl", "openxlsx", "janitor",
  "survey", "srvyr", "forcats", "lubridate", "ggplot2"
))
```

---

## ▶️ How to Run

### Option 1: Step-by-step

Run scripts sequentially from `01_metadata_setup.R` to `12_Sproxil_Master_Tabulation_Engine_Phase2B.R`.

### Option 2: Full pipeline

```r
source("scripts/99_run_pipeline.R")
```

---

## 📥 Inputs Required

- Raw survey dataset
- Data dictionary / metadata
- Recode specifications
- External population benchmarks
- Lookup tables

---

## 📤 Outputs

- Cleaned datasets
- Analysis-ready dataset
- QA reports
- Weighting diagnostics
- MIS-style tables
- Excel workbooks

---

## 🔁 Reproducibility

- Raw data is preserved and not overwritten
- All transformations are script-based
- Analytical decisions are documented
- Production weights are fixed
- Outputs are version-controlled

---

## 🔐 Data Confidentiality

⚠️ This repository may reference confidential survey data.

- Do not upload raw datasets publicly
- Remove credentials before sharing
- Share only approved outputs

---

## 👤 Maintainer

**Name:** [Your Name]  
**Organisation:** Corona Management Systems  
**Project:** Sproxil Malaria Insight Survey

---

## 📄 License

Internal and authorised use only.
