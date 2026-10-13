# nhanes-sarcopenia-index
Survey analysis of the composite sarcopenia index and albuminuria in NHANES 2011-2014


[![R Version](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://www.r-project.org/)
[![Survey Statistics](https://img.shields.io/badge/Design-Complex%20Survey-orange.svg)](https://cran.r-project.org/web/packages/survey/index.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository contains the complete analytical pipeline, data wrangling routines, and statistical models investigating the association between validated **sarcopenia indices** (appendicular lean mass, grip strength components) and **subclinical microvascular renal impairment** (urinary albumin-to-creatinine ratio, uACR) utilizing multi-cycle data from the National Health and Nutrition Examination Survey (NHANES).

---

## 📂 Repository Structure

```text
nhanes-sarcopenia-index/
├── data/                      # Raw XPT extracts and harmonized analytical data frames (local)
├── scripts/                   # R pipeline scripts
│   ├── baseline_export.R      # Cohort derivation, inclusion/exclusion filtering
│   ├── datafile_extraction_merger.R # NHANES cycle merging (DEMO, DXX_E, ALB_CR, BIOPRO)
│   ├── survey_regression.R    # Complex survey-weighted multivariable linear/logistic models
│   └── vif_rcs.R              # Multicollinearity diagnostics (VIF) and Restricted Cubic Splines
├── outputs/                   # Generated regression tables, diagnostic plots, and figures
├── README.md                  # Project documentation
└── LICENSE                    # MIT Open-Source License

## Methodology & Analytical Approach :

# Complex Survey Design Accounting: Incorporates masked variance units (SDMVPSU), strata (SDMVSTRA), and mobile examination center (MEC) sample weights (WTMEC2YR) across multiple NHANES survey cycles to ensure nationally representative estimates

# Data Harmonization & Merging (scripts/datafile_extraction_merger.R):Demographic files (DEMO) for age, sex, race/ethnicity, and poverty-income ratio (PIR).Dual-energy X-ray absorptiometry (DXX_E) for appendicular lean mass (ALM) quantification.

# Laboratory markers (ALB_CR, BIOPRO) for urinary albumin, urinary creatinine, serum creatinine (eGFR calculation via CKD-EPI), and glycemic parameters.

# Statistical Modeling (scripts/survey_regression.R & scripts/vif_rcs.R):Survey-weighted multivariable linear regression evaluating log(uACR) as the primary outcome

# Assessment of multicollinearity using Variance Inflation Factors (VIF).
# Non-linearity testing and dose-response curve estimation via Restricted Cubic Splines (RCS).

 
