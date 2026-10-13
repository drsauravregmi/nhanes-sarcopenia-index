
##----------------BASELINE CHARACTERISTICS TABLE------------------

library(tableone)

# --- 1. Recode Variables and Build Quartiles FIRST ---
q_cuts <- svyquantile(~composite_sarcopenia_index, design = nhanes_design, quantiles = c(0, 0.25, 0.5, 0.75, 1.0))
q_breaks <- as.numeric(q_cuts[[1]][, 1])

q_breaks[1] <- q_breaks[1] - 0.01
q_breaks[5] <- q_breaks[5] + 0.01

nhanes_analytic <- nhanes_analytic %>%
  mutate(
    sarcopenia_q = cut(
      composite_sarcopenia_index,
      breaks = q_breaks,
      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
    ),
    Gender = factor(RIAGENDR, levels = c("1", "2"), labels = c("Male", "Female")),
    Race_Ethnicity = factor(
      RIDRETH1,
      levels = c("1", "2", "3", "4", "5"),
      labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other/Multi-Racial")
    ),
    Ever_Smoked = factor(ever_smoked, levels = c(0, 1), labels = c("No", "Yes"))
  )

# --- 2. Re-create Design Object AFTER Mutating Data ---
nhanes_design <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC4YR,
  nest    = TRUE,
  data    = nhanes_analytic
)

# --- 3. Run Table 1 ---
vars <- c(
  "RIDAGEYR", "Gender", "Race_Ethnicity", "INDFMPIR", 
  "BMXBMI", "BMXWAIST", "ALM_kg", "MGDCGSZ", 
  "composite_sarcopenia_index", "uACR", "LBXSGL", 
  "pulse_pressure", "Ever_Smoked"
)

factorVars <- c("Gender", "Race_Ethnicity", "Ever_Smoked")
nonNormalVars <- c("uACR", "LBXSGL")

table1_weighted <- svyCreateTableOne(
  vars       = vars,
  strata     = "sarcopenia_q",
  data       = nhanes_design,
  factorVars = factorVars,
  test       = TRUE
)

# Print Table 1
print(
  table1_weighted, 
  nonnormal     = nonNormalVars,
  showAllLevels = FALSE, 
  formatOptions = list(big.mark = ","),
  noSpaces      = TRUE
)


# Export regression summary table
write.csv(summary_table, "nhanes_sarcopenia_uacr_regressions.csv", row.names = FALSE)

# Export Table 1 as a data frame
table1_df <- print(table1_weighted, nonnormal = nonNormalVars, showAllLevels = FALSE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(table1_df, "nhanes_baseline_table1.csv")
