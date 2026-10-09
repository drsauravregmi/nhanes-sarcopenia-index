# Specify Survey Design
nhanes_design <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC4YR,
  nest    = TRUE,
  data    = nhanes_analytic
)

# --- Regression Models ---
m1 <- svyglm(log_uACR ~ composite_sarcopenia_index, design = nhanes_design)

m2 <- svyglm(
  log_uACR ~ composite_sarcopenia_index + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1),
  design = nhanes_design
)

m3 <- svyglm(
  log_uACR ~ composite_sarcopenia_index + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1) +
    LBXSGL + pulse_pressure,
  design = nhanes_design
)

m4a_mass <- svyglm(
  log_uACR ~ z_ALM_BMI + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1) +
    LBXSGL + pulse_pressure,
  design = nhanes_design
)

m4b_strength <- svyglm(
  log_uACR ~ z_Grip_BMI + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1) +
    LBXSGL + pulse_pressure,
  design = nhanes_design
)

m4c_comodel <- svyglm(
  log_uACR ~ z_ALM_BMI + z_Grip_BMI + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1) +
    LBXSGL + pulse_pressure,
  design = nhanes_design
)

m5_denominator <- svyglm(
  log_URXUMA ~ composite_sarcopenia_index + log_URXUCR + RIDAGEYR + factor(RIAGENDR) + 
    factor(RIDRETH1) + LBXSGL + pulse_pressure,
  design = nhanes_design
)

m6_lifestyle <- svyglm(
  log_uACR ~ composite_sarcopenia_index + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1) +
    LBXSGL + pulse_pressure + ever_smoked + INDFMPIR,
  design = nhanes_design
)

# --- Summary Results Extraction ---
extract_model_results <- function(model, model_name = "Model") {
  coef_mat <- summary(model)$coefficients
  df_res <- data.frame(
    Model      = model_name,
    Term       = rownames(coef_mat),
    Beta       = coef_mat[, 1],
    SE         = coef_mat[, 2],
    p_value    = coef_mat[, 4],
    Pct_Change = (exp(coef_mat[, 1]) - 1) * 100,
    CI_Lower   = (exp(coef_mat[, 1] - 1.96 * coef_mat[, 2]) - 1) * 100,
    CI_Upper   = (exp(coef_mat[, 1] + 1.96 * coef_mat[, 2]) - 1) * 100
  )
  return(df_res)
}

summary_table <- rbind(
  extract_model_results(m1, "Model 1: Unadjusted"),
  extract_model_results(m2, "Model 2: Demographic Adjusted"),
  extract_model_results(m3, "Model 3: Fully Adjusted"),
  extract_model_results(m4a_mass, "Model 4a: Mass Only (z_ALM)"),
  extract_model_results(m4b_strength, "Model 4b: Strength Only (z_Grip)"),
  extract_model_results(m4c_comodel, "Model 4c: Independent Co-Model"),
  extract_model_results(m5_denominator, "Model 5: Denominator Sensitivity"),
  extract_model_results(m6_lifestyle, "Model 6: Extended Lifestyle")
)

exposure_summary <- subset(
  summary_table, 
  Term %in% c("composite_sarcopenia_index", "z_ALM_BMI", "z_Grip_BMI")
)

print(exposure_summary, row.names = FALSE)
