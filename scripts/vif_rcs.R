# ==============================================================================
# ADVANCED BIOMETRIC MODELING: VIF DIAGNOSTICS & RESTRICTED CUBIC SPLINES
# ==============================================================================

library(rms)
library(car)


# ==============================================================================
# PART 1: VARIANCE INFLATION FACTOR (VIF) CHECK FOR MODEL 4c
# ==============================================================================

cat("\n--- Running VIF Diagnostic for Model 4c (Mass & Strength Co-Model) ---\n")

# Fit an unweighted proxy linear model to check collinearity metric
vif_model <- lm(log(uACR) ~ z_ALM_BMI + z_Grip_BMI + RIDAGEYR + RIAGENDR + BMXBMI, data = nhanes_design$variables)

# Calculate VIF values
vif_results <- car::vif(vif_model)
print(vif_results)


# ==============================================================================
# PART 2: RESTRICTED CUBIC SPLINES (RCS) FOR NON-LINEARITY ANALYSIS
# ==============================================================================

cat("\n--- Setting up Restricted Cubic Splines using 'rms' ---\n")

dd <- datadist(nhanes_design$variables)
options(datadist = "dd")

# Using log(uACR) as the dependent variable to account for right-skewness
rcs_model <- ols(log(uACR) ~ rcs(composite_sarcopenia_index, 4) + RIDAGEYR + RIAGENDR + BMXBMI, 
                 data = nhanes_design$variables)

# Print model summary to inspect non-linear terms (nonlinear p-value)
print(rcs_model)

# 3. Test specifically for non-linearity (Wald test for nonlinear knots)
anova_rcs <- anova(rcs_model)
print(anova_rcs)


