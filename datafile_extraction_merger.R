install.packages("nhanesA")
library(nhanesA)
library(tidyverse)

# DEMO: Demographics & survey weights 
# BMX: Body measures 
# MGX: Muscle strength (grip test)
# DXX: DXA body composition (appendicular lean mass)
# BIOPRO: Standard blood lab chemistry panel
# ALB_CR: Urine albumin & creatinine
# BPX: Blood pressure (systolic & diastolic)
# KIQ_U: Kidney conditions questionnaire (CKD & dialysis)

# Cycle G is 2011-2012

demo_g   <- nhanes("DEMO_G")
bmx_g    <- nhanes("BMX_G")
mgx_g    <- nhanes("MGX_G")
dxx_g    <- nhanes("DXX_G")
alb_g    <- nhanes("ALB_CR_G")
bpx_g    <- nhanes("BPX_G")
kiq_g    <- nhanes("KIQ_U_G")
biopro_g <- nhanes("BIOPRO_G")

# Cycle H is 2013-2014


demo_h   <- nhanes("DEMO_H")
bmx_h    <- nhanes("BMX_H")
mgx_h    <- nhanes("MGX_H")
dxx_h    <- nhanes("DXX_H")
alb_h    <- nhanes("ALB_CR_H")
bpx_h    <- nhanes("BPX_H")
kiq_h    <- nhanes("KIQ_U_H")
biopro_h <- nhanes("BIOPRO_H")

# Now to merge required variables :


# Cycle G
g_sub <- demo_g %>% 
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, RIDEXPRG, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(bmx_g    %>% select(SEQN, BMXHT, BMXWT, BMXBMI, BMXWAIST), by = "SEQN") %>%
  left_join(mgx_g    %>% select(SEQN, MGDCGSZ), by = "SEQN") %>%
  left_join(dxx_g    %>% select(SEQN, DXDLALE, DXDRALE, DXDLLLE, DXDRLLE), by = "SEQN") %>%
  left_join(biopro_g %>% select(SEQN, LBXSCR, LBXSGL), by = "SEQN") %>%
  left_join(alb_g    %>% select(SEQN, URXUMA, URXUCR), by = "SEQN") %>%
  left_join(bpx_g    %>% select(SEQN, BPXSY1, BPXDI1), by = "SEQN") %>%
  left_join(kiq_g    %>% select(SEQN, KIQ022, KIQ025), by = "SEQN")

# Cycle H
h_sub <- demo_h %>% 
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, RIDEXPRG, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(bmx_h    %>% select(SEQN, BMXHT, BMXWT, BMXBMI, BMXWAIST), by = "SEQN") %>%
  left_join(mgx_h    %>% select(SEQN, MGDCGSZ), by = "SEQN") %>%
  left_join(dxx_h    %>% select(SEQN, DXDLALE, DXDRALE, DXDLLLE, DXDRLLE), by = "SEQN") %>%
  left_join(biopro_h %>% select(SEQN, LBXSCR, LBXSGL), by = "SEQN") %>%
  left_join(alb_h    %>% select(SEQN, URXUMA, URXUCR), by = "SEQN") %>%
  left_join(bpx_h    %>% select(SEQN, BPXSY1, BPXDI1), by = "SEQN") %>%
  left_join(kiq_h    %>% select(SEQN, KIQ022, KIQ025), by = "SEQN")

# Creating a master data set and filtering out exclusions 
nhanes_analytic <- bind_rows(g_sub, h_sub) %>%
  mutate(
    WTMEC4YR = WTMEC2YR / 2,    #Accounting for weightage !!
    ALM_kg = (DXDLALE + DXDRALE + DXDLLLE + DXDRLLE) / 1000,
    ALM_BMI = ALM_kg / BMXBMI,
    Grip_BMI = MGDCGSZ / BMXBMI,
    z_ALM = scale(ALM_BMI)[,1],
    z_Grip = scale(Grip_BMI)[,1],
    composite_sarcopenia_index = z_ALM + z_Grip,
    uACR = (URXUMA / URXUCR) * 100,
    log_uACR = log(uACR),
    pulse_pressure = BPXSY1 - BPXDI1
  ) %>%
  filter(
    RIDAGEYR >= 20 & RIDAGEYR <= 59,
    is.na(RIDEXPRG) | RIDEXPRG != 1,
    is.na(KIQ022) | KIQ022 != 1,
    is.na(KIQ025) | KIQ025 != 1,
    !is.na(composite_sarcopenia_index),
    !is.na(uACR)
  )

cat("Analytic Sample N =", nrow(nhanes_analytic), "\n")




library(survey)

# Define the primary sampling units (PSU), strata, and 4-year MEC weights
nhanes_design <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC4YR,
  nest    = TRUE,
  data    = nhanes_analytic
)


# Model 1: Crude / Unadjusted Model
m1 <- svyglm(log_uACR ~ composite_sarcopenia_index, design = nhanes_design)

# Model 2: Adjusted for Core Demographics (Age, Sex, Race)
m2 <- svyglm(
  log_uACR ~ composite_sarcopenia_index + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1), 
  design = nhanes_design
)

# Model 3: Fully Adjusted Model (Demographics + Metabolic/Vascular Covariates)
m3 <- svyglm(
  log_uACR ~ composite_sarcopenia_index + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH1) + 
    LBXSGL + pulse_pressure, 
  design = nhanes_design
)

# Model Summaries
summary(m3)


# Extract summary table for Model 3
coef_m3 <- summary(m3)$coefficients

# Convert log scale beta to percentage change with 95% CIs
results <- data.frame(
  Estimate_Beta = coef_m3[, 1],
  SE            = coef_m3[, 2],
  p_value       = coef_m3[, 4],
  Pct_Change    = (exp(coef_m3[, 1]) - 1) * 100,
  CI_Lower      = (exp(coef_m3[, 1] - 1.96 * coef_m3[, 2]) - 1) * 100,
  CI_Upper      = (exp(coef_m3[, 1] + 1.96 * coef_m3[, 2]) - 1) * 100
)

print(results)


