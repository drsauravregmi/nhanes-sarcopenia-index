library(nhanesA)
library(tidyverse)
library(survey)

# Load Datasets (G is 2011-2012, H is 2013-2014)
demo_g   <- nhanes("DEMO_G")
bmx_g    <- nhanes("BMX_G")
mgx_g    <- nhanes("MGX_G")
dxx_g    <- nhanes("DXX_G")
alb_g    <- nhanes("ALB_CR_G")
bpx_g    <- nhanes("BPX_G")
kiq_g    <- nhanes("KIQ_U_G")
biopro_g <- nhanes("BIOPRO_G")
smq_g    <- nhanes("SMQ_G")

demo_h   <- nhanes("DEMO_H")
bmx_h    <- nhanes("BMX_H")
mgx_h    <- nhanes("MGX_H")
dxx_h    <- nhanes("DXX_H")
alb_h    <- nhanes("ALB_CR_H")
bpx_h    <- nhanes("BPX_H")
kiq_h    <- nhanes("KIQ_U_H")
biopro_h <- nhanes("BIOPRO_H")
smq_h    <- nhanes("SMQ_H")



# Initialize flowchart tracker
flowchart_tracker <- data.frame(Step = character(), Unweighted_n = numeric(), Weighted_Total = numeric(), stringsAsFactors = FALSE)

log_step <- function(step_name, data) {
  unw_n <- nrow(data)
  wt_tot <- if("WTMEC4YR" %in% names(data)) sum(data$WTMEC4YR, na.rm = TRUE) else NA
  flowchart_tracker <<- rbind(flowchart_tracker, data.frame(Step = step_name, Unweighted_n = unw_n, Weighted_Total = wt_tot))
}

# Load Datasets (Cycle G: 2011-2012 & Cycle H: 2013-2014)
demo_g   <- nhanes("DEMO_G"); bmx_g   <- nhanes("BMX_G"); mgx_g   <- nhanes("MGX_G"); dxx_g   <- nhanes("DXX_G"); alb_g   <- nhanes("ALB_CR_G"); bpx_g   <- nhanes("BPX_G"); kiq_g   <- nhanes("KIQ_U_G"); biopro_g <- nhanes("BIOPRO_G"); smq_g   <- nhanes("SMQ_G")
demo_h   <- nhanes("DEMO_H"); bmx_h   <- nhanes("BMX_H"); mgx_h   <- nhanes("MGX_H"); dxx_h   <- nhanes("DXX_H"); alb_h   <- nhanes("ALB_CR_H"); bpx_h   <- nhanes("BPX_H"); kiq_h   <- nhanes("KIQ_U_H"); biopro_h <- nhanes("BIOPRO_H"); smq_h   <- nhanes("SMQ_H")

# Cycle G Merging
g_sub <- demo_g %>% 
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, RIDEXPRG, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR) %>%
  left_join(bmx_g    %>% select(SEQN, BMXHT, BMXWT, BMXBMI, BMXWAIST), by = "SEQN") %>%
  left_join(mgx_g    %>% select(SEQN, MGDCGSZ), by = "SEQN") %>%
  left_join(dxx_g    %>% select(SEQN, DXDLALE, DXDRALE, DXDLLLE, DXDRLLE), by = "SEQN") %>%
  left_join(biopro_g %>% select(SEQN, LBXSCR, LBXSGL), by = "SEQN") %>%
  left_join(alb_g    %>% select(SEQN, URXUMA, URXUCR), by = "SEQN") %>%
  left_join(bpx_g    %>% select(SEQN, BPXSY1, BPXDI1), by = "SEQN") %>%
  left_join(kiq_g    %>% select(SEQN, KIQ022, KIQ025), by = "SEQN") %>%
  left_join(smq_g    %>% select(SEQN, SMQ020), by = "SEQN")

# Cycle H Merging
h_sub <- demo_h %>% 
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, RIDEXPRG, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR) %>%
  left_join(bmx_h    %>% select(SEQN, BMXHT, BMXWT, BMXBMI, BMXWAIST), by = "SEQN") %>%
  left_join(mgx_h    %>% select(SEQN, MGDCGSZ), by = "SEQN") %>%
  left_join(dxx_h    %>% select(SEQN, DXDLALE, DXDRALE, DXDLLLE, DXDRLLE), by = "SEQN") %>%
  left_join(biopro_h %>% select(SEQN, LBXSCR, LBXSGL), by = "SEQN") %>%
  left_join(alb_h    %>% select(SEQN, URXUMA, URXUCR), by = "SEQN") %>%
  left_join(bpx_h    %>% select(SEQN, BPXSY1, BPXDI1), by = "SEQN") %>%
  left_join(kiq_h    %>% select(SEQN, KIQ022, KIQ025), by = "SEQN") %>%
  left_join(smq_h    %>% select(SEQN, SMQ020), by = "SEQN")

# Step 0: Combined Raw Dataset
combined_raw <- bind_rows(g_sub, h_sub) %>%
  mutate(WTMEC4YR = WTMEC2YR / 2)
log_step("1. Initial Combined NHANES (2011-2014 Cycles)", combined_raw)

# Step-by-step filtering with tracking
step1_age <- combined_raw %>% filter(RIDAGEYR >= 20 & RIDAGEYR <= 59)
log_step("2. Filtered Age 20-59 Years", step1_age)

step2_preg <- step1_age %>% filter(is.na(RIDEXPRG) | RIDEXPRG != 1)
log_step("3. Excluded Pregnancy", step2_preg)

step3_kidney <- step2_preg %>% filter((is.na(KIQ022) | KIQ022 != 1) & (is.na(KIQ025) | KIQ025 != 1))
log_step("4. Excluded Prior Kidney Disease", step3_kidney)

# Final Transform & Complete-Case Analytical Sample
nhanes_analytic <- step3_kidney %>%
  mutate(
    RIAGENDR = as.character(RIAGENDR),
    RIDRETH1 = as.character(RIDRETH1),
    ALM_kg = (DXDLALE + DXDRALE + DXDLLLE + DXDRLLE) / 1000,
    ALM_BMI = ALM_kg / BMXBMI,
    Grip_BMI = MGDCGSZ / BMXBMI,
    z_ALM_BMI = scale(ALM_BMI)[,1],   
    z_Grip_BMI = scale(Grip_BMI)[,1],  
    composite_sarcopenia_index = z_ALM_BMI + z_Grip_BMI,
    uACR = (URXUMA / URXUCR) * 100,
    log_uACR = log(uACR),
    log_URXUMA = log(URXUMA),          
    log_URXUCR = log(URXUCR),          
    pulse_pressure = BPXSY1 - BPXDI1,
    ever_smoked = case_when(
      as.numeric(SMQ020) == 1 ~ 1,
      as.numeric(SMQ020) == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(composite_sarcopenia_index) & !is.na(uACR))

log_step("5. Final Complete-Case Analytic Cohort", nhanes_analytic)

# Print final STROBE flowchart numbers table
print(flowchart_tracker)

cat("Analytic Sample N =", nrow(nhanes_analytic), "\n")

# Specify Survey Design
nhanes_design <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC4YR,
  nest    = TRUE,
  data    = nhanes_analytic
)
