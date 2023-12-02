## load in libraries for cleaning
library(janitor)
library(tidyverse)
library(readxl)

# load helpers
source("scripts/helpers.R")

# get path to raw excel file
source("data/dataLoad.R")

#Group 1: ECLS; Variable: “Baseline (Arm 1: ECLS)” and “ECLS (Arm 1: ECLS)”
group_1 <- c("baseline_arm_1", "ecls_arm_1")
group_2 <- c("baseline_arm_2", "ecls_arm_2", "impella_arm_2")

#Group 2: ECMELLA, Variable: “Baseline (Arm 2: ECLS + Impella)” and 
#         “ECLS  (Arm 2: ECLS + Impella)” or “Impella (Arm 2: ECLS + Impella)”

other_cs <- c("“{reason_ecls_other}", "Acute Lung Injury", 
              "Ventricular Septal Defect", "Pulmonary Embolism", 
              "Postpartum Cardiomyopathy", "Toxic Cardiomyopathy", 
              "Valvular Cardiomyopathy", "Myokarditis")


clean <- data %>%
  # create groupings
  mutate(
    group = case_when(redcap_event_name %in% group_1 ~ "ECLS",
                      redcap_event_name %in% group_2 ~ "ECMELLA")
    ) %>%
  
  # PSM features
  mutate(
    age       = coalesce(data$age_ecls_1, data$age_ecls_2),
    sex       = sex.factor,
    bmi       = (weight / (height/100)^2), 
    lactate   = pre_lactate,          # TODO: the correct lactate?
    vis_score = as.numeric(pre_vis)   # TODO: correct vis score?
    ) %>%
  
  # table 1
  mutate(
    diabetes    = as.logical(diabetes),
    ckd_yn      = as.logical(ckd),
    ckd_stage   = as.factor(ckd_spec),
    copd_yn     = as.logical(copd),
    copd_stage  = as.factor(copd_spec),
    ph          = ph,
    rrt_type    = renal_repl.factor,
    cr          = crea,
    mi_yn       = as.logical(pre_cardiac_arrest),
    postcard    = as.logical(pre_postcard),
    cpb_fail    = as.logical(pre_failure_cpb),
    ecpr        = (reason_ecls.factor == "Cardiopulmonary Reanimation"),
    #nephtox_rx = ... TODO: need order of antibiotic_spec___1
    ) %>%
  
  # causes of cardiogenic shock
  mutate(
    # AMICS
    cs_amics = case_when(
    (reason_ecls.factor == "Cardiopulmonary Reanimation" & mi_yn) | reason_ecls.factor == "Acute Myocardial Infarction" ~ T,
    .default = F),
    
    # AHF-CS
    cs_ahf = case_when(
      reason_ecls.factor %in% c("Dilatative Cardiomyopathy", "Ischemic Cardiomyopathy") ~ T,
      .default = F),
    
    # PCCS
    cs_pccs = case_when(
      reason_ecls.factor == "Cardiopulmonary Reanimation" & (postcard | cpb_fail) ~ T,
      .default = F),
    # OTHER
    cs_other = case_when(
      (reason_ecls.factor == "Cardiopulmonary Reanimation" & !postcard & !cpb_fail & !mi_yn) | reason_ecls.factor %in% other_cs ~ T,
      .default = F)
    ) %>%
  
  mutate(
    cs_etiology = case_when(
      cs_amics ~ "AMICS",
      cs_ahf ~ "AHF",
      cs_pccs ~ "PCCS",
      cs_other ~ "Other",
      .default = "No cardiogenic shock"
      )
    ) %>%
  
  # aki staging
  mutate(
    #aki_1 = ...
    #aki_2 = ...
    #aki_3 = ...
    rrt_yn = (rrt_type %in% c("Continuous Hemofiltration", "Hemodialysis"))
    ) %>%
  
  # fill vertically for time-invariant factors
  group_by(record_id) %>%
  fill(id, age, sex, bmi, vis_score, diabetes, ckd_yn, ckd_stage, copd_yn, 
       copd_stage, mi_yn, postcard, cpb_fail, ecpr,
       .direction = "downup")

  # time (vertical) variables
  # TODO: how is best to pivot this?
  #mutate(
    #rrt_duration = ...
    #vent_duration = ...
    #icu_los = ...
    #hosp_los = ...
    #hosp_surv_yn = ...
  #)

constructed <- clean %>%
  select(record_id, redcap_event_name, redcap_repeat_instrument, 
         redcap_repeat_instance, id, 
         group, age, sex, bmi, lactate, vis_score, diabetes, ckd_yn, 
         ckd_stage, copd_yn, copd_stage, ph, rrt_yn, rrt_type, cr, mi_yn, 
         postcard,cpb_fail, ecpr, cs_etiology)

# examine data distribution and types
write_csv(constructed, "data/cleaned_analysis_data.csv",
          na = "NA", append = FALSE)
