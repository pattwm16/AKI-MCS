## load in libraries for cleaning
library(janitor)
library(tidyverse)
library(readxl)

# load helpers
source("helpers.R")

# get path to raw excel file
source("dataLoad.R")

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
                      redcap_event_name %in% group_2 ~ "ECMELLA") %>% as.factor()
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
  
  # collect etiologies (logical) as single column
  mutate(
    cs_etiology = case_when(
      cs_amics ~ "AMICS",
      cs_ahf ~ "AHF",
      cs_pccs ~ "PCCS",
      cs_other ~ "Other",
      .default = NA
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
       copd_stage, lactate, mi_yn, postcard, cpb_fail, ecpr, cs_etiology,
       .direction = "downup")

  # time (vertical) variables
  # TODO: how is best to pivot this?

# duration of RRT
rrt_duration <- clean %>% 
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & rrt_yn) & (!rrt_type %in% c(NA, "No"))) %>%
  mutate(rrt_duration = as.difftime(n, units = "days")) %>% 
  select(record_id, rrt_duration) %>%
  filter(rrt_duration != 0)

## patch column
clean <- clean %>% 
  add_column(rrt_duration = NA) %>%
  mutate(rrt_duration = as.difftime(as.character(rrt_duration), units = "days")) %>%
  rows_patch(rrt_duration)

# duration of ventilation
vent_duration <- clean %>% 
  mutate(
    vent_type = as.factor(vent.factor),
    extub_reason = as.factor(extub.factor),
    intub_pre_ecls = as.logical(intubation),
    extub_date = as.Date(extub_date, format = "%m/%d/%y"),
    intub_date = as.Date(ventil_start_date, format = "%m/%d/%y"),
    death_date = as.Date(death_date, format = "%m/%d/%y")
    ) %>% 
  
  # fill in vent type and edge dates for all rows of record_id
  fill(extub_date, intub_date, extub_reason, intub_pre_ecls,
       .direction = "downup") %>%
  
  # group for different approaches to calculate ventilation time
  mutate(
    tubed_prior_ecls_w_date = (intub_pre_ecls & ventil_date),
    tubed_prior_ecls_wo_date = (intub_pre_ecls & !ventil_date),
    nottubed_prior_ecls_w_date = (!intub_pre_ecls & ventil_date),
    nottubed_prior_ecls_wo_date = (!intub_pre_ecls & !ventil_date),
    vent_duration = case_when(tubed_prior_ecls_w_date & (extub.factor %in% c("no extubation before death", "intubated at time of death")) ~ death_date - intub_date,
                              extub.factor == "orotracheally extubated" ~ extub_date - intub_date)
    ) %>%
  fill(tubed_prior_ecls_w_date, tubed_prior_ecls_wo_date,
       nottubed_prior_ecls_w_date, nottubed_prior_ecls_wo_date,
       .direction = "downup")

# intubated before ECLS with date
tracheostomy <- vent_duration %>% 
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & tubed_prior_ecls_w_date & (extub_reason == "tracheostomy")) & (vent_type != "Invasive Ventilation")) %>% 
  mutate(vent_duration = as.difftime(n, units = "days")) %>% 
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>% 
  rows_patch(tracheostomy)

## transferred while intubated or with tracheostomy
trnsf_prior_intub <- vent_duration %>% 
  tally(redcap_repeat_instrument == "hemodynamics_ventilation_medication" & tubed_prior_ecls_wo_date & (extub_reason %in% c("transferred with tracheostoma", "transferred intubated")) & (vent_type == "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>% 
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>% 
  rows_patch(trnsf_prior_intub)
  
# intubated before ECLS without date
tube_prior_wo_date <- vent_duration %>% 
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & tubed_prior_ecls_wo_date) & (vent_type != "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>% 
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>% 
  rows_patch(tube_prior_wo_date)

# intubated after ECLS with date
# TODO: not sure what "ON MCS" means...
tube_after_w_date <- vent_duration %>% 
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & nottubed_prior_ecls_w_date) & (vent_type != "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>% 
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>% 
  rows_patch(tube_after_w_date)

# intubated after ECLS with date
tube_after_wo_date <- vent_duration %>% 
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & nottubed_prior_ecls_wo_date) & (vent_type != "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>% 
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>% 
  rows_patch(tube_after_wo_date)

    #icu_los = ...
    #hosp_los = ...
    #hosp_surv_yn = ...

constructed <- vent_duration %>%
  select(record_id, id, group, age, sex, bmi, lactate, vis_score, diabetes, 
         ckd_yn, ckd_stage, copd_yn, copd_stage, rrt_yn, rrt_type, rrt_duration,
         mi_yn, postcard, cpb_fail, ecpr, cs_etiology, vent_duration) %>%
  group_by(record_id) %>%
  filter(row_number()==1) # condense to a single row per patient

# ph and creatinine have multiple various length readings for each patient

# examine data distribution and types
write_csv(constructed, "../data/cleaned_analysis_data.csv",
          na = "NA", append = FALSE)
