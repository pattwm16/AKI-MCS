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
  
  # antibiotic list
  mutate(
    any_abx    = as.logical(antibiotic),
    cefuroxim  = as.logical(antibiotic_spec___1),
    piptazo    = as.logical(antibiotic_spec___2),
    meropenem  = as.logical(antibiotic_spec___3),
    vanc_iv    = as.logical(antibiotic_spec___4),
    vanc_po    = as.logical(antibiotic_spec___5),
    linezolid  = as.logical(antibiotic_spec___6),
    dapto      = as.logical(antibiotic_spec___7),
    pcn_g      = as.logical(antibiotic_spec___8),
    flucoxciln = as.logical(antibiotic_spec___9),
    rifampicin = as.logical(antibiotic_spec___10),
    gentamycin = as.logical(antibiotic_spec___11),
    tobramycin = as.logical(antibiotic_spec___12),
    ciproflox  = as.logical(antibiotic_spec___13),
    other_abx  = as.logical(antibiotic_spec___14),
    erythromyc = as.logical(antibiotic_spec___15),
    caspofungn = as.logical(antibiotic_spec___16),
    amph_b_inh = as.logical(antibiotic_spec___17),
    metronid   = as.logical(antibiotic_spec___18)
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
    cr          = as.numeric(crea),
    mi_yn       = as.logical(pre_cardiac_arrest),
    postcard    = as.logical(pre_postcard),
    cpb_fail    = as.logical(pre_failure_cpb),
    ecpr        = (reason_ecls.factor == "Cardiopulmonary Reanimation"),
    aki_yn      = as.logical(aki),
    nephtox_rx  = (vanc_iv | gentamycin | tobramycin)
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
  fill(id, age, sex, bmi, vis_score, diabetes, death, ckd_yn, ckd_stage, copd_yn, 
       copd_stage, lactate, mi_yn, postcard, cpb_fail, ecpr, cs_etiology,
       .direction = "downup")

  # time (vertical) variables
  # TODO: how is best to pivot this?

# duration of RRT
rrt_duration <- clean %>% 
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & rrt_yn) & (!rrt_type %in% c(NA, "No"))) %>%
  mutate(rrt_duration = as.difftime(n, units = "days")) %>% 
  select(record_id, rrt_duration) 

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

# take median/max/min of creatinine and ph
ph_cr_median <- vent_duration %>%
  filter(redcap_repeat_instrument == "labor") %>%
  group_by(id) %>%
  mutate(med_cr = median(cr, na.rm = T),
         max_cr = max(cr, na.rm = F),
         min_cr = min(cr, na.rm = F),
         med_ph = median(ph, na.rm = T),
         max_ph = max(ph, na.rm = F),
         min_ph = min(ph, na.rm = F)) %>%
  ungroup() %>%
  select(record_id, med_cr, max_cr, min_cr, 
         med_ph, max_ph, min_ph) %>% 
  group_by(record_id) %>%
  filter(row_number()==1)

## patch column
vent_duration <- vent_duration %>% 
  add_column(med_cr = NA, med_ph = NA,
             min_cr = NA, min_ph = NA,
             max_cr = NA, max_ph = NA) %>%
  mutate(med_cr = as.double(med_cr),
         med_ph = as.double(med_ph),
         min_cr = as.double(min_cr),
         min_ph = as.double(min_ph),
         max_cr = as.double(max_cr),
         max_ph = as.double(max_ph)) %>%
  rows_patch(ph_cr_median)

    #icu_los = ...
    #hosp_los = ...
    #hosp_surv_yn = ...

# create ever received nephrotoxic drugs
nephrotox_rx <- vent_duration %>%
  filter(redcap_repeat_instrument == "hemodynamics_ventilation_medication") %>%
  group_by(id) %>%
  mutate(rx_nephrotox = case_when(is.na(nephtox_rx) ~ NA,
                                  any(nephtox_rx == T) ~ T,
                                  .default = F),
         abx_yn       = any(any_abx == T)
         ) %>%
  ungroup() %>%
  select(record_id, rx_nephrotox, any_abx) %>%
  group_by(record_id) %>%
  filter(row_number()==1)

## patch column
vent_duration <- vent_duration %>% 
  add_column(rx_nephrotox = NA, abx_yn = NA) %>%
  rows_patch(nephrotox_rx)


constructed <- vent_duration %>%
  select(record_id, id, group, age, sex, med_cr, min_cr, max_cr, med_ph, min_ph,
         max_ph, bmi, lactate, vis_score, diabetes, ckd_yn, ckd_stage, copd_yn, 
         copd_stage, rrt_yn,rrt_type, rrt_duration, mi_yn, postcard, cpb_fail, 
         ecpr, cs_etiology, vent_type, vent_duration, extub_reason, extub_date, 
         intub_date, death, death_date, aki_yn, abx_yn, rx_nephrotox,
         any_abx, cefuroxim, piptazo, meropenem, vanc_iv, vanc_po, linezolid,
         dapto, pcn_g, flucoxciln, rifampicin, gentamycin, tobramycin, 
         ciproflox, other_abx, erythromyc, caspofungn, amph_b_inh, metronid
         ) %>%
  group_by(record_id) %>%
  filter(row_number()==1) %>% # condense to a single row per patient
  filter(!is.na(death))       # only include patient with outcomes


# examine data distribution and types
write_csv(constructed, "../data/cleaned_analysis_data.csv",
          na = "NA", append = FALSE)
