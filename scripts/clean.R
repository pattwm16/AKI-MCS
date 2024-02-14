## load in libraries for cleaning
library(janitor)
library(readxl)
library(haven)
library(tidyverse)

# load helpers
source("scripts/helpers.R")

# get path to raw excel file
source("scripts/data_with_icu_los.R")

#Group 1: ECLS; Variable: “Baseline (Arm 1: ECLS)” and “ECLS (Arm 1: ECLS)”
#Group 2: ECMELLA, Variable: “Baseline (Arm 2: ECLS + Impella)” and
#         “ECLS  (Arm 2: ECLS + Impella)” or “Impella (Arm 2: ECLS + Impella)”
group_1 <- c("baseline_arm_1", "ecls_arm_1")
group_2 <- c("baseline_arm_2", "ecls_arm_2", "impella_arm_2")

# cs etiologies for later selection
other_cs <- c("{reason_ecls_other}", "Acute Lung Injury",
              "Ventricular Septal Defect", "Pulmonary Embolism",
              "Postpartum Cardiomyopathy", "Toxic Cardiomyopathy",
              "Valvular Cardiomyopathy", "Myokarditis")

# fill down aki values
data <- data %>%
  group_by(record_id) %>%
  fill(aki, .direction = "down")

clean <- data %>%
  # create groupings
  mutate(
    group = as.factor(case_when(redcap_event_name %in% group_1 ~ "ECLS",
                                redcap_event_name %in% group_2 ~ "ECMELLA")),
    death = as.logical(death)
  ) %>%

  # potential confounders
  mutate(
    age         = coalesce(age_ecls_1, age_ecls_2),
    sex         = sex.factor,
    bmi         = (weight / (height / 100)^2),

    # pre tMCS
    pre_lactate = pre_lactate,
    pre_cr      = as.numeric(pre_crea),
    pre_ph      = as.numeric(pre_ph),
    vis_score = case_when(log(as.numeric(pre_vis)) > 10 ~ 10,
                          TRUE ~ as.numeric(pre_vis))
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
    diabetes      = as.logical(diabetes),
    ckd_yn        = as.logical(ckd),
    ckd_stage     = as.factor(ckd_spec),
    copd_yn       = as.logical(copd),
    copd_stage    = as.factor(copd_spec),
    ph            = as.numeric(ph),
    pre_rrt_yn    = as.logical(pre_renal_repl),
    pre_rrt_type  = as.factor(pre_renal_repl_spec.factor),
    cr            = as.numeric(crea),
    mi_yn         = as.logical(pre_cardiac_arrest),
    postcard      = as.logical(pre_postcard),
    cpb_fail      = as.logical(pre_failure_cpb),
    ecpr          = (reason_ecls.factor == "Cardiopulmonary Reanimation"),
    aki_yn        = as.logical(aki),
    nephtox_rx    = ifelse((vanc_iv | gentamycin | tobramycin), TRUE, FALSE)
  ) %>%

  # boolean ecls and rrt variables
  mutate(
    ecls_start = parse_date_time(paste(ecls_start_date, ecls_start_time), "%m/%d/%y %H:%M:%S"),
    ecls_stop = parse_date_time(paste(ecls_stop_date, ecls_stop_time), "%m/%d/%y %H:%M:%S"),
    impella_start = parse_date_time(paste(impella_start_date, impella_start_time), "%m/%d/%y %H:%M:%S"),
    impella_stop = parse_date_time(paste(impella_stop_date, impella_stop_time), "%m/%d/%y %H:%M:%S"),
    ecls_rrt_type = as.factor(renal_repl.factor), # during ecls
  ) %>%

  # causes of cardiogenic shock
  mutate(
    # AMICS
    cs_amics = case_when(
    (reason_ecls.factor == "Cardiopulmonary Reanimation" & mi_yn) | reason_ecls.factor == "Acute Myocardial Infarction" ~ TRUE,
    .default = FALSE),

    # AHF-CS
    cs_ahf = case_when(
      reason_ecls.factor %in% c("Dilatative Cardiomyopathy", "Ischemic Cardiomyopathy") ~ TRUE,
      .default = FALSE),

    # PCCS
    cs_pccs = case_when(
      reason_ecls.factor == "Cardiopulmonary Reanimation" & (postcard | cpb_fail) ~ TRUE,
      reason_ecls.factor == "Postcardiotomy Syndrom" ~ TRUE,
      reason_ecls.factor == "{reason_ecls_other}" & cpb_fail ~ TRUE,
      .default = FALSE),

    # OTHER
    cs_other = case_when(
      (reason_ecls.factor == "Cardiopulmonary Reanimation" & !postcard & !cpb_fail & !mi_yn) | reason_ecls.factor %in% other_cs ~ TRUE,
      .default = FALSE)
  ) %>%

  # collect etiologies (logical) as single column
  mutate(
    cs_etiology = case_when(
      cs_pccs ~ "PCCS",
      cs_amics ~ "AMICS",
      cs_ahf ~ "AHF",
      cs_other ~ "Other",
      .default = NA_character_
    )
  ) %>%

  # aki staging
  mutate(
    aki_s1 = as.logical((aki_1_1 == 1) | (aki_1_2 == 1)),
    aki_s2 = as.logical((aki_2 == 2)),
    aki_s3 = as.logical(((aki_3_1 == 3) | (aki_3_2 == 3))),
    aki_rrt = as.logical(aki_3_2 == 3),
  ) %>%
  # TODO: add AKI:RRT from protocol

  # fill vertically for time-invariant factors
  group_by(record_id) %>%
  fill(id, age, sex, bmi, vis_score, diabetes, death, ckd_yn, ckd_stage,
       copd_yn,
       copd_stage, pre_lactate, mi_yn, postcard, cpb_fail, ecpr, cs_etiology,
       pre_rrt_yn, pre_rrt_type,
       # TODO: should these be filled here?
       aki_yn, aki_s1, aki_s2, aki_s3, pre_cr, pre_ph,
       ecls_start, ecls_stop, impella_start, impella_stop,
       pat_admit_date, discharg_hospital_date, death_date,
       .direction = "downup") %>%
  # fill NAs with FALSE
  mutate(
    aki_s1  = replace_na(aki_s1, FALSE),
    aki_s2  = replace_na(aki_s2, FALSE),
    aki_s3  = replace_na(aki_s3, FALSE),
    aki_rrt = replace_na(aki_rrt, FALSE),
    aki_yn  = replace_na(aki_yn, FALSE),
    aki_max = case_when(
      aki_rrt ~ "rrt",
      aki_s3 ~ "s3",
      aki_s2 ~ "s2",
      aki_s1 ~ "s1",
      TRUE ~ "no aki"
    )
  ) %>%
  ungroup()

# time (vertical) variables
# duration of RRT
rrt_duration <- clean %>%
  group_by(record_id) %>%
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication") & (!ecls_rrt_type %in% c(NA, "No")),
        name = 'rrt_duration') %>%
  mutate(rrt_duration = as.difftime(rrt_duration, units = "days")) %>%
  # TODO: get rid of all 0 values and return as NA
  select(record_id, rrt_duration)

## patch column
clean <- clean %>%
  add_column(rrt_duration = NA) %>%
  mutate(rrt_duration = as.difftime(as.character(rrt_duration), units = "days")) %>%
  rows_patch(., rrt_duration)

# rrt during ecls
rrt_yn <- clean %>%
  group_by(record_id) %>%
  summarise(rrt_yn = any(!is.na(ecls_rrt_type) & ecls_rrt_type != "No")) %>%
  select(record_id, rrt_yn)

## patch column
clean <- clean %>%
  add_column(rrt_yn = NA) %>%
  rows_patch(., rrt_yn)

# duration of ventilation
vent_duration <- clean %>%
  mutate(
    vent_yn        = as.factor(pre_ventilation),
    vent_type      = as.factor(vent.factor),
    extub_reason   = as.factor(extub.factor),
    intub_pre_ecls = as.logical(intubation),
    intub_date     = parse_date_time(ventil_start_date, "%m/%d/%y"),
    extub_date     = parse_date_time(extub_date, "%m/%d/%y"),
    death_date     = parse_date_time(death_date, "%m/%d/%y")
  ) %>%
  group_by(record_id) %>%
  # fill in vent type and edge dates for all rows of record_id
  fill(extub_date, intub_date, extub_reason, intub_pre_ecls, rrt_yn,
       .direction = "downup") %>%

  # group for different approaches to calculate ventilation time
  mutate(
    # are ecls and date avail?
    tubed_prior_ecls_w_date = (intub_pre_ecls & ventil_date),
    tubed_prior_ecls_wo_date = (intub_pre_ecls & !ventil_date),
    nottubed_prior_ecls_w_date = (!intub_pre_ecls & ventil_date),
    nottubed_prior_ecls_wo_date = (!intub_pre_ecls & !ventil_date),

    # vent time by available data / ventilation types
    vent_duration = case_when(
      tubed_prior_ecls_w_date & (extub.factor %in% c("no extubation before death", "intubated at time of death")) ~ death_date - intub_date,
      extub.factor == "orotracheally extubated" ~ extub_date - intub_date,
      .default = as.difftime(0, units = "secs"))
  ) %>%
  mutate(vent_duration = as.difftime(vent_duration, units = "days")) %>%
  fill(tubed_prior_ecls_w_date, tubed_prior_ecls_wo_date,
       nottubed_prior_ecls_w_date, nottubed_prior_ecls_wo_date,
       .direction = "downup") %>%
  ungroup()

# intubated before ECLS with date
tracheostomy <- vent_duration %>%
  group_by(record_id) %>%
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & tubed_prior_ecls_w_date & (extub_reason == "tracheostomy")) & (vent_type != "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>%
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>%
  rows_patch(., tracheostomy)

## transferred while intubated or with tracheostomy
trnsf_prior_intub <- vent_duration %>%
  group_by(record_id) %>%
  tally(redcap_repeat_instrument == "hemodynamics_ventilation_medication" & tubed_prior_ecls_wo_date & (extub_reason %in% c("transferred with tracheostoma", "transferred intubated")) & (vent_type == "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>%
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>%
  rows_patch(., trnsf_prior_intub)

# intubated before ECLS without date
tube_prior_wo_date <- vent_duration %>%
  group_by(record_id) %>%
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & tubed_prior_ecls_wo_date) & (vent_type != "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>%
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>%
  rows_patch(., tube_prior_wo_date)

# intubated after ECLS with date
# TODO: not sure what "ON MCS" means...
tube_after_w_date <- vent_duration %>%
  group_by(record_id) %>%
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & nottubed_prior_ecls_w_date) & (vent_type != "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>%
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>%
  rows_patch(., tube_after_w_date)

# intubated after ECLS with date
tube_after_wo_date <- vent_duration %>%
  group_by(record_id) %>%
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication" & nottubed_prior_ecls_wo_date) & (vent_type != "Invasive Ventilation")) %>%
  mutate(vent_duration = as.difftime(n, units = "days")) %>%
  select(record_id, vent_duration) %>%
  filter(vent_duration != 0)

## patch column
vent_duration <- vent_duration %>%
  rows_patch(., tube_after_wo_date)

# take median/max/min of creatinine and ph
ph_cr_median <- vent_duration %>%
  group_by(record_id) %>%
  filter(redcap_repeat_instrument == "labor") %>%
  mutate(med_cr = median(cr, na.rm = TRUE),
         max_cr = max(cr, na.rm = FALSE),
         min_cr = min(cr, na.rm = FALSE),
         med_ph = median(ph, na.rm = TRUE),
         max_ph = max(ph, na.rm = FALSE),
         min_ph = min(ph, na.rm = FALSE)) %>%
  ungroup() %>%
  select(record_id, med_cr, max_cr, min_cr,
         med_ph, max_ph, min_ph) %>%
  group_by(record_id) %>%
  filter(row_number() == 1)

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
  rows_patch(., ph_cr_median)

# create hosp_los
lengths_of_stay <- vent_duration %>%
  mutate(
    hosp_admit_date = parse_date_time(paste(pat_admit_date, pat_admit_time), "%m/%d/%y %H:%M:%S"),
    icu_admit_date = parse_date_time(paste(pat_icu_date_i, pat_icu_time_i), "%m/%d/%y %H:%M:%S", exact = TRUE),
    hosp_disch_date = parse_date_time(paste(discharg_hospital_date, discharg_hospital_time), "%m/%d/%y %H:%M:%S"),
    icu_disch_date  = parse_date_time(paste(discharg_icu_date, discharg_icu_time), "%m/%d/%y %H:%M:%S")
  ) %>%
  select(record_id, hosp_admit_date, hosp_disch_date, icu_admit_date, icu_disch_date, death_date, death) %>%
  # TODO: this may fix the issue with discharge and death dates
  group_by(record_id) %>%
  fill(hosp_admit_date, hosp_disch_date, icu_admit_date, icu_disch_date, death_date,
       .direction = "downup") %>%
  mutate(
    hosp_los        = case_when(is.na(hosp_disch_date) ~ death_date - hosp_admit_date,
                                .default = hosp_disch_date - hosp_admit_date),
    # TODO: assuming that those with missing icu admission dates were admitted directly to ICU
    icu_los         =  case_when(is.na(icu_admit_date) & is.na(icu_disch_date) ~ hosp_los,
                                 is.na(icu_admit_date) & !is.na(icu_disch_date) ~ icu_disch_date - hosp_admit_date,
                                 !is.na(icu_admit_date) ~ death_date - icu_admit_date,
                                 ),
    hosp_surv_yn    = case_when(!death & !is.na(hosp_disch_date) ~ TRUE,
                                death & (hosp_disch_date < death_date) ~ TRUE,
                                death & is.na(hosp_disch_date) ~ FALSE,
                                death & !is.na(death_date) ~ FALSE,
                                .default = NA),
    lost_to_fu      = (is.na(death_date) & is.na(hosp_admit_date))
  ) %>%
  filter(row_number() == 1)

## patch columns
vent_duration <- vent_duration %>%
  add_column(hosp_admit_date = as.POSIXct(NA), hosp_disch_date = as.POSIXct(NA), hosp_los = as.POSIXct(NA),
             admit_disch_los = as.POSIXct(NA), admit_death_los = as.POSIXct(NA), hosp_surv_yn = NA,
             lost_to_fu = NA, icu_admit_date = as.POSIXct(NA), icu_disch_date = as.POSIXct(NA),
             icu_los = as.duration(NA)
             ) %>%
  mutate(
    hosp_los = as.difftime(as.character(hosp_los), units = "days"),
    icu_los  = as.difftime(as.character(icu_los), units = "days")
        ) %>%
  rows_patch(., lengths_of_stay)


# create ever received nephrotoxic drugs during hospital stay
nephrotox_rx <- vent_duration %>%
  group_by(record_id) %>%
  filter(redcap_repeat_instrument == "hemodynamics_ventilation_medication") %>%
  group_by(id) %>%
  mutate(
    rx_nephrotox = case_when(is.na(nephtox_rx) ~ FALSE,
                             any(nephtox_rx == TRUE) ~ TRUE,
                             .default = FALSE),
    abx_yn       = any(any_abx == TRUE)
  ) %>%
  ungroup() %>%
  select(record_id, rx_nephrotox, any_abx) %>%
  group_by(record_id) %>%
  filter(row_number() == 1)

## patch column
vent_duration <- vent_duration %>%
  add_column(rx_nephrotox = NA, abx_yn = NA) %>%
  rows_patch(., nephrotox_rx)

# MCS durations
mcs_duration <- vent_duration %>%
  mutate(
    ecls_duration = replace_na(ecls_stop - ecls_start, duration(0, "days")),
    impella_duration = replace_na(impella_stop - impella_start, duration(0, "days")),
  ) %>%
  select(record_id, ecls_duration, impella_duration, ecls_start,
         ecls_stop, impella_start, impella_stop) %>%
  group_by(record_id) %>%
  filter(row_number() == 1)

## patch column
vent_duration <- vent_duration %>%
  add_column(ecls_duration = NA, impella_duration = NA) %>%
  mutate(ecls_duration = as.difftime(as.character(ecls_duration), units = "days"),
         impella_duration = as.difftime(as.character(impella_duration), units = "days")) %>%
  rows_patch(., mcs_duration)

# create final output dataframe
constructed <- vent_duration %>%
  select(
    # id and groups
    record_id, id, group, death,
    # demographics and hx
    age, sex, height, weight, bmi, diabetes, ckd_yn, ckd_stage, copd_yn, copd_stage, mi_yn,
    rx_nephrotox,
    # hospital stay
    hosp_admit_date, hosp_disch_date, icu_admit_date, icu_disch_date, death_date,
    hosp_los, icu_los, hosp_surv_yn, lost_to_fu,
    # mcs
    ecls_start, ecls_stop, impella_start, impella_stop,
    ecls_duration, impella_duration,
    # clinical markers
    pre_cr, pre_ph, med_cr, min_cr, max_cr, med_ph, min_ph, max_ph, pre_lactate, vis_score,
    # rrt
    pre_rrt_yn, pre_rrt_type,
    ecls_rrt_type, rrt_yn, rrt_duration,
    # rrt_yn is during ecls
    # icu
    postcard, cpb_fail, ecpr, cs_etiology, vent_type, vent_duration,
    intub_date, extub_date, extub_reason,
    # aki
    aki_yn, aki_max, aki_s1, aki_s2, aki_s3,
    # antibiotics
    abx_yn, cefuroxim, piptazo, meropenem, vanc_iv, vanc_po, linezolid, dapto,
    pcn_g, flucoxciln, rifampicin, gentamycin, tobramycin, ciproflox,
    erythromyc, caspofungn, amph_b_inh, metronid, other_abx
  ) %>%
  group_by(record_id) %>%
  mutate(
    rx_nephrotox = replace_na(rx_nephrotox, FALSE),
    ecpr = replace_na(ecpr, FALSE),
    mi_yn = replace_na(mi_yn, FALSE),
    vent_duration = as.numeric(vent_duration, units = "days"),
    log_vis_score = log(vis_score)
  ) %>%
  filter(row_number() == 1) %>%    # condense to a single row per patient
  filter(!is.na(hosp_surv_yn)) %>%      # only include patient with outcomes
  # TODO: 347 and 850 died before admitted?
  filter(hosp_los >= lubridate::ddays(0)) %>%
  ungroup()

# create gradated RRT groups
constructed <- mutate(constructed,
                      rrt_group = case_when(
                        !pre_rrt_yn & !rrt_yn ~ "No RRT",
                        pre_rrt_yn & !rrt_yn ~ "RRT before and during tMCS",
                        !pre_rrt_yn & rrt_yn ~ "RRT during tMCS only",
                        pre_rrt_yn & rrt_yn ~ "RRT before and during tMCS"
                      )) %>%
                      mutate(rrt_group = fct_relevel(rrt_group, "RRT before and during tMCS", after = Inf))

# examine data distribution and types
write_csv(constructed, "data/cleaned_analysis_data.csv",
          na = "NA", append = FALSE)

# save R object
save(constructed, file = "data/cleaned_analysis_data.Rda")

## identify outliers and extreme values
# here values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are considered as outliers.
# Values above Q3 + 3xIQR or below Q1 - 3xIQR are considered as extreme points (or extreme outliers).

(outliers <- constructed %>%
  rstatix::identify_outliers("age"))
