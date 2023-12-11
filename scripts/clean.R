## load in libraries for cleaning
library(janitor)
library(tidyverse)
library(readxl)

# flexible values

# load helpers
source("scripts/helpers.R")

# get path to raw excel file
source("scripts/dataLoad.R")

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
                      redcap_event_name %in% group_2 ~ "ECMELLA") %>% as.factor(),
    death = as.logical(death)
  ) %>%

  # PSM features
  mutate(
    age       = coalesce(data$age_ecls_1, data$age_ecls_2),
    sex       = sex.factor,
    bmi       = (weight / (height / 100)^2),
    lactate   = pre_lactate,          # TODO: the correct lactate?

    # pre_vis had non-numeric values that threw coercion errors
    pre_vis   = as.character(pre_vis), # convert to character
    pre_vis   = na_if(pre_vis, ""),    # replace empty strings with NA
    ## replace non-numeric values with NA
    pre_vis   = ifelse(grepl("^\\d+$", pre_vis), pre_vis, NA),
    pre_vis   = as.numeric(pre_vis), # convert to numeric
    vis_score = case_when(
      as.numeric(pre_vis) > 100 ~ 100,
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
    nephtox_rx    = (vanc_iv | gentamycin | tobramycin)
  ) %>%

  # boolean ecls and rrt variables
  # boolean ecls and rrt variables
  mutate(
    ecls_start_date = as.Date(ecls_start_date, format = "%m/%d/%y"),
    ecls_stop_date = as.Date(ecls_stop_date, format = "%m/%d/%y"),
    impella_start_date = as.Date(impella_start_date, format = "%m/%d/%y"),
    impella_stop_date = as.Date(impella_stop_date, format = "%m/%d/%y"),
        #ecls_rrt_yn   = as.logical(...),
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
    aki_s1 = as.logical((aki_1_1 == 1) | (aki_1_2 == 1)),
    aki_s2 = as.logical((aki_2 == 2)),
    aki_s3 = as.logical(((aki_3_1 == 3) | (aki_3_2 == 3))),
  ) %>%

  # fill vertically for time-invariant factors
  group_by(record_id) %>%
  fill(id, age, sex, bmi, vis_score, diabetes, death, ckd_yn, ckd_stage,
      copd_yn,
      copd_stage, lactate, mi_yn, postcard, cpb_fail, ecpr, cs_etiology,
      pre_rrt_yn, pre_rrt_type,
      # TODO: should these be filled here?
      aki_s1, aki_s2, aki_s3,
      ecls_start_date, ecls_stop_date, impella_start_date, impella_stop_date,
      .direction = "downup") %>%
  ungroup()

# time (vertical) variables
# duration of RRT
rrt_duration <- clean %>%
  group_by(record_id) %>%
  tally((redcap_repeat_instrument == "hemodynamics_ventilation_medication") & (!ecls_rrt_type %in% c(NA, "No")),
        name = 'rrt_duration') %>%
  mutate(rrt_duration = as.difftime(rrt_duration, units = "days")) %>%
  select(record_id, rrt_duration)

## patch column
clean <- clean %>%
  add_column(rrt_duration = NA) %>%
  mutate(rrt_duration = as.difftime(as.character(rrt_duration),
    units = "days")) %>%
  rows_patch(., rrt_duration)

# duration of ventilation
vent_duration <- clean %>%
  mutate(
    vent_yn   = as.factor(pre_ventilation),
    vent_type = as.factor(vent.factor),
    extub_reason = as.factor(extub.factor),
    intub_pre_ecls = as.logical(intubation),
    intub_date = as.Date(ventil_start_date, format = "%m/%d/%y"),
    extub_date = as.Date(extub_date, format = "%m/%d/%y"),
    death_date = as.Date(death_date, format = "%m/%d/%y")
  ) %>%

  # fill in vent type and edge dates for all rows of record_id
  fill(extub_date, intub_date, extub_reason, intub_pre_ecls,
       .direction = "downup") %>%

  # group for different approaches to calculate ventilation time
  mutate(
    # are ecls and date avail?
    tubed_prior_ecls_w_date = (intub_pre_ecls & ventil_date),
    tubed_prior_ecls_wo_date = (intub_pre_ecls & !ventil_date),
    nottubed_prior_ecls_w_date = (!intub_pre_ecls & ventil_date),
    nottubed_prior_ecls_wo_date = (!intub_pre_ecls & !ventil_date),

    # vent time by available data / ventilation types
    vent_duration = case_when(tubed_prior_ecls_w_date & (extub.factor %in% c("no extubation before death", "intubated at time of death")) ~ death_date - intub_date,
                              extub.factor == "orotracheally extubated" ~ extub_date - intub_date,
                              .default = as.difftime(0, units = "days"))
    ) %>%
  fill(tubed_prior_ecls_w_date, tubed_prior_ecls_wo_date,
       nottubed_prior_ecls_w_date, nottubed_prior_ecls_wo_date,
       .direction = "downup")

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
    hosp_admit_date = as.Date(pat_admit_date, format = "%m/%d/%y"),
    hosp_disch_date = as.Date(discharg_hospital_date, format = "%m/%d/%y")
  ) %>%
  select(record_id, hosp_admit_date, hosp_disch_date, death_date, death) %>%
  fill(hosp_admit_date, hosp_disch_date, death_date,
       .direction = "downup") %>%
  group_by(record_id) %>%
  # TODO: which to use to calc if there is both a discharge and death?
  mutate(
    admit_disch_los = hosp_disch_date - hosp_admit_date,
    admit_death_los = death_date - hosp_admit_date,
    hosp_los        = case_when(is.na(hosp_disch_date) ~ admit_death_los,
                               !is.na(hosp_disch_date) ~ admit_disch_los,
                               .default = NA),
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
  add_column(hosp_admit_date = NA, hosp_disch_date = NA, hosp_los = NA,
             admit_disch_los = NA, admit_death_los = NA, hosp_surv_yn = NA,
             lost_to_fu = NA) %>%
  mutate(hosp_admit_date = as.Date(hosp_admit_date, format = "%m/%d/%y"),
         hosp_disch_date = as.Date(hosp_disch_date, format = "%m/%d/%y"),
         hosp_los = as.difftime(as.character(hosp_los), units = "days"),
         admit_disch_los = as.difftime(as.character(admit_disch_los), units = "days"),
         admit_death_los = as.difftime(as.character(admit_death_los), units = "days")) %>%
  rows_patch(., lengths_of_stay)


# icu_los = ...

# create ever received nephrotoxic drugs
nephrotox_rx <- vent_duration %>%
  group_by(record_id) %>%
  filter(redcap_repeat_instrument == "hemodynamics_ventilation_medication") %>%
  group_by(id) %>%
  mutate(rx_nephrotox = case_when(is.na(nephtox_rx) ~ NA,
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
    ecls_duration = replace_na(ecls_stop_date - ecls_start_date, duration(0, "days")),
    impella_duration = replace_na(impella_stop_date - impella_start_date, duration(0, "days")),
  ) %>%
  select(record_id, ecls_duration, impella_duration, ecls_start_date,
         ecls_stop_date, impella_start_date, impella_stop_date) %>%
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
    record_id, id, group, death, death_date,
    # demographics and hx
    age, sex, bmi, diabetes, ckd_yn, ckd_stage, copd_yn, copd_stage, mi_yn,
    rx_nephrotox,
    # hospital stay
    hosp_admit_date, hosp_disch_date, hosp_los, admit_disch_los,
    admit_death_los, hosp_surv_yn, lost_to_fu,
    # mcs
    ecls_start_date, ecls_stop_date, impella_start_date, impella_stop_date,
    ecls_duration, impella_duration,
    # clinical markers
    med_cr, min_cr, max_cr, med_ph, min_ph, max_ph, lactate, vis_score,
    # rrt
    pre_rrt_yn, pre_rrt_type,
      #ecls_rrt_yn,
    ecls_rrt_type, rrt_duration,
    # icu
    postcard, cpb_fail, ecpr, cs_etiology, vent_type, vent_duration,
    intub_date, extub_date, extub_reason,
    # aki
    aki_yn, aki_s1, aki_s2, aki_s3,
    # antibiotics
    abx_yn, cefuroxim, piptazo, meropenem, vanc_iv, vanc_po, linezolid, dapto,
    pcn_g, flucoxciln, rifampicin, gentamycin, tobramycin, ciproflox,
    erythromyc, caspofungn, amph_b_inh, metronid, other_abx
  ) %>%
  group_by(record_id) %>%
  filter(row_number()==1) %>%    # condense to a single row per patient
  filter(!is.na(death)) %>%      # only include patient with outcomes
  # TODO: 347 and 850 died before admitted?
  filter(hosp_los >= lubridate::ddays(0)) %>%
  filter(is.na(vent_duration) | vent_duration >= lubridate::ddays(x = 0))

# examine data distribution and types
write_csv(constructed, "data/cleaned_analysis_data.csv",
          na = "NA", append = FALSE)

# save R object
save(constructed, file = "data/cleaned_analysis_data.Rda")
