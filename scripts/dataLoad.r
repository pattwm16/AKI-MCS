#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
# NB: data came in .csv file with commas separating integers and decimals
#     did find/replace in excel for commas -> period
#     output file for that is 'no_comma_raw.csv' from
#     ECLSDatenbank-LuzieNierenfunktionE_DATA_2023-11-17_1325.csv
data=read.csv('data/no_comma_raw.csv')

#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_event_name)="Event Name"
label(data$redcap_repeat_instrument)="Repeat Instrument"
label(data$redcap_repeat_instance)="Repeat Instance"
label(data$id)="Identificationnumber"
label(data$sex)="Sex"
label(data$weight)="Weight"
label(data$height)="Height"
label(data$bmi)="BMI"
label(data$history_arrhythmia)="History of Cardiac Arrhythmia "
label(data$history_arrhythmia_spec___1)="Cardiac Arrhythmia  (choice=Atrial Fibrillation)"
label(data$history_arrhythmia_spec___2)="Cardiac Arrhythmia  (choice=Atrial Flutter)"
label(data$history_arrhythmia_spec___3)="Cardiac Arrhythmia  (choice=Ventricular Tachycardia)"
label(data$history_arrhythmia_spec___4)="Cardiac Arrhythmia  (choice=Ventricular Fibrillation)"
label(data$history_arrhythmia_spec___5)="Cardiac Arrhythmia  (choice=AV-Block II)"
label(data$history_arrhythmia_spec___6)="Cardiac Arrhythmia  (choice=AV-Block III)"
label(data$history_arrhythmia_spec___7)="Cardiac Arrhythmia  (choice=Sinus node dysfunction)"
label(data$diabetes)="Diabetes mellitus "
label(data$diabetes_spec)="Type of Diabetes mellitus"
label(data$ckd)="Chronic kidney disease"
label(data$ckd_spec)="Stage of Chronic kidney disease"
label(data$copd)="Chronic obstructive pulmonary disease"
label(data$copd_spec)="Type of COPD "
label(data$extub)="Extubation"
label(data$extub_date)="Date of Extubation"
label(data$extub_time)="Time of Extubation "
label(data$tracheo_date)="Date of Tracheostomy"
label(data$tracheo_time)="Time of Tracheostomy "
label(data$tracheo_decann)="Successful Decannulation?"
label(data$tracheo_decann_date)="Date of successful Decannulation "
label(data$discharge___1)="Discharged  (choice=Death)"
label(data$discharge___2)="Discharged  (choice=Discharged from ICU)"
label(data$discharge___3)="Discharged  (choice=Discharged from DHZB)"
label(data$discharge___4)="Discharged  (choice=Discharged from last hospital)"
label(data$discharg_icu_date)="Date of ICU discharge"
label(data$discharg_icu_time)="Time of ICU discharge "
label(data$discharg_hospital_date)="Date of Hospital discharge"
label(data$discharg_hospital_time)="Time of Hospital discharge "
label(data$discharg_location)="Discharge location"
label(data$discharg_last_hospital_date)="Date of Hospital discharge (last Hospital)"
label(data$death)="Death"
label(data$death_date)="Date of Death"
label(data$death_time)="Time of Death"
label(data$date_fu_death)="Date of last Follow-up"
label(data$pat_admit_date)="Hospital Admit Date"
label(data$pat_admit_time)="Hospital Admit Time"
label(data$reason_ecls)="Cardiomyopathy/Reason for ECLS"
label(data$reason_ecls_other)="Other Reason for ECLS-Implantation"
label(data$pre_postcard)="Heart Surgery in the last 7 days?   (Postcardiotomy Syndrome) "
label(data$pre_failure_cpb)="Failure to Wean from Cardiopulmonary Bypass/ECLS? "
label(data$pre_mi)="Pre ECLS Myocardial Infarction "
label(data$pre_mi_date)="Date of Myocardial Infarction"
label(data$pre_mi_type)="Type of Myocardial Infarction"
label(data$pre_cardiac_arrest)="Pre ECLS Cardiac Arrest"
label(data$pre_cardiac_arrest_dur)="Duration of Cardiac Arrest "
label(data$pre_cardiac_arrest_type___1)="Type of Cardiac Arrest (choice=Out of Hospital)"
label(data$pre_cardiac_arrest_type___2)="Type of Cardiac Arrest (choice=In Hospital)"
label(data$pre_cardiac_arrest_type___3)="Type of Cardiac Arrest (choice=unknown)"
label(data$pre_cardiac_arrest_rhythm___1)="Electrocardiographic Rhythm during Cardiac Arrest (choice=Ventricular Fibrillation)"
label(data$pre_cardiac_arrest_rhythm___2)="Electrocardiographic Rhythm during Cardiac Arrest (choice=Pulseless Ventricular Tachycardia)"
label(data$pre_cardiac_arrest_rhythm___3)="Electrocardiographic Rhythm during Cardiac Arrest (choice=Pulseless Electrical Activity)"
label(data$pre_cardiac_arrest_rhythm___4)="Electrocardiographic Rhythm during Cardiac Arrest (choice=Asystole)"
label(data$pre_cardiac_arrest_rhythm___5)="Electrocardiographic Rhythm during Cardiac Arrest (choice=unknown)"
label(data$intubation)="Intubation before ECLS-Implantation"
label(data$intubation_after)="Intubation after ECLS-Implantation"
label(data$ventil_date)="Invasive Ventilation start date available?"
label(data$ventil_start_date)="Invasive Ventilation start date"
label(data$pre_vent_duration)="Pre ECLS Ventilation Duration"
label(data$ventil_time)="Invasive Ventilation start time available?"
label(data$ventil_start_time)="Invasive Ventilation start time"
label(data$pre_renal_repl)="Renal Replacement Therapy"
label(data$pre_renal_repl_spec)="Specific Renal Replacement Therapy"
label(data$pre_renal_repl_start)="Start date of Renal Replacement Therapy"
label(data$pre_arrhythmia)="Cardiac Arrhythmia "
label(data$pre_arrhythmia_spec___1)="Cardiac Arrhythmia (choice=Atrial Fibrillation)"
label(data$pre_arrhythmia_spec___2)="Cardiac Arrhythmia (choice=Atrial Flutter)"
label(data$pre_arrhythmia_spec___3)="Cardiac Arrhythmia (choice=Ventricular Tachycardia)"
label(data$pre_arrhythmia_spec___4)="Cardiac Arrhythmia (choice=Ventricular Fibrillation)"
label(data$pre_arrhythmia_spec___5)="Cardiac Arrhythmia (choice=AV-Block II)"
label(data$pre_arrhythmia_spec___6)="Cardiac Arrhythmia (choice=AV-Block III)"
label(data$pre_arrhythmia_spec___7)="Cardiac Arrhythmia (choice=Sinus node dysfunction)"
label(data$pre_vasoactive___1)="Vasoactive Infusion (choice=Dobutamine)"
label(data$pre_vasoactive___2)="Vasoactive Infusion (choice=Dopamine)"
label(data$pre_vasoactive___3)="Vasoactive Infusion (choice=Enoximone)"
label(data$pre_vasoactive___4)="Vasoactive Infusion (choice=Epinephrine)"
label(data$pre_vasoactive___5)="Vasoactive Infusion (choice=Esmolol)"
label(data$pre_vasoactive___6)="Vasoactive Infusion (choice=Levosimendan)"
label(data$pre_vasoactive___7)="Vasoactive Infusion (choice=Metaraminol)"
label(data$pre_vasoactive___8)="Vasoactive Infusion (choice=Metoprolol)"
label(data$pre_vasoactive___9)="Vasoactive Infusion (choice=Milrinone)"
label(data$pre_vasoactive___10)="Vasoactive Infusion (choice=Nicardipine)"
label(data$pre_vasoactive___11)="Vasoactive Infusion (choice=Nitroglycerin)"
label(data$pre_vasoactive___12)="Vasoactive Infusion (choice=Nitroprusside)"
label(data$pre_vasoactive___13)="Vasoactive Infusion (choice=Norepinephrine)"
label(data$pre_vasoactive___14)="Vasoactive Infusion (choice=Phenylephrine)"
label(data$pre_vasoactive___15)="Vasoactive Infusion (choice=Tolazoline)"
label(data$pre_vasoactive___16)="Vasoactive Infusion (choice=Empressin)"
label(data$pre_vasoactive___17)="Vasoactive Infusion (choice=None)"
label(data$pre_vasoactive___18)="Vasoactive Infusion (choice=Unknown)"
label(data$pre_dobutamine)="Dobutamine Dose"
label(data$pre_epinephrine)="Epinephrine Dose "
label(data$pre_norepinephrine)="Norepinephrine Dose"
label(data$pre_vasopressin)="Empressin Dose"
label(data$pre_milrinone)="Milrinone Dose"
label(data$pre_vis)="Vasoactive Inotropic Score (VIS)"
label(data$pre_assess_date)="Date of Assessment"
label(data$pre_assess_time)="Time of Assessment"
label(data$pre_pco2)="pCO2"
label(data$pre_p02)="pO2"
label(data$pre_ph)="pH "
label(data$pre_hco3)="HCO3"
label(data$pre_be)="BE "
label(data$pre_k)="Kalium"
label(data$pre_na)="Natrium"
label(data$pre_sa02)="Sa02"
label(data$pre_gluc)="Glucose"
label(data$pre_lactate)="Lactate"
label(data$pre_svo2)="Svo2 "
label(data$pre_ventilation)="Ventilation"
label(data$pre_02l)="O2 "
label(data$pre_fi02)="Fi02"
label(data$pre_vent_spec)="Ventilation Specifics"
label(data$pre_vent_type)="Ventilator Type"
label(data$pre_conv_vent_rate)="Conventional Ventilation Rate"
label(data$pre_vent_map)="Mean Airway Pressure"
label(data$pre_vent_pip)="Peak Inspiratory Pressure"
label(data$pre_vent_peep)="Positive end-exspiratory Pressure "
label(data$pre_hr)="Heart Rate"
label(data$pre_sys_bp)="Systolic Blood Pressure"
label(data$pre_dia_bp)="Diastolic Blood Pressure"
label(data$pre_mean_bp)="Mean Blood Pressure"
label(data$pre_cvd)="Central Venous Pressure"
label(data$pre_sp02)="Sp02"
label(data$pre_temp)="Temperature "
label(data$pre_pcwp)="Pulmonary Capillary Wedge Pressure "
label(data$pre_sys_pap)="Systolic Pulmonary Arterial Pressure"
label(data$pre_dia_pap)="Diastolic Pulmonary Arterial Pressure"
label(data$pre_mean_pap)="Mean Pulmonary Arterial Pressure"
label(data$pre_ci)="Cardiac Index"
label(data$pre_gcs)="Glagow Coma Scale "
label(data$pre_lab_results_elso)="Time of laboratory results purchase"
label(data$pre_wbc)="White Blood Cells"
label(data$pre_hb)="Hemoglobin"
label(data$pre_hct)="Hematocrit"
label(data$pre_plt)="Platelets"
label(data$pre_crp_m)="CRP measured? "
label(data$pre_crp)="CRP"
label(data$pre_pct_m)="Procalcitonin measured?"
label(data$pre_pct)="Procalcitonin"
label(data$pre_alb)="Albumin"
label(data$pre_ptt)="PTT"
label(data$pre_quick)="Quick"
label(data$pre_inr)="INR"
label(data$pre_ck)="CK"
label(data$pre_ckmb)="CK - MB"
label(data$pre_got)="GOT"
label(data$pre_ldh)="LDH"
label(data$pre_lipase)="Lipase"
label(data$pre_crea)="Creatinin"
label(data$pre_urea)="Urea"
label(data$pre_cc)="Creatinine Clearance"
label(data$pre_fhb)="free Hemoglobin "
label(data$pre_hapto)="Haptoglobin "
label(data$pre_bili)="Bilirubin (total)"
label(data$pre_trop_m)="Troponin measured? "
label(data$pre_trop)="Troponin"
label(data$prevaecls_hemodynamics_ventilation_labor_complete)="Complete?"
label(data$ecls_order)="ECLS Order"
label(data$ecls_start_date)="Start Date"
label(data$ecls_start_time)="Start Time "
label(data$ecls_icu)="Place where ECLS received"
label(data$ecls_type)="ECLS Type veno-venous ECMO = pulmonary ECLS-Implantation during Resuscitation = ECPR"
label(data$ecls_mode)="ECLS Mode"
label(data$ecls_stop_date)="Stop Date"
label(data$ecls_stop_time)="Stop Time"
label(data$disc_reason___1)="Discontinuation Reason (choice=Expected recovery)"
label(data$disc_reason___2)="Discontinuation Reason (choice=Death or Poor Prognosis)"
label(data$disc_reason___3)="Discontinuation Reason (choice=Resource Limitation)"
label(data$disc_reason___4)="Discontinuation Reason (choice=ECLS Complication)"
label(data$disc_reason___14)="Discontinuation Reason (choice=MCS)"
label(data$disc_reason___15)="Discontinuation Reason (choice=Transplantation)"
label(data$mcs_post_ecls___1)="Mechanical Circulatory Support after ECLS  (choice=temporary RVAD)"
label(data$mcs_post_ecls___2)="Mechanical Circulatory Support after ECLS  (choice=implantable RVAD)"
label(data$mcs_post_ecls___3)="Mechanical Circulatory Support after ECLS  (choice=temporary LVAD)"
label(data$mcs_post_ecls___4)="Mechanical Circulatory Support after ECLS  (choice=implantable LVAD)"
label(data$mcs_post_ecls___5)="Mechanical Circulatory Support after ECLS  (choice=temporary BiVAD)"
label(data$mcs_post_ecls___6)="Mechanical Circulatory Support after ECLS  (choice=implantable BiVAD)"
label(data$mcs_post_ecls___7)="Mechanical Circulatory Support after ECLS  (choice=paracorporeal BiVAD)"
label(data$mcs_post_ecls___8)="Mechanical Circulatory Support after ECLS  (choice=Pumpless Lung Assist)"
label(data$impella_start_date)="Start Date"
label(data$impella_start_time)="Start Time "
label(data$impella_impl_place)="Place where Impella received"
label(data$impella_impl_form)="Form of Impella Implantation"
label(data$impella_type)="Impella Type"
label(data$impella_access_site)="Access site"
label(data$impella_stop_date)="Stop Date"
label(data$impella_stop_time)="Stop Time"
label(data$ecmella)="ECLS and Impella Support in the last 24 hours?"
label(data$assess_time_point)="Time Point"
label(data$assess_date_hemo)="Date of Assessment"
label(data$hr)="Heart Rate"
label(data$sys_bp)="Systolic Blood Pressure "
label(data$dia_bp)="Diastolic Blood Pressure"
label(data$mean_bp)="Mean Blood Pressure"
label(data$sp02)="Sp02 "
label(data$pcwp)="Pulmonary Capillary Wedge Pressure "
label(data$sys_pap)="Systolic Pulmonary Arterial Pressure"
label(data$dia_pap)="Diastolic Pulmonary Arterial Pressure "
label(data$mean_pap)="Mean Pulmonary Arterial Pressure"
label(data$ci)="Cardiac Index"
label(data$vasoactive_med)="Vasoactive Infusion?"
label(data$vasoactive_spec___1)="Vasoactive Infusion (choice=Dobutamine)"
label(data$vasoactive_spec___2)="Vasoactive Infusion (choice=Dopamine)"
label(data$vasoactive_spec___3)="Vasoactive Infusion (choice=Enoximone)"
label(data$vasoactive_spec___4)="Vasoactive Infusion (choice=Epinephrine)"
label(data$vasoactive_spec___5)="Vasoactive Infusion (choice=Esmolol)"
label(data$vasoactive_spec___6)="Vasoactive Infusion (choice=Levosimendan)"
label(data$vasoactive_spec___7)="Vasoactive Infusion (choice=Metaraminol)"
label(data$vasoactive_spec___8)="Vasoactive Infusion (choice=Metoprolol)"
label(data$vasoactive_spec___9)="Vasoactive Infusion (choice=Milrinone)"
label(data$vasoactive_spec___10)="Vasoactive Infusion (choice=Nicardipine)"
label(data$vasoactive_spec___11)="Vasoactive Infusion (choice=Nitroglycerin)"
label(data$vasoactive_spec___12)="Vasoactive Infusion (choice=Nitroprusside)"
label(data$vasoactive_spec___13)="Vasoactive Infusion (choice=Norepinephrine)"
label(data$vasoactive_spec___14)="Vasoactive Infusion (choice=Phenylephrine)"
label(data$vasoactive_spec___15)="Vasoactive Infusion (choice=Tolazoline)"
label(data$vasoactive_spec___16)="Vasoactive Infusion (choice=Empressin)"
label(data$vasoactive_spec___17)="Vasoactive Infusion (choice={vasoactive_o})"
label(data$vasoactive_o)="Other Vasoactive Infusion"
label(data$dobutamine)="Dobutamine Dose"
label(data$epinephrine)="Epinephrine Dose "
label(data$norepinephrine)="Norepinephrine Dose"
label(data$vasopressin)="Empressin Dose"
label(data$milrinone)="Milrinone Dose"
label(data$vent)="Ventilation"
label(data$o2)="02"
label(data$fi02)="Fi02"
label(data$vent_spec)="Ventilation Specifics"
label(data$vent_type)="Ventilation Type "
label(data$conv_vent_rate)="Conventional Ventilation Rate"
label(data$vent_map)="Mean Airway Pressure"
label(data$vent_pip)="Peak Inspiratory Pressure "
label(data$vent_peep)="Positive end-exspiratory Pressure "
label(data$rass___1)="Richmond Agitation Sedation Scale (RASS) (choice=Combative (4))"
label(data$rass___2)="Richmond Agitation Sedation Scale (RASS) (choice=Very agitated (3))"
label(data$rass___3)="Richmond Agitation Sedation Scale (RASS) (choice=Agitated (2))"
label(data$rass___4)="Richmond Agitation Sedation Scale (RASS) (choice=Restless (1))"
label(data$rass___5)="Richmond Agitation Sedation Scale (RASS) (choice=Alert and calm (0))"
label(data$rass___6)="Richmond Agitation Sedation Scale (RASS) (choice=Drowsy (-1))"
label(data$rass___7)="Richmond Agitation Sedation Scale (RASS) (choice=Light sedation (-2))"
label(data$rass___8)="Richmond Agitation Sedation Scale (RASS) (choice=Moderate sedation (-3))"
label(data$rass___9)="Richmond Agitation Sedation Scale (RASS) (choice=Deep sedation (-4))"
label(data$rass___10)="Richmond Agitation Sedation Scale (RASS) (choice=Unarousable (-5))"
label(data$gcs)="Glasgow Coma Scale "
label(data$mobil)="Level of Mobilization during ECLS "
label(data$antibiotic)="Antibiotic/Antimycotic Treatment?"
label(data$antibiotic_spec___13)="Specific Antibiotic Treatment (choice=Ciprofloxacin)"
label(data$antibiotic_spec___1)="Specific Antibiotic Treatment (choice=Cefuroxim)"
label(data$antibiotic_spec___2)="Specific Antibiotic Treatment (choice=Piperacillin/Tazobactam)"
label(data$antibiotic_spec___3)="Specific Antibiotic Treatment (choice=Meropenem)"
label(data$antibiotic_spec___4)="Specific Antibiotic Treatment (choice=Vancomycin i.v.)"
label(data$antibiotic_spec___5)="Specific Antibiotic Treatment (choice=Vancomycin p.o.)"
label(data$antibiotic_spec___6)="Specific Antibiotic Treatment (choice=Linezolid)"
label(data$antibiotic_spec___7)="Specific Antibiotic Treatment (choice=Daptomycin)"
label(data$antibiotic_spec___8)="Specific Antibiotic Treatment (choice=Penicllin G)"
label(data$antibiotic_spec___9)="Specific Antibiotic Treatment (choice=Flucloxacillin)"
label(data$antibiotic_spec___10)="Specific Antibiotic Treatment (choice=Rifampicin)"
label(data$antibiotic_spec___11)="Specific Antibiotic Treatment (choice=Gentamycin)"
label(data$antibiotic_spec___12)="Specific Antibiotic Treatment (choice=Tobramycin)"
label(data$antibiotic_spec___15)="Specific Antibiotic Treatment (choice=Erythromycin)"
label(data$antibiotic_spec___16)="Specific Antibiotic Treatment (choice=Caspofungin)"
label(data$antibiotic_spec___17)="Specific Antibiotic Treatment (choice=Amphotericin B inh.)"
label(data$antibiotic_spec___18)="Specific Antibiotic Treatment (choice=Metronidazol)"
label(data$antibiotic_spec___14)="Specific Antibiotic Treatment (choice={antibiotic_spec_o})"
label(data$antibiotic_spec_o)="Other Antibiotic Treatment"
label(data$antiviral)="Antiviral Treatment?"
label(data$antiviral_spec)="Specific Antiviral Treatment"
label(data$transfusion_coag)="Transfusion or Coagulation Products required?"
label(data$ery_t)="Red Blood Cell Transfusion"
label(data$thromb_t)="Thrombocyte Transfusion"
label(data$ffp_t)="Fresh Frozen Plasma Transfusion"
label(data$ppsb_t)="PPSB Administration"
label(data$fib_t)="Fibrinogen Administration"
label(data$at3_t)="Antithrombin III Administration"
label(data$organ_support___1)="Other Organ Support (choice=Inhaled Anesthetic)"
label(data$organ_support___2)="Other Organ Support (choice=Inhalted Epoprostenol)"
label(data$organ_support___3)="Other Organ Support (choice=Inhaled Nitric Oxide)"
label(data$organ_support___4)="Other Organ Support (choice=Liquid Ventilation)"
label(data$organ_support___5)="Other Organ Support (choice=Surfactant)"
label(data$organ_support___6)="Other Organ Support (choice=Plasmapheresis)"
label(data$organ_support___7)="Other Organ Support (choice=Therapeutic Hypothermia < 35°C degrees C)"
label(data$organ_support___9)="Other Organ Support (choice=MARS)"
label(data$organ_support___10)="Other Organ Support (choice=Cytosorb)"
label(data$organ_support___8)="Other Organ Support (choice=None)"
label(data$medication___1)="Medication (choice=Alprostadil)"
label(data$medication___2)="Medication (choice=IV Bicarbonate)"
label(data$medication___3)="Medication (choice=Prostacyclin Analogues)"
label(data$medication___4)="Medication (choice=Narcotics/Sedative Agents)"
label(data$medication___5)="Medication (choice=Neuromuscular blockers)"
label(data$medication___10)="Medication (choice=Opiods)"
label(data$medication___11)="Medication (choice=Antipsychotic medication)"
label(data$medication___6)="Medication (choice=Sildenafil)"
label(data$medication___7)="Medication (choice=Systemic Steriods)"
label(data$medication___8)="Medication (choice=Trometamol)"
label(data$medication___9)="Medication (choice=None)"
label(data$renal_repl)="Renal Replacement Therapy required?"
label(data$urine)="Total Urine Output"
label(data$aki_3_3)="Acute Kidney Injury 3 = Failure "
label(data$output_renal_repl)="Total Output via Renal Replacement Therapy "
label(data$fluid_balance)="Fluid Balancing "
label(data$fluid_balance_numb)="Fluid Balancing"
label(data$ecmella_2)="ECLS and Impella support in the last 24 hours?"
label(data$assess_time_point_labor)="Time Point"
label(data$assess_date_labor)="Date of Assessment"
label(data$pc02)="pC02"
label(data$p02)="pO2"
label(data$ph)="pH"
label(data$hco3)="HCO3"
label(data$be)="BE "
label(data$sa02)="Sa02"
label(data$k)="Kalium"
label(data$na)="Natrium"
label(data$gluc)="Glucose"
label(data$lactate)="Lactate"
label(data$sv02)="Sv02"
label(data$date_assess_labor)="Date of Assessment"
label(data$time_assess_labor)="Time of Assessment"
label(data$wbc)="White Blood Cells"
label(data$hb)="Hemoglobin"
label(data$hct)="Hematocrit"
label(data$plt)="Platelets"
label(data$ptt)="PTT"
label(data$quick)="Quick"
label(data$inr)="INR"
label(data$ck)="CK"
label(data$ckmb)="CK - MB"
label(data$got)="GOT"
label(data$ldh)="LDH"
label(data$lipase)="Lipase"
label(data$albumin)="Albumin"
label(data$crp)="CRP"
label(data$pct)="Procalcitonin"
label(data$fhb)="free Hemoglobin"
label(data$hapto)="Haptoglobin"
label(data$bili)="Bilirubin (total)"
label(data$crea)="Creatinin"
label(data$aki_1_1)="AKI Stadium 1 Krea 1,5-1,9x"
label(data$aki_1_2)="AKI Stadium 1 Krea >0.3mg/dl"
label(data$aki_2)="AKI Stadium 2 Krea 2-2,9x"
label(data$aki_3_1)="AKI Stadium 3 Krea>3x"
label(data$aki_3_2)="AKI Stadium 3 4mg/dl with acute rise "
label(data$urea)="Urea"
label(data$cc)="Creatinine Clearance"
label(data$labor_complete)="Complete?"
label(data$disc_reason_i___1)="Discontinuation Reason (choice=Expected recovery)"
label(data$disc_reason_i___2)="Discontinuation Reason (choice=Death or Poor Prognosis)"
label(data$disc_reason_i___3)="Discontinuation Reason (choice=Resource Limitation)"
label(data$disc_reason_i___4)="Discontinuation Reason (choice=MCS Complication)"
label(data$disc_reason_i___5)="Discontinuation Reason (choice=VAD)"
label(data$disc_reason_i___6)="Discontinuation Reason (choice=Transplantation)"
label(data$vad_post_imp___5)="VAD after Impella (choice=temporary RVAD)"
label(data$vad_post_imp___1)="VAD after Impella (choice=implantable RVAD)"
label(data$vad_post_imp___2)="VAD after Impella (choice=implantable LVAD)"
label(data$vad_post_imp___3)="VAD after Impella (choice=implantable BiVAD)"
label(data$vad_post_imp___4)="VAD after Impella (choice=paracorporeal BiVAD)"
label(data$aki)="Acute kidney injury "
label(data$aki_stage___1)="Highest stage of acute kidney injury (choice=1)"
label(data$aki_stage___2)="Highest stage of acute kidney injury (choice=2)"
label(data$aki_stage___3)="Highest stage of acute kidney injury (choice=3)"
label(data$aki_stage_1___1)="AKI Stadium 1 (choice=Krea 1.5 -1,9x)"
label(data$aki_stage_1___2)="AKI Stadium 1 (choice=Krea >0,3mg/dl)"
label(data$aki_stage_3___1)="AKI Stadium 3 (choice=Krea >3x)"
label(data$aki_stage_3___2)="AKI Stadium 3 (choice=Krea 4mg/dl with acute rise)"
label(data$aki_stage_3___3)="AKI Stadium 3 (choice=CVVHDF)"
label(data$age_ecls_1)="Age at ECLS"
label(data$age_ecls_2)="Age at ECLS"
label(data$date_complication)="Date of Assessment"
label(data$complication_time_point)="Time Point"
label(data$neurological_complications)="Neurological Complications"
label(data$brain_death)="Brain Death"
label(data$seizures_clinic)="Seizures Clinically determined"
label(data$seizures_eeg)="Seizures Confirmed by EEG"
label(data$cns_diffuse_ischemia)="CNS diffuse ischemia"
label(data$cns_local_ischemia)="CNS Infarction"
label(data$cns_hemorrhage)="Intra/Extra parenchymal CNS Hemorrhage "
label(data$intraventricular_cns_hemorrhage)="Intraventricular CNS hemorrhage"
label(data$neurosurgical_intervention)="Neurosurgical Intervention performed"
label(data$nc_o)="Other Neurologic Complications"
label(data$nc_o_spec)="Specific Other Neurologic Complications"
label(data$limb_complications)="Patient Limb Complications"
label(data$compartment_syndrome)="Limb Compartment Syndrome"
label(data$fasciotomy)="Fasciotomy "
label(data$amputation)="Limb Amputation"
label(data$limb_cannula)="Limb ischemia requiring limb reperfusion cannula"
label(data$lc_o)="Other Limb Complication"
label(data$lc_o_spec)="Specific Other Limb Complication"
label(data$kidney_comp)="Kidney Complications"
label(data$crea1)="Creatinine 1,5 -3,0"
label(data$crea2)="Creatinine >3"
label(data$cvvhdf)="Renal Replacement Therapy required"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("baseline_arm_1","ecls_arm_1","baseline_arm_2","ecls_arm_2","impella_arm_2","baseline_arm_3","impella_arm_3","baseline_arm_4","ecls_arm_4"))
data$redcap_repeat_instrument.factor = factor(data$redcap_repeat_instrument,levels=c("surgery","coronary_angiography","echocardiography","infections","additional_mcs","eclsmode","eclsequipment","pump","hemodynamics_ventilation_medication","labor","complications","mode_and_equipment_impella","impellaassessment_and_complications","impella_follow_up"))
data$sex.factor = factor(data$sex,levels=c("0","1"))
data$history_arrhythmia.factor = factor(data$history_arrhythmia,levels=c("1","0"))
data$history_arrhythmia_spec___1.factor = factor(data$history_arrhythmia_spec___1,levels=c("0","1"))
data$history_arrhythmia_spec___2.factor = factor(data$history_arrhythmia_spec___2,levels=c("0","1"))
data$history_arrhythmia_spec___3.factor = factor(data$history_arrhythmia_spec___3,levels=c("0","1"))
data$history_arrhythmia_spec___4.factor = factor(data$history_arrhythmia_spec___4,levels=c("0","1"))
data$history_arrhythmia_spec___5.factor = factor(data$history_arrhythmia_spec___5,levels=c("0","1"))
data$history_arrhythmia_spec___6.factor = factor(data$history_arrhythmia_spec___6,levels=c("0","1"))
data$history_arrhythmia_spec___7.factor = factor(data$history_arrhythmia_spec___7,levels=c("0","1"))
data$diabetes.factor = factor(data$diabetes,levels=c("1","0"))
data$diabetes_spec.factor = factor(data$diabetes_spec,levels=c("1","2","3","4","5"))
data$ckd.factor = factor(data$ckd,levels=c("1","0"))
data$ckd_spec.factor = factor(data$ckd_spec,levels=c("1","2","3","4","5","6"))
data$copd.factor = factor(data$copd,levels=c("1","0"))
data$copd_spec.factor = factor(data$copd_spec,levels=c("1","2","3","4","5"))
data$extub.factor = factor(data$extub,levels=c("1","2","3","4","5","6"))
data$tracheo_decann.factor = factor(data$tracheo_decann,levels=c("1","0"))
data$discharge___1.factor = factor(data$discharge___1,levels=c("0","1"))
data$discharge___2.factor = factor(data$discharge___2,levels=c("0","1"))
data$discharge___3.factor = factor(data$discharge___3,levels=c("0","1"))
data$discharge___4.factor = factor(data$discharge___4,levels=c("0","1"))
data$discharg_location.factor = factor(data$discharg_location,levels=c("1","2","3","4","5"))
data$death.factor = factor(data$death,levels=c("1","0"))
data$reason_ecls.factor = factor(data$reason_ecls,levels=c("1","13","2","3","4","5","6","7","9","10","11","12","8"))
data$pre_postcard.factor = factor(data$pre_postcard,levels=c("1","0"))
data$pre_failure_cpb.factor = factor(data$pre_failure_cpb,levels=c("1","0"))
data$pre_mi.factor = factor(data$pre_mi,levels=c("1","0"))
data$pre_mi_type.factor = factor(data$pre_mi_type,levels=c("1","2","3"))
data$pre_cardiac_arrest.factor = factor(data$pre_cardiac_arrest,levels=c("1","0"))
data$pre_cardiac_arrest_type___1.factor = factor(data$pre_cardiac_arrest_type___1,levels=c("0","1"))
data$pre_cardiac_arrest_type___2.factor = factor(data$pre_cardiac_arrest_type___2,levels=c("0","1"))
data$pre_cardiac_arrest_type___3.factor = factor(data$pre_cardiac_arrest_type___3,levels=c("0","1"))
data$pre_cardiac_arrest_rhythm___1.factor = factor(data$pre_cardiac_arrest_rhythm___1,levels=c("0","1"))
data$pre_cardiac_arrest_rhythm___2.factor = factor(data$pre_cardiac_arrest_rhythm___2,levels=c("0","1"))
data$pre_cardiac_arrest_rhythm___3.factor = factor(data$pre_cardiac_arrest_rhythm___3,levels=c("0","1"))
data$pre_cardiac_arrest_rhythm___4.factor = factor(data$pre_cardiac_arrest_rhythm___4,levels=c("0","1"))
data$pre_cardiac_arrest_rhythm___5.factor = factor(data$pre_cardiac_arrest_rhythm___5,levels=c("0","1"))
data$intubation.factor = factor(data$intubation,levels=c("1","2","3"))
data$intubation_after.factor = factor(data$intubation_after,levels=c("1","0"))
data$ventil_date.factor = factor(data$ventil_date,levels=c("1","0"))
data$ventil_time.factor = factor(data$ventil_time,levels=c("1","0"))
data$pre_renal_repl.factor = factor(data$pre_renal_repl,levels=c("1","0"))
data$pre_renal_repl_spec.factor = factor(data$pre_renal_repl_spec,levels=c("1","2","3","4"))
data$pre_arrhythmia.factor = factor(data$pre_arrhythmia,levels=c("1","0"))
data$pre_arrhythmia_spec___1.factor = factor(data$pre_arrhythmia_spec___1,levels=c("0","1"))
data$pre_arrhythmia_spec___2.factor = factor(data$pre_arrhythmia_spec___2,levels=c("0","1"))
data$pre_arrhythmia_spec___3.factor = factor(data$pre_arrhythmia_spec___3,levels=c("0","1"))
data$pre_arrhythmia_spec___4.factor = factor(data$pre_arrhythmia_spec___4,levels=c("0","1"))
data$pre_arrhythmia_spec___5.factor = factor(data$pre_arrhythmia_spec___5,levels=c("0","1"))
data$pre_arrhythmia_spec___6.factor = factor(data$pre_arrhythmia_spec___6,levels=c("0","1"))
data$pre_arrhythmia_spec___7.factor = factor(data$pre_arrhythmia_spec___7,levels=c("0","1"))
data$pre_vasoactive___1.factor = factor(data$pre_vasoactive___1,levels=c("0","1"))
data$pre_vasoactive___2.factor = factor(data$pre_vasoactive___2,levels=c("0","1"))
data$pre_vasoactive___3.factor = factor(data$pre_vasoactive___3,levels=c("0","1"))
data$pre_vasoactive___4.factor = factor(data$pre_vasoactive___4,levels=c("0","1"))
data$pre_vasoactive___5.factor = factor(data$pre_vasoactive___5,levels=c("0","1"))
data$pre_vasoactive___6.factor = factor(data$pre_vasoactive___6,levels=c("0","1"))
data$pre_vasoactive___7.factor = factor(data$pre_vasoactive___7,levels=c("0","1"))
data$pre_vasoactive___8.factor = factor(data$pre_vasoactive___8,levels=c("0","1"))
data$pre_vasoactive___9.factor = factor(data$pre_vasoactive___9,levels=c("0","1"))
data$pre_vasoactive___10.factor = factor(data$pre_vasoactive___10,levels=c("0","1"))
data$pre_vasoactive___11.factor = factor(data$pre_vasoactive___11,levels=c("0","1"))
data$pre_vasoactive___12.factor = factor(data$pre_vasoactive___12,levels=c("0","1"))
data$pre_vasoactive___13.factor = factor(data$pre_vasoactive___13,levels=c("0","1"))
data$pre_vasoactive___14.factor = factor(data$pre_vasoactive___14,levels=c("0","1"))
data$pre_vasoactive___15.factor = factor(data$pre_vasoactive___15,levels=c("0","1"))
data$pre_vasoactive___16.factor = factor(data$pre_vasoactive___16,levels=c("0","1"))
data$pre_vasoactive___17.factor = factor(data$pre_vasoactive___17,levels=c("0","1"))
data$pre_vasoactive___18.factor = factor(data$pre_vasoactive___18,levels=c("0","1"))
data$pre_ventilation.factor = factor(data$pre_ventilation,levels=c("5","1","6","7","4"))
data$pre_vent_spec.factor = factor(data$pre_vent_spec,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25"))
data$pre_vent_type.factor = factor(data$pre_vent_type,levels=c("1","2"))
data$pre_lab_results_elso.factor = factor(data$pre_lab_results_elso,levels=c("1","2"))
data$pre_crp_m.factor = factor(data$pre_crp_m,levels=c("1","0"))
data$pre_pct_m.factor = factor(data$pre_pct_m,levels=c("1","0"))
data$pre_trop_m.factor = factor(data$pre_trop_m,levels=c("1","0"))
data$prevaecls_hemodynamics_ventilation_labor_complete.factor = factor(data$prevaecls_hemodynamics_ventilation_labor_complete,levels=c("0","1","2"))
data$ecls_order.factor = factor(data$ecls_order,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$ecls_icu.factor = factor(data$ecls_icu,levels=c("1","2","3","4","5","6","8","7"))
data$ecls_type.factor = factor(data$ecls_type,levels=c("1","2","3"))
data$ecls_mode.factor = factor(data$ecls_mode,levels=c("1","2","3","7","8","4","5","6"))
data$disc_reason___1.factor = factor(data$disc_reason___1,levels=c("0","1"))
data$disc_reason___2.factor = factor(data$disc_reason___2,levels=c("0","1"))
data$disc_reason___3.factor = factor(data$disc_reason___3,levels=c("0","1"))
data$disc_reason___4.factor = factor(data$disc_reason___4,levels=c("0","1"))
data$disc_reason___14.factor = factor(data$disc_reason___14,levels=c("0","1"))
data$disc_reason___15.factor = factor(data$disc_reason___15,levels=c("0","1"))
data$mcs_post_ecls___1.factor = factor(data$mcs_post_ecls___1,levels=c("0","1"))
data$mcs_post_ecls___2.factor = factor(data$mcs_post_ecls___2,levels=c("0","1"))
data$mcs_post_ecls___3.factor = factor(data$mcs_post_ecls___3,levels=c("0","1"))
data$mcs_post_ecls___4.factor = factor(data$mcs_post_ecls___4,levels=c("0","1"))
data$mcs_post_ecls___5.factor = factor(data$mcs_post_ecls___5,levels=c("0","1"))
data$mcs_post_ecls___6.factor = factor(data$mcs_post_ecls___6,levels=c("0","1"))
data$mcs_post_ecls___7.factor = factor(data$mcs_post_ecls___7,levels=c("0","1"))
data$mcs_post_ecls___8.factor = factor(data$mcs_post_ecls___8,levels=c("0","1"))
data$impella_impl_place.factor = factor(data$impella_impl_place,levels=c("1","2"))
data$impella_impl_form.factor = factor(data$impella_impl_form,levels=c("1","2"))
data$impella_type.factor = factor(data$impella_type,levels=c("1","2","3","4","5"))
data$impella_access_site.factor = factor(data$impella_access_site,levels=c("1","2","3","4","5","6","7"))
data$ecmella.factor = factor(data$ecmella,levels=c("1","0"))
data$assess_time_point.factor = factor(data$assess_time_point,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$vasoactive_med.factor = factor(data$vasoactive_med,levels=c("1","0"))
data$vasoactive_spec___1.factor = factor(data$vasoactive_spec___1,levels=c("0","1"))
data$vasoactive_spec___2.factor = factor(data$vasoactive_spec___2,levels=c("0","1"))
data$vasoactive_spec___3.factor = factor(data$vasoactive_spec___3,levels=c("0","1"))
data$vasoactive_spec___4.factor = factor(data$vasoactive_spec___4,levels=c("0","1"))
data$vasoactive_spec___5.factor = factor(data$vasoactive_spec___5,levels=c("0","1"))
data$vasoactive_spec___6.factor = factor(data$vasoactive_spec___6,levels=c("0","1"))
data$vasoactive_spec___7.factor = factor(data$vasoactive_spec___7,levels=c("0","1"))
data$vasoactive_spec___8.factor = factor(data$vasoactive_spec___8,levels=c("0","1"))
data$vasoactive_spec___9.factor = factor(data$vasoactive_spec___9,levels=c("0","1"))
data$vasoactive_spec___10.factor = factor(data$vasoactive_spec___10,levels=c("0","1"))
data$vasoactive_spec___11.factor = factor(data$vasoactive_spec___11,levels=c("0","1"))
data$vasoactive_spec___12.factor = factor(data$vasoactive_spec___12,levels=c("0","1"))
data$vasoactive_spec___13.factor = factor(data$vasoactive_spec___13,levels=c("0","1"))
data$vasoactive_spec___14.factor = factor(data$vasoactive_spec___14,levels=c("0","1"))
data$vasoactive_spec___15.factor = factor(data$vasoactive_spec___15,levels=c("0","1"))
data$vasoactive_spec___16.factor = factor(data$vasoactive_spec___16,levels=c("0","1"))
data$vasoactive_spec___17.factor = factor(data$vasoactive_spec___17,levels=c("0","1"))
data$vent.factor = factor(data$vent,levels=c("5","1","6","2"))
data$vent_spec.factor = factor(data$vent_spec,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25"))
data$vent_type.factor = factor(data$vent_type,levels=c("1","2"))
data$rass___1.factor = factor(data$rass___1,levels=c("0","1"))
data$rass___2.factor = factor(data$rass___2,levels=c("0","1"))
data$rass___3.factor = factor(data$rass___3,levels=c("0","1"))
data$rass___4.factor = factor(data$rass___4,levels=c("0","1"))
data$rass___5.factor = factor(data$rass___5,levels=c("0","1"))
data$rass___6.factor = factor(data$rass___6,levels=c("0","1"))
data$rass___7.factor = factor(data$rass___7,levels=c("0","1"))
data$rass___8.factor = factor(data$rass___8,levels=c("0","1"))
data$rass___9.factor = factor(data$rass___9,levels=c("0","1"))
data$rass___10.factor = factor(data$rass___10,levels=c("0","1"))
data$mobil.factor = factor(data$mobil,levels=c("1","2","3","4","5","6","7","8","9","10","11"))
data$antibiotic.factor = factor(data$antibiotic,levels=c("1","0"))
data$antibiotic_spec___13.factor = factor(data$antibiotic_spec___13,levels=c("0","1"))
data$antibiotic_spec___1.factor = factor(data$antibiotic_spec___1,levels=c("0","1"))
data$antibiotic_spec___2.factor = factor(data$antibiotic_spec___2,levels=c("0","1"))
data$antibiotic_spec___3.factor = factor(data$antibiotic_spec___3,levels=c("0","1"))
data$antibiotic_spec___4.factor = factor(data$antibiotic_spec___4,levels=c("0","1"))
data$antibiotic_spec___5.factor = factor(data$antibiotic_spec___5,levels=c("0","1"))
data$antibiotic_spec___6.factor = factor(data$antibiotic_spec___6,levels=c("0","1"))
data$antibiotic_spec___7.factor = factor(data$antibiotic_spec___7,levels=c("0","1"))
data$antibiotic_spec___8.factor = factor(data$antibiotic_spec___8,levels=c("0","1"))
data$antibiotic_spec___9.factor = factor(data$antibiotic_spec___9,levels=c("0","1"))
data$antibiotic_spec___10.factor = factor(data$antibiotic_spec___10,levels=c("0","1"))
data$antibiotic_spec___11.factor = factor(data$antibiotic_spec___11,levels=c("0","1"))
data$antibiotic_spec___12.factor = factor(data$antibiotic_spec___12,levels=c("0","1"))
data$antibiotic_spec___15.factor = factor(data$antibiotic_spec___15,levels=c("0","1"))
data$antibiotic_spec___16.factor = factor(data$antibiotic_spec___16,levels=c("0","1"))
data$antibiotic_spec___17.factor = factor(data$antibiotic_spec___17,levels=c("0","1"))
data$antibiotic_spec___18.factor = factor(data$antibiotic_spec___18,levels=c("0","1"))
data$antibiotic_spec___14.factor = factor(data$antibiotic_spec___14,levels=c("0","1"))
data$antiviral.factor = factor(data$antiviral,levels=c("1","0"))
data$transfusion_coag.factor = factor(data$transfusion_coag,levels=c("1","0"))
data$organ_support___1.factor = factor(data$organ_support___1,levels=c("0","1"))
data$organ_support___2.factor = factor(data$organ_support___2,levels=c("0","1"))
data$organ_support___3.factor = factor(data$organ_support___3,levels=c("0","1"))
data$organ_support___4.factor = factor(data$organ_support___4,levels=c("0","1"))
data$organ_support___5.factor = factor(data$organ_support___5,levels=c("0","1"))
data$organ_support___6.factor = factor(data$organ_support___6,levels=c("0","1"))
data$organ_support___7.factor = factor(data$organ_support___7,levels=c("0","1"))
data$organ_support___9.factor = factor(data$organ_support___9,levels=c("0","1"))
data$organ_support___10.factor = factor(data$organ_support___10,levels=c("0","1"))
data$organ_support___8.factor = factor(data$organ_support___8,levels=c("0","1"))
data$medication___1.factor = factor(data$medication___1,levels=c("0","1"))
data$medication___2.factor = factor(data$medication___2,levels=c("0","1"))
data$medication___3.factor = factor(data$medication___3,levels=c("0","1"))
data$medication___4.factor = factor(data$medication___4,levels=c("0","1"))
data$medication___5.factor = factor(data$medication___5,levels=c("0","1"))
data$medication___10.factor = factor(data$medication___10,levels=c("0","1"))
data$medication___11.factor = factor(data$medication___11,levels=c("0","1"))
data$medication___6.factor = factor(data$medication___6,levels=c("0","1"))
data$medication___7.factor = factor(data$medication___7,levels=c("0","1"))
data$medication___8.factor = factor(data$medication___8,levels=c("0","1"))
data$medication___9.factor = factor(data$medication___9,levels=c("0","1"))
data$renal_repl.factor = factor(data$renal_repl,levels=c("1","2","3"))
data$fluid_balance.factor = factor(data$fluid_balance,levels=c("1","2"))
data$ecmella_2.factor = factor(data$ecmella_2,levels=c("1","0"))
data$assess_time_point_labor.factor = factor(data$assess_time_point_labor,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$labor_complete.factor = factor(data$labor_complete,levels=c("0","1","2"))
data$disc_reason_i___1.factor = factor(data$disc_reason_i___1,levels=c("0","1"))
data$disc_reason_i___2.factor = factor(data$disc_reason_i___2,levels=c("0","1"))
data$disc_reason_i___3.factor = factor(data$disc_reason_i___3,levels=c("0","1"))
data$disc_reason_i___4.factor = factor(data$disc_reason_i___4,levels=c("0","1"))
data$disc_reason_i___5.factor = factor(data$disc_reason_i___5,levels=c("0","1"))
data$disc_reason_i___6.factor = factor(data$disc_reason_i___6,levels=c("0","1"))
data$vad_post_imp___5.factor = factor(data$vad_post_imp___5,levels=c("0","1"))
data$vad_post_imp___1.factor = factor(data$vad_post_imp___1,levels=c("0","1"))
data$vad_post_imp___2.factor = factor(data$vad_post_imp___2,levels=c("0","1"))
data$vad_post_imp___3.factor = factor(data$vad_post_imp___3,levels=c("0","1"))
data$vad_post_imp___4.factor = factor(data$vad_post_imp___4,levels=c("0","1"))
data$aki.factor = factor(data$aki,levels=c("1","0"))
data$aki_stage___1.factor = factor(data$aki_stage___1,levels=c("0","1"))
data$aki_stage___2.factor = factor(data$aki_stage___2,levels=c("0","1"))
data$aki_stage___3.factor = factor(data$aki_stage___3,levels=c("0","1"))
data$aki_stage_1___1.factor = factor(data$aki_stage_1___1,levels=c("0","1"))
data$aki_stage_1___2.factor = factor(data$aki_stage_1___2,levels=c("0","1"))
data$aki_stage_3___1.factor = factor(data$aki_stage_3___1,levels=c("0","1"))
data$aki_stage_3___2.factor = factor(data$aki_stage_3___2,levels=c("0","1"))
data$aki_stage_3___3.factor = factor(data$aki_stage_3___3,levels=c("0","1"))
data$complication_time_point.factor = factor(data$complication_time_point,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))
data$neurological_complications.factor = factor(data$neurological_complications,levels=c("1","0"))
data$brain_death.factor = factor(data$brain_death,levels=c("1","0"))
data$seizures_clinic.factor = factor(data$seizures_clinic,levels=c("1","0"))
data$seizures_eeg.factor = factor(data$seizures_eeg,levels=c("1","0"))
data$cns_diffuse_ischemia.factor = factor(data$cns_diffuse_ischemia,levels=c("1","0"))
data$cns_local_ischemia.factor = factor(data$cns_local_ischemia,levels=c("1","0"))
data$cns_hemorrhage.factor = factor(data$cns_hemorrhage,levels=c("1","0"))
data$intraventricular_cns_hemorrhage.factor = factor(data$intraventricular_cns_hemorrhage,levels=c("1","0"))
data$neurosurgical_intervention.factor = factor(data$neurosurgical_intervention,levels=c("1","0"))
data$nc_o.factor = factor(data$nc_o,levels=c("1","0"))
data$limb_complications.factor = factor(data$limb_complications,levels=c("1","0"))
data$compartment_syndrome.factor = factor(data$compartment_syndrome,levels=c("1","0"))
data$fasciotomy.factor = factor(data$fasciotomy,levels=c("1","0"))
data$amputation.factor = factor(data$amputation,levels=c("1","0"))
data$limb_cannula.factor = factor(data$limb_cannula,levels=c("1","0"))
data$lc_o.factor = factor(data$lc_o,levels=c("1","0"))
data$kidney_comp.factor = factor(data$kidney_comp,levels=c("1","0"))
data$crea1.factor = factor(data$crea1,levels=c("1","0"))
data$crea2.factor = factor(data$crea2,levels=c("1","0"))
data$cvvhdf.factor = factor(data$cvvhdf,levels=c("1","0"))

levels(data$redcap_event_name.factor)=c("Baseline (Arm 1: ECLS)","ECLS (Arm 1: ECLS)","Baseline (Arm 2: ECLS + Impella)","ECLS  (Arm 2: ECLS + Impella)","Impella (Arm 2: ECLS + Impella)","Baseline (Arm 3: Impella)","Impella (Arm 3: Impella)","Baseline (Arm 4: OxyRVAD/vvECMO)","ECLS (Arm 4: OxyRVAD/vvECMO)")
levels(data$redcap_repeat_instrument.factor)=c("Surgery","Coronary Angiography","Echocardiography","Infections","Additional MCS","ECLS-Mode","ECLS-Equipment","Pump","Hemodynamics, Ventilation, Medication","Labor","Complications","Mode and Equipment Impella","Impella-Assessment and Complications","Impella - Follow up")
levels(data$sex.factor)=c("female","male")
levels(data$history_arrhythmia.factor)=c("Ja","Nein")
levels(data$history_arrhythmia_spec___1.factor)=c("Unchecked","Checked")
levels(data$history_arrhythmia_spec___2.factor)=c("Unchecked","Checked")
levels(data$history_arrhythmia_spec___3.factor)=c("Unchecked","Checked")
levels(data$history_arrhythmia_spec___4.factor)=c("Unchecked","Checked")
levels(data$history_arrhythmia_spec___5.factor)=c("Unchecked","Checked")
levels(data$history_arrhythmia_spec___6.factor)=c("Unchecked","Checked")
levels(data$history_arrhythmia_spec___7.factor)=c("Unchecked","Checked")
levels(data$diabetes.factor)=c("Ja","Nein")
levels(data$diabetes_spec.factor)=c("Typ I","Typ II - diet treated","Typ II - drug treated","Typ II - insulin dependent","unknown")
levels(data$ckd.factor)=c("Ja","Nein")
levels(data$ckd_spec.factor)=c("Normal or high GFR (GFR > 90 mL/min)","Mild CKD (GFR = 60-89 mL/min)","Moderate CKD (GFR = 45-59 mL/min)","Moderate CKD (GFR = 30-44 mL/min)","Severe CKD (GFR = 15-29 mL/min)","End Stage CKD (GFR < 15 mL/min)")
levels(data$copd.factor)=c("Ja","Nein")
levels(data$copd_spec.factor)=c("Stadium I (only drug Treatment if needed)","Stadium II (>= 1 long acting bronchodilator)","Stadium III (>=1 long acting bronchodilator plus inhalative steroids)","Stadium IV (mentioned drug treatment plus oxygen therapy)","unknown")
levels(data$extub.factor)=c("orotracheally extubated","transferred intubated","intubated at time of death","tracheostomy","transferred with tracheostoma","no extubation before death")
levels(data$tracheo_decann.factor)=c("Ja","Nein")
levels(data$discharge___1.factor)=c("Unchecked","Checked")
levels(data$discharge___2.factor)=c("Unchecked","Checked")
levels(data$discharge___3.factor)=c("Unchecked","Checked")
levels(data$discharge___4.factor)=c("Unchecked","Checked")
levels(data$discharg_location.factor)=c("Home","Transferred to other Hospital","Transferred to Long Term Care or Rehab","Transferred to Hospice","Unknown")
levels(data$death.factor)=c("Ja","Nein")
levels(data$reason_ecls.factor)=c("Ischemic Cardiomyopathy","Acute Myocardial Infarction","Dilatative Cardiomyopathy","Valvular Cardiomyopathy","Myokarditis","Postpartum Cardiomyopathy","Toxic Cardiomyopathy","Postcardiotomy Syndrom","Pulmonary Embolism","Acute Lung Injury","Ventricular Septal Defect","Cardiopulmonary Reanimation","{reason_ecls_other}")
levels(data$pre_postcard.factor)=c("Ja","Nein")
levels(data$pre_failure_cpb.factor)=c("Ja","Nein")
levels(data$pre_mi.factor)=c("Ja","Nein")
levels(data$pre_mi_type.factor)=c("NSTEMI","STEMI","unknown")
levels(data$pre_cardiac_arrest.factor)=c("Ja","Nein")
levels(data$pre_cardiac_arrest_type___1.factor)=c("Unchecked","Checked")
levels(data$pre_cardiac_arrest_type___2.factor)=c("Unchecked","Checked")
levels(data$pre_cardiac_arrest_type___3.factor)=c("Unchecked","Checked")
levels(data$pre_cardiac_arrest_rhythm___1.factor)=c("Unchecked","Checked")
levels(data$pre_cardiac_arrest_rhythm___2.factor)=c("Unchecked","Checked")
levels(data$pre_cardiac_arrest_rhythm___3.factor)=c("Unchecked","Checked")
levels(data$pre_cardiac_arrest_rhythm___4.factor)=c("Unchecked","Checked")
levels(data$pre_cardiac_arrest_rhythm___5.factor)=c("Unchecked","Checked")
levels(data$intubation.factor)=c("Yes","No","Pre-Existing tracheostomy")
levels(data$intubation_after.factor)=c("Ja","Nein")
levels(data$ventil_date.factor)=c("Ja","Nein")
levels(data$ventil_time.factor)=c("Ja","Nein")
levels(data$pre_renal_repl.factor)=c("Ja","Nein")
levels(data$pre_renal_repl_spec.factor)=c("chronic hemodialysis","home peritoneal dialysis","hemodialysis due to AKI","CVVHDF due to AKI")
levels(data$pre_arrhythmia.factor)=c("Ja","Nein")
levels(data$pre_arrhythmia_spec___1.factor)=c("Unchecked","Checked")
levels(data$pre_arrhythmia_spec___2.factor)=c("Unchecked","Checked")
levels(data$pre_arrhythmia_spec___3.factor)=c("Unchecked","Checked")
levels(data$pre_arrhythmia_spec___4.factor)=c("Unchecked","Checked")
levels(data$pre_arrhythmia_spec___5.factor)=c("Unchecked","Checked")
levels(data$pre_arrhythmia_spec___6.factor)=c("Unchecked","Checked")
levels(data$pre_arrhythmia_spec___7.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___1.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___2.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___3.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___4.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___5.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___6.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___7.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___8.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___9.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___10.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___11.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___12.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___13.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___14.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___15.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___16.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___17.factor)=c("Unchecked","Checked")
levels(data$pre_vasoactive___18.factor)=c("Unchecked","Checked")
levels(data$pre_ventilation.factor)=c("Invasive Ventilation","Non invasive Ventilation","High Flow Therapy","Hand Bag Valve Ventilation","No ventilation")
levels(data$pre_vent_spec.factor)=c("IPPV","BIPAP","SIMV","ASB","PC-BIPAP","PC-PSV","PC-CMV","PC-SIMV","PC-PC-APRV","PC-AC","VC-CMV","VC-SIMV","VC-MMV","VC-AC","SPN-CPAP/PS","BiLevel","A/C VC","A/C PC","A/C PRVC","SIMV-VC","SIMV-PC","BiLevel-VG","CPAP/PS","SBT","NIV")
levels(data$pre_vent_type.factor)=c("Conventional","HFO")
levels(data$pre_lab_results_elso.factor)=c("last 6 hours","last 24 hours")
levels(data$pre_crp_m.factor)=c("Ja","Nein")
levels(data$pre_pct_m.factor)=c("Ja","Nein")
levels(data$pre_trop_m.factor)=c("Ja","Nein")
levels(data$prevaecls_hemodynamics_ventilation_labor_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$ecls_order.factor)=c("1","2","3","4","5","6","7","8","9","10")
levels(data$ecls_icu.factor)=c("Adult Cardiac","Adult Non Surgical","Cardiovascular ICU","ECLS ICU","Emergency Department","Surgical ICU","Operating Room","out of hospital")
levels(data$ecls_type.factor)=c("Pulmonary","Cardiac","ECPR")
levels(data$ecls_mode.factor)=c("veno-arterial","veno-venous","veno-veno-arterial","veno-arterial-venous","veno-veno--veno-arterial","oxy-RVAD","A-VCO2R","V-V ECCO2R")
levels(data$disc_reason___1.factor)=c("Unchecked","Checked")
levels(data$disc_reason___2.factor)=c("Unchecked","Checked")
levels(data$disc_reason___3.factor)=c("Unchecked","Checked")
levels(data$disc_reason___4.factor)=c("Unchecked","Checked")
levels(data$disc_reason___14.factor)=c("Unchecked","Checked")
levels(data$disc_reason___15.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___1.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___2.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___3.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___4.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___5.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___6.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___7.factor)=c("Unchecked","Checked")
levels(data$mcs_post_ecls___8.factor)=c("Unchecked","Checked")
levels(data$impella_impl_place.factor)=c("Hybrid OR","Cath Lab")
levels(data$impella_impl_form.factor)=c("interventional","surgical")
levels(data$impella_type.factor)=c("Impella 2.5","Impella CP","Impella 5.0","Impella 5.5","Impella RP")
levels(data$impella_access_site.factor)=c("Ascending Aorta","Arteria axillaris right","Arteria axillaris left","Arteria femoralis right","Arteria femoralis left","Vena femoralis right","Vena femoralis left")
levels(data$ecmella.factor)=c("Ja","Nein")
levels(data$assess_time_point.factor)=c("24h (day 1)","48h (day 2)","72h (day 3)","96h (day 4)","120h (day 5)","144h (day 6)","168h (day 7)","192h (day 8)","216h (day 9)","240h (day 10)")
levels(data$vasoactive_med.factor)=c("Ja","Nein")
levels(data$vasoactive_spec___1.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___2.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___3.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___4.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___5.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___6.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___7.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___8.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___9.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___10.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___11.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___12.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___13.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___14.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___15.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___16.factor)=c("Unchecked","Checked")
levels(data$vasoactive_spec___17.factor)=c("Unchecked","Checked")
levels(data$vent.factor)=c("Invasive Ventilation","Non invasive Ventilation","High Flow Therapy","No Ventilation")
levels(data$vent_spec.factor)=c("IPPV","BIPAP","SIMV","ASB","PC-BIPAP","PC-PSV","PC-CMV","PC-SIMV","PC-PC-APRV","PC-AC","VC-CMV","VC-SIMV","VC-MMV","VC-AC","SPN-CPAP/PS","BiLevel","A/C VC","A/C PC","A/C PRVC","SIMV-VC","SIMV-PC","BiLevel-VG","CPAP/PS","SBT","NIV")
levels(data$vent_type.factor)=c("Conventional","HFO")
levels(data$rass___1.factor)=c("Unchecked","Checked")
levels(data$rass___2.factor)=c("Unchecked","Checked")
levels(data$rass___3.factor)=c("Unchecked","Checked")
levels(data$rass___4.factor)=c("Unchecked","Checked")
levels(data$rass___5.factor)=c("Unchecked","Checked")
levels(data$rass___6.factor)=c("Unchecked","Checked")
levels(data$rass___7.factor)=c("Unchecked","Checked")
levels(data$rass___8.factor)=c("Unchecked","Checked")
levels(data$rass___9.factor)=c("Unchecked","Checked")
levels(data$rass___10.factor)=c("Unchecked","Checked")
levels(data$mobil.factor)=c("Nothing (lying in bed)","Sitting in bed, exercises in bed","Passively moved to chair (no Standing)","Sitting over edge of bed","Standing (with or without assist)","Transferring bed to chair","Marching on spot (at bedside)","Walking with assistance of 2 or more People","Walking with assistance of 1 Person","Walking independently with a gail aid","Walking independently without a gail aid")
levels(data$antibiotic.factor)=c("Ja","Nein")
levels(data$antibiotic_spec___13.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___1.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___2.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___3.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___4.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___5.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___6.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___7.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___8.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___9.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___10.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___11.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___12.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___15.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___16.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___17.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___18.factor)=c("Unchecked","Checked")
levels(data$antibiotic_spec___14.factor)=c("Unchecked","Checked")
levels(data$antiviral.factor)=c("Ja","Nein")
levels(data$transfusion_coag.factor)=c("Ja","Nein")
levels(data$organ_support___1.factor)=c("Unchecked","Checked")
levels(data$organ_support___2.factor)=c("Unchecked","Checked")
levels(data$organ_support___3.factor)=c("Unchecked","Checked")
levels(data$organ_support___4.factor)=c("Unchecked","Checked")
levels(data$organ_support___5.factor)=c("Unchecked","Checked")
levels(data$organ_support___6.factor)=c("Unchecked","Checked")
levels(data$organ_support___7.factor)=c("Unchecked","Checked")
levels(data$organ_support___9.factor)=c("Unchecked","Checked")
levels(data$organ_support___10.factor)=c("Unchecked","Checked")
levels(data$organ_support___8.factor)=c("Unchecked","Checked")
levels(data$medication___1.factor)=c("Unchecked","Checked")
levels(data$medication___2.factor)=c("Unchecked","Checked")
levels(data$medication___3.factor)=c("Unchecked","Checked")
levels(data$medication___4.factor)=c("Unchecked","Checked")
levels(data$medication___5.factor)=c("Unchecked","Checked")
levels(data$medication___10.factor)=c("Unchecked","Checked")
levels(data$medication___11.factor)=c("Unchecked","Checked")
levels(data$medication___6.factor)=c("Unchecked","Checked")
levels(data$medication___7.factor)=c("Unchecked","Checked")
levels(data$medication___8.factor)=c("Unchecked","Checked")
levels(data$medication___9.factor)=c("Unchecked","Checked")
levels(data$renal_repl.factor)=c("Hemodialysis","Continuous Hemofiltration","No")
levels(data$fluid_balance.factor)=c("positive","negative")
levels(data$ecmella_2.factor)=c("Ja","Nein")
levels(data$assess_time_point_labor.factor)=c("24h (day 1)","48h (day 2)","72h (day 3)","96h (day 4)","120h (day 5)","144h (day 6)","168h (day 7)","192h (day 8)","216h (day 9)","240h (day 10)")
levels(data$labor_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$disc_reason_i___1.factor)=c("Unchecked","Checked")
levels(data$disc_reason_i___2.factor)=c("Unchecked","Checked")
levels(data$disc_reason_i___3.factor)=c("Unchecked","Checked")
levels(data$disc_reason_i___4.factor)=c("Unchecked","Checked")
levels(data$disc_reason_i___5.factor)=c("Unchecked","Checked")
levels(data$disc_reason_i___6.factor)=c("Unchecked","Checked")
levels(data$vad_post_imp___5.factor)=c("Unchecked","Checked")
levels(data$vad_post_imp___1.factor)=c("Unchecked","Checked")
levels(data$vad_post_imp___2.factor)=c("Unchecked","Checked")
levels(data$vad_post_imp___3.factor)=c("Unchecked","Checked")
levels(data$vad_post_imp___4.factor)=c("Unchecked","Checked")
levels(data$aki.factor)=c("Ja","Nein")
levels(data$aki_stage___1.factor)=c("Unchecked","Checked")
levels(data$aki_stage___2.factor)=c("Unchecked","Checked")
levels(data$aki_stage___3.factor)=c("Unchecked","Checked")
levels(data$aki_stage_1___1.factor)=c("Unchecked","Checked")
levels(data$aki_stage_1___2.factor)=c("Unchecked","Checked")
levels(data$aki_stage_3___1.factor)=c("Unchecked","Checked")
levels(data$aki_stage_3___2.factor)=c("Unchecked","Checked")
levels(data$aki_stage_3___3.factor)=c("Unchecked","Checked")
levels(data$complication_time_point.factor)=c("24h (day 1)","48h (day 2)","72h (day 3)","96h (day 4)","120h (day 5)","144h (day 6)","168h (day 7)","192h (day 8)","216h (day 9)","240h (day 10)","day 11","day 12","day 13","day 14","day 15","day 16","day 17","day 18","day 19","day 20")
levels(data$neurological_complications.factor)=c("Ja","Nein")
levels(data$brain_death.factor)=c("Ja","Nein")
levels(data$seizures_clinic.factor)=c("Ja","Nein")
levels(data$seizures_eeg.factor)=c("Ja","Nein")
levels(data$cns_diffuse_ischemia.factor)=c("Ja","Nein")
levels(data$cns_local_ischemia.factor)=c("Ja","Nein")
levels(data$cns_hemorrhage.factor)=c("Ja","Nein")
levels(data$intraventricular_cns_hemorrhage.factor)=c("Ja","Nein")
levels(data$neurosurgical_intervention.factor)=c("Ja","Nein")
levels(data$nc_o.factor)=c("Ja","Nein")
levels(data$limb_complications.factor)=c("Ja","Nein")
levels(data$compartment_syndrome.factor)=c("Ja","Nein")
levels(data$fasciotomy.factor)=c("Ja","Nein")
levels(data$amputation.factor)=c("Ja","Nein")
levels(data$limb_cannula.factor)=c("Ja","Nein")
levels(data$lc_o.factor)=c("Ja","Nein")
levels(data$kidney_comp.factor)=c("Ja","Nein")
levels(data$crea1.factor)=c("Ja","Nein")
levels(data$crea2.factor)=c("Ja","Nein")
levels(data$cvvhdf.factor)=c("Ja","Nein")
