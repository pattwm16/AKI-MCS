# table 1
library(tidyverse)
library(flextable)
library(table1)

# read data
data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(cs_etiology = forcats::fct_relevel(cs_etiology, "Other", , after = 3)) %>%
  mutate(ckd_stage = as.ordered(ckd_stage)) %>%
  mutate(copd_stage = as.ordered(copd_stage)) %>%
  mutate(pre_ph = pre_ph / 100) #%>%
  # ensure this reflects the regression dataset
  #filter(complete.cases(age, sex, bmi, pre_lactate, log_vis_score, rrt_group, group, aki_max, aki_yn, hosp_surv_yn))

# parse labels and units
label(data$age)          <- "Age"
label(data$sex)          <- "Sex"
label(data$bmi)          <- "BMI"
label(data$diabetes)     <- "Diabetes (Type II)"
label(data$ckd_yn)       <- "Chronic kidney disease"
label(data$copd_yn)      <- "Chronic obstructive pulmonary disease"
label(data$copd_stage)   <- "Chronic obstructive pulmonary disease (stage)"
label(data$pre_lactate)  <- "Lactate (prior to tMCS)"
label(data$pre_ph)       <- "pH (prior to tMCS)"
label(data$pre_cr)       <- "Creatinine (prior to tMCS)"
label(data$vis_score)    <- "Vasoactive-inotropic score (VIS)ᵃ"
label(data$pre_rrt_yn)   <- "RRT required (prior to tMCS)"
label(data$mi_yn)        <- "Previous cardiac arrest"
label(data$ecpr)         <- "eCPR"
label(data$rx_nephrotox) <- "Nephrotoxic drugsᵇ"
label(data$cs_etiology)  <- "Etiology of cardiogenic shock"

units(data$age)     <- "years"
units(data$pre_lactate) <- "mg/dL" # TODO: is this the right unit?
units(data$med_cr)  <- "mg/dL" # TODO: is this the right unit?

footnote <- paste0("ᵃ VIS, a weighted sum of vasopressor and inotropic medications, quantifies the amount of pharmacological cardiovascular support in patients on tMCS.<br>", "ᵇ Vancomycin (IV), gentamycin, or tobramycin")

# create table 1
# TODO: should we report log(vis_score)?
(tab1 <- data %>%
  table1(
    ~ age + sex + bmi + diabetes + ckd_yn + copd_yn +
      pre_lactate + pre_ph + vis_score + pre_rrt_yn + pre_cr + cs_etiology + mi_yn +
      ecpr + rx_nephrotox | group,
    data = ., footnote = footnote
  ))

# save tables as .docx and image
t1flex(tab1) %>%
  save_as_docx(path = "tbls/table1.docx")

t1flex(tab1) %>%
  bg(bg = "white", part = "all") %>%
  save_as_image(path = "tbls/table1.png")
