# table 1
library(tidyverse)
library(flextable)
library(table1)

data <- read_csv("data/cleaned_analysis_data.csv")

# parse label
label(data$age) <- "Age"
label(data$sex) <- "Sex"
label(data$bmi) <- "BMI"
label(data$diabetes) <- "Type II Diabetes" # TODO: I assume?
label(data$ckd_yn) <- "Chronic kidney disease"
label(data$ckd_stage) <- "Chronic kidney disease stage"
label(data$copd_yn) <- "Chronic obstructive pulmonary disease"
label(data$copd_stage) <- "Chronic obstructive pulmonary disease stage"
label(data$lactate) <- "Lactate"
label(data$med_ph) <- "Median pH"
label(data$med_cr) <- "Median creatinine"
label(data$vis_score) <- "Vasoactive-inotropic score (VIS)"
label(data$pre_rrt_yn) <- "RRT prior to tMCS"
label(data$mi_yn) <- "Previous cardiac arrest"
label(data$ecpr) <- "eCPR"
label(data$rx_nephrotox) <- "Nephrotoxic drugs"
label(data$cs_etiology) <- "Cause of cardiogenic shock"

units(data$age)     <- "years"
units(data$lactate) <- "mg/dL" # TODO: is this the right unit?
units(data$med_cr)  <- "mg/dL" # TODO: is this the right unit?


# create table 1
tab1 <- data %>%
  table1(
  ~ age + sex + bmi + diabetes + ckd_yn + ckd_stage + copd_yn + copd_stage +
    lactate + med_ph + vis_score + pre_rrt_yn + med_cr + cs_etiology + mi_yn + 
    ecpr + rx_nephrotox | group, 
  data = .
  )

# save tables as .docx and image
t1flex(tab1) %>% 
  save_as_docx(path="tbls/table1.docx")

t1flex(tab1) %>% 
  bg(bg = "white", part = "all") %>%
  save_as_image(path="tbls/table1.png")
