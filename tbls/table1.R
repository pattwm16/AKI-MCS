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
# label(data$lactate) <- "Lactate"
# label(data$ph) <- "pH"
label(data$vis_score) <- "Vasoactive-inotropic score (VIS)"
label(data$rrt_yn) <- "RRT prior to tMCS"
label(data$mi_yn) <- "Previous cardiac arrest"
label(data$ecpr) <- "eCPR"
# label(data$neph_tox) <- "Nephrotoxic drugs"
label(data$cs_etiology) <- "Cause of cardiogenic shock"

units(data$age)   <- "years"

# create table 1
tab1 <- table1(
  # TODO: [add when cleaned in clean.R] cr + neph_tox + lactate + ph +
  ~ age + sex + bmi + diabetes + ckd_yn + ckd_stage + copd_yn + copd_stage +
  vis_score + rrt_yn + cs_etiology + mi_yn + ecpr |
    group, 
  data = data
  )

t1flex(tab1) %>% 
  bg(bg = "white", part = "all") %>%
  save_as_docx(path="tbls/table1.docx")

t1flex(tab1) %>% 
  save_as_image(path="tbls/table1.png")