# table 1
library(tidyverse)
library(flextable)
library(table1)

# read data
data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(cs_etiology = forcats::fct_relevel(cs_etiology, "No shock")) %>%
  mutate(cs_etiology = forcats::fct_relevel(cs_etiology, "Other", , after = length(levels(cs_etiology)))) %>%
  mutate(ckd_stage = as.ordered(ckd_stage)) %>%
  mutate(copd_stage = as.ordered(copd_stage))

# parse labels and units
label(data$age)          <- "Age"
label(data$sex)          <- "Sex"
label(data$bmi)          <- "BMI"
label(data$diabetes)     <- "Diabetes (Type II)"     # TODO: I assume?
label(data$ckd_yn)       <- "Chronic kidney disease"
label(data$ckd_stage)    <- "Chronic kidney disease (stage)ᵃ"
label(data$copd_yn)      <- "Chronic obstructive pulmonary disease"
label(data$copd_stage)   <- "Chronic obstructive pulmonary disease (stage)"
label(data$lactate)      <- "Lactate"
label(data$med_ph)       <- "Median pH"
label(data$med_cr)       <- "Median creatinine"
label(data$vis_score)    <- "Vasoactive-inotropic score (VIS)ᵇ"
label(data$pre_rrt_yn)   <- "RRT required (prior to tMCS)"
label(data$mi_yn)        <- "Previous cardiac arrest"
label(data$ecpr)         <- "eCPR"
label(data$rx_nephrotox) <- "Nephrotoxic drugsᶜ "
label(data$cs_etiology)  <- "Etiology of cardiogenic shock"

units(data$age)     <- "years"
units(data$lactate) <- "mg/dL" # TODO: is this the right unit?
units(data$med_cr)  <- "mg/dL" # TODO: is this the right unit?

footnote <- paste0("ᵃ Stage 3a and 3b were coded as 3 and 4, respectively. The remaining stages were increased by one value.<br>", "ᵇ VIS, a weighted sum of vasopressor and inotropic medications, quantifies the amount of pharmacological cardiovascular support in patients on tMCS.<br>", "ᶜ  Vancomycin (IV), gentamycin, or tobramycin")

# create table 1
tab1 <- data %>%
  table1(
    ~ age + sex + bmi + diabetes + ckd_yn + ckd_stage + copd_yn + copd_stage +
      lactate + med_ph + vis_score + pre_rrt_yn + med_cr + cs_etiology + mi_yn +
      ecpr + rx_nephrotox | group,
    data = ., footnote = footnote
  )

# save tables as .docx and image
t1flex(tab1) %>%
  save_as_docx(path = "tbls/table1.docx")

t1flex(tab1) %>%
  bg(bg = "white", part = "all") %>%
  save_as_image(path = "tbls/table1.png")
