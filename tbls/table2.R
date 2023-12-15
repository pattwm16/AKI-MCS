# table 2
library(tidyverse)
library(flextable)
library(table1)

#load("../data/cleaned_analysis_data.Rda")
data <- read_csv("data/cleaned_matched_data.csv")

# parse label
label(data$aki_s1)          <- "AKI - Stadium 1"
label(data$aki_s2)          <- "AKI - Stadium 2"
label(data$aki_s3)          <- "AKI - Stadium 3"
label(data$pre_rrt_yn)      <- "RRT prior to tMCS"
label(data$rrt_duration)    <- "Duration of RRT"
label(data$vent_duration)   <- "Duration of ventilation"
#label(data$icu_los)        <- "Length of ICU stay"
label(data$hosp_los)        <- "Length of hospital stay"
label(data$hosp_surv_yn)    <- "Survival to hospital discharge"

units(data$rrt_duration)    <- "days"
units(data$vent_duration)   <- "days"
#units(data$icu_los)        <- "days"
units(data$hosp_los)        <- "days"


# create table 2
tab2 <- data %>%
  table1(
    ~ aki_s1 + aki_s2 + aki_s3 + pre_rrt_yn + pre_rrt_yn + rrt_duration +
      vent_duration +
      # icu_los +
      hosp_los + hosp_surv_yn | group,
    data = .
  )

# save tables as .docx and image
t1flex(tab2) %>%
  save_as_docx(path = "tbls/table2.docx")

t1flex(tab2) %>%
  bg(bg = "white", part = "all") %>%
  save_as_image(path = "tbls/table2.png")
