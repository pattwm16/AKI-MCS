# table 2
library(tidyverse)
library(flextable)
library(table1)

data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(
    vent_duration = as.numeric(vent_duration, units = "days"),
    aki_max = case_when(
    aki_max == "no aki" ~ "No AKI",
    aki_max == "s1" ~ "Stage 1",
    aki_max == "s2" ~ "Stage 2",
    aki_max == "s3" ~ "Stage 3",
    aki_max == "rrt" ~ "RRT",
    TRUE ~ aki_max
  )) %>%
  mutate(aki_max = fct_relevel(aki_max, "No AKI", "AKI stage 1", "AKI stage 2", "AKI stage 3", "RRT"))

# parse label
label(data$aki_max)         <- "Max KDIGO AKI Stage" # TODO: during what time were we looking?
label(data$pre_rrt_yn)      <- "RRT prior to tMCS"
label(data$rrt_duration)    <- "Duration of RRT"
label(data$vent_duration)   <- "Duration of ventilation"
label(data$icu_los)         <- "Length of ICU stay"
label(data$hosp_los)        <- "Length of hospital stay"
label(data$hosp_surv_yn)    <- "Survival to hospital discharge"

# add labels
units(data$rrt_duration)    <- "days"
units(data$vent_duration)   <- "days"
units(data$icu_los)         <- "days"
units(data$hosp_los)        <- "days"

# create table 2
(tab2 <- data %>%
  table1(
    ~ aki_max + pre_rrt_yn + pre_rrt_yn + rrt_duration +
      vent_duration +
      icu_los +
      hosp_los + hosp_surv_yn | group,
    data = .
  ))

# save tables as .docx and image
t1flex(tab2) %>%
  save_as_docx(path = "tbls/table2.docx")

t1flex(tab2) %>%
  bg(bg = "white", part = "all") %>%
  save_as_image(path = "tbls/table2.png")
