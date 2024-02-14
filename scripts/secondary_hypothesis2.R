# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv")

# label matched_data
labelled::var_label(matched_data) <- list(
  group = 'tCMS group',
  aki_yn = 'AKI on tCMS'
)

# secondary hypothesis 2 ---
# survival to hospital discharge in ECMELLA patients with AKI is superior to
# va-ECLS patients with AKI (dichotomous)

# ECMELLA pts vs. va-ECLS pts + aki_yn
matched_data %>%
  group_by(group) %>%
  tabyl(aki_yn, hosp_surv_yn)

# fit logistic regression model
model.full <- data %>%
  glm(hosp_surv_yn ~ age + sex + bmi + vis_score + pre_cr + rrt_group + group * aki_yn,
      family = binomial, data = .)

plot(effects::predictorEffect(model, pred = "group", xlevels = list(group = c("ECMELLA", "va-ECLS"))))

model.full %>%
  tbl_regression(., exponentiate = TRUE) %>%
  add_n() %>%
  italicize_levels() %>%
  add_global_p() %>%
  add_glance_source_note() %>%
  modify_caption("**Secondary hypothesis 2: Survival to hospital discharge in ECMELLA patients with AKI is superior to va-ECLS patients with AKI (dichotomous)**") %>%
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/secondary_hypothesis_2.docx")
