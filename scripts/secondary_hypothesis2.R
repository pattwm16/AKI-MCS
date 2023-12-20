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
model <- matched_data %>%
  glm(hosp_surv_yn ~ group * aki_yn,
      family = binomial, weights = weights, data = .)

plot(effects::predictorEffect(model, pred = "group", xlevels = list(group = c("ECMELLA", "va-ECLS"))))

model %>%
  tbl_regression(., exponentiate = TRUE) %>%
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/secondary_hypothesis_2.docx")