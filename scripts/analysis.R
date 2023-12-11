# analysis

# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv")

# primary hypothesis ---
# patients in CS w/ tMCS have lower survival to hospital discharge rate
# when additionally needing RRT

# identify patients in CS w/ tMCS
# treatment is rrt
# outcome is hosp_surv_yn
matched_data %>%
  filter(cs_etiology != "No shock") %>%
  group_by(rrt_yn) %>%
  tabyl(rrt_yn, hosp_surv_yn)

# Fit the logistic regression model
matched_data %>%
  filter(cs_etiology != "No shock") %>%
  glm(hosp_surv_yn ~ rrt_yn, family = binomial, weights = weights, data = .) %>%
  tbl_regression(., exponentiate = TRUE) %>%

  # save as .docx
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/primary_hypothesis.docx")

# secondary hypothesis 1 ---
# survival to hospital discharge in patients treated with tMCS or CS suffering
# from AKI correlates inversely with the severity of AKI-stadium

# ANALYSIS TBD...

# secondary hypothesis 2 ---
# survival to hospital discharge in ECMELLA patients with AKI is superior to
# va-ECLS patients with AKI (dichotomous)

# ECMELLA pts vs. va-ECLS pts + aki_yn
matched_data %>%
  group_by(group) %>%
  tabyl(aki_yn, hosp_surv_yn)

# fit logistic regression model
matched_data %>%
  glm(hosp_surv_yn ~ aki_yn * group, family = binomial, weights = weights, data = .) %>%
  tbl_regression(., exponentiate = TRUE) %>%
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/secondary_hypothesis_2.docx")
