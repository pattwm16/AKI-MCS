# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(effects)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv")


# secondary hypothesis 1 ---
# survival to hospital discharge in patients treated with tMCS or CS suffering
# from AKI correlates inversely with the severity of AKI-stadium

# fit logistic regression model
model <- matched_data %>%
  mutate(aki_max = forcats::fct_relevel(aki_max,
                               "no aki",
                               "s1",
                               "s2",
                               "s3")) %>%
  mutate(aki_max = as.factor(ifelse(aki_max %in% c("no aki", "s1"),
  "no aki/s1",
  aki_max))) %>%
  MASS::polr(aki_max ~ group,
             data = ., Hess = TRUE)

plot(effects::predictorEffect(model, pred = "group", xlevels = list(group = c("ECMELLA", "va-ECLS"))))

model %>%
  tbl_regression(., exponentiate = TRUE) %>%
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/secondary_hypothesis_1.docx")
