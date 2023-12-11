# analysis

# libraries
library(MatchIt)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv")

# patients in CS w/ tMCS have lower survival to hospital discharge rate
# when additionally needing RRT

# identify patients in CS w/ tMCS
# treatment is rrt
# outcome is hosp_surv_yn
matched_data %>%
  filter(cs_etiology != "No shock") %>%
  group_by(rrt_yn) %>%
  tabyl(rrt_yn, hosp_surv_yn)




fit <- lm(hosp_surv_yn ~ rrt_yn * (age + sex + bmi + lactate + vis_score),
          data = matched_data, weights = weights)

plot(fit)

marginaleffects::avg_comparisons(fit,
                variables = "rrt_yn",
                vcov = ~subclass,
                newdata = subset(matched_data, rrt_yn == 0),
                wts = "weights")

# sec 1

# sec 2
fit1 <- lm(death ~ aki_yn * (age + sex + bmi + lactate + vis_score),
          data = matched_data, weights = weights)

marginaleffects::avg_comparisons(fit1,
                                 variables = "aki_yn",
                                 vcov = ~subclass,
                                 newdata = subset(matched_data, aki_yn == 1),
                                 wts = "weights")
