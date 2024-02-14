# libraries
library(WeightIt)
library(cobalt)
library(splines)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(group = as.factor(group)) %>%
  # TODO: this reduces the number of effective cases for comparison
  # age: 99.8%; sex: 100%; bmi: 92.0%; lactate: 83.2%;
  # vis_score: 48.2%
  filter(complete.cases(age, sex, bmi, lactate, vis_score))

# what interactions should we take into account?
data %>%
  select(age, sex, bmi, lactate, vis_score) %>%
  mutate(sex = as.factor(sex)) %>%
  mutate_all(as.double) %>%
  as.matrix() %>%
  Hmisc::rcorr(., type = c("pearson", "spearman")) %>%
  broom::tidy() %>%
  filter(abs(estimate) >= 0.3) # consider correlations above 0.3

# propensity score matching
# age, sex, bmi, lactate, vis
# 1:1 NN PS matching w/o replacement

weighted_subjects <- data %>%
  weightit(formula = hosp_surv_yn ~ ns(age,2) + sex + bmi + lactate + vis_score,
           data = ., method = "ps", estimand = "ATT")
bal.tab(weighted_subjects)
summary(weighted_subjects)

v <- data.frame(old = c("age", "sex_male", "bmi", "mi_yn", "ecpr", "rx_nephrotox"),
                new = c("Age", "Sex (ref = Male)", "BMI",
                        "Prior MI", "eCPR", "Nephrotoxic drugs"))

# love plot to assess balance
(weighted_subjects %>% love.plot(thresholds = c(m = .1),
                                var.order = "unadjusted",
                                var.names = v,
                                position = "bottom",
                                abs = TRUE,
                                line = TRUE,
                                stars = "raw"))

# generate table with balance statistics
(data %>% bal.tab(group ~ age + sex + bmi + cs_etiology + mi_yn + ecpr + rx_nephrotox, data = .,
                  thresholds = c(m = .1, v = 2),
                  s.d.denom = "pooled"))
(weighted_subjects %>% bal.tab(thresholds = c(m = .1, v = 2)))

# save plot
ggsave("figs/love_plot.png",
       width = 12.1, units = c("cm"))

weighted_data <- data %>%
  mutate(weights = weighted_subjects$weights)

write_csv(weighted_data, "data/cleaned_weighted_data.csv",
          na = "NA", append = FALSE)
