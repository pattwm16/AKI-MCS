# analysis

# libraries
library(MatchIt)
library(tidyverse)

# data load
data <- read_csv("../data/cleaned_analysis_data.csv")

# propensity score matching
# age, sex, bmi, lactate, vis
# 1:1 NN PS matching w/o replacement
matched_subjects <- data %>%
  mutate(group = as.factor(group)) %>%
  filter(if_all(c(age, sex, bmi, lactate, vis_score), complete.cases)) %>%
  matchit(formula = group ~ age + sex + bmi + lactate + vis_score,
          # use covariate balanced propensity score matching
          data = ., method = "nearest", distance = "cbps", ratio = 1)


v <- data.frame(old = c("age", "sex_male", "bmi", "lactate", 
                        "vis_score"),
                new = c("Age", "Sex (ref = Male)", "BMI", 
                        "Lactate", "VIS"))

# love plot to assess balance
matched_subjects %>% cobalt::love.plot(thresholds = c(m = .1), 
                                       var.order = "unadjusted", 
                                       var.names = v,
                                       position = "bottom",
                                       abs = TRUE,
                                       stars = "raw")

matched_data <- match.data(matched_subjects) %>%
  mutate(grp = group)

matched_data %>%
  ggplot(aes(x=age, y=lactate, color=group)) +
  geom_point() +
  hrbrthemes::theme_ipsum()

fit <- lm(death ~ rrt_duration * (age + sex + bmi + lactate + vis_score), 
          data = matched_data, weights = weights)

plot(fit)

marginaleffects::avg_comparisons(fit,
                variables = "rrt_duration",
                vcov = ~subclass,
                newdata = subset(matched_data, rrt_duration == 0),
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
