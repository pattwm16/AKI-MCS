# analysis

# libraries
library(MatchIt)
library(tidyverse)

# data load
data <- read_csv("../data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv")

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
