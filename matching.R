# libraries
library(MatchIt)
library(cobalt)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(group = as.factor(group)) %>%
  # TODO: this reduces the number of effective cases for comparison
  filter(if_all(c(age, sex, bmi, lactate, vis_score), complete.cases))

# propensity score matching
# age, sex, bmi, lactate, vis
# 1:1 NN PS matching w/o replacement
matched_subjects <- data %>%
  matchit(formula = group ~ age + sex + bmi + lactate + vis_score,
          # use covariate balanced propensity score matching
          data = ., method = "nearest", distance = "glm", ratio = 1)


v <- data.frame(old = c("age", "sex_male", "bmi", "lactate", 
                        "vis_score"),
                new = c("Age", "Sex (ref = Male)", "BMI", 
                        "Lactate", "VIS"))

# love plot to assess balance
(matched_subjects %>% love.plot(thresholds = c(m = .1), 
                                       var.order = "unadjusted", 
                                       var.names = v,
                                       position = "bottom",
                                       abs = TRUE,
                                       stars = "raw"))

# generate table with balance statistics
(data %>% bal.tab(group ~ age + sex + bmi + lactate + vis_score, data = .,
                  thresholds = c(m = .1, v = 2)))
(matched_subjects %>% bal.tab(thresholds = c(m = .1, v = 2)))

# save plot
ggsave("figs/love_plot.svg", 
       width = 12.1, units = c('cm'))

# create matched data and save
matched_data <- match.data(matched_subjects) %>%
  mutate(grp = group)

write_csv(matched_data, "data/cleaned_matched_data.csv",
          na = "NA", append = FALSE)
