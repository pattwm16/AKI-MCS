# libraries
library(MatchIt)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")

# propensity score matching
# age, sex, bmi, lactate, vis
# 1:1 NN PS matching w/o replacement
matched_subjects <- data %>%
  mutate(group = as.factor(group)) %>%
  filter(if_all(c(age, sex, bmi, lactate, vis_score), complete.cases)) %>%
  matchit(formula = group ~ age + sex + bmi + lactate + vis_score,
          # use covariate balanced propensity score matching
          data = ., method = "nearest", distance = "glm", ratio = 1)


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

# save plot
ggsave("figs/love_plot.svg", 
       width = 12.1, units = c('cm'))

# create matched data and save
matched_data <- match.data(matched_subjects) %>%
  mutate(grp = group)

write_csv(matched_data, "data/cleaned_matched_data.csv",
          na = "NA", append = FALSE)