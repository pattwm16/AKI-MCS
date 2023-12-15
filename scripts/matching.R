# libraries
library(MatchIt)
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

matched_subjects <- data %>%
  matchit(formula = group ~ age + sex + ns(bmi) + ns(lactate) * vis_score,
          data = ., method = "nearest",
          distance = "glm", link = "logit",
          ratio = 1)

# Evaluate matching graphically ----
# examine propensity scores, jitter plot, stratified by treatment
png("figs/propensity_scores_by_trt.png",
    width = 25, height = 25, units = "cm", res = 300)
plot(matched_subjects, type = "jitter", interactive = FALSE)
dev.off()

# evaluate match of densities after matching
png("figs/propensity_scores_densities.png",
    width = 25, height = 25, units = "cm", res = 300)
plot(matched_subjects, type = "density", interactive = FALSE,
     which.xs = ~ `age` + `ns(bmi)` + `ns(lactate)`)
dev.off()

# examine qq plot for matched_sets
png("figs/propensity_scores_qq.png",
    width = 25, height = 25, units = "cm", res = 300)
plot(matched_subjects, type = "qq", interactive = FALSE,
     which.xs = c("age", "ns(bmi)", "ns(lactate)"))
dev.off()


v <- data.frame(old = c("age", "sex_male", "ns(bmi)_1", "ns(lactate)_1",
                        "vis_score"),
                new = c("Age", "Sex (ref = Male)", "BMI",
                        "Lactate", "VIS"))

# love plot to assess balance
(matched_subjects %>% love.plot(thresholds = c(m = .1),
                                var.order = "unadjusted",
                                var.names = v,
                                position = "bottom",
                                abs = TRUE,
                                line = TRUE,
                                stars = "raw"))

# generate table with balance statistics
(data %>% bal.tab(group ~ age + sex + bmi + ns(lactate) + vis_score, data = .,
                  thresholds = c(m = .1, v = 2),
                  s.d.denom = "pooled"))
(matched_subjects %>% bal.tab(thresholds = c(m = .1, v = 2)))

# save plot
ggsave("figs/love_plot.png",
       width = 12.1, units = c("cm"))

# create matched data and save
matched_data <- match.data(matched_subjects) %>%
  mutate(grp = group)

write_csv(matched_data, "data/cleaned_matched_data.csv",
          na = "NA", append = FALSE)
