# libraries
library(MatchIt)
library(broom)
library(splines)
library(janitor)
library(gtsummary)
library(Hmisc)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(rrt_group = fct_relevel(rrt_group, "RRT before and during tMCS", after = Inf))
#data <- read_csv("data/cleaned_weighted_data.csv")

label(data$rrt_group)    <- "Renal replacement therapy"
label(data$group)        <- "tMCS group"
label(data$age)          <- "Age"
label(data$sex)          <- "Sex"
label(data$bmi)          <- "BMI"
label(data$pre_cr)       <- "Creatinine (prior to tMCS)"
label(data$log_vis_score) <- "log(Vasoactive-inotropic score)"

# primary hypothesis ---
# patients in CS w/ tMCS have lower survival to hospital discharge rate
# when additionally needing RRT

# regression analysis
# outcome: hosp_surv_yn (binary, died/survived)
# confounders: age, sex, bmi, vis_score, lactate
# treatment: rrt_group (factor, none/before/after/before&after)

reg_data <- data %>%
  select(age, sex, bmi, log_vis_score, pre_cr, rrt_group, hosp_surv_yn) %>%
  filter(complete.cases(.))

model.full  <- glm(hosp_surv_yn ~ age + sex + bmi + log_vis_score + pre_cr + rrt_group,
                   family = "binomial",
                   data = data)

## save as .docx
(model.full %>%
  tbl_regression(., exponentiate = TRUE) %>%
  add_n() %>%
  italicize_levels() %>%
  add_glance_source_note() %>%
  modify_caption("**Primary hypothesis: RRT requirement and survival to hospital discharge**")) %>%
  as_gt() %>%
  gt::gtsave(filename = "regs/primary_hypothesis.docx")

# check linearity
probabilities <- predict(model.full, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Survived to discharge", "Died prior to discharge")

linearity_assumption <- as_tibble(cbind(reg_data, probabilities, predicted.classes)) %>%
  mutate(logit = log(probabilities / (1 - probabilities))) %>%
  select(-probabilities) %>%
  select_if(is.numeric) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
predictors <- colnames(linearity_assumption)

linearity_assumption %>%
  ggplot(aes(predictor.value, exp(logit))) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~ predictors, scales = "free_x")

ggsave("regs/diagnostics/pa1/linearity.png", bg = 'white')

# check residuals for patterns
png("regs/diagnostics/pa1/residuals.png")
plot(resid(model.full))
dev.off()

# check for collinearity
car::vif(model.full) %>%
  as_tibble(rownames = "predictors") %>%
  select(predictors, df = Df, gvif = GVIF) %>%
  write_csv("regs/diagnostics/pa1/vif.csv")

# check for influential values
png("regs/diagnostics/pa1/outliers.png")
plot(model.full, which = 4, id.n)
dev.off()

# marginal effects
plot_predictions(model.full,
    type = "invlink(link)",
    condition = c('rrt_group')) +
  labs(title = "Predicted probability of survival to hospital discharge",
       color  = "Renal replacement therapy",
       x     = "tMCS group",
       y = "Odds ratio for survival to discharge") +
  theme_classic()

ggsave("figs/pa1_marginaleffects.png", bg = 'white')
