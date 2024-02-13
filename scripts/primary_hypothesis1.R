# libraries
library(MatchIt)
library(broom)
library(splines)
library(janitor)
library(gtsummary)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")

label(data$rrt_group)      <- "Renal replacement therapy"
label(data$group)          <- "tMCS group"

# count need for RRT by survival to discharge
data %>%
  group_by(rrt_group) %>%
  tabyl(rrt_group, hosp_surv_yn) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>%
  adorn_title()

# primary hypothesis ---
# patients in CS w/ tMCS have lower survival to hospital discharge rate
# when additionally needing RRT

# regression analysis
# outcome: hosp_surv_yn (binary, died/survived)
# confounders: age, sex, bmi, vis_score, lactate
# treatment: rrt_group (factor, none/before/after/before&after)

reg_data <- data %>%
  select(age, sex, bmi, vis_score, pre_cr, rrt_group, hosp_surv_yn) %>%
  filter(complete.cases(.))

model.unadj <- glm(hosp_surv_yn ~ rrt_group, family = "binomial",
                   data = reg_data)

model.full  <- glm(hosp_surv_yn ~ age + sex + bmi + vis_score + pre_cr + rrt_group + group,
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
  gt::gtsave(filename = "tbls/regs/primary_hypothesis_logit.docx")

# check linearity
probabilities <- predict(model.full, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Survived to discharge", "Died prior to discharge")

linearity_assumption <- as_tibble(cbind(reg_data, probabilities, predicted.classes)) %>%
  mutate(logit = log(probabilities / (1 - probabilities))) %>%
  select_if(is.numeric) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
predictors <- colnames(linearity_assumption)

linearity_assumption %>%
  ggplot(aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~ predictors, scales = "free_y")

ggsave("figs/linearity_assumption.png", bg = 'white')

# check for influential values
plot(model.full, which = 4, id.n = 3)

# marginal effects
plot_predictions(model.full, condition = c('group', 'rrt_group')) +
  labs(title = "Predicted probability of survival to hospital discharge",
       color  = "Renal replacement therapy",
       x     = "tMCS group",
       y = "OR") +
  theme_bw()

ggsave("figs/predicted_prob_survival.png", bg = 'white')