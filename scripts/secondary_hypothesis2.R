# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(Hmisc)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
#matched_data <- read_csv("data/cleaned_matched_data.csv")

# label matched_data
labelled::var_label(data) <- list(
  group = 'tCMS group',
  aki_yn = 'AKI on tCMS'
)

# secondary hypothesis 2 ---
# survival to hospital discharge in ECMELLA patients with AKI is superior to
# va-ECLS patients with AKI (dichotomous)

# ECMELLA pts vs. va-ECLS pts + aki_yn
data %>%
  group_by(group) %>%
  tabyl(aki_yn, hosp_surv_yn)

# fit logistic regression model
reg_data <- data %>%
  select(age, sex, bmi, vis_score, pre_cr, rrt_group, group, aki_max, hosp_surv_yn) %>%
  filter(complete.cases(.))

model.full <- data %>%
  glm(hosp_surv_yn ~ age + sex + bmi + log_vis_score + pre_cr + rrt_group + group * aki_yn,
      family = "binomial", data = .)

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

ggsave("regs/diagnostics/sa2/linearity.png", bg = 'white')

# check residuals for patterns
png("regs/diagnostics/sa2/residuals.png")
plot(resid(model.full), type = "p")
dev.off()

# check for collinearity
car::vif(model.full, type = 'predictor')

# check for influential values
png("regs/diagnostics/sa2/outliers.png")
plot(model.full, which = 4, id.n = 3)
dev.off()

plot(effects::predictorEffect(model.full, pred = "group", xlevels = list(group = c("ECMELLA", "va-ECLS"))))

model.full %>%
  tbl_regression(., exponentiate = TRUE) %>%
  add_n() %>%
  italicize_levels() %>%
  add_global_p() %>%
  add_glance_source_note() %>%
  modify_caption("**Secondary hypothesis 2: Survival to hospital discharge in ECMELLA patients with AKI is superior to va-ECLS patients with AKI (dichotomous)**") %>%
  as_gt() %>%
  gt::gtsave(filename = "regs/secondary_hypothesis_2.docx")

# marginal effects
plot_predictions(model.full, type = "invlink(link)", condition = c('aki_yn', 'group')) +
  labs(title = "Predicted probability of survival to hospital discharge",
       color  = "tMCS modality",
       x     = "RRT while on tMCS",
       y = "Odds ratio for survival to discharge") +
  theme_bw()

ggsave("figs/sa2_marginaleffects.png", bg = 'white')