# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(effects)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_weighted_data.csv") %>%
  mutate(aki_max = case_when(
    aki_max == "no aki" ~ "No aki",
    aki_max == "s1" ~ "S1",
    aki_max == "s2" ~ "S2",
    aki_max == "s3" ~ "S3",
    aki_max == "rrt" ~ "RRT",
    TRUE ~ aki_max
  ))

# label matched_data
labelled::var_label(data) <- list(
  group = 'tCMS group',
  aki_max = 'Maximal AKI stadium'
)

# secondary hypothesis 1 ---
# survival to hospital discharge in patients treated with tMCS or CS suffering
# from AKI correlates inversely with the severity of AKI-stadium

# what is the distribution of the aki_max variable?
data %>%
  group_by(aki_max) %>%
  tabyl(aki_max)
# fit logistic regression model
# no aki should be reference level
reg_data <- data %>%
  filter(complete.cases(age, sex, bmi, vis_score, pre_cr, rrt_group, aki_max, hosp_surv_yn))

model.full <- data %>%
  glm(hosp_surv_yn ~ age + sex + bmi + vis_score + pre_cr + rrt_group + aki_max,
      data = ., family = "quasibinomial")

png("figs/marginal_effects_plot.png", width = 40, height = 30, units = "cm", res = 300)
plot(effects::predictorEffects(model.full))
dev.off()

model.full %>%
  tbl_regression(., exponentiate = TRUE) %>%
  add_n() %>%
  italicize_levels() %>%
  add_global_p() %>%
  add_glance_source_note() %>%
  modify_caption("**Secondary hypothesis 1: Survival to hospital discharge in patients treated with tMCS or CS suffering from AKI correlates inversely with the severity of AKI-stadium**") %>%
  as_gt() %>%
  gt::gtsave(filename = "regs/secondary_hypothesis_1.docx")

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

# check residuals for patterns
png("regs/diagnostics/sa1/residuals.png")
plot(resid(model.full), type = "p")
dev.off()

# check for collinearity
vif(model.full)

# check for influential values
png("regs/diagnostics/sa1/outliers.png")
plot(model.full, which = 4, id.n = 3)
dev.off()

# marginal effects
plot_predictions(model.full, type = "invlink(link)", condition = c('aki_max')) +
  labs(title = "Predicted probability of survival to hospital discharge",
       color  = "AKI stadium",
       x     = "Creatinine",
       y = "Odds ratio for survival to discharge") +
  theme_bw()

ggsave("figs/predicted_prob_survival.png", bg = 'white')