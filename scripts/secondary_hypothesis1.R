# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(marginaleffects)
library(Hmisc)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(aki_max = fct_relevel(aki_max, "No AKI"),
         rrt_group = fct_relevel(rrt_group, "RRT before and during tMCS", after = Inf))

# label matched_data
label(data$rrt_group)     <- "Renal replacement therapy"
label(data$group)         <- "tMCS group"
label(data$age)           <- "Age"
label(data$sex)           <- "Sex"
label(data$bmi)           <- "BMI"
label(data$pre_cr)        <- "Creatinine (prior to tMCS)"
label(data$log_vis_score) <- "log(Vasoactive-inotropic score)"
label(data$aki_max)       <- "Max KDIGO AKI Stage"

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
  select(age, sex, bmi, log_vis_score, rrt_group, aki_max, hosp_surv_yn) %>%
  filter(complete.cases(.))

model.full <- data %>%
  glm(hosp_surv_yn ~ age + sex + bmi + log_vis_score + rrt_group + aki_max,
      data = ., family = "quasibinomial")

# png("figs/marginal_effects_plot.png", width = 40, height = 30, units = "cm", res = 300)
# plot(effects::predictorEffects(model.full))
# dev.off()

model.full %>%  
  tbl_regression(., exponentiate = TRUE) %>%
  add_n() %>%
  italicize_levels() %>%
  #add_global_p() %>%
  add_glance_source_note() %>%
  modify_caption("**Secondary hypothesis 1: Survival to hospital discharge in patients treated with tMCS or CS suffering from AKI correlates inversely with the severity of AKI-stadium**") %>%
  as_gt() %>%
  gt::gtsave(filename = "regs/secondary_hypothesis_1.docx")

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

ggsave("regs/diagnostics/sa1/linearity.png", bg = 'white')

# check residuals for patterns
png("regs/diagnostics/sa1/residuals.png")
plot(resid(model.full), type = "p")
dev.off()

# check for collinearity
car::vif(model.full) %>%
  as_tibble(rownames = "predictors") %>%
  select(predictors, df = Df, gvif = GVIF) %>%
  write_csv("regs/diagnostics/sa1/vif.csv")

# check for influential values
png("regs/diagnostics/sa1/outliers.png")
plot(model.full, which = 4, id.n = 3)
dev.off()

# marginal effects
marginaleffects::plot_predictions(model.full, type = "invlink(link)", condition = c('aki_max')) +
  labs(title = "Predicted probability of survival to hospital discharge",
       color  = "AKI stadium",
       x     = "Creatinine",
       y = "Odds ratio for survival to discharge") +
  theme_bw()

ggsave("figs/sa1_marginaleffects.png", bg = 'white')
