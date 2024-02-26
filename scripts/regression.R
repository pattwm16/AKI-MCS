# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(Hmisc)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv") %>%
  mutate(rrt_group = fct_relevel(rrt_group, 
                                 "RRT before and during tMCS", after = Inf),
         aki_max = fct_relevel(aki_max, 
                               "No AKI", 
                               "AKI stage 1", 
                               "AKI stage 2", 
                               "AKI stage 3"),
         sex = fct_recode(sex, "Male" = "male", "Female" = "female"))

## regression analysis
# outcome: hosp_surv_yn (binary, died/survived)
# confounders: age, sex, bmi, vis_score, lactate
# treatment: rrt_group (factor, none/before/after/before&after)

reg_data <- data %>%
  select(age, sex, bmi, pre_lactate, log_vis_score, rrt_group, group, aki_max, aki_yn, hosp_surv_yn) %>%
  filter(complete.cases(.)) %>%
  mutate(sex = factor(sex), 
         grp = factor(group),
         .keep = "unused")

# label data
label(reg_data$rrt_group)    <- "Renal replacement therapy"
label(reg_data$grp)        <- "tMCS group"
label(reg_data$age)          <- "Age"
label(reg_data$sex)          <- "Sex"
label(reg_data$bmi)          <- "BMI"
label(reg_data$log_vis_score) <- "log(Vasoactive-inotropic score)"
label(reg_data$pre_lactate)   <- "Lactate prior to tMCS (mmol/L)"
label(reg_data$aki_max)       <- "Max KDIGO AKI stage"

# fit a model for all hypotheses
model.full  <- glm(hosp_surv_yn ~ age + sex + bmi + pre_lactate + log_vis_score + rrt_group + aki_max + grp,
                   family = "binomial",
                   data = reg_data)

# test assumptions
# 1. linearity for continuous predictors in logit
# Select only numeric predictors
probs <- predict(model.full, type = "response")
cbind(reg_data, probs) %>%
  select_if(is.numeric) %>%
  mutate(logit = log(probs/(1-probs))) %>%
  gather(key = "predictors", value = "predictor.value", -logit) %>%
  filter(predictors != "probs") %>%
  ggplot(., aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title = "Linearity assumption for continuous predictors",
       x = "Logit(odds)",
       y = "Predictor value")

# plot and save marginal effects
png("figs/marginal_effects_plot.png", width = 40, height = 30, units = "cm", res = 300)
plot(effects::predictorEffects(model.full))
dev.off()

# save the model as a table
model.full %>%  
  tbl_regression(., exponentiate = TRUE) %>%
  add_n() %>%
  italicize_levels() %>%
  #add_global_p() %>%
  #add_glance_source_note() %>%
  modify_caption("**Survival to hospital discharge in patients treated with tMCS**") %>%
  as_gt() %>%
  gt::gtsave(filename = "regs/regression_table.docx")

# plot model as a forest plot
#model.full %>%
results <- exp(cbind("or" = coef(model.full), 
          confint.default(model.full, level = 0.95))) %>%
  clean_names() %>%
  as.data.frame() %>%
  filter(rownames(.) != "intercept")

# aki_max
(aki_max_plot <- results %>%
  filter(row_number() %in% c(11, 10, 9, 8)) %>%
  arrange(desc(or)) %>%
  ggplot(., aes(x = or, y = rownames(.))) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = x97_5_percent, xmin = x2_5_percent), 
                 size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_linedraw(base_size = 15) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_discrete(labels = c("S1", "S2", "S3", "RRT")) +
  #scale_x_continuous(breaks = seq(0,5,1) ) +
  coord_trans(x = "log10") +
  labs(caption = "Reference level: No AKI",
       x = "Odds ratio",
       y = "AKI stage"))
ggsave("figs/aki_max_plot.png", aki_max_plot, width = 10, height = 5, units = "in", dpi = 300)

# rrt_group
(rrt_group_plot <- results %>%
  filter(row_number() %in% c(6, 7)) %>%
  ggplot(., aes(x = or, y = rownames(.))) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = x97_5_percent, xmin = x2_5_percent), 
                 size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_linedraw(base_size = 15) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_discrete(labels = c("Before and during tMCS", "During tMCS only")) +
  coord_trans(x = "log10") +
  labs(caption = "Reference level: No RRT",
       x = "Odds ratio",
       y = "RRT Group"))
ggsave("figs/rrt_group_plot.png", rrt_group_plot, width = 10, height = 5, units = "in", dpi = 300)

ggarrange(aki_max_plot, rrt_group_plot, ncol = 1, nrow = 2,
          labels = c("A", "B"), common.legend = T)

# survival to hospital discharge in ECMELLA patients with AKI is superior to
# va-ECLS patients with AKI (dichotomous)
plot_predictions(model.full, by = c('aki_max', 'grp')) + 
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  labs(subtitle = "Is survival to discharge in ECMELLA patients\nwith AKI is superior to va-ECLS patients with AKI?", 
       x = "",
       y = "Predicted probability of survival",
       color = "tMCS group")
ggsave("figs/aki_max_grp_plot.png", width = 10, height = 5, units = "in", dpi = 300)

