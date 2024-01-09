# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(effects)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv") %>%
  mutate(aki_max = case_when(
    aki_max == "no aki" ~ "No aki",
    aki_max == "s1" ~ "S1",
    aki_max == "s2" ~ "S2",
    aki_max == "s3" ~ "S3",
    TRUE ~ aki_max
  ))

# label matched_data
labelled::var_label(matched_data) <- list(
  group = 'tCMS group',
  aki_max = 'Maximal AKI stadium'
)

# secondary hypothesis 1 ---
# survival to hospital discharge in patients treated with tMCS or CS suffering
# from AKI correlates inversely with the severity of AKI-stadium

# what is the distribution of the aki_max variable?
matched_data %>%
  group_by(aki_max) %>%
  tabyl(aki_max)
# fit logistic regression model
model <- matched_data %>%
  glm(hosp_surv_yn ~ group * aki_max,
      data = .)

png("figs/marginal_effects_plot.png", width = 40, height = 30, units = "cm", res = 300)
plot(effects::predictorEffects(model))
dev.off()

model %>%
  tbl_regression(., exponentiate = TRUE) %>%
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/secondary_hypothesis_1.docx")
