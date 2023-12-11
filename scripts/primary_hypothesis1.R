# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv")

# primary hypothesis ---
# patients in CS w/ tMCS have lower survival to hospital discharge rate
# when additionally needing RRT

# identify patients in CS w/ tMCS
# treatment is rrt
# outcome is hosp_surv_yn

# count need for RRT by survival to discharge
matched_data %>%
  filter(cs_etiology != "No shock") %>%
  group_by(rrt_yn) %>%
  tabyl(rrt_yn, hosp_surv_yn) %>%
  adorn_totals(c("col", "row")) %>%
  adorn_title()

# Fit the logistic regression model
model <- matched_data %>%
  filter(cs_etiology != "No shock") %>%
  glm(hosp_surv_yn ~ rrt_yn, family = binomial, weights = weights, data = .)

## save as .docx
model %>%
  tbl_regression(., exponentiate = TRUE) %>%
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/primary_hypothesis.docx")

# visualize the difference
# Calculate the proportions
proportions <- matched_data %>%
    group_by(cs_etiology, rrt_yn, hosp_surv_yn) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(prop = n / sum(n)) %>%
    mutate(hosp_surv_yn = recode(as.character(hosp_surv_yn), "TRUE" = "Survived to discharge", "FALSE" = "Died prior to discharge"))

# Create the bar chart
p1 <- ggplot(proportions, aes(x = cs_etiology, y = prop, fill = rrt_yn)) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.6) +
    scale_fill_manual(values = c("TRUE" = "#DAA520", "FALSE" = "lightgrey"), labels = c("TRUE" = "Required", "FALSE" = "Not required")) +
    facet_grid(hosp_surv_yn ~ .) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "CS Etiology", y = "", fill = "Need for RRT") +
    hrbrthemes::theme_ipsum()

ggsave("figs/need_rrt_by_hosp_surv.png", plot = p1, bg = 'white')
