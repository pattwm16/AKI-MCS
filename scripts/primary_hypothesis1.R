# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv") %>%
  mutate(cs_yn = case_when(cs_etiology != "No shock" ~ "No", TRUE ~ "Yes"))
weighted_data <- read_csv("data/cleaned_weighted_data.csv")

# primary hypothesis ---
# patients in CS w/ tMCS have lower survival to hospital discharge rate
# when additionally needing RRT

# count need for RRT by survival to discharge
matched_data %>%
  group_by(rrt_yn) %>%
  tabyl(rrt_yn, hosp_surv_yn, cs_yn) %>%
  adorn_totals(c("col", "row")) %>%
  adorn_title()

# label matched_data
labelled::var_label(matched_data) <- list(
  group = 'tCMS group',
  cs_yn = 'Cardiogenic shock',
  rrt_yn = 'Need for RRT'
)

# Fit the logistic regression model
model <- glm(hosp_surv_yn ~ cs_yn * rrt_yn, family = binomial,
             data = matched_data)

## save as .docx
model %>%
  tbl_regression(., exponentiate = TRUE) %>%
  add_nevent() %>%
  as_gt() %>%
  gt::gtsave(filename = "tbls/regs/primary_hypothesis.docx")

# Calculate the proportions
proportions <- matched_data %>%
  group_by(cs_etiology, rrt_yn, hosp_surv_yn) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n)) %>%
  mutate(hosp_surv_yn = recode(as.character(hosp_surv_yn), "TRUE" = "Survived to discharge", "FALSE" = "Died prior to discharge")) %>%
  mutate(cs_etiology = forcats::fct_relevel(cs_etiology, "No shock")) %>%
  mutate(cs_etiology = forcats::fct_relevel(cs_etiology, "Other", , after = length(levels(cs_etiology)))) %>%
  mutate(hosp_surv_yn = forcats::fct_rev(hosp_surv_yn))

# Create the bar chart
(p1 <- ggplot(proportions, aes(x = cs_etiology, y = prop, fill = rrt_yn)) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.6) +
    geom_text(aes(label = n), position = position_fill(vjust = 0.5), size = 3) +
    scale_fill_manual(values = c("TRUE" = "#DAA520", "FALSE" = "lightgrey"), labels = c("TRUE" = "Required", "FALSE" = "Not required")) +
    facet_grid(hosp_surv_yn ~ .) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Etiology of cardiogenic shock",
         y = "",
         fill = "Need for RRT",
         caption = "RRT was defined as one or more days of either hemodialysis or continuous hemofiltration while undergoing tMCS"
        ) +
    hrbrthemes::theme_ipsum(grid = "Y"))

ggsave("figs/need_rrt_by_hosp_surv.png", plot = p1, bg = 'white')
