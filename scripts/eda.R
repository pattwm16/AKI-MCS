# exploratory data analysis

library(tidyverse)

data <- read_csv("data/cleaned_analysis_data.csv") %>%
  janitor::clean_names()

# summary statistics

plot(data$age, data$bmi)
