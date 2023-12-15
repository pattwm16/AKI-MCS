# analysis

# libraries
library(MatchIt)
library(broom)
library(janitor)
library(gtsummary)
library(tidyverse)

# data load
data <- read_csv("data/cleaned_analysis_data.csv")
matched_data <- read_csv("data/cleaned_matched_data.csv")

# secondary hypothesis 1 ---
# survival to hospital discharge in patients treated with tMCS or CS suffering
# from AKI correlates inversely with the severity of AKI-stadium


