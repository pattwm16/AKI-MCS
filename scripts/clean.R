# cleaning script

# load in libraries for cleaning
library(janitor)
library(tidyverse)
library(readxl)

# load helpers
source("scripts/helpers.R")

# get path to raw excel file
path <- "../data/"

path %>%
  excel_sheets() %>%
  set_names() %>% 
  map(read_then_csv, path = path)

dir(pattern = "^data.*\\.csv$")