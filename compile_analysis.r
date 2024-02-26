# analysis wrapper script

# set working directory to scripts folder
#setwd(paste0(getwd(), ""))

# first, load and clean the data
source("scripts/clean.R")

# create tables 1 and 2
source("tbls/table1.R")
source("tbls/table2.R")

# next, optionally run weighting or matching
#source("scripts/weighting.R")
#source("scripts/matching.R")

# finally, run the analysis
source("scripts/regression.R")
