# load all dependencies
library('assertthat')
#library('av')
library('reticulate')
library('rjson')
library('tinsel')
library('RSQLite')
library('tidyverse')

# Source individiual scipts...

# save the current working directory
saved_dir <- getwd()
# try to change the working and sorce the functions
tryCatch({
  # get this script's location
  script.dir <- dirname(sys.frame(1)$ofile)
  # set location to scipt location
  setwd(script.dir)
  # source data management functions (and EEG and ECG functions)
  source("data_management_functions.R")
  # source the aggregation functions
  source("dashboard_aggregate.R")
# reset the working directory no matter the outcome
}, finally = {
  # reset working directory
  setwd(saved_dir)
})
