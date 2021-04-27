################################
# Script to stop the site and 
# deactivate a daemon for auto-rendering
# rmd files if their data changes.
################################
library("future")
# source the daemon_utils
source("R_scripts/daemon_utils.R")

# set the plan to not be sequential
plan(multisession)

# stop the rmd render daemon
async_stopRMDDaemon <- future({stopRMDDaemon()})

# stop the blogdown daemon
async_stopBlogdownDaemon <- future({stopBlogdownDaemon()})
