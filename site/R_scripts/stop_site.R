################################
# Script to stop the site and 
# deactivate a daemon for auto-rendering
# rmd files if their data changes.
################################

# source the daemon_utils
source("R_scripts/daemon_utils.R")

# stop the rmd render daemon
stopRMDDaemon()

# stop the blogdown daemon
startBlogdownDaemon()
