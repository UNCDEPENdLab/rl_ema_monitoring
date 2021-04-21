################################
# Script to launch the site and 
# activate a daemon for auto-rendering
# rmd files if their data changes.
################################

# source the daemon_utils
source("R_scripts/daemon_utils.R")

#launch the blogdown daemon
startBlogdownDaemon()

# launch the rmd render daemon
startRMDDaemon()
