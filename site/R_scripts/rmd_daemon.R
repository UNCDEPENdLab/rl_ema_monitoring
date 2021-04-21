# load the daemon utils
source("R_scripts/daemon_utils.R")
# install hugo 
#blogdown::install_hugo("0.82.0")

# main loop
while(TRUE) {
  # run the daemon's main function
  runRMDRerender()
  # sleep for two seconds
  Sys.sleep(2)
}