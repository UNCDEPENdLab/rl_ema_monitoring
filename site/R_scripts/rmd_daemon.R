# load the daemon utils
source("R_scripts/daemon_utils.R")

# main loop
while(TRUE) {
  tryCatch({
    # run the daemon's main function
    runRMDRerender()
    # sleep for two seconds
    Sys.sleep(2)
  })
}