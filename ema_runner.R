####################################
# This is the script used to run the back-end 
# functions utilizing the following arguments:
#
#   root:
#     Path to the root directory of this ema project.
#     default: the current director
#     other: give a path to a different directory
#
#   data:
#     Path from the root directory to the data.
#     default: root/data
#     other: give a path to a different directory from root
#
#   config: 
#     Path to a configuration file.
#     default: the current directory and config.json
#     other: give a path to a different config file
#     Note: config will override any other arguments given.
#
#   creds:
#     Path to a credentials file.
#     default: the current directory and cred.json
#     other: give a path to a different cred file
#     Note: Cred file should only be accessible with high privilege.
#
#   subjects:
#     The subjects to be run through the pipeline.
#     default: "all" -> will run the given process for 
#       for all subjects
#     other: should be given a list of subjects.
#
#   pull:
#     Whether or not to do a fresh pull of the subject data
#     from Google Drive.
#     default: TRUE -> pull the subject data
#     other: FALSE -> do not pull data
#
#   sched:
#     Whether or not to process the schedule data.
#     default: TRUE -> run the schedule preprocessing
#     other: FALSE -> do not run the schedule preprocesing
#
#   physio:
#     Whether or not to process the physio data.
#     Note: Will set sched to TRUE if it is TRUE.
#     default: TRUE -> run the physio preprocessing
#     other: FALSE -> do not run the physio preprocesing
#
#   redcap:
#     Whether or not to pull data from redcap.
#     default: TRUE -> pull data from redcap
#     other: FALSE -> do not pull data from redcap
#     
####################################

# import dependent packages
library("pacman")
pacman::p_load(reticulate, RSQLite, dplyr, tidyr, lubridate, rjson, R.utils)

# source relevant R files and their functions
## Currently set to load dependent function from the same directory ##
source("data_management_functions.R")
source("dashboard_aggregate.R")

# main function to be run
run_ema <- function(root=NULL, subjects="all", pull=TRUE, sched=TRUE, physio=TRUE, redcap=TRUE) {
  # SET ROOT
  ## Need to refactor the repo first ##
  #if(is.null(root) != TRUE) {
  #  setwd(root)
  #}
  
  # PULL DATA FROM GDRIVES
  ## Need to implement multithreaded subject pull to accomodate updating json first ##
  if(pull == TRUE) {
    # pull data
    print("Running data pull...")
    # update path information
    
  }
  
  # GET DATA SUMMARY
  path_info <- get_ema_subject_metadata(root_dir = root)
  #names(path_info) <- c("schedule","physio","video")
  
  # GET SCHED DATA
  if(sched == TRUE) {
    #print(path_info$schedule)
    print("Running schedule calculation/aggregation...")
    output <- proc_schedule(schedule_df = path_info$schedule,tz=Sys.timezone(),days_limit=60,force_reproc=F)
  }
  
  # GET PHYSIO DATA
  if(physio == TRUE){
    print("Running physio calculation/aggregation...")
    output_physio <- proc_physio(physio_df = path_info$physio,sch_pro_output=output, tz="EST",thread=4,force_reload=FALSE,force_reproc=FALSE,
                                 eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
                                 ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
    )
  }
  
  # GET REDCAP DATA
  if(redcap == TRUE) {
    print("Running REDCap data pull...")
  }
}

