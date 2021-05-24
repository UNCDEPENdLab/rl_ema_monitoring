####################################
# This is the script used to run the back-end 
# functions utilizing the following arguments:
#
#   root:
#     Path to the root directory of this ema project.
#     default: the current director
#     other: give a path to a different directory
#
#   dataPath:
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
#     Note: Will set redcap to TRUE if it is TRUE.
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
#   nthreads:
#     Number of nthreads to spawn when processing physio data.
#     default: 4
#
#   output: 
#     Path to output the schedule and physio data files to.
#     default: Path given by the dataPath argument.
#
#   render:
#     Whether or not to render the landing page or subject pages.
#     default: TRUE -> will render entire site
#     other: 
#       FALSE -> will not render site
#       A list of subjects pages to render.
#     Note: "main" can be used to signify rendering of the home page.
#
#   site:
#     Path to where the directory hosting the hugo site is located.
#     default:
#
#   force_proc:
#     Whether or not to force reprocessing.
#     default: False -> do not reprocess
#     other: TRUE -> reprocess
#     note: Should run a force if data structure changes
#
####################################

# import dependent packages
library("pacman")
pacman::p_load(reticulate, RSQLite, dplyr, tidyr, lubridate, rjson, R.utils, REDCapR)

setwd("dashboard/study_management")

# source relevant R files and their functions
## Currently set to load dependent function from the same directory ##
source("data_management_functions.R")
source("dashboard_aggregate.R")
source("../../ECG_Dashboard2.R")
source("../../EEG_Dashboard.R")

# main function to be run
run_ema <- function(root=NULL, dataPath=getwd(), subjects="all", pull=TRUE, sched=TRUE, physio=TRUE, redcap=TRUE, nthreads=4, output=NULL, render=TRUE, site=NULL, force_proc=FALSE, force_reload=FALSE) {
  # SET ROOT
  ## Need to refactor the repo first ##
  #if(is.null(root) != TRUE) {
  #  setwd(root)
  #}
  
  # handle that physio depends on schedule
  if(physio == TRUE) {
    sched = TRUE
  }
  # handle that sched depends on redcap
  if(sched == TRUE) {
    redcap = TRUE
  }
  # Currently overrides the data directory to known test machine path
  dataPath = "/Users/shanebuckley/desktop/rl_ema_monitoring/data"
  # Currently overrides root to be rl_ema_monitoring
  root = "rl_ema_monitoring"
  # Get the list of active subjects (statically set for now)
  active <- getActiveList(root_dir = root)
  # get the list of subjects to run
  if(subjects == "all") {
    subjects <- active
  }
  ## TODO: Implement to get output subjects as subjects within the input list and flagged as active
  #else {
  #  
  #}
  
  # PULL DATA FROM GDRIVES
  ## Need to implement multithreaded subject pull to accomodate updating json ##
  if(pull == TRUE) {
    print("Running data pull...")
    # pull data
    # iterate through the active subjects
    for(subj in active) {
      pull_files(id=subj, path=dataPath)
    }
  }
  
  # find the root path
  root_path <- paste0(findRoot(root), '/', root)
  # update path information
  build_config(rootDir=root_path)
  
  # GET DATA SUMMARY
  path_info <- get_ema_subject_metadata(root_dir = root)
  #names(path_info) <- c("schedule","physio","video")
  
  # GET REDCAP DATA
  if(redcap == TRUE) {
    print("Running REDCap data pull...")
    # Get the credentials (standard path for now)
    
    creds <- get_redcap_creds(cred_path="../../data/redcap.json")
    # Load the data
    redcap_data <<- "Hello" #redcap_pull(uri=creds$uri, token=creds$token, active=active)
  }
  
  # GET SCHED DATA
  if(sched == TRUE) {
    #print(path_info$schedule)
    print("Running schedule calculation/aggregation...")
    # Run schedule
    output <<- proc_schedule(schedule_df = path_info$schedule,tz=Sys.timezone(),days_limit=60,force_reproc=force_proc)
    # Run REDCap merging into "output"
    output$redcap <<- redcap_data
    # Save the schedule output
    print("Saving the schedule results...")
    save(output, file=paste0(dataPath, '/output_schedule.Rdata'))
  }
  
  # GET PHYSIO DATA
  ## Note currently possible because schedule and physio are not fully decoupled ##
  if(physio == TRUE){
    print("Running physio calculation/aggregation...")
    # Run physio
    output_physio <<- proc_physio(physio_df = path_info$physio,sch_pro_output=output, tz="EST",thread=nthreads,force_reload=force_reload,force_reproc=force_proc,
                                  eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
                                  ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
    )
    # Save the physio output
    print("Saving the physio results...")
    save(output_physio, file=paste0(dataPath, '/output_physio.Rdata'))
  }
  
  
  # Make a new page on the site for these subjects
  for(new_subj in output$newdata_IDs) {
    tryCatch(
      {
        print(paste0("Making page for ", new_subj, "..."))
      }
    )
  }
  # RENDER THE SITE
  if(render == TRUE) {
    setwd(paste0(site, '/content'))
    # render the home page
    
    # render any subjects
    for(id in active) {
      curr_path <- paste0('subjects/', id)
      # if the md and the Rmd for the subject does not exist
      if(file.exists(paste0(curr_path, '/_index.Rmd')) == FALSE) {
        tryCatch({
          # then create a subject Rmd from the template
          blogdown::hugo_cmd(paste0("new subjects/", id, " -s .."))
        })
        tryCatch({
          # rename the .md file to a .Rmd (this allows Go to be used properly in archetypes)
          file.rename(paste0(curr_path, "/_index.md"), paste0(curr_path, "/_index.Rmd"))
        })
      }
      # attempt to render the subject's graph pages
      if(file.exists(paste0(curr_path, '/plot_ecg.Rmd')) == FALSE) {
        tryCatch({
          # rename the .md file to a .Rmd (this allows Go to be used properly in archetypes)
          file.rename(paste0(curr_path, '/plot_ecg.md'), paste0(curr_path, '/plot_ecg.Rmd'))
        })
      }
      # attempt to render the subject's graph pages
      if(file.exists(paste0(curr_path, '/plot_eeg.Rmd')) == FALSE) {
        tryCatch({
          # rename the .md file to a .Rmd (this allows Go to be used properly in archetypes)
          file.rename(paste0(curr_path, '/plot_eeg.md'), paste0(curr_path, '/plot_eeg.Rmd'))
        })
      }
    }
  }
}

#run_ema(pull=FALSE, sched=TRUE, physio=FALSE, redcap=FALSE, force_proc=TRUE, nthreads = 1, site="/Users/shanebuckley/desktop/rl_ema_monitoring/site", render=FALSE) # , force_reload=TRUE
run_ema(force_proc=TRUE, force_reload=TRUE, nthreads = 4, site="/Users/shanebuckley/desktop/rl_ema_monitoring/site") # , force_reload=TRUE

