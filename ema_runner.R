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
#   cleanup:
#     Whether or not to ru  the cleanup of the data and graphs.
#     default: TRUE -> will run the data cleanup
#     other: 
#       FALSE -> will not cleanup the data
#     Note: will run if render is set to TRUE
#
#   render:
#     Whether or not to render the landing page or subject pages.
#     default: TRUE -> will render entire site
#     other: 
#       FALSE -> will not render site
#       A list of subjects pages to render.
#     Note: "main" can be used to signify rendering of the home page.
#
#   push:
#     Whether or not to push the site
#     default: FALSE -> will not push site
#     other:
#       TRUE -> will push site
#
#   force_proc:
#     Whether or not to force reprocessing.
#     default: False -> do not reprocess
#     other: TRUE -> reprocess
#     note: Should run a force if data structure changes
#
#   save_lite:
#     Whether or not to include raw data in physio processing output
#     default: FALSE -> includes raw data
#     other: TRUE -> excludes raw data
#
####################################

# import dependent packages
library("pacman")
pacman::p_load(reticulate, RSQLite, dplyr, tidyr, lubridate, rjson, R.utils, REDCapR, zoo)

setwd("dashboard/study_management")

# source relevant R files and their functions
## Currently set to load dependent function from the same directory ##
source("data_management_functions.R")
source("dashboard_aggregate.R")
source("render_utils.R")
source("../../ECG_Dashboard2.R")
source("../../EEG_Dashboard.R")
source("../../rmarkdown_site/report_functions/report_functions.R")


# main function to be run
run_ema <- function(root=NULL, subjects="all", pull=TRUE, sched=TRUE, physio=TRUE, redcap=TRUE, nthreads=4, output=NULL, render=TRUE, force_proc=FALSE, force_reload=TRUE, save_lite=FALSE, cleanup_data=TRUE, push=FALSE) {
  # SET ROOT
  ## Need to refactor the repo first ##
  #if(is.null(root) != TRUE) {
  #  setwd(root)
  #}
  # SET THE REPO DIR AT THE CURRENT DIRECTORY
  # handle that physio depends on schedule
  if(physio == TRUE) {
    sched = TRUE
  }
  # handle that sched depends on redcap
  #if(sched == TRUE) {
  #  redcap = TRUE
  #}
  # Currently overrides the data directory to known test machine path
  #dataPath <- "/Users/shanebuckley/desktop/rl_ema_monitoring/data"
  dataPath <<- get_cfg_var_p(var="data")
  #print(dataPath)
  videoPath <<- get_cfg_var_p(var="videos")
  videoURL <<- get_cfg_var_p(var="video_url")
  sitePath <<- get_cfg_var_p(var="site_path")
  sitePush <<- get_cfg_var_p(var="site_push")
  # Currently overrides root to be rl_ema_monitoring
  #root <- "rl_ema_monitoring"
  root <- basename(get_cfg_var_p(var="root"))
  repoRoot <<- get_cfg_var_p(var="root")
  # Get the list of active subjects (statically set for now)
  active <<- getActiveList(root_dir = root) 
  print(active)
  # get the list of subjects to run
  if(subjects != "all") {
    #@subjects <- active
    active <- subjects
  }
  ## TODO: Implement to get output subjects as subjects within the input list and flagged as active
  #else {
  #  
  #}
  
  # find the root path
  root_path <<- paste0(findRoot(root), '/', root)
  # update path information -> need to run a rebuild for every json config at the root (currently: cfg, data, videos)
  build_config(rootDir=root_path) # rebuilds the project's cfg.json
  build_config(file_name="data") # rebuilds the data.json
  build_config(file_name="videos") # rebuilds the videos.json
  
  # PULL DATA FROM GDRIVES
  ## Need to implement multithreaded subject pull to accomodate updating json ##
  if(pull == TRUE) {
    print("Running data pull...")
    # pull data
    # iterate through the active subjects
    for(subj in active) {
      pull_files(id=subj, path=dataPath, clinical_path=videoPath, clinical_url=videoURL, pull_all=FALSE)
    }
  }
  
  # update path information -> need to run a rebuild for every json config at the root (currently: cfg, data, videos)
  build_config(rootDir=root_path) # rebuilds the project's cfg.json
  build_config(file_name="data") # rebuilds the data.json
  build_config(file_name="videos") # rebuilds the videos.json
  # just call rebuild_project.py through a system call
  #system(paste0("python ", get_cfg_var_p(var="root"), '/rebuild_project.py'))
  
  # GET DATA SUMMARY
  path_info <- get_ema_subject_metadata(root_dir = root)
  #names(path_info) <- c("schedule","physio","video")
  
  # GET REDCAP DATA
  if(redcap == TRUE) {
    print("Running REDCap data pull...")
    # Get the credentials (standard path for now)
    
    creds <<- get_redcap_creds(cred_path=paste0(dataPath, "/redcap.json"))
    # Load the data
    #redcap_data <<- redcap_pull(uri=creds$uri, token=creds$token, active=active)
  }
  
  run_sched <- function() {
    #print(path_info$schedule)
    print("Running schedule calculation/aggregation...")
    # Run schedule
    output <- proc_schedule(schedule_df = path_info$schedule,tz=Sys.timezone(),days_limit=60,force_reproc=force_proc)
    # Run REDCap merging into "output"
    #if(redcap == TRUE) {
    #  output$redcap <- redcap_data
    #}
    #output <<- output_sched
    # Save the schedule output
    print("Saving the schedule results...")
    save(output, file=paste0(dataPath, '/output_schedule.Rdata'))
    return(output)
  }
  
  run_physio <- function(output=NULL) {
    print("Running physio calculation/aggregation...")
    # Run physio
    output_physio <- proc_physio(physio_df = path_info$physio,sch_pro_output=output, tz="EST",thread=nthreads,force_reload=force_reload,force_reproc=force_proc, save_lite=save_lite,
                                  eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
                                  ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
    )
    # Save the physio output
    print("Saving the physio results...")
    save(output_physio, file=paste0(dataPath, '/output_physio.Rdata'))
  }
  
  # GET SCHED DATA
  if(sched == TRUE) {
    output <- run_sched()
    if(physio == TRUE){
      # GET PHYSIO DATA
      output <<- output
      run_physio(output=output)
    }
  }
  
  # update path information -> need to run a rebuild for every json config at the root (currently: cfg, data, videos)
  build_config(rootDir=root_path) # rebuilds the project's cfg.json
  build_config(file_name="data") # rebuilds the data.json
  build_config(file_name="videos") # rebuilds the videos.json
  
  reports_path <<- ""
  plots_path <<- ""
  
  # load the schedule and physio if they are not loaded by previous steps
  if(!exists("output_physio")){
    load(paste0(dataPath, "/output_physio.Rdata"))
    output_physio <<- output_physio
  } else if(is.null(output_physio)) {
    load(paste0(dataPath, "/output_physio.Rdata"))
    output_physio <<- output_physio
  }
  
  #print("here")
  #print(output)
  if(!exists("output")){
    load(paste0(dataPath, "/output_schedule.Rdata"))
    output <<- output
  } else if(is.null(output)) {
    load(paste0(dataPath, "/output_schedule.Rdata"))
    output <<- output
  }
  
  if(cleanup_data == TRUE){
    # to run the cleanup function, loop through the subject IDs, resourcing cleanup, re-running it with the current sid
    for(s in active) {
      subj <<- s
      if(redcap == TRUE) {
        checklist <<- get_redcap_checklist_r(rc_url=creds$uri, rc_token=creds$token, subj_id=s)
      }
      reports_path <<- paste0(dataPath, '/Subjects/', s, '/reports')
      plots_path <<- paste0(dataPath, '/Subjects/', s, '/plots')
      # create a reports directory for the subject if one does not exist
      dir.create(reports_path, showWarnings = FALSE, recursive=TRUE)
      # create a plots directory for the subject if one does not exist
      dir.create(plots_path, showWarnings = FALSE, recursive=TRUE)
      # try to get the rds file and append it to the list of rds files
      source("dashboard_cleanup.R")
    }
    build_config(rootDir=root_path) # rebuilds the project's cfg.json
    build_config(file_name="data") # rebuilds the data.json
    # create an empty list for the rds data paths
    rds_list <- list()
    # generate an overall overview rds by aggregating over the subject's overall.rds files.
    for(s in active) {
      try({
        # get the path of the file
        subject_overall_path <- paste0(getPathFromCfg(root_dir=root, cfg_name='data.json', sourced_file="overall.rds", keywords=s), '/overall.rds')
        rds_list <- append(rds_list, subject_overall_path)
      })
    }
    try({
      rds_combo <- rds_list %>%
        map_dfr(readRDS)
      saveRDS(rds_combo, paste0(dataPath, "/Subjects/overall_overview.rds"))
    })
  }
  # if rendering the site
  if(render==TRUE) {
    # if the site_dir does not exist, create it
    if(dir.exists(sitePath)==FALSE){
      dir.create(sitePath)
    }
    # run the render
    source("../../rmarkdown_site/render_site.R")
  }
  
  # if pushing the site
  if(push==TRUE) {
    # Michael's push function (rsync wrapper)
    push_site <- function(output_dir, push_dir) {
      checkmate::assert_directory_exists(output_dir)
      #have exe permission on files will yield 403 errors (some images are getting +x when sent to me)
      system(paste("find", output_dir, "-type f -print0 | xargs -0 chmod 644"))
      system(paste0("rsync -av ", output_dir, "/ ", push_dir))
    }
    # run the site push
    push_site(sitePath, sitePush)
  }
}
  

#run_ema(pull=FALSE, sched=TRUE, physio=FALSE, redcap=FALSE, force_proc=TRUE, nthreads = 1, site="/Users/shanebuckley/desktop/rl_ema_monitoring/site", render=FALSE) # , force_reload=TRUE
#run_ema(redcap=TRUE, save_lite=TRUE, render=FALSE, pull=TRUE, sched=TRUE, physio=TRUE, force_proc=TRUE, force_reload=TRUE, nthreads = 4, site="/Users/shanebuckley/desktop/rl_ema_monitoring/site") # , force_reload=TRUE
# pull only
#run_ema(redcap=FALSE, save_lite=FALSE, render=FALSE, pull=TRUE, sched=FALSE, physio=FALSE, cleanup_data=FALSE, nthreads = 4) #, site="/Users/shanebuckley/desktop/rl_ema_monitoring/site"
# everything but the render and redcap pull and site push
#run_ema(redcap=FALSE, save_lite=TRUE, render=FALSE, pull=TRUE, sched=TRUE, physio=TRUE, force_proc=TRUE, force_reload=TRUE, cleanup_data=TRUE, nthreads = 4) #, site="/Users/shanebuckley/desktop/rl_ema_monitoring/site")
# everything but the redcap pull and site push
#run_ema(redcap=FALSE, save_lite=TRUE, render=TRUE, pull=TRUE, sched=TRUE, physio=TRUE, force_proc=FALSE, force_reload=TRUE, cleanup_data=TRUE, nthreads = 4)
# only run cleanup layer
#run_ema(redcap=FALSE, save_lite=FALSE, render=FALSE, pull=FALSE, sched=FALSE, physio=FALSE, force_proc=FALSE, force_reload=FALSE, cleanup_data=TRUE, nthreads = 4)
# render only
run_ema(redcap=FALSE, save_lite=FALSE, render=TRUE, pull=FALSE, sched=FALSE, physio=FALSE, cleanup_data=FALSE, nthreads = 4, push=FALSE)
#subjects=list("221604", "221849"),
