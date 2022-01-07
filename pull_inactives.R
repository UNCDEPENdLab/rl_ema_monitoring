# This is a quick script to pull data for inactive participants

# import dependent packages
library("pacman")
library("anytime")
pacman::p_load(reticulate, RSQLite, dplyr, tidyr, lubridate, rjson, R.utils, REDCapR, zoo, anytime)

setwd("dashboard/study_management")

# source relevant R files and their functions
## Currently set to load dependent function from the same directory ##
source("data_management_functions.R")
source("dashboard_aggregate.R")

dataPath <<- get_cfg_var_p(var="data")
videoPath <<- get_cfg_var_p(var="videos")
videoURL <<- get_cfg_var_p(var="video_url")
videoRclone <<- get_cfg_var_p(var="video_rclone")
sitePath <<- get_cfg_var_p(var="site_path")
sitePush <<- get_cfg_var_p(var="site_push")

# mount the sharepoint with rclone if it is not mounted
mount_str <<- system(paste0("df | awk '{print $9}' | grep -Ex '", videoPath, "'"), intern=TRUE)
if(length(mount_str) == 0) {
  mount_str <<- ""
}
# if the mount was not found
if(mount_str != videoPath) {
  system(paste0("rclone cmount ", videoRclone, " ", videoPath, " --daemon --vfs-cache-mode full"))
  print("Mounting the remote data storage:")
  print(paste0("rclone cmount ", videoRclone, " ", videoPath, " --daemon --vfs-cache-mode full"))
  #exit()
} else {
  print("Remote data storage was already mounted.")
  #exit()
}

# ensure that the mount point is mounted (as to not accidentally save locally)
post_mount_str <<- system(paste0("df | awk '{print $9}' | grep -Ex '", videoPath, "'"), intern=TRUE)
if(post_mount_str != videoPath) {
  print(paste0("Looking for: ", videoPath))
  print(paste0("Found: ", post_mount_str))
  stop("The mount point was not established successfully...terminating.")
}

# Currently overrides root to be rl_ema_monitoring
#root <- "rl_ema_monitoring"
root <- basename(get_cfg_var_p(var="root"))
repoRoot <<- get_cfg_var_p(var="root")

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

# Get the list of inactive subjects (statically set for now)
inactive <<- getActiveList(root_dir = root, active=FALSE)
print(inactive)
# get the list of subjects to run
#if(subjects != "all") {
#  #@subjects <- active
#  inactive <- subjects
#}

# PULL DATA FROM GDRIVES
## Need to implement multithreaded subject pull to accomodate updating json ##

print("Running data pull...")
# pull data
# iterate through the active subjects
for(subj in inactive) {
  try({
    pull_files(id=subj, path=dataPath, clinical_path=videoPath, clinical_url=videoURL, pull_all=FALSE)
  })
}
