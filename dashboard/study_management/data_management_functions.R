get_schedule_info <- function(sid, data_dir=NULL) {
  checkmate::assert_string(sid)
  
  expect_dir <- file.path(data_dir, sid, "schedule")
  checkmate::assert_directory_exists(expect_dir)
  
  sched_info <- file.info(file.path(expect_dir, "subject_schedule_naming.db")) #not sure how date and ID get into name
  
  #and whatever other columns and info are derived from file system and json
  return(data.frame(subject_id=sid, last_cached=format(sched_info["mtime"], "%d%b%Y-%H%M%S")))
}

get_physio_info <- function(sid, data_dir=NULL) {
  checkmate::assert_string(sid)
  
  expect_dir <- file.path(data_dir, sid, "physio")
  checkmate::assert_directory_exists(expect_dir)
  
  #look up physio details from files and jsons here
  #return a data.frame with one row per physio file for this subject
  return(data.frame(subject_id=sid, last_cached=format(physio_info["mtime"], "%d%b%Y-%H%M%S")))
}

get_video_info <- function(sid, data_dir=NULL) {
  checkmate::assert_string(sid)
  
  expect_dir <- file.path(data_dir, sid, "video")
  checkmate::assert_directory_exists(expect_dir)
  
  #look up video details from files and jsons here
  #return a data.frame with one row per video file for this subject
  return(data.frame(subject_id=sid, last_cached=format(physio_info["mtime"], "%d%b%Y-%H%M%S")))
}

# Function to provide list of currently cached subject schedule files
#
# @param data_dir Root of Subjects data directory
# @importFrom checkmate assert_directory exists
# @importFrom dplyr bind_rows
# @return A three-element list containing cache info for 
get_ema_subject_metadata <- function(data_dir=NULL, trigger_refresh=FALSE) {
  checkmate::assert_directory_exists(data_dir)
  checkmate::assert_logical(trigger_refresh)

  if (isTRUE(trigger_refresh)) { refresh_ema_cache(data_dir) }
    
  folders <- list.dirs(path=data_dir, full.names=TRUE, recursive=FALSE)
  #something along the lines of ...
  sched_list <- list()
  physio_list <- list()
  video_list <- list()
  
  for (ff in folders) {
    sid <- basename(ff)
    #figure out schedule stuff
    sched_list[[sid]] <- get_schedule_info(sid, data_dir) #return a one-row data.frame summarizing status of schedule
    physio_list[[sid]] <- get_physio_info(sid, data_dir) #return multi-row data.frame, one row per subject physio file
    video_list[[sid]] <- get_video_info(sid, data_dir) #return multi-row data.frame, one row per subject video file
  }

  sched_df <- bind_rows(sched_list)
  # something like
  # subject_id  subject_folder                                                      last_cached   active   cache_failure
  # 9001        /projects/rl_ema_monitoring/Subjects/9001/schedule/9001_schedule.db    2Feb2021     TRUE           FALSE
  # 9002        /projects/rl_ema_monitoring/Subjects/9002/schedule/9002_schedule.db    2Feb2021    FALSE           FALSE
  
  physio_df <- bind_rows(physio_list)
  #something like (if forget: do we get one file per recording? If so, we'd have multiple rows per sub)
  # subject_id                                        physio_file    last_cached  active
  #       9001   /abspath/Subjects/9001/physio/somethingphysio.db        2Feb2021   TRUE
  #       9001   /abspath/Subjects/9001/physio/somethingphysio2.db       2Feb2021   TRUE
  #       9002   /abspath/Subjects/9002/physio/somethingphysio.db        2Feb2021   FALSE
  
  video_df <- bind_rows(video_list)
  #something like
  # subject_id                             video_file     last_cached  active
  #       9001   /abspath/Subjects/9001/video/day1.mp4       2Feb2021   TRUE
  #       9001   /abspath/Subjects/9001/video/day2.mp4       2Feb2021   TRUE
  #       9002   /abspath/Subjects/9002/video/day1.mp4       2Feb2021  FALSE
  
  return(list(schedule=sched_df, physio=physio_df, video=video_df))
}


# Refresh data cache
#
# @param data_dir Root of Subjects data directory
#
# @importFrom checkmate::assert_directory_exists
# @return a simple data.frame or list summarizing the caching process. Essentially a
#    log file in tabular form so that we know what data were downloaded and what data
#    were already up to date.
refresh_ema_cache <- function(
  data_dir=NULL,
  log_file=file.path(data_dir, sprintf("cache_transfer_%s.txt", format(Sys.time(), "%d%b%Y-%H%M")))
) {
  checkmate::assert_directory_exists(data_dir)

  #fire off whatever python scripts are needed to update all cached data from Google Drive
  #log_result <- system("python something here", intern=TRUE)
  #cat(log_result, file=log_file, sep="\n") #something along these lines
  #return(log_result) #as a data.frame or other summary object
}
