library('reticulate')
library('rjson')
library('tinsel')

# finds the given root directory for a file hierarchy
findRoot <- function(root_dir) {
  # get the current directory
  currDir <- getwd()
  # split the directory path into a list of 
  split_str <- strsplit(currDir,'/')[[1]]
  # check if you are already at "root_dir"
  if (tail(split_str, n=1) != root_dir) {
    # check to make sure that the "root_dir" is in the file path or raise an error
    if ((root_dir %in% split_str) == FALSE) {
      errorMessage <- paste("Error: The root directory given for this file hierarchy ", root_dir, " was not found.")
      stop(errorMessage)
    }
    # iterate through the path list until the "root_dir" dir is found
    i = 0
    for (item in split_str) {
      # if "root_dir" is found, then done
      if (item != root_dir) {
        i = i + 1
      }
      # else, increment i
      else {
        break()
      }
    }
    #j = length(split_str) - i
    # cut the string off after "root_dir"
    split_str <- split_str[1:i] #j
  } 
  # otherwise, remove the last root from the root path
  else {
    split_str <- head(split_str, -1)
  }
  # recombine the vector into a path string
  pathStr <- paste0(split_str, collapse = '/')
  # return the path string\
  #print(pathStr)
  return(pathStr)
}

# function that finds the root directory, follows a given path, and sources an R script by default or a python script if specified
sourceFromRoot <- function(root_dir, from_root, sourced_file, python=FALSE) {
  # creates the sourced paths from the inputs given
  source_path = paste0(findRoot(root_dir),'/',root_dir,'/', from_root, '/', sourced_file)
  #print(source_path)
  # by default, assume sourcing an R script
  if (python == FALSE) {
    source(source_path)
  }
  # otherwise, source a python script
  else {
    source_python(source_path, envir = globalenv())
  }
}

# function to reset the config file by running the python script directly on the system at the project root directory
reset_cfg <- function(root_dir) {
  # get the root directory path
  root_path <- paste0(findRoot(root_dir), '/', root_dir)
  # save the current working directory
  curr_dir <- getwd()
  # set the working dir to be the designated root
  setwd(root_path)
  # run the reset cfg script on the system
  system(paste0("python rebuild_config.py --root_name ", root_dir))
  # reset the working directory
  setwd(curr_dir)
}

# function that goes to the root and gets the path from the cfg.json file as a dataframe
# this function should effectively replace sourceFromRoot()
sourceFromCfg <- function(root_dir, sourced_file) {
  # get the root directory path
  root_path <- findRoot(root_dir)
  # get the whole path to cfg.json
  cfg_path <- paste0(root_path, '/', root_dir, '/cfg.json')
  # import json from python
  json <- import("json")
  # import python built-ins
  py <- import_builtins()
  # variable to assign the path to
  file_path <- NA
  # open the cfg.json file
  with(py$open(cfg_path, "r") %as% file, {
    s <- file$read()
    d <- r_to_py(json$loads(s))
    #print(d['subject']['gmail'])
    file_path <- py_to_r(d[sourced_file])
  })
  # THE ABOVE LINE WORK, LEAVE IT ALONE
  # combines the name and path to get the full destination string
  source_path = paste0(file_path, '/', sourced_file)
  # by default, assume sourcing an R script
  if (endsWith(sourced_file, '.R')) {
    source(source_path)
  }
  # otherwise, source a python script
  else if (endsWith(sourced_file, '.py')) {
    source_python(source_path, envir = globalenv())
  }
}

# add the set_status functions
sourceFromCfg('rl_ema_monitoring','set_status_func.py') # function used for cli is: add_subject_by_status(id, status)

# add the add_subject functions
sourceFromCfg('rl_ema_monitoring', 'add_subject_func.py') # function used for cli is: add_subject(id, gmail, status, path)

# add the momentum_pull functions
sourceFromCfg('rl_ema_monitoring', 'momentum_pull_func.py') # function used for cli is: pull_files(id, path)

# simple function to return a structured list of subjects currently cached in the data/Subjects directory
getSubjList <- function(data_dir) { 
  #Shane: not sure if we should be passing in some sort of config object
  #Shane: not sure if we should be accessing a json that has caching details like when a subject was last refreshed
  #Shane: we need a mechanism for determining if a subject is still active in data collection
  checkmate::assert_directory_exists(data_dir)
  
  subjects_dir <- file.path(data_dir, "Subjects")
  checkmate::assert_directory_exists(subjects_dir)
  
  sdirs <- list.dirs(subjects_dir, recursive = FALSE, full.names = TRUE)
  
  slist <- list()
  for (ss in sdirs) {
    this_subj <- list()
    expect_json <- file.path(ss, "subject.json")
    if (!checkmate::test_file_exists(expect_json)) {
      stop("Cannot find subject.json file for: ", ss)
    } else {
      meta <- jsonlite::read_json(expect_json)  #not entirely sure what to expect in here
      this_subj$id <- meta$subject$id
      #would be great for json to have some sort of date information about time last cached.
    }
    
    #schedule
    expect_sched <- Sys.glob(file.path(ss, "schedule", "*.db"))
    if (length(expect_sched) > 1L) {
      print(expect_sched)
      stop("Found multiple schedule files. I'm confused.")
    } else if (length(expect_sched) == 0L) {
      stop("Cannot find a schedule file in: ", file.path(ss, "schedule"))
    } else {
      this_subj$sched_file <- expect_sched[1L]
      this_subj$sched_date <- file.info(expect_sched[1L])$mtime
    }
    
    #physio
    expect_physio <- Sys.glob(file.path(ss, "physio", "*.db"))
    if (length(expect_physio) == 0L) {
      warning("Cannot find any physio db files in: ", file.path(ss, "physio"))
    } else {
      this_subj$physio_files <- expect_physio
    }
    
    #video -- not sure if it will always be mp4 or m4v or others
    expect_video <- Sys.glob(file.path(ss, "video", "*.mp4"))
    if (length(expect_video) == 0L) {
      warning("Cannot find any video mp4 files in: ", file.path(ss, "video"))
    } else {
      this_subj$video_files <- expect_video
    }
    
    slist[[ this_subj$id ]] <- this_subj
  }
  
  return(slist)
}

getSubjPageList <- function(dashboard_dir=getwd()) {
  site_pages <- get_report_cache(dashboard_dir)$page_summary
  #convert links
  site_pages <- site_pages %>% mutate_at(vars(ends_with("_page")), ~convert_to_link(href=., detect=TRUE))
  
  return(site_pages)
}

# loads the dashboard config file from a predefined dashboard path
loadDashConfig <- function() {
  # check if in dirs dashboard/, study_management/, etc...
  
}

# uses findRoot("rl_ema_monitoring") -> updates to use config file
getSubjectPath <- function(subject) {
  # load the json file: dashboard_config.json
  
  # findRoot function call
  pathStr <- findRoot("dashboard") # "rl_ema_monitoring"
  # append "/data/Subjects" to the "rl_ema_monitoring" path
  pathStr <- file.path(pathStr, "data", "Subjects", subject) # "/data/Subjects/"
  # return the path
  return(pathStr)
}

# returns the  for a subject, takes subject and data item to retrieve as inputs
# will return the entire table unless sql if cols is left as NA, also allows for multiple selections at once.
# note: cols should be given as a list
getSchedDataItem <- function(subjID, item=NA, cols=NA) {
  # TODO: add a list of lists option for input to select specific tables and specific columns simultaneously
  # get the path to the subject
  pathSubjSched <- paste0(getSubjectPath(subjID), "/schedule")
  # pattern string for the db file
  pat <- paste0("*_", subjID, "_schedule.db")
  # get a list of subject's schedule files
  fileList <- list.files(pathSubjSched, pattern = pat)
  # ensure there is only one schedule.db file located here (remainder should be archived in the archive directory)
  if (length(fileList) > 1) {
    errorMessage <- paste("Error: there is more than 1 schedule.db file at ", pathSubjSched)
    stop(errorMessage)
  } else if (length(fileList) == 0L) {
    stop("Cannot locate schedule db file in folder: ", pathSubjSched)
  }
  # load the schedule.db file
  data = dbConnect(SQLite(), paste0(pathSubjSched, '/', fileList))
  # if item is NA, return the entire subject db
  if (is.na(item)) {
    # get the list of tables in the DB
    namesDB <- dbListTables(data)
    # create an empty list to hold each table as an element
    tables <- list()
    # loop through the list of tables and append the dfs to the list
    k = 1
    for (dfName in namesDB) {
      # set the sql string
      sqlStr <- paste0("SELECT * FROM ", dfName[1])
      # get the current dataframe
      curr_df <- dbGetQuery(data, sqlStr)
      # append the table to the list
      tables <- append(tables,list(curr_df),0)
    }
    
    #close connection before return
    dbDisconnect(data)
    return(tables)
  }
  # SQL selection string
  sqlStr <- paste0("SELECT * FROM ", item)
  # select the data item from the db
  chosenItem = dbGetQuery(data, sqlStr)
  if (is.na(cols) != TRUE) {
    # first half of the sql string
    subStr1 = "SELECT "
    # second half of the sql string
    subStr2 = " FROM "
    # generate the sql string for column selection
    for (c in cols) {
      subStr1 <- paste0(subStr1, c, ", ")
    }
    # remove the ending ', '
    subStr1 <- substr(subStr1,1,nchar(subStr1)-2)
    # finalize the sql query string
    subStr1 <- paste0(subStr1, subStr2, "chosenItem")
    # run the sql query on the dataframe
    chosenItem <- sqldf(subStr1)
  }
  # return the data item from the db
  
  dbDisconnect(data)
  return(chosenItem)
}

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
