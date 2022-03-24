library('assertthat')
#library('av')
library('reticulate')
library('rjson')
library('tinsel')
library('RSQLite')
library('tidyverse')
library("dplyr")
library("yaml")

# set python environment via reticulate
use_python(conda_python(envname = "r-reticulate"), required = TRUE)
use_condaenv(condaenv = "r-reticulate") # , required = TRUE

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

# function that scans upwards to find a specified anchor file, by default, searches up from current directory
findAnchor <- function(anchor_file="cfg.yaml", start_from=getwd()) {
  #print(anchor_file)
  #print(start_from)
  # split the directory path into a list
  split_str <- strsplit(start_from,'/')[[1]]
  # iterate up through the file directory
  while (length(split_str) > 0) {
    # get the current path
    curr_dir = paste(split_str, collapse="/")
    # get the list of files at the current path
    curr_dir_files = list.files(curr_dir)
    # if the anchor file was found, return it's full path
    if(anchor_file %in% curr_dir_files) {
      return(paste0(curr_dir, '/', anchor_file))
    }
    # otherwise, pop the last element of the directory list
    split_str <- split_str[- length(split_str)]
  }
  # if the anchor file was not found, return NULL
  return(NULL)
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

# function that goes to the root and gets the path from the cfg.json and loads .db file
getPathFromCfg <- function(root_dir, cfg_name='cfg.json', sourced_file=NA, keywords=NA, exclusion=NA, pattern=FALSE, findRootDir=TRUE) {
  # if findRootDir is true, then we look for the root directory mathcing the argument for "root_dir"
  if(findRootDir == TRUE) {
    # get the root directory path
    root_path <- findRoot(root_dir)
    # get the whole path to cfg.json
    cfg_path <- paste0(root_path, '/', root_dir, '/', cfg_name)
  } else { # otherwise, treat the root dir given as an absolute path
    # get the whole path to cfg.json
    cfg_path <- paste0(root_dir, '/', cfg_name)
  }
  #print(soured_file)
  # get the whole path to cfg.json
  #cfg_path <- paste0(root_path, '/', root_dir, '/cfg.json')
  # import python built-ins
  py <- import_builtins()
  # import json from python
  json <- reticulate::import("json")
  # variable to assign the path to
  file_path <- NA
  # open the cfg.json file
  with(py$open(cfg_path, "r") %as% file, {
    # read in the file
    s <- file$read()
    # convert to a python object
    d <- r_to_py(json$loads(s))
    # initialize variable for file path
    f_path <- NA
    # if the file name is not a pattern (should be used as the file name)
    if (pattern == FALSE) {
      # get the item the python dict and convert back to r
      f_path <- py_to_r(d[sourced_file])
      # if the string is a pattern
    } else if (pattern == TRUE) {
      # get the list of keys from the json file
      keys <- names(json$loads(s))
      # convert the list of keys to a wide 1D dataframe
      keys <- data.frame(as.list(keys))
      # reduce the list of keys that contain the sourced_file argument as a substring
      keys <- keys[,grepl(paste0("*", sourced_file, "*"),names(keys))]
      # initialize the final result of this inner method
      f_path <- character()
      # use the remaining keys to select the paths from the json,iterate through the keys
      for (key in keys) {
        # append the key to the character string
        f_path <- append(f_path, py_to_r(d[key]))
      }
    }
    # if the file_path returned is a single string
    if (is.string(f_path) == TRUE) {
      #print('string')
      # set the file_path to this single returned string
      file_path <- f_path
      # if the file_path returned is a list
    } else if (is.character(f_path) == TRUE) {
      #print('list')
      # create a wide 1D dataframe with the list of paths
      f_path <- data.frame(as.list(f_path))
      # if there are keyword selectors
      if (is.na(keywords) != TRUE) {
        # iterate through the keywords given
        for (kword in keywords) {
          # drop the paths that do not contain the keyword
          f_path <- f_path[,grepl(paste0("*", kword),names(f_path))] # 2022-03-24 removed "*" after kword
          # break if you have reduced the number of paths to 1
          if (length(f_path) == 1) {
            break
          }
        }
      }
      message(f_path)
      # if there are exclusion substrings
      # if ((is.na(exclusion) != TRUE) && length(f_path) > 1) {
      #   # iterate through the keywords given
      #   for (exclude in exclusion) {
      #     # drop the paths that do not contain the keyword
      #     f_path <- f_path[,!grepl(paste0("*", exclude, "*"),names(f_path))]
      #     # break if you have reduced the number of paths to 1
      #     if (length(f_path) == 1) {
      #       break
      #     }
      #   }
      # }
      # ensure there is only a string returned
      if ((is.string(f_path) != TRUE) || (is.na(f_path) != FALSE)) {
        print("Error: From the filename given, keywords selected, and exclusion substrings; either no paths or multiple paths were returned.")
        return(NA)
      }
      # set the file_path to this single returned string
      file_path <- f_path
    }
  })
  # return the resultant file path
  return(file_path)
}

# function that goes to the root and gets the path from the cfg.json and loads a python or r script
# this function should effectively replace sourceFromRoot()
sourceFromCfg <- function(root_dir, sourced_file, cfg_name="cfg.json") {
  # get the root directory path
  root_path <- findRoot(root_dir)
  # get the whole path to cfg.json
  cfg_path <- paste0(root_path, '/', root_dir, '/', cfg_name)
  # import python built-ins
  py <- import_builtins()
  # import json from python
  json <- reticulate::import("json")
  # variable to assign the path to
  file_path <- NA
  # open the cfg.json file
  with(py$open(cfg_path, "r") %as% file, {
    # read in the file
    s <- file$read()
    # convert to a python object
    d <- r_to_py(json$loads(s))
    # get the item the python dict and convert back to r
    file_path <- py_to_r(d[sourced_file])
  })
  # combines the name and path to get the full destination string
  source_path = paste0(file_path, '/', sourced_file)
  # by default, assume sourcing an R script
  if (endsWith(sourced_file, '.R')) {
    # base r sourcing
    source(source_path)
  }
  # otherwise, source a python script
  else if (endsWith(sourced_file, '.py')) {
    # save the current working directory
    currDir = getwd()
    # set working directory to the path of the python script so that
    # it can handle its own module loading
    setwd(file_path)
    # source the python function
    source_python(source_path, envir = globalenv()) # use_condaenv(condaenv = "r-reticulate")
    # reset the working directory
    setwd(currDir)
  }
}

# add the set_status functions
sourceFromCfg('rl_ema_monitoring','set_status_func.py') # function used for cli is: add_subject_by_status(id, status)

# add the add_subject functions
sourceFromCfg('rl_ema_monitoring', 'add_subject_func.py') # function used for cli is: add_subject(id, gmail, status, path)

# add the momentum_pull functions
sourceFromCfg('rl_ema_monitoring', 'momentum_pull_func.py') # function used for cli is: pull_files(id, path)

# add the rebuild_config function
sourceFromCfg('rl_ema_monitoring', 'rebuild_config_funcs.py') # function used for updating the pathing info (rough file tracking)

# add the data_management python functions
sourceFromCfg('rl_ema_monitoring', 'data_management_functions.py')

# add the redcap function
sourceFromCfg('rl_ema_monitoring', 'RC_pull.R') # functions for REDCap implementation

# function to pull the list of active subjects
getActiveList <- function(root_dir=NULL, root_cfg_var='data', cfg='data.json', active=TRUE) { #
  # get the root directory path
  root_path <- findRoot(root_dir)
  #root_path <- get_cfg_var(cfg="cfg.yaml", var=root_cfg_var)
  # get the whole path to cfg.json
  cfg_path <- paste0(root_path, '/', root_dir, '/', cfg)
  # import python built-ins
  py <- import_builtins()
  # import json from python
  json <- reticulate::import("json")
  # variable to assign the path to
  file_path <- NA
  # open the cfg.json file
  with(py$open(cfg_path, "r") %as% file, {
    # read in the file
    s <- file$read()
    # convert to a python object
    d <- r_to_py(json$loads(s))
    # get the item the python dict and convert back to r
    file_path <- py_to_r(d['subject_status.json'])
  })
  active_subjs <- NA
  # get the active subjects from the json file
  with(py$open(paste0(file_path, '/', 'subject_status.json'), "r") %as% file, {
    # read in the file
    s <- file$read()
    # convert to a python object
    d <- r_to_py(json$loads(s))
    # get the item the python dict and convert back to r
    if(active == TRUE) {
      active_subjs <- py_to_r(d$subjects$active)
    } else if(active == FALSE) {
      active_subjs <- py_to_r(d$subjects$inactive)
    }
  })
  active_list <- as.list(active_subjs)
  return(active_list)
}

# simple function to return a structured list of subjects currently cached in the data/Subjects directory (from system)
getSubjListSys <- function(active_sids=NULL, data_dir=NULL) {
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

# gets a list of subjects from the json files
getSubjListJson <- function(root_path) {

}

getSubjPageList <- function(dashboard_dir=getwd()) {
  site_pages <- get_report_cache(dashboard_dir)$page_summary
  #convert links
  site_pages <- site_pages %>% mutate_at(vars(ends_with("_page")), ~convert_to_link(href=., detect=TRUE))

  return(site_pages)
}

# returns the  for a subject, takes subject and data item to retrieve as inputs
# will return the entire table unless sql if cols is left as NA, also allows for multiple selections at once.
# note: cols should be given as a list
getSchedDataItem <- function(subjID,abs_path=NULL,item=NA, cols=NA) {
  # TODO: add a list of lists option for input to select specific tables and specific columns simultaneously
  if(is.null(abs_path)){
    # get the path to the subject
    pathSubjSched <- getPathFromCfg('rl_ema_monitoring', '_schedule.db', subjID, 'archive', pattern=TRUE)
    # pattern string for the db file
    pat <- paste0(subjID, "_schedule.db") # "*_",
    # get a list of subject's schedule files
    fileList <- list.files(pathSubjSched, pattern = pat)
    #print(paste0(pathSubjSched, '/', fileList))
    # ensure there is only one schedule.db file located here (remainder should be archived in the archive directory)
    if (length(fileList) > 1) {
      errorMessage <- paste("Error: there is more than 1 schedule.db file at ", pathSubjSched)
      stop(errorMessage)
    } else if (length(fileList) == 0L) {
      stop("Cannot locate schedule db file in folder: ", pathSubjSched)
    }
  } else {
    pathSubjSched <- dirname(abs_path)
    fileList <- basename(abs_path)
  }

  # load the schedule.db file
  data = dbConnect(SQLite(), paste0(pathSubjSched, '/', fileList))
  # if item is NA, return the entire subject db
  if (is.na(item)) {
    # get the list of tables in the DB
    namesDB <- dbListTables(data)
    # create an empty list to hold each table as an element
    # tables <- list()
    # # loop through the list of tables and append the dfs to the list
    # k = 1
    # for (dfName in namesDB) {
    #   # set the sql string
    #   sqlStr <- paste0("SELECT * FROM ", dfName[1])
    #   # get the current dataframe
    #   tables[[dfName]] <- dbGetQuery(data, sqlStr)
    #   # append the table to the list
    # }
    tables<-lapply(namesDB,function(dfName){
      dbGetQuery(data, paste0("SELECT * FROM ", dfName))
    })
    names(tables) <- namesDB
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
  # disconnect from the db file
  dbDisconnect(data)
  # return the data item from the db
  return(chosenItem)
}

get_data_info_r <- function(sid, data_type, keywords=vector(mode = "list"), exclusion=vector(mode = "list"), data_cfg="subject.json", cfg_name="data.json", pattern=TRUE) {
  # gets the data running the imported python function as a csv string
  csv_str <- get_data_info(sid=sid, data_type=data_type, keywords=keywords, exclusion=exclusion, data_cfg=data_cfg, cfg_name=cfg_name, pattern=pattern, as_str=TRUE)
  #print(csv_str)
  if(csv_str == ''){
    return(NA)
  }
  # loads the csv string into a dataframe
  data_df <- read.table(text = csv_str, sep =",", header = TRUE, stringsAsFactors = FALSE)
  # returns the dataframe
  return(data_df)
}

get_schedule_info <- function(sid=sid) {
  # running of the general info getter
  ret_val <- get_data_info_r(sid=sid, data_type="schedule", data_cfg="subject.json", cfg_name="data.json", pattern=TRUE)
  # set the subject_id column to a characters instead of integers
  ret_val$subject_id <- as.character(ret_val$subject_id)
  # return the schedule data
  return(ret_val)
}

get_physio_info <- function(sid=sid) {
  # running of the general info getter
  ret_val <- get_data_info_r(sid=sid, data_type="physio", data_cfg="subject.json", cfg_name="data.json", pattern=TRUE)
  # set the subject_id column to a characters instead of integers
  ret_val$subject_id <- as.character(ret_val$subject_id)
  # return the schedule data
  return(ret_val)
}

get_video_info <- function(sid=sid) {
  # running of the general info getter
  ret_val <- get_data_info_r(sid=sid, data_type="video", data_cfg="subject.json", cfg_name="data.json", pattern=TRUE)
  # set the subject_id column to a characters instead of integers
  ret_val$subject_id <- as.character(ret_val$subject_id)
  # return the schedule data
  return(ret_val)
}

# convenience function to pull a data item from the cfg.yaml file (contains important runtime info)
get_cfg_var <- function(cfg="cfg.yaml", start_from=NULL, var=NULL) {
  if (is.null(start_from)){
    start = getwd()
  } else {
    start = start_from
  }
  # get the path to the cfg.yaml file
  yaml_path = findAnchor(anchor_file=cfg, start_from=start)
  #print(yaml_path)
  # read the cfg.yaml data into the instance of R
  yaml_data = read_yaml(yaml_path)
  # get the specific variable
  ret_val = yaml_data[[var]]
  # return the selected variable
  return(ret_val)
}

get_redcap_checklist_r <- function(rc_url, rc_token, subj_id) {
  # gets the data running the imported python function as a csv string
  csv_str <- get_subj_redcap_checklist(rc_url=rc_url, rc_token=rc_token, subj_id=subj_id, as_str=TRUE)
  # loads the csv string into a dataframe
  data_df <- read.table(text = csv_str, sep =",", header = TRUE, stringsAsFactors = FALSE)
  # returns the dataframe
  return(data_df)
}

# Function to provide list of currently cached subject schedule files
# A vector of subject_ids can be passed in lieu of a root dir variable. This is
# especially useful when getting metadata for a specific subject.
# @param data_dir Root of Subjects data directory
# @importFrom checkmate assert_directory exists
# @importFrom dplyr bind_rows
# @return A three-element list containing cache info for
get_ema_subject_metadata <- function(root_dir=NULL, subject_list=NULL, trigger_refresh=FALSE)  {
  #checkmate::assert_directory_exists(data_dir)
  checkmate::assert_logical(trigger_refresh)

  if (isTRUE(trigger_refresh)) { refresh_ema_cache(data_dir) }

  #folders <- list.dirs(path=data_dir, full.names=TRUE, recursive=FALSE)

  #something along the lines of ...
  sched_list <- list()
  physio_list <- list()
  video_list <- list()

  # get the root dir
  if(is.null(root_dir)){
    root_dir <- basename(get_cfg_var(var="root"))
  }

  # get a list of all participants
  if(is.null(subject_list)){
     subject_list <- getActiveList(root_dir=root_dir)
  }

  #for (ff in folders) {
  for (sid in subject_list) {
    print(sid)
    #sid <- basename(ff)
    #figure out schedule stuff
    print("running schedule...")
    sched_list[[sid]] <- get_schedule_info(sid=sid) #return a one-row data.frame summarizing status of schedule # , data_dir
    print("running physio...")
    physio_list[[sid]] <- get_physio_info(sid=sid) #return multi-row data.frame, one row per subject physio file # , data_dir
    print("running video...")
    video_list[[sid]] <- get_video_info(sid=sid) #return multi-row data.frame, one row per subject video file # , data_dir
  }
  print("binding schedule")
  # drops subjects whose data has not yet been collected (set to active -> are about to start ema, but have not started yet)
  sched_list <- sched_list[sapply(sched_list, function(x) dim(x)[1]) > 0]
  sched_df <- bind_rows(sched_list)
  # something like
  # subject_id  subject_folder                                                      last_cached   active   cache_failure
  # 9001        /projects/rl_ema_monitoring/Subjects/9001/schedule/9001_schedule.db    2Feb2021     TRUE           FALSE
  # 9002        /projects/rl_ema_monitoring/Subjects/9002/schedule/9002_schedule.db    2Feb2021    FALSE           FALSE
  print("binding physio")
  # drops subjects whose data has not yet been collected (set to active -> are about to start ema, but have not started yet)
  physio_list <- physio_list[sapply(physio_list, function(x) dim(x)[1]) > 0]
  physio_df <- bind_rows(physio_list)
  #something like (if forget: do we get one file per recording? If so, we'd have multiple rows per sub)
  # subject_id                                        physio_file    last_cached  active
  #       9001   /abspath/Subjects/9001/physio/somethingphysio.db        2Feb2021   TRUE
  #       9001   /abspath/Subjects/9001/physio/somethingphysio2.db       2Feb2021   TRUE
  #       9002   /abspath/Subjects/9002/physio/somethingphysio.db        2Feb2021   FALSE
  print("binding video")
  # drops subjects whose data has not yet been collected (set to active -> are about to start ema, but have not started yet)
  video_list <- video_list[sapply(video_list, function(x) dim(x)[1]) > 0]
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
  # get a list of all subject

  # repull all subject data

  #

  #fire off whatever python scripts are needed to update all cached data from Google Drive
  #log_result <- system("python something here", intern=TRUE)
  #cat(log_result, file=log_file, sep="\n") #something along these lines
  #return(log_result) #as a data.frame or other summary object
}
#function to reformat dates from how they appear in the db file to a more readable format
date_format <- function(dt=NULL){
  year <- str_extract(dt,"\\d{4}")
  month <- str_extract(dt,"-\\d{2}-") %>% str_extract("\\d{2}")
  day <- str_extract(dt,"\\d{2}$")
  return(paste0(month,"/",day,"/",year))
}

#function to match responses to intoxication questions to the blocks they preceded
get_intox_resp <- function(block_time=NULL,id=NULL){
  preceding_responses <- filter(output$proc_data[[id]]$raw_data$drugs_check, time < block_time) #grab only intox questions completed before the block started
  most_recent_time <- filter(preceding_responses, time==max(preceding_responses$time)) #find the most recent of these times in order to identify the intox question completed right before the block
  return(most_recent_time$answer)
}

