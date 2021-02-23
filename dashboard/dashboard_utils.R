# This R script reformats the code from Subject_performance_monitoring.R 
# into utilities in the form of functions based on the file structure
# of this repo.
# Note: these functions should be location agnostic to be called anywhere within 
# the rl_ema_moitoring directory

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
      # else, inrement i
      else {
        break()
      }
    }
    #j = i - 1
    # cut the string off after "root_dir"
    split_str <- split_str[1:i] #j
  } # else, do nothing
  # recombine the vector into a path string
  pathStr <- paste0(split_str, collapse = '/')
  # return the path string
  return(pathStr)
}

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

# sets the datetime that a caching was run
setCacheTime <- function() {
  
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
