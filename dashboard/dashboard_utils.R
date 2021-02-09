# This R script reformats the code from Subject_performance_monitoring.R 
# into utilities in the form of functions based on the file structure
# of this repo.
# Note: these functions should be location agnostic to be called anywhere within 
# the rl_ema_moitoring directory
rm(list = ls())
graphics.off()
library("RSQLite")
library("plyr")
library("dplyr")
library("ggplot2")
library("zoo")
library("sqldf")

# finds the given root directory for a file hierarchy
findRoot <- function(root_dir) {
  # get the current directory
  currDir <- getwd()
  # split the directory path into a list of 
  split_str <- strsplit(currDir,'/')[[1]]
  # check if you are already at "root_dir"
  if (tail(split_str, n=1) != "root_dir") {
    # check to make sure that the "root_dir" is in the file path or raise an error
    if ((root_dir %in% split_str) == FALSE) {
      errorMessage <- paste("Error: The root directory given for this file hierarchy ", root_dir, " was not found.")
      stop(errorMessage)
    }
    # iterate through the path list until the "root_dir" dir is found
    i = -1
    for (item in split_str) {
      # if "root_dir" is found, then done
      if (item != "root_dir") {
        i = i + 1
      }
      # else, inrement i
      else {
        break()
      }
    }
    # cut the string off after "root_dir"
    split_str <- split_str[1:i]
  } # else, do nothing
  # recombine the vector into a path string
  pathStr <- paste0(split_str, collapse = '/')
  # return the path string
  return(pathStr)
}

# uses findRoot("rl_ema_monitoring")
getSubjectPath <- function(subject) {
  # findRoot function call
  pathStr <- findRoot("rl_ema_monitoring")
  # append "/data/Subjects" to the "rl_ema_monitoring" path
  pathStr <- paste0(pathStr, "/data/Subjects/", subject)
  # return the path
  return(pathStr)
}

# returns the  for a subject, takes subject and data item to retrieve as inputs
# will return the entire table unless sql if cols is left as NA, also allows for multiple selections at once.
getSchedDataItem <- function(subjID, item, cols=NA) {
  # get the path to the subject
  pathSubjSched <- paste0(getSubjectPath(subjID), "/schedule")
  # pattern string for the db file
  pat <- paste0("*_", subjID, "_schedule.db")
  # get a list of subject's schedue files
  fileList <- list.files(pathSubjSched, pattern = pat)
  # ensure there is only one schedule.db file located here (remainder should be archived in the archive directory)
  if (length(fileList) > 1) {
    errorMessage <- paste("Error: there is more than 1 schedule.db file at ", pathSubjSched)
    stop(errorMessage)
  }
  # load the schedule.db file
  data = dbConnect(SQLite(), paste0(pathSubjSched, '/', fileList))
  # SQL selection string
  sqlStr <- paste("SELECT * FROM ", item)
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
  return(chosenItem)
}

