
#where to put cached schedule databases and Google authentication tokens
cache_dir <- normalizePath("~/Downloads/momentum_db_aggregation", mustWork = FALSE)
tokens_dir <- file.path(cache_dir, "tokens")


if (!dir.exists(cache_dir)) {
  message("creating database cache directory: ", cache_dir)
  dir.create(cache_dir, recursive=TRUE)
}

#tokens directory
if (!dir.exists(tokens_dir)) {
  message("creating google drive tokens directory: ", tokens_dir)
  dir.create(tokens_dir, recursive=TRUE)
}

#' @param accounts A character vector of all gmail addresses having Google Drive information.
#' @param cache_dir Path to cache all schedule db files
#' @param tokens_dir Path for all OAuth Google Drive authentication tokens
#' @param refresh_cache Number of hours to trigger re-downloading of schedule database file.
#'    Zero always refreshes, while 5 would refresh any database file more than 5 hours old on the
#'    local machine cache.
get_schedule_dbs <- function(accounts, cache_dir=getwd(), tokens_dir=getwd(), refresh_cache=0) {
  require(googledrive) 
  
  checkmate::assert_directory_exists(tokens_dir)
  checkmate::assert_directory_exists(cache_dir)
  checkmate::assert_numeric(refresh_cache, lower=0)
  
  db_files <- rep(NA_character_, length(accounts))
  
  for (aa in 1:length(accounts)) {
    #authenticate to google drive (use cached token, if available)
    googledrive::drive_auth(email = accounts[aa], cache = tokens_dir)
    
    #look for schedule db file (naming will depend on ID)
    sched_results <- drive_find(".*schedule\\.db")
    
    if (nrow(sched_results) > 1L) {
      message("More than one schedule db file found for: ", accounts[aa], ". Not sure how to proceed. Skipping to next subject.")
      print(sched_results)
      next
    } else if (nrow(sched_results) == 0L) {
      message("No schedule db file found for: ", accounts[aa], ". Not sure how to proceed. Skipping to next subject.")
      print(sched_results)
      next
    }
    
    #get handle to file information for DB
    sched_handle <- drive_get(id=sched_results$id[1L])
    
    #Create expected cached file name. Tack on id to file handle for clarity
    outname <- file.path(cache_dir, sub(".db", paste0("_", sched_results$id[1L], ".db"), sched_results$name[1L], fixed=TRUE))
    
    #timezones on Google Drive come through in UTC time code
    #convert local time to UTC for time comparison
    
    download_db <- TRUE
    if (file.exists(outname)) {
      ff <- file.info(outname)
      modtime_local <- lubridate::with_tz(ff$mtime, tz="UTC")
      modtime_remote <- lubridate::as_datetime(sched_handle$drive_resource[[1]]$modifiedTime) #unsolved problem: timezone
      tdiff <- difftime(modtime_remote, modtime_local, units="hours")
      
      if (tdiff < refresh_cache) { download_db <- FALSE } #just use existing file
    }
    
    if (download_db) {
      message("Downloading: ", sched_handle$name[1L], " for account: ", accounts[aa])
      drive_download(file = sched_handle$name[1L], path = outname, overwrite = TRUE) 
    } else {
      message("Using cached file: ", outname, " for account: ", accounts[aa])
    }
    
    db_files[aa] <- outname #return location of cached SQLite database
  }
  
  return(db_files)
}

#emails <- c(paste0("dependlabphones", c("", 2:5), "@gmail.com"))
emails <- c(paste0("dependlabphones", c(2:5), "@gmail.com"))
schedule_dbs <- get_schedule_dbs(emails, cache_dir=cache_dir, tokens_dir=tokens_dir, refresh_cache = 2) #2 hours

for (ss in schedule_dbs) {
  #load sqlite database here, run quality checks, aggregate reports, etc.
}
