dataFileStr2List <- function(dataStr=NULL) {
  ##########################################
  # Function to convert a data file path
  # given as a string to a one-row
  # dataframe with path, mtime, and used_by.
  #########################################
  # get the file's path
  file_path <- dirname(dataStr)
  # get the file's modification time
  file_mtime <- as.double(file.info(paste0(getwd(), '/data/', dataStr))$mtime)
  # create the list
  data_list <- list(file_path, file_mtime, "")
  # assign column names
  names(data_list) <- c('file_path', 'mtime', 'used_by')
  # return the list
  return(data_list)
}

addUsedBy <- function(base_str=NULL, append_str=NULL) {
  #####################################################
  # Basic function for adding to 'used_by'
  # property of a data file.
  #####################################################
  if(base_str == "") {
    return(append_str)
  }
  else {
    return(paste0(base_str, ',', append_str))
  }
}

buildData2ContentFromSys <- function() {
  ######################################
  # Function to get a json-writable data
  # struct relating site/content to site/data.
  ######################################
  # get a list of data files
  lf_data <- as.list(list.files(path = './data', pattern = '*.json|*.yaml|*.toml|*.csv|*.tsv|*.rds|*.RData|*.db', recursive = TRUE))
  # get a list of rmd files
  lf_content <- as.list(list.files(path = './content', pattern = '*.Rmd', recursive = TRUE))
  # initialize the return list
  lf_out <- list()
  # convert each data file from a string to a single row df containing file_path, file_name, and mtime
  for(data_file in lf_data) {
    # convert the item to a df
    lf_out[[toString(data_file)]] <- dataFileStr2List(dataStr=data_file)
  }
  # populate used_by to the data files by adding content with a shared file path
  for(content_file in lf_content) {
    cf_str <- toString(content_file)
    count = 1
    for(data_file in lf_out) {
      # if the paths of the data file and content file are the same
      fpath <- data_file$file_path
      if (dirname(cf_str) == fpath) {
        # append the content file to the data file's list of used_by
        lf_out[[count]]$used_by <- addUsedBy(base_str=lf_out[[count]]$used_by, append_str=cf_str)
      }
      # iterate the count
      count = count + 1
    }
  }
  # return the data structure
  return(lf_out)
}

setData2ContentJSON <- function(new_json=NULL) {
  ##############################################
  # Basic function to save the new data.json
  # file to a known destination.
  ##############################################
  # creating the data for JSON file
  jsonData <- rjson::toJSON(new_json)
  # writing into JSON file
  write(jsonData, "R_scripts/data.json") 
}


getData2ContentFromJSON <- function() {
  #####################################
  # Basic function to load the old data.json
  # file from a known destination.
  #####################################
  return(rjson::fromJSON(file="R_scripts/data.json"))
}

addRMD <- function(base_list=NULL, append_str=NULL) {
  ###################################################
  # Basic function for adding to 'used_by'
  # property of a data file to a list of RMD files
  # to re-render.
  ###################################################
  # convert the append_str to a list
  #print(append_str)
  append_list <- as.list(strsplit(append_str, ",")[[1]])
  # get the difference between the base list and append list
  new_rmds <- setdiff(append_list, base_list)
  # add the new rmds to the base list
  new_list <- append(base_list, new_rmds)
  # return the appended rmd list
  return(new_list)
}

getRMDsToRender <- function(old_data=NULL, new_data=NULL) {
  #########################################################
  # Function to get a list of data files
  # with new modification times.
  #########################################################
  # initialize the return value
  rmds2render <- list()
  # get lists of the old and new files
  old_files <- as.list(names(old_data))
  new_files <- as.list(names(new_data))
  #print(old_files)
  #print(new_files)
  # get lists of the created files, deleted files, and existing files
  # exist in new_data, but not old_data
  created_files <- setdiff(new_files, old_files)
  # exist in new_data, but not old_data
  deleted_files <- setdiff(old_files, new_files)
  # exist in new_data and old_data
  existing_files <- intersect(new_files, old_files)
  #print(existing_files)
  # iterate through created files get files to render from new_data
  for(created_file in created_files) {
    #print(created_file)
    #print(new_data[[created_file]])
    rmds2render <- addRMD(rmds2render, new_data[[created_file]]$used_by)
  }
  # iterate through deleted files get files to render from old_data
  for(deleted_file in deleted_files) {
    rmds2render <- addRMD(rmds2render, old_data[[deleted_file]]$used_by)
  }
  # iterate through the existing files to check if they have been modified
  for(existing_file in existing_files) {
    # if the modification time has changed
    if(toString(old_data[[existing_file]]$mtime) != toString(new_data[[existing_file]]$mtime)) {
      rmds2render <- addRMD(rmds2render, new_data[[existing_file]]$used_by)
    }
  }
  # return the list of rmds to re-render
  return(rmds2render)
}

renderRMDs <- function(render_list=NULL) {
  ########################################
  # Basic function to change the modification
  # time of rmd files so that they are automatically
  # re-rendered by Hugo.
  ########################################
  for(rmd in render_list) {
    fs::file_touch(paste0('content/', rmd))
  }
}

runRMDRerender <- function() {
  ############################
  # Function to run the entire
  # process of re-rendering RMD files
  # based on the last modification of
  # data files.
  ############################
  # get the current system state
  new_data <- buildData2ContentFromSys()
  # load the old system state
  old_data <- getData2ContentFromJSON()
  # get the rmds to re-render
  render_list <- getRMDsToRender(old_data=old_data, new_data=new_data)
  # re-render the rmds
  renderRMDs(render_list=render_list)
  # save the new system state
  setData2ContentJSON(new_json=new_data)
}

exportRMDPID <- function(pid=NULL) {
  #######################
  # Basic function to save the daemon
  # pid to a file from a known destination.
  #######################
  write(pid, "R_scripts/rmd_daemon_pid.txt")
}

importRMDPID <- function() {
  #######################
  # Basic function to load the daemon
  # pid from a file from a known destination.
  #######################
  return(readLines('R_scripts/rmd_daemon_pid.txt'))
}

exportBlogdownPort <- function(pid=NULL) {
  #######################
  # Basic function to save the daemon
  # pid to a file from a known destination.
  #######################
  write(pid, "R_scripts/blogdown_pid.txt")
}

importBlogdownPID <- function() {
  #######################
  # Basic function to load the daemon
  # pid from a file from a known destination.
  #######################
  return(readLines('R_scripts/blogdown_pid.txt'))
}

startRMDDaemon <- function() {
  #######################
  # Basic function to start the daemon.
  #######################
  # start the daemon
  pid <- sys::exec_background("Rscript", "R_scripts/rmd_daemon.R")
  # save the pid to memory
  exportRMDPID(pid=pid)
}

stopRMDDaemon <- function() {
  #######################
  # Basic function to stop the daemon.
  #######################
  # load the pid
  pid = importRMDPID()
  # stop the daemon
  tools::pskill(pid)
  # daemon clean-up
  sys::exec_status(pid)
  rm(pid)
}

startBlogdownDaemon <- function() {
  #######################
  # Basic function to start the daemon.
  #######################
  # launch blogdown as a daemon with a given port
  # start the daemon
  pid <- sys::exec_background("Rscript", "R_scripts/blogdown_daemon.R")
  # save the pid to memory
  exportBlogdownPID(pid=pid)
}

stopBlogdownDaemon <- function() {
  #######################
  # Basic function to stop the daemon.
  #######################
  # load the pid
  pid = importBlogdownPID()
  # stop the daemon
  tools::pskill(pid)
  # daemon clean-up
  sys::exec_status(pid)
  rm(pid)
}