################################
# Script to launch the site and 
# activate a daemon for auto-rendering
# rmd files if their data changes.
################################
library("future")
library("optparse")
library("yaml")
# source the daemon_utils
source("R_scripts/daemon_utils.R")

# create the system argument list
option_list = list(
  make_option(c("-P", "--port"), default='4321', type='character',
              help="Port that the server should be made public on. Defaults to default Hugo port."),
  make_option(c("-H", "--host"), default='127.0.0.1', type='character',
              help="IP address that the server should be hosted on. Defaults to localhost.")
)
# create the argument parser object
opt = parse_args(OptionParser(option_list=option_list))

# set the baseurl within the project yaml file
old_yaml <- read_yaml("config.yaml")
new_baseurl <- paste0('http://', opt$host, ':', opt$port, '/')
write_yaml()

# set the plan to not be sequential
plan(multisession)

# create an asynchronous function for rmd render daemon
async_startRMDDaemon <- future({startRMDDaemon()})

# create an asynchronous function for blogdown daemon
async_startBlogdownDaemon <- future({startBlogdownDaemon(portNum=opt$port, host=opt$host)})
