# install reticluate
install.packages("reticulate")
install.packages("blogdown")
# load reticulate
library(reticulate)
# get the list of conda environments
conda_envs = conda_list()
conda_envs = list(conda_envs$name)
# if the conda environment does not exists
if (! "r-reticulate" %in% conda_envs) {
  conda_create("r-reticulate")
}
# install hugo via blogdown
blogdown::install_hugo()

# function to load the files
processFile = function(filepath) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    install.packages(line, quiet=TRUE)
  }
  
  close(con)
}

# install any remaining dependencies from text file
processFile("r_packages.txt")

