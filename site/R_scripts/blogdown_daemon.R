library("optparse")
# create the system argument list
option_list = list(
  make_option(c("-P", "--port"), default='4321', type='character',
              help="Port that the server should be made public on. Defaults to default Hugo port."),
  make_option(c("-H", "--host"), default='127.0. 0.1', type='character',
              help="IP address that the server should be hosted on. Defaults to localhost.")
)
# create the argument parser object
opt = parse_args(OptionParser(option_list=option_list))

# boolean for if the site is being served locally or not
serve_local <- FALSE
if (opt$host == '127.0.0.1') {
  serve_local <- TRUE
}

# set blogdown to not use LiveReload to rerender .Rmd files
options(blogdown.knit.on_save = FALSE)
# start blogdown
blogdown::serve_site(daemon=TRUE, port=opt$port, host=opt$host) 

# main loop to keep blogdown alive
while(TRUE) {
  # sleep for two seconds
  Sys.sleep(2)
}