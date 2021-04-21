# start blogdown
blogdown::serve_site(daemon=TRUE)

# main loop to keep blogdown alive
while(TRUE) {
  # sleep for two seconds
  Sys.sleep(2)
}