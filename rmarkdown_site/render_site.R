get_subject_list <- function(data_dir) {
  jj <- rjson::fromJSON(file = file.path(data_dir, "subject_status.json"))$subjects
  active <- data.frame(id=jj$active, status="active")
  inactive <- data.frame(id=jj$inactive, status="inactive")
  rbind(active, inactive)
} 


render_subject_reports(
  data_dir="/Users/hallquist/Downloads/data",
  output_dir="/Users/hallquist/Data_Analysis/Momentum/rl_ema_monitoring/rmarkdown_site/rendered_site",
  debug=FALSE
)

render_subject_reports <- function(data_dir, output_dir, rerender_mins=10, force=FALSE, debug=FALSE) {
  checkmate::assert_directory_exists(data_dir)
  #extant <- get_report_cache(data_dir)$page_summary 
  
  slist <- get_subject_list(data_dir)
  
  for (ss in seq_len(nrow(slist))) {
    this_subj <- slist[ss, , drop=FALSE]
    # look at whether the compliance report is older than the latest data
    # if data are newer than render_date, we need to regenerate the page
    #extant_this_subj <- extant %>% filter(id==this_subj$id)
    render_subject <- TRUE
    
    # if (nrow(extant_this_subj) > 1L) {
    #   print(extant_this_subj)
    #   stop("found more than one record for this subject in cache: ", expect_cache)
    # } else if (isTRUE(force)) {
    #   render_subject <- TRUE
    # } else if (nrow(extant_this_subj) == 1L) {
    #   date <- extant_this_subj %>% pull(render_date)
    #   data_date <- this_subj$sched_date
    #   if (difftime(date, data_date, units="mins") < -1*rerender_mins) {
    #     render_subject <- TRUE
    #   }
    #   
    #   #also try re-rendering automatically on previous error (Shane: could have more elegant error tracking/re-rendering scheme)
    #   if (extant_this_subj$page == "Rendering Error") { render_subject <- TRUE }
    # } else if (nrow(extant_this_subj) == 0L) {
    #   render_subject <- TRUE
    # }
    
    if (isTRUE(render_subject)) {
      #do the work
      outfile <- paste0(this_subj$id, ".html") #could be handled by a config$file approach
      result <- tryCatch(expr={
        rmarkdown::render(
          "report_generators/subject_report.Rmd", 
          params=list(id=this_subj$id, data_dir=data_dir, output_dir=output_dir, render_debug=debug), 
          output_dir = file.path(output_dir, "Subjects"), output_file = outfile,
          output_yaml = "_site.yml") #this doesn't preserve the navbar
      }, error=function(e) { warning("Error rendering subject report for: ", this_subj$id); return("Error") })
      
      if (result == "Error") { 
        page <- "Rendering Error"  #Shane: would be good to log these errors so that clicking would take the user to a log of the error.
      } else {
        page <- file.path(outfile)
      }
      
      #insert_into_report_cache(data_dir, id=this_subj$id, list(page_summary=list(page=page, render_date=Sys.time())))
    }
    
  }
  
}
