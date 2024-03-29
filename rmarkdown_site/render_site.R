#site_settings <- list(
#  data_dir="/Users/hallquist/Downloads/subject_reports_09_15",
#  site_dir="/Users/hallquist/Data_Analysis/Momentum/rl_ema_monitoring/rmarkdown_site",
#  output_dir="/Users/hallquist/Data_Analysis/Momentum/rl_ema_monitoring/rmarkdown_site/rendered_site",
#  debug=FALSE
#)

site_settings <- list(
  data_dir="/Volumes/bierka_root/datamesh/RAW/Momentum_App/data",
  site_dir="/Users/dnplserv/rl_ema_monitoring/rmarkdown_site",
  output_dir="/Users/dnplserv/momentum_site",
  debug=FALSE
)

# site settings using yaml config params
# site_settings <- list(
#   data_dir=dataPath,
#   site_dir=paste0(repoRoot, '/rmarkdown_site'),
#   output_dir=sitePath,
#   debug=FALSE
# )

#setwd(site_settings$site_dir)
#source("report_functions/report_functions.R")

render_subject_reports <- function(data_dir, site_dir, output_dir, rerender_mins=10, force=FALSE, debug=FALSE) {
  checkmate::assert_directory_exists(data_dir)
  checkmate::assert_directory_exists(site_dir)
  setwd(site_dir)
  #extant <- get_report_cache(data_dir)$page_summary 
  
  #slist <- active #<- should be able to use the active global here to not run all subjects
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
      
      # Calling rmarkdown::render in this session generates problems with contaminating the global environment
      # and global variable scope within the knitted markdown. To mirror what happens when you hit 'Knit' in
      # Rstudio (which works well here), we need to initialize a clean R session that includes the globalenv()
      # See here: https://stackoverflow.com/questions/32257970/knitr-inherits-variables-from-a-users-environment-even-with-envir-new-env
      
      result <- tryCatch(expr={
        #rmarkdown::render(
        render_separately(
          file.path(site_dir, "subject_report.Rmd"),
          params=list(id=this_subj$id, data_dir=data_dir, output_dir=output_dir, render_debug=debug),
          output_dir = file.path(output_dir, "Subjects"), 
          output_file = outfile,
          output_options = list(
            self_contained=FALSE,
            lib_dir=file.path(output_dir, "Subjects", "site_libs")
          )
        )
        #output_yaml = "_site.yml") #this doesn't preserve the navbar
      }, error=function(e) { warning("Error rendering subject report for: ", this_subj$id); return("Error") })
      
      if (result == "Error") { 
        page <- "Rendering Error"  #Shane: would be good to log these errors so that clicking would take the user to a log of the error.
      } else {
        page <- file.path(outfile)
      }
      
      #insert_into_report_cache(data_dir, id=this_subj$id, list(page_summary=list(page=page, render_date=Sys.time())))
    }
  }
  
  file.copy(file.path(site_dir, "dashboard.css"), file.path(output_dir, "Subjects", "dashboard.css"))
  
}

render_main_pages <- function(data_dir, site_dir, output_dir, debug=FALSE) {
  #just index for now...
  result <- tryCatch(expr={
    #rmarkdown::render(
    render_separately(
      file.path(site_dir, "index.Rmd"),
      params=list(data_dir=data_dir, output_dir=output_dir, render_debug=debug),
      output_dir = output_dir,
      output_options = list(
        self_contained=FALSE,
        lib_dir=file.path(output_dir, "site_libs")
      )
    )
  }, error=function(e) { warning("Error rendering index.html"); return("Error") }
  )
}

do.call(render_subject_reports, site_settings)
do.call(render_main_pages, site_settings)

#source("push_site.R")
