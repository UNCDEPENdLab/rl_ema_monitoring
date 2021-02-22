#utilities for rendering reports for each subject

#little helper to create an href tag in HTML
#example of on_click_js:
# convert_to_link(label="Logout", click_js_string="TRUE", on_click_js="Shiny.setInputValue('logout_session', '%s', {priority: \"event\"});")
#example of detect

convert_to_link <- function(href="#", label=NULL, on_click_js=NULL, click_js_string=NULL, detect=FALSE) {
  require(htmltools)
  if (isTRUE(detect)) { 
    stopifnot(is.null(label)) 
  } else {
    if (is.null(label)) { stop("Must provide a label when detect=FALSE") }
  }
  
  if (!is.null(label)) { stopifnot(length(href) == length(label)) }
    
  sapply(seq_along(href), function(xx) {
    if (is.na(href[xx])) { return("Missing") }
    this_label <- ifelse(isTRUE(detect), tools::file_path_sans_ext(basename(href[xx])), label[xx])
    if (!is.null(on_click_js)) {
      as.character(tags$a(href = href[xx], onclick = sprintf(on_click_js, param), this_label))    
    } else {
      as.character(tags$a(href = href[xx], this_label))  
    }
  })
}

insert_into_report_cache <- function(dashboard_dir, id=NULL, arguments=list()) {
  report_info <- get_report_cache(dashboard_dir, id=id)
  
  if ("page_summary" %in% names(arguments)) {
    page_summary <- report_info$page_summary
    id_row <- which(page_summary$id == id)
    if (length(id_row) == 0L) {
      df <- init_user_page_summary #start with NAs for new subject
      df$id <- id
      page_summary <- rbind(page_summary, df) #tack onto object
      id_row <- nrow(page_summary) #last row is new id
    } else {
      df <- page_summary[id_row,,drop=FALSE]
    }
    
    #may need to move beyond the $page_summary idea if we want the cache to have various other details in different elements
    stopifnot(all(names(arguments$page_summary) %in% names(df)))
    df[,names(arguments$page_summary)] <- arguments$page_summary #use data.frame-as-list syntax for simplicity
    
    page_summary[id_row,] <- df
    
    report_info[["page_summary"]] <- page_summary
  } 
  
  if ("session_info" %in% names(arguments)) {
    report_info[["session_info"]] <- arguments[["session_info"]]
  }
  
  #write updated cache
  saveRDS(report_info, file = file.path(dashboard_dir, "site_page_cache.rds"))
  
}

get_report_cache <- function(dashboard_dir, id=NULL) {
  #Shane: would be good to abstract some of these variables to a config object passed into the function
  expect_cache <- file.path(dashboard_dir, "site_page_cache.rds")
  if (!checkmate::test_file_exists(expect_cache)) {
    #initialize object
    summary_null <- init_user_page_summary %>% slice(0) #generates a 0-row data.frame of the right structure
    df <- list(page_summary=summary_null, session_info=NULL) #other stuff here, like perhaps more detailed info
    saveRDS(df, file=expect_cache) #write blank cache to disk
  } else {
    df <- readRDS(expect_cache) 
  }
  
  # if (!is.null(id)) {
  #   df$page_summary <- df$page_summary %>% filter(id==id)
  # }
  
  return(df)
  
}

render_subject_compliance_reports <- function(slist, dashboard_dir, rerender_mins=10, force=FALSE) {
  checkmate::assert_list(slist)
  checkmate::assert_directory_exists(dashboard_dir)
  extant <- get_report_cache(dashboard_dir)$page_summary 
  
  for (ss in slist) {
    # look at whether the compliance report is older than the latest data
    # if data are newer than compliance_render_date, we need to regenerate the page
    extant_this_subj <- extant %>% filter(id==ss$id)
    render_subject <- FALSE
    
    if (nrow(extant_this_subj) > 1L) {
      print(extant_this_subj)
      stop("found more than one record for this subject in cache: ", expect_cache)
    } else if (isTRUE(force)) {
      render_subject <- TRUE
    } else if (nrow(extant_this_subj) == 1L) {
      compliance_date <- extant_this_subj %>% pull(compliance_render_date)
      data_date <- ss$sched_date
      if (difftime(compliance_date, data_date, units="mins") < -1*rerender_mins) {
        render_subject <- TRUE
      }
      
      #also try re-rendering automatically on previous error (Shane: could have more elegant error tracking/re-rendering scheme)
      if (extant_this_subj$compliance_page == "Rendering Error") { render_subject <- TRUE }
    } else if (nrow(extant_this_subj) == 0L) {
      render_subject <- TRUE
    }
    
    if (isTRUE(render_subject)) {
      #do the work
      outdir <- "behavioral_compliance" #could be handled by a config$compliance_dir approach
      outfile <- paste0(ss$id, ".html") #could be handled by a config$compliance_file approach
      result <- tryCatch(expr={
        rmarkdown::render("report_generators/render_behavioral_compliance.Rmd", params=list(id=ss$id), 
                        output_dir = file.path("rendered_site", outdir), output_file = outfile, #Shane: move 'rendered_site' to global config?
                        output_yaml = "_site.yml") #this doesn't preserve the navbar
        }, error=function(e) { warning("Error rendering behavioral compliance for: ", ss$id); return("Error") })
      
      if (result == "Error") { 
        compliance_page <- "Rendering Error"  #Shane: would be good to log these errors so that clicking would take the user to a log of the error.
      } else {
        compliance_page <- file.path(outdir, outfile)
      }

      insert_into_report_cache(dashboard_dir, id=ss$id, list(page_summary=list(compliance_page=compliance_page, compliance_render_date=Sys.time())))
    }
    
  }
  
}
