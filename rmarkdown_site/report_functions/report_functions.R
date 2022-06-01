get_subject_list <- function(data_dir) {
  jj <- rjson::fromJSON(file = file.path(data_dir, "subject_status.json"))$subjects
  active <- data.frame(id=jj$active, status="active")
  inactive <- data.frame(id=jj$inactive, status="inactive")
  rbind(active, inactive)
}

#helper function to render report in separate R session to avoid environment contamination
render_separately <- function(...) callr::r(
  function(...) rmarkdown::render(..., envir = parent.frame()), args = list(...), show = TRUE)

#general wrapper that implements all, unchecked, summaries 3-file approach
get_cleaned_data <- function(id, data_dir, what, quiet=FALSE) {
  ret_list <- list(all=NULL, unchecked=NULL, summaries=NULL)
  
  main_file <- unchecked_file <- summaries_file <- NULL
  if (what == "sleep") {
    main_file <- dashboard_file_check(id, data_dir, "sleep.rds", "sleep diary")
    unchecked_file <- dashboard_file_check(id, data_dir, "sleep_unchecked.rds", "unchecked sleep diary", signal="none")
    summaries_file <- dashboard_file_check(id, data_dir, "sleep_summaries.rds", "sleep summaries", signal=ifelse(isTRUE(quiet), "none", "warning")) # should exist
  } else if (what == "hr") {
    main_file <- dashboard_file_check(id, data_dir, "hr.rds", "heart rate")
    unchecked_file <- dashboard_file_check(id, data_dir, "hr_unchecked.rds", "unchecked heart rate", signal="none")
    summaries_file <- dashboard_file_check(id, data_dir, "hr_summaries.rds", "heart rate summaries", signal="none") #not used at present
  } else if (what=="eeg") {
    main_file <- dashboard_file_check(id, data_dir, "eeg.rds", "EEG")
    unchecked_file <- dashboard_file_check(id, data_dir, "eeg_unchecked.rds", "unchecked EEG", signal="none")
    summaries_file <- dashboard_file_check(id, data_dir, "eeg_summaries.rds", "EEG summaries", signal="none") #not used at present
  } else if (what == "task_performance") {
    main_file <- dashboard_file_check(id, data_dir, "performance.rds", "task performance")
    unchecked_file <- dashboard_file_check(id, data_dir, "performance_unchecked.rds", "unchecked task performance", signal="none")
    summaries_file <- dashboard_file_check(id, data_dir, "performance_summaries.rds", "task performance summaries", signal="none") #not used at present
  } else if (what == "task_compliance") {
    main_file <- dashboard_file_check(id, data_dir, "compliance.rds", "task compliance")
    unchecked_file <- dashboard_file_check(id, data_dir, "compliance_unchecked.rds", "unchecked task compliance", signal="none")
    summaries_file <- dashboard_file_check(id, data_dir, "compliance_summaries.rds", "task compliane summaries", signal="none") #not used at present
  } else if (what == "mood") {
    main_file <- dashboard_file_check(id, data_dir, "mood.rds", "mood diary")
    unchecked_file <- dashboard_file_check(id, data_dir, "mood_unchecked.rds", "unchecked mood diary", signal="none")
    summaries_file <- dashboard_file_check(id, data_dir, "mood_summaries.rds", "mood diary summaries", signal=ifelse(isTRUE(quiet), "none", "warning")) #should exist
  } else if (what == "overview") {
    main_file <- dashboard_file_check(id, data_dir, "overall.rds", "subject overview", signal=ifelse(isTRUE(quiet), "none", "error"))
  } else if (what == "completion") {
    main_file <- dashboard_file_check(id, data_dir, "completeness.rds", "completion")
  } else if (what == "payment") {
    main_file <- dashboard_file_check(id, data_dir, "payment.rds", "payment")
  } else {
    stop("Unclear what to load")
  }
  
  read_and_wrangle <- function(rds) {
    df <- readRDS(rds)
    #always make date and block upper case
    if ("date" %in% names(df)) { 
      df <- df %>% dplyr::rename(Date=date) 
      if (is.list(df$Date)) { df$Date <- unlist(df$Date) } #nested list problem (temporary)
    }
    if ("block" %in% names(df)) { df <- df %>% dplyr::rename(Block=block) }
    return(df)
  }
  
  if (!is.null(main_file)) ret_list[["all"]] <- read_and_wrangle(main_file)
  if (!is.null(unchecked_file)) ret_list[["unchecked"]] <- read_and_wrangle(unchecked_file)
  if (!is.null(summaries_file)) ret_list[["summaries"]] <- readRDS(summaries_file)
  
  return(ret_list)
}


#helper function to use default formatting across the subject dashboard report, while allowing user to override
dashboard_reactable <- function(...) {
  r_list <- list(...)
  r_names <- names(r_list)
  defaults <- list(
    theme = reactablefmtr::journal(),
    defaultColDef = colDef(class = "cell", headerClass = "header"),
    borderless = TRUE,
    defaultSorted = c("Date", "Block"),
    defaultSortOrder = "desc",
    fullWidth = FALSE
  )
  
  # fill in any default fields not already populated in input
  for (nn in names(defaults)) {
    if (!nn %in% r_names) r_list[[nn]] <- defaults[[nn]]
  }
  
  # TODO: add href tag here and write.csv data to destination
  if (isTRUE(dds$data_download_links)) {
    #path must be relative to output directory
    #p_base <- R.utils::getRelativePath(p_dest, relativeTo=s_base)
    
    #tags$a(href = , url)
  }
  
  do.call(reactable, r_list) #, envir=knitr::knit_global())
}

# helper function to check for existence of expected file in subjects reports
dashboard_file_check <- function(id, data_dir, file_name, file_desc, signal="error") {
  expect_file <- file.path(data_dir, "Subjects", id, "reports", file_name)
  if (!checkmate::test_file_exists(expect_file)) {
    if (signal=="error") {
      dashboard_error("Cannot find", file_desc, "file:", expect_file)  
    } else if (signal == "warning") {
      dashboard_warning("Cannot find", file_desc, "file:", expect_file)
    } else if (signal == "none") {
      dashboard_debug("No", file_desc, "file exists:", expect_file) #only prints when debug is on
    }
    expect_file <- NULL #set back to absent
  }
  return(expect_file)
}


#by default, $ is converted to a mathjax expression in pandoc.
#need to escape this
escape_msg <- function(msg) {
  gsub("$", "\\$", msg, fixed=TRUE)
}

#need to amend this for formatting
dashboard_message<- function(..., print=TRUE) {
  msg <- escape_msg(paste(...))
  tag <- htmltools::HTML(paste0("<p class='dashboard-message'><b>Message:</b> ", msg, "</p>"))
  if (isTRUE(print)) {
    cat(tag)
    return(invisible(NULL))
  } else {
    return(tag)
  }
}

#need to amend this for formatting
dashboard_warning <- function(..., print=TRUE) {
  msg <- escape_msg(paste(...))
  tag <- htmltools::HTML(paste0("<p class='dashboard-warning'><b>Warning:</b> ", msg, "</p>"))
  if (isTRUE(print)) {
    cat(tag)
    return(invisible(NULL))
  } else {
    return(tag)
  }
}

#need to amend this for formatting
dashboard_error <- function(..., print=TRUE) {
  msg <- escape_msg(paste(...))
  tag <- htmltools::HTML(paste0("<p class='dashboard-error'><b>Error:</b> ", msg, "</p>"))
  if (isTRUE(print)) {
    cat(tag)
    return(invisible(NULL))
  } else {
    return(tag)
  }
}

#need to amend this for formatting
dashboard_debug <- function(..., print=TRUE) {
  if (isTRUE(render_debug)) { #global var
    msg <- escape_msg(paste(...))
    tag <- htmltools::HTML(paste0("<p class='dashboard-debug'><b>Debug:</b> ", msg, "</p>"))
    if (isTRUE(print)) {
      cat(tag)
      return(invisible(NULL))
    } else {
      return(tag)
    }
  } else {
    return(invisible(NULL))
  }
}

#small helper function for formatting dates for display
dashboard_date <- function(d, in_func=anytime::anytime, out_fmt="%m/%d/%Y") {
  if (is.list(d)) d <- unlist(d) #some nested lists being passed
  dobj <- in_func(d)
  dobj %>% format(out_fmt)
}

# wrapper function to render a child Rmd document and print an HTML error if it fails
render_child <- function(rmd, section_name) {
  checkmate::assert_file_exists(rmd)
  res <- tryCatch(
    knitr::knit_child(rmd, quiet = TRUE),
    error=function(e) {
      h <- htmltools::div(
        class="section-fail",
        htmltools::h3(paste(rmd, "failed to knit")),
        dashboard_error(as.character(e))
      )
      return(as.character(h))
    }
  )
  cat(res, sep = '\n')
}

include_subject_figure <- function(fname, desc=NULL) {
  if (is.null(desc)) desc <- fname
  abs_loc <- file.path(p_dest, fname)
  rel_loc <- file.path(p_base, fname)
  if (file.exists(abs_loc)) {
    knitr::include_graphics(rel_loc)
  } else {
    dashboard_warning("No", desc, "graph at: ", abs_loc)
  }
}

get_all_overviews <- function(data_dir, quiet=TRUE) {
  slist <- get_subject_list(data_dir)
  overview_data <- do.call(rbind, lapply(slist$id, function(id) {
    tryCatch({
      df <- get_overview_data(id, data_dir, quiet=TRUE)$all
      return(df)
    }, error = function(e){
      print(paste0(id, ": ", e))
    })
  }))
  
  #populate missing subject overviews with NAs
  all_df <- slist %>% left_join(overview_data, by="id")
  return(list(
    active=all_df %>% 
      dplyr::filter(status=="active") %>% 
      dplyr::select(-status),
    inactive=all_df %>% 
      dplyr::filter(status=="inactive") %>%
      dplyr::select(-status)
  ))
}

## tabled function for getting stuff directly from proc schedule
# get_subject_compliance <- function(data_dir) {
#   sched_file <- file.path(data_dir, "output_schedule.Rdata")
#   checkmate::assert_file_exists(sched_file)
#   
#   proc_sched <- local(get(load(sched_file)))
#   
# }
# 
# x <- get_subject_compliance("/Users/hallquist/Downloads/subject_reports_09_15")
