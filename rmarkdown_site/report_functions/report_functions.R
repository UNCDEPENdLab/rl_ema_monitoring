#general wrapper that implements all, unchecked, summaries 3-file approach
get_cleaned_data <- function(id, data_dir, what) {
  ret_list <- list(all=NULL, unchecked=NULL, summaries=NULL)
  
  main_file <- unchecked_file <- summaries_file <- NULL
  if (what == "sleep") {
    main_file <- dashboard_file_check(id, data_dir, "sleep.rds", "sleep diary")
    unchecked_file <- dashboard_file_check(id, data_dir, "sleep_unchecked.rds", "unchecked sleep diary", signal="none")
    summaries_file <- dashboard_file_check(id, data_dir, "sleep_summaries.rds", "sleep summaries", signal="warning") # should exist
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
    summaries_file <- dashboard_file_check(id, data_dir, "mood_summaries.rds", "mood diary summaries", signal="warning") #should exist
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


#need to amend this for formatting
dashboard_message<- function(...) {
  msg <- paste(...)
  htmltools::HTML(paste0("<p class='dashboard-message'><b>Message:</b> ", msg, "</p>"))
}

#need to amend this for formatting
dashboard_warning <- function(...) {
  msg <- paste(...)
  htmltools::HTML(paste0("<p class='dashboard-warning'><b>Warning:</b> ", msg, "</p>"))
}

#need to amend this for formatting
dashboard_error <- function(...) {
  msg <- paste(...)
  htmltools::HTML(paste0("<p class='dashboard-error'><b>Error:</b> ", msg, "</p>"))
}

#need to amend this for formatting
dashboard_debug <- function(...) {
  if (isTRUE(render_debug)) { #global var
    msg <- paste(...)
    htmltools::HTML(paste0("<p class='dashboard-debug'><b>Debug:</b> ", msg, "</p>"))
  }
}

#small helper function for formatting dates for display
dashboard_date <- function(d, in_func=anytime::anydate, out_fmt="%m/%d/%Y") {
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
