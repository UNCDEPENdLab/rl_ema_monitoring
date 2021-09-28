# generate reactable of task_compliance data for display
render_task_compliance_table_long <- function(task_compliance_data, field=NULL) {
  checkmate::assert_list(task_compliance_data)
  if (is.null(task_compliance_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
  }
  
  #select fields to display
  #to_render <- task_compliance_data[[field]] %>%
  #  dplyr::select(scheduled_time, delay, type, is_missing, delayednotmissing)

  to_render <- task_compliance_data[[field]]  

  #formatter for delay columns
  delay_fmt <- function(...) {
    colDef(
      style=function(value, index) {
        this_delay <- to_render$delayednotmissing[index]
        report_type <- to_render$type[index]
        
        #exempt sleep diaries from delay highlighting if the diary is scheduled between 1am and 5am
        sleep_okay <- FALSE
        if (report_type == "Sleep") {
          this_time <- to_render$scheduled_posixct[index]
          day_start <- lubridate::floor_date(this_time, "day") #12am
          early_block <- day_start + dhours(1) # should be 1am
          late_block <- day_start + dhours(5) # should be 5am
          sleep_okay <- this_time > early_block & this_time < late_block
        }
          
        if (isTRUE(sleep_okay)) { # exemption
          list(background = dds$task_compliance$delay$good$background, color=dds$task_compliance$delay$good$text)
        } else if (is.na(this_delay)) {
          list(background = dds$task_compliance$delay$missing$background, color=dds$task_compliance$delay$missing$text)
        } else if (this_delay < dds$task_compliance$delay$good$max) {
          list(background = dds$task_compliance$delay$good$background, color=dds$task_compliance$delay$good$text)
        } else if (this_delay >= dds$task_compliance$delay$good$max && this_delay <= dds$task_compliance$delay$bad$min) {
          list(background = dds$task_compliance$delay$mediocre$background, color=dds$task_compliance$delay$mediocre$text)
        } else { # this_delay > dds$task_compliance$delay$bad$min
          list(background = dds$task_compliance$delay$bad$background, color=dds$task_compliance$delay$bad$text)
        }
      },
      ...
    )
  }
  
  tbl <- dashboard_reactable(
    data = to_render,
    columns=list(
      # scheduled_time=colDef(name="Scheduled"),
      # start_time=colDef(name="Started"),
      # completed_time=colDef(name="Completed"),
      scheduled_posixct=colDef(show=FALSE), #hidden column only used for delay formatting
      scheduled_time=delay_fmt(name="Scheduled"),
      start_time=delay_fmt(name="Started"),
      completed_time=delay_fmt(name="Completed"),
      delayednotmissing=delay_fmt(name="Delay (min)", format=colFormat(digits=2)),
      is_missing=colDef(name="Missing")
    ),
    defaultSorted="scheduled_time",
    defaultSortOrder="asc",
    defaultColDef = colDef(minWidth = 60),
    fullWidth=TRUE,
    theme = reactablefmtr::journal(font_size = 12, header_font_size = 12)
  )
  
  tbl
}

get_task_compliance_data_long <- function(id, data_dir) {
  task_compliance_data <- get_cleaned_data(id, data_dir, "task_compliance")
  n_miss <- sum(is.na(task_compliance_data$all$scheduled_time))
  if (n_miss > 0) {
    dashboard_warning(
      "The task compliance RDS object contains", n_miss, 
      "missing dates. These rows will be dropped before proceeding!"
    )
  }
  
  # task compliance-specific transformations applied to both checked and unchecked
  wrangle_task_compliance <- function(df) {
    long_dt <- df %>%
      #dplyr::rename(Date=scheduled_time) %>%
      dplyr::mutate(
        scheduled_posixct=scheduled_time,
        scheduled_time=dashboard_date(scheduled_time, out_fmt="%m/%d/%Y %I:%M %p"),
        start_time=dashboard_date(start_time, out_fmt="%m/%d/%Y %I:%M %p"),
        completed_time=dashboard_date(completed_time, out_fmt="%m/%d/%Y %I:%M %p"),
        type=dplyr::recode(type, trials="Games", "Mood Questionnaire"="Mood", "Sleep Diary"="Sleep", 
                           "Daily recording"="Video", "5m Resting State"="5 min Resting", "End questionnaire"="End Qs")
      ) %>%
      dplyr::select(scheduled_posixct, scheduled_time, start_time, completed_time, type, is_missing, delayednotmissing) %>%
      arrange(scheduled_time)

    if (any(long_dt$type == "End Qs")) {
      which_end <- which(long_dt$type == "End Qs")
      if (length(which_end) > 1L) {
        dashboard_warning("Cannot filter out fields beyond end questionnaires because there are", length(which_end), "records")
      } else {
        completed_experiment <- long_dt$completed_time[which_end]
        n_future <- which(long_dt$scheduled_time > completed_experiment)
        dashboard_debug("There are", n_future, "records in compliance that occur after the experiment was complete.")
        long_dt <- long_dt %>%
          dplyr::filter(scheduled_time < !!completed_experiment)
      }
    }
    return(long_dt)
  }

  if (!is.null(task_compliance_data$all)) {
    task_compliance_data$all <- task_compliance_data$all %>%
      dplyr::filter(!is.na(scheduled_time)) %>%
      wrangle_task_compliance()
  } else {
    dashboard_warning("No task compliance data found. task_compliance_data$all is NULL in get_task_compliance_data.")
  }

  if (!is.null(task_compliance_data$unchecked)) {
    n_miss <- sum(is.na(task_compliance_data$unchecked$scheduled_time))
    if (n_miss > 0) {
      dashboard_warning(
        "The unchecked task compliance RDS object contains", n_miss,
        "missing dates. These rows will be dropped before proceeding!"
      )
    }
    
    task_compliance_data$unchecked <- task_compliance_data$unchecked %>% 
      dplyr::filter(!is.na(scheduled_time)) %>%
      wrangle_task_compliance()
  }

  return(task_compliance_data)
}

####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources

# ```{r compliance, include=FALSE}
# compliance <-info[c("scheduled_time","delay","type", "duration")]
# compliance$scheduled_time <- as.Date(compliance$scheduled_time)
# compliance$delay <- round(as.numeric(compliance$delay), digits = 0)
# compliance$s_type[compliance$type=="trials"] <- "Behavioral Game"
# compliance$s_type[compliance$type=="questionnaires"] <- sapply(info$spec[compliance$type=="questionnaires"],`[[`,3)
# compliance$delay <- round(compliance$delay)
# 
# #filters out NA/incomplete/upcoming data
# compliance_filtered <- compliance %>% filter(!is.na(s_type))
# 
# 
# compliance_filtered$is_missing <- is.na(compliance_filtered$duration)|compliance_filtered$delay>=1440
# compliance_filtered$delayednotmissing <- ifelse(compliance_filtered$is_missing, NA, compliance_filtered$delay)
# 
# compliance_addon <- compliance_filtered[compliance_filtered$is_missing, ]
# 
# compliance_addon$days_delayed <- floor(compliance_addon$delay/1440)
# 
# compliance_addon$scheduled_time <- compliance_addon$scheduled_time + (compliance_addon$days_delayed)
# 
# compliance_addon$delayednotmissing <- compliance_addon$delay - (compliance_addon$days_delayed * 1440)
# 
# compliance_addon$days_delayed <- NULL
# compliance_addon$is_missing <- FALSE
# compliance_filtered_2 <- rbind(compliance_filtered, compliance_addon)
# 
# #line 164 (call redcap_pull via Rdata file) will need to be redone once we know how redcap integration works (with multiple subjects)
# 
# 
# ema_checklist <- rc[c("Date", "Checklist Complete?")] 
# 
# compliance_filtered_2$checklist <- ema_checklist$`Checklist Complete?`[match(compliance_filtered_2$scheduled_time, ema_checklist$Date)]
# print(compliance_filtered_2$checklist)
# 
# compliance_filtered_2<- filter(compliance_filtered_2, scheduled_time < "2020-12-11" )
# 
# compliance_unchecked <- compliance_filtered_2[compliance_filtered_2$checklist != "Yes"| is.na(compliance_filtered_2$checklist), ]
# 
# #pivot tables
# 
# 
# library("pivottabler")
# compliance_pf <- PivotTable$new()
# compliance_pf$addData(compliance_filtered_2)
# compliance_pf$addColumnDataGroups("s_type", addTotal=FALSE)
# compliance_pf$addRowDataGroups("scheduled_time", addTotal=FALSE)
# compliance_pf$defineCalculation(calculationName = "delayednotmissing", summariseExpression = "ifelse(is.na(mean(as.numeric(delayednotmissing), na.rm=TRUE)), 0, mean(as.numeric(delayednotmissing), na.rm=TRUE))", caption = "avg delay")
# compliance_pf$defineCalculation(calculationName = "is_missing", summariseExpression = "length(which(is_missing))", caption = "number missing")
# compliance_pf$evaluatePivot()
# 
# compliance_pun <- PivotTable$new()
# compliance_pun$addData(compliance_unchecked)
# compliance_pun$addColumnDataGroups("s_type", addTotal=FALSE)
# compliance_pun$addRowDataGroups("scheduled_time", addTotal=FALSE)
# compliance_pun$defineCalculation(calculationName = "delayednotmissing", summariseExpression = "ifelse(is.na(mean(as.numeric(delayednotmissing), na.rm=TRUE)), 0, mean(as.numeric(delayednotmissing), na.rm=TRUE))", caption = "avg delay")
# compliance_pun$defineCalculation(calculationName = "is_missing", summariseExpression = "length(which(is_missing))", caption = "number missing")
# compliance_pun$evaluatePivot()
# 
# cells <- compliance_pun$findCells(minValue=121, maxValue=240, includeNull=FALSE, includeNA=FALSE)
# compliance_pun$setStyling(cells=cells, declarations=list("background-color"="#FFEB9C", "color"="#9C5700"))
# 
# # apply the red style for an average delay of 4 hours or greater
# cells <- compliance_pun$findCells(minValue=241, includeNull=FALSE, includeNA=FALSE)
# compliance_pf$setStyling(cells=cells, declarations=list("background-color"="#FFC7CE", "color"="#9C0006"))
# 
# # apply the green style for an average delay of between 0 and 2 hours
# #cells <- compliance_pf$findCells(minValue=0, maxValue=120, includeNull=FALSE, includeNA=FALSE)
# #compliance_pf$setStyling(cells=cells, declarations=list("background-color"="#C6EFCE", "color"="#006100"))
# 
# # apply the yellow style for an average delay of between 2 and 4 hours
# cells <- compliance_pf$findCells(minValue=121, maxValue=240, includeNull=FALSE, includeNA=FALSE)
# compliance_pf$setStyling(cells=cells, declarations=list("background-color"="#FFEB9C", "color"="#9C5700"))
# 
# # apply the red style for an average delay of 4 hours or greater
# cells <- compliance_pf$findCells(minValue=241, includeNull=FALSE, includeNA=FALSE)
# compliance_pf$setStyling(cells=cells, declarations=list("background-color"="#FFC7CE", "color"="#9C0006"))
# ```

### Task Compliance {.tabset}

#### Unchecked
# ```{r compliance table, echo=FALSE}
# compliance_pun$renderPivot()
# ```
# 
# #### All 
# ```{r compliance full, echo=FALSE}
# compliance_pf$renderPivot()
# ```
