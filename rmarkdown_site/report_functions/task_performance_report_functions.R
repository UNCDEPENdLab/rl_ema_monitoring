# generate reactable of task_performance data for display
render_task_performance_table <- function(task_performance_data, field=NULL) {
  checkmate::assert_list(task_performance_data)
  if (is.null(task_performance_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
  }
  
  #select fields to display
  to_render <- task_performance_data[[field]] %>%
    dplyr::select(Date, Block, abs_accurate_feed, relative_accuracy_feed, abs_accurate_nofeed, 
                  relative_accuracy_nofeed, mean_rt, IDe_bias, intox) #, c_earn, d_earn)
  
  
  #formatter for feedback columns
  feedback_fmt <- function(...) {
    colDef(
      html=TRUE,
      style=function(value) {
        if (is.na(value)) {
          list(background = dds$task_performance$missing_accuracy$background, color=dds$task_performance$missing_accuracy$background)
        } else if (value <= dds$task_performance$objective_feedback$bad$max) {
          list(background = dds$task_performance$objective_feedback$bad$background, color=dds$task_performance$objective_feedback$bad$text)
        } else if (value > dds$task_performance$objective_feedback$bad$max && value < dds$task_performance$objective_feedback$good$min) {
          list(background = dds$task_performance$objective_feedback$mediocre$background, color=dds$task_performance$objective_feedback$mediocre$text)
        } else { # value >= dds$task_performance$objective_feedback$good$min
          list(background = dds$task_performance$objective_feedback$good$background, color=dds$task_performance$objective_feedback$good$text)
        }
      },
      ...
    )
  }
  
  #formatter for no feedback columns
  no_feedback_fmt <- function(...) {
    colDef(
      html=TRUE,
      style=function(value) {
        if (is.na(value)) {
          # NAs for no feedback trials are treated as okay
          list(background = dds$task_performance$missing_accuracy$background, color=dds$task_performance$missing_accuracy$background)
        } else if (value <= dds$task_performance$no_feedback$bad$max) {
          list(background = dds$task_performance$no_feedback$bad$background, color=dds$task_performance$no_feedback$bad$text)
        } else if (value > dds$task_performance$no_feedback$bad$max && value < dds$task_performance$no_feedback$good$min) {
          list(background = dds$task_performance$no_feedback$mediocre$background, color=dds$task_performance$no_feedback$mediocre$text)
        } else { # value >= dds$task_performance$no_feedback$good$min
          list(background = dds$task_performance$no_feedback$good$background, color=dds$task_performance$no_feedback$good$text)
        }
      },
      ...
    )
  }
  
  tbl <- dashboard_reactable(
    data = to_render,
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      Block=colDef(style=list(fontWeight = "bold"), minWidth=70),
      abs_accurate_feed=feedback_fmt(name="Objective % correct (feed.)"),
      relative_accuracy_feed=feedback_fmt(name="Experienced % correct (feed.)"),
      abs_accurate_nofeed=no_feedback_fmt(name="Objective % correct (no feed.)"),
      relative_accuracy_nofeed=no_feedback_fmt(name="Experienced % correct (no feed.)"),
      mean_rt=colDef(name="RT (ms)", minWidth=70, style=function(value) {
        if (value < dds$task_performance$rt$bad$max) {
          list(background = dds$task_performance$rt$bad$background, color=dds$task_performance$rt$bad$text)
        } else {
          list(background = dds$task_performance$rt$good$background, color=dds$task_performance$rt$good$text)
        }
      }),
      IDe_bias=colDef(name="Left bias", minWidth=70, format=colFormat(suffix = "%"), style=function(value) {
        bias <- abs(value - 50)
        if (bias >= dds$task_performance$side_bias$bad$min) {
          list(background = dds$task_performance$side_bias$bad$background, color=dds$task_performance$side_bias$bad$text)
        } else {
          list(background = dds$task_performance$side_bias$good$background, color=dds$task_performance$side_bias$good$text)
        }
      }),
      intox=colDef(name="Intox?", minWidth=70, style=function(value) {
        if (value == "No") {
          list(background = dds$task_performance$intoxicated$no$background, color=dds$task_performance$intoxicated$no$text)
        } else {
          list(background = dds$task_performance$intoxicated$yes$background, color=dds$task_performance$intoxicated$yes$text)
        }
      })
      #,
      #c_earn=colDef(name="Cumulative earnings", format = colFormat(currency = "USD")), #format $
      #d_earn=colDef(name="Daily earnings", format = colFormat(currency = "USD")) #format $
    )
  )
  
  tbl
}

get_task_performance_data <- function(id, data_dir) {
  task_performance_data <- get_cleaned_data(id, data_dir, "task_performance")
  
  # task performance-specific transformations applied to both checked and unchecked
  wrangle_task_performance <- function(df) {
    df %>%    
      dplyr::mutate(
        Date=dashboard_date(Date),
      ) %>% group_by(Date) %>%
      mutate(d_earn=max(c_earn, na.rm=T) - min(c_earn, na.rm=T)) %>% ungroup() %>%
      arrange(desc(Date))
  }
  
  if (!is.null(task_performance_data$all)) {
    task_performance_data$all <- task_performance_data$all %>% wrangle_task_performance()
  } else {
    dashboard_warning("No task performance data found. task_performance_data$all is NULL in get_task_performance_data.")
  }
  
  if (!is.null(task_performance_data$unchecked)) {
    task_performance_data$unchecked <- task_performance_data$unchecked %>% wrangle_task_performance()
  }
  
  return(task_performance_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources
# 
# \newline
# ### Task performance {.tabset}
# #### Unchecked
# ```{r perf table un, echo=FALSE, warning=FALSE}
# #Task performance table
# if(nrow(task_unchecked) > 0){
#   kbl(task_unchecked) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, bold = T) %>%
#     column_spec(3, background=case_when(task_unchecked[,3]>80 ~ "#C6EFCE",task_unchecked[,3]>70 & task_unchecked[,3]<=80 ~ "#FFEB9C", task_unchecked[,3]<70 ~ "#ffc7ce")) %>%
#     column_spec(4, background=case_when(task_unchecked[,4]>80 ~ "#C6EFCE",task_unchecked[,4]>70 & task_unchecked[,4]<=80 ~ "#FFEB9C", task_unchecked[,4]<70 ~ "#ffc7ce")) %>%
#     column_spec(5, color=ifelse(task_unchecked[,5] > 55 | is.na(task_unchecked[,5]), "#D8D8D8", "black")) %>%
#     column_spec(6, color=ifelse(task_unchecked[,6] > 55 | is.na(task_unchecked[,6]), "#D8D8D8","black")) %>%
#     column_spec(7, color=ifelse(task_unchecked[,7] > 1400, "#E6E6E6","black")) %>%
#     column_spec(8, color=ifelse(task_unchecked[,8] >= 67 | task_unchecked[,8] < 67, "#D8D8D8","black")) %>%
#     column_spec(9, color=ifelse(task_unchecked[,9] == "No", "#D8D8D8","black"))
# }
# ```
# 
# #### All blocks
# ```{r perf table, echo=FALSE, warning=FALSE}
# if(nrow(task) > 0){
#   kbl(task) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, bold = T) %>%
#     column_spec(3, background=case_when(task[,3]>80 ~ "#C6EFCE",task[,3]>70 & task[,3]<=80 ~ "#FFEB9C", task[,3]<70 ~ "#ffc7ce")) %>%
#     column_spec(4, background=case_when(task[,4]>80 ~ "#C6EFCE",task[,4]>70 & task[,4]<=80 ~ "#FFEB9C", task[,4]<70 ~ "#ffc7ce")) %>%
#     column_spec(5, color=ifelse(task[,5] > 55 | is.na(task[,5]), "#D8D8D8", "black")) %>%
#     column_spec(6, color=ifelse(task[,6] > 55 | is.na(task[,6]), "#D8D8D8","black")) %>%
#     column_spec(7, color=ifelse(task[,7] > 1400, "#E6E6E6","black")) %>%
#     column_spec(8, color=ifelse(task[,8] >= 67 | task[,8] < 67, "#D8D8D8","black")) %>%
#     column_spec(9, color=ifelse(task[,9] == "No", "#D8D8D8","black"))
# }
# ```
# 
#
#task performance table wrangling
# task <- rename(perf,"Date"=date) #get rid of blocks not played
# task <- left_join(task,checklist, by="Date")
# task$Date <- lapply(task$Date,date_format) #reformat dates (these dates don't come from blk_dt)
# task <- arrange(task, -row_number()) #flip df so that most recent dates are first
# 
# #create cumulative earnings column
# task$c_earn <- c()
# task$c_earn[nrow(task)] <- task$earning[nrow(task)]
# for(i in (nrow(task)-1):1){
#   task$c_earn[i] <- task$earning[i] + task$c_earn[i+1]
# }
# 
# #TEMPORARY: adding in an intox response with a very early start time to make sure that all blocks match. This will not be necessary with real data, when there will be an intox response preceding every block.
# temp <- data.frame("time"=1504931510000,"answer"="Yes")
# output$proc_data[[params$id]]$raw_data$drugs_check <- rbind(output$proc_data[[params$id]]$raw_data$drugs_check,temp)
# 
# #create intoxication column
# raw_s_data <- output$proc_data[[params$id]]$raw_data$sessions
# block_times <- as.numeric(raw_s_data$start_time_ms)
# raw_s_data$intox <- lapply(block_times, get_intox_resp, id=params$id)
# intox_df <- select(raw_s_data, block, intox)
# 
# #join column to task df
# task <- left_join(task, intox_df, by="block")
# 
# task$abs_accurate_feed <- task$abs_accurate_feed*100
# task$relative_accuracy_feed <- task$relative_accuracy_feed*100
# task$relative_accuracy_nofeed <- task$relative_accuracy_nofeed*100
# task$abs_accurate_nofeed <- task$abs_accurate_nofeed*100
# task$IDe_bias <- task$IDe_bias*100
# task$mean_rt <- task$mean_rt*1000
# 
# task[,c(3:8)] <- round(task[,c(3:8)]) #round percentages to nearest whole percent
# 
# 
# rownames(task) <- NULL
# 
# task_unchecked <- filter(task,`Checklist Complete?`=="No")
# task_unchecked <- transmute(task_unchecked, "Date"=Date, "Block"=block, "Objective % correct (feed.)"=abs_accurate_feed, "Experienced % correct (feed.)"=relative_accuracy_feed, "Objective % correct (no feed.)"=abs_accurate_nofeed, "Experienced % correct (no feed.)"=relative_accuracy_nofeed, "RT(ms)"=mean_rt, "Left %"=IDe_bias, "Intoxicated?"=intox, "Cumulative earnings"=c_earn)
# task <- transmute(task, "Date"=Date, "Block"=block, "Objective % correct (feed.)"=abs_accurate_feed, "Experienced % correct (feed.)"=relative_accuracy_feed, "Objective % correct (no feed.)"=abs_accurate_nofeed, "Experienced % correct (no feed.)"=relative_accuracy_nofeed, "RT(ms)"=mean_rt, "Left %"=IDe_bias, "Intoxicated?" = intox, "Cumulative earnings"=c_earn)
