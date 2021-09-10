# generate reactable of task_performance data for display
render_task_performance_table <- function(task_performance_data, field=NULL) {
  stopifnot(field %in% names(task_performance_data))
  if (is.null(task_performance_data[[field]])) {
    dashboard_message("Nothing to display!")
    return(invisible(NULL))
  }
  
  
  to_render <- task_performance_data[[field]] %>%
    dplyr::select(Date, block, abs_accurate_feed, relative_accuracy_feed, abs_accurate_nofeed, 
                  relative_accuracy_nofeed, mean_rt, IDe_bias, intox, c_earn)
  
  
  #formatter for feedback columns
  feedback_fmt <- function(...) {
    colDef(
      style=function(value) {
        if (value < dds$task_performance$objective_feedback$bad$max) {
          list(background = dds$task_performance$objective_feedback$bad$background, color=dds$task_performance$objective_feedback$bad$text)
        } else if (value >= dds$task_performance$objective_feedback$bad$max && value <= dds$task_performance$objective_feedback$good$min) {
          list(background = dds$task_performance$objective_feedback$mediocre$background, color=dds$task_performance$objective_feedback$mediocre$text)
        } else { # value > dds$task_performance$objective_feedback$good$min
          list(background = dds$task_performance$objective_feedback$good$background, color=dds$task_performance$objective_feedback$good$text)
        }
      },
      ...
    )
  }
  
  #formatter for no feedback columns
  no_feedback_fmt <- function(...) {
    colDef(
      style=function(value) {
        # NAs for no feedback trials are treated as okay
        if (is.na(value) || value < dds$task_performance$no_feedback$bad$max) {
          list(background = dds$task_performance$no_feedback$bad$background, color=dds$task_performance$no_feedback$bad$text)
        } else { # value >= dds$task_performance$no_feedback$bad$max
          list(background = dds$task_performance$no_feedback$good$background, color=dds$task_performance$no_feedback$good$text)
        }
      },
      ...
    )
  }
  

  
  #     column_spec(1, bold = T) %>%
  #     column_spec(2, bold = T) %>%
  #     column_spec(3, background=case_when(task_unchecked[,3]>80 ~ "#C6EFCE",task_unchecked[,3]>70 & task_unchecked[,3]<=80 ~ "#FFEB9C", task_unchecked[,3]<70 ~ "#ffc7ce")) %>%
  #     column_spec(4, background=case_when(task_unchecked[,4]>80 ~ "#C6EFCE",task_unchecked[,4]>70 & task_unchecked[,4]<=80 ~ "#FFEB9C", task_unchecked[,4]<70 ~ "#ffc7ce")) %>%
  #     column_spec(5, color=ifelse(task_unchecked[,5] > 55 | is.na(task_unchecked[,5]), "#D8D8D8", "black")) %>%
  #     column_spec(6, color=ifelse(task_unchecked[,6] > 55 | is.na(task_unchecked[,6]), "#D8D8D8","black")) %>%
  #     column_spec(7, color=ifelse(task_unchecked[,7] > 1400, "#E6E6E6","black")) %>%
  #     column_spec(8, color=ifelse(task_unchecked[,8] >= 67 | task_unchecked[,8] < 67, "#D8D8D8","black")) %>%
  #     column_spec(9, color=ifelse(task_unchecked[,9] == "No", "#D8D8D8","black"))
  
  
  tbl <- reactable(
    data = to_render,
    theme = journal(),
    defaultColDef = colDef(class = "cell", headerClass = "header"),
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      block=colDef(name="Block", style=list(fontWeight = "bold")),
      abs_accurate_feed=feedback_fmt(name="Objective % correct (feed.)"),
      relative_accuracy_feed=feedback_fmt(name="Experienced % correct (feed.)"),
      abs_accurate_nofeed=no_feedback_fmt(name="Objective % correct (no feed.)"),
      relative_accuracy_nofeed=no_feedback_fmt(name="Experienced % correct (no feed.)"),
      mean_rt=colDef(name="RT (ms)", style=function(value) {
        if (value < dds$task_performance$rt$bad$max) {
          list(background = dds$task_performance$rt$bad$background, color=dds$task_performance$rt$bad$text)
        } else {
          list(background = dds$task_performance$rt$good$background, color=dds$task_performance$rt$good$text)
        }
      }),
      IDe_bias=colDef(name="Left bias", format=colFormat(suffix = "%"), style=function(value) {
        bias <- abs(value - 50)
        if (bias > dds$task_performance$side_bias$bad$min) {
          list(background = dds$task_performance$side_bias$bad$background, color=dds$task_performance$side_bias$bad$text)
        } else {
          list(background = dds$task_performance$side_bias$good$background, color=dds$task_performance$side_bias$good$text)
        }
      }),
      intox=colDef(name="Intoxicated?", style=function(value) {
        if (value == "No") {
          list(background = dds$task_performance$intoxicated$no$background, color=dds$task_performance$intoxicated$no$text)
        } else {
          list(background = dds$task_performance$intoxicated$yes$background, color=dds$task_performance$intoxicated$yes$text)
        }
      }),
      c_earn=colDef(name="Cumulative earnings", format = colFormat(currency = "USD")) #format $
    ),
    borderless = TRUE,
    defaultSorted = c("Date", "block"),
    defaultSortOrder = "desc",
    fullWidth = FALSE
  )
  
  tbl
}

get_task_performance_data <- function(id, data_dir) {
  task_performance_data <- get_cleaned_data(id, data_dir, "task_performance")
  
  # task performance-specific transformations applied to both checked and unchecked
  wrangle_task_performance <- function(df) {
    df %>%    
      mutate(
        Date=dashboard_date(Date),
      ) %>%
      arrange(desc(Date))
  }
  
  task_performance_data$all <- task_performance_data$all %>% wrangle_task_performance()
  
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
