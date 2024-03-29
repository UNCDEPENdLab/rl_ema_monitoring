# generate reactable of sleep data for display
render_sleep_table <- function(sleep_data, field=NULL) {
  checkmate::assert_list(sleep_data)
  if (is.null(sleep_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
  }
  
  if (is.null(sleep_data$summaries$sleep_dist)) {
    dashboard_warning(
      "No sleep_dist variable found in sleep_data$summaries$sleep_dist.",
      "Defaulting to 0 (all black text)."
    )
    sleep_data$summaries$sleep_dist <- 0
  }
  
  sleep_dist <- sleep_data$summaries$sleep_dist
  to_render <- sleep_data[[field]]
  
  #TODO: Need $Sleep field, which may come from RedCAP (see sleep.R)
  gray_fmt <- function(...) {
    colDef(
      style=function() {
        if (sleep_dist > dds$sleep$sleep_dist_gray_gt_x) {
          list(color = "#D8D8D8") #gray out values where sleep_dist is > 50
        } else {
          list(color = "black")
        }
      },
      ...
    )
  }
  
  sleep_details <- function(index) {
    diary_data <- to_render[index,,drop=FALSE]

    if(!"number_of_events" %in% names(diary_data)) {
      has_diary <- FALSE
    } else {
      has_diary <- diary_data$number_of_events > 0
    }
    if (isFALSE(has_diary)) { return(NULL) } #no details
    
    detail <- htmltools::div(
      class = "sleep-detail",
      reactable(
        data = diary_data %>% 
          dplyr::select(category, description, Good_Bad, Physical_Pleasure_Physical_Pain, 
                        Loved_Lonely, Powerful_Weak, Safe_Threatened),
        columns=list(
          category = colDef(name="Category", width=125),
          description = colDef(name="Description", width=225),
          #time_ago = colDef(name="Min. ago"), #not present in currnt data
          Good_Bad = colDef(name="Good-Bad"),
          Physical_Pleasure_Physical_Pain = colDef(name="Pleasure-Pain"),
          Loved_Lonely = colDef(name="Loved-Lonely"),
          Powerful_Weak = colDef(name="Powerful-Weak"),
          Safe_Threatened = colDef(name="Safe-Threatened")  
        ),
        fullWidth=TRUE,
        defaultColDef = colDef(minWidth = 50),
        outlined=TRUE)
    )
    
    detail
  }
  
  tbl <- dashboard_reactable(
    data=to_render %>%
      dplyr::select(Date, did_not_sleep, sleep_latency, woke_many_times, woke_early, overall),
    columns=list(
      did_not_sleep = gray_fmt(name="Didn't sleep"),
      sleep_latency = gray_fmt(name="Sleep latency"),
      woke_many_times = gray_fmt(name="Woke many times"),
      woke_early = gray_fmt(name="Woke early"),
      overall = gray_fmt(name="Overall")
    ),
    defaultSorted = c("Date"),
    details=sleep_details
  )
  
  tbl
}

get_sleep_data <- function(id, data_dir) {
  sleep_data <- get_cleaned_data(id, data_dir, "sleep")
  
  # sleep-specific transformations applied to both checked and unchecked
  wrangle_sleep <- function(df) {
    df %>%    
      mutate(
        Date=dashboard_date(Date),
        #insomnia_ratio = cumsum(did_not_sleep)/n(), #this is the running ratio
        insomnia_ratio = sum(did_not_sleep)/n(),
        did_not_sleep = factor(did_not_sleep, levels=c(TRUE, FALSE), labels=c("Yes", "No"))
      ) %>%
      arrange(desc(Date))
  }
  
  if (!is.null(sleep_data$all)) {
    sleep_data$all <- sleep_data$all %>% wrangle_sleep()
  } else {
    dashboard_warning("No sleep data found. sleep_data$all is NULL in get_sleep_data")
  }
  
  #TODO: come back and use _summaries.rds once that's regenerated within master
  #insomnia_ratio <- sum(sleep$did_not_sleep)/nrow(sleep) #not entirely sure what the objective is here
  
  if (!is.null(sleep_data$unchecked)) {
    sleep_data$unchecked <- sleep_data$unchecked %>% wrangle_sleep()
  }
  
  return(sleep_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources

## Wrangling code

#looks like there will be a file with suffix _unchecked if there are unchecked records?
#merge against redcap, expect checked == "Yes" or "No" (or NA)
#sleep <- sleep %>% left_join(redcap_sleep, by="id")
#sleep$checked <- "No"

#sleep_checked <- sleep %>% filter(checked=="Yes")
#sleep_unchecked <- sleep %>% filter(checked=="No")


#Sleep diary wrangling for display
#sleep$answer_time <- str_extract(sleep$answer_time,"\\d{4}-\\d{2}-\\d{2}") #chop off the hour from the answer time
#sleep <- rename(sleep,"Date"=answer_time)
#sleep <- left_join(sleep,checklist, by="Date") #TODO: REDCAP
#sleep$Date <- lapply(sleep$Date,date_format) #format appropriately

#insomnia_ratio <- output$sample_form_summary$num_no_sleep/nrow(sleep) #this will be used to determine the color of the font in the "Didn't sleep" column

#diary_check <- output$redcap %>% filter(ID==params$id) %>% select(`Sleep`,`Sleep Notes`,Date)
#diary_check$Date <- as.character(diary_check$Date)
#sleep$Date <- as.character(sleep$Date)
#sleep <- left_join(sleep, diary_check, by="Date")#adding RA check columns

#sleep <- arrange(sleep, -row_number()) #make the most recent block come first
#this will be used to determine the color of the font of the sleep quality ratings
#sl_di_avg_row <- filter(output$sample_form_summary, ID==params$id)
#sleep_dist <- sl_di_avg_row$sleep_di_avg

#diary_unchecked <- filter(sleep,`Checklist Complete?`=="No")
# diary_unchecked <- transmute(diary_unchecked, "Date"=Date, "Problems"=Sleep, 
#                              "Notes"=`Sleep Notes`, "Didn't sleep"=did_not_sleep, "Sleep latency"=sleep_latency,
#                              "Woke many times"=woke_many_times, "Woke early"=woke_early, "Overall"=overall)
# 
# sleep <- transmute(sleep, "Date"=Date,"Problems"=Sleep, "Notes"=`Sleep Notes`, "Didn't sleep"=did_not_sleep, "Sleep latency"=sleep_latency, "Woke many times"=woke_many_times, "Woke early"=woke_early, "Overall"=overall)
# 
# checked <- sleep %>%

# ```{r sleep, include=FALSE}
# #Sleep diary table wrangling
# diary <- proc_sched$form_dfs$`Sleep Diary`
# diary$answer_time <- str_extract(diary$answer_time,"\\d{4}-\\d{2}-\\d{2}") #chop off the hour from the answer time
# diary <- rename(diary,"Date"=answer_time)
# diary <- left_join(diary,checklist, by="Date")
# diary$Date <- lapply(diary$Date,date_format) #format appropriately
# insomnia_ratio <- output$sample_form_summary$num_no_sleep/nrow(diary) #this will be used to determine the color of the font in the "Didn't sleep" column
# 
# diary_check <- output$redcap %>% filter(ID==params$id) %>% select(`Sleep`,`Sleep Notes`,Date)
# diary_check$Date <- as.character(diary_check$Date)
# diary$Date <- as.character(diary$Date)
# diary <- left_join(diary, diary_check, by="Date")#adding RA check columns
# 
# diary <- arrange(diary, -row_number()) #make the most recent block come first
# #this will be used to determine the color of the font of the sleep quality ratings
# sl_di_avg_row <- filter(output$sample_form_summary, ID==params$id)
# sleep_dist <- sl_di_avg_row$sleep_di_avg
# 
# diary_unchecked <- filter(diary,`Checklist Complete?`=="No")
# diary_unchecked <- transmute(diary_unchecked, "Date"=Date, "Problems"=Sleep, "Notes"=`Sleep Notes`, "Didn't sleep"=did_not_sleep, "Sleep latency"=sleep_latency, "Woke many times"=woke_many_times, "Woke early"=woke_early, "Overall"=overall)
# diary <- transmute(diary, "Date"=Date,"Problems"=Sleep, "Notes"=`Sleep Notes`, "Didn't sleep"=did_not_sleep, "Sleep latency"=sleep_latency, "Woke many times"=woke_many_times, "Woke early"=woke_early, "Overall"=overall)
# ```


# kbl(sleep_data$unchecked) %>%
#   kable_styling(fixed_thead = T) %>%
#   column_spec(1, bold = T) %>%
#   column_spec(2, background=if_else(is.na(sleep_data$unchecked[2]), "#C6EFCE", "#ffc7ce")) %>%
#   column_spec(3, background=if_else(is.na(sleep_data$unchecked[3]), "#C6EFCE", "#ffc7ce")) %>%
#   column_spec(4, color=if_else(sleep_data$summaries$insomnia_ratio < .2, "#D8D8D8", "black")) %>%
#   column_spec(5, color=if_else(sleep_data$summaries$sleep_dist > 50, "#D8D8D8", "black")) %>%
#   column_spec(6, color=if_else(sleep_data$summaries$sleep_dist > 50, "#D8D8D8", "black")) %>%
#   column_spec(7, color=if_else(sleep_data$summaries$sleep_dist > 50, "#D8D8D8", "black")) %>%
#   column_spec(8, color=if_else(sleep_data$summaries$sleep_dist > 50, "#D8D8D8", "black"))
