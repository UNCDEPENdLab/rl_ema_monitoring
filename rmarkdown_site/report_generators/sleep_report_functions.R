# generate reactable of sleep data for display
render_sleep_table <- function(sleep_data, field=NULL) {
  stopifnot(field %in% names(sleep_data))
  if (is.null(sleep_data[[field]])) {
    dashboard_message("Nothing to display!")
    return(invisible(NULL))
  }
  
  if (is.null(sleep_data$summaries$sleep_dist)) {
    dashboard_warning(
      "No sleep_dist variable found in sleep_data$summaries$sleep_dist.",
      "Defaulting to 0 (all black text)."
    )
    sleep_data$summaries$sleep_dist <- 0
  }
  
  sleep_dist <- sleep_data$summaries$sleep_dist
  to_render <- sleep_data[[field]] %>%
    dplyr::select(Date, did_not_sleep, sleep_latency, woke_many_times, woke_early, overall)
  
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
  
  tbl <- reactable(
    data=to_render,
    defaultColDef = colDef(class = "cell", headerClass = "header"),
    columns=list(
      did_not_sleep = gray_fmt(name="Didn't sleep"),
      sleep_latency = gray_fmt(name="Sleep latency"),
      woke_many_times = gray_fmt(name="Woke many times"),
      woke_early = gray_fmt(name="Woke early"),
      overall = gray_fmt(name="Overall")
    ),
    borderless = TRUE,
    defaultSorted = "Date",
    defaultSortOrder = "desc",
    fullWidth = FALSE
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
      ) %>%
      arrange(desc(Date))
  }
  
  sleep_data$all <- sleep_data$all %>% wrangle_sleep()
  
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