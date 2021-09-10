# generate reactable of mood data for display
render_mood_table <- function(mood_data, field=NULL) {
  stopifnot(field %in% names(mood_data))
  if (is.null(mood_data[[field]])) {
    dashboard_message("Nothing to display!")
    return(invisible(NULL))
  }
  
  to_render <- mood_data[[field]] %>%
    dplyr::select(Date, number_of_events, Valence, Arousal, 
                  Anxious, Elated, Sad, Irritable, Energetic)

  #"Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events,
  # mood_unchecked <- transmute(mood_unchecked, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)
  
  
  #kbl(mood_unchecked) %>%
    #     kable_styling(fixed_thead = T) %>%
    #     column_spec(1, bold = T) %>%
    #     column_spec(2, background=if_else(is.na(mood_unchecked[2]), "#C6EFCE", "#ffc7ce")) %>%
    #     column_spec(3, background=if_else(is.na(mood_unchecked[3]), "#C6EFCE", "#ffc7ce")) %>%
    #     column_spec(4, color="#D8D8D8") %>%
    #     column_spec(5, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
    #     column_spec(6, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
    #     column_spec(7, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
    #     column_spec(8, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
    #     column_spec(9, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
    #     column_spec(10, color=if_else(val_avg > 50, "#D8D8D8", "black"))
  # }
  
  #not clear why val_avg weas being used above to gray out all columns -- waiting on this
  tbl <- dashboard_reactable(
    data = to_render,
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      number_of_events=colDef(name="Number of events")
    ),
    defaultSorted="Date"
  )
  
  tbl
}

get_mood_data <- function(id, data_dir) {
  mood_data <- get_cleaned_data(id, data_dir, "mood")
  
  # mood-specific transformations applied to both checked and unchecked
  wrangle_mood <- function(df) {
    df %>%    
      mutate(
        Date=dashboard_date(Date),
      ) %>%
      arrange(desc(Date))
  }
  
  mood_data$all <- mood_data$all %>% wrangle_mood()
  
  if (!is.null(mood_data$unchecked)) {
    mood_data$unchecked <- mood_data$unchecked %>% wrangle_mood()
  }
  
  return(mood_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources

#Mood report table wrangling
# mood <- proc_sched$form_dfs$`Mood Questionnaire`
# mood$answer_time <- str_extract(mood$answer_time,"\\d{4}-\\d{2}-\\d{2}") #chop off the hour from the answer time
# mood <- rename(mood,"Date"=answer_time)
# mood$Date <- as.character(mood$Date)
# mood <- left_join(mood,checklist, by="Date")
# mood$Date <- lapply(mood$Date,date_format) #format appropriately
# 
# mood_check <- output$redcap %>% filter(ID==params$id) %>% select(`Mood`,`Mood Notes`,Date)
# mood_check$Date <- as.character(mood_check$Date)
# mood$Date <- as.character(mood$Date)
# 
# mood <- left_join(mood, mood_check, by="Date")#adding RA check column
# mood <- arrange(mood, -row_number()) #make the most recent block come first
# 
# #this will be used to determine the color of the font of the mood ratings
# val_avg_row <- filter(output$sample_form_summary, ID==params$id)
# val_avg <- sl_di_avg_row$val_arr_dis_avg
# 
# mood_unchecked <- filter(mood,`Checklist Complete?`=="No")
# mood_unchecked <- transmute(mood_unchecked, "Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)
# mood <- transmute(mood, "Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)


# \newline
# ### Mood questionnaire {.tabset}
# #### Unchecked
# ```{r mood unchecked, echo=FALSE, warning=FALSE}
# #Mood table
# #Will need to change the figure for val_arr_dis_avg when we figure out that threshold
# if(nrow(mood_unchecked) > 0){
#   kbl(mood_unchecked) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, background=if_else(is.na(mood_unchecked[2]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(3, background=if_else(is.na(mood_unchecked[3]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(4, color="#D8D8D8") %>%
#     column_spec(5, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(6, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(7, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(8, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(9, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(10, color=if_else(val_avg > 50, "#D8D8D8", "black"))
# }
# ```
# 
# #### All blocks
# ```{r mood table, echo=FALSE, warning=FALSE}
# #Mood table
# #Will need to change the figure for val_arr_dis_avg when we figure out that threshold
# if(nrow(mood) > 0){
#   kbl(mood) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, background=if_else(is.na(mood[2]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(3, background=if_else(is.na(mood[3]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(4, color="#D8D8D8") %>%
#     column_spec(5, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(6, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(7, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(8, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(9, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(10, color=if_else(val_avg > 50, "#D8D8D8", "black"))
# }
# ```
