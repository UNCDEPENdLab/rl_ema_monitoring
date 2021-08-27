mood <- proc_sched$form_dfs$`Mood Questionnaire`
mood <- unnest(mood)
mood$event_df <- NULL
mood$answer_time <- str_extract(mood$answer_time,"\\d{4}-\\d{2}-\\d{2}") #chop off the hour from the answer time
mood <- rename(mood,"Date"=answer_time)
mood$Date <- as.character(mood$Date)
mood <- arrange(mood, -row_number()) #make the most recent block come first

#this will be used to determine the color of the font of the mood ratings
val_avg_row <- filter(output$sample_form_summary, ID==subj)
val_avg <- val_avg_row$val_arr_dis_avg

# ensure that the data is a flat dataframe (no nested lists/dataframes)
mood[] <- lapply(mood, list_to_cv)

# make the subdir if it does not exist
if(dir.exists(paste0(dataPath, "/Subjects/", subj, "/reports")) != TRUE){
  dir.create(paste0(dataPath, "/Subjects/", subj, "/reports"))
}

# create the unchecked mood csv if there is data to do so
if(exists("checklist")) {
  mood <- left_join(mood,checklist, by="Date")
  mood$Date <- lapply(mood$Date,date_format) #format appropriately
  mood_check <- output$redcap %>% filter(ID==subj) %>% select(`Mood`,`Mood Notes`,Date)
  mood_check$Date <- as.character(mood_check$Date)
  mood$Date <- as.character(mood$Date)
  mood <- left_join(mood, mood_check, by="Date")#adding RA check column
  mood_unchecked <- transmute(mood_unchecked, "Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)
  mood <- transmute(mood, "Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)
  # output the sleep table to a csv
  write_csv(mood, paste0(dataPath, "/Subjects/", subj, "/reports/mood_unchecked.csv"))
  saveRDS(mood, paste0(dataPath, "/Subjects/", subj, "/reports/mood_unchecked.rds"))
}

# output the sleep table to a csv
write_csv(mood, paste0(dataPath, "/Subjects/", subj, "/reports/mood.csv"))
saveRDS(mood, paste0(dataPath, "/Subjects/", subj, "/reports/mood.rds"))
# save the sleep_dist for the rmd as an rdata file
saveRDS(val_avg, paste0(dataPath, "/Subjects/", subj, "/reports/val_avg.rds"))
