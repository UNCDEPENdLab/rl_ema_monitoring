#Sleep diary table wrangling
diary <- proc_sched$form_dfs$`Sleep Diary`
diary <- unnest(diary, cols=c("event_df"))
diary$event_df <- NULL
diary$answer_time <- str_extract(diary$answer_time,"\\d{4}-\\d{2}-\\d{2}") #chop off the hour from the answer time
diary <- rename(diary,"Date"=answer_time)
#diary$Date <- lapply(diary$Date,date_format) #format appropriately
insomnia_ratio <- output$sample_form_summary$num_no_sleep/nrow(diary) #this will be used to determine the color of the font in the "Didn't sleep" column

#this will be used to determine the color of the font of the sleep quality ratings
sl_di_avg_row <- filter(output$sample_form_summary, ID==subj)
sleep_dist <- sl_di_avg_row$sleep_di_avg

# ensure that the data is a flat dataframe (no nested lists/dataframes)
diary[] <- lapply(diary, list_to_cv)

# make the subdir if it does not exist
if(dir.exists(paste0(dataPath, "/Subjects/", subj, "/reports")) != TRUE){
  dir.create(paste0(dataPath, "/Subjects/", subj, "/reports"))
}

# create the unchecked diary csv if there is data to do so
if (exists("checklist")) {
  diary <- left_join(diary,checklist, by="Date")
  diary_unchecked <- filter(diary,`Checklist Complete?`=="No")
  diary_check <- output$redcap %>% filter(ID==subj) %>% select(`Sleep`,`Sleep Notes`,Date)
  diary_check$Date <- as.character(diary_check$Date)
  diary$Date <- as.character(diary$Date)
  diary <- left_join(diary, diary_check, by="Date")#adding RA check columns
  diary <- arrange(diary, -row_number()) #make the most recent block come first
  # output the sleep table to a csv
  write_csv(diary, paste0(dataPath, "/Subjects/", subj, "/reports/sleep_unchecked.csv"))
  saveRDS(diary, paste0(dataPath, "/Subjects/", subj, "/reports/sleep_unchecked.rds"))
}

# output the sleep table to a csv
write_csv(diary, paste0(dataPath, "/Subjects/", subj, "/reports/sleep.csv"))
saveRDS(diary, paste0(dataPath, "/Subjects/", subj, "/reports/sleep.rds"))

# save the summaries (sleep_dist, insomnia_ratio) for the rmd as an rdata file
saveRDS(name_list(sleep_dist, insomnia_ratio), paste0(dataPath, "/Subjects/", subj, "/reports/sleep_summaries.rds"))

