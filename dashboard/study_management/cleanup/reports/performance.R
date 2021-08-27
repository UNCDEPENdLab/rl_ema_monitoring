#task performance table wrangling
task <- rename(perf,"Date"=date) #get rid of blocks not played
#task$Date <- lapply(task$Date,date_format) #reformat dates (these dates don't come from blk_dt)
task <- arrange(task, -row_number()) #flip df so that most recent dates are first
# ensure that the data is a flat dataframe (no nested lists/dataframes)
#mood[] <- lapply(mood, list_to_cv)
#create cumulative earnings column
task$c_earn <- c()
task$c_earn[nrow(task)] <- task$earning[nrow(task)]
for(i in (nrow(task)-1):1){
  task$c_earn[i] <- task$earning[i] + task$c_earn[i+1]
}
#TEMPORARY: adding in an intox response with a very early start time to make sure that all blocks match. This will not be necessary with real data, when there will be an intox response preceding every block.
#temp <- data.frame("time"=1504931510000,"answer"="Yes")
#output$proc_data[[subj]]$raw_data$drugs_check <- rbind(output$proc_data[[subj]]$raw_data$drugs_check,temp)
#create intoxication column
raw_s_data <- output$proc_data[[subj]]$raw_data$sessions
block_times <- as.numeric(raw_s_data$start_time_ms)
raw_s_data$intox <- lapply(block_times, get_intox_resp, id=subj)
intox_df <- select(raw_s_data, block, intox)
#join column to task df
task <- left_join(task, intox_df, by="block")
task$abs_accurate_feed <- task$abs_accurate_feed*100
task$relative_accuracy_feed <- task$relative_accuracy_feed*100
task$relative_accuracy_nofeed <- task$relative_accuracy_nofeed*100
task$abs_accurate_nofeed <- task$abs_accurate_nofeed*100
task$IDe_bias <- task$IDe_bias*100
task$mean_rt <- task$mean_rt*1000
task[,c(3:8)] <- round(task[,c(3:8)]) #round percentages to nearest whole percent
rownames(task) <- NULL

# ensure that the data is a flat dataframe (no nested lists/dataframes)
task <- unnest(task)
#task[] <- lapply(task, list_to_cv)

# create the unchecked mood csv if there is data to do so
if(exists("checklist")) {
  task <- left_join(task,checklist, by="Date")
  task_unchecked <- filter(task,`Checklist Complete?`=="No")
  task_unchecked <- transmute(task_unchecked, "Date"=Date, "Block"=block, "Objective % correct (feed.)"=abs_accurate_feed, "Experienced % correct (feed.)"=relative_accuracy_feed, "Objective % correct (no feed.)"=abs_accurate_nofeed, "Experienced % correct (no feed.)"=relative_accuracy_nofeed, "RT(ms)"=mean_rt, "Left %"=IDe_bias, "Intoxicated?"=intox, "Cumulative earnings"=c_earn)
  task <- transmute(task, "Date"=Date, "Block"=block, "Objective % correct (feed.)"=abs_accurate_feed, "Experienced % correct (feed.)"=relative_accuracy_feed, "Objective % correct (no feed.)"=abs_accurate_nofeed, "Experienced % correct (no feed.)"=relative_accuracy_nofeed, "RT(ms)"=mean_rt, "Left %"=IDe_bias, "Intoxicated?" = intox, "Cumulative earnings"=c_earn)
  # output the performance table to a csv
  #write_csv(mood, paste0(dataPath, "/Subjects/", subj, "/reports/performance_unchecked.csv"))
}

# output the sleep table to a csv
write_csv(task, paste0(dataPath, "/Subjects/", subj, "/reports/performance.csv"))
saveRDS(task, paste0(dataPath, "/Subjects/", subj, "/reports/performance.rds"))
