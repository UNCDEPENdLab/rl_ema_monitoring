eeg <- rename(eeg,"Block"=block)
eeg_dates <- left_join(eeg,blk_dt,by="Block") #add date and checklist column
eeg_avg <- mutate(eeg_dates,"avg"=(per_Ch_1 + per_Ch_2 + per_Ch_3 +per_Ch_4)/4) #create column for avg EEG signal 
eeg_row_ordered <- arrange(eeg_avg, -row_number()) #flip df so most recent blocks are first
#convert all to percentages, rounded
eeg_row_ordered$per_Ch_1 <- eeg_row_ordered$per_Ch_1*100
eeg_row_ordered$per_Ch_1 <- round(eeg_row_ordered$per_Ch_1)
eeg_row_ordered$per_Ch_2 <- eeg_row_ordered$per_Ch_2*100 
eeg_row_ordered$per_Ch_2 <- round(eeg_row_ordered$per_Ch_2)
eeg_row_ordered$per_Ch_3 <- eeg_row_ordered$per_Ch_3*100 
eeg_row_ordered$per_Ch_3 <- round(eeg_row_ordered$per_Ch_3)
eeg_row_ordered$per_Ch_4 <- eeg_row_ordered$per_Ch_4*100
eeg_row_ordered$per_Ch_4 <- round(eeg_row_ordered$per_Ch_4)
eeg_row_ordered$avg <- eeg_row_ordered$avg*100
eeg_row_ordered$avg <- round(eeg_row_ordered$avg)

# create the unchecked mood csv if there is data to do so
if(exists("checklist")) {
  eeg_unchecked <- filter(eeg_row_ordered, checklist=="No")
  write_csv(eeg_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/eeg_unchecked.csv"))
  saveRDS(eeg_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/eeg_unchecked.rds"))
}

# output the sleep table to a csv
write_csv(eeg_col_named, paste0(dataPath, "/Subjects/", subj, "/reports/eeg.csv"))
saveRDS(eeg_col_named, paste0(dataPath, "/Subjects/", subj, "/reports/eeg.rds"))
