#eeg <- rename(eeg,"Block"=block)
# 2021-10-06 AndyP assume we now have eeg_rawsum from dashboard aggregate.R output of get_good_EEG

library(dplyr) # do we need to re-load?  AndyP 2021-10-06

warning('2021-10-06-AndyP assume we now have eeg_rawsum from dashboard aggregate.R output of get_good_EEG, delete this warning when fixed')
eeg <- left_join(eeg,blk_dt,by="block") #add date and checklist column
eeg <- eeg %>% rename(block='nbl')
eeg <- eeg %>% mutate(per_Ch_1 = Ngood1/Ntotal, per_Ch_2 = Ngood2/Ntotal, per_Ch_3 = Ngood3/Ntotal, per_Ch_4 = Ngood4/Ntotal)
eeg <- mutate(eeg,"avg"=(per_Ch_1 + per_Ch_2 + per_Ch_3 +per_Ch_4)/4) #create column for avg EEG signal 
eeg <- arrange(eeg, -row_number()) #flip df so most recent blocks are first
#convert all to percentages, rounded
eeg$per_Ch_1 <- round(eeg$per_Ch_1*100)
eeg$per_Ch_2 <- round(eeg$per_Ch_2*100) 
eeg$per_Ch_3 <- round(eeg$per_Ch_3*100) 
eeg$per_Ch_4 <- round(eeg$per_Ch_4*100)
eeg$avg <- round(eeg$avg*100)
eeg$goodTrials <- round(eeg$Ngood_by_Block*100)
#eeg_col_named <- transmute(eeg, "Date"=Date, "Block"=Block, "Overall % good"=avg, "Ch. 1 % good"=per_Ch_1, "Ch. 2 % good"=per_Ch_2, "Ch. 3 % good"=per_Ch_3, "Ch. 4 % good"=per_Ch_4)

# create the unchecked mood csv if there is data to do so
if(exists("checklist")) {
  eeg_unchecked <- filter(eeg, checklist=="No")
  #write_csv(eeg_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/eeg_unchecked.csv"))
  saveRDS(eeg_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/eeg_unchecked.rds"))
}

# output the sleep table to a csv
#write_csv(eeg_col_named, paste0(dataPath, "/Subjects/", subj, "/reports/eeg.csv"))
saveRDS(eeg, paste0(dataPath, "/Subjects/", subj, "/reports/eeg.rds"))
