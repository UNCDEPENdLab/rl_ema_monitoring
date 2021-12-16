#eeg_rawsum <- rename(eeg_rawsum,"Block"=block)
# 2021-10-06 AndyP assume we now have eeg_rawsum from dashboard aggregate.R output of get_good_EEG

library(dplyr) # do we need to re-load?  AndyP 2021-10-06

#warning('2021-10-06-AndyP assume we now have eeg_rawsum from dashboard aggregate.R output of get_good_EEG, delete this warning when fixed')
if (any(colnames(eeg_rawsum)=='block')){
  warning('block already named on eeg_rawsum...how???')
} else {
  eeg_rawsum <- eeg_rawsum %>% rename(block='nbl')
}
eeg_rawsum <- left_join(eeg_rawsum,blk_dt,by="block") #add date and checklist column
eeg_rawsum <- eeg_rawsum %>% mutate(per_Ch_1 = Ngood1/Ntotal, per_Ch_2 = Ngood2/Ntotal, per_Ch_3 = Ngood3/Ntotal, per_Ch_4 = Ngood4/Ntotal)
eeg_rawsum <- mutate(eeg_rawsum,"avg"=(per_Ch_1 + per_Ch_2 + per_Ch_3 +per_Ch_4)/4) #create column for avg eeg_rawsum signal
eeg_rawsum <- arrange(eeg_rawsum, -row_number()) #flip df so most recent blocks are first
#convert all to percentages, rounded
eeg_rawsum$per_Ch_1 <- round(eeg_rawsum$per_Ch_1*100)
eeg_rawsum$per_Ch_2 <- round(eeg_rawsum$per_Ch_2*100)
eeg_rawsum$per_Ch_3 <- round(eeg_rawsum$per_Ch_3*100)
eeg_rawsum$per_Ch_4 <- round(eeg_rawsum$per_Ch_4*100)
eeg_rawsum$avg <- round(eeg_rawsum$avg*100)
eeg_rawsum$goodTrials <- round(eeg_rawsum$Ngood_by_Block*100)
eeg_rawsum$miss_Ch_1 <- round(100 - eeg_rawsum$per_Ch_1)
eeg_rawsum$miss_Ch_2 <- round(100 - eeg_rawsum$per_Ch_2)
eeg_rawsum$miss_Ch_3 <- round(100 - eeg_rawsum$per_Ch_3)
eeg_rawsum$miss_Ch_4 <- round(100 - eeg_rawsum$per_Ch_4)
#eeg_col_named <- transmute(eeg_rawsum, "Date"=Date, "Block"=Block, "Overall % good"=avg, "Ch. 1 % good"=per_Ch_1, "Ch. 2 % good"=per_Ch_2, "Ch. 3 % good"=per_Ch_3, "Ch. 4 % good"=per_Ch_4)

# create the unchecked mood csv if there is data to do so
if(exists("checklist")) {
  eeg_unchecked <- filter(eeg_rawsum, checklist=="No")
  #write_csv(eeg_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/eeg_unchecked.csv"))
  saveRDS(eeg_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/eeg_unchecked.rds"))
}

# output the sleep table to a csv
#write_csv(eeg_col_named, paste0(dataPath, "/Subjects/", subj, "/reports/eeg_rawsum.csv"))
saveRDS(eeg_rawsum, paste0(dataPath, "/Subjects/", subj, "/reports/eeg.rds"))
