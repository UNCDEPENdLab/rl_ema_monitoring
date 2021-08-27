#HR table wrangling
hr <- rename(hr,"Block"=block)
hr_dates <- left_join(hr,blk_dt,by="Block") #add date column
hr_ordered <- arrange(hr_dates, -row_number()) #most recent blocks first
#convert all to percentages, rounded
hr_ordered$per_Good <- hr_ordered$per_Good*100
hr_ordered$per_Good <- round(hr_ordered$per_Good)
hr_ordered <- transmute(hr_ordered, "Date"=Date, "Block"=Block, "Good signal %"=per_Good) 

# create the unchecked mood csv if there is data to do so
if(exists("checklist")) {
  hr_unchecked <- filter(hr_ordered, checklist=="No")
  hr_unchecked <- transmute(hr_unchecked, "Date"=Date, "Block"=Block, "Good signal %"=per_Good) 
  write_csv(hr_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/hr_unchecked.csv"))
  saveRDS(hr_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/hr_unchecked.rds"))
}

# output the sleep table to a csv
write_csv(hr_ordered, paste0(dataPath, "/Subjects/", subj, "/reports/hr.csv"))
saveRDS(hr_ordered, paste0(dataPath, "/Subjects/", subj, "/reports/hr.rds"))
