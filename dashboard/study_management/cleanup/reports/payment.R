
#save the output
output_temp <- output

# load the subject rdata file
load(paste0(dataPath, "/Subjects/", subj, "/schedule/", subj, "_schedule_proc.rdata"))
payment <- output$payment

#reset the output
output <- output_temp

# output the task payment table to a csv
write_csv(payment, paste0(dataPath, "/Subjects/", subj, "/reports/payment.csv"))
saveRDS(payment, paste0(dataPath, "/Subjects/", subj, "/reports/payment.rds"))
