compliance <-info[c("scheduled_time","delay","type", "duration")]
compliance$scheduled_time <- as.Date(compliance$scheduled_time)
compliance$delay <- round(as.numeric(compliance$delay), digits = 0)
compliance$s_type[compliance$type=="trials"] <- "Behavioral Game"
compliance$s_type[compliance$type=="questionnaires"] <- sapply(info$spec[compliance$type=="questionnaires"],`[[`,3)
compliance$delay <- round(compliance$delay)
#filters out NA/incomplete/upcoming data
compliance_filtered <- compliance %>% filter(!is.na(s_type))
compliance_filtered$is_missing <- is.na(compliance_filtered$duration)|compliance_filtered$delay>=1440
compliance_filtered$delayednotmissing <- ifelse(compliance_filtered$is_missing, NA, compliance_filtered$delay)
compliance_addon <- compliance_filtered[compliance_filtered$is_missing, ]
compliance_addon$days_delayed <- floor(compliance_addon$delay/1440)
compliance_addon$scheduled_time <- compliance_addon$scheduled_time + (compliance_addon$days_delayed)
compliance_addon$delayednotmissing <- compliance_addon$delay - (compliance_addon$days_delayed * 1440)
compliance_addon$days_delayed <- NULL
compliance_addon$is_missing <- FALSE
compliance_filtered_2 <- rbind(compliance_filtered, compliance_addon)
compliance_filtered_2 <- unnest(compliance_filtered_2)

# create the unchecked mood csv if there is data to do so
if(exists("checklist")) {
  # (call redcap_pull via Rdata file) will need to be redone once we know how redcap integration works (with multiple subjects)
  ema_checklist <- rc[c("Date", "Checklist Complete?")] 
  compliance_filtered_2$checklist <- ema_checklist$`Checklist Complete?`[match(compliance_filtered_2$scheduled_time, ema_checklist$Date)]
  print(compliance_filtered_2$checklist)
  compliance_filtered_2 <- filter(compliance_filtered_2, scheduled_time < "2020-12-11" )
  compliance_unchecked <- compliance_filtered_2[compliance_filtered_2$checklist != "Yes"| is.na(compliance_filtered_2$checklist), ]
  write_csv(compliance_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/compliance_unchecked.csv"))
  saveRDS(compliance_unchecked, paste0(dataPath, "/Subjects/", subj, "/reports/compliance_unchecked.rds"))
}

# output the sleep table to a csv
write_csv(compliance_filtered_2, paste0(dataPath, "/Subjects/", subj, "/reports/compliance.csv"))
saveRDS(compliance_filtered_2, paste0(dataPath, "/Subjects/", subj, "/reports/compliance.rds"))

