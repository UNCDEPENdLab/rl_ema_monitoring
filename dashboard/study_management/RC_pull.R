library(tidyverse)
library(REDCapR)
library(rjson)

get_redcap_creds <- function(cred_path=NULL) {
  # use rjson to load the credentials
  creds <- fromJSON(file = cred_path)
  # return the result
  return(creds)
}

redcap_pull <- function(uri=NULL, token=NULL, activeList=NULL){
  redcap.vars <- REDCapR::redcap_variables(redcap_uri=uri, token=token)$data$export_field_name
  df <- as.data.frame(replicate(length(redcap.vars), as.numeric()))
  names(df) <- redcap.vars
  redcap.vars <- df
  redcap.vars %>%
    select(record_id, starts_with("ema"), starts_with("rl_ema")) %>%
    select(!matches("[^_]+_1$")) -> redcap.vars
  #source("/Users/bealanger/github/rl_ema_monitoring/dashboard/study_management/data_management_functions.R")
  #active <- getActiveList(root_dir="rl_ema_monitoring")
  #active <- (c("30497", "2")) # replace with Shane's "getActiveList" function from data_management_functions
  active <- (c(unlist(activeList, use.names=FALSE)))
  metafields <- c("study_progress", "group_assign", "rl_start", "fmri_date", "week1_payment", "week2_payment",
                  "fmri_payment", "week3_payment", "week4_payment", "scan_complete", "rl_complete")
  dfy <- as.data.frame(replicate(length(metafields), as.numeric()))
  names(dfy) <- metafields
  metafields <- dfy
  checkboxes <- redcap.vars %>% select(contains("___"))
  names(checkboxes) <- gsub("___[0-9]*", "", names(checkboxes))
  checkboxes <- subset(checkboxes, select = !duplicated(names(checkboxes)))
  redcap.vars %>%
    select(!contains("___")) -> redcap.vars
  redcap.vars <- merge(redcap.vars, checkboxes)
  redcap.vars <- merge(redcap.vars, metafields)
  momentum <- REDCapR::redcap_read_oneshot(redcap_uri=uri, 
                                           token=token,
                                           fields=names(redcap.vars),
                                           records=active)$data
  momentum %>%
    mutate(group_assign=ifelse(group_assign==1, "Community", "Borderline"),
           scan_complete=ifelse(scan_complete==1, "Yes", "No"),
           rl_complete=ifelse(rl_complete==1, "Yes", "No"),
           ema_overview=ifelse(ema_overview==1, "Yes", "No"),
           ema_flag=ifelse(ema_flag==1, "Yes", "No"),
           ema_weekcheck=ifelse(ema_weekcheck==1, "Yes", "No"),
           ema_weekcheck_threshold=ifelse(ema_weekcheck_threshold==1, "Yes", "No"),
           ema_weekcheck_paid=ifelse(ema_weekcheck_paid==1, "Yes", "No"),
           ema_checklist_complete=ifelse(ema_checklist_complete==1, "Yes", "No")) -> momentum
  study_progress <- data.frame(value=1:9,
                               label=c("Scheduling", "Screening", "Baseline Interviews", "Cognitive Assessments/SR",
                                       "RL-EMA", "Complete", "Ineligible", "Other", "Dropped Out"))
  momentum$study_progress <- study_progress$label[match(momentum$study_progress, study_progress$value)]
  momentum %>%
    group_by(record_id) %>%
    mutate(across(names(metafields), ~ dplyr::first(.))) %>%
    filter(!redcap_event_name=="metadata_arm_1") -> momentum
  for (i in 1:nrow(momentum)) {
    day <- stringr::str_split(momentum$redcap_event_name[i], pattern="_")[[1]][3]
    momentum$redcap_event_name[i] <- day
  }
  rcmetadata <- redcap_metadata_read(redcap_uri=uri, token=token)$data
  checkbox <- function(checkbox="registration_race") {
    choices <- rcmetadata[rcmetadata$field_name==checkbox, "select_choices_or_calculations"]$select_choices_or_calculations
    dfx <- REDCapR::checkbox_choices(select_choices=choices) 
    dfx$variable <- checkbox
    dfx$varmap <- paste0(dfx$variable, "___", dfx$id)
    return(dfx)
  }
  checkboxes <- names(checkboxes)
  map <- checkboxes
  checkboxes <- lapply(checkboxes, checkbox)
  x <- c()
  y <- c()
  for (i in 1:length(map)) {
    if (map[i]==checkboxes[i][[1]]$variable[1]) {
      tempdf <- momentum[which(startsWith(names(momentum), checkboxes[i][[1]]$variable[1]))]
      tempnames <- names(tempdf)
      for (n in 1:length(tempnames)) {
        y <- append(y, tempnames[n])
        tempnames[n] <- checkboxes[i][[1]]$label[n][match(tempnames[n], checkboxes[i][[1]]$varmap[n])]
        x <- append(x, tempnames[n])
      }
    }
  }
  dfz <- data.frame(x, y)
  for (j in 1:length(names(momentum))) {
    if (names(momentum)[j] %in% dfz$y) {
      vari <- dfz$x[which(dfz$y==names(momentum)[j])]
      curr <- dfz$y[which(dfz$y==names(momentum)[j])]
      names(momentum)[which(names(momentum)==curr)] <- vari
    }
  }
  momentum %>% filter(!is.na(None)) -> momentum # take out this line!
  ​
  # OVERALL ISSUES #
  momentum$Overall_Issues <- "x" 
  for (k in 1:nrow(momentum)) {
    xyz <- c()
    temprow <- momentum[which(names(momentum) %in% checkboxes[7][[1]]$label)][k,]
    for (z in 1:length(names(temprow))) {
      if (temprow[z][[1]]==1) {
        xyz <- append(xyz, names(temprow)[z])
        xyz <- toString(xyz)
      }
    }
    if (length(xyz)==0){
      momentum$Overall_Issues[k] <- NA 
    } else {momentum$Overall_Issues[k] <- xyz }
  }
  ​
  # COMPLIANCE #
  momentum$Compliance_Checklist <- "x"
  for (k in 1:nrow(momentum)) {
    xyz <- c()
    temprow <- momentum[which(names(momentum) %in% checkboxes[1][[1]]$label)][k,]
    for (z in 1:length(names(temprow))) {
      if (temprow[z][[1]]==1) {
        xyz <- append(xyz, names(temprow)[z])
        xyz <- toString(xyz)
      }
    }
    if (length(xyz)==0){
      momentum$Compliance_Checklist[k] <- NA 
    } else {momentum$Compliance_Checklist[k] <- xyz }
  }
  ​
  # TASK PERFORMANCE #
  momentum$Task_Performance_Checklist <- "x"
  for (k in 1:nrow(momentum)) {
    xyz <- c()
    temprow <- momentum[which(names(momentum) %in% checkboxes[2][[1]]$label)][k,]
    for (z in 1:length(names(temprow))) {
      if (temprow[z][[1]]==1) {
        xyz <- append(xyz, names(temprow)[z])
        xyz <- toString(xyz)
      }
    }
    if (length(xyz)==0){
      momentum$Task_Performance_Checklist[k] <- NA 
    } else {momentum$Task_Performance_Checklist[k] <- xyz }
  }
  ​
  # EEG/HR #
  momentum$EEGHR_Checklist <- "x"
  for (k in 1:nrow(momentum)) {
    xyz <- c()
    temprow <- momentum[which(names(momentum) %in% checkboxes[3][[1]]$label)][k,]
    for (z in 1:length(names(temprow))) {
      if (temprow[z][[1]]==1) {
        xyz <- append(xyz, names(temprow)[z])
        xyz <- toString(xyz)
      } 
    }
    if (length(xyz)==0){
      momentum$EEGHR_Checklist[k] <- NA 
    } else {momentum$EEGHR_Checklist[k] <- xyz }
  }
  ​
  # SLEEP #
  momentum$Sleep_Checklist <- "x"
  for (k in 1:nrow(momentum)) {
    xyz <- c()
    temprow <- momentum[which(names(momentum) %in% checkboxes[4][[1]]$label)][k,]
    for (z in 1:length(names(temprow))) {
      if (temprow[z][[1]]==1) {
        xyz <- append(xyz, names(temprow)[z])
        xyz <- toString(xyz)
      } 
    }
    if (length(xyz)==0){
      momentum$Sleep_Checklist[k] <- NA 
    } else {momentum$Sleep_Checklist[k] <- xyz }
  }
  ​
  # MOOD #
  momentum$Mood_Checklist <- "x"
  for (k in 1:nrow(momentum)) {
    xyz <- c()
    temprow <- momentum[which(names(momentum) %in% checkboxes[5][[1]]$label)][k,]
    for (z in 1:length(names(temprow))) {
      if (temprow[z][[1]]==1) {
        xyz <- append(xyz, names(temprow)[z])
        xyz <- toString(xyz)
      } 
    }
    if (length(xyz)==0){
      momentum$Mood_Checklist[k] <- NA 
    } else {momentum$Mood_Checklist[k] <- xyz }
  }
  ​
  # VIDEO #
  momentum$Video_Checklist <- "x"
  for (k in 1:nrow(momentum)) {
    xyz <- c()
    temprow <- momentum[which(names(momentum) %in% checkboxes[6][[1]]$label)][k,]
    for (z in 1:length(names(temprow))) {
      if (temprow[z][[1]]==1) {
        xyz <- append(xyz, names(temprow)[z])
        xyz <- toString(xyz)
      } 
    }
    if (length(xyz)==0){
      momentum$Video_Checklist[k] <- NA 
    } else {momentum$Video_Checklist[k] <- xyz }
  }
  ​
  # FINAL DF CREATION #
  names(momentum)
  momentum %>%
    select(ID=record_id,
           Group=group_assign,
           Day=redcap_event_name,
           Date=ema_date,
           `Study Progress`=study_progress,
           `RL Start Date`=rl_start,
           `EMA RA`=ema_ra_assigned,
           `RL Complete`=rl_complete,
           `Issues Today?`=ema_overview,
           `Flagged?`=ema_flag,
           `Overall Issues`=Overall_Issues,
           `Compliance`=Compliance_Checklist,
           `Compliance Notes`=ema_compliance_notes,
           `Task Performance`=Task_Performance_Checklist,
           `Task Performance Notes`=ema_task_performance_notes,
           EEGHR=EEGHR_Checklist,
           `EEGHR Notes`=ema_s_notes,
           Sleep=Sleep_Checklist,
           `Sleep Notes`=ema_rc_notes_1_v2,
           Mood=Mood_Checklist,
           `Mood Notes`=ema_mood_notes,
           Video=Video_Checklist,
           `Video Notes`=ema_video_notes,
           `Other Issues Notes`=ema_other_notes,
           `End of 7 Day Period?`=ema_weekcheck,
           `Weekly Compliance`=ema_week_compliance,
           `Weekly Threshold (85%)`=ema_weekcheck_threshold,
           `Paid?`=ema_weekcheck_paid,
           `Payment Notes`=ema_weekcheck_paynotes,
           `Checklist Complete?`=ema_checklist_complete,
           `Overall Notes`=ema_check_notes,
           `fMRI Date`=fmri_date,
           `Scan Complete`=scan_complete,
           `Week 1 Payment`=week1_payment,
           `Week 2 Payment`=week2_payment,
           `Week 3 Payment`=week3_payment,
           `Week 4 Payment`=week4_payment) -> final
  return(final)
}
