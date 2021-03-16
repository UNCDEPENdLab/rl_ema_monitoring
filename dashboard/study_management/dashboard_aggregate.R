####Souce dependent functions:
source("./dashboard/study_management/data_management_functions.R")
###Dependent functions:
require(lubridate)
if (FALSE) {
 ###Use metadata function for paths
 path_info<-get_ema_subject_metadata(data_dir = "./data/Subjects/")
 ###But...shane is not done with that yet, use his output design to manulaly put out a path_info object for now...
 path_info<-list(schedule=data.frame(subject_id="shane",schedule_path=list.files("data/Subjects/Shane/schedule",pattern = "*.db",full.names = T,recursive = F,include.dirs = F),stringsAsFactors = F),
                 physio=data.frame(subject_id="shane",physio_path=list.files("data/Subjects/Shane/physio/",full.names = T),schedule_path=list.files("data/Subjects/Shane/schedule",pattern = "*.db",full.names = T,recursive = F,include.dirs = F),stringsAsFactors = F),
                 video=data.frame(subject_id="shane",video_path=list.files("data/Subjects/Shane/video/",full.names = T),stringsAsFactors = F))


 expect_df <- data.frame(type=c("schedule","physio","video"),expected=c(4,42,4))
 #example for proc_schedule:
 output<-proc_schedule(schedule_df = path_info$schedule)
 ##Output a list of :
 output$proc_data #is a list with a length equal to the number of db files, include data imported from db
 output$subj_info #is a list with a length equal to the number of subjects, include subject information
 output$sample_info_df #is a dataframe wtih a row number equal to the number of subejcts, include subject level information.
}


#####Functions:
proc_schedule <- function(schedule_df = NULL, expect_df = NULL,tz="EST") {
 #load in data using shane's function
 raw_data <- lapply(1:nrow(schedule_df),function(i){
  db_raw <- getSchedDataItem(subjID = schedule_df$subject_id[[i]],abs_path = schedule_df$schedule_path[[i]])
  db_raw$ID <- schedule_df$subject_id[[i]]
  return(db_raw)
 })
 proc_data <- lapply(raw_data,proc_schedule_single,tz=tz)

 ####do more aggregation here:
 #####NEED MORE SUBJ DATA FOR AGGREGATION########
 sample_info <- do.call(rbind,lapply(proc_data,`[[`,"info_df"))
 sp_info_sq <- split(sample_info,sample_info$ID)
 sample_info$compliance <- is.na(sample_info$completed_time)
 overall_info<-cbind(aggregate(compliance ~ ID,data = sample_info,FUN = mean),do.call(rbind,lapply(raw_data,`[[`,"subject")))
 overall_info$completed_session <- aggregate(session_number ~ ID,data = sample_info[!is.na(sample_info$completed_time),],FUN = max)$session_number
 return(list(proc_data=proc_data,subj_info = sp_info_sq,sample_info_df=overall_info))
}

ms_to_date = function(ms, t0="1970-01-01", timezone) {
 sec = ms / 1000
 as.POSIXct(sec, origin=t0, tz=timezone)
}

proc_schedule_single <- function(raw_single,tz="EST") {
 ###NO TIMEZONE INFORMATION!!!!Using EST at the moment;
 raw_single$sessions$start_timestamp<-lubridate::parse_date_time(raw_single$sessions$start_timestamp,"%b $d, %Y %I:%M:%S %p",tz = tz)
 raw_single$sessions$stop_timestamp<-lubridate::parse_date_time(raw_single$sessions$stop_timestamp,"%b $d, %Y %I:%M:%S %p",tz=tz)
 ###Part I: Trial
 time_vars <- c("scheduled_time","stim_time","choice_time","feedback_time")
 for (tx in time_vars) {
  raw_single$trials[[tx]] <- ms_to_date(raw_single$trials[[tx]],timezone = tz)
 }
 tr_info_by_block<-do.call(rbind,lapply(split(raw_single$trials,raw_single$trials$block),function(ix){
  rx<-ix[1,c("block","scheduled_time")]
  rx$start_time <- ix$stim_time[1]
  rx$completed_time <- ix$feedback_time[nrow(ix)]
  rx$duration <- difftime(rx$completed_time,rx$start_time,units = "mins")
  rx$delay <- difftime(rx$start_time,rx$scheduled_time,units = "mins")
  return(rx)
 }))
 trial_info_df<-merge(raw_single$sessions,tr_info_by_block,by = "block",all = T)
 trial_info_df$session_number<-match(round(as.numeric(trial_info_df$scheduled_time),0),unique(round(as.numeric(trial_info_df$scheduled_time),0)))
 trial_info_df<-trial_info_df[,-grep("_ms",names(trial_info_df))]
 trial_info_df$spec <-unlist(apply(trial_info_df[c("block","start_trial","last_trial")],1,list),recursive = F)
 trial_info_df<-trial_info_df[c("session_number","scheduled_time","start_time","completed_time","duration","delay","spec")]
 trial_info_df$type <- "trials"
 #assign session number to each of the different type of data:
 raw_single$trials$session_number<-info_df$session_number[match(raw_single$trials$block,info_df$block)]

 ##Part II: questionnaires:
 time_vars <- c("scheduled_time","start_time","completed_time")
 for (tx in time_vars) {
  raw_single$questionnaires[[tx]] <- ms_to_date(raw_single$questionnaires[[tx]],timezone = tz)
 }
 raw_single$answers$answer_time <- ms_to_date(raw_single$answers$answer_time,timezone = tz)

 raw_single$questionnaires$session_number<-sapply(raw_single$questionnaires$scheduled_time,function(x){
  dx<-difftime(x,trial_info_df$scheduled_time,units = "mins")
  dx[abs(dx)>30] <- NA
  if(length(which.min(dx))>0){
   trial_info_df$session_number[which.min(abs(dx))]
  } else {
   return(NA)
  }
 },USE.NAMES = F)
 session_info_df <- raw_single$questionnaires
 session_info_df <- session_info_df[order(session_info_df$scheduled_time),]
 session_info_df$duration<-difftime(session_info_df$completed_time,session_info_df$start_time,units = "mins")
 session_info_df$delay<-difftime(session_info_df$start_time,session_info_df$scheduled_time,units = "mins")
 session_info_df$spec <- unlist(apply(session_info_df[c("type","number","description")],1,list),recursive = F)
 session_info_df<-session_info_df[c("session_number","scheduled_time","start_time","completed_time","duration","delay","spec")]
 session_info_df$type <- "questionnaires"
 #update answer DF as well"
 raw_single$answers$session_number<-session_info_df$session_number[match(round(as.numeric(raw_single$answers$answer_time,0)),round(as.numeric(session_info_df$completed_time),0))]
 ##Part II: proc answer df:
 ##Proc questionnaire data here: not yet
 #######SKIP########
 ###return proc_answer object###########
 info_df <- rbind(session_info_df,trial_info_df)
 info_df <- info_df[order(info_df$scheduled_time),]
 info_df$ID <- raw_single$ID
 rownames(info_df) <- NULL
 return(list(raw_data=raw_single,info_df = info_df,ID=raw_single$ID))
}

proc_physio <- function(physio_df = NULL, sample_rate=256.03, sd_times=10, pre=500,post=1500,expect_df = NULL,tz="EST") {

 lapply(1:nrow(physio_df),function(i) {
  session_datetime <- gsub(".db","",gsub(".*_physio_","",basename(physio_df$physio_path[i])))
  session_datetime <- gsub("[^0-9A-Za-z///' ]","" , session_datetime ,ignore.case = TRUE)
  session_datetime<-lubridate::parse_date_time(session_datetime,"%b $d %Y %I%M%S %p",tz = tz)
  EEG_data<-load_EEG(subject_name = physio_df$subject_id[i],abs_path = list(physio_path=physio_df$physio_path[i],schedule_path=physio_df$schedule_path[i]),sample_rate=sample_rate, sd_times=sd_times)
  recorded_times<-ms_to_date(EEG_data$EEG_data$recorded_times,timezone = tz)
  epochs_around_feedback<-get_epochs_around_feedback(EEG_data = EEG_data$EEG_data,pre = pre,post=post,sample_rate=sample_rate,fbt=EEG_data$behavior$feedback_time)
 })

}







