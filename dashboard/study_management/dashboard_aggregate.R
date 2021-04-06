####Souce dependent functions:
root_dir = getwd()

# source(file.path(root_dir,"dashboard/study_management/data_management_functions.R"))
# source(file.path(root_dir,"EEG_Dashboard.R"))
# source(file.path(root_dir,"ECG_Dashboard.R"))
###Dependent functions:
require(lubridate)
if (FALSE) {
  ###!!!!!!Use this to get path_info if you have standardized data folder including .json for each subject and .db files!!!!!!######
  path_info<-get_ema_subject_metadata(root_dir = "rl_ema_monitoring")
  ###Otherwise use this chunk (9-27) to get the data to get path_info
  root_dir = "./data/Subjects"
  path_info<-lapply(c("schedule","physio","video"),function(dj){
    list_id <- list.dirs(path = root_dir,recursive = F,full.names = F)
    if(dj == "video") {
      pat = ".*.mp4"
    } else {
      pat = ".db"
    }
    do.call(rbind,lapply(list_id,function(idx){
      db_files<-list.files(file.path(root_dir,idx,dj),pattern = pat,full.names = T,recursive = F,include.dirs = F)
      if(length(db_files)>0){
        dx<-data.frame(subject_id=idx,file_path=db_files,stringsAsFactors = F)
        return(dx)
      } else {
        return(NULL)
      }
    }))
  })
  names(path_info) <- c("schedule","physio","video")
  #########END########

  #example for proc_schedule:
  output<-proc_schedule(schedule_df = path_info$schedule)
  ##Output a list of :
  output$proc_data #is a list with a length equal to the number of db files, include data imported from db
  output$subj_info #is a list with a length equal to the number of subjects, include subject information on compliance
  output$sample_info_df #is a dataframe wtih a row number equal to the number of subejcts, include sample level information on compliance .
  output$subj_performance # is a list with a length equal to the number of subjects, include subject information on performance
  output$sample_performance #is a dataframe wtih a row number equal to the number of subejcts, include subject level information on performance.

  #example for proc_physio:
  ##!!!!Must first proc the schedule data as physio uses the trial level data to generate percentage;
  output_physio <- proc_physio(physio_df = path_info$physio,sch_pro_output=output, tz="EST",
                               eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
                               ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
                               )
  names(output_physio)
  #list: two data sets: EEG & ECG
  ###within each data sets:
  #####proc: list of proc'ed eeg raw data, length of subjects
  #####fb: list of proc'ed eeg data, near the feedback times, length of subjects
  #####*summary: list of summary data frame for each subject, used for dashboard table generation, length of subjects
  #####*sample_summary: a data.frame of all subjects, used for dashboard overall table, nrow of subjects

}

#####Session number is dependent on scheduled time. Possible 1 game per day but with 2 sessions worth of data.


#####Functions:
load_db <- function(dbpath,table_names=NULL) {
  dbdata = dbConnect(SQLite(), dbpath)
  all_table_names <- dbListTables(dbdata)
  if(is.null(table_names)){
    table_names<-all_table_names
  } else if (any(!table_names %in% all_table_names)) {
    table_names <- table_names[table_names %in% all_table_names]
    message("No table(s) named: ",paste(table_names[!table_names %in% all_table_names],collapse = ", ")," in database file: \n",dbpath)
  }
  if(length(table_names)<1){
    message("No tables to pull.")
    return(NULL)
  }
  tables<-lapply(table_names,function(dfName){
    dbGetQuery(dbdata, paste0("SELECT * FROM ", dfName))
  })
  names(tables) <- table_names
  #close connection before return
  dbDisconnect(dbdata)
  return(tables)
}

ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

proc_schedule <- function(schedule_df = NULL,tz="EST") {
  #load in data using shane's function
  raw_data <- lapply(1:nrow(schedule_df),function(i){
    db_raw <- getSchedDataItem(subjID = schedule_df$subject_id[[i]],abs_path = schedule_df$file_path[[i]])
    db_raw$ID <- schedule_df$subject_id[[i]]
    return(db_raw)
  })
  proc_data <- lapply(raw_data,proc_schedule_single,tz=tz)
  names(proc_data) <- sapply(proc_data,`[[`,"sID")
  ####do more aggregation here:
  #####NEED MORE SUBJ DATA FOR AGGREGATION########
  sample_info <- do.call(rbind,lapply(proc_data,`[[`,"info_df"))
  performance_info <- do.call(rbind,lapply(proc_data,`[[`,"performance_info"))
  sp_info_sq <- split(sample_info,sample_info$ID)
  pr_info_sq <- split(performance_info,performance_info$ID)
  sample_info$compliance <- is.na(sample_info$completed_time)
  overall_info<-cbind(aggregate(compliance ~ ID,data = sample_info,FUN = mean),do.call(plyr::rbind.fill,lapply(raw_data,`[[`,"subject")))
  overall_info$completed_session <- aggregate(session_number ~ ID,data = sample_info[!is.na(sample_info$completed_time),],FUN = max)$session_number

  pr_info_subjwise <-  do.call(rbind,lapply(proc_data,`[[`,"performance_overall"))

  return(list(proc_data=proc_data,
              subj_info = sp_info_sq,sample_info_df=overall_info,
              subj_performance = performance_info, sample_performance = pr_info_subjwise))
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
  trials_1<-raw_single$trials

  ##Accuracy
  for (i in 1:nrow(trials_1)){
    if(is.na(trials_1$choice[i])) {next}
    trials_1$rank1[i]=raw_single$stimuli$rank[trials_1$stim1[i]+1]
    trials_1$rank2[i]=raw_single$stimuli$rank[trials_1$stim2[i]+1]
    trials_1$accuracy[i]=((trials_1$rank1[i]>trials_1$rank2[i])&&(trials_1$choice[i]==0)||(trials_1$rank1[i]<trials_1$rank2[i])&&(trials_1$choice[i]==1))
    if (trials_1$rank1[i]==trials_1$rank2[i])
      trials_1$accuracy[i]=NA
  }
  trials_1$accuracy[is.na(trials_1$choice)]<-NA
  ##Accuracy relative to probabilities that were experienced (constantly updating until an image switches to its no-feedback phase)##

  trials_1$relative_stim1=rep(NaN, nrow(trials_1))
  trials_1$relative_stim2=rep(NaN, nrow(trials_1))
  for (i in 2:nrow(trials_1)){
    trials_1$relative_stim1[i]=mean(trials_1$outcome[which(trials_1$stim1==trials_1$stim1[i]&trials_1$choice==0&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i])|trials_1$stim2==trials_1$stim1[i]&trials_1$choice==1&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i]))])
    trials_1$relative_stim2[i]=mean(trials_1$outcome[which(trials_1$stim1==trials_1$stim2[i]&trials_1$choice==0&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i])|trials_1$stim2==trials_1$stim2[i]&trials_1$choice==1&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i]))])
  }
  trials_1$relative_accuracy=NA
  index=which(!is.nan(trials_1$relative_stim1)&!is.nan(trials_1$relative_stim2))

  #accuracy according to experienced probabilities (differences of less than 10% between probabilties are omitted)
  for (i in index){
    trials_1$relative_accuracy[i]=((trials_1$relative_stim1[i]>trials_1$relative_stim2[i]+0.1)&&(trials_1$choice[i]==0)||(trials_1$relative_stim1[i]+0.1<trials_1$relative_stim2[i])&&(trials_1$choice[i]==1))
    if (((abs(trials_1$relative_stim1[i]-trials_1$relative_stim2[i])<0.1)&&(abs(trials_1$relative_stim2[i]-trials_1$relative_stim1[i]))<0.1))
      trials_1$relative_accuracy[i]=NA
  }
  #get RT
  trials_1$rt <- as.numeric(difftime(trials_1$choice_time,trials_1$stim_time,units = "secs"))

  info_by_block<-lapply(split(trials_1,trials_1$block),function(ix){
    #Compliance:
    rx<-ix[1,c("block","scheduled_time")]
    rx$start_time <- ix$stim_time[1]
    rx$completed_time <- ix$feedback_time[nrow(ix)]
    rx$duration <- difftime(rx$completed_time,rx$start_time,units = "mins")
    rx$delay <- difftime(rx$start_time,rx$scheduled_time,units = "mins")

    ##Performance data:
    px <- data.frame(block=unique(ix$block),
                     date=unique(as.Date(ix$feedback_time)),
                     side_bias=mean(ix$choice,na.rm = T),
                     mean_rt = mean(ix$rt,na.rm = T),
                     abs_accurate_feed = mean(as.numeric(ix[which(ix$feedback==1),]$accuracy),na.rm = T),
                     relative_accuracy_feed = mean(as.numeric(ix[which(ix$feedback==1),]$relative_accuracy),na.rm = T),
                     abs_accurate_nofeed = mean(as.numeric(ix[which(ix$feedback==0),]$accuracy),na.rm = T),
                     relative_accuracy_nofeed = mean(as.numeric(ix[which(ix$feedback==0),]$relative_accuracy),na.rm = T),

                     stringsAsFactors = F)


    return(list(compliance=rx,performance=px))
  })
  tr_info_by_block <- do.call(rbind,lapply(info_by_block,`[[`,"compliance"))
  pr_info_by_block <- do.call(rbind,lapply(info_by_block,`[[`,"performance"))
  #Process compliance
  trial_info_df<-merge(raw_single$sessions,tr_info_by_block,by = "block",all = T)
  trial_info_df$session_number<-match(round(as.numeric(trial_info_df$scheduled_time),0),unique(round(as.numeric(trial_info_df$scheduled_time),0)))
  trial_info_df<-trial_info_df[,-grep("_ms",names(trial_info_df))]
  trial_info_df$spec <-unlist(apply(trial_info_df[c("block","start_trial","last_trial")],1,list),recursive = F)

  #assign session number to each of the different type of data:
  trials_1$session_number<-trial_info_df$session_number[match(trials_1$block,trial_info_df$block)]
  pr_info_by_block$session_number<-trial_info_df$session_number[match(pr_info_by_block$block,trial_info_df$block)]

  trial_info_df<-trial_info_df[c("session_number","scheduled_time","start_time","completed_time","duration","delay","spec")]
  trial_info_df$type <- "trials"

  px_overall <- data.frame(ID=raw_single$ID,
                           side_bias=mean(trials_1$choice,na.rm = T),
                           mean_rt = mean(trials_1$rt,na.rm = T),
                           abs_accurate_overall = mean(as.numeric(trials_1$accuracy),na.rm = T),
                           relative_accuracy_feed = mean(as.numeric(trials_1$relative_accuracy),na.rm = T),
                           abs_accurate_feed = mean(as.numeric(trials_1[which(trials_1$feedback==1),]$accuracy),na.rm = T),
                           relative_accuracy_feed = mean(as.numeric(trials_1[which(trials_1$feedback==1),]$relative_accuracy),na.rm = T),
                           abs_accurate_nofeed = mean(as.numeric(trials_1[which(trials_1$feedback==0),]$accuracy),na.rm = T),
                           relative_accuracy_nofeed = mean(as.numeric(trials_1[which(trials_1$feedback==0),]$relative_accuracy),na.rm = T),
                           stringsAsFactors = F)

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
  pr_info_by_block$ID <- raw_single$ID
  rownames(info_df) <- NULL
  rownames(pr_info_by_block) <- NULL
  raw_single$trials <- trials_1

  return(list(raw_data=raw_single,info_df = info_df,performance_info=pr_info_by_block,performance_overall=px_overall,sID=raw_single$ID))
}

proc_physio <- function(physio_df = NULL,sch_pro_output=NULL, tz="EST",
                        eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
                        ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
                        ) {

  exp_out<-lapply(unique(physio_df$subject_id),function(IDx){
    print(IDx)
    #Load the physio data
    physio_concat <- load_physio_single(physio_df$file_path[physio_df$subject_id==IDx])
    #Get the matching behavioral data
    behav_df <- output$proc_data[[IDx]]$raw_data$trials
    behav_df <- behav_df[which(!is.na(behav_df$stim_time)),]
    sess_map<-unique(behav_df[c("block","session_number")])

    ###EEG
    eeg_raw <- load_EEG(EEGd = physio_concat$eeg,sample_rate = eeg_sample_rate,sd_times = sd_times)
    eeg_proc <- eeg_epochs_around_feedback(EEG_data = eeg_raw,
                                           pre = eeg_pre,post = eeg_post,sample_rate = eeg_sample_rate,
                                           fbt = as.numeric(behav_df$feedback_time)*1000)
    eeg_rawsum <- get_good_EEG(blocks=behav_df$block,a2f=eeg_proc)
    eeg_summary <- eeg_rawsum[1:4] / eeg_rawsum$ntrial
    names(eeg_summary) <- paste("per_Ch",1:4,sep = "_")
    eeg_summary$block <- eeg_rawsum$nbl
    eeg_summary$per_worst <- apply(eeg_summary[1:4],1,min,na.rm=T)
    eeg_summary$session_number<-sess_map$session_number[match(eeg_summary$block,sess_map$block)]
    eeg_summary$ID <- IDx
    eeg_summary <- eeg_summary[order(names(eeg_summary))]
    eeg_ov <- data.frame(t(apply(eeg_summary[paste("per_Ch",1:4,sep = "_")],2,mean,na.rm=T)))
    eeg_ov$avg_allCh <- apply(eeg_ov,1,mean,na.rm=T)
    eeg_ov$worst_allCh_allblocks <- min(eeg_summary[,paste("per_Ch",1:4,sep = "_")])
    eeg_ov$ID <- IDx

    ###ECG
    ecg_raw <- load_ECG(ECGd = physio_concat$ecg,HRstep = HRstep,sample_rate = ecg_sample_rate)
    ecg_fb <- ecg_epochs_around_feedback(ECG_data = ecg_raw,fbt = as.numeric(behav_df$feedback_time)*1000,
                                         pre = ecg_pre,post = ecg_post,sample_rate = ecg_sample_rate)
    ecg_summary <- get_good_ECG(blocks = behav_df$block,a2f = ecg_fb)
    ecg_summary$session_number<-sess_map$session_number[match(ecg_summary$block,sess_map$block)]
    ecg_summary$ID <- IDx
    ecg_summary <- ecg_summary[order(names(ecg_summary))]
    ecg_ov <- aggregate(per_Good ~ ID,data = ecg_summary,FUN = mean,na.rm=T)
    ecg_ov$worst_allblocks <- min(ecg_summary$per_Good)

    return(list(eeg_proc = eeg_raw,eeg_fb = eeg_proc, eeg_summary = eeg_summary, eeg_ov = eeg_ov,
           ecg_proc = ecg_raw,ecg_fb = ecg_fb, ecg_summary = ecg_summary, ecg_ov = ecg_ov))
  })

  nax <- c("proc","fb","summary")
  IDlist <- unique(physio_df$subject_id)
  output_fin<-lapply(c("eeg","ecg"),function(ay){
    output_ls<-lapply(nax,function(ax){
      output <- lapply(exp_out,`[[`,paste(ay,ax,sep = "_"))
      names(output) <- IDlist
      return(output)
    })
    names(output_ls) <- nax
    output_ls$sample_summary <- do.call(rbind, lapply(exp_out,`[[`,paste(ay,"ov",sep = "_")))
    return(output_ls)
  })
  names(output_fin) <- c("eeg","ecg")
  return(output_fin)
}

load_physio_single <- function(allpaths_sub) {
  all_dt <- lapply(allpaths_sub,load_db,table_names=c("EEG_muse","Polar_heartrate"))
  eeg_aggregate <- do.call(rbind,lapply(all_dt,`[[`,"EEG_muse"))
  eeg_aggregate <- eeg_aggregate[order(eeg_aggregate$recording_time),]
  ecg_aggregate <- do.call(rbind,lapply(all_dt,`[[`,"Polar_heartrate"))
  ecg_aggregate <- ecg_aggregate[order(ecg_aggregate$time_ms),]
  return(list(eeg=eeg_aggregate,ecg=ecg_aggregate))
}









