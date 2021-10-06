####Souce dependent functions:
#root_dir = getwd()
root_dir <- getwd()
repo_path <- dirname(root_dir)
#source(file.path(root_dir,"dashboard/study_management/data_management_functions.R"))
#source(file.path(root_dir,"EEG_Dashboard.R"))
#source(file.path(root_dir,"ECG_Dashboard2.R"))
###Dependent functions:
require(lubridate)
if (FALSE) {
  ###!!!!!!Use this to get path_info if you have standardized data folder including .json for each subject and .db files!!!!!!######
  path_info <- get_ema_subject_metadata(root_dir = "rl_ema_monitoring")
  ###Otherwise use this chunk (9-27) to get the data to get path_info
  root_dir <- "./data/Subjects"
  path_info <- lapply(c("schedule","physio","video"),function(dj){
    list_id <- list.dirs(path = root_dir,recursive = F,full.names = F)
    if(dj == "video") {
      pat = ".*.mp4"
    } else {
      pat = "*.db"
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
  output <- proc_schedule(schedule_df = path_info$schedule,tz=Sys.timezone(),days_limit=60,force_reproc=T)
  ##Output a list of :
  output$proc_data #is a list with a length equal to the number of db files, include data imported from db
  output$subj_info #is a list with a length equal to the number of subjects, include subject information on compliance
  output$sample_info_df #is a dataframe wtih a row number equal to the number of subejcts, include sample level information on compliance .
  output$subj_performance # is a list with a length equal to the number of subjects, include subject information on performance
  output$sample_performance #is a dataframe wtih a row number equal to the number of subejcts, include subject level information on performance.

  #example for proc_physio:
  ##!!!!Must first proc the schedule data as physio uses the trial level data to generate percentage;
  output_physio <- proc_physio(physio_df = path_info$physio,sch_pro_output=output, tz="EST",thread=4,
                               force_reload=FALSE,force_reproc=FALSE,save_lite=F,
                               eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
                               ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
                               )
  names(output_physio)
  save(output,output_physio,file = "aggregate_output.rdata")
  #list: two data sets: EEG & ECG
  ###within each data sets:
  #####proc: list of proc'ed eeg raw data, length of subjects
  #####fb: list of proc'ed eeg data, near the feedback times, length of subjects
  #####*summary: list of summary data frame for each subject, used for dashboard table generation, length of subjects
  #####*sample_summary: a data.frame of all subjects, used for dashboard overall table, nrow of subjects
  save(output,output_physio,file = "fin_output.rdata")
}

#####Session number is dependent on scheduled time. Possible 1 game per day but with 2 sessions worth of data.


#####Functions:
load_db <- function(dbpath,table_names=NULL) {
  print(dbpath)
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

ms_to_date <- function(ms, t0="1970-01-01", timezone=Sys.timezone()) {
  as.POSIXct(ms / 1000, origin=t0, tz=timezone)
}

proc_schedule <- function(schedule_df = NULL,days_limit=35,task_limit=56,force_reproc=FALSE,tz="EST") {
  #load in data using shane's function
  raw_data <<- lapply(1:nrow(schedule_df),function(i){
    dbpath <- schedule_df$file_path[[i]] # paste0(schedule_df$file_path[[i]], '/', schedule_df$file_name[[i]])
    db_raw <- load_db(dbpath=dbpath,table_names = NULL)
    db_raw$ID <- schedule_df$subject_id[[i]]
    db_raw$data_mtime <- lubridate::as_datetime(file.info(schedule_df$file_path[[i]])$mtime,tz=tz)
    db_raw$data_folder <- dirname(schedule_df$file_path[[i]])
    return(db_raw)
  })
  # run the proc_schedule_single
  proc_data <- lapply(raw_data,proc_schedule_single,tz=tz,days_limit=days_limit,force_reproc=force_reproc)
  # drop any NA produced
  proc_data <- proc_data[!is.na(proc_data)]
  names(proc_data) <- sapply(proc_data,`[[`,"ID")
  # drop any items in the raw_data that failed schedule processing
  #raw_data_dropped <- 
  ####do more aggregation here:
  #####NEED MORE SUBJ DATA FOR AGGREGATION########
  sample_info <- do.call(rbind,lapply(proc_data,`[[`,"info_df"))
  performance_info <- do.call(rbind,lapply(proc_data,`[[`,"performance_info"))
  performance_info <- performance_info[!is.na(performance_info$date),]
  sp_info_sq <- split(sample_info,sample_info$ID)
  pr_info_sq <- split(performance_info,performance_info$ID)
  sample_info$compliance <- !is.na(sample_info$completed_time)
  overall_info<-cbind(aggregate(compliance ~ ID,data = sample_info[which(sample_info$scheduled_time <= Sys.Date()),],FUN = mean),do.call(plyr::rbind.fill,lapply(raw_data,`[[`,"subject")))
  #overall_info$completed_session <- aggregate(session_number ~ ID,data = sample_info[!is.na(sample_info$completed_time),],FUN = max)$session_number

  pr_info_subjwise <-  do.call(rbind,lapply(proc_data,`[[`,"performance_overall"))
  q_summary <-  do.call(rbind,lapply(proc_data,`[[`,"form_summary"))

  return(list(proc_data=proc_data,newdata_IDs=sapply(proc_data,`[[`,"ID",USE.NAMES = F)[sapply(proc_data,`[[`,"new_data")],
              subj_info = sp_info_sq,sample_info_df=overall_info,
              subj_performance = performance_info, sample_performance = pr_info_subjwise,
              sample_form_summary = q_summary))
}

calcu_accuracy<- function(trials_1,stimuli) {
  trials_1 <- trials_1[which(!is.na(trials_1$choice)),]
  for (i in 1:length(trials_1$block)){
    trials_1$stim2[trials_1$stim2 < 0] <- NA
    trials_1$rank1[i]=stimuli$rank[trials_1$stim1[i]+1]
    trials_1$rank2[i]=stimuli$rank[trials_1$stim2[i]+1]
    trials_1$accuracy[i]=((trials_1$rank1[i]>trials_1$rank2[i])&&(trials_1$choice[i]==0)||(trials_1$rank1[i]<trials_1$rank2[i])&&(trials_1$choice[i]==1))
    if (is.na(trials_1$rank2[i]) || trials_1$rank1[i]==trials_1$rank2[i])
      trials_1$accuracy[i]=NA
  }
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


  return(trials_1)

}

proc_schedule_single <- function(raw_single,days_limit=60,force_reproc=FALSE,tz="EST") {
  #print(raw_single$ID)
  tryCatch({
    output_path <- file.path(raw_single$data_folder,paste(raw_single$ID,"_schedule_proc.rdata",sep = ""))
    if(file.exists(output_path) && !force_reproc) {
      load(output_path)
      if(difftime(raw_single$data_mtime,output$raw_data$data_mtime,units = "mins") < 60){
        #if data less than 60 minutes in different don't proc
        output$new_data <- FALSE
        message("No new schedule data for: ",output$ID)
        return(output)
      }
    }
    message("Processing schedule file for: ",raw_single$ID)
  
    ###Part I: Trial
    time_vars <- c("scheduled_time","stim_time","choice_time","feedback_time")
    for (tx in time_vars) {
      raw_single$trials[[tx]] <- ms_to_date(raw_single$trials[[tx]],timezone = tz)
    }
  
  
    trials_df<-calcu_accuracy(trials_1=raw_single$trials,stimuli=raw_single$stimuli)
  
    #get RT
    trials_df$rt <- as.numeric(difftime(trials_df$choice_time,trials_df$stim_time,units = "secs"))
  
    info_by_block<-lapply(split(trials_df,trials_df$block),function(ix){
      #print(unique(ix$block))
      #Compliance:
      rx<-ix[1,c("block","scheduled_time")]
      rx$start_time <- ix$stim_time[1]
      #adding max here to show that possible 2 repsonse time? idk; change down below as well
      rx$completed_time <- max(ix$feedback_time)
      rx$duration <- difftime(rx$completed_time,rx$start_time,units = "mins")
      rx$delay <- difftime(rx$start_time,rx$scheduled_time,units = "mins")
  
      ##Performance data:
      px <- data.frame(block=unique(ix$block),
                       date=max(as.Date(ix$feedback_time)),
                       IDe_bias=mean(ix$choice,na.rm = T),
                       mean_rt = mean(ix$rt,na.rm = T),
                       abs_accurate_feed = mean(as.numeric(ix[which(ix$feedback==1),]$accuracy),na.rm = T),
                       relative_accuracy_feed = mean(as.numeric(ix[which(ix$feedback==1),]$relative_accuracy),na.rm = T),
                       abs_accurate_nofeed = mean(as.numeric(ix[which(ix$feedback==0),]$accuracy),na.rm = T),
                       relative_accuracy_nofeed = mean(as.numeric(ix[which(ix$feedback==0),]$relative_accuracy),na.rm = T),
                       earning = sum(ix$outcome * 0.15,na.rm = T),
                       stringsAsFactors = F)
  
  
      return(list(compliance=rx,performance=px))
    })
    tr_info_by_block <- do.call(rbind,lapply(info_by_block,`[[`,"compliance"))
    pr_info_by_block <- do.call(rbind,lapply(info_by_block,`[[`,"performance"))
    #Process compliance
    trial_info_df<-merge(raw_single$sessions,tr_info_by_block,by = "block",all = T)
    trial_info_df<-trial_info_df[,-grep("_ms",names(trial_info_df))]
    trial_info_df$spec <-unlist(apply(trial_info_df[c("block","start_trial","last_trial")],1,list),recursive = F)
  
    trial_info_df<-trial_info_df[c("scheduled_time","start_time","completed_time","duration","delay","spec")]
    trial_info_df$type <- "trials"
  
  
  
    if (max(trials_df$block)>14){
      pr_info_by_block$bad <- NA
      pr_info_by_block$bad[6:nrow(pr_info_by_block)] <- (pr_info_by_block$relative_accuracy_feed<0.7)[6:nrow(pr_info_by_block)]
      percentage_last_ten<-as.numeric(which(pr_info_by_block$bad[(nrow(pr_info_by_block)-9):nrow(pr_info_by_block)])*10)
      percentage_last_ten<-ifelse(length(percentage_last_ten)<1,0,percentage_last_ten)
  
      if (percentage_last_ten>20){
        mean_RT_low_performance=mean(pr_info_by_block$mean_rt[which(pr_info_by_block$bad & c(rep(FALSE,(nrow(pr_info_by_block)-10)),rep(TRUE,10)))])
        mean_side_bias_low_performance=mean(pr_info_by_block$IDe_bias[which(pr_info_by_block$bad & c(rep(FALSE,(nrow(pr_info_by_block)-10)),rep(TRUE,10)))])
      } else {
        mean_RT_low_performance <- NA
        mean_side_bias_low_performance <- NA
      }
    }
    px_overall <- data.frame(ID=raw_single$ID,
                             IDe_bias=mean(trials_df$choice,na.rm = T),
                             mean_rt = mean(trials_df$rt,na.rm = T),
                             abs_accurate_overall = mean(c(pr_info_by_block$abs_accurate_feed,pr_info_by_block$abs_accurate_nofeed),na.rm = T),
                             relative_accuracy_overall = mean(c(pr_info_by_block$relative_accuracy_feed,pr_info_by_block$relative_accuracy_nofeed),na.rm = T),
                             abs_accurate_feed = mean(pr_info_by_block$abs_accurate_feed,na.rm = T),
                             relative_accuracy_feed = mean(pr_info_by_block$relative_accuracy_feed,na.rm = T),
                             abs_accurate_nofeed = mean(pr_info_by_block$abs_accurate_nofeed,na.rm = T),
                             relative_accuracy_nofeed = mean(pr_info_by_block$relative_accuracy_nofeed,na.rm = T),
                             stringsAsFactors = F)
                              #percentage_last_ten=percentage_last_ten,
                              #mean_RT_low_performance=mean_RT_low_performance,
                              #mean_side_bias_low_performance=mean_side_bias_low_performance,
  
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
  
  
    ##Part 0: Session Assignment:
    #### Session information data frame ####
    session_info_df <- raw_single$questionnaires
    session_info_df <- session_info_df[order(session_info_df$scheduled_time),]
    session_info_df$duration<-difftime(session_info_df$completed_time,session_info_df$start_time,units = "mins")
    session_info_df$delay<-difftime(session_info_df$start_time,session_info_df$scheduled_time,units = "mins")
    session_info_df$spec <- unlist(apply(session_info_df[c("type","number","description")],1,list),recursive = F)
    session_info_df<-session_info_df[c("scheduled_time","start_time","completed_time","duration","delay","spec")]
    session_info_df$type <- "questionnaires"
  
    #######Info session##########
    info_df <- rbind(session_info_df,trial_info_df)
    info_df <- info_df[order(info_df$scheduled_time),]
    info_df$ID <- raw_single$ID
    rownames(info_df) <- NULL
    rownames(pr_info_by_block) <- NULL
  
    ###Filter out entries after data pulled:
    info_df <- info_df[which(difftime(info_df$scheduled_time,raw_single$data_mtime) <= 0),]
  
    ##Get start date:
    startdate <- as.Date(info_df$scheduled_time[1],tz = tz)
    info_df$days <- difftime(as.Date(info_df$scheduled_time,tz = tz),startdate,units = "day")
  
    #clean up:
    info_df <- info_df[!duplicated(round(info_df$scheduled_time)),]
    info_df$type[info_df$type=="questionnaires"] <- as.character(sapply(info_df$spec[info_df$type=="questionnaires"],`[[`,3))
    info_df$spec <- NULL
  
    ##For each day
    info_sp<-split(info_df,info_df$days)
  
    #hard code this for now, an expected events and corresponding number for each day:
    expected_df <- data.frame(type = c("Sleep Diary","Mood Questionnaire","Daily recording","trials","5m Resting State"),
                              num = c(1,3,1,2,2),stringsAsFactors = F)
  
  
    info_df<-rbind(info_sp[[1]],do.call(rbind,lapply(info_sp[2:length(info_sp)],function(ifp){
      for(i in 1:nrow(expected_df)) {
        if(length(which(ifp$type == expected_df$type[i])) != expected_df$num[i]) {
          toadd <- expected_df$num[i] - length(which(ifp$type == expected_df$type[i]))
          arg <- as.list(rep(NA,toadd+1))
          arg[[1]] <- ifp
          ifp <- do.call(rbind,arg)
          ifp[(nrow(ifp)-toadd+1):nrow(ifp),c("type","ID","days")] <- data.frame(type=expected_df$type[i],ID=ifp$ID[1],days=ifp$days[1])
        }
      }
      return(ifp)
    })))
  
  
    #update answer DF as well"
    raw_single$answers$session_number<-session_info_df$session_number[match(round(as.numeric(raw_single$answers$answer_time,0)),round(as.numeric(session_info_df$completed_time),0))]
    ##Part II: proc answer df:
    ##Proc questionnaire data here: not yet
    #Find answer:
    form_data <- raw_single$answers
    form_data <- form_data[form_data$answer!="",]
  
    if(nrow(raw_single$sleep)>0) {
      tk <- form_data[rep(1,nrow(raw_single$sleep)),]
      tk$questionnaire_type <- NA
      tk$questionnaire_name <- "Sleep Diary"
      tk$questionnaire_number <- (100-nrow(raw_single$sleep)):99
      tk$question <- 0
      tk$answer <- "sleep latency=NA, woke many times=NA, woke early=NA, overall=NA"
      tk$answer_time <- raw_single$sleep$time
      if(unique(class(tk$answer_time))=="integer64") {
        tk$answer_time <- ms_to_date(tk$answer_time,timezone = tz)
      }
      form_data <- rbind(form_data,tk)
    }
  
    form_data$answer_prog <- text_proc(form_data$answer)
    fdata_sp <- split(form_data,form_data$questionnaire_name)
    #proc all the other first
    form_proc <- lapply(fdata_sp,function(tkd){
      #print(unique(tkd$questionnaire_name))
      if(unique(tkd$questionnaire_name) %in% c("Mood Questionnaire","Sleep Diary","End questionnaire")) {
        evt_q_index <- ifelse(unique(tkd$questionnaire_name) == "Mood Questionnaire",2,1)
        if(unique(tkd$questionnaire_name) == "Mood Questionnaire") {
          evt_q_index <- 2
        } else if (unique(tkd$questionnaire_name) == "Sleep Diary") {
          evt_q_index <- 1
        } else {
          evt_q_index <- 99
        }
        tkf<-do.call(rbind,lapply(split(tkd,tkd$questionnaire_number),function(mda){
          mdb<-do.call(cbind,lapply(mda$answer_prog[mda$question!=evt_q_index],as.data.frame))
          if(evt_q_index %in% mda$question) {
            ###Event df proc
            if(is.null(names(mda$answer_prog[[which(mda$question==evt_q_index)]]))) {
              md_evt <- do.call(rbind,lapply(mda$answer_prog[[which(mda$question==evt_q_index)]],as.data.frame))
            } else {
              md_evt <- as.data.frame(mda$answer_prog[[which(mda$question==evt_q_index)]])
            }
  
            names(md_evt)[names(md_evt)=="V_1"] <- "description"
            names(md_evt)[names(md_evt)=="V_2"] <- "time_ago"
            if(is.null(md_evt$category)) {
              md_evt$category <- "event/activity:unknown"
            }
            names(md_evt)<-gsub(".","_",names(md_evt),fixed = T)
            mdb$event_df <- list(event_df=md_evt)
            mdb$number_of_events <- nrow(mdb$event_df$event_df)
          } else {
            mdb$event_df <- NA
            mdb$number_of_events <- 0
          }
          mdc <- cbind(mda[1,c("questionnaire_name","questionnaire_type","answer_time")],mdb)
          return(mdc)
        }))
      } else {
        tke<-do.call(rbind,lapply(tkd$answer_prog,as.data.frame,sep="_"))
        tkf <- cbind(tkd[c("questionnaire_name","questionnaire_type","answer_time","question")],tke)
      }
      names(tkf)<-gsub(".","_",names(tkf),fixed = T)
      tkf$ID <- raw_single$ID
      #tkf[tkf=="NA"] <- NA
      tkf <- tkf[order(tkf$answer_time),]
      rownames(tkf)<-NULL
      return(tkf)
    })
  
    form_proc$`Mood Questionnaire`$v_a_distance <- sqrt((as.numeric(form_proc$`Mood Questionnaire`$Valence)^2) + (as.numeric(form_proc$`Mood Questionnaire`$Arousal)^2) )
    form_proc$`Sleep Diary`$did_not_sleep<-is.na(form_proc$`Sleep Diary`$questionnaire_type)
    if(!is.null(form_proc$`End questionnaire`)) {
      names(form_proc$`End questionnaire`)[grepl("X[[i]]",names(form_proc$`End questionnaire`),fixed = T)] <- paste0("V",1:length(which(grepl("X[[i]]",names(form_proc$`End questionnaire`),fixed = T))))
      form_proc$`End questionnaire`$event_df <- NULL
      form_proc$`End questionnaire`$number_of_events <- NULL
    }
    ##summary stats for how much they answered
    q_sum <- data.frame(ID = raw_single$ID,val_arr_dis_avg = mean(form_proc$`Mood Questionnaire`$v_a_distance,na.rm = T))
    e_sum <- data.frame(as.list(apply(form_proc$`Mood Questionnaire`[c("Anxious","Elated","Sad","Irritable","Energetic")],2,function(x){mean(as.numeric(x),na.rm = T)})))
    names(e_sum) <- paste(names(e_sum),"avg",sep = "_")
    s_sum <- data.frame(as.list(apply(form_proc$`Sleep Diary`[c("sleep_latency","woke_many_times","woke_early","overall")],2,function(x){mean(as.numeric(x),na.rm = T)})))
    names(s_sum) <- paste(names(s_sum),"avg",sep = "_")
  
    q_sum <- cbind(q_sum,e_sum,s_sum)
    q_sum$emo_rate_avg <- mean(unlist(e_sum),na.rm = T)
    q_sum$sleep_di_avg <- mean(unlist(s_sum),na.rm = T)
    q_sum$avg_evt_num <- mean(form_proc$`Mood Questionnaire`$number_of_events)
    q_sum$avg_sleep_evt_num <- mean(form_proc$`Sleep Diary`$number_of_events)
    q_sum$num_no_sleep <- length(which(form_proc$`Sleep Diary`$did_not_sleep))
    q_sum$val_emo_cor <- cor(apply(form_proc$`Mood Questionnaire`[c("Anxious","Elated","Sad","Irritable","Energetic")],1,function(x){mean(as.numeric(x),na.rm = T)}),form_proc$`Mood Questionnaire`$v_a_distance)
    ###return proc_answer object###########
  
  
    pr_info_by_block$ID <- raw_single$ID
    raw_single$trials <- trials_df
  
    output <- list(raw_data=raw_single,
                   info_df = info_df,
                   performance_info=pr_info_by_block,performance_overall=px_overall,
                   form_dfs=form_proc,form_summary=q_sum,
                   ID=raw_single$ID)
    save(output, file = output_path)
  
    return(list(raw_data=raw_single, new_data=TRUE,
                info_df = info_df,
                performance_info=pr_info_by_block,performance_overall=px_overall,
                form_dfs=form_proc,form_summary=q_sum,
                ID=raw_single$ID))
  }, error = function(err){
    traceback()
    print(paste0(raw_single$ID, " did not successfully have their schedule file processed."))
    return(NA)
  })
}
####Proc physio
proc_physio <- function(physio_df = NULL,sch_pro_output=NULL, tz="EST", thread=4,
                        force_reload=FALSE,force_reproc=FALSE,save_lite=F,
                        eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
                        ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
                        ) {
  if (.Platform$OS.type == "windows") {
    message("Forking is not available on windows. Parallelization will be turned off.")
    thread=1
  }
  # modify physio_df to have physio_df$file_path include the file name
  #physio_df <- within(physio_df, file_path <- paste0(file_path, '/', file_name))

  exp_out<-lapply(unique(physio_df$subject_id),function(IDx){
    physio_files_new <- physio_df$file_path[physio_df$subject_id==IDx]
    physio_rawcache_file <- file.path(unique(dirname(physio_df$file_path[physio_df$subject_id==IDx])),paste(IDx,"_physio_raw.rdata",sep = ""))
    physio_proc_file <- file.path(unique(dirname(physio_df$file_path[physio_df$subject_id==IDx])),paste(IDx,"_physio_proc.rdata",sep = ""))

    if(!force_reload && file.exists(physio_rawcache_file)) {
      load(physio_rawcache_file)
      physio_files_diff <- physio_files_new[!physio_files_new %in% physio_files]
      message("Found ",length(physio_files_diff), " new physio files for: ",IDx)
    } else {
      physio_files_diff <- physio_files_new
      physio_concat <- NULL
      physio_files <- NULL
    }
    if(length(physio_files_diff)>0) {
      #Load the physio data, para for muiltiple files
      par_cl <- parallel::makeCluster(spec = thread,type = "FORK")
      message("Loading new physio data for: ",IDx)
      physio_concat <- load_physio_single(allpaths_sub = physio_files_diff,old_data=physio_concat,cl = par_cl)
      parallel::stopCluster(par_cl)
      physio_files<-unique(c(physio_files,physio_files_new))
      save(physio_files,physio_concat,file = physio_rawcache_file)
    }

    if(!force_reproc && length(physio_files_diff) < 1 && file.exists(physio_proc_file)) {
      message("Loading processed physio data for: ",IDx)
      load(physio_proc_file)
      output$new_data <- FALSE
      if(save_lite) {
        output <- output[c("new_data","ID","eeg_fb","eeg_summary", "eeg_ov","ecg_fb","ecg_summary", "ecg_ov")]
        output$lite <- TRUE
      } else {
        output$lite <- FALSE
      }
      return(output)
    }

    #Get the matching behavioral data
    behav_df <- output$proc_data[[IDx]]$raw_data$trials
    behav_df <- behav_df[which(!is.na(behav_df$stim_time)),]
    if(is.null(behav_df$session_number)) {
      behav_df$session_number <- NA
    }
    sess_map<-unique(behav_df[c("block","session_number")])

    ###EEG
    message("Processing new EEG data for: ",IDx)
    eeg_list <- load_EEG(EEGd = physio_concat$eeg,sample_rate = eeg_sample_rate,sd_times = sd_times)
    eeg_raw <- eeg_list[[1]]
    eeg_missing <- eeg_list[[2]]
    eeg_fb <- eeg_epochs_around_feedback(EEG_data = eeg_raw,
                                           pre = eeg_pre,post = eeg_post,sample_rate = eeg_sample_rate,
                                           fbt = as.numeric(behav_df$feedback_time)*1000)
    eeg_rawsum <- get_good_EEG(blocks=behav_df$block,a2f=eeg_fb,sd_times=sd_times)
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
    message("Processing new ECG data for: ",IDx)
    ecg_raw <- load_ECG(ECGd = physio_concat$ecg,HRstep = HRstep,sample_rate = ecg_sample_rate)
    ecg_fb <- ecg_epochs_around_feedback2(ECG_data = ecg_raw,fbt = as.numeric(behav_df$feedback_time)*1000,
                                         pre = ecg_pre,post = ecg_post,sample_rate = ecg_sample_rate,thread=thread)
    ecg_summary <- get_good_ECG(blocks = behav_df$block,ch1_a2f = ecg_fb)
    ecg_summary$session_number<-sess_map$session_number[match(ecg_summary$block,sess_map$block)]
    ecg_summary$ID <- IDx
    ecg_summary <- ecg_summary[order(names(ecg_summary))]
    ecg_ov <- aggregate(per_Good ~ ID,data = ecg_summary,FUN = mean,na.rm=T)
    ecg_ov$worst_allblocks <- min(ecg_summary$per_Good)

    output <- list(new_data=TRUE,ID=IDx,lite=F,
                   eeg_proc = eeg_raw,eeg_fb = eeg_fb, eeg_summary = eeg_summary, eeg_ov = eeg_ov, eeg_missing = eeg_missing, eeg_rawsum = eeg_rawsum,
                   ecg_proc = ecg_raw,ecg_fb = ecg_fb, ecg_summary = ecg_summary, ecg_ov = ecg_ov)
    save(output,file = physio_proc_file)

    if(save_lite) {
      output <- list(new_data=TRUE,ID=IDx,lite=T,
                     eeg_fb = eeg_fb,eeg_summary = eeg_summary, eeg_ov = eeg_ov,eeg_missing = eeg_missing, eeg_rawsum = eeg_rawsum,
                     ecg_fb = ecg_fb,ecg_summary = ecg_summary, ecg_ov = ecg_ov)

      return(output)
    } else {
      return(output)
    }




  })
  if(save_lite) {
    nax <- c("fb","summary")
  } else {
    nax <- c("proc","fb","summary")
  }

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
  output_fin$newdata_IDs <- IDlist[sapply(exp_out,`[[`,"new_data")]
  return(output_fin)
}

load_physio_single <- function(allpaths_sub,old_data=NULL,cl=NULL) {
  if(is.null(cl)) {
    all_dt <- lapply(allpaths_sub,load_db,table_names=c("EEG_muse","Polar_heartrate"))
  } else {
    all_dt <- parallel::parLapply(cl,allpaths_sub,load_db,table_names=c("EEG_muse","Polar_heartrate"))
  }
  eeg_aggregate <- do.call(rbind,lapply(all_dt,`[[`,"EEG_muse"))
  ecg_aggregate <- do.call(rbind,lapply(all_dt,`[[`,"Polar_heartrate"))
  if(!is.null(old_data)){
    eeg_aggregate <- rbind(old_data$eeg,eeg_aggregate)
    ecg_aggregate <- rbind(old_data$ecg,ecg_aggregate)
  }
  eeg_aggregate <- eeg_aggregate[order(eeg_aggregate$recording_time),]
  ecg_aggregate <- ecg_aggregate[order(ecg_aggregate$time_ms),]
  return(list(eeg=eeg_aggregate,ecg=ecg_aggregate))
}

###Proc questionnaire, just in case it gets complicated:
text_proc <- function(input_text = NULL) {
  type_indx <- rep(NA,length(input_text))
  output_ls <- as.list(rep(NA,length(input_text)))
  ###Get the video string:
  ###It could be faster to not use index, however, some gsub might work differently
  type_indx[nchar(input_text)==1] <- "single_digit"
  type_indx[grepl(".mp4",input_text,fixed = T)] <- "video"
  type_indx[grepl("\t",input_text,fixed = T)] <- "tx1"
  type_indx[grepl(", ",input_text,fixed = T)] <- "tx2"
  type_indx[grepl("\n",input_text,fixed = T)] <- "ls1"
  type_indx[grepl("category=",input_text,fixed = T)] <-"ls2"

  output_ls[type_indx=="single_digit"]<-as.numeric(input_text[type_indx=="single_digit"])
  output_ls[type_indx=="video"]<-sapply(input_text[type_indx=="video"],list,USE.NAMES = F)
  output_ls[type_indx=="tx1"]<-lapply(strsplit(input_text[type_indx=="tx1"],"\t"),split_n_name)
  output_ls[type_indx=="tx2"]<-lapply(strsplit(input_text[type_indx=="tx2"],", "),split_n_name)
  output_ls[type_indx=="ls1"]<-lapply(strsplit(input_text[type_indx=="ls1"],"\n"),function(x){
    lapply(strsplit(x,"\t"),split_n_name)
  })
  input_text[type_indx=="ls2"] <- gsub("\ncategory=","/NTcategory=",input_text[type_indx=="ls2"],fixed=T)
  output_ls[type_indx=="ls2"]<-lapply(strsplit(input_text[type_indx=="ls2"],"/NT"),function(x){
    ##fix some description punchuation problem with ,
    lapply(strsplit(gsub("\n",":",x),","),function(x){
      #toclean:
      xi <-which(!grepl("=",x))
      if(length(xi)>0){
        for (i in xi){
          x[i-1] <- paste(x[i-1],x[i],sep = ", ")
        }
        x <- x[-xi]
      }

      split_n_name(x)
    })
  })
  if(any(is.na(type_indx))) {
    message("Unsupported string input")
  }
  return(output_ls)
}

split_n_name <- function(x){
  names(x) <- paste("V",1:length(x),sep = "_")
  cp <- strsplit(x[grepl("=",x)],"=")
  names(x)[grepl("=",x)]<-sapply(cp,`[[`,1)
  x[grepl("=",x)]<-sapply(cp,`[[`,2)
  return(as.list(x))
}





