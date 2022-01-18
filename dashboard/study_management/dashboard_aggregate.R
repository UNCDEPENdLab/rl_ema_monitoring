####Souce dependent functions:
#root_dir = getwd()
root_dir <- getwd()
repo_path <- dirname(root_dir)
if(exists("failed") == FALSE){
  failed <<- list()
}
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
  output_physio <- proc_physio(physio_df = path_info$physio, tz="EST",thread=4,
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

# Notes (AndyP)
# load_db loads data from a database file, e.g. one physio file or a schedule file
#


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
  proc_data <<- lapply(raw_data,proc_schedule_single,tz=tz,days_limit=days_limit,force_reproc=force_reproc)
  # drop any NA produced
  #proc_data <- proc_data[!is.na(proc_data)]
  names(proc_data) <- sapply(proc_data,`[[`,"ID")
  #print(proc_data)
  # drop any items in the raw_data that failed schedule processing
  #raw_data_dropped <-
  ####do more aggregation here:
  #####NEED MORE SUBJ DATA FOR AGGREGATION########
  sample_info <- do.call(rbind,lapply(proc_data,`[[`,"info_df"))
  performance_info <- do.call(rbind,lapply(proc_data,`[[`,"performance_info"))
  performance_info <- performance_info[!is.na(performance_info$date),]
  #browser()
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

#function to get start_time for game once split into individual session
get_start_game <- function(games){
  # get the first stim time from the tibble
  start_time <- games %>% pull(stim_time) %>% '[['(1)
  # return the start time
  return(as.POSIXct(start_time/1000, origin="1970-01-01", tz="America/New_York"))
}

#function to get start_time for game once split into individual session
get_scheduled_game <- function(games){
  # get the first schedule time from the tibble
  scheduled_time <- games %>% pull(scheduled_time) %>% '[['(1)
  # return the schedule time
  return(as.POSIXct(scheduled_time/1000, origin="1970-01-01", tz="America/New_York"))
}

#function to get start_time for game once split into individual session
get_completed_game <- function(games){
  # get the last choice time from the tibble
  completed_time <- games %>% pull(choice_time) %>% tail(n=1)
  # return the completed time
  return(as.POSIXct(completed_time/1000, origin="1970-01-01", tz="America/New_York"))
}

#function to get start_time for game once split into individual session
get_block_game <- function(games){
  # arbitrarily grab the block from the first row of the block column
  block <- games %>% pull(block) %>% '[['(1)
  # return the block
  return(block)
}

# function to check if zero
is.zero <- function(x) {
  if(x == 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# function to convert the times from the questionnaire
get_end_q_time <- function(questionnaire) {
  # get the last choice time from the tibble
  end_time <- questionnaire %>%
    filter(description == 'End questionnaire') %>%
    '$'(scheduled_time) %>%
    lubridate::as_datetime()
  # return the completed time
  return(end_time)
}

# function to convert booleans to 'AM' or 'PM'
get_meridiem <- function(bool_item) {
  if(bool_item == TRUE) {
    return("AM")
  } else {
    return("PM")
  }
}

# function to get the game session
get_game_session <- function(session_tibble) {
  session <- tibble(
    start_time = min(session_tibble$start_time), # take the start time of the first block
    scheduled_time = min(session_tibble$scheduled_time), # take the scheduled time of the first block
    completed_time = max(session_tibble$completed_time), # take the completed time of the second block
    ID = session_tibble$ID, # get the ID, arbitrarily chosen as the first one
    days = session_tibble[[1, 'days']], # get the days, arbitrarily chosen as the first one
    weekday = session_tibble[[1, 'weekday']], # get the weekday, arbitrarily chosen as the first one
    meridiem = session_tibble[[1, 'meridiem']], # get the meridiem, arbitrarily chosen as the first one
    type = session_tibble[[1, 'type']], # get the type, arbitrarily chosen as the first one
    duration = sum(session_tibble$duration), # get the sums of the durations as the total duration
    delay = session_tibble[[1, 'delay']], # get the delay as the first delay
    missing = !(FALSE %in% session_tibble$missing), # if there is a FALSE in missing, this is TRUE, otherwise FALSE -> at least one game was found (so not missing)
    completed = !(TRUE %in% session_tibble$missing) # if there is a TRUE in missing, this is TRUE, otherwise, FALSE -> at least one game was not completed (so not completed)
  )
  # return the tibble
  return(unique(session))
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

    raw_games <- raw_single$trials
    raw_questionnaires <- raw_single$questionnaires

    ###Part I: Trial
    time_vars <- c("scheduled_time","stim_time","choice_time","feedback_time")
    #browser()
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
    } else {
      pr_info_by_block$bad <- NA
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
    #session_info_df['spec'] <- unlist(session_info_df['spec'])
    session_info_df$type <- "questionnaires"

    #######Info session##########
    #######DEPRECATED############
    # # replace w/A's code starting here #
    # info_df <- rbind(session_info_df,trial_info_df)
    # info_df <- info_df[order(info_df$scheduled_time),]
    # info_df$ID <- raw_single$ID
    # rownames(info_df) <- NULL
    # rownames(pr_info_by_block) <- NULL
    #
    # ###Filter out entries after data pulled:
    # info_df <- info_df[which(difftime(info_df$scheduled_time,raw_single$data_mtime) <= 0),]
    #
    # ##Get start date:
    # startdate <- as.Date(info_df$scheduled_time[1],tz = tz)
    # info_df$days <- difftime(as.Date(info_df$scheduled_time,tz = tz),startdate,units = "day")
    #
    # #clean up:
    # info_df <- info_df[!duplicated(round(info_df$scheduled_time)),]
    # info_df$type[info_df$type=="questionnaires"] <- as.character(sapply(info_df$spec[info_df$type=="questionnaires"],`[[`,3))
    # info_df$spec <- NULL
    #
    # ##For each day
    # info_sp<-split(info_df,info_df$days)
    #
    # #hard code this for now, an expected events and corresponding number for each day:
    # expected_df <- data.frame(type = c("Sleep Diary","Mood Questionnaire","Daily recording","trials","5m Resting State"),
    #                           num = c(1,4,1,2,2),stringsAsFactors = F)
    #
    # # expected_df <- data.frame(type = c("Sleep Diary","Mood Questionnaire","Daily recording","trials","5m Resting State", "Mood Post Task"),
    # #                           num = c(5,0,21,2,2,),stringsAsFactors = F)
    # #
    #
    # info_df<-rbind(info_sp[[1]],do.call(rbind,lapply(info_sp[2:length(info_sp)],function(ifp){
    #   for(i in 1:nrow(expected_df)) {
    #     if(length(which(ifp$type == expected_df$type[i])) != expected_df$num[i]) {
    #       toadd <- expected_df$num[i] - length(which(ifp$type == expected_df$type[i]))
    #       arg <- as.list(rep(NA,toadd+1))
    #       arg[[1]] <- ifp
    #       ifp <- do.call(rbind,arg)
    #       ifp[(nrow(ifp)-toadd+1):nrow(ifp),c("type","ID","days")] <- data.frame(type=expected_df$type[i],ID=ifp$ID[1],days=ifp$days[1])
    #     }
    #   }
    #   return(ifp)
    # })))
    #######DEPRECATED############

    #######Get info_df###########
    # get the game data
    games <- raw_games %>%
      #filter(block > 5) %>% # drop setup blocks
      filter(block < 1000) # drop fMRI blocks
    # get the games separated into blocks
    games_by_block <- games %>% group_split(block)
    # get the start time
    start_time = sapply(games_by_block, get_start_game)
    # get the scheduled time
    scheduled_time = sapply(games_by_block, get_scheduled_game)
    # get the completed time
    completed_time = sapply(games_by_block, get_completed_game)
    # get the blocks
    each_block <- sapply(games_by_block, get_block_game)
    # create a tibble of the pulled start, scheduled, and completed times
    games_by_block <- tibble(
      start_time = start_time,
      scheduled_time = scheduled_time,
      completed_time = completed_time,
      block = each_block) %>%
      mutate(ID = raw_single$ID) %>% # append the subject id column
      mutate(type = "Games") %>% # add the description
      mutate(start_time = as.POSIXct(start_time/1, origin="1970-01-01", tz="America/New_York")) %>%
      mutate(scheduled_time = as.POSIXct(scheduled_time/1, origin="1970-01-01", tz="America/New_York")) %>%
      mutate(completed_time = as.POSIXct(completed_time/1, origin="1970-01-01", tz="America/New_York")) %>%
      mutate(delay = round((start_time - scheduled_time), 2)) %>% # get the delay
      mutate(duration = round(completed_time - start_time, 2)) %>% # get the duration
      mutate(missing = is.na(completed_time)) %>% # get missingness
      mutate(weekday = weekdays(scheduled_time)) %>% # get the weekday
      mutate(meridiem = lubridate::am(scheduled_time) %>% sapply(get_meridiem)) %>% # get the meridiem
      mutate(type = paste0(type, ' ', meridiem)) # rename the games to include time

    # get the start date, NOTE: this is actually the day before to ensure accessing all element, decremented by 1 later
    start_date <- games_by_block %>% filter(block == 0) %>% '$'(scheduled_time) %>% '-'(lubridate::days())

    # get the questionnaire data
    questionnaires_by_session <- raw_questionnaires %>%
      as_tibble() %>%
      mutate(ID = raw_single$ID) %>% # append the subject id column
      mutate(start_time = as.POSIXct(start_time/1000, origin="1970-01-01", tz="America/New_York")) %>%
      mutate(scheduled_time = as.POSIXct(scheduled_time/1000, origin="1970-01-01", tz="America/New_York")) %>%
      mutate(completed_time = as.POSIXct(completed_time/1000, origin="1970-01-01", tz="America/New_York")) %>%
      mutate(delay = round((start_time - scheduled_time), 2)) %>% # get the delay
      mutate(duration = round(completed_time - start_time, 2)) %>% # get the duration
      mutate(missing = is.na(completed_time)) %>% # get missingness
      mutate(completed = !(is.na(completed_time))) %>% # get the completeness
      mutate(weekday = weekdays(scheduled_time)) %>% # get the weekday
      mutate(meridiem = lubridate::am(scheduled_time) %>% sapply(get_meridiem)) %>% # get the meridiem
      mutate(type = description) %>% # rename description
      mutate(start_date = start_date) %>%
      mutate(days = lubridate::day(scheduled_time) - lubridate::day(start_date) - 1)

    # End get the datetime of the most recent game session
    # If completed, then end of experiment questionnaire time
    # It's the earlier date of the two cases to work for all participants.
    if ('End questionnaire' %in% questionnaires_by_session$description) {
      drop_after <<- get_end_q_time(questionnaires_by_session)
    } else { # If currently active, then dashboard time, which will already be set at start of run.
      drop_after <<- dashboard_start_time
    }

    # filter out any contents after the drop date from the games and questionnaires
    questionnaires_by_session <- questionnaires_by_session %>% filter(scheduled_time < drop_after)
    games_by_block <- games_by_block %>% filter(scheduled_time < drop_after)

    # get the EMA day each task is on
    questionnaires_by_session <- questionnaires_by_session %>% mutate(days = round(difftime(scheduled_time, start_date, units='days') - 1,0))
    games_by_block <- games_by_block %>% mutate(days = round(difftime(scheduled_time, start_date, units='days') - 1,0))

    # add a block column to the questionnaire (just set all to -1...blocks are useful to keep for eeg data)
    #questionnaires_by_session <- questionnaires_by_session%>% mutate(block = -1)

    # squish the games to have AM and PM be a single item
    games_by_session <- games_by_block %>% group_split(type,days)
    #browser()
    games_by_session <- do.call(rbind, lapply(games_by_session, get_game_session))

    # get the info df (merge of games_by_session and questionnaires_by_session), concert this back to a base dataframe to match J's
    merge_items <- c('start_time', 'scheduled_time', 'completed_time', 'ID', 'days', 'weekday', 'meridiem', 'duration', 'delay', 'missing', 'completed', 'type') # , 'block'
    games_merge_object <- games_by_session %>% select(merge_items)
    questionnaires_merge_object <- questionnaires_by_session %>% select(merge_items)
    info_df <- rbind(questionnaires_merge_object, games_merge_object)
    # drop any items without a completed_time to cleanup instances where participants are dropped
    info_df <- info_df %>% filter(!is.na(completed_time))
    # convertv back to base dataframe from tibble
    info_df <- as.data.frame(info_df)
    #######Get info_df###########

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

    #browser()

    form_data$answer_prog <- text_proc(form_data$answer)
    fdata_sp <- split(form_data,form_data$questionnaire_name)

    #browser()

    ## REWRITE form_proc: FROM HERE ##
    #proc all the other first
    # form_proc <- lapply(fdata_sp,function(tkd){
    #   #print(unique(tkd$questionnaire_name))
    #   if(unique(tkd$questionnaire_name) %in% c("Mood Questionnaire","Sleep Diary","End questionnaire")) {
    #     evt_q_index <- ifelse(unique(tkd$questionnaire_name) == "Mood Questionnaire",2,1)
    #     if(unique(tkd$questionnaire_name) == "Mood Questionnaire") {
    #       evt_q_index <- 2
    #     } else if (unique(tkd$questionnaire_name) == "Sleep Diary") {
    #       evt_q_index <- 1
    #     } else {
    #       evt_q_index <- 99
    #     }
    #     tkf<-do.call(rbind,lapply(split(tkd,tkd$questionnaire_number),function(mda){
    #       mdb<-do.call(cbind,lapply(mda$answer_prog[mda$question!=evt_q_index],as.data.frame))
    #       if(evt_q_index %in% mda$question) {
    #         ###Event df proc
    #         if(is.null(names(mda$answer_prog[[which(mda$question==evt_q_index)]]))) {
    #           md_evt <- do.call(rbind,lapply(mda$answer_prog[[which(mda$question==evt_q_index)]],as.data.frame))
    #         } else {
    #           md_evt <- as.data.frame(mda$answer_prog[[which(mda$question==evt_q_index)]])
    #         }
    #
    #         names(md_evt)[names(md_evt)=="V_1"] <- "description"
    #         names(md_evt)[names(md_evt)=="V_2"] <- "time_ago"
    #         if(is.null(md_evt$category)) {
    #           md_evt$category <- "event/activity:unknown"
    #         }
    #         names(md_evt)<-gsub(".","_",names(md_evt),fixed = T)
    #         mdb$event_df <- list(event_df=md_evt)
    #         mdb$number_of_events <- nrow(mdb$event_df$event_df)
    #       } else {
    #         mdb$event_df <- NA
    #         mdb$number_of_events <- 0
    #       }
    #       mdc <- cbind(mda[1,c("questionnaire_name","questionnaire_type","answer_time")],mdb)
    #       return(mdc)
    #     }))
    #   } else {
    #     tke<-do.call(rbind,lapply(tkd$answer_prog,as.data.frame,sep="_"))
    #     tkf <- cbind(tkd[c("questionnaire_name","questionnaire_type","answer_time","question")],tke)
    #   }
    #   names(tkf)<-gsub(".","_",names(tkf),fixed = T)
    #   tkf$ID <- raw_single$ID
    #   #tkf[tkf=="NA"] <- NA
    #   tkf <- tkf[order(tkf$answer_time),]
    #   rownames(tkf)<-NULL
    #   return(tkf)
    # })
    ## REWRITE form_proc: TO HERE ##

    # form_proc$`Mood Questionnaire`$v_a_distance <- sqrt((as.numeric(form_proc$`Mood Questionnaire`$Valence)^2) + (as.numeric(form_proc$`Mood Questionnaire`$Arousal)^2) )
    # form_proc$`Sleep Diary`$did_not_sleep<-is.na(form_proc$`Sleep Diary`$questionnaire_type)
    # if(!is.null(form_proc$`End questionnaire`)) {
    #   names(form_proc$`End questionnaire`)[grepl("X[[i]]",names(form_proc$`End questionnaire`),fixed = T)] <- paste0("V",1:length(which(grepl("X[[i]]",names(form_proc$`End questionnaire`),fixed = T))))
    #   form_proc$`End questionnaire`$event_df <- NULL
    #   form_proc$`End questionnaire`$number_of_events <- NULL
    # }

    clean_daily_recording <- function(fdata_sp, subj_id) {
      # run the cleaning
      fdata_sp$`Daily recording` <- fdata_sp$`Daily recording` %>%
        # remove these items
        select(-answer_prog, -questionnaire_number, -answer_timestamp) %>%
        # rename answer
        rename('X[[i]]'=answer) %>%
        # add an ID column
        mutate(ID=subj_id)
      # return the updated object
      return(fdata_sp)
    }

    clean_end_questionnaire <- function(fdata_sp, subj_id) {
      # if the end questionnaire has not been completed yet
      end_q_complete = "End questionnaire" %in% names(fdata_sp)
      if(!end_q_complete) {
        # just return the data
        return(fdata_sp)
      }
      # get the answer list
      answer_list <- fdata_sp$`End questionnaire`$answer_prog
      # rename these to all be 'X[[i]]'
      names(answer_list) <- paste0("V", 1:length(answer_list))
      # run the cleaning
      end_q_data <- fdata_sp$`End questionnaire` %>%
        # remove these items
        select(-answer_prog,
               -questionnaire_number,
               -answer_timestamp,
               -answer,
               -question) %>%
        # just select the first elements
        "["(1,) %>%
        # convert to a list
        as.list
      # add the id
      end_q_data['ID'] <- subj_id
      # add the answers
      end_q_data <- append(end_q_data, answer_list)
      # apply these changes to the original df and convert list to a dataframe
      fdata_sp$`End questionnaire` <- as.data.frame(end_q_data)
      # return the updated object
      return(fdata_sp)
    }

    clean_mood_post_task <- function(fdata_sp, subj_id) {
      # get the answer list
      answer_list <- fdata_sp$`Mood post task`$answer_prog
      # unlist the answer_list items
      answer_list <- lapply(answer_list, unlist)
      # run the cleaning
      mood_post_task_data <- fdata_sp$`Mood post task` %>%
        # remove these items
        select(-answer_prog,
               -questionnaire_number,
               -answer_timestamp,
               -answer)
      # get the answer_prog as a dataframe
      answer_list <- t(do.call(cbind, answer_list))
      # combine the mood answer list with the mood post task data
      mood_post_task_data <- cbind(mood_post_task_data, answer_list)
      # add the id
      mood_post_task_data['ID'] <- subj_id
      # update the overall object
      fdata_sp$`Mood post task` <- mood_post_task_data
      # return the updated object
      return(fdata_sp)
    }

    clean_mood_questionnaire <- function(fdata_sp, subj_id) {
      # get the answer list
      mood_answers <- fdata_sp$`Mood Questionnaire`$answer_prog
      # unlist the mood_answers items
      mood_answers <- lapply(mood_answers, unlist)
      # coerce the data as a list into a column-based list
      mood_answers <- t(as.data.frame(t(mood_answers)))
      # get the number of items
      num_items <- length(fdata_sp$`Mood Questionnaire`$question)/3
      # coerce the data as a list into a dataframe and add an index
      #' Note: the following dataframe has column 'V1' as the index we added
      #' and 'V2' as the actual data. Indexes of 1 and 2 are added and further
      #' defined in the next multiline comment.
      mood_answers <- as.data.frame(
        cbind(
          rep(c(1,1,2), num_items),
          mood_answers)
      )
      # get a grouping
      #' Note: the mood questionnaire data exists in triples:
      #'   A: Valence and Arousual (1)
      #'   B: Anxious, Elated, Sad, Irritable, Energetic (1)
      #'   C: events/thoughts (2)
      #' Next bit of logic is to separate A+B and C into separate entities

      # get the ema data
      #' Here, the data is split by staggered in order of valence-arousal and
      #' emotional like so:
      #'   Index 1 is valence-arousal
      #'   Index 0 is emotion
      #' V1 is this index and V2 is the actual data
      ema_data <- mood_answers %>% filter(V1==1) %>% mutate(V1=c(1:nrow(.))%%2)
      # get the valence-arousal data
      val_data <- ema_data %>% filter(V1==1) %>%
        '['('V2') %>% # get the data from V2
        as.list %>% # convert to a list
        "[["(1) %>% # unlist
        as.data.frame # convert to dataframe
      # get the emotion data
      emo_data <- ema_data %>% filter(V1==0) %>%
        '['('V2') %>% # get the data from V2
        as.list %>% # convert to a list
        "[["(1) %>% # unlist
        as.data.frame # convert to dataframe
      # get the event data
      event_data <- mood_answers %>% filter(V1==2)
      # reformatting of the data
      event_data <- event_data$V2 %>%
        lapply(.,
               combine_by_response <- function(x) { # one-time use function
                 #browser()
                 colNames <- as.list(unique(names(x))) # get the names for the columns
                 x <- matrix(unlist(x), ncol = length(x)/8, nrow = 8) %>% # convert to a matrix
                   t %>% # transpose the matrix
                   as.data.frame # convert to a dataframe
                 colnames(x) <- colNames # apply the column names
                 return(x) # return
               }
        )
      # get the custom indices for the mood_questionnaire
      mq_idx <- fdata_sp$`Mood Questionnaire`$question
      # run the cleaning
      mood_questionnaire_data <- fdata_sp$`Mood Questionnaire` %>%
        # remove these items
        select(-answer_prog,
               -questionnaire_number,
               -answer_timestamp,
               -answer,
               -question)
      # drop the extra times
      #' Note: we get 3 identical times for each entry, but for some
      #' reason using distinct or unique functions do not handle this.
      #' Instead, we just assign indices 1, 2, and 3 and just drop 2 and 3.
      mood_questionnaire_data <- as.data.frame(
        cbind(
          mq_idx,
          mood_questionnaire_data)
      ) %>% filter(mq_idx == 1) %>% # remove the two extra copies
        select(-mq_idx) # drop the temporary index we added
      # add the valence-arousal data back to object
      mood_questionnaire_data <- cbind(mood_questionnaire_data, t(val_data))
      # add emotion data back to object
      mood_questionnaire_data <- cbind(mood_questionnaire_data, t(emo_data))
      # add the id
      mood_questionnaire_data['ID'] <- subj_id
      # update the overall object
      fdata_sp$`Mood Questionnaire` <- mood_questionnaire_data
      # add the events back
      fdata_sp$`Mood Questionnaire`$event_df <- event_data
      # add the number of events
      fdata_sp$`Mood Questionnaire`$number_of_events <- sapply(fdata_sp$`Mood Questionnaire`$event_df, nrow)
      # add the v_a_distance
      fdata_sp$`Mood Questionnaire`$v_a_distance <- sqrt((as.numeric(fdata_sp$`Mood Questionnaire`$Valence)^2) + (as.numeric(fdata_sp$`Mood Questionnaire`$Arousal)^2) )
      # return the updated object
      return(fdata_sp)
      #' Note that this implementation does not have the redundant event_df naming.
      #' This likely shouldn't hurt anything.
    }

    clean_sleep_diary <- function(fdata_sp, subj_id) {
      # get the answer list
      sleep_answers <- fdata_sp$`Sleep Diary`$answer_prog
      #browser()
      # unlist the sleep_answers items
      sleep_answers <- lapply(sleep_answers, unlist)
      # coerce the data as a list into a column-based list
      #' After this, V1 will be the raw string result and V2 will be the length
      sleep_answers <- t(as.data.frame(t(sleep_answers))) %>%
        as.data.frame %>%
        #rowwise %>%
        mutate("V2"=fdata_sp$`Sleep Diary`$question)
      # get the sleep quality (has a idx of 0)
      sleep_quality <- sleep_answers %>% filter(V2==0) %>%
        '['('V1') %>% # get the data from V1
        as.list %>% # convert to a list
        "[["(1) %>% # unlist
        as.data.frame %>% # convert to dataframe
        ( # run custom function to drop NA (requires saving and reapplying rowname)
          function(x) {
            # save the rownames
            colNames <- rownames(x)
            x <- x %>% naniar::replace_with_na_all(~.x == "NA") %>% # convert "NA" to NA
              t %>% # transpose
              as.data.frame #%>% # convert to dataframe
            #drop_na #%>% # drop NAs
            #t %>% # transpose
            #as.data.frame # convert to dataframe
            # re-apply the colnames
            colnames(x) <- colNames
            # rerurn the data
            return(x)
          }
        )
      # get the dream log (has a idx of 1)
      dream_log <- sleep_answers #%>% filter(V2==1)
      # complicated chaining of methods and regexes to pull out the data
      # dream_log <- str_split(toString(dream_log$V1), "c[(]") %>%
      #   lapply(., str_remove_all, "\"") %>% # remove '\'
      #   lapply(., str_remove_all, "`") %>% # remove '`'
      #   lapply(., str_remove_all, "[)][,]*") %>% # remove '),'
      #   lapply(., str_remove_all, "[\n]*") %>% # remove '\n'
      #   lapply(., str_split, ',') %>% # split by ',' to get name-value pairs
      #   lapply(., lapply, str_split, "=") %>% # split by '=' to separate name and value
      #   lapply(., lapply, lapply, str_trim) # remove extra white space
      # # delimit from the extra unneeded layer
      # dream_log <- dream_log[[1]] #%>% '[['(2:length(.))
      # # remove the first element, it's an empty string
      # dream_log[[1]] <- NULL
      # reformatting of the data
      dream_log <- dream_log$V1 %>%
        # lapply(.,  # mainly grabbing and applying names
        #        apply_name_to_value <- function(x) { # one-time use function
        #          names <- sapply(x, "[[", 1) # get the name
        #          names(x) <- names # apply the names
        #          x <- sapply(x, "[[", 2) # reduce value to just by the value (remove name)
        #          return(x) # return
        #        }
        # ) %>%
        # lapply(., lapply, '[[', 1) %>% # remove unneeded nesting
        lapply(.,
               combine_by_response <- function(x) { # one-time use function
                 colNames <- as.list(unique(names(x))) # get the names for the columns
                 x <- matrix(unlist(x), ncol = length(x)/8, nrow = 8) %>% # convert to a matrix
                   t %>% # transpose the matrix
                   as.data.frame # convert to a dataframe
                 colnames(x) <- colNames # apply the column names
                 return(x) # return
               }
        )
      # run a sub-function to reduce the dream logs to be NA if there is no
      # dream log and move the sleep log back one in the order otherwise

      # nested function to check is an item is zero (used to drop 0s in reindex_dream_log)
      is.zero <- function(item) {
        tryCatch({
          res <- item == 0
          if(is.na(res)) {
            return(FALSE)
          }
          if(length(res) > 1) {
            return(FALSE)
          }
          return(res)
        }, error = function(e) {
          return(FALSE)
        })
      }
      #browser()
      reindex_dream_log <- function(dream_log) {
        # initialize a dream log and a variable to be the index
        idx <- 1
        new_dream_log <- list()
        for(dream in dream_log) {
          #print("Dream Log:")
          #print(new_dream_log)
          # if this is not a dream log
          if(dim(dream)[1] != 0) {
            # get the previous index
            prev_idx <- idx - 1
            # set the previous item as this dream log
            new_dream_log[[prev_idx]] <- dream
            # set the current item to be 0 so that it may be dropped later
            new_dream_log <- append(new_dream_log, 0)
          } else {
            # set the current item to be NA
            new_dream_log <- append(new_dream_log, NA)
          }
          # increment the index
          idx <- idx + 1
        }
        #print(new_dream_log[-which(lapply(new_dream_log, is.zero) == FALSE)])
        # drop all zeros (runs an lapply nested in a which to get the indices of elements that are zero)
        new_dream_log <- new_dream_log[which(lapply(new_dream_log, is.zero) == FALSE)]
        # return the new dream log
        return(new_dream_log)
      }
      # re-index the dream log
      dream_log <- reindex_dream_log(dream_log)
      # drop unwanted data
      sleep_diary_data <- fdata_sp$`Sleep Diary` %>%
        filter(question==0) %>% # select the sleep quality items
        # remove these items
        select(-answer_prog,
               -questionnaire_number,
               -answer_timestamp,
               -answer,
               -question)
      # add the sleep quality data back to object
      sleep_diary_data <- cbind(sleep_diary_data, sleep_quality)
      # add the id
      sleep_diary_data['ID'] <- subj_id
      # update the overall object
      #sleep_diary_data$event_df <- dream_log
      # update the overall object (and convert it to a list)
      fdata_sp$`Sleep Diary` <- sleep_diary_data %>% as.list
      # add the events back
      fdata_sp$`Sleep Diary`$event_df <- dream_log
      # add the number of events
      fdata_sp$`Sleep Diary`$number_of_events <- sapply(fdata_sp$`Sleep Diary`$event_df, nrow)
      # set any null items in the 'number_of_events' to zero
      fdata_sp$`Sleep Diary`$number_of_events <- sapply(fdata_sp$`Sleep Diary`$number_of_events,
        function(x) { # nested function to correct the number of events data item
          # if the value is an integer
          if(is.number(x)) {
            # return the value
            return(x)
          # otherwise
          } else {
            # it should be set to 0
            return(0)
          }
        }
      )
      # add did_not_sleep
      fdata_sp$`Sleep Diary`$did_not_sleep <- is.na(fdata_sp$`Sleep Diary`$questionnaire_type)
      # convert the sleep diary into a dataframe (will convert to tibble just fine, but will not convert directly to a dataframe)
      fdata_sp$`Sleep Diary` <- fdata_sp$`Sleep Diary` %>% as_tibble %>% as.data.frame
      # rename the columns to have any white space replaced by '_'
      names(fdata_sp$`Sleep Diary`) <- lapply(names(fdata_sp$`Sleep Diary`), str_replace_all, ' ', '_')
      # return the updated object
      return(fdata_sp)
      #' Note that this implementation does not have the redundant event_df naming.
      #' This likely shouldn't hurt anything. Also, overall object is now a
      #' list and not a dataframe, hoping this won't break anything later on
      #' but there's a good chance to break in cleanup layer.
    }

    create_form_proc <- function(fdata_sp, subj_id, to_run=list('clean_daily_recording', 'clean_end_questionnaire', 'clean_mood_post_task', 'clean_mood_questionnaire', 'clean_sleep_diary')) {
      # basically provide a list of function that take the fdata_sp object and subj_id as input and run them within a try-catch
      # initialize form proc
      form_proc <- fdata_sp
      # sub function to run the provided functions
      run_form_proc_func <- function(fname, form_proc, subj_id) {
        resultant_form_proc <- tryCatch({
          print(paste0("Attempting to run: '", fname, "'"))
          form_proc <- do.call(fname, list(form_proc, subj_id))
        }, error = function(e) {
          # log the error
          print(e)
          # print the error trace
          #traceback(3)
          # log which task in processing failed
          print(paste0(fname, " failed to be executed correctly."))
          # return the unchanged form proc
          return(form_proc)
        })
        # return the resultant form proc of function run attempt
        return(resultant_form_proc)
      }
      # run the list of functions for the subject given
      for (func2run in to_run) {
        form_proc <- run_form_proc_func(func2run, form_proc, subj_id)
      }
      # return the overall result
      return(form_proc)
    }

    form_proc <- create_form_proc(fdata_sp, raw_single$ID)

    print("Base data cleaning finished...")

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

    #browser()

    print("Beginning payment, completion, and performance calculations...")
    
    # get the current schedule file
    cur_sched_file <- grep("*.db", list.files(paste0(dataPath, '/Subjects/', raw_single$ID, '/schedule')), value=TRUE)
    # append the path
    cur_sched_file <- paste(paste0(dataPath, '/Subjects/', raw_single$ID, '/schedule/', cur_sched_file))

    # METADATA FETCHING (site, initials, group)
    data = dbConnect(SQLite(), cur_sched_file)
    participant_info = dbGetQuery(data, "SELECT * FROM subject")
    trials_1 <- dbGetQuery(data, "SELECT * FROM trials")
    questionnaire_1 <- dbGetQuery(data, "SELECT * FROM questionnaires")
    group <- participant_info$study_group
    site <- case_when(participant_info$state == "PA" ~"Pitt", participant_info$state == "NC" ~"UNC")

    print("Metadata was fetched")
    #print(trials_1)
    #print(questionnaire_1)

    # COMPLETENESS CALCULATIONS (calendar day, ema day)
    completeness_table <- function(games_input, questionnaires_input, participant_status) {
      #cleans questionnaires input
      questionnaires <- questionnaires_input %>% 
        filter(type != 19) %>% #drops mood post task 
        drop_na(start_time | completed_time) %>% #drops any incomplete tasks
        mutate(start_timestamp = as.POSIXct(start_time/1000, origin="1970-01-01", tz="America/New_York")) %>% 
        mutate(scheduled_timestamp = as.POSIXct(scheduled_time/1000, origin="1970-01-01", tz="America/New_York")) %>% 
        #handles the case when a participant does a task between midnight and 3am, will attribute completeness to the previous day
        mutate(start_hour = hour(.$start_timestamp)) %>% 
        mutate(quest_dates = if_else(start_hour >= 0 & start_hour<=3, as.Date(format(.$scheduled_timestamp, '%Y-%m-%d')), as.Date(format(.$start_timestamp, '%Y-%m-%d'))))
      #creates df sorted by questionnaire type, and number completed on date
      questionnaires_summary <- questionnaires %>% 
        dplyr::group_by(type, description, quest_dates) %>% 
        dplyr::summarize(count = n())
      #cleans games input
      games <- games_input %>% 
        filter (block < 1000) %>% #drops fmri blocks 
        filter(trial == 0) %>% #grabs only first trial of block
        drop_na(stim_time) %>% #drops any incomplete games
        mutate(start_timestamp = as.POSIXct(stim_time/1000, origin="1970-01-01", tz="America/New_York")) %>% 
        mutate(scheduled_timestamp = as.POSIXct(scheduled_time/1000, origin="1970-01-01", tz="America/New_York")) %>% 
        #handles the case when a participant does a task between midnight and 3am, will attribute completeness to the previous day
        mutate(start_hour = hour(.$start_timestamp)) %>%
        mutate(games_dates = if_else(start_hour >= 0 & start_hour<=3, as.Date(format(.$scheduled_timestamp, '%Y-%m-%d')), as.Date(format(.$start_timestamp, '%Y-%m-%d'))))
      
      #creates df with number of games completed on date
      games_summary <- games %>% 
        dplyr::group_by(games_dates) %>% 
        dplyr::summarize(games_count = n())
      #start date when the participants starts the practice session-practice will be sliced later
      #end date is dependent on whether the participant is active or inactive
      #active: grabs all dates up to the previous day before run date 
      #unless dashboard ran on same day as practice, then end date is current day
      #inactive: grabs all dates up to the latest date in schedule file 
      start_date = games$games_dates[1]
      end_date = case_when(
        participant_status == "active" ~ Sys.Date(), 
        participant_status == "inactive" ~ as.Date(max(max(questionnaires_summary$quest_dates), max(games_summary$games_dates))))
      
      #missingness padding for all tasks 
      #complete function fills in zero for any missing dates  
      
      games_completeness <- games_summary %>% 
        complete(games_dates = seq.Date(start_date, end_date, by="day"), fill = list(games_count = 0)) 
      
      mood_completeness <- questionnaires_summary %>% 
        filter(type == 0) %>% 
        complete(quest_dates = seq.Date(start_date, end_date, by="day"), fill = list(count = 0)) %>% 
        rename(mood_count = count)
      
      rest_completeness <- questionnaires_summary %>% 
        filter(type == 2) %>% 
        complete(quest_dates = seq.Date(start_date, end_date, by="day"), fill = list(count = 0)) %>% 
        rename(rest_count = count)
      
      sleep_completeness <- questionnaires_summary %>% 
        filter(type == 5) %>% 
        complete(quest_dates = seq.Date(start_date, end_date, by="day"), fill = list(count = 0)) %>% 
        rename(sleep_count = count)
      
      video_completeness <- questionnaires_summary %>% 
        filter(type == 21) %>% 
        complete(quest_dates = seq.Date(start_date, end_date, by="day"), fill = list(count = 0)) %>% 
        rename(video_count = count)
      
      #completeness count contains the raw counts 
      completeness_count <- data.frame(dates = mood_completeness$quest_dates, sleep_count = sleep_completeness$sleep_count, mood_count = mood_completeness$mood_count, rest_count = rest_completeness$rest_count, games_count = games_completeness$games_count, video_count = video_completeness$video_count)
      #covers the first day case, slices off the practice session if past second day
      #on first day, presents practice session completeness instead 
      if (start_date < Sys.Date()-1) {
        completeness_count <- completeness_count %>%
          slice(2:n())
      } 
      
      if (participant_status == "active") {
        completeness_count <- completeness_count %>% 
          slice(1:(n()-1)) #removes last row with current run date bc possibility they can still complete tasks on current day
      } 
      
      #completeness percentage assumes 1 sleep diary, 4 mood reports, 2 resting states, 4 game blocks, and 1 end of day video
      completeness_perc <- completeness_count %>% 
        mutate(sleep_perc = (sleep_count/1)*100) %>% 
        mutate(mood_perc = (mood_count/4)*100) %>% 
        mutate(rest_perc = (rest_count/2)*100) %>%
        mutate(games_perc = (games_count/4)*100) %>%
        mutate(video_perc = (video_count/1)*100) %>% 
        select(dates, sleep_perc, mood_perc, rest_perc, games_perc, video_perc)
      
      #calculates the ema day and calendar day used in the overview table
      ema_day <- (max(games$block)-5)/4
      if (start_date < Sys.Date()-1) {
        cal_day <- as.numeric(max(completeness_perc$dates)-completeness_perc$dates[1]) + 1
      }
      else {cal_day <- 0}
      
      completeness_output <- list(completeness_perc, ema_day, cal_day)
      return(completeness_output)
    }
    
    # runs the subject with the assumption they are active
    # the "completeness_table" function can be run with 'participant_status' set
    # to inactive to generate a final report, but that is not implemented here.
    task_completeness <- completeness_table(
                              games_input = trials_1,
                              questionnaires_input = questionnaire_1,
                              participant_status = 'active')

    print("Completeness was calculated")

    # PAYMENT INFORMATION

    payment_df <- function(completeness_perc, games_input) {
      #adds calendar week to task percent completed df
      completeness_perc <- completeness_perc %>%
        mutate(cal_week = ((as.numeric(dates-dates[1]) %/% 7)+1))

      #average task completed % per calendar week
      completeness_summary <- completeness_perc %>%
        group_by(cal_week) %>%
        summarize(games_perc_byweek = round(mean(games_perc)), mood_perc_byweek = round(mean(mood_perc)), sleep_perc_byweek = round(mean(sleep_perc)), video_perc_byweek = round(mean(video_perc)))

      #calculates game earnings for all blocks completed, including practice and fmri
      games_all <- games_input %>%
        drop_na(stim_time) %>% #drops any incomplete games
        mutate(start_timestamp = as.POSIXct(stim_time/1000, origin="1970-01-01", tz="America/New_York")) %>%
        mutate(scheduled_timestamp = as.POSIXct(scheduled_time/1000, origin="1970-01-01", tz="America/New_York")) %>%
        #for regular games, use scheduled time, for fmri games, use start time bc scheduled time 1 year ahead
        mutate(game_dates = case_when(block <1000 ~ as.Date(format(.$scheduled_timestamp, '%Y/%m/%d')), block >= 1000 ~ as.Date(format(.$start_timestamp, '%Y/%m/%d')))) %>%
        mutate(cal_week = (as.numeric(game_dates-game_dates[325]) %/% 7)+1) #325 is the start of block 6 (first game of day 1)
      #sum game earnings by calender week
      payment_summary <- games_all %>%
        group_by(cal_week) %>%
        summarize(pay = sum(outcome)*.15)
      #initialize df with NAs
      payment_table <- data.frame(matrix(nrow=5,ncol=7))
      colnames(payment_table) <- c("cal_week", "payment_date", "game_earnings", "games_perc_completed", "mood_perc_completed", "sleep_perc_completed", "video_perc_completed")
      #df is created piecewise to allow for NAs for incompleteness
      payment_table <-  payment_table %>%
        mutate(cal_week = c(1,2,3,4,5)) %>%
        mutate(payment_date = c(as.character(completeness_perc$dates[1]+7), as.character(completeness_perc$dates[1]+14), as.character(completeness_perc$dates[1]+21), "On Termination Date", "On Termination Date")) %>%
        mutate(game_earnings = game_earnings <- c(payment_summary$pay[1]+payment_summary$pay[2], payment_summary$pay[3], payment_summary$pay[4], payment_summary$pay[5], payment_summary$pay[6])) %>%
        mutate(games_perc_completed = c(completeness_summary$games_perc_byweek[1], completeness_summary$games_perc_byweek[2], completeness_summary$games_perc_byweek[3], completeness_summary$games_perc_byweek[4], completeness_summary$games_perc_byweek[5])) %>%
        mutate(mood_perc_completed = c(completeness_summary$mood_perc_byweek[1], completeness_summary$mood_perc_byweek[2], completeness_summary$mood_perc_byweek[3], completeness_summary$mood_perc_byweek[4], completeness_summary$mood_perc_byweek[5])) %>%
        mutate(sleep_perc_completed = c(completeness_summary$sleep_perc_byweek[1], completeness_summary$sleep_perc_byweek[2], completeness_summary$sleep_perc_byweek[3], completeness_summary$sleep_perc_byweek[4], completeness_summary$sleep_perc_byweek[5])) %>%
        mutate(video_perc_completed = c(completeness_summary$video_perc_byweek[1], completeness_summary$video_perc_byweek[2], completeness_summary$video_perc_byweek[3], completeness_summary$video_perc_byweek[4], completeness_summary$video_perc_byweek[5]))

      return(payment_table)
    }
    #browser()
    payment <- payment_df(
                completeness_perc = task_completeness$completeness_table,
                games_input = trials_1)

    print("Payment was calculated")
    
    print("Completed payment, completion, and performance calculations.")

    pr_info_by_block$ID <- raw_single$ID
    raw_single$trials <- trials_df

    output <- list(raw_data=raw_single,
                   info_df = info_df,
                   performance_info=pr_info_by_block,performance_overall=px_overall,
                   form_dfs=form_proc,form_summary=q_sum,
                   ID=raw_single$ID,
                   games_by_block=games_by_block,
                   group=group,
                   site=site,
                   payment=payment,
                   task_completeness=task_completeness)
    save(output, file = output_path)

    return(list(raw_data=raw_single, new_data=TRUE,
                info_df = info_df,
                performance_info=pr_info_by_block,performance_overall=px_overall,
                form_dfs=form_proc,form_summary=q_sum,
                ID=raw_single$ID,
                games_by_block=games_by_block,
                group=group,
                site=site,
                payment=payment,
                task_completeness=task_completeness))
  }, error = function(err){
    # log the traceback
    #traceback(err)
    # log the step that failed for this subject
    print(paste0(raw_single$ID, " did not successfully have their schedule file processed."))
    print(err)
    # add the subject to the failed subject list
    active <<- active[active != raw_single$ID]
    # return NA
    #return(NA)
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
  par_cl <- parallel::makeCluster(spec = thread,type = "FORK")
  exp_out<-parallel::parLapply(par_cl,unique(physio_df$subject_id),function(IDx){
    physio_files_new <- physio_df$file_path[physio_df$subject_id==IDx]
    physio_rawcache_file <- file.path(unique(dirname(physio_df$file_path[physio_df$subject_id==IDx])),paste(IDx,"_physio_raw.rdata",sep = ""))
    physio_proc_file <- file.path(unique(dirname(physio_df$file_path[physio_df$subject_id==IDx])),paste(IDx,"_physio_proc.rdata",sep = ""))
    physio_concat <- NULL
    physio_files <- NULL
    message("Found ",length(physio_files_new), " total physio files for: ",IDx)
    #par_cl <- parallel::makeCluster(spec = thread,type = "FORK")
    message("Loading new physio data for: ",IDx)
    physio_concat_new <- load_physio_single(allpaths_sub = physio_files_new,old_data=NULL,cl = NULL)
    #parallel::stopCluster(par_cl)
    physio_files<-unique(c(physio_files,physio_files_new))
    save(physio_files,physio_concat_new,file = paste0(dataPath,'/Subjects/',IDx,'/physio/',IDx,'_physio_raw.rdata'))
    output <- NULL
    if(!force_reproc && file.exists(physio_proc_file)) {
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
    } else {
    }

    ##### Quickly grabbing .db schedule file for physio, uncoupling physio from schedule file processing
    ##### 2021-11-22 AndyP
    #####

    if (force_reproc){ # should generally be true
      path_to_schedule <- paste0(dataPath, '/Subjects/', IDx, '/schedule')
      sched_file <- list.files(path=path_to_schedule,pattern=paste0(IDx,'_schedule.db'))
      if (length(sched_file)==1){
        sched_data_for_physio = dbConnect(SQLite(), paste0(path_to_schedule, '/', sched_file))
        trials = dbGetQuery(sched_data_for_physio, "SELECT * FROM trials")
        new_block <- trials$block
        new_block <- new_block[new_block<1000]
        # get old processed schedule file to reproc only new fbt
        if (!force_reload){
          proc_sched_file <- list.files(path=path_to_schedule,pattern=paste0(IDx,'_schedule_proc.rdata'))
          if (length(proc_sched_file)>0){
            proc_physio <- output # proc physio also called output
            load(paste0(path_to_schedule,'/',proc_sched_file)) # this will load a new output
            old_sched <- output
            output <- proc_physio # rename to output
            rm(proc_physio)
            old_block <- old_sched$raw_data$trials$block
            old_block <- old_block[old_block<1000]
            old_block <- max(old_block)
            if (old_block > 8){
              trials=trials[-c(which(new_block<=old_block-6)),]
            }
          }
        }
        ## remove blocks that have not been played yet
        if (length(which(is.na(trials$choice)))!=0){
          trials=trials[-c(which(is.na(trials$choice))),]
        }
        fbt <- trials$feedback_time
        block <- trials$block
        trial <- trials$trial
        fbt <- fbt[block < 1000]
        trial <- trial[block < 1000]
        block <- block[block < 1000]
      } else {
        #Get the matching behavioral data
        behav_df <- output$proc_data[[IDx]]$raw_data$trials
        behav_df <- behav_df[which(!is.na(behav_df$stim_time)),]
        if(is.null(behav_df$session_number)) {
          behav_df$session_number <- NA
        }
        fbt <- behav_df$feedback_time
        fbt <- fbt[behav_df$block < 1000]
        sess_map<-unique(behav_df[c("block","session_number")])
        fbt <- as.numeric(fbt)*1000
        block <- behav_df$block
        block <- block[block < 1000]
        warning('Zero or multiple schedule .db files found for subject',IDx, 'reverting to processed schedule file')
      }
      trial_df <- tibble(block=block,trial=trial, fbt=fbt)
      ###EEG
      message("Processing new EEG data for: ",IDx)
      eeg_list <- load_EEG(EEGd = physio_concat_new$eeg, sample_rate = eeg_sample_rate,sd_times = sd_times) # updated AndyP 2021-12-02
      eeg_raw <- eeg_list[[1]]
      eeg_missing <- eeg_list[[2]]
      eeg_fb <- eeg_epochs_around_feedback(EEG_data = eeg_raw,
                                           pre = eeg_pre,post = eeg_post,sample_rate = eeg_sample_rate,
                                           fbt = fbt)


      eeg_stats <- NULL
      eeg_stats$mn1 <- median(eeg_raw$Ch1,na.rm=TRUE)
      eeg_stats$mn2 <- median(eeg_raw$Ch2,na.rm=TRUE)
      eeg_stats$mn3 <- median(eeg_raw$Ch3,na.rm=TRUE)
      eeg_stats$mn4 <- median(eeg_raw$Ch4,na.rm=TRUE)
      eeg_stats$sd01 <- sd(eeg_raw$Ch1,na.rm=TRUE)
      eeg_stats$sd02 <- sd(eeg_raw$Ch2,na.rm=TRUE)
      eeg_stats$sd03 <- sd(eeg_raw$Ch3,na.rm=TRUE)
      eeg_stats$sd04 <- sd(eeg_raw$Ch4,na.rm=TRUE)

      eeg_rawsum <- get_good_EEG(blocks=block,a2f=eeg_fb,sd_times=sd_times,eeg_stats=eeg_stats)
      eeg_summary <- eeg_rawsum[1:4] / eeg_rawsum$Ntotal
      names(eeg_summary) <- paste("per_Ch",1:4,sep = "_")
      eeg_summary$block <- eeg_rawsum$nbl
      eeg_summary$per_worst <- apply(eeg_summary[1:4],1,min,na.rm=T)
      #eeg_summary$session_number<-sess_map$session_number[match(eeg_summary$block,sess_map$block)]
      eeg_summary$ID <- IDx
      #eeg_summary <- eeg_summary[order(names(eeg_summary))]
      eeg_ov <- data.frame(t(apply(eeg_summary[paste("per_Ch",1:4,sep = "_")],2,mean,na.rm=T)))
      eeg_ov$avg_allCh <- apply(eeg_ov,1,mean,na.rm=T)
      eeg_ov$worst_allCh_allblocks <- min(eeg_summary[,paste("per_Ch",1:4,sep = "_")])
      eeg_ov$ID <- IDx

      ###ECG
      message("Processing new ECG data for: ",IDx)
      ecg_raw <- load_ECG(ECGd = physio_concat_new$ecg, HRstep = HRstep,sample_rate = ecg_sample_rate)
      rlex <- rle(is.na(ecg_raw$rate))
      end_x = cumsum(rlex$lengths)
      start_x = c(1, lag(end_x)[-1] + 1)
      for (ir in 1:length(start_x)){
        if (end_x[ir]-start_x[ir]>10000 && rlex$values[ir]==TRUE){
          ecg_raw$rate[start_x[ir]:end_x[ir]] = -1000
          ecg_raw$times[start_x[ir]:end_x[ir]] = -1000
        }
      }
      ecg_rate1 <- ecg_raw$rate[ecg_raw$rate!=-1000]
      ecg_times1 <- ecg_raw$times[ecg_raw$times!=-1000]
      rm(ecg_raw)
      ecg_raw <- tibble(rate=ecg_rate1,times=ecg_times1)

      ecg_fb <- ecg_epochs_around_feedback(ECG_data = ecg_raw,fbt = fbt,
                                           pre = ecg_pre,post = ecg_post,sample_rate = ecg_sample_rate)
      # fbt1 <- fbt
      # fbt2 <- NULL
      # rn <- rownames(ecg_fb)
      # iC <- 1
      # for (iF in 1:length(fbt1)){
      #   if (!any(rn==fbt1[iF])){
      #     fbt2[iC] <- fbt1[iF]
      #     iC<-iC+1
      #   }
      # }
      # ecg_fb1 <- ecg_epochs_around_feedback(ECG_data = ecg_raw,fbt = fbt2,pre = ecg_pre,post = ecg_post,sample_rate = ecg_sample_rate)
      ecg_summary <- get_good_ECG(blocks = block,ch1_a2f = ecg_fb)
      #ecg_summary$session_number<-sess_map$session_number[match(ecg_summary$block,sess_map$block)]
      ecg_summary$ID <- IDx
      #ecg_summary <- ecg_summary[order(names(ecg_summary))]
      ecg_ov <- NULL
      tryCatch({
        ecg_ov <- aggregate(per_Good ~ ID,data = ecg_summary,FUN = mean,na.rm=T)
        ecg_ov$worst_allblocks <- min(ecg_summary$per_Good)
      },
      error=function(e){
        message(paste0('error for overall ecg subject ',IDx, ' this is not used anymore anyway'))
      })

      path_to_physio <- paste0(dataPath,'/Subjects/',IDx,'/physio')
      physio_proc <- list.files(path_to_physio,pattern=paste0(IDx,'_physio_proc.rdata'))
      if (length(physio_proc)==1 && !force_reload){
        # load(paste0(path_to_physio,'/',physio_proc)) # loads a variable called output into global environment
        # # append physio to existing physio_proc.rdata
        # output$eeg_ov <- eeg_ov
        # output$eeg$proc$rrt <- c(output$eeg_proc$rrt,eeg_raw$rrt)
        # output$eeg$proc$Ch1 <- c(output$eeg$proc$Ch1,eeg_raw$Ch1)
        # output$eeg$proc$Ch2 <- c(output$eeg$proc$Ch2,eeg_raw$Ch2)
        # output$eeg$proc$Ch3 <- c(output$eeg$proc$Ch3,eeg_raw$Ch3)
        # output$eeg$proc$Ch4 <- c(output$eeg$proc$Ch4,eeg_raw$Ch4)
        # output$eeg$proc$g1 <- c(output$eeg$proc$g1,eeg_raw$g1)
        # output$eeg$proc$g2 <- c(output$eeg$proc$g2,eeg_raw$g2)
        # output$eeg$proc$g3 <- c(output$eeg$proc$g3,eeg_raw$g3)
        # output$eeg$proc$g4 <- c(output$eeg$proc$g4,eeg_raw$g4)
        # output$eeg_summary <- rbind(output$eeg_summary,eeg_summary)
        # output$eeg_fb$ch1 <- rbind(output$eeg_fb$ch1,eeg_fb$ch1)
        # output$eeg_fb$ch2 <- rbind(output$eeg_fb$ch2,eeg_fb$ch2)
        # output$eeg_fb$ch3 <- rbind(output$eeg_fb$ch3,eeg_fb$ch3)
        # output$eeg_fb$ch4 <- rbind(output$eeg_fb$ch4,eeg_fb$ch4)
        # output$eeg_fb$g1 <- rbind(output$eeg_fb$g1,eeg_fb$g1)
        # output$eeg_fb$g2 <- rbind(output$eeg_fb$g2,eeg_fb$g2)
        # output$eeg_fb$g3 <- rbind(output$eeg_fb$g3,eeg_fb$g3)
        # output$eeg_fb$g4 <- rbind(output$eeg_fb$g4,eeg_fb$g4)
        # output$eeg_missing <- output$eeg_missing+eeg_missing
        # output$eeg_rawsum <- rbind(output$eeg_rawsum,eeg_rawsum)
        # output$ecg_proc <- NULL # do not save for QC
        # output$ecg_fb <- rbind(output$ecg_fb,ecg_fb)
        # output$ecg_ov <- ecg_ov
        # output$ecg_summary <- rbind(output$ecg_summary,ecg_summary)
      }
      output <- list(new_data=TRUE,ID=IDx,lite=F,
                     eeg_proc = eeg_raw,eeg_fb = eeg_fb, eeg_summary = eeg_summary, eeg_ov = eeg_ov, eeg_missing = eeg_missing, eeg_rawsum = eeg_rawsum,
                     ecg_proc = ecg_raw,ecg_fb = ecg_fb, ecg_summary = ecg_summary, ecg_ov = ecg_ov, trial_df = trial_df)
      save(output,file = physio_proc_file)

    } else { # load physio_proc.rdata
      path_to_physio <- paste0(dataPath,'/Subjects/',IDx,'/physio')
      physio_proc <- list.files(path_to_physio,pattern=paste0(IDx,'_physio_proc.rdata'))
      if (length(physio_proc)==1){
        load(paste0(path_to_physio,'/',physio_proc)) # loads a variable called output into global environment
      }
    }

    if(save_lite) {
      output <- list(new_data=TRUE,ID=IDx,lite=T,
                     eeg_fb = eeg_fb,eeg_summary = eeg_summary, eeg_ov = eeg_ov,eeg_missing = eeg_missing, eeg_rawsum = eeg_rawsum,
                     ecg_fb = ecg_fb,ecg_summary = ecg_summary, ecg_ov = ecg_ov, ecg_missing = NA, ecg_rawsum = NA, trial_df = trial_df)

      return(output)
    } else {
      return(output)
    }
  })
  parallel::stopCluster(par_cl)
  if(save_lite) {
    nax <- c("fb","summary", "missing", "rawsum")
  } else {
    nax <- c("proc","fb","summary", "missing", "rawsum")
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
  tryCatch({
    output_fin$newdata_IDs <- IDlist[sapply(exp_out,`[[`,"new_data")]
  },
  error=function(e){
    message('no new data, not sure if this is important to fix rn AndyP 2021-12-03')
  })
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





