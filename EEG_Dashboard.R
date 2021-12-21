# 2021-02-18 AndyP
# This function analyzes EEG from an SQLlite .db file and extracts %avg per block and per channel for use in the Dashboard
# 2021-04-01 JiazhouC
# Modify the functions to be compatible with table generating funcs
# 2021-10-05 AndyP
# Multiple updates, Alon's original %good output now computed, missing data now computed, NAs not implemented for saving physio, but calculated during %good calculation

load_EEG <- function(subject_name,physio_file,abs_path=NULL,EEGd=NULL,sample_rate=256.03, sd_times=10){
  if(is.null(EEGd)) {
    subject_schedule <- paste0(subject_name,"_schedule.db")
    #subject_physio <- paste0(subject_name,"_physio.db")
    if (is.null(abs_path)) {
      # get the paths of the schedule and physio files
      schedule_path <- getPathFromCfg('rl_ema_monitoring', subject_schedule, keywords=NA, exclusion=c('archive'), pattern=TRUE)
      physio_path <- getPathFromCfg('rl_ema_monitoring', physio_file)

      # pattern string for the db file
      pat <- paste0(subject_name, "_schedule.db") # "*_",
      # get a list of subject's schedule files
      fileList <- list.files(schedule_path, pattern = pat)
      #print(paste0(pathSubjSched, '/', fileList))
      # ensure there is only one schedule.db file located here (remainder should be archived in the archive directory)
      if (length(fileList) > 1) {
        errorMessage <- paste("Error: there is more than 1 schedule.db file at ", schedule_path)
        stop(errorMessage)
      } else if (length(fileList) == 0L) {
        stop("Cannot locate schedule db file in folder: ", schedule_path)
      }
      subject_schedule <- paste0(schedule_path, '/', fileList)
      subject_physio <- paste0(physio_path, '/', physio_file)
    } else {
      subject_schedule <-  abs_path$schedule_path
      subject_physio <- abs_path$physio_path
    }

    behavior = dbConnect(SQLite(), subject_schedule)
    trials = dbGetQuery(behavior, "SELECT * FROM trials")
    ## remove blocks that have not been played yet
    if (length(which(is.na(trials$choice)))!=0){
      trials=trials[-c(which(is.na(trials$choice))),]}
    fbt <- trials$feedback_time

    EEG = dbConnect(SQLite(), subject_physio)
    EEGd = dbGetQuery(EEG, "SELECT * FROM EEG_muse ORDER BY recording_time ASC")
  }
  t <- EEGd$recording_time;

  real_recording_time <- function(t0,sample_rate) {
    rrt0 <- seq(t0[1],ceiling(t0[1]+(length(t0)-1)*1000/sample_rate),by=1000/sample_rate)
    rrt0 <- rrt0-max(rrt0-t0)
  }

  rrt <- real_recording_time(t,sample_rate)

  dt <- t-rrt
  msd <- rev(cummin(rev(dt)))
  ix <- which(msd>300)
  lastix = 0;
  while (length(ix)>0) {
    ix <- lastix + ix[1]
    tstart <- ix[1]
    tend <- length(t)
    t0 <- t[tstart:tend]
    rrt0 <- real_recording_time(t0,sample_rate)
    endix <- tend-tstart
    rrt[tstart:tend] <- rrt0
    dt <- t[ix:length(t)]-rrt[ix:length(t)]
    msd <- rev(cummin(rev(dt)))
    lastix <- ix[1] - 1
    ix <-which(msd>300)
  }

  Ch1 <- as.numeric(EEGd$EEG1)
  Ch2 <- as.numeric(EEGd$EEG2)
  Ch3 <- as.numeric(EEGd$EEG3)
  Ch4 <- as.numeric(EEGd$EEG4)

  md_ch1 <- sum(is.na(Ch1))
  md_ch2 <- sum(is.na(Ch2))
  md_ch3 <- sum(is.na(Ch3))
  md_ch4 <- sum(is.na(Ch4))

  g1 <- as.numeric(EEGd$ISGOOD1)
  g2 <- as.numeric(EEGd$ISGOOD2)
  g3 <- as.numeric(EEGd$ISGOOD3)
  g4 <- as.numeric(EEGd$ISGOOD4)

  EEG_data <- dplyr::tibble(rrt,Ch1,Ch2,Ch3,Ch4,g1,g2,g3,g4) # 2021-10-01 AndyP added g1-g4 as outputs
  Missing_data <- dplyr::tibble(md_ch1,md_ch2,md_ch3,md_ch4) # 2021-10-05 AndyP added md_ch1 i.e. missing data as outputs
  EEG_list <- list(EEG_data,Missing_data)

  return(EEG_list) # 2021-10-05 AndyP now returns a list of EEG_data, Missing_data

}

# get epochs around feedback +/- 500ms
eeg_epochs_around_feedback <- function(EEG_data,pre=500,post=1500,sample_rate=NULL,fbt=NULL){
  fbt <- fbt[!is.na(fbt)]

  step <- 1000/sample_rate
  pre <- round(pre/step,0)
  post <- round(post/step,0)
  Td <- 0
  Ta <- 0

  rrt <- EEG_data$rrt
  Ch1 <- EEG_data$Ch1
  Ch2 <- EEG_data$Ch2
  Ch3 <- EEG_data$Ch3
  Ch4 <- EEG_data$Ch4
  g1 <- EEG_data$g1
  g2 <- EEG_data$g2
  g3 <- EEG_data$g3
  g4 <- EEG_data$g4

  ch1_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  ch2_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  ch3_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  ch4_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  g1_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  g2_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  g3_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  g4_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);

  for (i in 1:length(fbt)){
    fbt0 <- which(rrt>fbt[i])
    if (length(fbt0)>0){
      if (fbt0[1]>1){
        ind <- seq(fbt0[1]-pre,fbt0[1]+post,by=1)
      } else {
        ind <- NULL
      }
    } else {
      ind <- NULL # 2021-05-24 AndyP, was recycling ind from last trial if length(fbt0)==0
    }
    dL <- pre+1+post
    aL <- length(ind)

    if (length(ind)>0){
      if (ind[length(ind)] > length(rrt)){
        addpost <- ind[length(ind)] - length(rrt)
        ind <- seq(ind[1],length(rrt),by=1)
      }else{
        addpost <- NULL
      }
    }else{
      addpost <- NULL
    }
    Td <- Td + dL
    Ta <- Ta + aL

    if (aL > 0 & !is.null(addpost)){
      ch1_a2f[i,] <- c(Ch1[ind],1:addpost) # 2021-05-24 AndyP corrected addpost bug addpost -> 1:addpost
      ch2_a2f[i,] <- c(Ch2[ind],1:addpost)
      ch3_a2f[i,] <- c(Ch3[ind],1:addpost)
      ch4_a2f[i,] <- c(Ch4[ind],1:addpost)
      g1_a2f[i,] <- c(g1[ind],1:addpost)
      g2_a2f[i,] <- c(g2[ind],1:addpost)
      g3_a2f[i,] <- c(g3[ind],1:addpost)
      g4_a2f[i,] <- c(g4[ind],1:addpost)
    } else if (aL >0 & is.null(addpost)){
      ch1_a2f[i,] <- c(Ch1[ind]) # 2021-05-24 AndyP corrected addpost bug
      ch2_a2f[i,] <- c(Ch2[ind])
      ch3_a2f[i,] <- c(Ch3[ind])
      ch4_a2f[i,] <- c(Ch4[ind])
      g1_a2f[i,] <- c(g1[ind])
      g2_a2f[i,] <- c(g2[ind])
      g3_a2f[i,] <- c(g3[ind])
      g4_a2f[i,] <- c(g4[ind])
    }
    
    Ch1 <- Ch1[rrt > fbt[i]]
    Ch2 <- Ch2[rrt > fbt[i]]
    Ch3 <- Ch3[rrt > fbt[i]]
    Ch4 <- Ch4[rrt > fbt[i]]
    g1 <- g1[rrt > fbt[i]]
    g2 <- g2[rrt > fbt[i]]
    g3 <- g3[rrt > fbt[i]]
    g4 <- g4[rrt > fbt[i]]
    rrt <- rrt[rrt > fbt[i]]
  }
  ch1_a2f <- as.data.frame(ch1_a2f)
  ch2_a2f <- as.data.frame(ch2_a2f)
  ch3_a2f <- as.data.frame(ch3_a2f)
  ch4_a2f <- as.data.frame(ch4_a2f)
  g1_a2f <- as.data.frame(g1_a2f)
  g2_a2f <- as.data.frame(g2_a2f)
  g3_a2f <- as.data.frame(g3_a2f)
  g4_a2f <- as.data.frame(g4_a2f)

  a2f <- list(ch1=ch1_a2f,ch2=ch2_a2f,ch3=ch3_a2f,ch4=ch4_a2f,g1=g1_a2f,g2=g2_a2f,g3=g3_a2f,g4=g4_a2f)

  return(a2f) # rows = number of trials, columns = number of timestamps
}

get_good_EEG <- function(blocks,a2f,sd_times=10){

  library("dplyr") # 2021-10-01 AndyP  do we need this library call here?

  nbl <- unique(blocks)

  ch1_a2f <- as.matrix(a2f$ch1)
  ch2_a2f <- as.matrix(a2f$ch2)
  ch3_a2f <- as.matrix(a2f$ch3)
  ch4_a2f <- as.matrix(a2f$ch4)
  g1 <- as.matrix(a2f$g1)
  g2 <- as.matrix(a2f$g2)
  g3 <- as.matrix(a2f$g3)
  g4 <- as.matrix(a2f$g4)

  # 2021-10-01 AndyP corrected bug, all g1 -> g1-g4
  ch1_a2f[g1==0 | g1==4] = NA
  ch2_a2f[g2==0 | g2==4] = NA
  ch3_a2f[g3==0 | g3==4] = NA
  ch4_a2f[g4==0 | g4==4] = NA


  mn <- median(ch1_a2f,na.rm=TRUE)
  sd0 <- sd(ch1_a2f,na.rm=TRUE)
  ch1_a2f[ch1_a2f > mn+sd_times*sd0 | ch1_a2f < mn-sd_times*sd0 | ch1_a2f < 1650/20 | ch1_a2f > 19*1650/20] = NA
  mn <- median(ch2_a2f,na.rm=TRUE)
  sd0 <- sd(ch2_a2f,na.rm=TRUE)
  ch2_a2f[ch2_a2f > mn+sd_times*sd0 | ch2_a2f < mn-sd_times*sd0 | ch2_a2f < 1650/20 | ch2_a2f > 19*1650/20] = NA
  mn <- median(ch3_a2f,na.rm=TRUE)
  sd0 <- sd(ch3_a2f,na.rm=TRUE)
  ch3_a2f[ch3_a2f > mn+sd_times*sd0 | ch3_a2f < mn-sd_times*sd0 | ch3_a2f < 1650/20 | ch3_a2f > 19*1650/20] = NA
  mn <- median(ch4_a2f,na.rm=TRUE)
  sd0 <- sd(ch4_a2f,na.rm=TRUE)
  ch4_a2f[ch4_a2f > mn+sd_times*sd0 | ch4_a2f < mn-sd_times*sd0 | ch4_a2f < 1650/20 | ch4_a2f > 19*1650/20] = NA



  # output of function
  # avgNgood is the average number of good sample percentage across all channels for each block
  # NgoodX is the number of good samples for channel X.  To get the percentage divide by Ntotal
  Ngood1 <- NULL
  Ngood2 <- NULL
  Ngood3 <- NULL
  Ngood4 <- NULL
  Ntotal <- NULL
  avgNgood <- NULL
  Ngood_by_Block <- NULL
  for (i in 1:length(nbl)){
    ix <-which(blocks==nbl[i])
    Ngood1[i] <- sum(!is.na(ch1_a2f[ix,])) # 2021-10-05 AndyP added comma, was this correct before?
    Ngood2[i] <- sum(!is.na(ch2_a2f[ix,]))
    Ngood3[i] <- sum(!is.na(ch3_a2f[ix,]))
    Ngood4[i] <- sum(!is.na(ch4_a2f[ix,]))
    Ntotal[i] <- nrow(ch1_a2f[ix,])*ncol(ch1_a2f[ix,])
    nT <- nrow(ch1_a2f[ix,])
    ch1_a2f_byB <- ch1_a2f[ix,];
    ch2_a2f_byB <- ch2_a2f[ix,];
    ch3_a2f_byB <- ch3_a2f[ix,];
    ch4_a2f_byB <- ch4_a2f[ix,];
    Ngood_by_Block_temp <- NULL
    for (iT in 1:nT){
      if (sum(rbind(is.na(ch1_a2f_byB[iT,]),is.na(ch2_a2f_byB[iT,]),is.na(ch3_a2f_byB[iT,]),is.na(ch4_a2f_byB[iT,])))>0){
        Ngood_by_Block_temp[iT] <- 0
      } else {
        Ngood_by_Block_temp[iT] <- 1
      }
    }
    Ngood_by_Block[i] <- mean(Ngood_by_Block_temp)
    #avgNgood[i] <- mean(rbind(Ngood1[i]/Ntotal[i],Ngood2[i]/Ntotal[i],Ngood3[i]/Ntotal[i],Ngood4[i]/Ntotal[i])) not useful
  }
  ntrial <- as.numeric(table(blocks))
  Ngood_df <- dplyr::tibble(Ngood1,Ngood2,Ngood3,Ngood4,Ntotal,nbl,ntrial,Ngood_by_Block)

  return(Ngood_df)

}


