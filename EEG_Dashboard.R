# 2021-02-18 AndyP
# This function analyzes EEG from an SQLlite .db file and extracts %avg per block and per channel for use in the Dashboard


load_EEG <- function(subject_name,sample_rate=256.03, sd_times=10){
  
  library("RSQLite")
  library("dplyr")
  
  subject_schedule <- paste0(subject_name,"_schedule.db")
  subject_physio <- paste0(subject_name,"_physio.db")
  
  behavior = dbConnect(SQLite(), subject_schedule)
  trials = dbGetQuery(behavior, "SELECT * FROM trials")
  ## remove blocks that have not been played yet 
  if (length(which(is.na(trials$choice)))!=0){
    trials=trials[-c(which(is.na(trials$choice))),]}
  fbt <- trials$feedback_time
  
  EEG = dbConnect(SQLite(), subject_physio)
  EEGd = dbGetQuery(EEG, "SELECT * FROM EEG_muse ORDER BY recording_time ASC")
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
  
  g1 <- as.numeric(EEGd$ISGOOD1)
  g2 <- as.numeric(EEGd$ISGOOD2)
  g3 <- as.numeric(EEGd$ISGOOD3)
  g4 <- as.numeric(EEGd$ISGOOD4)
  
  Ch1[g1==0 | g1==4] = NA
  Ch2[g1==0 | g1==4] = NA
  Ch3[g1==0 | g1==4] = NA
  Ch4[g1==0 | g1==4] = NA
  
  
  mn <- median(Ch1,na.rm=TRUE)
  sd0 <- sd(Ch1,na.rm=TRUE)
  Ch1[Ch1 > mn+sd_times*sd0 | Ch1 < mn-sd_times*sd0 | Ch1 < 1650/20 | Ch1 > 19*1650/20] = NA
  mn <- median(Ch2,na.rm=TRUE)
  sd0 <- sd(Ch2,na.rm=TRUE)
  Ch2[Ch2 > mn+sd_times*sd0 | Ch2 < mn-sd_times*sd0 | Ch2 < 1650/20 | Ch2 > 19*1650/20] = NA
  mn <- median(Ch3,na.rm=TRUE)
  sd0 <- sd(Ch3,na.rm=TRUE)
  Ch3[Ch3 > mn+sd_times*sd0 | Ch3 < mn-sd_times*sd0 | Ch3 < 1650/20 | Ch3 > 19*1650/20] = NA
  mn <- median(Ch4,na.rm=TRUE)
  sd0 <- sd(Ch4,na.rm=TRUE)
  Ch4[Ch4 > mn+sd_times*sd0 | Ch4 < mn-sd_times*sd0 | Ch4 < 1650/20 | Ch4 > 19*1650/20] = NA

  EEG_data <- dplyr::tibble(rrt,Ch1,Ch2,Ch3,Ch4)
  
  return(EEG_data)
  
}




# get epochs around feedback +/- 500ms
get_epochs_around_feedback <- function(EEG_data,pre=500,post=1500,sample_rate){
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
  
  ch1_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  ch2_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  ch3_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  ch4_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  for (i in 1:length(fbt)){
    fbt0 <- which(rrt>fbt[i])
    if (length(fbt0)>0){
        if (fbt0[1]>1){
        ind <- seq(fbt0[1]-pre,fbt0[1]+post,by=1)
      } else {
        ind <- NULL
      }
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
    
    if (aL > 0){
      ch1_a2f[i,] <- c(Ch1[ind],addpost)
      ch2_a2f[i,] <- c(Ch2[ind],addpost)
      ch3_a2f[i,] <- c(Ch3[ind],addpost)
      ch4_a2f[i,] <- c(Ch4[ind],addpost)
    }
  }
  ch1_a2f <- as.data.frame(ch1_a2f)
  ch2_a2f <- as.data.frame(ch2_a2f)
  ch3_a2f <- as.data.frame(ch3_a2f)
  ch4_a2f <- as.data.frame(ch4_a2f)
  
  a2f <- list(ch1=ch1_a2f,ch2=ch2_a2f,ch3=ch3_a2f,ch4=ch4_a2f)
  
  return(a2f) # rows = number of trials, columns = number of timestamps
}



get_good_EEG <- function(blocks,a2f){

  library("dplyr")
  
  blocks <- trials$block
  nbl <- unique(blocks)
  
  ch1_a2f <- as.matrix(a2f$ch1)
  ch2_a2f <- as.matrix(a2f$ch2)
  ch3_a2f <- as.matrix(a2f$ch3)
  ch4_a2f <- as.matrix(a2f$ch4)
  
  # output of function
  # avgNgood is the average number of good sample percentage across all channels for each block
  # NgoodX is the number of good samples for channel X.  To get the percentage divide by Ntotal
  Ngood1 <- NULL
  Ngood2 <- NULL
  Ngood3 <- NULL
  Ngood4 <- NULL
  Ntotal <- NULL
  avgNgood <- NULL
  for (i in 1:length(nbl)){
    ix <-which(blocks==nbl[i])
    Ngood1[i] <- sum(!is.na(ch1_a2f[ix]))
    Ngood2[i] <- sum(!is.na(ch2_a2f[ix]))
    Ngood3[i] <- sum(!is.na(ch3_a2f[ix]))
    Ngood4[i] <- sum(!is.na(ch4_a2f[ix]))
    Ntotal[i] <- length(ix)
    avgNgood[i] <- mean(rbind(Ngood1[i]/Ntotal[i],Ngood2[i]/Ntotal[i],Ngood3[i]/Ntotal[i],Ngood4[i]/Ntotal[i]))
  }
  
  Ngood_df <- dplyr::tibble(Ngood1,Ngood2,Ngood3,Ngood4,Ntotal,avgNgood)
  
  return(Ngood_df)
  
}  



