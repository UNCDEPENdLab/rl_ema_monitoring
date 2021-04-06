# 2021-03-09 AndyP
# This function analyzes ECG from an SQLlite .db file and extracts %avg per block for use in the Dashboard


library("RSQLite")
library("dplyr")

if(FALSE) {
  HRstep <- 10
  sample_rate <- 100

  behavior = dbConnect(SQLite(), "Mandy_schedule.db")
  trials = dbGetQuery(behavior, "SELECT * FROM trials")
  ## remove blocks that have not been played yet
  if (length(which(is.na(trials$choice)))!=0){
    trials=trials[-c(which(is.na(trials$choice))),]}
  fbt <- trials$feedback_time

  ECG = dbConnect(SQLite(), "Mandy_physio.db")
  ECGd = dbGetQuery(ECG,"SELECT time_ms, rr_intervals, heartrate, contact FROM Polar_heartrate ORDER BY time_ms ASC")


}

correctTimings <- function(times, intervals){
  # correct timings

  #mismatch function
  mismatch <- function(shift=0, x, y){
    y <- y + shift #apply shift to one series
    z <- sum( ((x-y) > 1000) * (x-y-1000) + (x-y < 0) * (y-x))
    return(z)
  }

  times <- as.matrix(times)
  intervals <- as.matrix(intervals)

  nz1 <- which(intervals != 0)[1]
  if (is.na(nz1)){
    timings <- NULL
    wiggleroom <- NULL
  } else {
    if (nz1>1){
      times<- times[-c(1:(nz1-1))]
      intervals <- intervals[-c(1:(nz1-1))]
    }
    newintervals <- intervals
    timings <- NULL
    timings[1] <- times[1];
    for (i in 2:length(newintervals)){
      timings[i]= timings[i-1] + newintervals[i]
    }
    todelete <- which(timings[1:(length(timings)-1)] == timings[2:length(timings)])
    while (!length(todelete)==0){
      timings <- timings[-c(todelete)]
      times <- times[-c(todelete)]
      todelete <- which(timings[1:(length(timings)-1)]==timings[2:length(timings)])
    }
    shft <- optim(-50, fn=mismatch, x=times, y=timings, method="Brent", lower=-2000, upper = 0)
    shft <- shft$par
    while (mismatch(shft[length(shft)],times,timings)==mismatch(shft[length(shft)]+1,times,timings)){shft <- c(shft,shft[length(shft)]+1)}
    if (length(shft)>1){shft <- c(shft[2:length(shft)],shft[1])}
    while (mismatch(shft[length(shft)],times,timings)==mismatch(shft[length(shft)]-1,times,timings)){shft <- c(shft,shft[length(shft)]-1)}
    timings <- c(times[1]-intervals[1],timings)
    timings <- timings + mean(shft)
    wiggleroom <- length(shft)
  }

  return(timings)
}


timings2samples <- function(timings,HRstep){
  i <- 0
  start <- ceiling(timings[1]/HRstep) * HRstep
  times <- NULL
  intervals <- NULL
  rate <- NULL
  for (t in seq(from=start, to=timings[length(timings)], by=HRstep)){
    i <- i+1
    times[i] <- t
    ind <- which(timings > t)[1]
    if (length(ind)>0){
      intervals[i] <- timings[ind]-timings[ind-1]
      rate[i] <- 60000/intervals[i]
    }
  }
  output <- cbind(times,intervals,rate)
  return(output)
}


load_ECG <- function(ECGd = NULL, HRstep = 10, sample_rate = 100) {
  hrt <- as.data.frame(as.numeric(ECGd$time_ms));
  hr <- as.data.frame(as.numeric(ECGd$heartrate));
  colnames(hrt)[1] <- "time"
  colnames(hr)[1] <- "heart_rate"
  nD = nrow(ECGd)
  intervals <- data.frame("intervals"=numeric(0))
  hrt1 <- data.frame("time"=numeric(0))
  hr1 <- data.frame("heart_rate"=numeric(0))

  for (i in 1:nD){
    temp1 <- strsplit(ECGd$rr_intervals[i],"\\[")
    temp2 <- strsplit(temp1[[1]][2],"]")

    if (is.na(temp2)){
      temp2 <- "0"
    }

    if (any(grepl(",",temp2[[1]][1]))){
      temp3 <- strsplit(temp2[[1]][1],",")
      for (j in 1:length(temp3[[1]])){
        intervals <- intervals %>% dplyr::add_row("intervals"=as.numeric(temp3[[1]][j]))
        hrt1 <- hrt1 %>% dplyr::add_row("time"=hrt$time[i])
        hr1 <- hr1 %>% dplyr::add_row("heart_rate"=hr$heart_rate[i])
      }

    } else {
      intervals <- intervals %>% dplyr::add_row("intervals"=as.numeric(temp2))
      hrt1 <- hrt1 %>% dplyr::add_row("time"=hrt$time[i])
      hr1 <- hr1 %>% dplyr::add_row("heart_rate"=hr$heart_rate[i])
    }
  }

  # find irregular times
  difftimes <- hrt1 %>% mutate(hrt1-lag(hrt1))
  difftimes <- difftimes[-c(1),]
  difftimes <- as.matrix(difftimes)
  ireg <- which(((difftimes+50) %% 1000) > 100)
  i <-1
  hrt1 <- as.matrix(hrt1$time)
  while (i < length(ireg)-1){
    i1 <- ireg[i]
    i2 <- ireg[i+1]
    if ((abs(difftimes[i1]+difftimes[i2]-2000) < 100) & (difftimes[i1] > difftimes[i2]) & all(difftimes[(i1+1):(i2-1)]>0)) {
      hrt1[(i1+1):i2] = hrt1[i2+1]
    }
    i = i+1
  }

  intervals <- intervals * 1000/1024
  intervals[is.na(intervals)]=0

  # find discontinuities and split to sections
  hrt1 <- as.data.frame(hrt1)
  difftimes <- hrt1 %>% mutate(hrt1-lag(hrt1))
  difftimes <- difftimes[-c(1),]
  difftimes <- as.matrix(difftimes)
  ilast <- c(which(difftimes != 0),nrow(hrt1))
  csumint <- as.matrix(cumsum(intervals))
  a <- as.data.frame(hrt1$V1[ilast]-hrt1$V1[1]-csumint[ilast])
  deviations <- as.matrix(a %>% mutate(a-lag(a)))
  deviations <- deviations[-c(1),]
  isplit <- which(abs(deviations)>1000)+1
  isplit <- ilast[isplit]
  minseg <- 10

  hr1 <- as.matrix(hr1)
  hrt1 <- as.matrix(hrt1)
  intervals <- as.matrix(intervals)

  while (length(isplit)>0 & isplit[1] < minseg){
    hrt1 <- hrt1[-c(1:(isplit[1]-1))]
    hr1 <- hr1[-c(1:(isplit[1]-1))]
    intervals <- intervals[-c(1:(isplit[1]-1))]
    isplit <- isplit - isplit[1] +1
    isplit <- isplit[-c(1)]
  }

  todelete <- NULL
  iC <- 1

  ####ISSUE!!!!!#####
  ####Special case for the dataset "123"
  ##If isplit is of lenght of zero, the it will stop...
  ###The syntax also wouldn't allow for isplit == 1, because i in 1:0 is not gonna work.
  ###################

  #print(isplit)
  if (length(isplit)>0) {
    for (i in 1:(length(isplit)-1)){
      if (isplit[i+1]-isplit[i] < minseg){
        todelete[iC] <- i+1
        iC <- iC+1
      }
    }

    for (i in 1:length(todelete)){
      inds <- isplit[(todelete[i]-1)]:isplit[todelete[i]]-1
      hrt1 <- hrt1[-c(inds)]
      hr1 <- hr1[-c(inds)]
      intervals <- intervals[-c(inds)]
      isplit[todelete[i]:length(isplit)] = isplit[todelete[i]:length(isplit)]-length(inds)
      isplit <- isplit[-c(todelete[i])]
      todelete <- todelete-1
    }

  }
  HRsplit <- matrix(list(),length(isplit)+1,2)
  if (length(isplit)>1){
    HRsplit[[1,1]] <- hrt1[1:(isplit[1]-1)]
    HRsplit[[1,2]] <- intervals[1:(isplit[1]-1)]

    for (i in 1:(length(isplit)-1)){
      HRsplit[[i+1,1]] <- hrt1[isplit[i]:(isplit[i+1]-1)]
      HRsplit[[i+1,2]] <- intervals[isplit[i]:(isplit[i+1]-1)]
    }
  } else if (length(isplit)==1) {
    HRsplit[[1,1]] <- hrt1[1:(isplit[1]-1)]
    HRsplit[[1,2]] <- intervals[1:(isplit[1]-1)]
  } else {
    HRsplit[[,1]] <- hrt1
    HRsplit[[,2]] <- intervals
  }



  # merge sections
  beattimes <- NULL
  wiggleroom <- NULL
  beattimes <- matrix(list(),nrow(HRsplit),1)
  times1 <- matrix(list(),nrow(HRsplit),1)
  intervals1 <- matrix(list(),nrow(HRsplit),1)
  rate1 <- matrix(list(),nrow(HRsplit),1)
  for (i in 1:nrow(HRsplit)){
    if (sum(HRsplit[[i,2]]>0)>=2){
      timings <- correctTimings(HRsplit[[i,1]],HRsplit[[i,2]])
      beattimes[[i,1]] <- timings
      output <- timings2samples(timings,HRstep=10)
      times1[[i,1]] <- output[,1]
      intervals1[[i,1]] <- output[,2]
      rate1[[i,1]] <- output[,3]
    }
  }

  # check
  stopifnot(length(beattimes)==length(times1) | length(times1)==length(intervals1) | length(intervals1)==length(rate1))

  # merge data
  times2 <- as.vector(times1[[1,1]])
  intervals2 <- as.vector(intervals1[[1,1]])
  rate2 <- as.vector(rate1[[1,1]])
  beattimes2 <- as.vector(beattimes[[1,1]])
  for (i in 2:length(beattimes)){
    if (length(times1[[i,1]]) > 0){
      nanHRsteps <- ((times1[[i,1]][1] - times2[length(times2)]) / HRstep) - 1
      if (nanHRsteps > 0){
        temp_times <- times2[length(times2)]+seq(from=HRstep,to=nanHRsteps*HRstep,by=HRstep)
        times2[(length(times2)+1):(length(times2)+length(temp_times))] <- temp_times
      }
      temp_intervals <- rep(NA,length(times2)-length(intervals2))
      temp_rate <- rep(NA,length(times2)-length(rate2))
      beattimes2[(length(beattimes2)+1):(length(beattimes2)+length(beattimes[[i,1]]))] <- beattimes[[i,1]]
      times2[(length(times2)+1):(length(times2)+length(times1[[i,1]]))] <- times1[[i,1]]
      temp_intervals[(length(temp_intervals)+1):(length(temp_intervals)+length(intervals1[[i,1]]))] <- intervals1[[i,1]]
      intervals2[(length(intervals2)+1):(length(intervals2)+length(temp_intervals))] <- temp_intervals
      temp_rate[(length(temp_rate)+1):(length(temp_rate)+length(rate1[[i,1]]))] <- rate1[[i,1]]
      rate2[(length(rate2)+1):(length(rate2)+length(temp_rate))] <- temp_rate
    }
  }

  ECG_data <- data.frame(times=times2,rate=rate2)
  return(ECG_data)
}








# get epochs around feedback +/- 500ms
ecg_epochs_around_feedback <- function(ECG_data,fbt,pre=1000,post=10000,sample_rate=100){
  step <- 1000/sample_rate
  pre <- round(pre/step,0)
  post <- round(post/step,0)
  Td <- 0
  Ta <- 0

  Ch1 <- ECG_data$rate
  rrt <- ECG_data$times

  ch1_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
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
    }
  }
  ch1_a2f <- as.data.frame(ch1_a2f)

  return(ch1_a2f) # rows = number of trials, columns = number of timestamps
}



get_good_ECG <- function(blocks,a2f){
  nbl <- unique(blocks)
  ch1_a2f <- as.matrix(a2f)

  Ngood1 <- NULL
  Ntotal <- NULL
  perGood <- NULL
  for (i in 1:length(nbl)){
    ix <-which(blocks==nbl[i])
    sd0 <- rep(NA,length(ix))
    tempdata <- ch1_a2f[ix,]
    for (j in 1:length(ix)){
      sd0[j] <- sd(as.double(unlist(tempdata[j,])),na.rm=TRUE)
    }
    Ngood1[i] <- sum(!is.na(ch1_a2f[ix,]) & sd0 < 5*median(sd0),na.rm=TRUE)
    Ntotal[i] <- ncol(tempdata)*nrow(tempdata)
    perGood[i] <- Ngood1[i]/Ntotal[i]
  }
  Ngood_df <- dplyr::tibble(per_Good=perGood,block=nbl)
  return(Ngood_df)
}




