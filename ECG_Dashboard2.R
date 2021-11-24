# 2021-03-09 AndyP
# This function analyzes ECG from an SQLlite .db file and extracts %avg per block for use in the Dashboard

library("RSQLite")
library("dplyr")

#compile and source C++ functions if not already in cache
#Rcpp::sourceCpp("data_utils/timings2samples_cpp.cpp", cacheDir = getwd()) #not used at present because slower than block-wise

#set this to some path on your computer where the repo lives
#the cache holds the compiled function so it doesn't have to recompile later
if(!exists("repo_path",envir = .GlobalEnv)) {
  repo_path <- "~/Data_Analysis/Momentum"
}
##add C++11 flag for MacOS BigSur Clang & RcppArmadillo for R 4.0
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

#print(getwd())
#Rcpp::sourceCpp(file.path(repo_path, "rl_ema_monitoring/data_utils/timings2samples_block_cpp.cpp"), cacheDir = getwd())
Rcpp::sourceCpp("../../data_utils/timings2samples_block_cpp.cpp", cacheDir = getwd())
#Rcpp::sourceCpp("~/Momentum/rl_ema_monitoring/data_utils/timings2samples_block_cpp.cpp")
#warning('AndyP changed this path to work on his computer to debug ECG, please change back on dashboard if he forgets to reset it')

#test case
if(FALSE) {
  HRstep <- 10
  sample_rate <- 100

  behavior = dbConnect(SQLite(), "123_schedule.db")
  trials = dbGetQuery(behavior, "SELECT * FROM trials")
  ## remove blocks that have not been played yet
  if (length(which(is.na(trials$choice)))!=0){
    trials=trials[-c(which(is.na(trials$choice))),]}
  fbt <- trials$feedback_time

  ECG = dbConnect(SQLite(), "123_physio.db")
  ECGd = dbGetQuery(ECG,"SELECT time_ms, rr_intervals, heartrate, contact FROM Polar_heartrate ORDER BY time_ms ASC")

}

correctTimings <- function(times, intervals) {
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
  #Notes on expected data format for rr_intervals
  # "" means that no RR was recorded (often when contact=='false') -> converted to 0
  # [] indicates a blank RR -> converted to NA
  # [number, number] indicates multiple events within a time interval -> expanded to elements of intervals
  # [number] is a single interval -> expanded to one element of intervals
  
  rr_parse <- gsub("^$", "0", ECGd$rr_intervals) #empty rows become 0
  rr_parse <- gsub("[]", "NA", rr_parse, fixed=TRUE) #blank RRs become NA
  rr_parse <- gsub("[\\[\\]]", "", rr_parse, perl=TRUE) #delete [ and ] from all strings to split
  rr_list <- strsplit(rr_parse, "\\s*,\\s*", perl=TRUE) #split elements on comma
  n_times <- sapply(rr_list, length) #count numbers in each rr_interval for replicating heartrate and time_ms

  #not sure why we need a one-column data.frame as opposed to just a vector, but leaving as-is
  #consider changing intervals, hr1, hrt1 to vectors for simplicity?
  intervals <- data.frame(intervals=type.convert(unlist(rr_list), na.strings = "NA",as.is=TRUE))
  hr1 <- data.frame(heart_rate=rep(ECGd$heartrate, times=n_times))
  hrt1 <- data.frame(time=as.numeric(rep(ECGd$time_ms, times=n_times))) #stored as integer64 internally? Just make it numeric

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

  if (length(isplit)>0){
    todelete <- NULL
    iC <- 1
    for (i in 1:(length(isplit)-1)){
      if (isplit[i+1]-isplit[i] < minseg){
        todelete[iC] <- i+1
        iC <- iC+1
      }
    }
  }

  if (length(isplit)>0){
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
  
  if (length(isplit)>0){
    I0 <- NULL
    HRsplit <- matrix(list(),length(isplit)+1,2)
    HRsplit[[1,1]] <- hrt1[1:(isplit[1]-1)]
    HRsplit[[1,2]] <- intervals[1:(isplit[1]-1)]
    nosplit = FALSE
    for (i in 1:(length(isplit)-1)){
      HRsplit[[i+1,1]] <- hrt1[isplit[i]:(isplit[i+1]-1)]
      HRsplit[[i+1,2]] <- intervals[isplit[i]:(isplit[i+1]-1)]
    }
  } else {
    t0 <- NULL
    I0 <- NULL
    for (q in 1:length(intervals)){
      t0[q] <- hrt1[[q]]
      I0[q] <- intervals[[q]]
      nosplit = TRUE
    }
  }



  # merge sections
if (!nosplit){
  wiggleroom <- NULL
  beattimes <- matrix(list(),nrow(HRsplit),1)
  times1 <- matrix(list(),nrow(HRsplit),1)
  intervals1 <- matrix(list(),nrow(HRsplit),1)
  rate1 <- matrix(list(),nrow(HRsplit),1)
  iD <- 1;
  for (i in 1:nrow(HRsplit)){
    if ((sum(HRsplit[[i,2]]>0)>=2) & !nosplit){
      timings <- correctTimings(HRsplit[[i,1]],HRsplit[[i,2]])
      beattimes[[i,1]] <- timings
      output <- timings2samples_block_cpp(timings,HRstep=10)
      #output <- timings2samples(timings,HRstep=10)
      times1[[i,1]] <- output[,1]
      intervals1[[i,1]] <- output[,2]
      rate1[[i,1]] <- output[,3]
      iD <- iD+1;
    }
  }
}
  if ((sum(I0>0)>=2) & nosplit){
    beattimes <- matrix(list(),length(t0),1)
    timings <- correctTimings(t0,I0)
    beattimes <- timings
    output <- timings2samples_block_cpp(timings,HRstep=10)
    #output <- timings2samples(timings,HRstep=10)
    times1 <- output[,1]
    intervals1 <- output[,2]
    rate1 <- output[,3]
  }
  
  # check
  stopifnot(length(beattimes)==length(times1) | length(times1)==length(intervals1) | length(intervals1)==length(rate1))

  # merge data
  if (!nosplit){
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
  }
  if (nosplit){
    times2 <- times1
    intervals2 <- intervals1
    rate2 <- rate1
    beattimes2 <- beattimes
    for (i in 2:length(beattimes)){
      if (length(times1)> 0){
        nanHRsteps <- ((times1[1] - times2[length(times2)]) / HRstep) - 1
        if (nanHRsteps > 0){
          temp_times <- times2[length(times2)]+seq(from=HRstep,to=nanHRsteps*HRstep,by=HRstep)
          times2[(length(times2)+1):(length(times2)+length(temp_times))] <- temp_times
        }
        temp_intervals <- rep(NA,length(times2)-length(intervals2))
        temp_rate <- rep(NA,length(times2)-length(rate2))
        beattimes2[(length(beattimes2)+1):(length(beattimes2)+length(beattimes[i]))] <- beattimes[i]
        times2[(length(times2)+1):(length(times2)+length(times1[i]))] <- times1[i]
        temp_intervals[(length(temp_intervals)+1):(length(temp_intervals)+length(intervals1[i]))] <- intervals1[i]
        intervals2[(length(intervals2)+1):(length(intervals2)+length(temp_intervals))] <- temp_intervals
        temp_rate[(length(temp_rate)+1):(length(temp_rate)+length(rate1[i]))] <- rate1[i]
        rate2[(length(rate2)+1):(length(rate2)+length(temp_rate))] <- temp_rate
      }
    }

  }

  ECG_data <- data.frame(times=times2,rate=rate2)
  return(ECG_data)
}

# get epochs around feedback +/- 500ms
ecg_epochs_around_feedback <- function(ECG_data,fbt,pre=1000,post=10000,sample_rate=100){
  
  #library(parallel)
  #library(foreach)
  
  step <- 1000/sample_rate
  pre <- round(pre/step,0)
  post <- round(post/step,0)
  Td <- 0
  Ta <- 0

  Ch1 <- ECG_data$rate
  rrt <- ECG_data$times

  ch1_a2f <- matrix(NA,nrow=length(fbt),ncol=pre+post+1);
  for (i in 1:length(fbt)){ 
    print(paste0(i,'/',length(fbt)))
    fbt0 <- which(rrt>fbt[i])
    if (length(fbt0)>0){
      if (fbt0[1]>1){
        ind <- seq(fbt0[1]-pre,fbt0[1]+post,by=1)
      } else {
        ind <- NULL
      }
    } else {
      ind <- NULL # 2021-05-24 AndyP
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
      ch1_a2f[i,1:length(ind)] <- c(Ch1[ind],1:addpost) # 2021-05-24 AndyP
    } else if (aL > 0 & is.null(addpost)){
      ch1_a2f[i,1:length(ind)] <- c(Ch1[ind])
    }
  }
  ch1_a2f <- as.data.frame(ch1_a2f)

  return(ch1_a2f) # rows = number of trials, columns = number of timestamps
}

ecg_epochs_around_feedback2 <- function(ECG_data,fbt,pre=1000,post=10000,sample_rate=100,thread=6){
  step <- 1000/sample_rate
  pre <- round(pre/step,0)
  post <- round(post/step,0)
  ECG_data$Date <- as.Date(as.POSIXct(ECG_data$times/1000,origin='1970-01-01',tz='America/New_York'))
  cl<-parallel::makeForkCluster(thread)
  fbt_sp <- split(fbt,as.Date(as.POSIXct(fbt/1000,origin='1970-01-01',tz='America/New_York')))
  ch1_a2f<-do.call(rbind,parallel::parLapply(cl,1:length(fbt_sp),function(y){
    system(paste0("echo Processing Day ",y," of ECG data"))
    sub_ECG <- ECG_data[which(ECG_data$Date==names(fbt_sp)[[y]]),]
    ax<-do.call(rbind,lapply(fbt_sp[[y]],function(x) {
      fb0 <- which(sub_ECG$times > x)[1]
      if(is.na(fb0)){
        return(matrix(NA,ncol = pre+post+1,nrow = 1))
      }
      indx <- (fb0-pre):(fb0+post)
      indx[indx<1 | indx > nrow(sub_ECG)] <- NA
      a<-matrix(sub_ECG$rate[indx],nrow = 1)
      rownames(a)<-x
      return(a)
    }))
    return(ax)
  }))
  parallel::stopCluster(cl)

  return(ch1_a2f) # rows = number of trials, columns = number of timestamps
}

get_good_ECG <- function(blocks,ch1_a2f){
  nbl <- unique(blocks)

  Nmissing <- NULL
  Ntrials <- NULL
  Nnoisy <- NULL
  perGood <- NULL
  for (i in 1:length(nbl)){
    ix <-which(blocks==nbl[i])
    tempdata <- ch1_a2f[ix,]
    temp <- NA
    for (ic in 1:nrow(tempdata)){
      temp[ic] <- sum(is.na(tempdata[ic,]))
    }
    Nmissing[i] <- sum(temp > 0)
    temp1 <- NULL
    for (ir in 1:nrow(tempdata)){
      sd0 <- sd(tempdata[ic,],na.rm=TRUE)
      temp1[ir] <- sum(sd0 > 5*median(sd0,na.rm=TRUE))
    }
    Nnoisy[i] <- sum(temp1)
    Ntrials[i] <- length(ix)
    perGood[i] <- (Ntrials[i]-Nnoisy[i]-Nmissing[i])/Ntrials[i]
  }
  Ngood_df <- dplyr::tibble(per_Good=perGood,block=nbl)
  return(Ngood_df)
}

