# get the
# subj -> subj  Shane -> AndyP
# 2021-09-10 AndyP we are now plotting sliding blocks of step 5
sliding_window = FALSE

# 2021-11-30 AndyP using individual subject's processed physio
path_to_physio <- paste0(dataPath,'/Subjects/',subj,'/physio')
physio_proc <- list.files(path_to_physio,pattern=paste0(subj,'_physio_proc.rdata'))
if (length(physio_proc)==1){
  all_output <- output # preserve global variable output (which is the processed schedule file) into a temporary variable
  load(paste0(path_to_physio,'/',physio_proc)) # loads a variable called output into global environment
  num_block <- output$eeg_summary$block
  df <- output$ecg_fb
  trial_df <- output$trial_df
  blocks <- trial_df$block
  trials <- trial_df$trial
  output <- all_output # revert output to processed schedule file
}

if (sliding_window){
  if (length(num_block)>=5){
    loopseq <- seq(from=5,to=length(num_block),by=1)
  } else { # fewer than 5 blocks, just plot what we have
    loopseq <- 1
  }
} else {
  loopseq <- 1:length(num_block)
}

a2f <- round(ncol(df)/6) # default times are -1000 to 10000 ms for ECG, just getting feedback time = 0
#y <- rep(seq(1,nrow(df),length.out=nrow(df)),1)
y <- seq(from=-1000, to=10000, length.out=ncol(df));
a2f <- which(y==0)


#path_to_schedule <- paste0(dataPath, '/Subjects/', subj, '/schedule')
#sched_file <- list.files(path=path_to_schedule,pattern=paste0(subj,'_schedule.db'))
#if (length(sched_file)==1){
  # sched_data_for_physio = dbConnect(SQLite(), paste0(path_to_schedule, '/', sched_file))
  # data = dbGetQuery(sched_data_for_physio, "SELECT * FROM trials")
  # ## remove blocks that have not been played yet
  # if (length(which(is.na(data$choice)))!=0){
  #   data=data[-c(which(is.na(data$choice))),]
  # }
  # fbt <- data$feedback_time
  # blocks <- data$block
  # trials <- data$trial
  # choice <- data$choice
  # fbt <- fbt[blocks < 1000]
  # trials <- trials[blocks < 1000]
  # choice <- choice[blocks < 1000]
  # blocks <- blocks[blocks < 1000]
  # trials <- trials[!is.na(fbt)]
  # choice <- choice[!is.na(fbt)]
  # blocks <- blocks[!is.na(fbt)]
  # fbt <- fbt[!is.na(fbt)]
  # fbt <- fbt[blocks <= max(num_block)]
  # trials <- trials[blocks <=max(num_block)]
  # choice <- choice[blocks <=max(num_block)]
  # blocks <- blocks[blocks <=max(num_block)]
  #} else {
  #warning('Zero or multiple schedule .db files found for subject',subj, 'reverting to processed schedule file, be warned this has caused errors in the past')
  #data <- output$proc_data[[subj]]$raw_data$trials$trial
  #trials <- trials[-c(which(is.na(output$proc_data[[subj]]$raw_data$trials$choice)))]
  #blocks <- output$proc_data[[subj]]$raw_data$trials$block
  #blocks <- blocks[-c(which(is.na(output$proc_data[[subj]]$raw_data$trials$choice)))]
  # remove trials that have not been played
  # notplayed <- which(is.na(output$proc_data[[subj]]$raw_data$trials$choice))
  # if (length(notplayed)>0){
  #   trials <- data[-c(notplayed)]
  #   blocks <- blocks[-c(notplayed)]
  # }
  # fbt <- fbt[blocks < 1000]
  # trials <- data[blocks < 1000]
  # blocks <- blocks[blocks < 1000]
#}

for (i in loopseq) {
  if (blocks[i] < 1000){
  try({
    if (i==min(loopseq)){
      df <- df %>% mutate(trial=trials, block=blocks)
      isX <- df %>% select(starts_with("X"))
      isV <- df %>% select(starts_with("V"))
      if (length(isX)>0){
        df <- df %>% pivot_longer(cols=starts_with("X"),names_to="t", values_to="X")
      } else if (length(isV)>0){
        df <- df %>% pivot_longer(cols=starts_with("V"),names_to="t", values_to="X")
      } else {
        warning('Check column names of df0 and see what letter R appended to the beginning of them.  Add that as a conditional in the if else here as above')
      }
      df <- df %>% filter(block < 1000)
      df <- df %>% mutate(t0=as.numeric(substr(t,2,nchar(t))))
      df <- df %>% dplyr::group_by(block,t0) %>% dplyr::summarize(mean1 = mean(X, na.rm = TRUE), st_err=sd(X,na.rm=TRUE)/(sqrt(length(X)-sum(is.na(X)))))
      df1 <- df %>% group_by(t0) %>% summarize(mean2 = mean(mean1,na.rm=TRUE),st_err = sd(mean1,na.rm=TRUE)/(sqrt(length(mean1)-sum(is.na(mean1)))))
      # check that file system expected exists and create the plot
      #fpath <- paste0(site, "/static/static/subjects/", subj)
      # create the directory if it doesn't exist
      #dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
      # save the graph as an image file
      fig_name = paste0("ecg_plot_overall.png")
      #if (!file.exists(paste0(plots_path, "/", fig_name))) {
        ecg_plot <- ggplot(df1, aes(x=t0,y=mean2)) + geom_line() + geom_point() +
          geom_ribbon(aes(ymin = mean2-st_err, ymax = mean2+st_err), linetype=2, alpha=0.1) +
          scale_x_continuous(breaks=c(0,a2f,max(df$t0)),labels=c(-1000,0,10000),name='time [ms]') +
          geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
          ggtitle(sprintf("Subject ECG Blocks Overall"))
        png(paste0(plots_path, "/", fig_name), res=300, width=7, height=7, units="in")
        print(ecg_plot)
        dev.off()
      #}
    }
    if (sliding_window){
      if (length(num_block)>=5){
        df0 <- df %>% filter(block==i-4:i)
      } else {
        df0 <- df# do not filter, there is only 1 window to plot
      }
    } else {
      df0 <- df %>% filter(block==i-1)
    }

    fig_name = paste0("ecg_plot_", toString(i-1), ".png")
    if (!file.exists(paste0(plots_path, "/", fig_name))) {
      # create the plot
      #eeg_plot <- NULL
      try({
        if (sliding_window){
          if (length(num_block)>=5){
            png(paste0(plots_path, "/", fig_name), res=300, width=7, height=7, units="in")
            ecg_plot <- ggplot(df0, aes(x=t0,y=mean1)) + geom_line() + geom_point() +
              geom_ribbon(aes(ymin = mean1-st_err, ymax = mean1+st_err), linetype=2, alpha=0.1) +
              scale_x_continuous(breaks=c(0,a2f,max(df0$t0)),labels=c(-1000,0,10000),name='time [ms]') +
              geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
              ggtitle(sprintf("Subject %s ECG Blocks %s - %s", subj, toString(i-4),toString(i)))
            print(ecg_plot)
            dev.off()
          }
          else {
            png(paste0(plots_path, "/", fig_name), res=300, width=7, height=7, units="in")
            ecg_plot <- ggplot(df0, aes(x=t0,y=mean1)) + geom_line() + geom_point() +
              geom_ribbon(aes(ymin = mean1-st_err, ymax = mean1+st_err), linetype=2, alpha=0.1) +
              scale_x_continuous(breaks=c(0,a2f,max(df0$t0)),labels=c(-1000,0,10000),name='time [ms]') +
              geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
              ggtitle(sprintf("Subject %s ECG Blocks %s - %s", subj, toString(1),toString(4)))
            print(ecg_plot)
            dev.off()
          }
        } else {
          png(paste0(plots_path, "/", fig_name), res=300, width=7, height=7, units="in")
          ecg_plot <- ggplot(df0, aes(x=t0,y=mean1)) + geom_line() + geom_point() +
            geom_ribbon(aes(ymin = mean1-st_err, ymax = mean1+st_err), linetype=2, alpha=0.1) +
            scale_x_continuous(breaks=c(0,a2f,max(df0$t0)),labels=c(-1000,0,10000),name='time [ms]') +
            geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
            ggtitle(sprintf("Subject %s ECG Blocks %s", subj, toString(i-1)))
          print(ecg_plot)
          dev.off()
        }
      })
    }
  })
  }
}

