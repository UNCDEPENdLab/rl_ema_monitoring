# get the 
# subj -> subj  Shane -> AndyP
# 2021-09-10 AndyP we are now plotting sliding blocks of step 5
sliding_window = FALSE
num_block <- output_physio$ecg$summary[[subj]]$block # get the total number of blocks played
if (sliding_window){
  if (length(num_block)>=5){
    loopseq <- seq(from=5,to=length(num_block),by=1)
  } else { # fewer than 5 blocks, just plot what we have
    loopseq <- 1
  }
} else {
  loopseq <- 1:length(num_block)
}

# plot for individual subject iS out of a cached physio file
df <- output_physio$ecg$fb
df <- data.frame(df[[subj]])

a2f <- round(ncol(df)/6) # default times are -1000 to 10000 ms for ECG, just getting feedback time = 0
#y <- rep(seq(1,nrow(df),length.out=nrow(df)),1)
y <- seq(from=-1000, to=10000, length.out=ncol(df));
a2f <- which(y==0)
trials <- output$proc_data[[subj]]$raw_data$trials$trial
#trials <- trials[-c(which(is.na(output$proc_data[[subj]]$raw_data$trials$choice)))]
blocks <- output$proc_data[[subj]]$raw_data$trials$block
#blocks <- blocks[-c(which(is.na(output$proc_data[[subj]]$raw_data$trials$choice)))]
# remove trials that have not been played
notplayed <- which(is.na(output$proc_data[[subj]]$raw_data$trials$choice))
if (length(notplayed)>0){
  trials <- trials[-c(notplayed)]
  blocks <- blocks[-c(notplayed)]
}



for (i in loopseq) {
  if (blocks[i] < 1000){
  try({
    if (i==1){
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

