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
y <- seq(from=-1000, to=10000, by=10);
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
  try({
    if (sliding_window){
      if (length(num_block)>=5){
        df0 <- df %>% mutate(trial=trials, blocks==blocks) %>% filter(blocks==i-4:i)  
      } else {
        df0 <- df %>% mutate(trial=trials, blocks==blocks) # do not filter, there is only 1 window to plot
      }
    } else {
      df0 <- df %>% mutate(trial=trials, blocks==blocks) %>% filter(blocks==i)
    }
    isX <- df0 %>% select(starts_with("X"))
    isV <- df0 %>% select(starts_with("V"))
    if (length(isX)>0){
      df0 <- df0 %>% pivot_longer(cols=starts_with("X"),names_to="t", values_to="X")
    } else if (length(isV)>0){
      df0 <- df0 %>% pivot_longer(cols=starts_with("V"),names_to="t", values_to="X")
    } else {
      warning('Check column names of df0 and see what letter R appended to the beginning of them.  Add that as a conditional in the if else here as above')
    }
    df0 <- df0 %>% mutate(t0=as.numeric(substr(t,2,nchar(t))))
    df0 <- df0 %>% dplyr::group_by(t0) %>% dplyr::summarize(mean = mean(X, na.rm = TRUE), st_err=sd(X,na.rm=TRUE)/(sqrt(length(X)-sum(is.na(X)))))
    # check that file system expected exists and create the plot
    #fpath <- paste0(site, "/static/static/subjects/", subj)
    # create the directory if it doesn't exist
    #dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
    # save the graph as an image file
    fig_name = paste0("ecg_plot_", toString(i-1), ".png")
    if (!file.exists(paste0(plots_path, "/", fig_name))) {
      # create the plot
      #eeg_plot <- NULL
      try({
        if (sliding_window){
          if (length(num_block)>=5){
            ecg_plot <- ggplot(df0, aes(x=t0,y=mean)) + geom_line() + geom_point() + 
              geom_ribbon(aes(ymin = mean-st_err, ymax = mean+st_err), linetype=2, alpha=0.1) + 
              scale_x_continuous(breaks=c(0,a2f,max(df0$t0)),labels=c(-1000,0,10000),name='time [ms]') + 
              geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
              ggtitle(sprintf("Subject %s ECG Blocks %s - %s", subj, toString(i-4),toString(i)))
            png(paste0(plots_path, "/", fig_name), res=300, width=7, height=7, units="in")
          }
          else {
            ecg_plot <- ggplot(df0, aes(x=t0,y=mean)) + geom_line() + geom_point() + 
              geom_ribbon(aes(ymin = mean-st_err, ymax = mean+st_err), linetype=2, alpha=0.1) + 
              scale_x_continuous(breaks=c(0,a2f,max(df0$t0)),labels=c(-1000,0,10000),name='time [ms]') + 
              geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
              ggtitle(sprintf("Subject %s ECG Blocks %s - %s", subj, toString(1),toString(4)))
            png(paste0(plots_path, "/", fig_name), res=300, width=7, height=7, units="in")  
          }
        } else {
          ecg_plot <- ggplot(df0, aes(x=t0,y=mean)) + geom_line() + geom_point() + 
            geom_ribbon(aes(ymin = mean-st_err, ymax = mean+st_err), linetype=2, alpha=0.1) + 
            scale_x_continuous(breaks=c(0,a2f,max(df0$t0)),labels=c(-1000,0,10000),name='time [ms]') + 
            geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
            ggtitle(sprintf("Subject %s ECG Blocks %s - %s", subj, toString(i-1)))
          png(paste0(plots_path, "/", fig_name), res=300, width=7, height=7, units="in")  
        }
        print(ecg_plot)
        dev.off()
      })
    }
  })
}

