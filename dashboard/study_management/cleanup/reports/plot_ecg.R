# get the 
# subj -> subj  Shane -> AndyP
num_block <- output_physio$ecg$summary[[subj]]$block

# plot for individual subject iS out of a cached physio file
df <- output_physio$ecg$fb
df <- data.frame(df[[subj]])

a2f <- round(ncol(df)/6) # default times are -1000 to 10000 ms for ECG
y <- rep(seq(1,nrow(df),length.out=nrow(df)),1)
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

for (i in num_block) {
  try({
    df0 <- df %>% mutate(trial=trials, blocks=blocks) %>% filter(blocks==i)  
    df0 <- df0 %>% pivot_longer(cols=starts_with("X"),names_to="t", values_to="X")
    df0 <- df0 %>% mutate(t0=as.numeric(substr(t,2,nchar(t))))
    df0 <- df0 %>% dplyr::group_by(t0) %>% dplyr::summarize(mean = mean(X, na.rm = TRUE), st_err=var(X,na.rm=TRUE)/sqrt(length(X)-sum(is.na(X))))
    # check that file system expected exists and create the plot
    fpath <- paste0(site, "/static/static/subjects/", subj)
    # create the directory if it doesn't exist
    dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
    # save the graph as an image file
    fig_name = paste0("ecg_plot_", toString(i), ".png")
    if (!file.exists(paste0(fpath, "/", fig_name))) {
      # create the plot
      #eeg_plot <- NULL
      try({
        ecg_plot <- ggplot(df0, aes(x=t0,y=mean)) + geom_line() + geom_point() + 
          geom_ribbon(aes(ymin = mean-st_err, ymax = mean+st_err), linetype=2, alpha=0.1) + 
          scale_x_continuous(breaks=c(0,a2f,max(df$t0)),labels=c(-1000,0,100000),name='time [ms]') + 
          geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) +
          ggtitle(sprintf("Subject %s ECG Block %s", subj, toString(i)))
        png(paste0(fpath, "/", fig_name), res=300, width=7, height=7, units="in")
        print(ecg_plot)
        dev.off()
      })
    }
  })
}
