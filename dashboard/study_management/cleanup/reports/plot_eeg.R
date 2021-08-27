# get the 
# params$id -> subj  Shane -> AndyP
num_block <- output_physio$eeg$summary[[subj]]$block
# preliminary calcs
df <- output_physio$eeg$fb
df <- df[[subj]]
a2f <- round(ncol(df$ch1)/4) # default times are -500 to 1500 ms for EEG
y <- rep(seq(1,nrow(df$ch1),length.out=nrow(df$ch1)),4)
name <- names(df)
dq <- rbind(df[[name[1]]],df[[name[[2]]]],df[[name[3]]],df[[name[4]]]) # concatenate channels 1-4

trials <- output$proc_data[[subj]]$raw_data$trials$trial
blocks <- output$proc_data[[subj]]$raw_data$trials$block
# remove trials that have not been played
notplayed <- which(is.na(output$proc_data[[subj]]$raw_data$trials$choice))
if (length(notplayed)>0){
  trials <- trials[-c(notplayed)]
  blocks <- blocks[-c(notplayed)]
}
blocks <- rep(blocks,4)
trials <- rep(trials,4)

# get an index for each trial (they are all the same length nT)
nT = nrow(dq)/4
dq <- dq %>% mutate(channel=case_when(
  as.numeric(rownames(dq)) <= nT ~ 1,
  as.numeric(rownames(dq)) <= nT*2 & as.numeric(rownames(dq))  > nT ~ 2,
  as.numeric(rownames(dq)) <= nT*3 & as.numeric(rownames(dq)) > nT*2  ~ 3,
  as.numeric(rownames(dq)) > nT*3  ~ 4
))

for (i in num_block) {
  try({
    # plot for individual subject iS out of a cached physio file
    dq0 <- dq %>% mutate(trial=trials, blocks=blocks) %>% filter(blocks==i)
    dq0 <- dq0 %>% pivot_longer(cols=starts_with("V"),names_to="t", values_to="V")
    dq0 <- dq0 %>% mutate(t0=as.numeric(substr(t,2,nchar(t))))
    dq0 <- dq0 %>% mutate(zscore=(V-mean(V,na.rm=TRUE))/sd(V,na.rm=TRUE))
    dq0 <- dq0 %>% filter(abs(zscore) <= 5)
    # check that file system expected exists and create the plot
    fpath <- paste0(site, "/static/static/subjects/", subj)
    # create the directory if it doesn't exist
    dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
    # save the graph as an image file
    fig_name = paste0("eeg_plot_", toString(i), ".png")
    # if the file doesn't exist
    #print("HELLO")
    if (!file.exists(paste0(fpath, "/", fig_name))) {
      # create the plot
      #eeg_plot <- NULL
      try({
        eeg_plot <- ggplot(dq0, aes(t0,trial,fill=V)) + geom_tile() + facet_wrap(~channel) + 
          scale_x_continuous(breaks=c(0,a2f,max(dq0$t0)),labels=c(-500,0,1500),name='time [ms]') +
          geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) + 
          scale_fill_viridis_c(option = "plasma",begin=0,end=1)
      # save the image
      #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
      #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
      #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
      png(paste0(fpath, "/", fig_name))
      print(eeg_plot)
      dev.off()
      })
    }
  })
}