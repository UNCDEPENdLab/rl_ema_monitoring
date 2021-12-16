

library(pals)

sliding_window = FALSE

# 2021-11-30 AndyP using individual subject's processed physio
path_to_physio <- paste0(dataPath,'/Subjects/',subj,'/physio')
physio_proc <- list.files(path_to_physio,pattern=paste0(subj,'_physio_proc.rdata'))
if (length(physio_proc)==1){
  all_output <- output # preserve global variable output (which is the processed schedule file) into a temporary variable
  load(paste0(path_to_physio,'/',physio_proc)) # loads a variable called output into global environment
  num_block <- output$eeg_summary$block
  df <- output$eeg_fb
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

#a2f <- round(ncol(df$ch1)/4) # default times are -500 to 1500 ms for EEG
#y <- rep(seq(1,nrow(df$ch1),length.out=nrow(df$ch1)),4)
y <- seq(from=-500, to=1500, length.out=length(df$ch1));
name <- names(df)
dq <- rbind(df[[name[1]]],df[[name[[2]]]],df[[name[3]]],df[[name[4]]]) # concatenate channels 1-4
dg <- rbind(df[[name[5]]],df[[name[[6]]]],df[[name[7]]],df[[name[8]]]) # concatenate channels 1-4

#path_to_schedule <- paste0(dataPath, '/Subjects/', subj, '/schedule')
#sched_file <- list.files(path=path_to_schedule,pattern=paste0(subj,'_schedule.db'))
#if (length(sched_file)==1){
  # sched_data_for_physio = dbConnect(SQLite(), paste0(path_to_schedule, '/', sched_file))
  # data = dbGetQuery(sched_data_for_physio, "SELECT * FROM trials")
  # ## remove blocks that have not been played yet
  # if (length(which(is.na(data$choice)))!=0){
  #   data=data[-c(which(is.na(data$choice))),]
  #   }
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
  # warning('Zero or multiple schedule .db files found for subject',IDx, 'reverting to processed schedule file, be warned this has caused errors in the past')
  # data <- output$proc_data[[subj]]$raw_data$trials$trial
  # #trials <- trials[-c(which(is.na(output$proc_data[[subj]]$raw_data$trials$choice)))]
  # blocks <- output$proc_data[[subj]]$raw_data$trials$block
  # #blocks <- blocks[-c(which(is.na(output$proc_data[[subj]]$raw_data$trials$choice)))]
  # # remove trials that have not been played
  # notplayed <- which(is.na(output$proc_data[[subj]]$raw_data$trials$choice))
  # if (length(notplayed)>0){
  #   trials <- data[-c(notplayed)]
  #   blocks <- blocks[-c(notplayed)]
  # }
  # fbt <- fbt[blocks < 1000]
  # trials <- data[blocks < 1000]
  # blocks <- blocks[blocks < 1000]
#}

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
dg <- dg %>% mutate(channel=case_when(
  as.numeric(rownames(dg)) <= nT ~ 1,
  as.numeric(rownames(dg)) <= nT*2 & as.numeric(rownames(dg))  > nT ~ 2,
  as.numeric(rownames(dg)) <= nT*3 & as.numeric(rownames(dg)) > nT*2  ~ 3,
  as.numeric(rownames(dg)) > nT*3  ~ 4
))

for (i in loopseq) {
  if (blocks[i] < 1000){
    try({
      # plot for individual subject iS out of a cached physio file
      if (i==min(loopseq)){
        dq1 <- dq %>% mutate(trial=trials,blocks=blocks,totaltrial=rep(1:nT,4))
        dg1 <- dg %>% mutate(trial=trials,blocks=blocks,totaltrial=rep(1:nT,4))
        isX <- dq1 %>% select(starts_with("X"))
        isV <- dq1 %>% select(starts_with("V"))
      if (length(isX)>0){
          dq1 <- dq1 %>% pivot_longer(cols=starts_with("X"),names_to="ix",values_to="V")
          dg1 <- dg1 %>% pivot_longer(cols=starts_with("X"),names_to="ix",values_to="V")
      } else if (length(isV)>0){
          dq1 <- dq1 %>% pivot_longer(cols=starts_with("V"),names_to="ix", values_to="V")
          dg1 <- dg1 %>% pivot_longer(cols=starts_with("V"),names_to="ix", values_to="V")
      } else {
        warning('Check column names of dq0 and see what letter R appended to the beginning of them.  Add that as a conditional in the if else here as above')
      }
        sd_times <- 10
        dq1 <- dq1 %>% mutate(t=rep(y,nrow(dq1)/length(y)))
        dg1 <- dg1 %>% mutate(t=rep(y,nrow(dq1)/length(y)))
        dq1 <- dq1 %>% filter(blocks < 1000)
        dq1 <- dq1 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix))))
        dg1 <- dg1 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix))))
        A <- dq1 %>% select('channel','totaltrial','t0','V')
        dg1 <- dg1 %>% rename(G="V")
        dg1 <- inner_join(A,dg1,by=c('channel','totaltrial','t0'))
        rm(dq1)
        dg1 <- dg1 %>% mutate(V1 = case_when(
          G==1 ~ V,
          G==2 ~ V,
          G==3 ~ NA_real_,
          G==4 ~ NA_real_,
          TRUE~as.numeric(G)
        ))
        dg1 <- dg1 %>% mutate(mn=median(V1,na.rm=TRUE),sd=sd(V1,na.rm=TRUE)) %>% ungroup()
        dg1 <- dg1 %>% group_by(channel) %>% mutate(nanout = case_when(
          V1 > mn+sd_times*sd | V1 < mn-sd_times*sd | V1 < 1650/20 | V1 > 19*1650/20 ~ 1,
          V1 <= mn+sd_times*sd | V1 >= mn-sd_times*sd | V1 >= 1650/20 | V1 <= 19*1650/20 ~ 0
        )) %>% ungroup()
        dg1 <- dg1 %>% mutate(G1 = case_when(
          is.na(nanout) ~ 3,
          G==3 ~ 2,
          G==4 ~ 2,
          nanout==1 ~ 1,
          G==1 & nanout==0 ~ 0,
          G==2 & nanout==0 ~ 0
        ))
      }
      if (sliding_window){
        if (length(num_block)>=5){
          dq0 <- dg1 %>% filter(blocks==i-4:i)
          dg0 <- dg1 %>% filter(blocks==i-4:i)
        } else {
          dg0 <- dg1  # do not filter, there is only 1 window to plot
          dg0 <- dg1
        }
      } else {
        dq0 <- dg1 %>% filter(blocks==i-1)
        dg0 <- dg1 %>% filter(blocks==i-1)
      }

      dq0 <- dq0 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix)))) %>% arrange(channel,trial,t0)
      dq0 <- dq0 %>% mutate(t=rep(y,4*length(unique(dq0$trial))))
      dg0 <- dg0 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix)))) %>% arrange(channel,trial,t0)
      dg0 <- dg0 %>% mutate(t=rep(y,4*length(unique(dg0$trial))))

      #dq0 <- dq0 %>% filter(abs(zscore) <= 5)
      # check that file system expected exists and create the plot
      #fpath <- paste0(site, "/static/static/subjects/", subj)
      # create the directory if it doesn't exist
      #dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
      # save the graph as an image file
      if (i==min(loopseq)){
        fig_name = paste0("eeg_plot_overall.png")
        #if (!file.exists(paste0(plots_path, "/", fig_name))) {
          try({
            eeg_plot <- ggplot(dg1, aes(t,totaltrial,fill=V)) +
              geom_raster(interpolate=TRUE) + facet_wrap(~channel,scales="free") +
              scale_y_continuous(breaks=dg1$totaltrial[which(diff(as.matrix(dg1$blocks))==1)],labels=dg1$blocks[which(diff(as.matrix(dg1$blocks))==1)]) +
              scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
              theme(axis.text.y = element_text(size=2.5)) +
              ylab('blocks') +
              geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
              #geom_hline(yintercept = dq1$totaltrial[which(diff(as.matrix(dq1$blocks))==1)],size=0.1) +
              #scale_fill_viridis_c(option = "plasma",begin=0,end=1) +
              scale_fill_gradientn(colours=parula(1000),guide="colourbar",limits=c(0, 1650)) +
              ggtitle(sprintf("Subject %s EEG All Blocks", subj))
            # save the image
            #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
            png(paste0(plots_path, "/", fig_name), res=450, width=8, height=12, units="in")
            print(eeg_plot)
            dev.off()
          })
        #}
        fig_name = paste0("eeg_plot_missingness_overall.png")
        #if (!file.exists(paste0(plots_path, "/", fig_name))) {
          try({
            eeg_plot <- ggplot(dg1, aes(t,totaltrial,fill=G1)) +
              geom_raster(interpolate=TRUE) + facet_wrap(~channel,scales="free") +
              scale_y_continuous(breaks=dg1$totaltrial[which(diff(as.matrix(dg1$blocks))==1)],labels=dg1$blocks[which(diff(as.matrix(dg1$blocks))==1)]) +
              scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
              theme(axis.text.y = element_text(size=2.5)) +
              ylab('blocks') +
              geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
              #geom_hline(yintercept = dq1$totaltrial[which(diff(as.matrix(dq1$blocks))==1)],size=0.1) +
              #scale_fill_viridis_c(option = "plasma",begin=0,end=1) +
              scale_fill_gradientn(colours=parula(4),guide="colourbar",limits=c(0, 3)) +
              ggtitle(sprintf("Subject %s EEG All Blocks", subj))
            # save the image
            #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
            png(paste0(plots_path, "/", fig_name), res=450, width=8, height=12, units="in")
            print(eeg_plot)
            dev.off()
          })
        #}
      }
      fig_name = paste0("eeg_plot_", toString(i-1), ".png")
      # if the file doesn't exist
      #print("HELLO")
      if (!file.exists(paste0(plots_path, "/", fig_name))) {
        # create the plot
        #eeg_plot <- NULL
        try({
          if (sliding_window){
            if (length(num_block)>=5){
              eeg_plot <- ggplot(dq0, aes(t,trial,fill=V)) + geom_tile() + facet_wrap(~channel) +
                scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
                geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
                scale_fill_gradientn(colours=parula(1000),guide="colourbar",limits=c(0, 1650)) +
                ggtitle(sprintf("Subject %s EEG Blocks %s - %s", subj, toString(i-4), toString(i)))
              # save the image
              #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
              png(paste0(plots_path, "/", fig_name), res=300, width=8, height=8, units="in")
            } else {
              eeg_plot <- ggplot(dq0, aes(t,trial,fill=V)) + geom_tile() + facet_wrap(~channel) +
                scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
                geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
                scale_fill_gradientn(colours=parula(1000),guide="colourbar",limits=c(0, 1650)) +
                ggtitle(sprintf("Subject %s EEG Blocks %s - %s", subj, toString(1), toString(4)))
              # save the image
              #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
              png(paste0(plots_path, "/", fig_name), res=300, width=8, height=8, units="in")
            }
          } else {
            eeg_plot <- ggplot(dq0, aes(t,trial,fill=V)) +
              geom_raster(interpolate=TRUE) + facet_wrap(~channel) +
              scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
              geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
              scale_fill_gradientn(colours=parula(1000),guide="colourbar",limits=c(0, 1650))+
              ggtitle(sprintf("Subject %s EEG Block %s", subj, toString(i-1)))
            # save the image
            #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
            png(paste0(plots_path, "/", fig_name), res=300, width=8, height=8, units="in")
          }
          print(eeg_plot)
          dev.off()
        })
      }

      fig_name = paste0("eeg_plot_missingness_", toString(i-1), ".png")

      if (!file.exists(paste0(plots_path, "/", fig_name))) {
        # create the plot
        #eeg_plot <- NULL
        try({
          if (sliding_window){
            if (length(num_block)>=5){
              eeg_plot <- ggplot(dg0, aes(t,trial,fill=V1)) + geom_tile() + facet_wrap(~channel) +
                scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
                geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
                scale_fill_gradientn(colours=parula(4),guide="colourbar",limits=c(0, 3)) +
                ggtitle(sprintf("Subject %s EEG Blocks %s - %s", subj, toString(i-4), toString(i)))
              # save the image
              #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
              png(paste0(plots_path, "/", fig_name), res=300, width=8, height=8, units="in")
            } else {
              eeg_plot <- ggplot(dg0, aes(t,trial,fill=V1)) + geom_tile() + facet_wrap(~channel) +
                scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
                geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
                scale_fill_gradientn(colours=parula(4),guide="colourbar",limits=c(0, 3)) +
                ggtitle(sprintf("Subject %s EEG Blocks %s - %s", subj, toString(1), toString(4)))
              # save the image
              #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
              png(paste0(plots_path, "/", fig_name), res=300, width=8, height=8, units="in")
            }
          } else {
            eeg_plot <- ggplot(dg0, aes(t,trial,fill=G1)) +
              geom_raster(interpolate=TRUE) + facet_wrap(~channel) +
              scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
              geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) +
              scale_fill_gradientn(colours=parula(4),guide="colourbar",limits=c(0, 3))+
              ggtitle(sprintf("Subject %s EEG Block %s", subj, toString(i-1)))
            # save the image
            #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
            png(paste0(plots_path, "/", fig_name), res=300, width=8, height=8, units="in")
          }
          print(eeg_plot)
          dev.off()
        })
      }
    })
  }
}

