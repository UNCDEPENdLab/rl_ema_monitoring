

library(pals)

sliding_window = FALSE

num_block <- output_physio$eeg$summary[[subj]]$block
if (sliding_window){
  if (length(num_block)>=5){
    loopseq <- seq(from=5,to=length(num_block),by=1)
  } else { # fewer than 5 blocks, just plot what we have
    loopseq <- 1
  }
} else {
  loopseq <- 1:length(num_block)
}
# preliminary calcs
df <- output_physio$eeg$fb
df <- df[[subj]]
#a2f <- round(ncol(df$ch1)/4) # default times are -500 to 1500 ms for EEG
#y <- rep(seq(1,nrow(df$ch1),length.out=nrow(df$ch1)),4)
y <- seq(from=-500, to=1500, length.out=length(df$ch1));
name <- names(df)
dq <- rbind(df[[name[1]]],df[[name[[2]]]],df[[name[3]]],df[[name[4]]]) # concatenate channels 1-4
dg <- rbind(df[[name[5]]],df[[name[[6]]]],df[[name[7]]],df[[name[8]]]) # concatenate channels 1-4

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
      if (i==1){
        dq1 <- dq %>% mutate(trial=trials,blocks=blocks,totaltrial=rep(1:nT,4))
        dg1 <- dg %>% mutate(trial=trials,blocks=blocks,totaltrial=rep(1:nT,4))
      }
      if (sliding_window){
        if (length(num_block)>=5){
          dq0 <- dq %>% mutate(trial=trials, blocks==blocks) %>% filter(blocks==i-4:i)
          dg0 <- dg %>% mutate(trial=trials, blocks==blocks) %>% filter(blocks==i-4:i)
        } else {
          dq0 <- dq %>% mutate(trial=trials, blocks==blocks) # do not filter, there is only 1 window to plot
          dg0 <- dg %>% mutate(trial=trials, blocks==blocks)
        }
      } else {
        dq0 <- dq %>% mutate(trial=trials,blocks==blocks) %>% filter(blocks==i)
        dg0 <- dg %>% mutate(trial=trials,blocks==blocks) %>% filter(blocks==i)
      }
      isX <- dq0 %>% select(starts_with("X"))
      isV <- dq0 %>% select(starts_with("V"))
      if (length(isX)>0){
        dq0 <- dq0 %>% pivot_longer(cols=starts_with("X"),names_to="ix", values_to="V")
        dg0 <- dg0 %>% pivot_longer(cols=starts_with("X"),names_to="ix", values_to="V")
        if (i==1){
          dq1 <- dq1 %>% pivot_longer(cols=starts_with("X"),names_to="ix",values_to="V")
          dg1 <- dg1 %>% pivot_longer(cols=starts_with("X"),names_to="ix",values_to="V")
        }
      } else if (length(isV)>0){
        dq0 <- dq0 %>% pivot_longer(cols=starts_with("V"),names_to="ix", values_to="V")
        dg0 <- dg0 %>% pivot_longer(cols=starts_with("V"),names_to="ix", values_to="V")
        if (i==1){
          dq1 <- dq1 %>% pivot_longer(cols=starts_with("V"),names_to="ix", values_to="V")
          dg1 <- dg1 %>% pivot_longer(cols=starts_with("V"),names_to="ix", values_to="V")
        }
      } else {
        warning('Check column names of dq0 and see what letter R appended to the beginning of them.  Add that as a conditional in the if else here as above')
      }
      dq0 <- dq0 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix)))) %>% arrange(channel,trial,t0)
      dq0 <- dq0 %>% mutate(t=rep(y,4*length(unique(dq0$trial))))
      dg0 <- dg0 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix)))) %>% arrange(channel,trial,t0)
      dg0 <- dg0 %>% mutate(t=rep(y,4*length(unique(dg0$trial))))
      dg0 <- dg0 %>% mutate(V1 = case_when(
        V==1 ~ 1,
        V==2 ~ 1,
        V==3 ~ 0,
        V==4 ~ 0
      ))
      if (i==1){
        dq1 <- dq1 %>% filter(blocks < 1000)
        dq1 <- dq1 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix))))
        dq1 <- dq1 %>% mutate(t=rep(y,nrow(dq1)/length(y)))
        dq1 <- dq1 %>% group_by(channel) %>% mutate(zscore=(V-mean(V,na.rm=TRUE))/sd(V,na.rm=TRUE))
        #dq1 <- dq1 %>% filter(abs(zscore) <= 5)
        dq1 <- dq1 %>% mutate(V1 = case_when(
          V < 700 ~ 700,
          V >=700 & V<=900 ~ V,
          V >900 ~ 900
        ))
        
        dg1 <- dg1 %>% filter(blocks < 1000)
        dg1 <- dg1 %>% mutate(t0=as.numeric(substr(ix,2,nchar(ix))))
        dg1 <- dg1 %>% mutate(t=rep(y,nrow(dg1)/length(y)))
        dg1 <- dg1 %>% mutate(V1 = case_when(
          V==1 ~ 1,
          V==2 ~ 1,
          V==3 ~ 0,
          V==4 ~ 0
        ))
        
      }
      dq0 <- dq0 %>% mutate(zscore=(V-mean(V,na.rm=TRUE))/sd(V,na.rm=TRUE))
      
      
      #dq0 <- dq0 %>% filter(abs(zscore) <= 5)  
      # check that file system expected exists and create the plot
      #fpath <- paste0(site, "/static/static/subjects/", subj)
      # create the directory if it doesn't exist
      #dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
      # save the graph as an image file
      if (i==1){
        fig_name = paste0("eeg_plot_overall.png")
        if (!file.exists(paste0(plots_path, "/", fig_name))) {
          try({
            eeg_plot <- ggplot(dq1, aes(t,totaltrial,fill=V)) + 
              geom_raster(interpolate=TRUE) + facet_wrap(~channel,scales="free") + 
              scale_y_continuous(breaks=dq1$totaltrial[which(diff(as.matrix(dq1$blocks))==1)],labels=dq1$blocks[which(diff(as.matrix(dq1$blocks))==1)]) +
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
        }
        fig_name = paste0("eeg_plot_missingness_overall.png")
        if (!file.exists(paste0(plots_path, "/", fig_name))) {  
          try({
            eeg_plot <- ggplot(dg1, aes(t,totaltrial,fill=V1)) + 
              geom_raster(interpolate=TRUE) + facet_wrap(~channel,scales="free") + 
              scale_y_continuous(breaks=dg1$totaltrial[which(diff(as.matrix(dg1$blocks))==1)],labels=dg1$blocks[which(diff(as.matrix(dg1$blocks))==1)]) +
              scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
              theme(axis.text.y = element_text(size=2.5)) +
              ylab('blocks') + 
              geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) + 
              #geom_hline(yintercept = dq1$totaltrial[which(diff(as.matrix(dq1$blocks))==1)],size=0.1) +
              #scale_fill_viridis_c(option = "plasma",begin=0,end=1) +
              scale_fill_gradientn(colours=parula(2),guide="colourbar",limits=c(0, 1)) +
              ggtitle(sprintf("Subject %s EEG All Blocks", subj))
            # save the image
            #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
            #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
            png(paste0(plots_path, "/", fig_name), res=450, width=8, height=12, units="in")
            print(eeg_plot)
            dev.off()
          })
          
          
          
        }
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
              ggtitle(sprintf("Subject %s EEG Block %s", subj, toString(i)))
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
                scale_fill_gradientn(colours=parula(2),guide="colourbar",limits=c(0, 1)) +
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
                scale_fill_gradientn(colours=parula(2),guide="colourbar",limits=c(0, 1)) +
                ggtitle(sprintf("Subject %s EEG Blocks %s - %s", subj, toString(1), toString(4)))
              # save the image
              #plotly::export(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #plotly::orca(p=eeg_plot, file=paste0(site, "/", fpath, "/", fig_name))
              #ggsave(filename=fig_name, path=fpath, plot=eeg_plot)
              png(paste0(plots_path, "/", fig_name), res=300, width=8, height=8, units="in") 
            }
          } else {
            eeg_plot <- ggplot(dg0, aes(t,trial,fill=V1)) + 
              geom_raster(interpolate=TRUE) + facet_wrap(~channel) + 
              scale_x_continuous(breaks=c(-500,0,500,1000,1500),name='time [ms]') +
              geom_vline(xintercept = 0, lty = "dashed", color = "#FF0000", size = 2) + 
              scale_fill_gradientn(colours=parula(2),guide="colourbar",limits=c(0, 1))+
              ggtitle(sprintf("Subject %s EEG Block %s", subj, toString(i)))
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

