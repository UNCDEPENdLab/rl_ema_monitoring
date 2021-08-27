#learning trend: a sliding window in a size of 10 trials which calculates subject's accuracy according to designated probabilities (starts being calculated only from block 6, after the practice session)
test_learning=subset(trials_1, (((trials_1$block>5)&trials_1$feedback==1)))
test_learning=test_learning[-c(3:13)]
test_learning=test_learning[-c(4:8)]
if (nrow(test_learning)!=0){
  learning_matrix=data.frame(block=rep(c(6:max(test_learning$block)), each=39), window=rep(c(1:39), max(test_learning$block)-5), accuracy=rep(NA, 39*(max(test_learning$block)-5)))
  t=1
  for (i in seq(from=1, to=which(learning_matrix==max(learning_matrix$block))[1], by=39)){
    learning_matrix[i:(i+38),3]=rollapply(test_learning$accuracy[t:(t+47)], width = 10, by = 1, FUN = mean, align = "left")
    t=t+48
  }
  # check that file system expected exists and create the plot
  fpath <- paste0(site, "/static/static/subjects/", subj)
  # create the directory if it doesn't exist
  dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
  # save the graph as an image file
  fig_name = paste0("lr_plot.png")
  accuracy_by_window=plyr::ddply(learning_matrix, c("window"),summarise,mean=mean(accuracy, na.rm = T))
  try({
    lr_graph <- ggplot(data=accuracy_by_window, aes(x=window, y=mean*100)) +
      geom_line(color="skyblue3") +
      ylab("% correct choice")+
      xlab("window")+
      ggtitle("Learning process")
    png(paste0(fpath, "/", fig_name))
    print(lr_graph)
    dev.off()
  })
}
