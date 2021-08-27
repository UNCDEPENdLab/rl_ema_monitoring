# check that file system expected exists and create the plot
fpath <- paste0(site, "/static/static/subjects/", subj)
# create the directory if it doesn't exist
dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
# save the graph as an image file
fig_name = paste0("arousal_energetic_graph.png")
try({
  a_vs_e_graph <-ggplot(arousal_energetic, aes(x=arousal, y=energetic)) + geom_point()+ylab("Energetic")+xlab("Arousal")+
    geom_point(color = "#00AFBB")+ 
    ggtitle("Arousal/Energetic")
  png(paste0(fpath, "/", fig_name))
  print(a_vs_e_graph)
  dev.off()
})