# check that file system expected exists and create the plot
fpath <- paste0(site, "/static/static/subjects/", subj)
# create the directory if it doesn't exist
dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
# save the graph as an image file
fig_name = paste0("elated_sad_graph.png")
try({
  e_vs_s_graph <-ggplot(valence_elated_sad, aes(x=valence, y=elated_sad)) + geom_point()+ylab("Elated-Sad")+xlab("Valence")+
    geom_point(color = "#00AFBB")+ 
    ggtitle("Correlation of Valence with (Elated-Sad)") +
    labs(subtitle=sprintf("Subject ID: %s", subj))
  png(paste0(fpath, "/", fig_name), res=300, width=5, height=5, units="in")
  print(e_vs_s_graph)
  dev.off()
})