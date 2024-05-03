# check that file system expected exists and create the plot
#fpath <- paste0(site, "/static/static/subjects/", subj)
# create the directory if it doesn't exist
#dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
# save the graph as an image file

fig_name = paste0("rt_graph.png")
try({
  rt_graph <- ggplot(data=RT_by_block, aes(x=block, y=mean, group=feedback, colour=feedback)) +
    geom_line() +
    geom_hline(yintercept=1400, linetype="dashed", color = "firebrick2") +
    geom_point(aes(shape = feedback), size=2) +
    ylab("RT (ms)")+
    ggtitle("Reaction time by block and feedback type") +
    labs(subtitle=sprintf("Subject ID: %s", subj)) +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE))
    #scale_x_continuous(breaks = seq(0, max(RT_by_block$block),by = 1))
  
  if (exists("blocks_with_poor_performance")){
    if (length(blocks_with_poor_performance)==1){
      if (all(!is.na(blocks_with_poor_performance))){
        if (!isempty(blocks_with_poor_performance$poor_blocks)) {
          if (!any(is.na(blocks_with_poor_performance$poor_blocks))){
            rt_graph <- rt_graph + geom_point(data=blocks_with_poor_performance, aes(x=poor_blocks, y=1400), color="darkorchid1", size=2.2, inherit.aes = F)
          }
        }
      }
    }
  }
  png(paste0(plots_path, "/", fig_name), res=300, width=9, height=5, units="in")
  print(rt_graph)
  dev.off()
})
