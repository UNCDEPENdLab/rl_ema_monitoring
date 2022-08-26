# check that file system expected exists and create the plot
#fpath <- paste0(site, "/static/static/subjects/", subj)
# create the directory if it doesn't exist
#dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
# save the graph as an image file
fig_name = paste0("sb_graph.png")
try({
  sb_graph <- ggplot(data=side_bias_by_block, aes(x=block, y=mean)) +
    geom_line() +
    ylab("side bias")+
    geom_hline(yintercept=0, linetype="dashed", color = "firebrick2") +
    geom_hline(yintercept=1, linetype="dashed", color = "firebrick2") +
    ggtitle("Side bias by block") +
    labs(subtitle=sprintf("Subject ID: %s", subj)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE))
    #scale_x_continuous(breaks = seq(0, max(side_bias_by_block$block),by = 1))
  if (exists("blocks_with_poor_performance")){
    if (length(blocks_with_poor_performance==1)){
      if (!is.na(blocks_with_poor_performance)) {
        if (!isempty(blocks_with_poor_performance$poor_blocks)){
          sb_graph=sb_graph+geom_point(data=blocks_with_poor_performance, aes(x=poor_blocks, y=0.5), color="darkorchid1", size=2.2)
        }
      }
    }
  }
  png(paste0(plots_path, "/", fig_name), res=300, width=9, height=5, units="in")
  print(sb_graph)
  dev.off()
})