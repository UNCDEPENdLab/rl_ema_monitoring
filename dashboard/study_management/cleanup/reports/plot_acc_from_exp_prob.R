# check that file system expected exists and create the plot
#fpath <- paste0(site, "/static/static/subjects/", subj)
# create the directory if it doesn't exist
#dir.create(path=paste0(site, "/static/static/subjects/", subj), showWarnings = FALSE, recursive = TRUE)
# save the graph as an image file
fig_name = paste0("acc_from_exp_prob.png")
try({
  acc_from_exp_prob_graph <- ggplot(data=relative_accuracy_by_block, aes(x=block, y=mean*100, group=feedback, colour=feedback)) +
    geom_line() +
    geom_point(aes(shape = feedback), size=2)+
    geom_hline(yintercept=overall_relative_with_feedback_mean, linetype="dashed", color = "cyan4")+
    geom_hline(yintercept=overall_relative_no_feedback_mean, linetype="dashed", color = "firebrick2")+
    ylab("% correct choice")+
    ggtitle("Accuracy according to experienced probabilities") +
    labs(subtitle=sprintf("Subject ID: %s", subj)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(0, max(relative_accuracy_by_block$block),by = 1))+
    geom_text(aes(0,overall_relative_with_feedback_mean,label =round(overall_relative_with_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="cyan4")+
    geom_text(aes(0,overall_relative_no_feedback_mean,label =round(overall_relative_no_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="firebrick2")
  png(paste0(plots_path, "/", fig_name), res=300, width=9, height=5, units="in")
  print(acc_from_exp_prob_graph)
  dev.off()
})