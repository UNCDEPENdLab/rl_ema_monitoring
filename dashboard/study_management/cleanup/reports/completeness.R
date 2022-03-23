completeness <- output$proc_data[[subj]]$task_completeness$completeness_table
try({
  html_name = "completeness_plot.html"
  fig_name = "completeness_plot.png"
  folder_name = "completeness_plot_files"
  
  completeness_figure <- plotly::plot_ly(completeness, x=~dates, y=~mood_perc, name = 'Mood Reports', type = 'scatter', mode = 'lines+markers') %>% 
      plotly::add_trace(y=~games_perc, name = 'Games', mode = 'lines+markers') %>% 
      plotly::add_trace(y=~sleep_perc, name = 'Sleep Diary', opacity=0.5, line=list(dash = 'dash'), mode = 'lines') %>% 
      plotly::add_trace(y=~video_perc, name = 'End of Day Videos', opacity=0.5, line=list(dash = 'dash', color = 'purple'), mode = 'lines') %>% 
      plotly::layout(title = paste0("Percentage Complete for EMA Tasks\nSubj ID: ", subj), xaxis = list(title = "Calendar Date", tickangle = 45, tickmode='linear'),
             yaxis = list(title = "% Complete"))
  # plotly is annoying with image export -> first convert to html
  htmlwidgets::saveWidget(completeness_figure, file = paste0(plots_path, '/', html_name))
  # convert html to png
  webshot::webshot(paste0(plots_path, '/', html_name), paste0(plots_path, '/', fig_name))
  # finally, delete the html file and the folder it creates
  file.remove(paste0(plots_path, '/', html_name))
  unlink(paste0(plots_path, '/', folder_name), recursive=TRUE)
  #plotly_IMAGE(fig_games_by_session, file = paste0(fig_name), width=5*96, height=5*96) # path, "/", 
  #png(paste0(plots_path, "/", fig_name), res=300, width=5, height=5, units="in")
  #print(fig_games_by_session)
  #dev.off()
})

# output the task completeness table to a csv
write_csv(completeness, paste0(dataPath, "/Subjects/", subj, "/reports/completeness.csv"))
saveRDS(completeness, paste0(dataPath, "/Subjects/", subj, "/reports/completeness.rds"))
