###time series of game delay throughout experiment 
#split by morning and evening game sessions 
games_delay_am <- as_tibble(info) %>% 
  filter(type == 'Games AM') %>%
  select(delay, days, meridiem) %>% 
  mutate(days = as.integer(days)) %>%
  filter(days != 0) #%>%
  #filter(meridiem == 'AM')

games_delay_pm <- as_tibble(info) %>% 
  filter(type == 'Games PM') %>%
  select(delay, days, meridiem) %>% 
  mutate(days = as.integer(days)) %>%
  filter(days != 0) #%>%
  #filter(meridiem == 'PM')

session_length = max(max(games_delay_am$days), max(games_delay_pm$days))

#fill the missing game sessions with NAs so you can see missingness on the plot
#and ensures both am/pm sessions are the same length so you can group the bars 
games_delay_am <- games_delay_am %>% 
  complete(days = 1:session_length, fill = list(delay = NA))
games_delay_am$meridiem <- rep("AM", session_length) 
games_delay_am <- games_delay_am %>% mutate(meridiem = "AM")

games_delay_pm <- games_delay_pm %>% 
  complete(days = 1:session_length, fill = list(delay = NA))
games_delay_pm$meridiem <- rep("PM", session_length)
games_delay_pm <- games_delay_pm %>% mutate(meridiem = "PM")

try({
  html_name = "game_delay_plot.html"
  fig_name = "game_delay_plot.png"
  folder_name = "game_delay_plot_files"
  fig_games_by_session <- plot_ly(x = games_delay_am$days, y = games_delay_am$delay, type = 'bar', name = 'AM Session') %>% 
    add_trace(y = games_delay_pm$delay, name = 'PM Session') %>% 
    layout(title = "Minutes Late to Scheduled Game Sessions", 
           xaxis = list(title="Game Session Day", tick0=0, dtick=1),          
           yaxis = list(title="Minutes Late"))
  # plotly is annoying with image export -> first convert to html
  htmlwidgets::saveWidget(fig_games_by_session, file = paste0(plots_path, '/', html_name))
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