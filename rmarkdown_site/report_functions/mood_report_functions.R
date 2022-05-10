# generate reactable of mood data for display
render_mood_table <- function(mood_data, field=NULL) {
  checkmate::assert_list(mood_data)
  if (is.null(mood_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
  }
  
  to_render <- mood_data[[field]]
  event_data <- mood_data[[paste0(field, "_events")]]

  #inspired by: https://glin.github.io/reactable/articles/cran-packages/cran-packages.html
  event_details <- function(index) {
    episode_match <- to_render$episode_number[index]
    
    # compare against events
    evt_match <- event_data %>% dplyr::filter(episode_number == !!episode_match)
    
    detail <- htmltools::div(
      class = "mood-detail",
      reactable(
        data = evt_match %>% dplyr::select(-Date, -episode_number),
        columns=list(
          category = colDef(name="Category", width=125),
          description = colDef(name="Description", width=225),
          time_ago = colDef(name="Hours. ago"),
          Good_Bad = colDef(name="Good-Bad"),
          Physical_Pleasure_Physical_Pain = colDef(name="Pleasure-Pain"),
          Loved_Lonely = colDef(name="Loved-Lonely"),
          Powerful_Weak = colDef(name="Powerful-Weak"),
          Safe_Threatened = colDef(name="Safe-Threatened")  
        ),
        fullWidth=TRUE,
        defaultColDef = colDef(minWidth = 50),
        outlined=TRUE)
    )
    
    detail
  }
  
  #"Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events,
  # mood_unchecked <- transmute(mood_unchecked, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)
  
  #not clear why val_avg weas being used above to gray out all columns -- waiting on this
  tbl <- dashboard_reactable(
    data = to_render %>% dplyr::select(-episode_number),
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      number_of_events=colDef(name="Number of events")
    ),
    defaultSorted="Date",
    details = event_details
  )
  
  tbl
}


get_mood_data <- function(id, data_dir) {
  mood_data <- get_cleaned_data(id, data_dir, "mood")
  
  # generate unique episode number to ensure that reports with multiple events are grouped properly
  # grouping by date alone doesn't work because a date may have many events in separate episodes (prompts)
  encode_episode <- function(df) {
    if (is.null(df)) { return(NULL) }
    
    #cur_group_id accurately groups identical records within day based on 4 affects, but does not follow row order
    bb <- df %>% group_by(Date, Valence, Arousal, Anxious, Elated) %>%
      mutate(idx = cur_group_id()) %>% ungroup() 
    
    # look at repeats of each idx, then reorder from highest to lowest
    # this assumes that the row order in the original file was sorted in some finer way (e.g., time)
    rr <- rle(bb$idx)
    bb$episode_number <- rep(max(bb$idx):min(bb$idx), times = rr$lengths)
    bb$idx <- NULL
    
    return(bb)
  }
  
  mood_data$all <- encode_episode(mood_data$all)
  mood_data$unchecked <- encode_episode(mood_data$unchecked)
  
  # mood-specific transformations applied to both checked and unchecked
  wrangle_mood <- function(df) {
    #filter down to just the rows that vary by mood report, not event
    df %>%
      dplyr::select(Date, number_of_events, Valence, Arousal, 
                    Anxious, Elated, Sad, Irritable, Energetic, episode_number
      ) %>%
      distinct() %>% # use this to drop the event-related row repeats
      mutate(Date=dashboard_date(Date)) %>%
      arrange(desc(Date))
  }
  
  wrangle_events <- function(df) {
    df %>% dplyr::select(
      Date, category, description, time_ago, Good_Bad, Physical_Pleasure_Physical_Pain,
      Loved_Lonely, Powerful_Weak, Safe_Threatened, episode_number
    ) %>%
      mutate(Date=dashboard_date(Date))
  }
  
  if (!is.null(mood_data$all)) {
    # The mood.rds objects are stored with repeated rows for the mood ratings, where the repeats are
    #   the multiple events that are reported. Each event has distinct Good/Bad ratings. But for mood
    #   reports overall, we want to filter to just the first row of each event, then separate out event data
    mood_data$all_events <- mood_data$all %>% wrangle_events()
    mood_data$all <- mood_data$all %>% wrangle_mood()
  } else {
    dashboard_warning("No mood data found. mood_data$all is NULL in get_mood_data.")
  }
  
  if (!is.null(mood_data$unchecked)) {
    mood_data$unchecked_events <- mood_data$unchecked %>% wrangle_events()
    mood_data$unchecked <- mood_data$unchecked %>% wrangle_mood()
  }
  
  return(mood_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources

#Mood report table wrangling
# mood <- proc_sched$form_dfs$`Mood Questionnaire`
# mood$answer_time <- str_extract(mood$answer_time,"\\d{4}-\\d{2}-\\d{2}") #chop off the hour from the answer time
# mood <- rename(mood,"Date"=answer_time)
# mood$Date <- as.character(mood$Date)
# mood <- left_join(mood,checklist, by="Date")
# mood$Date <- lapply(mood$Date,date_format) #format appropriately
# 
# mood_check <- output$redcap %>% filter(ID==params$id) %>% select(`Mood`,`Mood Notes`,Date)
# mood_check$Date <- as.character(mood_check$Date)
# mood$Date <- as.character(mood$Date)
# 
# mood <- left_join(mood, mood_check, by="Date")#adding RA check column
# mood <- arrange(mood, -row_number()) #make the most recent block come first
# 
# #this will be used to determine the color of the font of the mood ratings
# val_avg_row <- filter(output$sample_form_summary, ID==params$id)
# val_avg <- sl_di_avg_row$val_arr_dis_avg
# 
# mood_unchecked <- filter(mood,`Checklist Complete?`=="No")
# mood_unchecked <- transmute(mood_unchecked, "Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)
# mood <- transmute(mood, "Date"=Date, "Problems"=Mood,"Notes"=`Mood Notes`,"Number of events"=number_of_events, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)


# \newline
# ### Mood questionnaire {.tabset}
# #### Unchecked
# ```{r mood unchecked, echo=FALSE, warning=FALSE}
# #Mood table
# #Will need to change the figure for val_arr_dis_avg when we figure out that threshold
# if(nrow(mood_unchecked) > 0){
#   kbl(mood_unchecked) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, background=if_else(is.na(mood_unchecked[2]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(3, background=if_else(is.na(mood_unchecked[3]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(4, color="#D8D8D8") %>%
#     column_spec(5, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(6, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(7, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(8, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(9, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(10, color=if_else(val_avg > 50, "#D8D8D8", "black"))
# }
# ```
# 
# #### All blocks
# ```{r mood table, echo=FALSE, warning=FALSE}
# #Mood table
# #Will need to change the figure for val_arr_dis_avg when we figure out that threshold
# if(nrow(mood) > 0){
#   kbl(mood) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, background=if_else(is.na(mood[2]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(3, background=if_else(is.na(mood[3]), "#C6EFCE", "#ffc7ce")) %>%
#     column_spec(4, color="#D8D8D8") %>%
#     column_spec(5, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(6, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(7, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(8, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(9, color=if_else(val_avg > 50, "#D8D8D8", "black")) %>%
#     column_spec(10, color=if_else(val_avg > 50, "#D8D8D8", "black"))
# }
# ```

#inspired by: https://glin.github.io/reactable/articles/cran-packages/cran-packages.html
# pkg <- pkgs[index, ]
# urls <- unlist(strsplit(gsub(",", " , ", pkg$URL, perl = TRUE), "[ \n]"))
# 
# pkg_field <- function(name, ...) {
#   if (any(is.na(...))) NULL
#   else tagList(div(class = "detail-label", name), ...)
# }
# 
# detail <- div(
#   class = "package-detail",
#   div(class = "detail-header", pkg$Package, span(class = "detail-title", pkg$Title)),
#   div(class = "detail-description", pkg$Description),
#   pkg_field("Version", pkg$Version),
#   pkg_field("Depends", pkg$Depends),
#   pkg_field("Imports", pkg$Imports),
#   pkg_field("Suggests", pkg$Suggests),
#   pkg_field("Author", pkg$Author),
#   pkg_field("License", pkg$License),
#   pkg_field("URL", lapply(urls, function(url) {
#     if (grepl("https?://", url)) tags$a(href = url, url)
#     else if (identical(url, ",")) ", "
#     else url
#   })),
#   pkg_field("System Requirements", pkg$SystemRequirements)
# )
# 
# if (length(versions[[pkg$Package]]) > 0) {
#   archived <- pkg_field(
#     "Archived Versions",
#     reactable(
#       versions[[pkg$Package]],
#       pagination = FALSE,
#       defaultColDef = colDef(headerClass = "header"),
#       columns = list(
#         Date = colDef(name = "Published", align = "right", width = 120, cell = function(value) {
#           strftime(value, format = "%b %d, %Y")
#         })
#       ),
#       fullWidth = FALSE,
#       class = "archived-table",
#       theme = reactableTheme(cellPadding = "8px 12px")
#     )
#   )
#   detail <- tagAppendChild(detail, archived)
# }