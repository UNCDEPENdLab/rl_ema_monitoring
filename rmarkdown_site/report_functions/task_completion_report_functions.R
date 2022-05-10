
render_task_completion_table <- function(task_completion_data) {
  
  to_render <- task_completion_data %>% as.list()
  
  compliance_col_list <- function(data) {
    columns <- list(
      Date=colDef(style=list(fontWeight = "bold", minWidth=90))
    )

    col_groups <- list()

    for (uu in seq_along(uniq_types)) {
      uname <- uniq_types[uu]
      col_groups[[uu]] <- colGroup(name=uname, columns = grep(paste0("\\.", uname), names(data), value=TRUE))
    }

    return(list(cols=columns, groups=col_groups))
  }
  
  reactable::reactable(task_completion_data)
}

get_task_completion_data <- function(id, data_dir) {
  task_completion_data <- get_cleaned_data(id, data_dir, "completion")$all
  
  # task compliance-specific transformations applied to both checked and unchecked
  wrangle_task_completion <- function(df) {
    wide_dt <- df %>%   
      dplyr::rename(Date=dates, Sleep=sleep_perc, Mood=mood_perc, Rest=rest_perc,
                    Games=games_perc, Video=video_perc) %>%
      dplyr::mutate(
        Date=dashboard_date(Date),
      )
  }
  
  if (!is.null(task_completion_data)) {
    task_completion_data <- task_completion_data %>%
      wrangle_task_completion()
  } else {
    dashboard_warning("No task completion data found.")
  }
  
  return(task_completion_data)
}

