# generate reactable of overview data for display
render_overview_table <- function(overview_data, field=NULL, add_links=FALSE) {
  checkmate::assert_list(overview_data)
  if (is.null(overview_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
  }
  
  to_render <- overview_data[[field]]

  id_fmt <- function(..., add_links=FALSE) {
    if (isTRUE(add_links)) {
      colDef(
        html=TRUE,
        style=list(fontWeight = "bold"),
        cell=function(value, index) {
          subj_plot <- file.path("Subjects", paste0(value, ".html"))
          htmltools::HTML(sprintf("<a href='%s'>%s</a>", subj_plot, value))
        }
      )
    } else {
      colDef(...)
    }
  } 
  
  #not clear why val_avg weas being used above to gray out all columns -- waiting on this
  tbl <- dashboard_reactable(
    data = to_render,
    defaultSorted="id",
    columns=list(
      id=id_fmt(add_links = add_links)
    ),
    theme=reactablefmtr::journal(font_size=12, header_font_size = 12),
    defaultColDef = colDef(minWidth = 70),
    sortable=FALSE
  )
  
  tbl
}

get_overview_data <- function(id, data_dir, quiet=FALSE) {
  overview_data <- get_cleaned_data(id, data_dir, "overview", quiet=quiet)
   
  #unclear if the all/unchecked distinction applies to overview -- seems like 'no'
  
  # overview-specific transformations applied to both checked and unchecked
  wrangle_overview <- function(df) {
    #filter down to just the rows that vary by overview report, not event
    df %>% dplyr::rename(id=ID) %>%
      #dplyr::mutate(across(starts_with("Avg"), round, digits=2))
      dplyr::mutate(across(where(is.numeric), round, digits=2)) %>%
      dplyr::select(`id`,
                    `Avg Obj Correct (no feedback)`, 
                    `Avg Obj Correct (w/ feedback)`, 
                    `Avg Rel Correct (no feedback)`, 
                    `Avg Rel Correct (w/ feedback)`,
                    `EEG Average`,
                    `HR Average`,
                    `Left %`,
                    `Valence/Arousal Distance from Origin`,
                    `Emotion Distance from 0`,
                    `Emotion/Valence Correlation`)
  }
  
  if (!is.null(overview_data$all)) {
    overview_data$all <- overview_data$all %>% wrangle_overview()
  } else {
    if (isFALSE(quiet)) { dashboard_warning("No overview data found. overview_data$all is NULL in get_overview_data.") }
    dashboard_debug("No overview data found. overview_data$all is NULL in get_overview_data.")
  }
  
  if (!is.null(overview_data$unchecked)) {
    overview_data$unchecked <- overview_data$unchecked %>% wrangle_overview()
  }
  
  return(overview_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources
