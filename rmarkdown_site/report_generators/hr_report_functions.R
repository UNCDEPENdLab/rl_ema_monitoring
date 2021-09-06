# generate reactable of hr data for display
render_hr_table <- function(hr_data, field=NULL) {
  stopifnot(field %in% names(hr_data))
  if (is.null(hr_data[[field]])) {
    dashboard_message("Nothing to display!")
    return(invisible(NULL))
  }
  
  to_render <- hr_data[[field]] %>%
    dplyr::select(Date, Block, per_Good)

  tbl <- reactable(
    data = to_render,
    theme = journal(),
    defaultColDef = colDef(class = "cell", headerClass = "header"),
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      Block=colDef(style=list(fontWeight = "bold")),
      per_Good=colDef(name="Good signal %", style=function(value) list(background=ifelse(value > 90, "#C6EFCE", "#ffc7ce")))
    ),
    borderless = TRUE,
    defaultSorted = c("Date", "Block"),
    defaultSortOrder = "desc",
    fullWidth = FALSE
  )
  
  tbl
}

get_hr_data <- function(id, data_dir) {
  hr_data <- get_cleaned_data(id, data_dir, "hr")
  
  # hr-specific transformations applied to both checked and unchecked
  wrangle_hr <- function(df) {
    df %>%    
      mutate(
        Date=dashboard_date(Date),
      ) %>%
      arrange(desc(Date))
  }
  
  hr_data$all <- hr_data$all %>% wrangle_hr()
  
  if (!is.null(hr_data$unchecked)) {
    hr_data$unchecked <- hr_data$unchecked %>% wrangle_hr()
  }
  
  return(hr_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources
