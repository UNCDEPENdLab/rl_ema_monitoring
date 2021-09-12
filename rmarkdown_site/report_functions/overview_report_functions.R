# generate reactable of overview data for display
render_overview_table <- function(overview_data, field=NULL) {
  stopifnot(field %in% names(overview_data))
  if (is.null(overview_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
  }
  
  to_render <- overview_data[[field]]

  #inspired by: https://glin.github.io/reactable/articles/cran-packages/cran-packages.html
  event_details <- function(index) {
    pkg <- pkgs[index, ]
    urls <- unlist(strsplit(gsub(",", " , ", pkg$URL, perl = TRUE), "[ \n]"))
    
    pkg_field <- function(name, ...) {
      if (any(is.na(...))) NULL
      else tagList(div(class = "detail-label", name), ...)
    }
    
    detail <- div(
      class = "package-detail",
      div(class = "detail-header", pkg$Package, span(class = "detail-title", pkg$Title)),
      div(class = "detail-description", pkg$Description),
      pkg_field("Version", pkg$Version),
      pkg_field("Depends", pkg$Depends),
      pkg_field("Imports", pkg$Imports),
      pkg_field("Suggests", pkg$Suggests),
      pkg_field("Author", pkg$Author),
      pkg_field("License", pkg$License),
      pkg_field("URL", lapply(urls, function(url) {
        if (grepl("https?://", url)) tags$a(href = url, url)
        else if (identical(url, ",")) ", "
        else url
      })),
      pkg_field("System Requirements", pkg$SystemRequirements)
    )
    
    if (length(versions[[pkg$Package]]) > 0) {
      archived <- pkg_field(
        "Archived Versions",
        reactable(
          versions[[pkg$Package]],
          pagination = FALSE,
          defaultColDef = colDef(headerClass = "header"),
          columns = list(
            Date = colDef(name = "Published", align = "right", width = 120, cell = function(value) {
              strftime(value, format = "%b %d, %Y")
            })
          ),
          fullWidth = FALSE,
          class = "archived-table",
          theme = reactableTheme(cellPadding = "8px 12px")
        )
      )
      detail <- tagAppendChild(detail, archived)
    }
    
    detail
  }
  
  #not clear why val_avg weas being used above to gray out all columns -- waiting on this
  tbl <- dashboard_reactable(
    data = to_render,
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      number_of_events=colDef(name="Number of events")
    ),
    defaultSorted="Date",
    details = event_details
  )
  
  tbl
}

get_overview_data <- function(id, data_dir) {
  overview_data <- get_cleaned_data(id, data_dir, "overview")
  
  # The overview.rds objects are stored with repeated rows for the overview ratings, where the repeats are
  #   the multiple events that are reported. Each event has distinct Good/Bad ratings. But for overview
  #   reports overall, we want to filter to just the first row of each event, then separate out event data
  overview_data$all_events <- overview_data$all %>%
    dplyr::select(
      Date, category, description, time_ago, Good_Bad, Physical_Pleasure_Physical_Pain,
      Loved_Lonely, Powerful_Weak, Safe_Threatened)
  
  # overview-specific transformations applied to both checked and unchecked
  wrangle_overview <- function(df) {
    #filter down to just the rows that vary by overview report, not event
    df %>%
      dplyr::select(Date, number_of_events, Valence, Arousal, 
                    Anxious, Elated, Sad, Irritable, Energetic
      ) %>%
      distinct() %>% # use this to drop the event-related row repeats
      mutate(
        Date=dashboard_date(Date),
      ) %>%
      arrange(desc(Date))
  }
  
  overview_data$all <- overview_data$all %>% wrangle_overview()
  
  if (!is.null(overview_data$unchecked)) {
    overview_data$unchecked_events <- overview_data$unchecked %>%
      dplyr::select(
        Date, category, description, time_ago, Good_Bad, Physical_Pleasure_Physical_Pain,
        Loved_Lonely, Powerful_Weak, Safe_Threatened)
    
    overview_data$unchecked <- overview_data$unchecked %>% wrangle_overview()
  }
    
  
  return(overview_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources
