# generate reactable of eeg data for display
render_eeg_table <- function(eeg_data, field=NULL) {
  stopifnot(field %in% names(eeg_data))
  if (is.null(eeg_data[[field]])) {
    dashboard_message("Nothing to display!")
    return(invisible(NULL))
  }
  
  to_render <- eeg_data[[field]] %>%
    dplyr::select(Date, Block, avg, per_Ch_1, per_Ch_2, per_Ch_3, per_Ch_4)
  
  eeg_ch_fmt <- function(..., thresh=100) {
    colDef(
      html = TRUE,
      style=function(value) {
        if (value < thresh) {
          list(background = dds$eeg$bad_ch_colors$background, color=dds$eeg$bad_ch_colors$text) #flag bad cells
        } else {
          list(background = dds$eeg$good_ch_colors$background, color=dds$eeg$good_ch_colors$text)
        }
      },
      cell=function(value, index) {
        eeg_plot <- paste0(file.path(data_dir, "Subjects", id, "plots", paste0("eeg_plot_", to_render$Block[index], ".png")))
        htmltools::HTML(sprintf("<a href='%s' target='popup' onclick=\"window.open('%s','popup','width=%d,height=%d'); return false;\">%s</a>", 
                                eeg_plot, eeg_plot, dds$eeg$plot_window_width, dds$eeg$plot_window_height, value))
        # doesn't seem to pass onclick
        # htmltools::tags$a(target="popup", href=eeg_plot,
        #                   onclick=paste0("window.open('", eeg_plot, "','popup','width=900,height=900'); return false;"),
        #                   value)
      },
      ...
    )
  }
  
  tbl <- reactable(
    data = to_render,
    theme = journal(),
    defaultColDef = colDef(class = "cell", headerClass = "header"),
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      Block=colDef(style=list(fontWeight = "bold")),
      avg=eeg_ch_fmt(name="Overall %", thresh = dds$eeg$overall_bad_lt_x),
      per_Ch_1=eeg_ch_fmt(name="Ch. 1 % good", thresh = dds$eeg$ch_bad_lt_x),
      per_Ch_2=eeg_ch_fmt(name="Ch. 2 % good", thresh = dds$eeg$ch_bad_lt_x),
      per_Ch_3=eeg_ch_fmt(name="Ch. 3 % good", thresh = dds$eeg$ch_bad_lt_x),
      per_Ch_4=eeg_ch_fmt(name="Ch. 4 % good", thresh = dds$eeg$ch_bad_lt_x)
    ),
    borderless = TRUE,
    defaultSorted = c("Date", "Block"),
    defaultSortOrder = "desc",
    fullWidth = FALSE
  )
  
  tbl
}

get_eeg_data <- function(id, data_dir) {
  eeg_data <- get_cleaned_data(id, data_dir, "eeg")
  
  # eeg-specific transformations applied to both checked and unchecked
  wrangle_eeg <- function(df) {
    df %>%    
      mutate(
        Date=dashboard_date(Date),
      ) %>%
      arrange(desc(Date))
  }
  
  eeg_data$all <- eeg_data$all %>% wrangle_eeg()
  
  if (!is.null(eeg_data$unchecked)) {
    eeg_data$unchecked <- eeg_data$unchecked %>% wrangle_eeg()
  }
  
  return(eeg_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources


# kbl(eeg_unchecked) %>%
#   kable_styling(fixed_thead = T) %>%
#   column_spec(1, bold = T) %>%
#   column_spec(2, bold = T) %>%
#   column_spec(3, background=if_else(eeg_unchecked$`Overall % good`>80, "#C6EFCE", "#ffc7ce")) %>%
#   column_spec(4, color=if_else(eeg_unchecked[4]>70, "#D8D8D8", "black")) %>%
#   column_spec(5, color=if_else(eeg_unchecked[5]>70, "#D8D8D8", "black")) %>%
#   column_spec(6, color=if_else(eeg_unchecked[6]>70, "#D8D8D8", "black")) %>%
#   column_spec(7, color=if_else(eeg_unchecked[7]>70, "#D8D8D8", "black"))
