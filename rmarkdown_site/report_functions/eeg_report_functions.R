# generate reactable of eeg data for display
render_eeg_table <- function(eeg_data, field=NULL) {
  stopifnot(field %in% names(eeg_data))
  if (is.null(eeg_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
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
  
  tbl <- dashboard_reactable(
    data = to_render,
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      Block=colDef(style=list(fontWeight = "bold")),
      avg=eeg_ch_fmt(name="Overall %", thresh = dds$eeg$overall_bad_lt_x),
      per_Ch_1=eeg_ch_fmt(name="Ch. 1 % good", thresh = dds$eeg$ch_bad_lt_x),
      per_Ch_2=eeg_ch_fmt(name="Ch. 2 % good", thresh = dds$eeg$ch_bad_lt_x),
      per_Ch_3=eeg_ch_fmt(name="Ch. 3 % good", thresh = dds$eeg$ch_bad_lt_x),
      per_Ch_4=eeg_ch_fmt(name="Ch. 4 % good", thresh = dds$eeg$ch_bad_lt_x)
    )
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
# 
# ```{r eeg, include=FALSE}
# #EEG table wrangling
# 
# eeg <- rename(eeg,"Block"=block)
# 
# eeg_dates <- left_join(eeg,blk_dt,by="Block") #add date and checklist column
# 
# eeg_avg <- mutate(eeg_dates,"avg"=(per_Ch_1 + per_Ch_2 + per_Ch_3 +per_Ch_4)/4) #create column for avg EEG signal 
# 
# eeg_row_ordered <- arrange(eeg_avg, -row_number()) #flip df so most recent blocks are first
# 
# #convert all to percentages, rounded
# eeg_row_ordered$per_Ch_1 <- eeg_row_ordered$per_Ch_1*100
# eeg_row_ordered$per_Ch_1 <- round(eeg_row_ordered$per_Ch_1)
# eeg_row_ordered$per_Ch_2 <- eeg_row_ordered$per_Ch_2*100 
# eeg_row_ordered$per_Ch_2 <- round(eeg_row_ordered$per_Ch_2)
# eeg_row_ordered$per_Ch_3 <- eeg_row_ordered$per_Ch_3*100 
# eeg_row_ordered$per_Ch_3 <- round(eeg_row_ordered$per_Ch_3)
# eeg_row_ordered$per_Ch_4 <- eeg_row_ordered$per_Ch_4*100
# eeg_row_ordered$per_Ch_4 <- round(eeg_row_ordered$per_Ch_4)
# eeg_row_ordered$avg <- eeg_row_ordered$avg*100
# eeg_row_ordered$avg <- round(eeg_row_ordered$avg)
# 
# eeg_col_named <- transmute(eeg_row_ordered, "Date"=Date, "Block"=Block, "Overall % good"=avg, "Ch. 1 % good"=per_Ch_1, "Ch. 2 % good"=per_Ch_2, "Ch. 3 % good"=per_Ch_3, "Ch. 4 % good"=per_Ch_4) 
# eeg_unchecked <- filter(eeg_row_ordered, checklist=="No")
# eeg_unchecked <- transmute(eeg_unchecked, "Date"=Date, "Block"=Block, "Overall % good"=avg, "Ch. 1 % good"=per_Ch_1, "Ch. 2 % good"=per_Ch_2, "Ch. 3 % good"=per_Ch_3, "Ch. 4 % good"=per_Ch_4) 
# ```
