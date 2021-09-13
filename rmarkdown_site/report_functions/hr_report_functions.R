# generate reactable of hr data for display
render_hr_table <- function(hr_data, field=NULL) {
  stopifnot(field %in% names(hr_data))
  if (is.null(hr_data[[field]])) {
    return(dashboard_message("Nothing to display!"))
  }
  
  to_render <- hr_data[[field]] %>%
    dplyr::select(Date, Block, per_Good)

  tbl <- dashboard_reactable(
    data = to_render,
    columns=list(
      Date=colDef(style=list(fontWeight = "bold")),
      Block=colDef(html=TRUE, style=list(fontWeight = "bold"), cell=function(value, index) {
        ecg_plot <- paste0(file.path(data_dir, "Subjects", id, "plots", paste0("ecg_plot_", to_render$Block[index], ".png")))
        htmltools::HTML(sprintf("<a href='%s' target='popup' onclick=\"window.open('%s','popup','width=%d,height=%d'); return false;\">%s</a>", 
                                ecg_plot, ecg_plot, dds$hr$plot_window_width, dds$hr$plot_window_height, value))
      }),
      per_Good=colDef(name="Good signal %", html=TRUE, style=function(value) {
        list(background=ifelse(value < dds$hr$hr_bad_lt_x, dds$hr$bad_hr_colors$background, dds$hr$good_hr_colors$background))
      }, cell=function(value, index) {
        ecg_plot <- paste0(file.path(data_dir, "Subjects", id, "plots", paste0("ecg_plot_", to_render$Block[index], ".png")))
        htmltools::HTML(sprintf("<a href='%s' target='popup' onclick=\"window.open('%s','popup','width=%d,height=%d'); return false;\">%s</a>", 
                                ecg_plot, ecg_plot, dds$hr$plot_window_width, dds$hr$plot_window_height, value))
      })
    )
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
  
  if (!is.null(hr_data$all)) {
    hr_data$all <- hr_data$all %>% wrangle_hr()
  } else {
    dashboard_warning("No HR data found. hr_data$all is NULL in get_hr_data.")
  }
  
  if (!is.null(hr_data$unchecked)) {
    hr_data$unchecked <- hr_data$unchecked %>% wrangle_hr()
  }
  
  return(hr_data)
}



####### LEFTOVERS FROM MASTER_GENERATOR.RMD and other sources
# 
# \newline
# ### HR {.tabset}
# #### Unchecked
# ```{r unch hr, echo=FALSE, warning=FALSE}
# #HR table
# if(nrow(hr_unchecked) > 0){
#   kbl(hr_unchecked) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, bold = T) %>%
#     column_spec(3, background=if_else(hr_unchecked[3]>90, "#C6EFCE", "#ffc7ce"))
# }
# ```
# 
# #### All blocks
# ```{r hr table, echo=FALSE, warning=FALSE}
# if(nrow(hr_ordered) > 0){
#   kbl(hr_ordered) %>%
#     kable_styling(fixed_thead = T) %>%
#     column_spec(1, bold = T) %>%
#     column_spec(2, bold = T) %>%
#     column_spec(3, background=if_else(hr_ordered[3]>90, "#C6EFCE", "#ffc7ce"))
# }
# ```

#HR table wrangling
# hr <- rename(hr,"Block"=block)
# 
# hr_dates <- left_join(hr,blk_dt,by="Block") #add date column
# 
# hr_ordered <- arrange(hr_dates, -row_number()) #most recent blocks first
# 
# #convert all to percentages, rounded
# hr_ordered$per_Good <- hr_ordered$per_Good*100
# hr_ordered$per_Good <- round(hr_ordered$per_Good)
# 
# hr_unchecked <- filter(hr_ordered, checklist=="No")
# hr_ordered <- transmute(hr_ordered, "Date"=Date, "Block"=Block, "Good signal %"=per_Good) 
# hr_unchecked <- transmute(hr_unchecked, "Date"=Date, "Block"=Block, "Good signal %"=per_Good) 