
render_payment_table <- function(payment_data) {
  
  to_render <- payment_data %>% as.list()
  
  reactable::reactable(payment_data)
}

get_payment_data <- function(id, data_dir) {
  payment_data <- get_cleaned_data(id, data_dir, "payment")$all
  
  # task compliance-specific transformations applied to both checked and unchecked
  wrangle_payment <- function(df) {
    wide_dt <- df %>%   
      dplyr::rename(`Calendar Week`=cal_week, `Payment Date`=payment_date, `Game Earnings`=game_earnings,
                    `Games Completed(%)`=games_perc_completed, `Mood Reports(%)`=mood_perc_completed,
                    `Sleep Report(%)`=sleep_perc_completed, `Videos Completed(%)`=video_perc_completed) %>%
      dplyr::mutate(
        `Payment Date`=dashboard_date(`Payment Date`),
      )
  }
  
  if (!is.null(payment_data)) {
    payment_data <- payment_data %>%
      wrangle_payment()
  } else {
    dashboard_warning("No task payment data found.")
  }
  
  return(payment_data)
}

t <- get_payment_data(id, data_dir)
render_payment_table(t)
