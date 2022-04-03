#' Send Summary Alerts
#'
#' @return
#' @export
#'
#' @examples
#' #odb_summary_get()
odb_summary_get <- function(){
  
  if (file.exists("odb_last_pings.RDS")) {
    odb_last_pings <- readr::read_rds("odb_last_pings.RDS")
  } else {
    return(NULL)
  }
  
  if (nrow(odb_last_pings) > 0) {
    site_summary <- odb_last_pings %>%
      summarise_site_pings() %>% 
      group_by(telegram_group_name, telegram_group_id) %>%
      summarise(summary_text = paste0(summary_text, collapse = '\n')) %>%
      walk2(
        .x = glue(
          "<b>Summary at {format(lubridate::with_tz(lubridate::now(), tzone = 'Europe/London'), '%H:%M')}</b>\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{.$summary_text}"
        ),
        .y = .$telegram_group_id,
        .f = ~ send_telegram(.x, .y)
      )
  }
}