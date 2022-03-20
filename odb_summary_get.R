pkgload::load_all(".")

library(tidyverse)
library(sf)
library(leaflet)
library(rvest)
library(telegram.bot)
library(glue)
library(lubridate)

source("functions/functions.R")

if (file.exists("odb_last_pings.RDS")) {
  odb_last_pings <- read_rds("odb_last_pings.RDS")
} else {
  odb_last_pings <- odb_live[0,]
}

if (nrow(odb_last_pings) > 0) {
  site_summary <- odb_last_pings %>%
    summarise_site_pings() %>% 
    group_by(telegram_group_name, telegram_group_id) %>%
    summarise(summary_text = paste0(summary_text, collapse = '\n')) %>%
    walk2(
      .x = glue(
        "<b>Summary at {format(now(), '%H:%M')}</b>\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{.$summary_text}"
      ),
      .y = .$telegram_group_id,
      .f = ~ send_telegram(.x, .y)
    )
}