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
    filter(timestamp >= now() - minutes(10)) %>%
    mutate(
      on_xc = ifelse(
        nearest_site_distance > units::set_units(5000, metre) &
          ground_speed_kph > 2,
        1,
        0
      ),
      flying = ifelse(ground_speed_kph > 2, 1, 0)
    ) %>%
    group_by(telegram_group_name, telegram_group_id, nearest_site_name) %>%
    mutate(lat = mean(ifelse(on_xc == 0, lat, NA), na.rm = TRUE),
           long = mean(ifelse(on_xc == 0, long, NA), na.rm = TRUE)) %>%
    group_by(
      aircraft_type_name,
      telegram_group_name,
      telegram_group_id,
      nearest_site_name,
      lat,
      long
    ) %>%
    summarise(
      max_alt = ifelse(sum(flying) - sum(on_xc) > 0,
                       max(
                         ifelse(on_xc == 0 & flying == 1, alt_feet, NA), na.rm = TRUE
                       ), NA),
      avg_alt = ifelse(sum(flying) - sum(on_xc) > 0,
                       mean(
                         ifelse(on_xc == 0 & flying == 1, alt_feet, NA), na.rm = TRUE
                       ), NA),
      flying = sum(flying) - sum(on_xc),
      parawaiting = sum(ifelse(ground_speed_kph <= 2, 1, 0)),
      on_xc = sum(on_xc)
    ) %>%
    mutate(
      summary_text = glue(
        "{aircraft_type_name} {flying}|{parawaiting}|{on_xc}|{round(avg_alt, 0)}'|{round(max_alt, 0)}'"
      )
    ) %>%
    group_by(telegram_group_name,
             telegram_group_id,
             nearest_site_name,
             lat,
             long) %>%
    summarise(summary_text = paste0(summary_text, collapse = '\n')) %>%
    mutate(
      summary_text = glue(
        "<b>{nearest_site_name}</b>\n{summary_text}\nhttps://live.glidernet.org/#c={lat},{long}&z=13&m=4&s=1&w=0&n=0"
      )
    ) %>%
    group_by(telegram_group_name, telegram_group_id) %>%
    summarise(summary_text = paste0(summary_text, collapse = '\n')) %>%
    walk2(
      .x = glue(
        "Summary at {format(now(), '%H:%M')}\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{.$summary_text}"
      ),
      .y = .$telegram_group_id,
      .f = ~ send_telegram(.x, .y)
    )
}