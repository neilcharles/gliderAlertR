library(tidyverse)
library(sf)
library(leaflet)
library(rvest)
library(telegram.bot)
library(glue)
library(lubridate)

message_limit <- 100
pg_takeoff_size <- 2000

if(month(now()) %in% c(10,11,12,1,2)){
  xc_milestone_interval <- 10
} else {
  xc_milestone_interval <- 25
}

source("functions/functions.R")

sites <- read_csv("sites_clean.csv") %>%
  st_as_sf(coords = c("takeoff_lon", "takeoff_lat"),
           crs = 4326)

odb_live <- read_ogn_live() %>% 
  filter(timestamp >= now() - minutes(5))

#----------- Load first pings --------------------------------------------------

if (file.exists("odb_first_pings.RDS")) {
  odb_first_pings <- read_rds("odb_first_pings.RDS")
} else {
  odb_first_pings <- odb_live[0, ]
}

#------------ tag live pings for probable paragliders --------------------------

odb_live_sf <- get_site_distances(odb_live, sites)

probable_paragliders <- odb_live_sf %>%
  dplyr::filter(
    nearest_site_distance <= units::set_units(pg_takeoff_size, metre)
    & ground_speed_kph <= 100
    & aircraft_type_name %in% c("Paraglider", "Hang glider", "Possible PG or HG", "", NA)
  )

#Filter live pings for new probable paragliders or already designated as
#paraglider based on first ping
odb_live <- odb_live %>%
  filter(
    registration2 %in% odb_first_pings$registration2 |
      registration2 %in% probable_paragliders$registration2
  )

#Add pg takeoff site
st_geometry(odb_live_sf) <-
  NULL  #Remove geo to allow standard left join

odb_live_sf <- odb_live_sf %>%
  select(registration2, nearest_site_name, nearest_site_distance)

odb_live <- odb_live %>%
  left_join(odb_live_sf, by = "registration2") %>% 
  left_join(
    select(sites, takeoff_name, telegram_group_name),
    by = c("nearest_site_name" = "takeoff_name")
  ) %>%
  left_join(telegram_groups(), by = "telegram_group_name")

#Rebuild the first ping table if it didn't exist (to ensure all cols included)
if (!file.exists("odb_first_pings.RDS")) {
  odb_first_pings <- odb_live[0, ]
}

#------------ update first pings -----------------------------------------------

odb_new_pings <-
  filter(odb_live,!registration2 %in% odb_first_pings$registration2)

odb_first_pings_updated <- odb_first_pings %>%
    union_all(odb_new_pings)

#------------ Load most recent pings--------------------------------------------

if (file.exists("odb_last_pings.RDS")) {
  odb_last_pings <- read_rds("odb_last_pings.RDS")
} else {
  odb_last_pings <- odb_live[0, ]
}

#------------ Calculate distances from first ping ------------------------------

xc_distances <-
  select(odb_first_pings_updated, registration2, long, lat)  %>%
  left_join(select(odb_last_pings, registration2, long, lat), by = "registration2") %>%
  rename(
    long_first = long.x,
    lat_first = lat.x,
    long_last = long.y,
    lat_last = lat.y
  ) %>%
  left_join(select(odb_live, registration2, long, lat), by = "registration2") %>%
  rename(long_live = long, lat_live = lat) %>%
  drop_na()

xc_distances$distance_last <-
  sf::st_as_sf(xc_distances,
               coords = c("long_first", "lat_first"),
               crs = 4326) %>%
  sf::st_distance(sf::st_as_sf(
    xc_distances,
    coords = c("long_last", "lat_last"),
    crs = 4326
  ), by_element = TRUE)

xc_distances$distance_live <-
  sf::st_as_sf(xc_distances,
               coords = c("long_first", "lat_first"),
               crs = 4326) %>%
  sf::st_distance(sf::st_as_sf(
    xc_distances,
    coords = c("long_live", "lat_live"),
    crs = 4326
  ), by_element = TRUE)

#distances to km
xc_distances <- xc_distances %>%
  mutate(
    distance_last = round(as.numeric(distance_last) / 1000, 0),
    distance_live = round(as.numeric(distance_live) / 1000, 0),
    xc_milestones_last = floor(distance_last / xc_milestone_interval) * xc_milestone_interval,
    xc_milestones_live = floor(distance_live / xc_milestone_interval) * xc_milestone_interval
  ) %>%
  filter(xc_milestones_live > xc_milestones_last)

site_summary <- odb_live %>%
  mutate(
    on_xc = ifelse(
      nearest_site_distance > units::set_units(5000, metre) &
        ground_speed_kph > 2,
      1,
      0
    ),
    flying = ifelse(ground_speed_kph > 2, 1, 0)
  ) %>%
  group_by(aircraft_type_name,
           telegram_group_name,
           telegram_group_id,
           nearest_site_name) %>%
  summarise(
    max_alt = ifelse(sum(flying) - sum(on_xc) > 0,
                     max(ifelse(
                       on_xc == 0, alt_feet, NA
                     ), na.rm = TRUE), NA),
    avg_alt = ifelse(sum(flying) - sum(on_xc) > 0,
                     mean(ifelse(
                       on_xc == 0, alt_feet, NA
                     ), na.rm = TRUE), NA),
    flying = sum(flying) - sum(on_xc),
    parawaiting = sum(ifelse(ground_speed_kph <= 2, 1, 0)),
    on_xc = sum(on_xc)
  ) %>% 
  mutate(summary_text = glue("<b>{nearest_site_name}</b>\n{aircraft_type_name} {flying}|{parawaiting}|{on_xc}|{round(avg_alt, 0)}'|{round(max_alt, 0)}'")) %>% 
  group_by(telegram_group_name, telegram_group_id) %>% 
  summarise(summary_text = paste0(summary_text, collapse = '\n'))

#bot$sendMessage(chat_id = 96373076, parse_mode = 'HTML', text = glue("Summary at {format(now(), '%H:%M')}\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{site_summary$summary_text[1]}"))

#------------ Send Telegram Messages -------------------------------------------

#Takeoff messages: started moving & gained altitude
# odb_live %>%
#   left_join(odb_last_pings, by = "registration2") %>%
#   filter(ground_speed_kph.y == 0 &
#            ((ground_speed_kph.x > 6 &
#            alt.x > alt.y + 30) |
#            (ground_speed_kph.x > 10))
#          ) %>%
#   top_n(message_limit, registration2) %>%
#   mutate(
#     telegram_message = glue(
#       "{aircraft_type_name.x} {registration_label.x} recently took off from {nearest_site_name.y}. Altitude {round(alt_feet.x,0)}' AMSL with ground speed {ground_speed_kph.x}kph https://live.glidernet.org/#c={lat.x},{long.x}&z=13&m=4&s=1&w=0&n=0"
#     )
#   ) %>%
#   filter(!is.na(telegram_group_id.x)) %>%
#   walk2(
#     .x = .$telegram_message,
#     .y = .$telegram_group_id.x,
#     .f = ~ send_telegram(.x, .y)
#   )
# 
#New trackers
if (nrow(odb_new_pings) > 0) {
  if (nrow(odb_new_pings) > message_limit) {
    odb_new_messages <- odb_new_pings[1:message_limit, ]
  } else {
    odb_new_messages <- odb_new_pings
  }

  odb_new_messages %>%
    filter(!nearest_site_name %in% odb_first_pings$nearest_site_name) %>%
    group_by(nearest_site_name) %>% 
    top_n(1, registration2) %>% 
    ungroup() %>% 
    mutate(
      telegram_message = glue(
        "{aircraft_type_name} {registration_label} is the first PG or HG for >1 hour seen at {nearest_site_name}. Altitude {round(alt_feet,0)}' AMSL with ground speed {ground_speed_kph}kph https://live.glidernet.org/#c={lat},{long}&z=13&m=4&s=1&w=0&n=0"
      )
    ) %>%
    filter(!is.na(telegram_group_id))
    # walk2(
    #   .x = .$telegram_message,
    #   .y = .$telegram_group_id,
    #   .f = ~ send_telegram(.x, .y)
    # )
}
# 
# #XC Distances
# xc_distances %>%
#   left_join(odb_live, by = "registration2") %>%
#   left_join(select(
#     odb_first_pings_updated,
#     registration2,
#     nearest_site_name,
#     telegram_group_id
#   ),
#   by = "registration2") %>%
#   top_n(message_limit, registration2) %>%
#   rowwise() %>%
#   mutate(location_name_live = geocode_location(lat = lat_live, long = long_live)) %>%
#   mutate(
#     telegram_message = glue(
#       "{aircraft_type_name} {registration_label} is on XC from {nearest_site_name.y}, passing {location_name_live} at {distance_live}km. Altitude {round(alt_feet,0)}' AMSL with ground speed {ground_speed_kph}kph https://live.glidernet.org/#c={lat},{long}&z=13&m=4&s=1&w=0&n=0"
#     )
#   ) %>%
#   filter(!is.na(telegram_group_id.y)) %>%
#   walk2(
#     .x = .$telegram_message,
#     .y = .$telegram_group_id.y,
#     .f = ~ send_telegram(.x, .y)
#   )

#------------ record most recent pings -----------------------------------------

odb_last_pings <-
  filter(odb_last_pings,!registration2 %in% odb_live$registration2) %>%
  union_all(odb_live)

#------------ Delete old records -----------------------------------------------

#Remove any device ID that hasn't been seen for an hour
old_records <- odb_last_pings %>%
  filter(timestamp <= min(odb_live$timestamp) - hms("01:00:00")) %>%
  pull(registration2)

odb_first_pings_updated <- odb_first_pings_updated %>%
  filter(!registration2 %in% old_records)

odb_last_pings <- odb_last_pings %>%
  filter(!registration2 %in% old_records)

#------------ save pings -------------------------------------------------------

write_rds(odb_first_pings_updated, "odb_first_pings.RDS")
write_rds(odb_last_pings, "odb_last_pings.RDS")
