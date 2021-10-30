library(tidyverse)
library(sf)
library(leaflet)
library(rvest)
library(telegram.bot)
library(glue)
library(lubridate)

message_limit <- 3
pg_takeoff_size <- 1000
xc_milestone_interval <- 10

source("functions/functions.R")

odb_live <- read_ogn_live()

#----------- Load first pings --------------------------------------------------

if(file.exists("odb_first_pings.RDS")){
  odb_first_pings <- read_rds("odb_first_pings.RDS")
} else {
  odb_first_pings <- odb_live[0,]
}

#------------ tag live pings for probable paragliders --------------------------

sites <- read_rds("sites_list.RDS")

odb_live_sf <- get_site_distances(odb_live, sites)

probable_paragliders <- odb_live_sf %>% 
  dplyr::filter(nearest_site_distance <= units::set_units(pg_takeoff_size, metre)
                & ground_speed_kph <= 100
                # & aircraft_type_name %in% c("Paraglider", "Hang glider", "Unknown aircraft type", "", NA)
                )

#Filter live pings for new probable paragliders or already designated as
#paraglider based on first ping
odb_live <- odb_live %>% 
  filter(registration2 %in% odb_first_pings$registration2 |
           registration2 %in% probable_paragliders$registration2)

#Add pg takeoff site
st_geometry(odb_live_sf) <- NULL  #Remove geo to allow standard left join

odb_live_sf <- odb_live_sf %>% 
  select(registration2, nearest_site_name, nearest_site_distance)

odb_live <- odb_live %>% 
  left_join(odb_live_sf, by = "registration2")


#------------ update first pings -----------------------------------------------

odb_new_pings <- filter(odb_live, !registration2 %in% odb_first_pings$registration2)

odb_first_pings <- odb_first_pings %>% 
  union_all(odb_new_pings)


#------------ Load most recent pings--------------------------------------------

if(file.exists("odb_last_pings.RDS")){
  odb_last_pings <- read_rds("odb_last_pings.RDS")
} else {
  odb_last_pings <- odb_live[0,]
}

#------------ Calculate distances from first ping ------------------------------

xc_distances <- select(odb_first_pings, registration2, long, lat)  %>% 
  left_join(select(odb_last_pings, registration2, long, lat), by = "registration2") %>% 
  rename(long_first = long.x, lat_first = lat.x, long_last = long.y, lat_last = lat.y) %>% 
  left_join(select(odb_live, registration2, long, lat), by = "registration2") %>%
  rename(long_live = long, lat_live = lat) %>% 
  drop_na()

xc_distances$distance_last <- sf::st_as_sf(xc_distances, coords = c("long_first", "lat_first"), crs = 4326) %>% 
  sf::st_distance(sf::st_as_sf(xc_distances, coords = c("long_last", "lat_last"), crs = 4326), by_element = TRUE)

xc_distances$distance_live <- sf::st_as_sf(xc_distances, coords = c("long_first", "lat_first"), crs = 4326) %>% 
  sf::st_distance(sf::st_as_sf(xc_distances, coords = c("long_live", "lat_live"), crs = 4326), by_element = TRUE)

#distances to km
xc_distances <- xc_distances %>% 
  mutate(distance_last = round(as.numeric(distance_last)/1000,0),
         distance_live = round(as.numeric(distance_live)/1000,0),
         xc_milestones_last = floor(distance_last / xc_milestone_interval) * xc_milestone_interval,
         xc_milestones_live = floor(distance_live / xc_milestone_interval) * xc_milestone_interval) %>% 
  filter(xc_milestones_live > xc_milestones_last)

#------------ Send Telegram Messages -------------------------------------------

#Takeoff messages started moving & gained altitude
odb_live %>% 
  left_join(odb_last_pings, by = "registration2") %>% 
  filter(ground_speed_kph.y==0 &
           ground_speed_kph.x > 20 &
           alt.x > alt.y + 30) %>% 
  top_n(message_limit, registration2) %>% 
  mutate(telegram_message_glidernet = glue("Aircraft {registration.x} recently took off and is currently at {round(alt_feet.x,0)}' AMSL doing {ground_speed_kph.y}kph https://live.glidernet.org/#c={lat.y},{long.y}&z=13&s=1&w=0&p=2"),
         telegram_message = glue("{aircraft_type_name.x} '{registration.x}' recently took off from {nearest_site_name.y}. Altitude {round(alt_feet.x,0)}' AMSL with ground speed {ground_speed_kph.x}kph https://www.gliderradar.com/center/{lat.x},{long.x}/zoom/13/time/15/maptype/terrain")
  ) %>% 
  pull(telegram_message) %>% 
  walk(.f = ~send_telegram(.x))

#New trackers
if(nrow(odb_new_pings)>0){
  
  if(nrow(odb_new_pings) > message_limit){
    odb_new_messages <- odb_new_pings[1:message_limit,]
  } else {
    odb_new_messages <- odb_new_pings
  }
  
  odb_new_messages %>% 
    mutate(telegram_message = glue("{aircraft_type_name} '{registration}' first location ping received at {nearest_site_name}. Altitude {round(alt_feet,0)}' AMSL with ground speed {ground_speed_kph}kph https://www.gliderradar.com/center/{lat},{long}/zoom/13/time/15/maptype/terrain")
    ) %>% 
    pull(telegram_message) %>% 
    walk(.f = ~send_telegram(.x))
}

#XC Distances
xc_distances %>% 
  left_join(odb_live, by = "registration2") %>% 
  left_join(select(odb_first_pings, registration2, nearest_site_name), by = "registration2") %>% 
  top_n(message_limit, registration2) %>% 
  rowwise() %>% 
  mutate(location_name_live = geocode_location(lat = lat_live, long = long_live)) %>% 
  mutate(telegram_message = glue("{aircraft_type_name} '{registration}' is on XC from {nearest_site_name.y}, passing {location_name_live} at {distance_live}km. Altitude {round(alt_feet,0)}' AMSL with ground speed {ground_speed_kph}kph https://www.gliderradar.com/center/{lat},{long}/zoom/13/time/15/maptype/terrain")) %>% 
  pull(telegram_message) %>% 
  walk(.f = ~send_telegram(.x))

#------------ record most recent pings -----------------------------------------

odb_last_pings <- filter(odb_last_pings, !registration2 %in% odb_live$registration2) %>% 
  union_all(odb_live)

#------------ Delete old records -----------------------------------------------

#Remove any device ID that hasn't been seen for an hour
old_records <- odb_last_pings %>% 
  filter(timestamp <= min(odb_live$timestamp) -hms("01:00:00")) %>% 
  pull(registration2)

odb_first_pings <- odb_first_pings %>% 
  filter(!registration2 %in% old_records)

odb_last_pings <- odb_last_pings %>% 
  filter(!registration2 %in% old_records)

#------------ save pings -------------------------------------------------------

write_rds(odb_first_pings, "odb_first_pings.RDS")
write_rds(odb_last_pings, "odb_last_pings.RDS")
