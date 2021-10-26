library(tidyverse)
library(sf)
library(leaflet)
library(rvest)
library(telegram.bot)
library(glue)
library(lubridate)

message_limit <- 3
pg_takeoff_size <- 1000

#setwd("C:/Users/neild/Documents/dev/glider_alertR")

send_telegram <- function(message = NULL){
  
  bot <- Bot(token = Sys.getenv('TELEGRAM_HILLTOP'))
  
  bot$sendMessage(chat_id = -1001798217889, text = message)   #chat with Neil 96373076
  
}

aircraft_codes <- read_rds("aircraft_codes.RDS")

odb_live <- xml2::read_xml("https://live.glidernet.org/lxml.php?a=0&b=59.6&c=49.8&d=2&e=-11") %>% 
  html_nodes("m") %>% 
  html_attr("a") %>% 
  as_tibble() %>% 
  separate(value, as.character(c(1:14)), sep = ",")

names(odb_live) <- c("lat", "long", "cn", "registration", "alt", "timestamp", "X1",
                         "track_deg", "ground_speed_kph", "vertical_speed_ms",
                         "aircraft_type_code", "receiver", "device_id", "registration2")

odb_live <- odb_live %>% 
  mutate(across(c("alt", "X1", "track_deg", "ground_speed_kph", "vertical_speed_ms", "lat", "long"), as.numeric),
         timestamp = today() + hms(timestamp),
         alt_feet = alt * 3.28) %>% 
  left_join(aircraft_codes, by = "aircraft_type_code")

#----------- Load first pings --------------------------------------------------

if(file.exists("odb_first_pings.RDS")){
  odb_first_pings <- read_rds("odb_first_pings.RDS")
} else {
  odb_first_pings <- odb_live[0,]
}


#------------ tag live pings for probable paragliders --------------------------

#Ping is in range of a paragliding takeoff
sites <- read_rds("sites_list.RDS")

sites_sf <- sites %>% st_as_sf(coords = c("takeoff_lon", "takeoff_lat"), crs = 4326)

odb_live_sf <- odb_live %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

nearest_site_index <- odb_live_sf %>% 
  st_nearest_feature(sites_sf)

odb_live_sf$nearest_site_name <- sites_sf$takeoff_name[nearest_site_index]

odb_live_sf$nearest_site_distance <- odb_live_sf %>% 
  st_distance(sites_sf[nearest_site_index,], by_element = TRUE)

probable_paragliders <- odb_live_sf %>% 
  filter(nearest_site_distance <= units::set_units(pg_takeoff_size, metre) &
         ground_speed_kph <= 100)

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


#------------ Send Telegram Messages -------------------------------------------

#Takeoff messages started moving & gained altitude
odb_live %>% 
  left_join(odb_last_pings, by = "registration2") %>% 
  filter(ground_speed_kph.y==0 &
         ground_speed_kph.x > 20 &
         alt.x > alt.y + 30) %>% 
  top_n(message_limit, registration2) %>% 
  mutate(telegram_message_glidernet = glue("Aircraft {registration.x} recently took off and is currently at {round(alt_feet.x,0)}' AMSL doing {ground_speed_kph.y}kph https://live.glidernet.org/#c={lat.y},{long.y}&z=13&s=1&w=0&p=2"),
         telegram_message = glue("{aircraft_type_name.x} '{registration.x}' recently took off from {nearest_site_name.y}. Altitude {round(alt_feet.x,0)}' AMSL with ground speed {ground_speed_kph.x}kph https://www.gliderradar.com/center/{lat.x},{long.x}/zoom/13/time/15")
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
    mutate(telegram_message = glue("{aircraft_type_name} '{registration}' first location ping received at {nearest_site_name}. Altitude {round(alt_feet,0)}' AMSL with ground speed {ground_speed_kph}kph https://www.gliderradar.com/center/{lat},{long}/zoom/13/time/15")
    ) %>% 
    pull(telegram_message) %>% 
    walk(.f = ~send_telegram(.x))
}

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
