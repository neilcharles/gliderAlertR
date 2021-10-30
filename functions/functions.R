send_telegram <- function(message = NULL, chat_id = -1001798217889){
  
  bot <- telegram.bot::Bot(token = Sys.getenv('TELEGRAM_HILLTOP'))
  
  bot$sendMessage(chat_id = chat_id, text = message)   #chat with Neil 96373076
  
}

telegram_groups <- function(){
  tibble(group_id = c(-1001691078874,
                      -1001545184005,
                      -1001798217889,
                      -1001768573848),
         group_name = c("Dales",
                        "Derbyshire",
                        "Pennine",
                        "Southern"))
  
}

aircraft_codes <- function(){
  tibble(
    aircraft_type_code = c(
      '0',
      '1',
      '2',
      '3',
      '4',
      '5',
      '6',
      '7',
      '8',
      '9',
      'A',
      'B',
      'C',
      'D',
      'E',
      'F'
    ),
    aircraft_type_name = c(
      'Unknown aircraft type',
      'Sailplane',  #Glider/motor-glider
      'Tow plane',
      'Helicopter',
      'Parachute',
      'Drop plane',
      'Hang glider',
      'Paraglider',
      'Powered aircraft',
      'Jet aircraft',
      'UFO',
      'Balloon',
      'Airship',
      'UAV',
      'Ground support',
      'Static object'
    )
  )
}

read_ogn_live <- function(){
  odb_live <- xml2::read_xml("https://live.glidernet.org/lxml.php?a=0&b=59.6&c=49.8&d=2&e=-11") %>% 
    rvest::html_nodes("m") %>% 
    rvest::html_attr("a") %>% 
    tibble::as_tibble() %>% 
    tidyr::separate(value, as.character(c(1:14)), sep = ",")
  
  names(odb_live) <- c("lat", "long", "cn", "registration", "alt", "timestamp", "X1",
                       "track_deg", "ground_speed_kph", "vertical_speed_ms",
                       "aircraft_type_code", "receiver", "device_id", "registration2")
  
  odb_live <- odb_live %>% 
    dplyr::mutate(across(c("alt", "X1", "track_deg", "ground_speed_kph", "vertical_speed_ms", "lat", "long"), as.numeric),
                  timestamp = lubridate::today() + lubridate::hms(timestamp),
                  alt_feet = alt * 3.28) %>% 
    dplyr::left_join(aircraft_codes(), by = "aircraft_type_code") %>% 
    tidyr::replace_na(list(aircraft_type_name = 'Unknown aircraft type'))
  
  odb_live
}

get_site_distances <- function(odb_live = NULL, sites = NULL){
  
  #Ping is in range of a paragliding takeoff
  sites_sf <- sites %>% sf::st_as_sf(coords = c("takeoff_lon", "takeoff_lat"), crs = 4326)
  
  odb_live_sf <- odb_live %>% 
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  nearest_site_index <- odb_live_sf %>% 
    sf::st_nearest_feature(sites_sf)
  
  odb_live_sf$nearest_site_name <- sites_sf$takeoff_name[nearest_site_index]
  
  odb_live_sf$nearest_site_distance <- odb_live_sf %>% 
    sf::st_distance(sites_sf[nearest_site_index,], by_element = TRUE)
  
  odb_live_sf
  
}

geocode_location <- function(lat = NULL, long = NULL){
  
  if(length(lat)==0 | length(long)==0) return(NA)
  
  googleway::set_key(Sys.getenv('GOOGLE_MAPS'))
  
  geocoded_location <- googleway::google_reverse_geocode(c(lat,long))
  
  # glue::glue('{geocoded_location[["results"]][["address_components"]][[2]][["long_name"]][[2]]}, {geocoded_location[["results"]][["address_components"]][[2]][["long_name"]][[3]]}')
  
  geocoded_location[["results"]][["address_components"]][[2]][["long_name"]][[2]]
}
