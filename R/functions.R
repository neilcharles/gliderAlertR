send_telegram <- function(message = NULL,
                          chat_id = -1001798217889,
                          override_daylight = FALSE) {
  bot <- telegram.bot::Bot(token = Sys.getenv('TELEGRAM_HILLTOP'))
  
  if(isDaylightNow() | override_daylight){
    bot$sendMessage(chat_id = chat_id,
                    parse_mode = 'HTML',
                    disable_web_page_preview = TRUE,
                    text = message)   #chat with Neil 96373076
  }  
}

telegram_groups <- function() {
  if(Sys.getenv("PG_ALERTS_LIVE")==TRUE) testing <- FALSE else testing <- TRUE
  if (!testing) {
    return(tibble::tibble(
      telegram_group_id = c(
        -1001688067917,-1001545184005,-1001226015011,-1001750937053,-1001757520671,-1001577094376,-1001679816287,-1001691078874,-1001690641916,-1001798217889,-1001768573848,-1001624842375,-1001571452843,-1001677231927,-1001719738514,-1001765135861
      ),
      telegram_group_name = c(
        "Borders",
        "Central",
        "East",
        "Highlands",
        "Isle of Wight",
        "Lakes",
        "Mid Wales & Mynd",
        "North East",
        "North Wales",
        "Pennines & Dales",
        "South East",
        "South Scotland & Grampian",
        "South Wales",
        "South West",
        "West",
        "Dunstable"
      )
    ))
  } else {
    return(tibble::tibble(
      telegram_group_id = c(
        96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076,96373076
      ),
      telegram_group_name = c(
        "Borders",
        "Central",
        "East",
        "Highlands",
        "Isle of Wight",
        "Lakes",
        "Mid Wales & Mynd",
        "North East",
        "North Wales",
        "Pennines & Dales",
        "South East",
        "South Scotland & Grampian",
        "South Wales",
        "South West",
        "West",
        "Dunstable"
      )
    ))
  }
  
}


aircraft_codes <- function() {
  tibble::tibble(
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
      '10',
      '11',
      '12',
      '13',
      '14',
      '15'
    ),
    aircraft_type_name = c(
      'Possible PG or HG',
      'Sailplane',
      #Glider/motor-glider
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

read_ogn_live <- function() {
  odb_live <-
    xml2::read_xml("https://live.glidernet.org/lxml.php?a=0&b=59.6&c=49.8&d=2&e=-11") %>%
    rvest::html_nodes("m") %>%
    rvest::html_attr("a") %>%
    tibble::as_tibble() %>%
    tidyr::separate(value, as.character(c(1:14)), sep = ",")
  
  names(odb_live) <-
    c(
      "lat",
      "long",
      "cn",
      "registration",
      "alt",
      "timestamp",
      "X1",
      "track_deg",
      "ground_speed_kph",
      "vertical_speed_ms",
      "aircraft_type_code",
      "receiver",
      "device_id",
      "registration2"
    )
  
  odb_live <- odb_live %>%
    dplyr::mutate(
      across(
        c(
          "alt",
          "X1",
          "track_deg",
          "ground_speed_kph",
          "vertical_speed_ms",
          "lat",
          "long"
        ),
        as.numeric
      ),
      timestamp = lubridate::today() + lubridate::hms(timestamp),
      alt_feet = alt * 3.28
    ) %>%
    dplyr::left_join(aircraft_codes(), by = "aircraft_type_code") %>%
    tidyr::replace_na(list(aircraft_type_name = 'Possible PG or HG')) %>%
    dplyr::mutate(
      registration_label = ifelse(
        is.na(registration) |
          registration == registration2 |
          registration == "" |
          registration == "NA",
        "(no OGN reg)",
        glue::glue("'{registration}'")
      )
    )
  
  odb_live
}

get_site_distances <- function(odb_live = NULL, sites = NULL) {
  #Ping is in range of a paragliding takeoff
  sites_sf <-
    sites %>% sf::st_as_sf(coords = c("takeoff_lon", "takeoff_lat"),
                           crs = 4326)
  
  odb_live_sf <- odb_live %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  nearest_site_index <- odb_live_sf %>%
    sf::st_nearest_feature(sites_sf)
  
  odb_live_sf$nearest_site_name <-
    sites_sf$takeoff_name[nearest_site_index]
  
  odb_live_sf$nearest_site_distance <- odb_live_sf %>%
    sf::st_distance(sites_sf[nearest_site_index, ], by_element = TRUE)
  
  odb_live_sf
  
}

geocode_location <- function(lat = NULL, long = NULL) {
  if (length(lat) == 0 | length(long) == 0)
    return(NA)
  
  googleway::set_key(Sys.getenv('GOOGLE_MAPS'))
  
  geocoded_location <-
    googleway::google_reverse_geocode(c(lat, long))
  
  #geocoded_location[["results"]][["address_components"]][[2]][["long_name"]][[2]]
  
  name_attempt <- geocoded_location[["results"]] %>%
    tibble::as_tibble() %>%
    dplyr::select(address_components) %>%
    tidyr::unnest(address_components) %>%
    dplyr::filter(stringr::str_detect(as.character(types), 'locality')) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::pull(short_name)
  
  if (length(name_attempt) > 0)
    return(name_attempt)
  
  name_attempt <- geocoded_location[["results"]] %>%
    tibble::as_tibble() %>%
    dplyr::select(address_components) %>%
    tidyr::unnest(address_components) %>%
    dplyr::filter(stringr::str_detect(as.character(types), 'administrative_area_level_3')) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::pull(short_name)
  
  if (length(name_attempt) > 0)
    return(name_attempt)
  
  geocoded_location[["results"]] %>%
    tibble::as_tibble() %>%
    dplyr::select(address_components) %>%
    tidyr::unnest(address_components) %>%
    dplyr::filter(stringr::str_detect(as.character(types), 'postal_town')) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::pull(short_name)
}

terrain_elevation <- function(lon = NULL, lat = NULL){
      
  if(length(lon) < 1 | length(lat) < 1) return(NA)
  
  df <- data.frame(x = lon, y = lat)
  
  tryCatch(
    expr = {
      elevation <- elevatr::get_elev_point(df, src="aws", prj = "EPSG:4326", overwrite = FALSE, z = 14)
    },
    error = function(e){
      elevation <- 0
    }
  )
  
  return(as.numeric(elevation$elevation) * 3.28)
}

summarise_site_pings <- function(pings){
  pings %>%
    dplyr::filter(timestamp >= lubridate::now() - lubridate::minutes(10)) %>%
    dplyr::mutate(
      on_xc = ifelse(
        nearest_site_distance > units::set_units(5000, metre) &
          ground_speed_kph > 2,
        1,
        0
      ),
      flying = ifelse(ground_speed_kph > 2, 1, 0)
    ) %>%
    dplyr::group_by(telegram_group_name, telegram_group_id, nearest_site_name) %>%
    dplyr::mutate(lat = mean(ifelse(on_xc == 0, lat, NA), na.rm = TRUE),
           long = mean(ifelse(on_xc == 0, long, NA), na.rm = TRUE)) %>%
    dplyr::group_by(
      aircraft_type_name,
      telegram_group_name,
      telegram_group_id,
      nearest_site_name,
      lat,
      long
    ) %>%
    dplyr::summarise(
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
    dplyr::mutate(
      summary_text = glue::glue(
        "{aircraft_type_name} {flying}|{parawaiting}|{on_xc}|{round(avg_alt, 0)}'|{round(max_alt, 0)}'"
      )
    ) %>%
    dplyr::group_by(telegram_group_name,
             telegram_group_id,
             nearest_site_name,
             lat,
             long) %>%
    dplyr::summarise(summary_text = paste0(summary_text, collapse = '\n')) %>%
    dplyr::mutate(
      summary_text = glue::glue(
        '<b>{nearest_site_name}</b>\n{summary_text}\n<a href="https://glideandseek.com/?viewport={lat},{long},14">GlideAndSeek Map</a>'
      )
    )
}

isDaylightNow <- function(date = Sys.Date(), lat = 52.4775215, lon = -1.9336708){
  lubridate::with_tz(lubridate::now(), tzone = 'Europe/London') <=
    suncalc::getSunlightTimes(date = date, lat = lat, lon = lon, keep = "sunset")$sunset + lubridate::minutes(30) &
    lubridate::with_tz(lubridate::now(), tzone = 'Europe/London') >=
    suncalc::getSunlightTimes(date = Sys.Date(), lat = lat, lon = lon, keep = "sunrise")$sunrise - lubridate::minutes(30)
}

#' Send a broadcast message to all telegram groups
#'
#' @param message 
#'
#' @return
#' @export
#'
#' @examples
#' #message_everyone("The alerts app has been updated")
message_everyone <- function(message, override_daylight){
  purrr::walk(.x = telegram_groups()$telegram_group_id, .f = ~ send_telegram(message, .x, override_daylight))
}
