send_telegram <- function(message = NULL,
                          chat_id = 96373076,
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
    # Live broadcast group ID's
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
      # Personal chat group with developer
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
  # Manual encoding of aircraft type names because OGN API returns a number
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
      #Glider/motor-glider - Official name replaced with 'sailplane'
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
  # Reads and formats glider ping information
  odb_live <-
    xml2::read_xml("https://live.glidernet.org/lxml.php?a=0&b=59.6&c=49.8&d=2&e=-11") |>
    rvest::html_nodes("m") |>
    rvest::html_attr("a") |>
    tibble::as_tibble() |>
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

  odb_live <- odb_live |>
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
      timestamp = lubridate::force_tz(lubridate::today() + lubridate::hms(timestamp), "UTC"),
      alt_feet = alt * 3.28
    ) |>
    dplyr::left_join(aircraft_codes(), by = "aircraft_type_code") |>
    tidyr::replace_na(list(aircraft_type_name = 'Possible PG or HG')) |>
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

get_site_distances <- function(pings_live = NULL, sites = NULL, longitude = "longitude", latitude = "latitude") {
  #Finds the nearest site for each location in a live pings table
  sites_sf <- sites |>
    sf::st_as_sf(coords = c("takeoff_lon", "takeoff_lat"),
                           crs = 4326)

  pings_live_sf <- pings_live |>
    sf::st_as_sf(coords = c(longitude, latitude), crs = 4326)

  nearest_site_index <- pings_live_sf |>
    sf::st_nearest_feature(sites_sf)

  pings_live_sf$nearest_site_name <-
    sites_sf$takeoff_name[nearest_site_index]

  pings_live_sf$nearest_site_distance <- pings_live_sf |>
    sf::st_distance(sites_sf[nearest_site_index, ], by_element = TRUE)

  tibble::tibble(site_name = pings_live_sf$nearest_site_name, site_distance = pings_live_sf$nearest_site_distance)

}

geocode_location <- function(lat = NULL, long = NULL) {

  # Uses Google Maps API locate the name of a lat long point
  # Tries a few possibilities in order of preference in case of null for e.g. locality

  if (length(lat) == 0 | length(long) == 0)
    return(NA)

  googleway::set_key(Sys.getenv('GOOGLE_MAPS'))

  geocoded_location <-
    googleway::google_reverse_geocode(c(lat, long))

  if(length(geocoded_location[["results"]])==0){
    return("_")
  }

  name_attempt <- geocoded_location[["results"]] |>
    tibble::as_tibble() |>
    dplyr::select(address_components) |>
    tidyr::unnest(address_components) |>
    dplyr::filter(stringr::str_detect(as.character(types), 'locality')) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull(short_name)

  if (length(name_attempt) > 0)
    return(name_attempt)

  name_attempt <- geocoded_location[["results"]] |>
    tibble::as_tibble() |>
    dplyr::select(address_components) |>
    tidyr::unnest(address_components) |>
    dplyr::filter(stringr::str_detect(as.character(types), 'administrative_area_level_3')) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull(short_name)

  if (length(name_attempt) > 0)
    return(name_attempt)

  geocoded_location[["results"]] |>
    tibble::as_tibble() |>
    dplyr::select(address_components) |>
    tidyr::unnest(address_components) |>
    dplyr::filter(stringr::str_detect(as.character(types), 'postal_town')) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull(short_name)
}

terrain_elevation <- function(lon = NULL, lat = NULL){
  # Get terrain height at a location for calculating height AGL

  if(length(lon) < 1 | length(lat) < 1) return(NA)

  df <- data.frame(x = lon, y = lat)
  elevation <- elevatr::get_elev_point(df, src="aws", prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", overwrite = FALSE, z = 14)

  tryCatch(
    expr = {
      return(as.numeric(elevation$elevation) * 3.28)
    },
    error = function(e){
      return(0)
    }
  )

}

add_telegram_groups <- function(pings, sites){

  site_groups <- sites |>
    dplyr::select(takeoff_name, telegram_group_name) |>
    dplyr::left_join(telegram_groups(), by = "telegram_group_name")

  pings |>
    dplyr::left_join(site_groups, by = c("takeoff_site" = "takeoff_name"))

}

summarise_site_pings <- function(pings, sites, max_age = 20, on_xc_distance = 5){

  test <- pings |>
    add_telegram_groups(sites) |>
  # Summarises glider pings table into a string to be sent as a Telegram message
    dplyr::filter(time >= lubridate::now() - lubridate::minutes(max_age)) |>
    dplyr::rowwise() |>
    dplyr::mutate(alt_agl = ifelse(
      xc_distance_cur > on_xc_distance,
      altitude - terrain_elevation(longitude, latitude),
      NA)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      flying = ifelse(
        (xc_distance_cur < on_xc_distance & ground_speed > 3) |
          altitude_agl >= 150,
        1, 0),
      on_xc = ifelse(
        xc_distance_cur >= on_xc_distance &
          flying==1,
        1,
        0
      ),
      parawaiting = ifelse(xc_distance_cur < on_xc_distance & flying==0, 1, 0)
    ) |>
    dplyr::group_by(telegram_group_name, telegram_group_id, takeoff_site) |>
    dplyr::mutate(lat = mean(ifelse(on_xc == 0, lat_cur, NA), na.rm = TRUE),
           long = mean(ifelse(on_xc == 0, lon_cur, NA), na.rm = TRUE)) |>
    dplyr::group_by(
      beacon_type,
      telegram_group_name,
      telegram_group_id,
      takeoff_site,
      lat,
      long
    ) |>
    dplyr::summarise(
      max_alt = ifelse(sum(flying) - sum(on_xc) > 0,
                       max(
                         ifelse(on_xc == 0 & flying == 1, altitude, NA), na.rm = TRUE
                       ), NA),
      avg_alt = ifelse(sum(flying) - sum(on_xc) > 0,
                       mean(
                         ifelse(on_xc == 0 & flying == 1, altitude, NA), na.rm = TRUE
                       ), NA),
      flying = sum(flying) - sum(on_xc),
      parawaiting = sum(parawaiting, na.rm = TRUE),
      on_xc = sum(on_xc),
      max_xc_distance = max(ifelse(on_xc==1, xc_distance_cur, 0)
      )
    ) |>
    dplyr::filter(flying > 0 | parawaiting > 0 | on_xc > 0) %>%
    dplyr::mutate(
      summary_text = glue::glue(
        "{beacon_type} {flying}|{parawaiting}|{on_xc}|{round(avg_alt, 0)}'|{round(max_alt, 0)}'"
      )
    ) |>
    dplyr::mutate(summary_text = ifelse(max_xc_distance > 0,
                                        glue::glue("{summary_text}\nFurthest airborne glider on XC is at {max_xc_distance}km"), summary_text)) |>
    dplyr::group_by(telegram_group_name,
             telegram_group_id,
             takeoff_site,
             lat,
             long) |>
    dplyr::summarise(summary_text = paste0(summary_text, collapse = '\n')) |>
    dplyr::mutate(
      summary_text = glue::glue(
        # '<b>{takeoff_site}</b>\n{summary_text}\n<a href="https://glideandseek.com/?viewport={lat},{long},14">GlideAndSeek Map</a>'
        # '<b>{takeoff_site}</b>\n{summary_text}\n<a href="https://live.safesky.app/map?lat={lat}&lng={long}&zoom=12.50">Map</a>'
        '<b>{takeoff_site}</b>\n{summary_text}\n<a href="https://puretrack.io/?l={lat},{long}&z=14.0">Puretrack</a>'
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
message_everyone <- function(message, override_daylight = FALSE){
  purrr::walk(.x = telegram_groups()$telegram_group_id, .f = ~ send_telegram(message, .x, override_daylight))
}
