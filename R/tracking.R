# Appends columns to raw safesky data to store site names etc.
pings_append_columns <- function(pings = NULL){
  pings |>
    dplyr::mutate(
    lat_origin = rep(NA, nrow(pings)),
    lon_origin = rep(NA, nrow(pings)),
    lat_cur = rep(NA, nrow(pings)),
    lon_cur = rep(NA, nrow(pings)),
    xc_distance_last = rep(NA, nrow(pings)),
    xc_distance_cur = rep(NA, nrow(pings)),
    xc_milestones_last = rep(NA, nrow(pings)),
    xc_milestones_cur = rep(NA, nrow(pings)),
    takeoff_site = rep(NA, nrow(pings)),
    nearest_site = rep(NA, nrow(pings)),
    nearest_site_distance = rep(NA, nrow(pings)),
    ping_status = rep(NA, nrow(pings)))
}

#' Reads sites databse from github
#'
#' @return
#' @export
#'
#' @examples
read_sites <- function(){
  readr::read_csv("https://raw.githubusercontent.com/neilcharles/uk_pg_sites/main/sites.csv") |>
    dplyr::filter(is.na(exclude) | !exclude) |>
    dplyr::select(-exclude, -notes)
}

#' Reads live data from safesky
#'
#' @return
#' @export
#'
#' @examples
read_safesky_live <- function(){

  pings <- httr::GET(url = 'https://public-api.safesky.app/v1/beacons/?altitude_max=10000&beacon_types=PARA_GLIDER%2CHAND_GLIDER&show_grounded=true&viewport=47.4753,-10.4867,59.5851,3.2422',
                     httr::add_headers(.headers = c('x-api-key'= Sys.getenv('SAFESKY')))) |>
    httr::content(as="text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  if(length(pings)==0){
    return(NULL)
  }

  pings <- pings |>
    dplyr::mutate(
      date = anytime::anydate(last_update),
      time = anytime::anytime(last_update),
    ) |>
    dplyr::mutate(beacon_type = dplyr::case_when(
      beacon_type == "PARA_GLIDER" ~ "Paraglider",
      beacon_type == "HAND_GLIDER" ~ "Hang glider",
      beacon_type == "MOTORPLANE" ~ "Motorplane",
      .default = as.character(beacon_type)
    )) |>
    dplyr::mutate(altitude = altitude * 3.28)

  pings

}

#' Reads live data from safesky
#'
#' @return
#' @export
#'
#' @examples
read_puretrack_live <- function(){

  body_data <- list(
    a  = NULL,
    b1l = "61.74650",
    b1g = "4.26710",
    b2l = "47.22574",
    b2g = "-14.73417",
    # s = "30-z6D_111339,30-z6D_111339",
    o = c(63, 6, 7, 17, 20),
    t = 15,
    a = NULL,
    i = 0,
    l = TRUE
  )

  # Convert the data to JSON format
  json_body_data <- jsonlite::toJSON(body_data, auto_unbox = TRUE)

  pings <- httr::POST(url = 'https://puretrack.io/api/live', httr::add_headers(`Content-Type` = "application/json"), body = json_body_data) |>
    httr::content(as="text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  if(length(pings)==0){
    return(NULL)
  }

  pings_processed <- tibble::as_tibble(pings$data) |>
    dplyr::mutate(unix_timestamp = as.numeric(stringr::str_extract(value, "(?<=T)\\d{10}")),
                  latitude = as.numeric(stringr::str_extract(value, "(?<=,L)-?\\d+\\.\\d+")),
                  longitude = as.numeric(stringr::str_extract(value, "(?<=,G)-?\\d+\\.\\d+")),
                  altitude = as.numeric(stringr::str_extract(value, "(?<=,A)\\d+(?=,)")),
                  ground_level = as.numeric(stringr::str_extract(value, "(?<=,g)\\d+(?=,)")),
                  ground_speed = as.numeric(stringr::str_extract(value, "(?<=S)(\\d+(\\.\\d+)?)")),
                  id = stringr::str_extract(value, "(?<=,K)[^,]+"),
                  user = stringr::str_extract(value, "(?<=,N)[^,]+"),
                  user2 = stringr::str_extract(value, "(?<=,B)[^,]+"),
                  user3 = stringr::str_extract(value, "(?<=,E)[^,]+"),
                  call_sign = ifelse(is.na(user), ifelse(is.na(user2), user3, user2), user),
                  beacon_type = stringr::str_extract(value, "(?<=,O)\\d+(?=,)")) |>
    dplyr::mutate(
      beacon_type = dplyr::case_when(
        beacon_type == "6" ~ "Hang glider",
        beacon_type == "7" ~ "Paraglider",
        .default = NA
      )
    ) |>
    dplyr::filter(!is.na(beacon_type)) |>
    dplyr::mutate(
      time = as.POSIXct(unix_timestamp, origin = "1970-01-01", tz = "UTC"),
    ) |>
    tidyr::replace_na(list("altitude" = 0, "ground_speed" = 0, ground_level = 0)) |>
    dplyr::mutate(altitude = round(altitude * 3.28, 0),
                  ground_level = round(ground_level * 3.28, 0),
                  ground_speed = round(ground_speed * 1.852, 0),
                  altitude_agl = altitude - ground_level
                  )

  pings_processed

}


# safesky_append_broadcast_groups(pings){
#
# }

#' Reads data from safesky, updates the cache and sends live telegram alerts
#'
#' @return
#' @export
#'
#' @examples
live_get <- function(pings_source = "puretrack", glider_milestone_count = 5, logging = FALSE, flying_altitude_agl = 300){

  message_limit <- 100
  pg_takeoff_size <- 1000

  # if(lubridate::month(lubridate::now()) %in% c(10,11,12,1,2)){
  #   xc_milestone_interval <- milestone_winter
  # } else {
  #   xc_milestone_interval <- milestone_summer
  # }

  sites <- read_sites()

  if(pings_source=="safesky"){
    pings_live <- read_safesky_live()
  } else if (pings_source=="puretrack"){
    pings_live <- read_puretrack_live()
  }

  if(is.null(pings_live)){
    message("No aircraft found")
    return(invisible(NULL))
  }

  pings_live <- pings_live |>
    dplyr::filter(time >= lubridate::now() - lubridate::minutes(5)) |>
    pings_append_columns() |>
    dplyr::mutate(
      lat_cur = latitude,
      lon_cur = longitude
    )

  # Logging --------------------------------------------------------------------

  if(logging){
    time_stamp <- lubridate::now()

    pings_live |>
      dplyr::mutate(
        read_timestamp = time_stamp
      ) |>
      readr::write_csv("logging.csv", append = TRUE)
  }

  #---- Load first pings or set empty table ------------------------------------
  if (file.exists("pings.RDS")) {
    pings_cache <- readr::read_rds("pings.RDS")
  } else {
    pings_cache <- pings_live[0, ] |>
      pings_append_columns()
  }

  #------------ Delete old records -----------------------------------------------

  #Remove any device ID that hasn't been seen for an hour
  pings_cache <- pings_cache |>
    dplyr::filter(time > lubridate::now() - lubridate::hours(1)) |>
    dplyr::mutate(ping_status = "cache")


  # Process new pings with id's that weren't in cache
  pings_new <- pings_live |>
    dplyr::filter(!id %in% pings_cache$id) |>
    dplyr::mutate(
      lat_origin = latitude,
      lon_origin = longitude
      ) |>
    dplyr::mutate(ping_status = "new")

  site_distances <- get_site_distances(pings_new, sites)

  pings_new$takeoff_site <- site_distances$site_name
  pings_new$nearest_site <- site_distances$site_name
  pings_new$nearest_site_distance <- site_distances$site_distance

  pings_new_at_site <- pings_new |>
    dplyr::filter(as.numeric(nearest_site_distance) <= pg_takeoff_size)

  # Update cached pings only retaining takeoff name and origin lat/lon
  pings_cache_join <- pings_cache |>
    tidyr::nest(cache = -id) |>
    dplyr::left_join(tidyr::nest(pings_live, live = -id), by = "id")

  # Glider ID's that were not in the latest data but are still being tracked
  pings_cache_no_update <- pings_cache_join |>
    dplyr::filter(!id %in% pings_live$id) |>
    tidyr::unnest(cache) |>
    dplyr::select(-live)

  # Glider ID's that have received an update - keep original takeoff and origin lat/lon
  pings_cache_update <- pings_cache_join |>
    dplyr::filter(id %in% pings_live$id) |>
    dplyr::mutate(live = purrr::map2(
      .x = cache,
      .y = live,
      .f = ~ dplyr::mutate(
        .y,
        takeoff_site = .x$takeoff_site,
        lat_origin = .x$lat_origin,
        lon_origin = .x$lon_origin,
        xc_distance_cur = .x$xc_distance_cur
      )
    )) |>
    dplyr::select(-cache) |>
    tidyr::unnest(live) |>
    dplyr::mutate(ping_status = "updated")

  #Append cache and new pings
  pings_all <- pings_cache_update |>
    dplyr::bind_rows(pings_cache_no_update) |>
    dplyr::bind_rows(pings_new_at_site) |>
    dplyr::select(-dplyr::contains(c("cache", "live")))  #needed in case the cache is empty because then columns don't unnest

  #Calculate nearest sites and distances (already done for new sites but it's fast so whatever)
  site_distances <- get_site_distances(pings_all, sites)

  pings_all$nearest_site <- site_distances$site_name
  pings_all$nearest_site_distance <- site_distances$site_distance

  #Store the previous XC distance to allow calculation of crossing a big number
  pings_all$xc_distance_last <- pings_all$xc_distance_cur

  # Calculate XC distances
  pings_all$xc_distance_cur <-
    sf::st_as_sf(pings_all,
                 coords = c("lon_origin", "lat_origin"),
                 crs = 4326) |>
    sf::st_distance(sf::st_as_sf(
      pings_all,
      coords = c("lon_cur", "lat_cur"),
      crs = 4326
    ), by_element = TRUE)

  # Set XC Milestone based on number of gliders across whole UK that are at distance intervals
  xc_milestone_interval <- 50

  if(nrow(dplyr::filter(pings_all, as.numeric(xc_distance_cur)/1000 > 40)) < glider_milestone_count) xc_milestone_interval <- 40
  if(nrow(dplyr::filter(pings_all, as.numeric(xc_distance_cur)/1000 > 30)) < glider_milestone_count) xc_milestone_interval <- 30
  if(nrow(dplyr::filter(pings_all, as.numeric(xc_distance_cur)/1000 > 20)) < glider_milestone_count) xc_milestone_interval <- 20
  if(nrow(dplyr::filter(pings_all, as.numeric(xc_distance_cur)/1000 > 10)) < glider_milestone_count) xc_milestone_interval <- 10

  # Ensure xc vars are numeric for comparison because they're logical on first run due to NA setup
  pings_all$xc_distance_last = as.numeric(pings_all$xc_distance_last)
  pings_all$xc_milestones_last = as.numeric(pings_all$xc_milestones_last)
  pings_all$xc_milestones_cur = as.numeric(pings_all$xc_milestones_cur)

  pings_all <- pings_all |>
    dplyr::mutate(xc_distance_cur = round(as.numeric(xc_distance_cur) / 1000, 0),
                  xc_milestones_last = floor(xc_distance_last / xc_milestone_interval) * xc_milestone_interval,
                  xc_milestones_cur = floor(xc_distance_cur / xc_milestone_interval) * xc_milestone_interval
    )

  pings_xc_milestone <- pings_all |>
    dplyr::filter(altitude_agl >= flying_altitude_agl) |>
    dplyr::filter(xc_milestones_cur > xc_milestones_last) |>
    dplyr::filter(!is.na(call_sign))

  summary_text <- summarise_site_pings(pings_all, sites, max_age = 10, flying_altitude_agl = flying_altitude_agl)

  #------------ Send New Site Telegram Messages --------------------------------

  #New trackers
  if (nrow(pings_new) > 0) {
    if (nrow(pings_new) > message_limit) {
      messages_new <- pings_new[1:message_limit, ]
    } else {
      messages_new <- pings_new
    }

    telegram_new <- summary_text |>
      dplyr::filter(!takeoff_site %in% pings_cache$takeoff_site) |>
      dplyr::filter(!is.na(telegram_group_id)) |>
      dplyr::mutate(telegram_message = glue::glue("<b>First pilots at site since >1 hour</b>\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{summary_text}"))

    purrr::walk2(
      .x = telegram_new$telegram_message,
      .y = telegram_new$telegram_group_id,
      .f = ~ send_telegram(.x, .y, override_daylight = FALSE)
    )
  }

  #------------ Send XC Milestone Telegram Messages ----------------------------

  if(nrow(pings_xc_milestone) > 0){
    telegram_xc_milestone <- pings_xc_milestone |>
      add_telegram_groups(sites) |>
      dplyr::filter(!is.na(telegram_group_id)) |>
      dplyr::mutate(location_name = geocode_location(latitude, longitude)) |>
      dplyr::mutate(telegram_message = glue::glue("<b>{call_sign}</b> is on XC, {xc_distance_cur}km from {takeoff_site}, passing {location_name} at {altitude}' ({altitude_agl}' AGL)\n<a href='https://puretrack.io/?k={id}&z=14.0'>Puretrack</a>"))

    purrr::walk2(
      .x = telegram_xc_milestone$telegram_message,
      .y = telegram_xc_milestone$telegram_group_id,
      .f = ~ send_telegram(.x, .y, override_daylight = FALSE)
    )
  }

  #------------ save pings -----------------------------------------------------

  readr::write_rds(pings_all, "pings.RDS")

}

#' Sends summary Telegram messages
#'
#' @return
#' @export
#'
#' @examples
summary_send <- function(){

  #---- Load pings or exit -----------------------------------------------------
  if (file.exists("pings.RDS")) {
    pings_cache <- readr::read_rds("pings.RDS")
  } else {
    return(invisible(NULL))
  }

  #------------ Delete old records ---------------------------------------------

  #Remove any device ID that hasn't been seen for an hour
  pings_cache <- pings_cache |>
    dplyr::filter(time > lubridate::now() - lubridate::hours(1)) |>
    dplyr::mutate(ping_status = "cache")


  summary_text <- summarise_site_pings(pings_cache, read_sites(), max_age = 10) |>
    dplyr::group_by(telegram_group_name, telegram_group_id) |>
    dplyr::summarise(summary_text = paste0(summary_text, collapse = '\n'))

  #------------ Send Telegram Messages -------------------------------------------

  telegram_messages <- summary_text |>
    dplyr::filter(!is.na(telegram_group_id)) |>
    dplyr::mutate(telegram_message = glue::glue("<b>{telegram_group_name} Summary</b>\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{summary_text}"))

  #   mutate(telegram_message = ifelse(on_xc > 0, glue::glue("{telegram_message}\n\nFurthest Glider on XC is at {xxxx}km")))

  purrr::walk2(
    .x = telegram_messages$telegram_message,
    .y = telegram_messages$telegram_group_id,
    .f = ~ send_telegram(.x, .y, override_daylight = FALSE)
  )

}
