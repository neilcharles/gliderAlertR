#' Send Live Alerts and log positions
#'
#' @return
#' @export
#'
#' @examples
#' #odb_live_get()
odb_live_get <- function(){

  message_limit <- 100
  pg_takeoff_size <- 100000

  if(lubridate::month(lubridate::now()) %in% c(10,11,12,1,2)){
    xc_milestone_interval <- 15
  } else {
    xc_milestone_interval <- 25
  }

  sites <- readr::read_csv("https://raw.githubusercontent.com/neilcharles/uk_pg_sites/main/sites.csv") %>%
    dplyr::filter(is.na(exclude) | !exclude) %>%
    dplyr::select(-exclude, -notes)

  sites <- sites %>%
    sf::st_as_sf(coords = c("takeoff_lon", "takeoff_lat"),
             crs = 4326)

  odb_live <- read_ogn_live() %>%
    dplyr::filter(timestamp >= lubridate::now() - lubridate::minutes(5))

  #----------- Load first pings --------------------------------------------------

  if (file.exists("odb_first_pings.RDS")) {
    odb_first_pings <- readr::read_rds("odb_first_pings.RDS")
  } else {
    odb_first_pings <- odb_live[0, ]
  }

  #------------ tag live pings for probable paragliders --------------------------

  odb_live_sf <- get_site_distances(odb_live, sites)

  probable_paragliders <- odb_live_sf %>%
    dplyr::filter(
      nearest_site_distance <= units::set_units(pg_takeoff_size, metre)
      & ground_speed_kph <= 100
      & substr(registration, 1, 2) != "G-"
      & aircraft_type_name %in% c("Paraglider", "Hang glider", "Possible PG or HG", "", NA)
    )

  #Filter live pings for new probable paragliders or already designated as
  #paraglider based on first ping
  odb_live <- odb_live %>%
    dplyr::filter(
      registration2 %in% odb_first_pings$registration2 |
        registration2 %in% probable_paragliders$registration2
    )

  #Add pg takeoff site
  sf::st_geometry(odb_live_sf) <-
    NULL  #Remove geo to allow standard left join

  odb_live_sf <- odb_live_sf %>%
    dplyr::select(registration2, nearest_site_name, nearest_site_distance)

  odb_live <- odb_live %>%
    dplyr::left_join(odb_live_sf, by = "registration2") %>%
    dplyr::left_join(
      dplyr::select(sites, takeoff_name, telegram_group_name),
      by = c("nearest_site_name" = "takeoff_name")
    ) %>%
    dplyr::left_join(telegram_groups(), by = "telegram_group_name")

  #Rebuild the first ping table if it didn't exist (to ensure all cols included)
  if (!file.exists("odb_first_pings.RDS")) {
    odb_first_pings <- odb_live[0, ]
  }

  #------------ update first pings -----------------------------------------------

  odb_new_pings <-
    dplyr::filter(odb_live,!registration2 %in% odb_first_pings$registration2)

  odb_first_pings_updated <- odb_first_pings %>%
    dplyr::union_all(odb_new_pings)

  #------------ Load most recent pings--------------------------------------------

  if (file.exists("odb_last_pings.RDS")) {
    odb_last_pings <- readr::read_rds("odb_last_pings.RDS")
  } else {
    odb_last_pings <- odb_live[0, ]
  }

  #------------ Calculate distances from first ping ------------------------------

  xc_distances <-
    dplyr::select(odb_first_pings_updated, registration2, device_id, long, lat, registration_label)  %>%
    dplyr::left_join(dplyr::select(odb_last_pings, registration2, long, lat), by = "registration2") %>%
    dplyr::rename(
      long_first = long.x,
      lat_first = lat.x,
      long_last = long.y,
      lat_last = lat.y
    ) %>%
    dplyr::left_join(dplyr::select(odb_live, registration2, long, lat, alt_feet), by = "registration2") %>%
    dplyr::rename(long_live = long, lat_live = lat, alt_feet_live = alt_feet) %>%
    tidyr::drop_na()

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
    dplyr::mutate(
      distance_last = round(as.numeric(distance_last) / 1000, 0),
      distance_live = round(as.numeric(distance_live) / 1000, 0),
      xc_milestones_last = floor(distance_last / xc_milestone_interval) * xc_milestone_interval,
      xc_milestones_live = floor(distance_live / xc_milestone_interval) * xc_milestone_interval
    ) %>%
    dplyr::filter(xc_milestones_live > xc_milestones_last) %>%
    dplyr::filter(registration_label != "(no OGN reg)") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ground_elevation = terrain_elevation(long_live, lat_live),
           alt_agl_live = alt_feet_live - ground_elevation) %>%
    dplyr::filter(alt_agl_live > 300) %>%
    dplyr::ungroup()

  #------------ Send Telegram Messages -------------------------------------------

  #New trackers
  if (nrow(odb_new_pings) > 0) {
    if (nrow(odb_new_pings) > message_limit) {
      odb_new_messages <- odb_new_pings[1:message_limit, ]
    } else {
      odb_new_messages <- odb_new_pings
    }

    odb_new_messages %>%
      dplyr::filter(!nearest_site_name %in% odb_first_pings$nearest_site_name) %>%
      summarise_site_pings() %>%
      dplyr::filter(!is.na(telegram_group_id)) %>%
      dplyr::mutate(telegram_message = glue::glue("<b>First pilots at site since >1 hour</b>\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{summary_text}")) %>%
        purrr::walk2(
          .x = .$telegram_message,
          .y = .$telegram_group_id,
          .f = ~ send_telegram(.x, .y)
        )
  }

  #XC Distances
  xc_distances %>%
    dplyr::left_join(odb_live, by = "registration2") %>%
    dplyr::left_join(dplyr::select(
      odb_first_pings_updated,
      registration2,
      nearest_site_name,
      telegram_group_id
    ),
    by = "registration2") %>%
    dplyr::top_n(message_limit, registration2) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(location_name_live = geocode_location(lat = lat_live, long = long_live)) %>%
    dplyr::mutate(
      telegram_message = glue::glue(
        "{aircraft_type_name} {registration_label.x} is on XC from <b>{nearest_site_name.y}</b>, passing {location_name_live} at {distance_live}km.\n{round(alt_feet,0)}' AMSL ({round(alt_agl_live,0)}' AGL) & {ground_speed_kph}kph\n<a href='https://glideandseek.com?aircraft={device_id.x}'>GlideAndSeek Map</a>"
      )
    ) %>%
    dplyr::filter(device_id.x != 0) %>%
    dplyr::filter(!is.na(telegram_group_id.y)) %>%
    purrr::walk2(
      .x = .$telegram_message,
      .y = .$telegram_group_id.y,
      .f = ~ send_telegram(.x, .y)
    )

  #------------ record most recent pings -----------------------------------------

  odb_last_pings <-
    dplyr::filter(odb_last_pings,!registration2 %in% odb_live$registration2) %>%
    dplyr::union_all(odb_live)

  #------------ Delete old records -----------------------------------------------

  #Remove any device ID that hasn't been seen for an hour
  old_records <- odb_last_pings %>%
    dplyr::filter(timestamp <= lubridate::now() - lubridate::hours(1)) %>%
    dplyr::pull(registration2)

  odb_first_pings_updated <- odb_first_pings_updated %>%
    dplyr::filter(!registration2 %in% old_records)

  odb_last_pings <- odb_last_pings %>%
    dplyr::filter(!registration2 %in% old_records)

  #------------ save pings -------------------------------------------------------

  readr::write_rds(odb_first_pings_updated, "odb_first_pings.RDS")
  readr::write_rds(odb_last_pings, "odb_last_pings.RDS")
}
