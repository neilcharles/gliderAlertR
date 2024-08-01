# Appends columns to raw safesky data to store site names etc.
safesky_append_columns <- function(pings = NULL){
  pings |>
    dplyr::mutate(
    lat_origin = rep(NA, nrow(pings)),
    lon_origin = rep(NA, nrow(pings)),
    lat_cur = rep(NA, nrow(pings)),
    lon_cur = rep(NA, nrow(pings)),
    xc_distance_prev = rep(NA, nrow(pings)),
    xc_distance_cur = rep(NA, nrow(pings)),
    takeoff_site = rep(NA, nrow(pings)),
    nearest_site = rep(NA, nrow(pings)),
    nearest_site_distance = rep(NA, nrow(pings)))
}

read_safesky_live <- function(){

  pings <- httr::GET(url = 'https://public-api.safesky.app/v1/beacons/?altitude_max=10000&beacon_types=MOTORPLANE%2CPARA_GLIDER%2CHAND_GLIDER&show_grounded=true&viewport=47.4753,-10.4867,59.5851,3.2422',
                     httr::add_headers(.headers = c('x-api-key'= Sys.getenv('SAFESKY')))) |>
    httr::content(as="text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    dplyr::mutate(
      date = anytime::anydate(last_update),
      time = anytime::anytime(last_update),
    )

  pings

}

safesky_live_get <- function(){

  message_limit <- 100
  pg_takeoff_size <- 1000

  if(lubridate::month(lubridate::now()) %in% c(10,11,12,1,2)){
    xc_milestone_interval <- 15
  } else {
    xc_milestone_interval <- 25
  }

  sites <- readr::read_csv("https://raw.githubusercontent.com/neilcharles/uk_pg_sites/main/sites.csv") |>
    dplyr::filter(is.na(exclude) | !exclude) |>
    dplyr::select(-exclude, -notes)

  pings_live <- read_safesky_live() |>
    dplyr::filter(time >= lubridate::now() - lubridate::minutes(5)) |>
    safesky_append_columns() |>
    dplyr::mutate(
      lat_cur = latitude,
      lon_cur = longitude
    )


  #---- Load first pings or set empty table ------------------------------------
  if (file.exists("pings.RDS")) {
    pings_cache <- readr::read_rds("pings.RDS")
  } else {
    pings_cache <- pings_live[0, ] |>
      safesky_append_columns()
  }

  # Process new pings with id's that weren't in cache
  pings_new <- pings_live |>
    dplyr::filter(!id %in% pings_cache$id) |>
    dplyr::mutate(
      lat_origin = latitude,
      lon_origin = longitude
      )

  site_distances <- get_site_distances(pings_new, sites)

  pings_new$takeoff_site <- site_distances$site_name
  pings_new$nearest_site <- site_distances$site_name
  pings_new$nearest_site_distance <- site_distances$site_distance

  # Update cached pings only retaining takeoff name and origin lat/lon
  # pings_cache <- pings_new[1:2,]

  pings_cache_join <- pings_cache |>
    tidyr::nest(data = -id) |>
    dplyr::left_join(tidyr::nest(pings_live, data = -id), by = "id")

  # Glider ID's that were not in the latest data but are still being tracked
  pings_cache_no_update <- pings_cache_join |>
    dplyr::filter(is.na(data.y)) |>
    tidyr::unnest(data.x) |>
    dplyr::select(-data.y)

  # Glider ID's that have received an update - keep original takeoff and origin lat/lon
  pings_cache_update <- pings_cache_join |>
    dplyr::filter(is.na(data.y)) |>
    dplyr::mutate(data.y = purrr::map2(
      .x = data.x,
      .y = data.y,
      .f = ~ dplyr::mutate(
        .y,
        takeoff_site = .x$takeoff_site,
        lat_origin = .x$lat_origin,
        lon_origin = .x$lon_origin
      )
    )) |>
    dplyr::select(-data.x) |>
    tidyr::unnest(data.y)

  #Append cache and new pings
  pings_all <- pings_cache_update |>
    dplyr::bind_rows(pings_cache_no_update) |>
    dplyr::bind_rows(pings_new) |>
    dplyr::select(-data.x, -data.y)  #needed in case the cache is empty because then columns don't unnest

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

  pings_all <- pings_all |>
    dplyr::mutate(xc_distance_cur = round(as.numeric(xc_distance_cur) / 1000, 0))

  summary_text <- summarise_site_pings(pings_all, sites)

  #distances to km
  # xc_distances <- xc_distances |>
  #   dplyr::mutate(
  #     distance_last = round(as.numeric(distance_last) / 1000, 0),
  #     distance_live = round(as.numeric(distance_live) / 1000, 0),
  #     xc_milestones_last = floor(distance_last / xc_milestone_interval) * xc_milestone_interval,
  #     xc_milestones_live = floor(distance_live / xc_milestone_interval) * xc_milestone_interval
  #   )
  #
  # #Filter and calculate AGL
  # xc_distances_report <- xc_distances |>
  #   dplyr::filter(xc_milestones_live > xc_milestones_last) |>
  #   dplyr::filter(registration_label != "(no OGN reg)") |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(ground_elevation = terrain_elevation(long_live, lat_live),
  #                 alt_agl_live = alt_feet_live - ground_elevation) |>
  #   dplyr::filter(alt_agl_live > 100) |> #filter to avoid reporting pilots driving home
  #   dplyr::ungroup()
  #
  # #--- Append XC distances to the live table so that site summaries can report count on XC -----
  # odb_live <- odb_live |>
  #   dplyr::left_join(
  #     dplyr::select(xc_distances,
  #                   registration2,
  #                   distance_live),
  #     by = 'registration2'
  #   )

  #------------ Send Telegram Messages -------------------------------------------

  #New trackers
  if (nrow(pings_new) > 0) {
    if (nrow(pings_new) > message_limit) {
      messages_new <- pings_new[1:message_limit, ]
    } else {
      odb_new_messages <- odb_new_pings
    }

    telegram_new <- summary_text |>
      dplyr::filter(!takeoff_site %in% pings_cache$takeoff_site) |>
      dplyr::filter(!is.na(telegram_group_id)) |>
      dplyr::mutate(telegram_message = glue::glue("<b>First pilots at site since >1 hour</b>\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\n{summary_text}"))

    telegram_new <- telegram_new |>
      mutate(telegram_message = ifelse(on_xc > 0, glue::glue("{telegram_message}\n\nFurthest Glider on XC is at {xxxx}km")))

    purrr::walk2(
      .x = telegram_new$telegram_message,
      .y = telegram_new$telegram_group_id,
      .f = ~ send_telegram(.x, .y)
    )
  }












  #Rebuild the last ping table if it didn't exist (to ensure all cols included)
  if (!file.exists("odb_last_pings.RDS")) {
    odb_last_pings <- odb_live[0, ]
  }

  #------------ record most recent pings -----------------------------------------

  odb_last_pings <-
    dplyr::filter(odb_last_pings,!registration2 %in% odb_live$registration2) |>
    dplyr::union_all(odb_live)

  #------------ Delete old records -----------------------------------------------

  #Remove any device ID that hasn't been seen for an hour
  old_records <- odb_last_pings |>
    dplyr::filter(timestamp <= lubridate::now() - lubridate::hours(1)) |>
    dplyr::pull(registration2)

  odb_first_pings_updated <- odb_first_pings_updated |>
    dplyr::filter(!registration2 %in% old_records)

  odb_last_pings <- odb_last_pings |>
    dplyr::filter(!registration2 %in% old_records) |>
    tidyr::replace_na(list(distance_live = 0))

  #------------ save pings -------------------------------------------------------

  readr::write_rds(odb_first_pings_updated, "odb_first_pings.RDS")
  readr::write_rds(odb_last_pings, "odb_last_pings.RDS")


}

# safesky_live_get()
