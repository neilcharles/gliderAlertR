test_that("tracking works", {

  if (file.exists("pings.RDS")) {
    file.remove("pings.RDS")
  }

  flights_log <- readr::read_csv(test_path("log", "logging.csv"), col_names = c(
    "value",
    "unix_timestamp",
    "latitude",
    "longitude",
    "altitude",
    "ground_level",
    "ground_speed",
    "id",
    "user",
    "user2",
    "user3",
    "call_sign",
    "beacon_type",
    "time",
    "altitude_agl",
    "lat_origin",
    "lon_origin",
    "lat_cur",
    "lon_cur",
    "xc_distance_last",
    "xc_distance_cur",
    "xc_milestones_last",
    "xc_milestones_cur",
    "takeoff_site",
    "nearest_site",
    "nearest_site_distance",
    "ping_status",
    "read_timestamp"
  ),
  show_col_types = FALSE)

  test_day <- flights_log |>
    dplyr::filter(substr(read_timestamp, 1, 10) == '2025-05-18') |>
    dplyr::filter(as.numeric(substr(read_timestamp, 12, 13)) > 6)

  for(i in unique(test_day$read_timestamp)){
    dummy_puretrack <- test_day |>
      dplyr::filter(read_timestamp==i)

    print(as.POSIXct(i, origin = "1970-01-01", tz = "UTC"))

    live_get(pings_source = "testlog", test_log = dummy_puretrack, test_timestamp = as.POSIXct(i, origin = "1970-01-01", tz = "UTC"))

  }

  if (file.exists("pings.RDS")) {
    file.remove("pings.RDS")
  }

})
