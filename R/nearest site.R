library(sf)

sites <- read_rds("sites_list.RDS")

sites_sf <- sites %>% st_as_sf(coords = c("takeoff_lon", "takeoff_lat"), crs = 4326)

odb_live_sf <- odb_live %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

nearest_site_index <- odb_live_sf %>% 
  st_nearest_feature(sites_sf)

odb_live_sf$nearest_site_name <- sites_sf$takeoff_name[nearest_site_index]

odb_live_sf$nearest_site_distance <- st_distance(odb_live_sf, sites_sf[nearest_site_index,], by_element = TRUE)

