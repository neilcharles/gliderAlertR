library(tidyverse)
library(jsonlite)

odb_ddb_raw <-
  jsonlite::read_json("http://ddb.glidernet.org/download/?j=1&t=1") %>%
  as_tibble() %>%
  unnest_wider(devices)

odb_ddb_raw %>%
  write_csv("odb_ddb_raw.csv")

#Glider type source
#http://wiki.glidernet.org/wiki:ogn-flavoured-aprs

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
) %>% 
  write_rds("aircraft_codes.RDS")
