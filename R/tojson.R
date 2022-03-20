odb_live$nearest_site_distance <- as.numeric(odb_live$nearest_site_distance)
test <- jsonlite::toJSON(odb_live)
write(test, "odb_live.json")



df <- data.frame(x = lon = c(-4.0766272,-4.0766272),
                 y = lat = c(53.0684756,53.0684756)

googleway::google_elevation(df_locations = df,
                 simplify = TRUE)

RCurl::httpGET('https://api.open-elevation.com/api/v1/lookup?locations=53.899203,-2.6136515',
               .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE))



