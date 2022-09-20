lat <- 40.416729
long <- -3.703339


point_sf <- tibble(lat = lat, long = long)
point_sf <- st_as_sf(point_sf, coords = c("long", "lat"), crs = 4326)
circle_sf <- st_buffer(point_sf, dist = 10000)


data <- getOsmData(circle_sf)


x <- data$osm_polygons 

