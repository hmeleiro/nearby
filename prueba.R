lat <- 40.416729
long <- -3.703339


point_sf <- tibble(lat = lat, long = long)
point_sf <- st_as_sf(point_sf, coords = c("long", "lat"), crs = 4326)
circle_sf <- st_buffer(point_sf, dist = 10000)


data <- getOsmData(circle_sf, tag = c("bank", "arts_centre", "atm"))


x <- data$osm_polygons 


features <- available_features()
x <- NULL
for (i in 1:length(features)) {
  message(features[i])
  tmp <- available_tags(features[i])
  
  if(length(tmp) > 0) {
    tmp <- tibble(features = features[i], tags = tmp)
    x <- rbind(x, tmp)
  }

}

saveRDS(x, "data/OSMfeatures.rds")


tags <- available_tags("shop")
library(stringr)
names <- str_replace_all(tags, "_", " ") %>% str_to_title()
names <- if_else(names %in% c("Bbq", "Atm"), toupper(names), names)
names(tags) <- names

saveRDS(tags, "data/amenity_tags.rds")

writeLines(tags, "data/amenity_tags.txt")
readLines("data/amenity_tags.txt")
