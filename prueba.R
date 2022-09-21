lat <- 40.416729
long <- -3.703339

library(smoothr)

point_sf <- tibble(lat = lat, long = long)
point_sf <- st_as_sf(point_sf, coords = c("long", "lat"), crs = 4326)
circle_sf <- st_buffer(point_sf, dist = 1000)


data <- getOsmData(circle_sf, tag = c("bank", "arts_centre", "atm"))


feature_tags <- c("amenity / restaurant", "amenity / bank")
feature_tags <- c("shop / organic")
x <- getOsmData(circle_sf, feature_tags)



x$osm_points %>% glimpse()

x$osm_polygons$name
x$osm_polygons$alt_name

x$osm_points %>% select(contains("name")) %>% View()


x$osm_points$name[x$osm_points$name %in% x$osm_polygons$name]

x$osm_polygons %>% select(contains("name")) %>% View()

mapdeck(
  style = mapdeck_style("light"), 
  location = c(long, lat),
  zoom = 20
) %>%
  add_sf(
    data = smooth(circle_sf, "ksmooth", smoothness = 4) , 
    fill_opacity = 0.1,
    stroke_opacity = 180,
    stroke_width = 10,
    stroke_colour = "black",
    # radius_min_pixels = 6,
    update_view = T
  )




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
