
lat <-  40.4424
lon <- -3.732
dist <- 2000

feature_tags <- c("amenity / restaurant", "amenity / school")
feature_tags <- feature_tags[1]

x <- getOsmData2(long, lat, 2000, feature_tags = feature_tags[1])


library(OpenStreetMap)




x <- opq_enclosing(lon = long, lat = lat) %>% 
  osmdata_sf()


x2 <- opq_around(lon = long, lat = lat, radius = 2000,
                 key = names(keys[2]),
                 value = keys[2]) %>% 
  osmdata_sf()


polygons <- c(x, x2)
polygons <- polygons$osm_polygons

library(mapdeck)

polygons <- x$osm_polygons

mapdeck(
  style = mapdeck_style("light"), 
  location = c(long, lat),
  zoom = 12
) %>%
  add_sf(layer_id = "osmdata",
         data = polygons, 
         fill_opacity = 1,
         fill_colour = "#ee6055",
         update_view = T,
         tooltip = "name"
  )  

