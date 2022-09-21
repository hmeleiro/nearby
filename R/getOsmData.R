getOsmData <- function(circle_sf, feature_tags) {

  feature_tags_split <- NULL
  for (i in 1:length(feature_tags)) {
    tmp <- sprintf('"%s" = "%s"',  
                   strsplit(feature_tags[i], split = " / ")[[1]][1], 
                   strsplit(feature_tags[i], split = " / ")[[1]][2]
                   )
    feature_tags_split <- c(feature_tags_split, tmp)
  }
  

  tryCatch({
    q <- opq(bbox = st_bbox(circle_sf)) %>%
      add_osm_features(feature_tags_split)
    
    
    message("Searching in OSM...")
    data <- osmdata_sf(q) 
    nresults <- nrow(data$osm_polygons)
    message("Got ", nresults, " results...")
    
  }, error = function(e) {
    data <- NULL
  })
  
  return(data)
}
