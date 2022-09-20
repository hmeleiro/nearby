#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapdeck)
library(osmdata)
library(sf)
library(dplyr)

mb_token = 'pk.eyJ1IjoiaG1lbGVpcm9zIiwiYSI6ImNqcGk0OG1nazEwMzMzcXBmbnAydzUxYnEifQ.mznrc0j3W0nyqnTw8p6JqA'
set_token(mb_token)


getOsmData <- function(circle_sf) {
  q <- opq(bbox = st_bbox(circle_sf)) %>%
    add_osm_feature("amenity", "cinema")
  
  data <- osmdata_sf(q) 
  return(data) 
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$script(src = "getGeoLoc.js"),
  
  h1("NearBy"),
  
  mapdeckOutput(outputId = "map", height = "800px")
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  data_sf <- reactive({
    point_sf <- tibble(lat = input$lat, long = input$long)
    point_sf <- st_as_sf(point_sf, coords = c("long", "lat"), crs = 4326)
    circle_sf <- st_buffer(point_sf, dist = 10000)
    
    list("point_sf" = point_sf, "circle_sf" = circle_sf)
  })
  
  dataOSM <- reactive({
    getOsmData(data_sf()$circle_sf)
  })
  
  output$map <- renderMapdeck({
    # Note that our mb_token value is read in from global.R
    mapdeck(
      style = mapdeck_style("light"), 
      # 55 degrees off of north.
      bearing = -55, 
      location = c(input$long, input$lat),
      zoom = 12
    )
  })
  
  observeEvent({input$geolocation}, {
    req(input$geolocation)
    print(input$geolocation)
    print(nrow(dataOSM()$osm_polygons))
    
    
    userLoc <- data_sf()$point_sf
    polygons <- dataOSM()$osm_polygons %>% 
      mutate(
        name = iconv(name, from="UTF-8", to="UTF-8"),
        popUp = sprintf(
          "<p class=popup-text>%s</p>
          <p class=popup-text>Type: %s</p>", 
          name, amenity
        )
      )
    
    # MAPBOX
    mapdeck_update(map_id = 'map', session = session) %>%
      # add_sf(
      #   data = data_sf()$circle_sf,
      #   fill_opacity = 0,
      #   stroke_opacity = 1,
      #   stroke_width = 3,
      #   focus_layer = F,
      #   update_view = F
      # ) %>%
      add_sf(
        data =userLoc , 
        radius_min_pixels = 6,
        update_view = F
      ) %>%
      add_sf(
        data = polygons, 
        fill_opacity = 1,
        fill_colour = "#202020",
        # fill_colour = "color",
        update_view = F,
        tooltip = "popUp"
      )   
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
