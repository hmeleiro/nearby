library(shiny)
library(mapdeck)
library(osmdata)
library(sf)
library(dplyr)

mb_token = 'pk.eyJ1IjoiaG1lbGVpcm9zIiwiYSI6ImNqcGk0OG1nazEwMzMzcXBmbnAydzUxYnEifQ.mznrc0j3W0nyqnTw8p6JqA'
set_token(mb_token)


getOsmData <- function(circle_sf, feature, tag) {
  
  tryCatch({
    q <- opq(bbox = st_bbox(circle_sf)) %>%
      add_osm_feature(feature, tag)
    
    message("Searching in OSM...")
    data <- osmdata_sf(q) 
    message("Got the results...")
  }, error = function(e) {
    data <- NULL
  })
  
  return(data) 
}

features <- readRDS("data/OSMfeatures.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$script(src = "getGeoLoc.js"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  h1("NearBy"),
  
  shiny::inputPanel(
    div(
      selectInput(
        inputId = "feature", 
        label = "Choose feature",
        choices = unique(features$features), 
        multiple = T, selectize = T),
      
      selectInput(
        inputId = "tag", 
        label = "Choose tag",choices = NULL,
        multiple = T, selectize = T),
      
      sliderInput(inputId = "dist", label = "At a distance of...",
                  min = 1000, max = 100000, 
                  animate = T, sep = ",", 
                  post = "m", value = 2000),
      actionButton(inputId = "SearchBtn", label = "Search"),
      class = "input-entries"
    ),

  ),
  
  mapdeckOutput(outputId = "map", height = "800px")
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  startFeature <- reactive({
    sample(unique(features$features), 1)
  })
  
  observe(
    updateSelectizeInput(
      session = session,
      inputId = "feature",
      selected = startFeature(),
      # selected = NULL,
      options = list(
        create = F,
        maxItems = 1,
        placeholder = 'Search an OSM feature',
        hideSelected = T
      )
    )
  )
  
  observe({
    req(input$feature)
    updateSelectizeInput(
      session = session,
      inputId = "tag",
      choices = features$tags[features$features == input$feature],
      options = list(
        create = F,
        maxItems = 3,
        placeholder = 'Type an OSM tag',
        hideSelected = T,
        closeAfterSelect = T
        
      )
    )
  }
  
  )
  
  data_sf <- reactive({
    req(input$geolocation)
    point_sf <- tibble(lat = input$lat, long = input$long)
    point_sf <- st_as_sf(point_sf, coords = c("long", "lat"), crs = 4326)
    circle_sf <- st_buffer(point_sf, dist = input$dist)
    
    list("point_sf" = point_sf, "circle_sf" = circle_sf)
  })
  
  dataOSM <- reactive({
    req(input$tag)
    getOsmData(data_sf()$circle_sf, input$feature, input$tag)
  })
  
  output$map <- renderMapdeck({
    userLoc <- data_sf()$point_sf
    
    # Note that our mb_token value is read in from global.R
    mapdeck(
      style = mapdeck_style("light"), 
      # bearing = -55, 
      location = c(input$long, input$lat),
      zoom = 16
    ) %>%
      add_sf(
        data = userLoc , 
        radius_min_pixels = 6,
        update_view = F
      )
  })
  
  observeEvent({input$tag}, {
    req(input$geolocation & nrow(dataOSM()$osm_polygons) > 0)
    
    print( nrow(dataOSM()$osm_polygons) )
    polygons <- dataOSM()$osm_polygons %>% 
      mutate(
        name = iconv(name, from="UTF-8", to="UTF-8"),
        popUp = sprintf(
          "<p class=popup-text>%s (%s)</p>", 
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
      # )  %>%
      add_sf(
        data = polygons, 
        fill_opacity = 1,
        fill_colour = "#202020",
        # fill_colour = "color",
        update_view = T,
        tooltip = "popUp"
      )   
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
