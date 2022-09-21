library(shiny)
library(mapdeck)
library(osmdata)
library(sf)
library(dplyr)
library(smoothr)

mb_token = 'pk.eyJ1IjoiaG1lbGVpcm9zIiwiYSI6ImNqcGk0OG1nazEwMzMzcXBmbnAydzUxYnEifQ.mznrc0j3W0nyqnTw8p6JqA'
set_token(mb_token)


source("R/getOsmData.R", encoding = "UTF-8")

features <- readRDS("data/OSMfeatures.rds") %>%
  transmute(group = features, label = tags, value = paste0(group , " / " ,tags))
# Get the kingdom group tags of each species
lookup_groups <- lapply(unique(features$group), function(x) {
  list(value = as.character(x), label = as.character(x))
})

# Define UI for application that draws a histogram
ui <- fillPage(padding = 15,title = "Nearby",
               
               tags$script(src = "getGeoLoc.js"),
               tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
               ),
               
               
               h1("Nearby"),
               # shiny::inputPanel(
               div(
                 
                 selectInput(
                   inputId = "feature", 
                   label = "Choose feature",
                   choices = NULL,
                   multiple = T, selectize = T),
                 
                 # selectInput(
                 #   inputId = "feature", 
                 #   label = "Choose feature",
                 #   choices = unique(features$features), 
                 #   multiple = T, selectize = T),
                 # 
                 # selectInput(
                 #   inputId = "tag", 
                 #   label = "Choose tag",choices = NULL,
                 #   multiple = T, selectize = T),
                 
                 class= "input-countainer-1"
                 
               ),
               
               div(
                 sliderInput(inputId = "dist", label = "At a distance of...", 
                             min = 1, max = 100, sep = ",", ticks = F,
                             post = "km", value = 2), 
                 class= "input-countainer-2"
               ),
               actionButton(inputId = "SearchBtn", label = "Search"),
               
               
               shiny::mainPanel(
                 mapdeckOutput(outputId = "map", height = "450px"), 
                 width = 12, class = "map-container"
               )
               
               
               
               
               
               
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  startFeature <- reactive({
    sample(unique(features$value), 1)
  })
  
  observe(
    updateSelectizeInput(server = T,
                         session = session,
                         inputId = "feature",
                         selected = startFeature(),
                         
                         choices = features,
                         # selected = NULL,
                         options = list(
                           optgroups = lookup_groups,
                           optgroupField = 'group',
                           searchField = c('group', 'label', 'value'),
                           create = F,
                           maxItems = 3,
                           placeholder = 'Search an OSM feature',
                           hideSelected = T,
                           render = I("{
        item: function(item, escape) {
          return '<div>' + item.group + ': ' + item.label + '<div>';
        },
        option: function(item, escape) {
          return '<div>' + item.label + '<div>';
        }
      }")
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
    circle_sf <- st_buffer(point_sf, dist = input$dist*1000)
    
    list("point_sf" = point_sf, "circle_sf" = smooth(circle_sf, "ksmooth", smoothness = 4) )
  })
  
  dataOSM <- reactive({
    req(input$SearchBtn)

      getOsmData(data_sf()$circle_sf, input$feature)
  })
  
  output$map <- renderMapdeck({
    userLoc <- data_sf()$circle_sf
    
    mapdeck(
      style = mapdeck_style("light"), 
      location = c(input$long, input$lat),
      zoom = 12
    ) %>%
      add_sf(
        layer_id = "userRadio",
        data = userLoc , 
        fill_opacity = 0.1,
        stroke_opacity = 180,
        stroke_width = 10,
        stroke_colour = "black",
        # radius_min_pixels = 6,
        update_view = F
      )
  })
  
  observeEvent({input$SearchBtn}, {
    req(input$geolocation & !is.null(dataOSM()))
    req(!is.null(dataOSM()$osm_polygons) & nrow(dataOSM()$osm_polygons) >0)

    userLoc <- data_sf()$circle_sf
    
    polygons <- dataOSM()$osm_polygons %>% 
      mutate(
        popUp = sprintf(
          "<p class=popup-text>%s</p>", 
          name
        )
      )
    
    # print(head(polygons))
    
    # MAPBOX
    mapdeck_update(map_id = 'map', session = session) %>%
      add_sf(layer_id = "osmdata",
        data = polygons, 
        fill_opacity = 1,
        fill_colour = "#ee6055",
        update_view = T,
        tooltip = "popUp"
      )   
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
