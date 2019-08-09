#
#
# Fri Aug  9 16:43:26 -03 2019
#
# Author: Marco Arthur ( arthurpbs at gmail )
# 
# Get data from OSM and plot using leaflet. So we can count and visualize
# Points of Interest
# 
library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(osmdata)
library(memoise)

get_osm_data <- function( city, amenity ) {
  # bound box for city
  bb <- getbb(city, format_out = "sf_polygon")
  # create overpass query for the amenity
  v <- unlist(strsplit(amenity, ':'))
  q <- opq(bb) %>% add_osm_feature(key = v[1], value=v[2])
  # get the data
  return <- osmdata_sf(q, quiet=F)
}

mget_osm <- memoise(get_osm_data)

show_text <- function( value ) { paste( "You chose: ", value)}
show_stats <- function(city, amenity) {
  city_data <- mget_osm(city, amenity)
  paste(city, " has a total of ", amenity, ": ", count(city_data$osm_points))

}

ui <- fluidPage(
  # Application title
  titlePanel("Simple osmdata retrieval and compare"),
  # Select a city
  selectInput(
      # the input id
      "city",
      # the label that will appear above the list
      "Choose a city:",
      # the choices to be made
      list(`São Paulo` = list("Ubatuba SP", "Caraguatatuba SP", "São Sebastião SP"),
           `Rio de Janeiro` = list("Parati RJ", "Angra dos Reis RJ"))
    ),
  
    # the selected value to be refered
    textOutput("place"),
  
  # Select an amenity
  selectInput(
      'amenity', "Choose an amenity:",
      c( "restaurants" = "amenity:restaurant",
         "hostels" = "tourism:hostel",
         "schools" = "amenity:school"
        )
    ),
    textOutput("interest"),
    mainPanel(textOutput("stats_panel")),
    leafletOutput("map", height=500, width='100%')
)

server <- function(input, output) {
  output$place <- renderText({
    show_text(input$city)
  })
  output$interest <- renderText({
    show_text(input$amenity)
  })
  output$stats_panel <- renderText({
    show_stats(input$city,input$amenity)
  })
  output$map <- renderLeaflet( {
    dset <- mget_osm(input$city, input$amenity)
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = dset$osm_points)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
