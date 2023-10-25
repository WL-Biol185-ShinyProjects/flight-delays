library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with flight_*
# 


flight_airport_names <- readRDS("~/flight-delays/data/airports.rds")

flight_get_coordinates <- function(name) {flight_coordinates <- flight_airport_names %>%
                                            filter(name == location)
                                            c(flight_coordinates$airport.longitude, flight_coordinates$airport.latitude)
                                          }

flight_plotter <- tabPanel("Flight Plotter",
                          sidebarLayout(
                              sidebarPanel(
                                    selectInput("flight_origin", "Origin:", 
                                                  choices=flight_airport_names$location
                                                ),
                                      selectInput("flight_destination", "Destination:",
                                                    choices=flight_airport_names$location
                                                  )
                                            ),
                                     mainPanel(plotOutput("flight_route_map"))
                                        )                        
                          )

flight_route_map <- function(input)  {
                                       renderPlot({
                                  
                                            gcIntermediate(flight_get_coordinates(input$flight_origin), flight_get_coordinates(input$flight_destination),      ## longitude, latitude
                                                                n=100,                
                                                                addStartEnd = TRUE,
                                                                sp=TRUE) %>%
                                                   leaflet() %>%
                                                   addTiles() %>%
                                                   addPolylines()
                                                  })
}

