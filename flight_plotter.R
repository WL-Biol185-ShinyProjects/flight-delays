library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)
library(maps)
library(dplyr)
library(sp)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with flight_*


flight_airport_names <- readRDS("data/airports.rds")

flight_get_city <- function(name) { d <- delay_airports %>%
                                          filter(name == location)
                                    c(d$city.longitude, d$city.latitude)
                                  }

flight_get_coordinates <- function(name) { flight_coordinates <- flight_airport_names %>%
                                            filter(name == location)
                                            c(flight_coordinates$airport.longitude, flight_coordinates$airport.latitude)
                                          }

flight_plotter <- tabPanel("Flight Plotter",
                          sidebarLayout(
                              sidebarPanel(
                                selectInput("flight_type", "Flight Type",
                                            c("Nonstop" = "nonstop", "One Stop" = "onestop", "Two Stop" = "twostop")
                                            ),
                                conditionalPanel(
                                                condition = "input.flight_type == 'nonstop'",
                                                selectInput("flight_origin_nonstop", "Origin:", 
                                                              choices=flight_airport_names$location),
                                                selectInput("flight_destination_nonstop", "Destination:",
                                                              choices=flight_airport_names$location),
                                                ),
                                conditionalPanel(
                                                condition = "input.flight_type == 'onestop'",
                                                selectInput("flight_origin_onestop", "Origin",
                                                            choices=flight_airport_names$location),
                                                selectInput("flight_waypoint_onestop", "Waypoint",
                                                            choices=flight_airport_names$location),
                                                selectInput("flight_destination_onestop", "Destination",
                                                            choices=flight_airport_names$location),
                                                ),
                                conditionalPanel(
                                                condition = "input.flight_type == 'twostop'",
                                                selectInput("flight_origin_twostop", "Origin",
                                                            choices=flight_airport_names$location),
                                                selectInput("flight_waypoint_1_twostop", "Waypoint 1",
                                                            choices=flight_airport_names$location),
                                                selectInput("flight_waypoint_2_twostop", "Waypoint 2",
                                                            choices=flight_airport_names$location),
                                                selectInput("flight_destination_twostop", "Destination",
                                                            choices=flight_airport_names$location),
                                                ),
                                checkboxInput("flight_weather", "Show Weather Radar", value = FALSE)
                                           ),
                                     mainPanel(leafletOutput("flight_route_map")
                                                )
                                        )
                          )

flight_route_map <- function(input)  {
                                       renderLeaflet({
                                  
                                                    if (input$flight_type == "nonstop" && input$flight_weather == FALSE) {
                                                    
                                                      gcIntermediate(flight_get_coordinates(input$flight_origin_nonstop), flight_get_coordinates(input$flight_destination_nonstop),     
                                                          n=100,                
                                                          addStartEnd = TRUE,
                                                          sp=TRUE) %>%
              
                                                      leaflet() %>%
                                                      addTiles() %>%
                                                      addPolylines()
                                                    }
                                         
                                                    else if (input$flight_type == "nonstop" && input$flight_weather == TRUE) {
                                                      
                                                      gcIntermediate(flight_get_coordinates(input$flight_origin_nonstop), flight_get_coordinates(input$flight_destination_nonstop),     
                                                                     n=100,                
                                                                     addStartEnd = TRUE,
                                                                     sp=TRUE) %>%
                                                        
                                                      leaflet() %>%
                                                      addTiles() %>%
                                                      addPolylines() %>%
                                                      addWMSTiles(
                                                        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
                                                        layers = "nexrad-n0r-900913",
                                                        options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                                        attribution = "Weather data © 2023 IEM Nexrad")
                                                      
                                                    }
                                         
                                                    else if (input$flight_type == "onestop" && input$flight_weather == FALSE) {
                                                      inter1 <- gcIntermediate(flight_get_coordinates(input$flight_origin_onestop), flight_get_coordinates(input$flight_waypoint_onestop),     
                                                              n=100,                
                                                              addStartEnd = TRUE,
                                                              sp=TRUE)
                                                      inter2 <- gcIntermediate(flight_get_coordinates(input$flight_waypoint_onestop), flight_get_coordinates(input$flight_destination_onestop),     
                                                              n=100,                
                                                              addStartEnd = TRUE,
                                                              sp=TRUE)
                                                      inters <- c(inter1, inter2)
                                                      
                                                      ll0 <- lapply( inters , function(x) `@`(x , "lines") )
                                                      ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
                                                      Sl <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 ) ) )
                                                      
                                                      leaflet(Sl) %>% 
                                                      addTiles() %>% 
                                                      addPolylines()
                                                    }
                                         
                                                    else if (input$flight_type == "onestop" && input$flight_weather == TRUE) {
                                                      inter1 <- gcIntermediate(flight_get_coordinates(input$flight_origin_onestop), flight_get_coordinates(input$flight_waypoint_onestop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inter2 <- gcIntermediate(flight_get_coordinates(input$flight_waypoint_onestop), flight_get_coordinates(input$flight_destination_onestop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inters <- c(inter1, inter2)
                                                      
                                                      ll0 <- lapply( inters , function(x) `@`(x , "lines") )
                                                      ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
                                                      Sl <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 ) ) )
                                                      
                                                      leaflet(Sl) %>% 
                                                      addTiles() %>% 
                                                      addPolylines() %>%
                                                      addWMSTiles(
                                                        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
                                                        layers = "nexrad-n0r-900913",
                                                        options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                                        attribution = "Weather data © 2023 IEM Nexrad")
                                                    }
                                         
                                                    else if (input$flight_type == "twostop" && input$flight_weather == FALSE) {
                                                      inter1 <- gcIntermediate(flight_get_coordinates(input$flight_origin_twostop), flight_get_coordinates(input$flight_waypoint_1_twostop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inter2 <- gcIntermediate(flight_get_coordinates(input$flight_waypoint_1_twostop), flight_get_coordinates(input$flight_waypoint_2_twostop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inter3 <- gcIntermediate(flight_get_coordinates(input$flight_waypoint_2_twostop), flight_get_coordinates(input$flight_destination_twostop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inters <- c(inter1, inter2, inter3)
                                                      
                                                      ll0 <- lapply( inters , function(x) `@`(x , "lines") )
                                                      ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
                                                      Sl <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 ) ) )
                                                      
                                                      leaflet(Sl) %>% 
                                                      addTiles() %>% 
                                                      addPolylines()
                                                    }
                                                    else if (input$flight_type == "twostop"  && input$flight_weather == TRUE) {
                                                      inter1 <- gcIntermediate(flight_get_coordinates(input$flight_origin_twostop), flight_get_coordinates(input$flight_waypoint_1_twostop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inter2 <- gcIntermediate(flight_get_coordinates(input$flight_waypoint_1_twostop), flight_get_coordinates(input$flight_waypoint_2_twostop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inter3 <- gcIntermediate(flight_get_coordinates(input$flight_waypoint_2_twostop), flight_get_coordinates(input$flight_destination_twostop),     
                                                                               n=100,                
                                                                               addStartEnd = TRUE,
                                                                               sp=TRUE)
                                                      inters <- c(inter1, inter2, inter3)
                                                      
                                                      ll0 <- lapply( inters , function(x) `@`(x , "lines") )
                                                      ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
                                                      Sl <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 ) ) )
                                                      
                                                      leaflet(Sl) %>% 
                                                      addTiles() %>% 
                                                      addPolylines() %>%
                                                      addWMSTiles(
                                                        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
                                                        layers = "nexrad-n0r-900913",
                                                        options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                                        attribution = "Weather data © 2023 IEM Nexrad")
                                                    }
                                             })
}

#flight_seat <- function(input) {  if (input$flight_type == "onestop") {
#                                    flight_city_orig <- flight_get_city(input$flight_origin_onestop)
#                                    flight_city_dest <- flight_get_city(input$flight_waypoint_onestop)
#                                    
#                                    flight_port_orig <- flight_get_coordinates(input$flight_origin_onestop)
#                                    flight_port_dest <- flight_get_coordinates(input$flight_waypoint_onestop)
#                                    
#                                    
#                                    # if flying west
#                                    if (flight_city_orig[2] < flight_city_dest[1]) {
#                                      
#                                    } 
#                                    
#                                    #if flying east
#                                    else if (flight_city_orig[1] < flight_city_dest[2]) {
#                                      
#                                    }
#                                  }
#  
#                                  else if (input$flight_type == "twostop") {
#                                    flight_city_orig <- flight_get_city(input$flight_origin_twostop)
#                                    flight_city_mid  <- flight_get_city(input$flight_waypoint_1_twostop)
#                                    flight_city_dest <- flight_get_city(input$flight_waypoint_2_twostop)
#                                    
#                                    flight_port_orig <- flight_get_coordinates(input$flight_origin_twostop)
#                                    flight_port_mid  <- flight_get_coordinates(input$flight_waypoint_1_twostop)
#                                    flight_port_dest <- flight_get_coordinates(input$flight_waypoint_2_twostop)
#                                  }
#                                  
#                               }
