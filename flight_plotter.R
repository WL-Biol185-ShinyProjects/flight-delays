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

flight_plotter <- tabPanel( "Flight Plotter",
                            tags$style(type = "text/css", "html, body { width: 100%; height: 150% } #controls { background-color: rgba(255,255,255,.75); padding: 30px; cursor: move; transition: opacity 500ms 1s; } .wrapper { position: fixed; top: 100px; left: 0; right: 0; bottom: 0; top: 0; overflow: hidden; padding: 0; } .leaflet-control-container { display: none; } em { font-size: 11px }"),
                              div( class = "wrapper",
                                  leafletOutput("flight_route_map", width = "100%", height = "100%" ),
                                  absolutePanel(id = "controls", fixed = TRUE, top = 60, right = "auto", left = 20, bottom = "auto", width = 330, height = "auto",
                                                h4("Flight Information"),
            
                                      selectInput("flight_type", "Flight Type",
                                                  c("Nonstop" = "nonstop", "One Stop" = "onestop", "Two Stop" = "twostop")
                                                  ),
                                          conditionalPanel(
                                                          condition = "input.flight_type == 'nonstop'",
                                                          
                                                          selectizeInput( inputId  = "flight_origin_nonstop"
                                                                          , label    = "Origin Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "John F Kennedy Intl (JFK)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          ),
                                                          selectizeInput( inputId  = "flight_destination_nonstop"
                                                                          , label    = "Destination Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "Los Angeles International (LAX)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          )
                                                          ),
                                          conditionalPanel(
                                                          condition = "input.flight_type == 'onestop'",
                                                          
                                                          selectizeInput( inputId  = "flight_origin_onestop"
                                                                          , label    = "Origin Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "John F Kennedy Intl (JFK)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          ),
                                                          selectizeInput( inputId  = "flight_waypoint_onestop"
                                                                          , label    = "Layover Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "Des Moines International (DEM)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          ),
                                                          selectizeInput( inputId  = "flight_destination_onestop"
                                                                          , label    = "Destination Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "Los Angeles International (LAX)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          )
                                                          ),
                                          conditionalPanel(
                                                          condition = "input.flight_type == 'twostop'",
                                                          
                                                          selectizeInput( inputId  = "flight_origin_twostop"
                                                                          , label    = "Origin Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "John F Kennedy Intl (JFK)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          ),
                                                          selectizeInput( inputId  = "flight_waypoint_1_twostop"
                                                                          , label    = "First Layover Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "Memphis International (MEM)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          ),
                                                          selectizeInput( inputId  = "flight_waypoint_2_twostop"
                                                                          , label    = "Second Layover Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "Des Moines International (DEM)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          ),
                                                          selectizeInput( inputId  = "flight_destination_twostop"
                                                                          , label    = "Destination Airport"
                                                                          , choices  = flight_airport_names$location
                                                                          , selected = "Los Angeles International (LAX)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          )
                                                          ),
                                      checkboxInput("flight_weather", "Show Weather Radar", value = FALSE)
                                                    ),
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

#flight_seat <- function(input) { 
#                                  
#                                  
#                               }
