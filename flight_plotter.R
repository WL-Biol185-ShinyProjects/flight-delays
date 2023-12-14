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

flight_get_coordinates <- function(name) { flight_coordinates <- flight_airport_names %>%
                                            filter(name == location)
                                            c(flight_coordinates$airport.longitude, flight_coordinates$airport.latitude)
                                          }

flight_plotter <- tabPanel( "Flight Plotter",
                            tags$style(type = "text/css", ".fas { font-size: 10px; } .btn { padding: 2px 10px !important; margin-bottom: 5px; } html, body { width: 100%; height: 150% } #controls { background-color: rgba(255,255,255,.65); padding: 30px; cursor: move; transition: 1s; } #controls:hover { background-color: rgba(255,255,255,.95); transition: 1s; } .wrapper { position: fixed; top: 100px; left: 0; right: 0; bottom: 0; top: 0; overflow: hidden; padding: 0; } .leaflet-control-container { display: none; } em { font-size: 11px }"),
                              div( class = "wrapper",
                                  leafletOutput("flight_route_map", width = "100%", height = "100%" ),
                                  absolutePanel(id = "controls", fixed = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
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
                                                                          , selected = "Minneapolis-St Paul Intl (MSP)"
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
                                                                          , selected = "Minneapolis-St Paul Intl (MSP)"
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
                                                                          , selected = "Los Angeles International (LAX)"
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
                                                                          , selected = "Honolulu International (HNL)"
                                                                          , options  = list( create = FALSE
                                                                                             , placeholder = "Search..."
                                                                                             , maxItems = "1"
                                                                                             , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                             , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                          )
                                                          )
                                                          ),
                                          h4("Where to sit", style = "display: inline;"),
                                          actionButton("seats", "", icon = icon("info")),
                                          uiOutput("flight_seat"),
                                          br(),
                                          checkboxInput("flight_weather", "Show Weather Radar", value = FALSE)
                                                        )
                          )
)
                          

flight_route_map <- function(input)  {
                                       renderLeaflet({
                                                if (input$flight_type == "nonstop" && input$flight_weather == FALSE) {
                                                  if (isTruthy(input$flight_origin_nonstop) & isTruthy(input$flight_destination_nonstop)) {
                                                    gcIntermediate(flight_get_coordinates(input$flight_origin_nonstop), flight_get_coordinates(input$flight_destination_nonstop),     
                                                        n=100,                
                                                        addStartEnd = TRUE,
                                                        sp=TRUE) %>%
                                                  
                                                    leaflet() %>%
                                                    addTiles() %>%
                                                    addPolylines()
                                                  } else {
                                                    leaflet() %>% 
                                                      addTiles() %>%
                                                      setView(lng = -96.25, lat = 39.50, zoom = 5)
                                                  }
                                                }
                                                    
                                                else if (input$flight_type == "nonstop" && input$flight_weather == TRUE) {
                                                  if (isTruthy(input$flight_origin_nonstop) & isTruthy(input$flight_destination_nonstop)) {
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
                                                  } else {
                                                    leaflet() %>% 
                                                      addTiles() %>%
                                                      setView(lng = -96.25, lat = 39.50, zoom = 5)
                                                  }
                                                }
                                                    
                                                else if (input$flight_type == "onestop" && input$flight_weather == FALSE) {
                                                  if (isTruthy(input$flight_origin_onestop) & isTruthy(input$flight_waypoint_onestop) & isTruthy(input$flight_destination_onestop)) {
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
                                                  } else {
                                                    leaflet() %>% 
                                                      addTiles() %>%
                                                      setView(lng = -96.25, lat = 39.50, zoom = 5)
                                                  }
                                                }
                                                        
                                                else if (input$flight_type == "onestop" && input$flight_weather == TRUE) {
                                                  if (isTruthy(input$flight_origin_onestop) & isTruthy(input$flight_waypoint_onestop) & isTruthy(input$flight_destination_onestop)) {
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
                                                  } else {
                                                    leaflet() %>% 
                                                      addTiles() %>%
                                                      setView(lng = -96.25, lat = 39.50, zoom = 5)
                                                  }
                                                }
                                                
                                                else if (input$flight_type == "twostop" && input$flight_weather == FALSE) {
                                                  if (isTruthy(input$flight_origin_twostop) & isTruthy(input$flight_waypoint_1_twostop) & isTruthy(input$flight_waypoint_2_twostop) & isTruthy(input$flight_destination_twostop)) {
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
                                                  } else {
                                                    leaflet() %>% 
                                                      addTiles() %>%
                                                      setView(lng = -96.25, lat = 39.50, zoom = 5)
                                                  }
                                                }
                                                else if (input$flight_type == "twostop"  && input$flight_weather == TRUE) {
                                                  if (isTruthy(input$flight_origin_twostop) & isTruthy(input$flight_waypoint_1_twostop) & isTruthy(input$flight_waypoint_2_twostop) & isTruthy(input$flight_destination_twostop)) {
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
                                                  } else {
                                                    leaflet() %>% 
                                                      addTiles() %>%
                                                      setView(lng = -96.25, lat = 39.50, zoom = 5)
                                                  }
                                                }
                                             })
}

flight_seat <- function(input) { renderUI({
                                    if (input$flight_type == "nonstop") {
                                      if (isTruthy(input$flight_origin_nonstop) & isTruthy(input$flight_destination_nonstop)) {
                                        flight_port_orig <- flight_get_coordinates(input$flight_origin_nonstop)
                                        flight_port_dest <- flight_get_coordinates(input$flight_destination_nonstop)
                                        
                                        # if we're flying more north/south than east/west
                                        if (abs(flight_port_dest[2]-flight_port_orig[2]) > abs(flight_port_dest[1]-flight_port_orig[1])) {
                                          # if flying north
                                          if (flight_port_dest[2] > flight_port_orig[2]) {
                                            tagList(
                                              p(strong("Flight 1 (morning): "), "Left side"),
                                              p(strong("Flight 1 (afternoon): "), "Right side")
                                            )
                                          }
                                          else {
                                            tagList(
                                              p(strong("Flight 1 (morning): "), "Right side"),
                                              p(strong("Flight 1 (afternoon): "), "Left side")
                                            )
                                          }
                                          
                                        } 
                                        else {
                                          # if flying east
                                          if (flight_port_dest[1] > flight_port_orig[1]) {
                                            tagList(
                                              p(strong("Flight 1: "), "Left side")
                                            )
                                          }
                                          else {
                                            tagList(
                                              p(strong("Flight 1: "), "Right side")
                                            )
                                          }
                                        }
                                      }
                                    }
                                      
                                    else if (input$flight_type == "onestop") {
                                      if (isTruthy(input$flight_origin_onestop) & isTruthy(input$flight_waypoint_onestop) & isTruthy(input$flight_destination_onestop)) {
                                        flight_port_orig <- flight_get_coordinates(input$flight_origin_onestop)
                                        flight_port_mid  <- flight_get_coordinates(input$flight_waypoint_onestop)
                                        flight_port_dest <- flight_get_coordinates(input$flight_destination_onestop)
                                        
                                        writing <- tagList()
                                        
                                        # flight 1
                                        
                                        if (abs(flight_port_mid[2]-flight_port_orig[2]) > abs(flight_port_mid[1]-flight_port_orig[1])) {
                                          # if flying north
                                          if (flight_port_mid[2] > flight_port_orig[2]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (morning): "), "Left side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (afternoon): "), "Right side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (morning): "), "Right side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (afternoon): "), "Left side"))
                                          }
                                          
                                        } 
                                        else {
                                          # if flying east
                                          if (flight_port_mid[1] > flight_port_orig[1]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1: "), "Left side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1: "), "Right side"))
                                          }
                                        }
                                        
                                        # flight 2
                                        
                                        if (abs(flight_port_dest[2]-flight_port_mid[2]) > abs(flight_port_dest[1]-flight_port_mid[1])) {
                                          # if flying north
                                          if (flight_port_dest[2] > flight_port_mid[2]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (morning): "), "Left side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (afternoon): "), "Right side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (morning): "), "Right side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (afternoon): "), "Left side"))
                                          }
                                          
                                        } 
                                        else {
                                          # if flying east
                                          if (flight_port_dest[1] > flight_port_mid[1]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2: "), "Left side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2: "), "Right side"))
                                          }
                                        }
                                        
                                        
                                        writing
                                      }
                                    }
                                   
                                    else if (input$flight_type == "twostop") {
                                      if (isTruthy(input$flight_origin_twostop) & isTruthy(input$flight_waypoint_1_twostop) & isTruthy(input$flight_waypoint_2_twostop) & isTruthy(input$flight_destination_twostop)) {
                                        flight_port_orig   <- flight_get_coordinates(input$flight_origin_twostop)
                                        flight_port_mid_1  <- flight_get_coordinates(input$flight_waypoint_1_twostop)
                                        flight_port_mid_2  <- flight_get_coordinates(input$flight_waypoint_2_twostop)
                                        flight_port_dest   <- flight_get_coordinates(input$flight_destination_twostop)
                                        
                                        writing <- tagList()
                                        
                                        # flight 1
                                        
                                        if (abs(flight_port_mid_1[2]-flight_port_orig[2]) > abs(flight_port_mid_1[1]-flight_port_orig[1])) {
                                          # if flying north
                                          if (flight_port_mid_1[2] > flight_port_orig[2]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (morning): "), "Left side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (afternoon): "), "Right side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (morning): "), "Right side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1 (afternoon): "), "Left side"))
                                          }
                                          
                                        } 
                                        else {
                                          # if flying east
                                          if (flight_port_mid_1[1] > flight_port_orig[1]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1: "), "Left side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 1: "), "Right side"))
                                          }
                                        }
                                        
                                        # flight 2
                                        
                                        if (abs(flight_port_mid_2[2]-flight_port_mid_1[2]) > abs(flight_port_mid_2[1]-flight_port_mid_1[1])) {
                                          # if flying north
                                          if (flight_port_mid_2[2] > flight_port_mid_1[2]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (morning): "), "Left side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (afternoon): "), "Right side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (morning): "), "Right side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2 (afternoon): "), "Left side"))
                                          }
                                          
                                        } 
                                        else {
                                          # if flying east
                                          if (flight_port_mid_2[1] > flight_port_mid_1[1]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2: "), "Left side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 2: "), "Right side"))
                                          }
                                        }
                                        
                                        # flight 3
                                        
                                        if (abs(flight_port_dest[2]-flight_port_mid_2[2]) > abs(flight_port_dest[1]-flight_port_mid_2[1])) {
                                          # if flying north
                                          if (flight_port_dest[2] > flight_port_mid_2[2]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 3 (morning): "), "Left side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 3 (afternoon): "), "Right side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 3 (morning): "), "Right side"))
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 3 (afternoon): "), "Left side"))
                                          }
                                          
                                        } 
                                        else {
                                          # if flying east
                                          if (flight_port_dest[1] > flight_port_mid_2[1]) {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 3: "), "Left side"))
                                          }
                                          else {
                                            writing <- tagAppendChild(writing, tags$p(strong("Flight 3: "), "Right side"))
                                          }
                                        }
                                        
                                        writing
                                      }
                                    }
                                 })
                                }