library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)
library(shinyTime)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with delay_*
# 

delay_airports <- readRDS("data/airports.rds")
delay_carriers <- readRDS("data/carriers.rds")

delay_get_coordinates <- function(code) { d <- delay_airports %>%
                                                filter(iata == code)
                                          c(d$airport.longitude, d$airport.latitude)
                                        }

delay_predictor <- tabPanel( "Delay Predictor"
                           , h2("Delay Prediction")
                           , fluidRow(
                               column( 3
                                     , selectizeInput( inputId  = "delay_origin"
                                                     , label    = "Origin Airport"
                                                     , choices  = unique(delay_airports$iata)
                                                     , selected = "PIT"
                                                     , options  = list( create = FALSE
                                                                      , placeholder = "Search..."
                                                                      , maxItems = "1"
                                                                      , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                      , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                      )
                                                      )
                                     ),
                               column( 3
                                     , selectizeInput( inputId  = "delay_dest"
                                                     , label    = "Destination Airport"
                                                     , choices  = unique(delay_airports$iata)
                                                     , selected = "ATL"
                                                     , options  = list( create = FALSE
                                                                      , placeholder = "Search..."
                                                                      , maxItems = "1"
                                                                      , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                      , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                      )
                                                     )
                                     ),
                               column( 3
                                     , timeInput( inputId = "delay_time"
                                                , label   = "Time of Departure"
                                                , value   = strptime("12:00", format = "%H:%M")
                                                , seconds = FALSE
                                                )
                                     ),
                               column( 2
                                     , selectizeInput( inputId  = "delay_carrier"
                                                     , label    = "Carrier"
                                                     , choices  = delay_carriers
                                                     , selected = "DL"
                                                     , options  = list( create = FALSE
                                                                      , placeholder = "Search..."
                                                                      , maxItems = "1"
                                                                      , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                      , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                      )
                                                     )
                               )
                             )
                             , br()
                             , fluidRow(
                                 column(7
                                       , h4("Flight Path")
                                       , leafletOutput("delay_predictor_map")
                                       ),
                                 column(5
                                       , h4("Delay Statistics")
                                       , tableOutput("delay_expected_table")
                                       )
                               )
                            )

delay_predictor_map <- function(input) { renderLeaflet({
                                          gcIntermediate( delay_get_coordinates(input$delay_origin)
                                                        , delay_get_coordinates(input$delay_dest)
                                                        , n=100
                                                        , addStartEnd=TRUE
                                                        , sp=TRUE
                                                        ) %>% 
                                          leaflet() %>% 
                                          addTiles() %>% 
                                          addPolylines()
                                         })
                                        }

delay_expected_table <- function(input) { renderTable({
                                            delay_orig <- input$delay_origin
                                            delay_dest <- input$delay_dest
                                            delay_time <- as.numeric(strftime(input$delay_time, "%H%m"))
                                            delay_carrier_table <- readRDS(paste0("data/", input$delay_carrier, "_full.rds"))
                                            
                                            delay_df <- delay_carrier_table %>% 
                                                          filter((ORIGIN == delay_orig & DEST == delay_dest) & (delay_time > (CRS_DEP_TIME - 1000) & delay_time < (CRS_DEP_TIME + 1000)))
                                            
                                            delay_ontime <- 100 * (nrow(filter(delay_df, ARR_DELAY < -8))/nrow(delay_df)) # chooses flights with delays greater than 8 minutes
                                            delay_cancelled <- 100 * ((nrow(filter(delay_df, CANCELLED == 1)))/nrow(delay_df))
                                            delay_worst <- min(delay_df$ARR_DELAY)
                                            
                                            delay_return <- matrix()
                                            delay_return <- rbind(delay_return, c("On-time Performance", format(delay_ontime, digits = 2)))
                                            delay_return <- rbind(delay_return, c("Cancellation History", format(delay_cancelled, digits = 2)))
                                            delay_return <- rbind(delay_return, c("Worst Recorded Delay", delay_worst))
                                            
                                            delay_return
                                          })
                                        }
