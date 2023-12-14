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
                            , tags$style(type = "text/css", "html, body { width: 100%; height: 150% } #controls { background-color: rgba(255,255,255,.65); padding: 30px; cursor: move; transition: .7s; } #controls:hover { background-color: rgba(255,255,255,.95); transition: .7s; } .wrapper { position: fixed; top: 100px; left: 0; right: 0; bottom: 0; top: 0; overflow: hidden; padding: 0; } .leaflet-control-container { display: none; } em { font-size: 11px }")
                            , div( class = "wrapper"
                                 , leafletOutput("delay_predictor_map", width = "100%", height = "100%" )
                                 , absolutePanel( id = "controls", fixed = TRUE, top = 60, right = 20, left = "auto", bottom = "auto", width = 330, height = "auto"
                                     , h4("Flight Options")
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
                                     , selectizeInput( inputId  = "delay_dest"
                                                           , label    = "Destination Airport"
                                                           , choices  = unique(delay_airports$iata)
                                                           , selected = "SFO"
                                                           , options  = list( create = FALSE
                                                                            , placeholder = "Search..."
                                                                            , maxItems = "1"
                                                                            , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                            , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                            )
                                                      )
                                     , selectizeInput( inputId  = "delay_carrier"
                                                               , label    = "Carrier"
                                                               , choices  = delay_carriers
                                                               , selected = "UA"
                                                               , options  = list( create = FALSE
                                                                                  , placeholder = "Search..."
                                                                                  , maxItems = "1"
                                                                                  , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                                  , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                               )
                                                      )
                                     , timeInput( inputId = "delay_time"
                                                      , label   = "Time of Departure"
                                                      , value   = strptime("08:15", format = "%H:%M")
                                                      , seconds = FALSE
                                                )
                                     , br()
                                     , h4("Delay Statistics")
                                     , uiOutput("delay_expected_table")
                                   )
                                 )
                          )

delay_predictor_map <- function(input) {  renderLeaflet({
                                            if (as.numeric(strftime(input$delay_time, "%H%m")) > 1800) {
                                              if (isTruthy(input$delay_origin) & isTruthy(input$delay_dest)) { 
                                                  gcIntermediate( delay_get_coordinates(input$delay_origin)
                                                                  , delay_get_coordinates(input$delay_dest)
                                                                  , n=100
                                                                  , addStartEnd=TRUE
                                                                  , sp=TRUE
                                                  ) %>% 
                                                  leaflet() %>%
                                                  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012) %>% 
                                                  addPolylines()
                                              } else {
                                                leaflet() %>% 
                                                  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012) %>%
                                                  setView(lng = -96.25, lat = 39.50, zoom = 5)
                                              }
                                            } else {
                                              if (isTruthy(input$delay_origin) & isTruthy(input$delay_dest)) { 
                                                gcIntermediate( delay_get_coordinates(input$delay_origin)
                                                                , delay_get_coordinates(input$delay_dest)
                                                                , n=100
                                                                , addStartEnd=TRUE
                                                                , sp=TRUE
                                                ) %>% 
                                                  leaflet() %>% 
                                                  addTiles() %>%
                                                  addPolylines()
                                              } else {
                                                leaflet() %>% 
                                                  addTiles() %>%
                                                  setView(lng = -96.25, lat = 39.50, zoom = 5)
                                              }
                                            }
                                          })
                                        }

delay_expected_table <- function(input) { renderUI({
                                            delay_orig <- input$delay_origin
                                            delay_dest <- input$delay_dest
                                            delay_time <- as.numeric(strftime(input$delay_time, "%H%m"))
                                            
                                            if (isTruthy(input$delay_carrier)) {
                                              delay_carrier_table <- readRDS(paste0("data/", input$delay_carrier, "_full.rds"))
                                            } else {
                                              tagList(p("Select an applicable carrier to view statistics"))
                                            }
                                            
                                            delay_df <- delay_carrier_table %>% 
                                                          filter((ORIGIN == delay_orig & DEST == delay_dest) & (delay_time > (CRS_DEP_TIME - 100) & delay_time < (CRS_DEP_TIME + 100)))
                                            
                                            delay_ontime <- 100 * (nrow(filter(delay_df, ARR_DELAY > -12))/nrow(delay_df)) # chooses flights with delays greater than 12 minutes
                                            delay_cancelled <- 100 * ((nrow(filter(delay_df, CANCELLED == 1)))/nrow(delay_df))
                                            delay_worst <- abs(min(delay_df$ARR_DELAY, na.rm = TRUE))
                                            
                                            if (!is.nan(delay_ontime)) {
                                              tagList(
                                                p(strong("On-time Performance:"), format(delay_ontime, digits = 2), "%"),
                                                p(strong("Cancellation History:"), format(delay_cancelled, digits = 2), "%"),
                                                p(strong("Worst Delay of 2018:"), delay_worst, "minutes"),
                                                em("*On-time performance is calculated from arrival delays greater than 12 minutes")
                                              )
                                            } else {
                                              tagList(
                                                p(strong("Route does not exist for the carrier or time selected."))
                                              )
                                            }
                                          })
                                        }
