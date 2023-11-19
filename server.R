library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)
library(dplyr)

source("flight_plotter.R")
source("delay_predictor.R")
source("carrier_performance.R")
source("getting_hijacked.R")
source("airplane_statistics.R")

function(input, output, session) {

  output$delay_predictor_map <- delay_predictor_map(input)
  output$delay_expected_table <- delay_expected_table(input)
    
  output$arr_delayPlot <- carrier_performance_arr_delay(input)
  output$delay_typesPlot <- carrier_performance_delay_types(input)
  output$reviewsPlot <- carrier_performance_reviews(input)
  
  output$flight_route_map <- flight_route_map(input)
  
  output$engine_information <- engine_information(input)
  output$aircraft_activity <- aircraft_activity(input)
  output$weight_capacity <- weight_capacity(input)
  output$landing_info <- landing_info(input)
  output$airplane_dimensions <- airplane_dimensions(input)
  output$regulatory <- regulatory(input)
  
  
  observeEvent(input$landingHelp, {
    dialog <- modalDialog( title = "Help"
                            , p("reghkwygituy")
                            , easyClose = TRUE
                            , fade = TRUE
                          )
    showModal(dialog)
  })
  
  output$flight_seat <- flight_seat(input)
  output$crashes_typePlot <- getting_hijacked_crashes(input)
  output$crash_expected_table <- crash_expected_table(input)

}
