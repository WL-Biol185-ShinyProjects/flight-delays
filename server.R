library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)

source("flight_plotter.R")
source("delay_predictor.R")
source("carrier_performance.R")

function(input, output, session) {
  
  output$delay_predictor_map  <- delay_predictor_map(input)

  observe({ output$delay_expected_table <- delay_expected_table(input) })
  
  output$arr_delayPlot <- carrier_performance_arr_delay(input)
  output$delay_typesPlot <- carrier_performance_delay_types(input)
  output$reviewsPlot <- carrier_performance_reviews(input)
  
  output$flight_route_map <- flight_route_map(input)
}