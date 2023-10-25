library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)

source("flight_plotter.R")
source("delay_predictor.R")
source("carrier_performance.R")

function(input, output, session) {
  
  output$plot <- delay_predictor_server(input)
  
  output$flight_route_map <- flight_route_map(input)
}