library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)

source("flight_plotter.R")
source("delay_predictor.R")
source("carrier_performance.R")
source("getting_hijacked.R")

navbarPage("Flights",
           flight_plotter,
           delay_predictor,
           carrier_performance,
           getting_hijacked
          )