library(leaflet)
library(tidyverse)
library(shiny)
library(shinyjs)
library(geosphere)
library(ggplot2)
library(dplyr)
library(httr)

source("flight_plotter.R")
source("delay_predictor.R")
source("carrier_performance.R")
source("airplane_statistics.R")
source("about_page.R")

navbarPage("Airborne Analytics",
           flight_plotter,
           delay_predictor,
           carrier_performance,
           airplane_statistics,
           about_page
          )