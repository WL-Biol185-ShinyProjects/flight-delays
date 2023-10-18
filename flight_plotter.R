library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with flight_*
# 

flight_plotter <- tabPanel("Flight Plotter")