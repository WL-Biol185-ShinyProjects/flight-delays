library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)
library(dplyr)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with carrier_*

carrier_carriers <- readRDS('data/carriers.rds')

getting_hijacked <- tabPanel('Crash Data',
    selectizeInput('chooseCarrier',
                   'Aircraft Carrier',
                    choices = carrier_carriers[-13],
                    selected = 'UA'),
    plotOutput('crashes_typePlot'),
    uiOutput('crash_expected_table')
)






