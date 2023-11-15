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

crashes <- readRDS('data/aircraft_crashes.rds')
carrier_carriers <- readRDS('data/carriers.rds')

getting_hijacked <- tabPanel('Getting Hijacked',
    selectizeInput('selectCarrier',
                'Aircraft Carrier',
                choices = carrier_carriers,
                selected = 'AA',
                multiple = TRUE),
    plotOutput('crashes_typePlot'),
    plotOutput('crash_expected_table')
)

getting_hijacked_crashes <- function(input) {
    renderPlot({
        crashes %>%
            filter(OP_CARRIER %in% input$selectCarrier) %>%
            count(INCIDENT_TYPE) %>%
            ggplot(aes_string('INCIDENT_TYPE',
                       'n',
                       fill = 'as.factor(OP_CARRIER)',
                       position = 'dodge')) +
                geom_bar(stat = 'identity',
                         position = 'dodge') +
                labs(title = 'REASONS FOR PLANE CRASH',
                    x = 'INCIDENT TYPE',
                    y = '# OF INCIDENTS / CARRIER')
    })
}

crash_expected_table <- function(input) {
    
} 




