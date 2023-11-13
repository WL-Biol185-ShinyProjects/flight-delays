library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)
library(shinyWidgets)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with carrier_*

crashes <- readRDS('data/aircraft_crashes.rds')
carrier_carriers <- readRDS('data/carriers.rds')

getting_hijacked <- tabPanel('Getting Hijacked',
    selectInput('selectCarrier',
                'Aircraft Carrier',
                choices = carrier_carriers,
                selected = 'AA'),
    plotOutput('crashes_typePlot'),
    plotOutput('crash_expected_table')
)

getting_hijacked_crashes <- function(input) {
    renderPlot({
        crashes %>%
            filter(OP_CARRIER == input$selectCarrier) %>%
            count(INCIDENT_TYPE) %>%
            ggplot(aes(INCIDENT_TYPE,
                       n,
                       position = 'dodge')) +
                geom_bar(stat = 'identity',
                         position = 'dodge') 
                #I need to fix bar graph to allow multiselect. Problem with count function and can't color code. Adding title crashes the app, idk why. 
                #+
                #ggtitle('REASONS FOR PLANE CRASH') +
                #xlab('INCIDENT TYPE'),
                #ylab('# OF INCIDENTS / CARRIER')
    })
}

crash_expected_table <- function(input) {
    
} 




