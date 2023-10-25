library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with carrier_*
# 
arr_delays <- readRDS('data/arr_delays.rds')
reviews <- readRDS('data/reviews.rds')
delay_types <- readRDS('data/delay_types.rds')

carrier_performance <- tabPanel("Carrier Performance", 
                        selectInput('selectCarrier', 'Aircraft Carrier',
                            choices = unique(arr_delays$OP_CARRIER)),
                        plotOutput('arr_delayPlot'))

carrier_performance_server <- function(input) { 
  renderPlot({
    print(input$selectCarrier)
    arr_delays %>%
      filter(OP_CARRIER == input$selectCarrier) %>%
      ggplot(aes(ARR_DELAY)) + geom_density()
  
  })
  
}

                                
                                

