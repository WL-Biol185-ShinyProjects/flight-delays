library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with carrier_*

reviews <- readRDS('data/reviews.rds')
delay_types <- readRDS('data/delay_types.rds')
carrier_carriers <- readRDS("data/carriers.rds")

carrier_performance <- tabPanel("Carrier Performance", 
                        selectInput('selectCarrier', 'Aircraft Carrier',
                            choices = carrier_carriers),
                        plotOutput('arr_delayPlot'),
                        plotOutput('delay_typesPlot'),
                        plotOutput('reviewsPlot'))

carrier_performance_arr_delay <- function(input) { 
  renderPlot({
    gc()
    lapply(input$selectCarrier, function(x) { 
      readRDS(paste0("data/", x, ".rds")) 
    }) %>%
      do.call(rbind, .) %>%
      ggplot(aes(ARR_DELAY)) + 
      geom_density() + 
      ggtitle('DENSITY OF ARRIVAL DELAY') + 
              xlab('ARRIVAL DELAY IN MINUTES') +
              ylab('DENSITY')
  })
}

carrier_performance_delay_types <- function(input) {
  renderPlot({
    delay_types %>%
      filter(OP_CARRIER == input$selectCarrier) %>%
      gather('DELAY_TYPE', 
             'COUNTS',
             'CARRIER_DELAY':'LATE_AIRCRAFT_DELAY') %>%
      ggplot(aes(DELAY_TYPE, COUNTS)) + 
      geom_bar(stat = 'identity') + 
      ggtitle('NUMBER OF DELAY TYPES') +
              xlab('DELAY TYPES') +
              ylab('NUMBER')
  })
}

carrier_performance_reviews <- function(input) {
  renderPlot({
    reviews %>%
      filter(OP_CARRIERS == input$selectCarrier) %>%
      ggplot(aes(REVIEWS)) + 
      geom_density() + 
      scale_x_continuous(breaks = 1:10, 
                         labels = 1:10) +
      ggtitle('DENSITY OF REVIEWS (1-10)') +
              xlab('REVIEWS (1-10)') +
              ylab('DENSITY')
  })
}
                                
                                

