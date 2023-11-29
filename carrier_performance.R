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

carrier_inList <- function(selectCarrier, a) {
    for (x in selectCarrier) {
        if (!(x %in% a)) {
            return(FALSE)
      }
    }
    return(TRUE)
}

carrier_performance <- tabPanel("Carrier Performance", 
    selectInput('selectCarrier', 
                'Aircraft Carrier',
                multiple = TRUE,
                choices = carrier_carriers,
                selected = c('NK', 'F9')),
    fluidPage(
        fluidRow(
            column(4, plotOutput('arr_delayPlot')),
            column(4, plotOutput('delay_typesPlot')),
            column(4, uiOutput('reviewsPlot'))
      )
    )
)


carrier_performance_arr_delay <- function(input) { 
    renderPlot({
        gc()
        lapply(input$selectCarrier, function(x) { 
            readRDS(paste0("data/", x, ".rds")) 
        }) %>%
            do.call(rbind, .) %>%
            ggplot(aes(ARR_DELAY,
                       fill = OP_CARRIER)) + 
                geom_density(alpha = .2) + 
                ggtitle('DENSITY OF ARRIVAL DELAY') + 
                xlab('ARRIVAL DELAY IN MINUTES') +
                ylab('DENSITY')
    })
}

carrier_performance_delay_types <- function(input) {
    renderPlot({
        delay_types %>%
            filter(OP_CARRIER %in% input$selectCarrier) %>%
            gather('DELAY_TYPE', 
                  'COUNTS',
                  'CARRIER_DELAY':'LATE_AIRCRAFT_DELAY') %>%
            ggplot(aes(DELAY_TYPE, 
                       COUNTS,
                       fill = OP_CARRIER,
                       position = 'dodge')) + 
                geom_bar(stat = 'identity',
                         position = 'dodge') +
                ggtitle('COUNTS OF DELAY TYPES') +
                xlab('DELAY TYPES') +
                ylab('COUNTS')
    })
}

carrier_performance_reviews <- function(input) {
    renderUI({
        a <- c('NK', 'WN', 'AA', 'DL', 'UA', 'AS', 'B6', 'F9')
        if (length(input$selectCarrier) == 1 && !(input$selectCarrier %in% a)) {
            return('DATA UNAVAILABLE FOR SELECTED CARRIER')
        } else {
              renderPlot({
                  reviews %>%
                      filter(OP_CARRIERS %in% input$selectCarrier) %>%
                      ggplot(aes(REVIEWS,
                                 fill = OP_CARRIERS)) +
                          geom_density() +
                          scale_x_continuous(breaks = 1:10, 
                                             labels = 1:10) +
                          ggtitle('DENSITY OF REVIEWS (1-10)') +
                          xlab('REVIEWS (1-10)') +
                          ylab('DENSITY')
              })
          }
    })
}