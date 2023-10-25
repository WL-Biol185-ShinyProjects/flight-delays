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
reviews <- readRDS('data/reviews.rds')
delay_types <- readRDS('data/delay_types.rds')
carrier_carriers <- readRDS("data/carriers.rds")

carrier_performance <- tabPanel("Carrier Performance", 
                        selectInput('selectCarrier', 'Aircraft Carrier',
                            choices = carrier_carriers),
                        plotOutput('arr_delayPlot'))

carrier_performance_server <- function(input) { 
  
  renderPlot({
    gc()
    lapply(input$selectCarrier, function(x) { 
      readRDS(paste0("data/", x, ".rds")) 
    }) %>%
      do.call(rbind, .) %>%
      ggplot(aes(ARR_DELAY)) + geom_density()
  })

  
}

                                
                                

