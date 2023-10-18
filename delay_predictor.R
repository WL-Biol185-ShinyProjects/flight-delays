library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with delay_*
# 

delay_predictor <- tabPanel("Delay Predictor",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("plotType", "Plot type",
                                             c("Scatter"="p", "Line"="l")
                                )
                              ),
                              mainPanel(
                                plotOutput("plot")
                              )
                            )
                            )

delay_predictor_server <- function(input) { renderPlot({
                                              plot(cars, type=input$plotType)
                                            })
                                          }