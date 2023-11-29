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
   tags$script(HTML("document.querySelectorAll('[data-value=\"Carrier Performance\"]')[1].style.paddingLeft = \"4vw\"; document.querySelectorAll('[data-value=\"Carrier Performance\"]')[1].style.paddingRight = \"4vw\";")),
    selectizeInput('chooseCarrier',
                   'Aircraft Carrier',
                    choices = carrier_carriers[-13],
                    selected = 'UA'),
    plotOutput('crashes_typePlot'),
    uiOutput('crash_expected_table')
)

getting_hijacked_crashes <- function(input) {
    renderPlot({
        crashes %>%
            filter(OP_CARRIER == input$chooseCarrier) %>%
            count(INCIDENT_TYPE) %>%
            ggplot(aes(INCIDENT_TYPE, n, fill = INCIDENT_TYPE)) +
                geom_bar(stat = 'identity',
                         position = 'dodge') +
                labs(title = 'REASONS FOR PLANE CRASH',
                    x = 'INCIDENT TYPE',
                    y = '# OF INCIDENTS / CARRIER')
    })
}

crash_expected_table <- function(input) { 
    renderUI({ 
        crash_hijacked <- 100 * (nrow(filter(crashes, 
                                             OP_CARRIER == input$chooseCarrier & (INCIDENT_TYPE == "Hijacking | repairable-damage" | INCIDENT_TYPE == "Hijacking | hull-loss" | INCIDENT_TYPE == "Criminal occurrence (sabotage, shoot down) | repairable-damage" | INCIDENT_TYPE == "Criminal occurrence (sabotage, shoot down) | hull-loss")))/nrow(filter(crashes, 
                                                                                                                                                                                                                                                                                                                                                             OP_CARRIER == input$chooseCarrier)))
        crash_collision <- 100 * (nrow(filter(crashes,
                                              OP_CARRIER == input$chooseCarrier & (INCIDENT_TYPE == "Accident | repairable-damage" | INCIDENT_TYPE == "Accident | hull-loss" | INCIDENT_TYPE == "other occurrence (ground fire, sabotage) | hull-loss" | INCIDENT_TYPE == "other occurrence (ground fire, sabotage) | repairable-damage")))/nrow(filter(crashes,   
                                                                                                                                                                                                                                                                                                                                                        OP_CARRIER == input$chooseCarrier)))
        
        tagList(
          p(strong("Percentage of Crashes due to Hijacking: "), format(crash_hijacked, digits = 2), "%"),
          p(strong("Percentage of Crashes due to Collision or Engine Failure: "), format(crash_collision, digits = 2), "%"),
        )
  })
} 




