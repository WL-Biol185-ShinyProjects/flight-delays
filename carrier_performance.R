library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)
library(shinyjs)

# variable name conventions:
#   start each variable name with the first word of file
#   so, all variable names on this file will be prefixed
#   with carrier_*

reviews <- readRDS('data/reviews.rds')
delay_types <- readRDS('data/delay_types.rds')
carrier_carriers <- readRDS("data/carriers.rds")
crashes <- readRDS('data/aircraft_crashes.rds')

carrier_inList <- function(selectCarrier, a) {
    for (x in selectCarrier) {
        if (!(x %in% a)) {
            return(FALSE)
      }
    }
    return(TRUE)
}

carrier_css <- "
  #preloader {
    position: fixed;
    top: 50px;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: 99999;
    display: flex;
    flex-flow: row nowrap;
    justify-content: center;
    align-items: center;
    background: none repeat scroll 0 0 #ffffff;
  }

  @keyframes pulse { 
    from {opacity: .6; transform: scale(1);}
    to {opacity: .001; transform: scale(3);}
  }

.centerpiece { 
    position: fixed;
    top: calc(50% + 50px);
    left: 50%;
    transform: translate(-50%, -50%);
  }

.pulse {
    width: 100px;
    height: 100px;
    border-radius: 100%;
    background-color: rgba(0, 0, 0, .4);
    animation-name: pulse;
    animation-duration: 1s;
    animation-iteration-count: infinite;
  }

.preloader-img { 
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    width: 120px;
    height: 120px
 }
  "

carrier_performance <- tabPanel("Carrier Performance",
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js"),
    useShinyjs(),
    inlineCSS(carrier_css),
    div(
      id = "preloader",
      img(
        class = "preloader-img",
        src = "preloader.png"
      ),
      div(
        class = "centerpiece",
        div(
          class = "pulse"
        )
      )
    ),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        selectInput('selectCarrier', 
                    'Aircraft Carrier',
                    multiple = TRUE,
                    choices = carrier_carriers,
                    selected = c('NK', 'F9')
        ), width = 2, 
        h3('Density of Arrival Delay'),
        p('Shows the density of arrival delay by domestic carrier.'),
        h3('Counts of Delay Types'),
        p('Shows the number of delays by various categories. 
          Aircraft delay - delay as a result of lateness by the same aircraft (if the plane was late at the previous airport and therefore late at yours).
          Carrier delay - delay within the control of the carrier (cleaning, fueling, etc).
          NAS delay - delay forced by the National Airspace System (heavy traffic, air traffic control).
          Security delay - delay caused by safety concerns, re-boarding, or evacuation. 
          Weather - delay caused by extreme, hazardous weather.'),
        h3('Density of Reviews'),
        p('Shows the density of passenger reviews from 1-10.'),
        h3('Reasons for Plane Crash'),
        p('Shows number of incidents by type for selected carriers. Incidents are either repairable or irrepairable.')
        ),
      
      mainPanel = mainPanel(
        fluidRow(
          column(6, plotOutput('arr_delayPlot')),
          column(6, plotOutput('delay_typesPlot'))
        ),
        fluidRow(
          column(6, uiOutput('reviewsPlot')),
          column(6, plotOutput('crashes_typePlot'))
        ), width = 10
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
                        ylab('DENSITY') +
                        theme(plot.title = element_text(size = 16, face = 'bold'),
                              axis.title.x = element_text(size = 14),
                              axis.title.y = element_text(size = 14),
                              axis.text.x = element_text(size = 12),
                              axis.text.y = element_text(size = 12),
                              legend.text = element_text(size = 12))
    })
}

carrier_performance_delay_types <- function(input) {
    renderPlot({
        delay_types %>%
            rename(AIRCRAFT = 'LATE_AIRCRAFT_DELAY', NAS = 'NAS_DELAY', CARRIER = 'CARRIER_DELAY', WEATHER = 'WEATHER_DELAY', SECURITY = 'SECURITY_DELAY') %>%
            filter(OP_CARRIER %in% input$selectCarrier) %>%
            gather('DELAY_TYPE', 
                   'COUNTS',
                   'CARRIER':'AIRCRAFT') %>%
            ggplot(aes(DELAY_TYPE, 
                       COUNTS,
                       fill = OP_CARRIER,
                       position = 'dodge')) + 
                geom_bar(stat = 'identity',
                         position = 'dodge') +
                ggtitle('COUNTS OF DELAY TYPES') +
                xlab('DELAY TYPES') +
                ylab('COUNTS') +
                theme(plot.title = element_text(size = 16, face = 'bold'),
                      axis.title.x = element_text(size = 14),
                      axis.title.y = element_text(size = 14),
                      axis.text.x = element_text(size = 12),
                      axis.text.y = element_text(size = 12),
                      legend.text = element_text(size = 12))
    })
}

carrier_performance_reviews <- function(input) {
    renderUI({
        renderPlot({
          runjs("$('#preloader').fadeIn(100);")
          perf_plot <- reviews %>%
                        filter(OP_CARRIERS %in% input$selectCarrier) %>%
                        ggplot(aes(REVIEWS,
                                   fill = OP_CARRIERS)) +
                            geom_density(alpha = .2) +
                            scale_x_continuous(breaks = 1:10, 
                                               labels = 1:10) +
                            ggtitle('DENSITY OF REVIEWS (1-10)') +
                            xlab('REVIEWS (1-10)') +
                            ylab('DENSITY') +
                            theme(plot.title = element_text(size = 16, face = 'bold'),
                                  axis.title.x = element_text(size = 14),
                                  axis.title.y = element_text(size = 14),
                                  axis.text.x = element_text(size = 12),
                                  axis.text.y = element_text(size = 12),
                                  legend.text = element_text(size = 12))
          runjs("$('#preloader').fadeOut(1000);")
          perf_plot
        })

    })
}

getting_hijacked_crashes <- function(input) {
  renderPlot({
      crashes %>%
          mutate(INCIDENT_TYPE = case_when(
              INCIDENT_TYPE == "Accident | repairable-damage" ~ "Repairable Accident",
              INCIDENT_TYPE == "Accident | hull-loss" ~ "Irrepairable Accident",
              INCIDENT_TYPE == "Hijacking | hull-loss" ~ "Irrepairable Hijacking",
              INCIDENT_TYPE == "Hijacking | repairable-damage" ~ "Repairable Hijacking",
              INCIDENT_TYPE == "other occurrence (ground fire, sabotage) | hull-loss" ~ "Other (irrepairable)",
              INCIDENT_TYPE == "other occurrence (ground fire, sabotage) | repairable-damage" ~ "Other (repairable)",
              INCIDENT_TYPE == "Criminal occurrence (sabotage, shoot down) | repairable-damage" ~ "Criminal (repairable)",
              INCIDENT_TYPE == "Criminal occurrence (sabotage, shoot down) | hull-loss" ~ "Criminal (irrepairable)")) %>%
          filter(OP_CARRIER %in% input$selectCarrier) %>%
          count(INCIDENT_TYPE, OP_CARRIER) %>%
          ggplot(aes(x = OP_CARRIER, 
                     y = n, 
                     fill = INCIDENT_TYPE)) +
              geom_bar(stat = 'identity',
                       position = 'stack',
                       width = 0.2) +
              labs(title = 'REASONS FOR PLANE CRASH',
                   x = 'CARRIER',
                   y = '# OF INCIDENTS') +
              theme(plot.title = element_text(size = 16, face = 'bold'),
                    axis.title.x = element_text(size = 14),
                    axis.title.y = element_text(size = 14),
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    legend.text = element_text(size = 12))
  })
}
