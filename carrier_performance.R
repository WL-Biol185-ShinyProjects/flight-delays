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
    top: 0;
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

  .spinner {
    border: 1px solid transparent;
    border-radius: 3px;
    position: relative;
  }

  .spinner:before {
    content: '';
    box-sizing: border-box;
    position: absolute;
    top: 50%;
    left: 50%;
    width: 45px;
    height: 45px;
    margin-top: -10px;
    margin-left: -10px;
    border-radius: 50%;
    border: 1px solid #575757;
    border-top-color: #ffffff;
    animation: spinner .9s linear infinite;
  }

  @-webkit-keyframes spinner {
    to {transform: rotate(360deg);}
  }

  @keyframes spinner {
    to {transform: rotate(360deg);}
  }

  @keyframes pulse { 
    from {opacity: .6; transform: scale(1);}
    to {opacity: .001; transform: scale(3);}
  }

.centerpiece { 
    position: fixed;
    top: 50%;
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
    tags$script(HTML("document.querySelectorAll('[data-value=\"Carrier Performance\"]')[1].style.paddingLeft = \"4vw\"; document.querySelectorAll('[data-value=\"Carrier Performance\"]')[1].style.paddingRight = \"4vw\"; ")),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js"),
    fluidPage(
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
      fluidRow(
        selectInput('selectCarrier', 
                    'Aircraft Carrier',
                    multiple = TRUE,
                    choices = carrier_carriers,
                    selected = c('NK', 'F9')
        )
      ),
      fluidRow(
        column(4, plotOutput('arr_delayPlot')),
        column(4, plotOutput('delay_typesPlot')),
        column(4, uiOutput('reviewsPlot')),
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
                ylab('COUNTS')
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
                            ylab('DENSITY')
          runjs("$('#preloader').fadeOut(1000);")
          perf_plot
        })

    })
}