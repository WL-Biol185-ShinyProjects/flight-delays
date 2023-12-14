library(leaflet)
library(tidyverse)
library(shiny)
library(geosphere)
library(ggplot2)
library(dplyr)
library(httr)

source("flight_plotter.R")
source("delay_predictor.R")
source("carrier_performance.R")
source("airplane_statistics.R")
source("about_page.R")

function(input, output, session) {

  output$delay_predictor_map <- delay_predictor_map(input)
  output$delay_expected_table <- delay_expected_table(input)
    
  output$arr_delayPlot <- carrier_performance_arr_delay(input)
  output$delay_typesPlot <- carrier_performance_delay_types(input)
  output$reviewsPlot <- carrier_performance_reviews(input)
  
  output$flight_route_map <- flight_route_map(input)
  
  output$engine_information <- engine_information(input)
  output$aircraft_activity <- aircraft_activity(input)
  output$weight_capacity <- weight_capacity(input)
  output$landing_info <- landing_info(input)
  output$airplane_dimensions <- airplane_dimensions(input)
  output$regulatory <- regulatory(input)
  output$aircraft_images <- aircraft_images(input)
  
  observeEvent(input$seats, {
    dialog <- modalDialog( title = "How we chose these seats"
                         , tags$div(tags$p("Seat choice has been optimized to not be on the side closest to the sun, so that you aren't blinded when you or someone else opens the window shade."), tags$ul(tags$li("Eastbound: Left side"), tags$li("Westbound: Right side"), tags$li("Northbound (morning): Left side"), tags$li("Northbound (afternoon): Right side"), tags$li("Southbound (morning): Right side"), tags$li("Southbound (afternoon): Left side")))
                         , easyClose = TRUE
                         , fade = TRUE
                         )
    showModal(dialog)
  })
  
  observeEvent(input$landingHelp, {
    dialog <- modalDialog( title = "Landing Gear Information"
                            , tags$div("Visit the Federal Aviation Administration's Order 5300.7 regarding landing gear classifications", tags$a(target = "_blank", href = "https://www.faa.gov/documentLibrary/media/Order/Construction_5300_7.pdf", "HERE"))
                            , easyClose = TRUE
                            , fade = TRUE
                          )
    showModal(dialog)
  })
  
  observeEvent(input$engineHelp, {
    dialog <- modalDialog( title = "Different Aircraft Engine Types"
                            , HTML(' <img src= "https://cdn.boldmethod.com/images/learn-to-fly/systems/4-types-of-turbine-engines/diagram-turbojet.jpg" height = 300 > ')
                            , p("")
                            , HTML(' <img src = "https://cdn.boldmethod.com/images/learn-to-fly/systems/4-types-of-turbine-engines/diagram-turboprop.jpg" height = 300 > ')
                            , p("")
                            , HTML(' <img src = "https://cdn.boldmethod.com/images/learn-to-fly/systems/how-does-your-piston-engine-work/engine-components.jpg" height = 300 > ')
                            , p("")
                            , HTML(' <img src = "https://cdn.boldmethod.com/images/learn-to-fly/systems/4-types-of-turbine-engines/diagram-turboshaft.jpg" height = 300 > ')
                            , p("")
                            , p("Diagrams © boldmethod")
                            , easyClose = TRUE
                            , fade = TRUE
    )
    showModal(dialog)
})
  
  observeEvent(input$regulatoryHelp, {
    dialog <- modalDialog( title = "FAA Regulatory Designations"
                           , h4("Consolidated Wake Turbulence Category (CWT)")
                           , tags$div("Visit the Federal Aviation Administration's Order JO 7110.126B regarding CWT categories", tags$a(target = "_blank", href = "https://www.faa.gov/documentLibrary/media/Order/2021-11-08_JO_7110.126B_Consolidated_Wake_Turbulence__FINAL.pdf", "HERE"))
                           , h4("Aircaft Approach Category (AAC)")
                           , p("Category A: speed less than 91 knots")
                           , p("Category B: speed greater than 91 knots but less than 121 knots")
                           , p("Category C: speed greater than 121 knots but less than 141 knots")
                           , p("Category D: speed greater than 141 knots but less than 166 knots")
                           , p("Category E: speed greater than 166 knots")
                           , h4("Aircraft Design Category (ADG)")
                           , p("Group I: tail height < 20ft, wingspan < 49ft")
                           , p("Group II: tail height 20-29ft, wingspan 49-79ft")
                           , p("Group III: tail height 30-45ft, wingspan 80-118ft")
                           , p("Group IV: tail height 45-60ft, wingspan 118-171ft")
                           , p("Group V: tail height 60-66ft, wingspan 171-214ft")
                           , p("Group VI: tail height 66-80ft, wingspan 214-262ft")
                           , h4("Wake Turbulence Category (WTC)")
                           , p("Category L: light aircraft under 7,000kg")
                           , p("Category M: medium aircraft between 7,000kg and 136,000kg")
                           , p("Category H: heavy aircraft greater than 136,000kg")
                           , p("Category J: super aircraft with special FAA designation")
                           , easyClose = TRUE
                           , fade = TRUE
    )
    showModal(dialog)
  })
  
  
  output$flight_seat <- flight_seat(input)
  output$crashes_typePlot <- getting_hijacked_crashes(input)

}
