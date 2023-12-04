library(shiny)

about_page <- tabPanel ("About",
  fluidPage(
    fluidRow(
       column(12,
             wellPanel(
               h3("Overview"),
               p("Airborne Analytics is a user friendly and feature rich application designed to provide a plethora of information about flights, airlines, and aircraft. Whether you're a traveler looking to book a flight or simply an aircraft enthusiast looking to learn, Airborne Analytics has you covered. This application offers a seamless and intuitive experience, making it easy for users to learn and make informed travel decisions.")
                      )
             
              )
             ),
  fluidRow(
     column(12,
           wellPanel(
             h3("Features"),
             h4("1. Flight Plotter"),
             p("Want to see the path your flight will likely follow? This application will generate accurate flight paths for nonstop, one-stop, or two-stop flights between any airports in the United States of America based on great circle calculation algorithms."),
             h4("2. Delay Predictor"),
             p("Looking to book a flight? Input your origin and destination airports along with departure time to see relevant delay statistics."),
             h4("3. Carrier Performance"),
             p("Not sure which carrier to fly? Input any number of USA based airlines to compare delay densities, delay counts, and overall airline reviews."),
             h4("4. Crash Statistics"),
             p("Scared of flying? See crash statistics across various USA based airlines."),
             h4("5. Airplane Information"),
             p("Curious about the specifications of a specific aircraft? Input the aircraft and watch as the application presents a myriad of data regarding that aircraft.")
                    )
           
           )
         ),
  fluidRow(
     column(12,
            wellPanel(
              h3("Resources Used"),
              h4("Packages:"),
              p("Leaflet,Tidyverse, Shiny, Geosphere, GGplot2, Dplyr, Sp, Shinytime"),
              h4("APIs:"),
              p("Iowa State University API for live weather radar images")
                     )
            )
         )
  )                            
)