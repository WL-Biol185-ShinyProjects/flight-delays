library(shiny)
library(httr)

aircrafts <- readRDS("data/aircrafts.rds")

get_airplanestats <- function(name) { 
  aircrafts %>%
  filter(name == Model_FAA)
}

airplane_statistics <- tabPanel("Airplane Information",
  tags$style(type = "text/css", "#landing-info { display: inline }", "#engine-info { display: inline }", "#regulatory-info { display: inline }"),
  fluidPage(
    fluidRow(
        column(12,
            wellPanel(
              h3("Select Aircraft"),
              selectizeInput( inputId  = "airplane_model"
                              , label = ""
                              , choices  = aircrafts$Model_FAA
                              , selected = "Airbus A320"
                              , options  = list( create = FALSE
                                                 , placeholder = "Search..."
                                                 , maxItems = "1"
                                                 , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                 , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                )
                            )
                      )
              )
            ),
    fluidRow(
        column(4,
          wellPanel(
            h4(id = "engine-info", "Engine Information"),
            actionButton("engineHelp", "", icon = icon("info")),
            uiOutput("engine_information")
                    )
              ),
        column(4,
          wellPanel(
            h4("Aircraft Activity"),
            uiOutput("aircraft_activity")
                    )
              ),
        column(4,
          wellPanel(
            h4("Weight Capacity"),
            uiOutput("weight_capacity")
                    )
              )
            ),
    fluidRow(
        column(4,
          wellPanel(
            h4(id = "landing-info", "Landing Information"),
            actionButton("landingHelp", "", icon = icon("info")), 
            uiOutput("landing_info")
                    )
              ),
        column(4,
          wellPanel(
            h4("Dimensions"),
            uiOutput("airplane_dimensions")   
                    )
              ),
        column(4,
          wellPanel(
            h4(id = "regulatory-info", "Regulatory Information"),
            actionButton("regulatoryHelp", "", icon = icon("info")),
            uiOutput("regulatory")  
                    )
              )
            ),
    fluidRow(
        column(12,
          wellPanel(
            h4("Aircraft Images"),
            uiOutput("aircraft_images")
                    )   
              )
            )
      )
)
  
engine_information <- function (input) {
  renderUI ({
    
    engine_row <- get_airplanestats(input$airplane_model)
    
    tagList(
      p(strong("Engine Type:"), engine_row$Physical_Class_Engine),
      p(strong("Number of Engines:"), engine_row$Num_Engines)
      
    )
  })
}

aircraft_activity <- function (input) {
  renderUI ({
    
    activity_row <- get_airplanestats(input$airplane_model)
    
    tagList(
      p(strong("Number Registered:"), activity_row$Registration_Count),
      p(strong("Operations 2021-2022:"), activity_row$Total_IFR_Operations_2021_2022)
      
    )
  })
}

weight_capacity <- function(input) {
  renderUI ({
    
    weight_row <- get_airplanestats(input$airplane_model)
    
    tagList(
      p(strong("Maximum Takeoff Weight:"), weight_row$MTOW_lb, "lbs"),
      p(strong("Maximum Landing Weight:"), weight_row$MALW_lb, "lbs")
      
    )
  })
}

landing_info <- function(input) {
  renderUI ({
    
    landing_row <- get_airplanestats(input$airplane_model)
    
    tagList(
      p(strong("Landing Gear Width:"), landing_row$Main_Gear_Width_ft, "ft"),
      p(strong("Landing Gear Configuration:"), landing_row$Main_Gear_Config),
      p(strong("Maximum Approach Speed:"), landing_row$Approach_Speed_knot, "knots")
      
    )
  })
}
airplane_dimensions <- function(input)  {
  renderUI ({
    
    dimensions_row <- get_airplanestats(input$airplane_model)
    
    tagList(
      p(strong("Wingspan:"), dimensions_row$Wingspan_ft_without_winglets_sharklets, "ft"),
      p(strong("Length:"), dimensions_row$Length_ft, "ft"),
      p(strong("Tail Height:"), dimensions_row$Tail_Height_at_OEW_ft, "ft"),
      p(strong("Parking Area:"), dimensions_row$Parking_Area_ft2, "ft^2")
        
    )
  })
}

regulatory <- function(input)  {
  renderUI ({
    
    regulatory_row <- get_airplanestats(input$airplane_model)
    
    tagList(
      p(strong("CWT Designation"), regulatory_row$CWT),
      p(strong("AAC Designation"), regulatory_row$AAC),
      p(strong("ADG Designation"), regulatory_row$ADG),
      p(strong("WTC Designation"), regulatory_row$ICAO_WTC)
      
    )
  })
}

aircraft_images <- function(input)  {
  renderUI ({
    
    query <- input$airplane_model
    query <- stringr::str_replace_all(query, " ", "%20")
    response <- GET(paste0("https://commons.wikimedia.org/w/api.php?action=query&list=search&srnamespace=6&srsearch=", query, "&srlimit=20&sroffset=20&prop=imageinfo&format=json"))
    
    file <- response %>%
      content(as = "text") %>%
      stringr::str_extract('(?<="title":")(.{1,300})(?=","pageid)')
    
    file <- stringr::str_replace_all(file, " ", "%20")
    img <- GET(paste0("https://commons.wikimedia.org/w/api.php?action=query&titles=", file, "&prop=imageinfo&iiprop=url&format=json"))
    
    link <- img %>%
      content(as = "text") %>%
      stringr::str_extract_all('(?<="url":")(.{1,100})(?=","descriptionurl)')
    
    img(src = link, width = "500px", height = "auto")
    
  })
  
  
}