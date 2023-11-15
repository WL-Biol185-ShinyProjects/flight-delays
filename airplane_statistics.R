library(leaflet)
library(shiny)

aircrafts <- readRDS("data/aircrafts.rds")

get_engine_type <- function(name) { df <- aircrafts %>%
                                      filter(name == Model_FAA)
                                      (df$Physical_Class_Engine)
}

get_engine_number <- function(name) { df <- aircrafts %>%
                                      filter(name == Model_FAA)
                                      (df$Num_Engines)
}

get_approach_speed <- function(name) { df <- aircrafts %>%
                                      filter(name == Model_FAA)
                                      (df$Approach_Speed_knot)
}

get_wingspan <- function(name) { df <- aircrafts %>%
                                  filter(name == Model_FAA)
                                  (df$Wingspan_ft_without_winglets_sharklets)
}
get_length <- function(name) { df <- aircrafts %>%
                                filter(name == Model_FAA)
                                (df$Length_ft)
}
get_wheelbase <- function(name) { df <- aircrafts %>%
                                  filter(name == Model_FAA)
                                  (df$Wheelbase_ft)
}

get_registration_count <- function(name) { df <- aircrafts %>%
                                    filter(name == Model_FAA)
                                    (df$FAA_Registry)
}

get_operations <- function(name) { df <- aircrafts %>%
                                    filter(name == Model_FAA)
                                    (df$Total_IFR_Operations_2021_2022)
}
airplane_statistics <- tabPanel("Airplane Information",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectizeInput( inputId  = "airplane_model"
                                                    , label    = "Select Aircraft"
                                                    , choices  = aircrafts$Model_FAA
                                                    , selected = "Boeing 747-300"
                                                    , options  = list( create = FALSE
                                                                       , placeholder = "Search..."
                                                                       , maxItems = "1"
                                                                       , onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                                                       , onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                                      )
                                                  )
                                               ),
                                  mainPanel(
                                    uiOutput("aircraft_specifications_table")
                                            )
                                          )
)

aircraft_specifications_table <- function(input) {
  renderUI({
    
    engine_class <- get_engine_type(input$airplane_model)
    engine_number <- get_engine_number(input$airplane_model)
    approach_speed <- get_approach_speed(input$airplane_model)
    wingspan <- get_wingspan(input$airplane_model)
    length <- get_length(input$airplane_model)
    wheelbase <- get_wheelbase(input$airplane_model)
    registration_count <- get_registration_count(input$airplane_model)
    operations <- get_operations(input$airplane_model)
    
    if(isTruthy(input$airplane_model)) {
      tagList(
        p(strong("Engine Type:"), engine_class),
        p(strong("Number of Engines:"), engine_number),
        p(strong("Aircraft Approach Speed:"), approach_speed, "knots"),
        p(strong("Wingspan Without Winglets:"), wingspan, "feet"),
        p(strong("Length of Aircraft:"), length, "feet"),
        p(strong("Wheelbase:"), wheelbase, "feet"),
        p(strong("Registration Count"), registration_count),
        p(strong("Operations 2021-2022"), operations)
        
      )
    }
    else {
      p(strong("Please Select an Aircraft to View Specifications"))
    }
    
  })


}