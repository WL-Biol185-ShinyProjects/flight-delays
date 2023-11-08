tidied_reviews <- reviews %>%
  rename(OP_CARRIERS = 'airline', REVIEWS = 'overall') %>%
  mutate(OP_CARRIERS = case_when(
    OP_CARRIERS == "Spirit Airlines" ~ "NK",
    OP_CARRIERS == "American Airlines" ~ 'AA',
    OP_CARRIERS == 'Delta Air Lines' ~ 'DL',
    OP_CARRIERS == 'United Airlines' ~ 'UA',
    OP_CARRIERS == 'Alaska Airlines' ~ 'AS',
    OP_CARRIERS == 'Jetblue Airways' ~ 'B6',
    OP_CARRIERS == 'Frontier Airlines' ~ 'F9',
    OP_CARRIERS == 'Southwest Airlines' ~ 'WN'))

tidied_reviews <- tidied_reviews %>%
  filter(OP_CARRIERS %in% c('NK',
                            'WN',
                            'AA',
                            'DL',
                            'UA',
                            'AS',
                            'B6',
                            'F9'))

#endeavor airlines doesn't exist on table (9E)
#atlantic southeast (EV)
#allegiant air (G4)
#Hawaiian air (HA)
#american eagle (MQ)
#comair (OH)
#skywest (OO)
#Mesa (YV)
#Midwest (YX)


airplane_crashes <- d %>%
    rename(OP_CARRIER = 'Aircaft_Operator', FLIGHT_TYPE = 'Aircaft_Nature', INCIDENT_TYPE = 'Incident_Category') %>%
    mutate(OP_CARRIER = case_when(
        OP_CARRIER == 'Delta Air Lines' ~ 'DL',
        OP_CARRIER =='American Airlines' ~ 'AA',
        OP_CARRIER =='Spirit Airlines' ~ 'NK',
        OP_CARRIER =='United Airlines' ~ 'UA',
        OP_CARRIER =='Alaska Airlines' ~ 'AS',
        OP_CARRIER == 'JetBlue Airways' ~ 'B6',
        OP_CARRIER == 'Frontier Airlines' ~ 'F9',
        OP_CARRIER == 'Southwest Airlines' ~ 'WN',
        OP_CARRIER == 'Allegiant Air' ~ 'G4',
        OP_CARRIER == 'Endeavor Air' ~ '9E',
        OP_CARRIER == 'Southeast Airlines' ~ 'EV',
        OP_CARRIER == 'Hawaiian Airlines' ~ 'HA',
        OP_CARRIER == 'American Eagle Airlines' ~ 'MQ',
        OP_CARRIER == 'Comair' ~ 'OH',
        OP_CARRIER == 'SkyWest Airlines' ~ 'OO',
        OP_CARRIER == 'Mesa Airlines' ~ 'YV',
        OP_CARRIER == 'Mesa Airlines, op.for United Express' ~ 'YV',
        OP_CARRIER == 'Mesa Airlines, opf US Airways' ~ 'YV',
        OP_CARRIER == 'Air Midwest, op.for US Airways Express' ~ 'YX')) 

airplane_crashes <- airplane_crashes %>%
    filter(OP_CARRIER %in% c('NK', 'WN', 'AA', 'DL', 'UA', 'AS', 'B6', 'F9', 'YX', 'YV', 'OO', 'OH', 'MQ', 'HA', 'G4', 'EV', '9E'))

airplane_crashes <- airplane_crashes %>%
    select(Incident_Date, Aircaft_Model, OP_CARRIER, FLIGHT_TYPE, INCIDENT_TYPE, Incident_Location, `Incident_Cause(es)`) %>%
    rename(DATE = 'Incident_Date', AIRCRAFT = 'Aircaft_Model', LOCATION = 'Incident_Location', CAUSE = 'Incident_Cause(es)')
