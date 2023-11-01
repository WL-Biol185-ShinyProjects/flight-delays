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