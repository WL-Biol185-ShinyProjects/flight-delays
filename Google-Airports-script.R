library(dplyr)
library(tidyverse)

merged_data = merged_data %>% rename('CITY LONGITUDE' = 'lng')

merged_data = merged_data %>% rename('CITY LATITUDE' = 'lat')

'airports-merged' = 'merged_data'

'merged_data' = 'merged_data'


> library(readr)
> airports <- read_csv("data/airports.csv")
Rows: 341 Columns: 7                                                                                                    
── Column specification ─────────────────────────────────────────────────────────────
Delimiter: ","
chr (5): IATA, AIRPORT, CITY, STATE, COUNTRY
dbl (2): LATITUDE, LONGITUDE

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(airports)
> library(readr)
> uscities <- read_csv("data/uscities.csv")
Rows: 28095 Columns: 17                                                            
0s── Column specification ─────────────────────────────────────────────────────────────
Delimiter: ","
chr (9): city, city_ascii, state_id, state_name, county_fips, county_name, source...
View(uscities)
> colnames(airports)[colnames(airports) == "LATITUDE"] <- "AIRPORT LATITUDE"
> colnames(airports)[colnames(airports) == "LONGITUDE"] <- "AIRPORT LONGITUDE"
> uscities_sub <- uscities[, c("city", "lat", "lng")]
> merged_data <- merge(airports, uscities_sub, by.x="city", by.y = "city", all.x=TRUE)
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> merged_data <- merge(airports, uscities_sub, by.x="city", by.y = "city", all.x=TRUE)
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> merged_data <- merge(airports, uscities_sub, by.x="city", by.y = "city", all.x=TRUE)
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> uscities_sub <- uscities[, c("city", "lat", "lng")]
> merged_data <- merge(airports, uscities_sub, by.x="city", by.y = "city", all.x=TRUE)
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> View(uscities_sub)
> merged_data <- merge(airports, uscities_sub, by.x="city", by.y = "city", all.x=TRUE)
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> 
  > 
  > merged_data <- merge(airports, uscities_sub, by.x="city", by.y = "city", all.x=TRUE)
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> merged_data <- merge(airports, uscities_sub, by.x="city", by.y = "city", all.x=TRUE)
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> merged_data <- merge(airports, uscities_sub, by.x="CITY", by.y = "CITY", all.x=TRUE)
Error in fix.by(by.y, y) : 'by' must specify a uniquely valid column
> merged_data <- merge(airports, uscities_sub, by.x="CITY", by.y = "city", all.x=TRUE)
> View(merged_data)
> #merged_data <- merge(airports, uscities_sub, by.x="CITY", by.y = "city", all.x=TRUE)
  > duplicateRows <- duplicated(airports$CITY)
> merged_data <- merge(airports, uscities_sub, by.x="CITY", by.y = "city")
> View(merged_data)
> uscities_sub <- uscities[, c("city", "state_id", "lat", "lng")]
> View(uscities_sub)
> uscities_sub <- uscities[, c("city", "state_id", "lat", "lng")]
> merged_data <- merge(airports, uscities_sub, by.x=c("CITY", "STATE"), by.y=c("city", "state_id"))










duplicateRows <- duplicated(airports$CITY)
merged_data <- merge(airports, uscities_sub, by.x="CITY", by.y = "city")
View(merged_data)
uscities_sub <- uscities[, c("city", "state_id", "lat", "lng")]
View(uscities_sub)
uscities_sub <- uscities[, c("city", "state_id", "lat", "lng")]
merged_data <- merge(airports, uscities_sub, by.x=c("CITY", "STATE"), by.y=c("city", "state_id"))
View(merged_data)
View(merged_data)


library(tidyverse)

merged_data %>%
  mutate(Location = paste0(merged_data$AIRPORT, 
                           '(', merged_data$IATA, ')'))



