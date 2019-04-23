#install and load packages
install.packages( "tidyr")

library(dplyr)
library(tidyr)
library(ggplot2)

##load flights into dataframe
flights_df <- data.frame(read.csv("~/Documents/School/Springboard/IDSCapstone/Datasets/flights.csv"))

# remove unneeded columns
novars <- names(flights_df) %in%
  c("TAIL_NUMBER", "TAXI_OUT", "WHEELS_OFF", "AIR_TIME", "DISTANCE", "WHEELS_ON", "TAXI_IN", "SCHEDULED_ARRIVAL", "ARRIVAL_TIME", "ARRIVAL_DELAY")
flights_df <- flights_df[!novars]

# remove canceled and diverted flights
flights_df <- flights_df %>% filter(CANCELLED == 0) %>% select(-c(CANCELLED, CANCELLATION_REASON))
flights_df <- flights_df %>% filter(DIVERTED == 0) %>% select(-DIVERTED)


# delay columns (AIR_SYSTEM_DELAY SECURITY_DELAY AIRLINE_DELAY LATE_AIRCRAFT_DELAY WEATHER_DELAY) contain a range of numbers
# Only concerned with the type of delay,if there was one; changing to 0 or 1  
flights_df <- flights_df %>% 
  mutate(AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY)) %>% 
  mutate(AIR_SYSTEM_DELAY = ifelse(AIR_SYSTEM_DELAY != 0, 1, AIR_SYSTEM_DELAY)) %>%
  mutate(SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY)) %>% 
  mutate(SECURITY_DELAY = ifelse(SECURITY_DELAY != 0, 1, SECURITY_DELAY)) %>%
  mutate(AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY)) %>% 
  mutate(AIRLINE_DELAY = ifelse(AIRLINE_DELAY != 0, 1, AIRLINE_DELAY)) %>%
  mutate(LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY)) %>% 
  mutate(LATE_AIRCRAFT_DELAY = ifelse(LATE_AIRCRAFT_DELAY != 0, 1, LATE_AIRCRAFT_DELAY)) %>%
  mutate(WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY)) %>% 
  mutate(WEATHER_DELAY = ifelse(WEATHER_DELAY != 0, 1, WEATHER_DELAY))
  
# remove flights for numeric airport codes
airports_df <- data.frame(read.csv("~/Documents/School/Springboard/IDSCapstone/Datasets/airports.csv")) %>% 
  select(IATA_CODE)
airports <- as.vector(airports_df$IATA_CODE) 
flights_df <- flights_df %>% filter(ORIGIN_AIRPORT %in% airports)

write.csv(flights_df, "~/Documents/School/Springboard/IDSCapstone/Datasets/flights_DW_clean.csv")


