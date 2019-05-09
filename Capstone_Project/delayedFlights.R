#install and load packages
install.packages( "tidyr")

library(dplyr)
library(tidyr)
library(ggplot2)

##load flights into dataframe
flights_df <- data.frame(read.csv("~/Documents/School/Springboard/IDSCapstone/Datasets/flights.csv", na.strings=c("","NA")))

# remove unneeded columns
novars <- names(flights_df) %in%
  c("TAIL_NUMBER", "TAXI_OUT", "WHEELS_OFF", "AIR_TIME", "DISTANCE", "WHEELS_ON", "TAXI_IN", "SCHEDULED_ARRIVAL", "ARRIVAL_TIME")
flights_df <- flights_df[!novars]

# remove canceled and diverted flights and unneeded columns 
flights_df <- flights_df %>% filter(CANCELLED == 0) %>% select(-c(CANCELLED, CANCELLATION_REASON))
flights_df <- flights_df %>% filter(DIVERTED == 0) %>% select(-DIVERTED)


# delay columns (AIR_SYSTEM_DELAY SECURITY_DELAY AIRLINE_DELAY LATE_AIRCRAFT_DELAY WEATHER_DELAY) contain a range of numbers
# Only concerned with the type of delay,if there was one; changing to 0 or 1  
##flights_df <- flights_df %>% 
##  mutate(AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY)) %>% 
##  mutate(AIR_SYSTEM_DELAY = ifelse(AIR_SYSTEM_DELAY != 0, 1, AIR_SYSTEM_DELAY)) %>%
##  mutate(SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY)) %>% 
##  mutate(SECURITY_DELAY = ifelse(SECURITY_DELAY != 0, 1, SECURITY_DELAY)) %>%
##  mutate(AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY)) %>% 
##  mutate(AIRLINE_DELAY = ifelse(AIRLINE_DELAY != 0, 1, AIRLINE_DELAY)) %>%
##  mutate(LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY)) %>% 
##  mutate(LATE_AIRCRAFT_DELAY = ifelse(LATE_AIRCRAFT_DELAY != 0, 1, LATE_AIRCRAFT_DELAY)) %>%
##  mutate(WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY)) %>% 
##  mutate(WEATHER_DELAY = ifelse(WEATHER_DELAY != 0, 1, WEATHER_DELAY))

flights_df <- flights_df %>% 
  mutate(AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY)) %>% 
  mutate(SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY)) %>% 
  mutate(AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY)) %>% 
  mutate(LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY)) %>% 
  mutate(WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY))
  

  
# remove flights for numeric airport codes
airports_df <- data.frame(read.csv("~/Documents/School/Springboard/IDSCapstone/Datasets/airports.csv", na.strings=c("","NA"))) %>% 
  select(IATA_CODE)
airports <- as.vector(airports_df$IATA_CODE) 
flights_df <- flights_df %>% filter(ORIGIN_AIRPORT %in% airports)

# create and order factor vars

flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 1] = 'Monday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 2] = 'Tuesday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 3] = 'Wednesday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 4] = 'Thursday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 5] = 'Friday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 6] = 'Saturday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 7] = 'Sunday'

flights_df$DAY_OF_WEEK <- ordered(flights_df$DAY_OF_WEEK, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

## merge year, month, day into one column ?transmute?

## ?gather delay reasons?

##flights_df$DAY_OF_WEEK <- as.factor(flights_df$DAY_OF_WEEK)
flights_df$MONTH <- as.factor(flights_df$MONTH)
    
### group by delay code and tally??

# keep top 50 airports only
flights_airport <- flights_df %>% group_by(ORIGIN_AIRPORT) %>% 
  tally %>% arrange(desc(n))
top_50 <- flights_airport[1:50,]
top_50 <- as.vector(top_50$ORIGIN_AIRPORT) 
flights_df <- flights_df %>% filter(ORIGIN_AIRPORT %in% top_50)



######
#late_flights <- 
#early_flights <-


write.csv(flights_df, "~/Documents/School/Springboard/IDSCapstone/Datasets/flights_DW_clean.csv")
