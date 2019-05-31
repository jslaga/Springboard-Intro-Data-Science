#install and load packages
install.packages( "tidyr")
install.packages("randomForest")
install.packages("maps")

library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(scales)
library(maps)

setwd("~/Documents/School/Springboard/IDSCapstone/")

##load flights into dataframe
flights_df <- data.frame(read.csv("Datasets/flights.csv", na.strings=c("","NA")))
total_flights <- nrow(flights_df)

# remove unneeded columns
novars <- names(flights_df) %in%
  c("TAIL_NUMBER", "TAXI_OUT", "WHEELS_OFF", "TAXI_OUT", "AIR_TIME", "DISTANCE", "WHEELS_ON", "TAXI_IN", "SCHEDULED_ARRIVAL", "ARRIVAL_TIME")
flights_df <- flights_df[!novars]

# remove canceled and diverted flights and unneeded columns 
flights_df <- flights_df %>% filter(CANCELLED == 0) %>% select(-c(CANCELLED, CANCELLATION_REASON))
flights_df <- flights_df %>% filter(DIVERTED == 0) %>% select(-DIVERTED)


# Fill in NA for Delay types
flights_df <- flights_df %>% 
  mutate(AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY)) %>% 
  mutate(SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY)) %>% 
  mutate(AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY)) %>% 
  mutate(LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY)) %>% 
  mutate(WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY))

# remove flights for numeric airport codes
airports_df <- data.frame(read.csv("Datasets/airports.csv", na.strings=c("","NA"))) %>% 
  select(IATA_CODE)
airports <- as.vector(airports_df$IATA_CODE) 
flights_df <- flights_df %>% filter(ORIGIN_AIRPORT %in% airports)

# keep top 50 airports only
flights_airport <- flights_df %>% group_by(ORIGIN_AIRPORT) %>% 
  tally %>% arrange(desc(n))
top_50 <- flights_airport[1:50,]
top_50 <- as.vector(top_50$ORIGIN_AIRPORT) 
flights_df <- flights_df %>% filter(ORIGIN_AIRPORT %in% top_50)

# Create variable - DELAYED and DAYPART 
flights_df <- flights_df %>% 
  mutate(DELAYED = if_else(DEPARTURE_DELAY > 0 , 1, 0)) %>%
  mutate(DAYPART = case_when(SCHEDULED_DEPARTURE < 500 ~ 'Night',
                             SCHEDULED_DEPARTURE < 1200 ~ 'Morning',
                             SCHEDULED_DEPARTURE < 1700 ~ 'Afternoon',
                             SCHEDULED_DEPARTURE < 2100 ~ 'Evening',
                             TRUE ~ 'Night'))
flights_df$DELAYED <- as.factor(flights_df$DELAYED)
flights_df$DAYPART <- as.factor(flights_df$DAYPART)

# create and order factor vars
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 1] = 'Monday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 2] = 'Tuesday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 3] = 'Wednesday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 4] = 'Thursday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 5] = 'Friday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 6] = 'Saturday'
flights_df$DAY_OF_WEEK[flights_df$DAY_OF_WEEK == 7] = 'Sunday'

flights_df$DAY_OF_WEEK <- ordered(flights_df$DAY_OF_WEEK, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

flights_df$MonthName <- flights_df$MONTH
flights_df$MonthName[flights_df$MonthName == 1] = 'January'
flights_df$MonthName[flights_df$MonthName == 2] = 'February'
flights_df$MonthName[flights_df$MonthName == 3] = 'March'
flights_df$MonthName[flights_df$MonthName == 4] = 'April'
flights_df$MonthName[flights_df$MonthName == 5] = 'May'
flights_df$MonthName[flights_df$MonthName == 6] = 'June'
flights_df$MonthName[flights_df$MonthName == 7] = 'July'
flights_df$MonthName[flights_df$MonthName == 8] = 'August'
flights_df$MonthName[flights_df$MonthName == 9] = 'September'
flights_df$MonthName[flights_df$MonthName == 10] = 'October'
flights_df$MonthName[flights_df$MonthName == 11] = 'November'
flights_df$MonthName[flights_df$MonthName == 12] = 'December'

flights_df$MonthName <- ordered(flights_df$MonthName,levels =
                                    c("January", "February", "March", "April", "May", "June", 
                                      "July", "August", "September",  "October", "November", "December"))

write.csv(flights_df, "Datasets/flights_DW_clean.csv")


late_flights <- filter(flights_df, DEPARTURE_DELAY > 0)
ontime_flights <- filter(flights_df, DEPARTURE_DELAY <= 0)

summary(late_flights)
str(late_flights)

##examine delays by day, month, airline
total_by_day <- flights_df %>% group_by(DAY_OF_WEEK) %>% tally
total_by_month <- flights_df %>% group_by(MonthName) %>% tally
total_by_airline <- flights_df %>% group_by(AIRLINE) %>% tally
##  ??NAME COLUMSN???*************
late_by_day <- late_flights %>% group_by(DAY_OF_WEEK) %>% tally 
late_by_month <- late_flights %>% group_by(MonthName) %>% tally
late_by_airline <- late_flights %>% group_by(AIRLINE) %>% tally

total_by_day <- left_join(late_by_day, total_by_day, by = "DAY_OF_WEEK") %>% arrange(desc(n.x))
total_by_month <- left_join(late_by_month, total_by_month, by = "MonthName") %>% arrange(desc(n.x))
total_by_airline <- left_join(late_by_airline, total_by_airline, by = "AIRLINE") %>% arrange(desc(n.x))

total_by_day
total_by_month
total_by_airline

total_by_daypart <- late_flights %>% group_by(DAYPART) %>% tally

## flights by day of week
ggplot(late_flights, aes(x = DAY_OF_WEEK)) + 
  geom_bar()

# Days of month
ggplot(late_flights, aes(x = MonthName)) + 
  geom_bar()
  
  
##examine only top 10 airlines; US has incomplete data; used AS instead
included <- c("WN", "DL", "UA", "AA", "OO", "EV", "B6", "AS", "MQ", "NK") 
late_flights <- late_flights %>% filter(AIRLINE %in% included)


## distribution of departure delays
ggplot(late_flights, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15) +
  scale_x_continuous(limits = c(0, 500))

ggplot(late_flights, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15) +
  scale_x_continuous(limits = c(0, 500)) +
  facet_grid(cols = vars(AIRLINE))

delay_totals <- late_flights %>% group_by(DEPARTURE_DELAY) %>% 
  tally %>% arrange(desc(n))
# direct correlation between length of departure delay and frequency

ggplot(late_flights, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()
# a majority of the delays are brief; American Airlines has the longest delays

##examine short and long delays
short <- filter(late_flights, DEPARTURE_DELAY < 60)
extreme <- filter(late_flights, DEPARTURE_DELAY >600) #10+ hours


ggplot(short, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()

ggplot(extreme, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()

##look at weather delays
##scaled to normalize across the airlines regardless of number of flights
weather_delays <- late_flights %>% subset(WEATHER_DELAY > 0) %>% group_by((MONTH), AIRLINE) %>%
  summarise(Total_Time = sum(WEATHER_DELAY))

weather_delays$Proportion <- weather_delays$Total_Time/sum(weather_delays$Total_Time) ##scale

ggplot(weather_delays) +
  geom_line(aes(x=weather_delays$`(MONTH)`, y=weather_delays$Proportion, color = AIRLINE)) +
  labs(title = "Weather Delays By Month", x = "Month", y = "Minutes Delayed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,vjust=0.5), 
        plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(breaks = pretty_breaks(12)) +
  facet_wrap(~AIRLINE)

#######
summer_delays <- late_flights %>% subset(WEATHER_DELAY > 0) %>% subset(MONTH %in% c(5, 6, 7, 8))

nonsummer_delays <- late_flights %>% subset(WEATHER_DELAY > 0) %>% subset(!MONTH %in% c(5, 6, 7, 8))

delay_df <- late_flights %>% subset(WEATHER_DELAY > 0) %>% mutate(Season = ifelse(MONTH %in% c(5, 6, 7, 8), "summer", "winter"))

ggplot(delay_df, aes(delay_df$Season, delay_df$WEATHER_DELAY)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,150))

## t test averages between summer and non summer delays
t.test(summer_delays$WEATHER_DELAY, nonsummer_delays$WEATHER_DELAY)
## while there are more delays in summer, there's no sig diff in average delay througout the year; 
## p-value = 0.09289 ; ~10% chance that pops are same 



#############################################################
##where are the airports with the delays?
##looking at SW summer months  May. june July
WN_summer <- late_flights %>% subset(WEATHER_DELAY > 0) %>% subset(AIRLINE == "WN") %>% 
  subset(MONTH %in% c(5, 6, 7, 8)) %>% group_by(ORIGIN_AIRPORT) %>%
  summarise(Total_Time = sum(WEATHER_DELAY)) 

target_airlines <- c("WN", "DL", "UA", "AA", "OO", "EV", "B6", "AS", "MQ", "NK")

target_df <- flights_df %>% filter(AIRLINE %in% target_airlines)
target_df$LAT <- NA
target_df$LONG <- NA

air_dat <- read.delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",header=FALSE,sep=",")
# V7 lat, V8 long, v5 airport

# replace over groups of airports
airports <- unique(target_df$ORIGIN_AIRPORT)

for (i in 1:length(airports)) {
  airport <- as.character(airports[i])
  air_lat <- air_dat$V7[air_dat$V5 == airport] #checking lat for airport
  air_long <- air_dat$V8[air_dat$V5 == airport] #checking long for airport
  
  target_df$LAT[target_df$ORIGIN_AIRPORT == airport] <- air_lat
  target_df$LONG[target_df$ORIGIN_AIRPORT == airport] <- air_long
}

###MAPS
map_df <- target_df %>% subset(WEATHER_DELAY > 0) %>% group_by(ORIGIN_AIRPORT, LAT, LONG) %>%
  summarise(Total_Time = sum(WEATHER_DELAY))

us <- map_data("state")

ggplot() +  
  geom_map(aes(x = long, y = lat, map_id = region), data = us,
           map = us, fill = "88", size = 0.15, color = "black") +
  geom_point(data = map_df, 
             aes(x = LONG, y = LAT,  colour = map_df$Total_Time/sum(map_df$Total_Time)),
             shape = 16) +
  scale_color_gradient(low="beige", high="blue")


## Linear regression models
model1 <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY, data=late_flights)
summary(model1)

model2 <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY + DEPARTURE_DELAY:DAYPART, data=late_flights)
summary(model2)

model3 <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY + DAYPART + DAY_OF_WEEK, data=flights_df)
summary(model3)


#Split the data into training dataset and testing dataset with a 75/25 ratio.
set.seed(666)
split <- sample(nrow(late_flights), floor(0.75*nrow(late_flights)))
Final_train <- late_flights[split,]
Final_test <- late_flights[-split,]

TrainModel<- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY + DAYPART, data=Final_train)

prediction <- predict(TrainModel, Final_test)
plot(prediction, Final_test$ARRIVAL_DELAY)


