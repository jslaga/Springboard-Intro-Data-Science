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
flights_df <- data.frame(read.csv("Datasets/flights_DW_clean.csv"))

late_flights <- filter(flights_df, DEPARTURE_DELAY > 0)
early_flights <- filter(flights_df, DEPARTURE_DELAY < 0)

summary(late_flights)
str(late_flights)

###############
## distribution of departure delays
ggplot(late_flights, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15)

ggplot(late_flights, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()
# a majority of the delays are brief



#short vs long delays - what types of delays cause longest delay?
short <- filter(late_flights, DEPARTURE_DELAY < 60)
extreme <- filter(late_flights, DEPARTURE_DELAY >900)

ggplot(short, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()

ggplot(extreme, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()

#weather_delays <- short %>% subset(WEATHER_DELAY > 0) %>% group_by((MONTH)) %>%
#  tally %>% arrange(desc(n))
#plot(weather_delays$DEPARTURE_DELAY, weather_delays$MONTH)

weather_delays <- late_flights %>% subset(WEATHER_DELAY > 0) %>% group_by((MONTH), AIRLINE) %>%
  summarise(Total_Time = sum(WEATHER_DELAY))

ggplot(weather_delays) +
  geom_line(aes(x=weather_delays$`(MONTH)`, y=weather_delays$Total_Time, color = AIRLINE)) +
  labs(title = "Weather Delays By Month", x = "Month", y = "Minutes Delayed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,vjust=0.5), 
          plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(breaks = pretty_breaks(12)) +
  facet_wrap(~AIRLINE)

##where are the airports with the delays?

## 
target_airlines <- c("AA", "DL", "WN")
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
           map = us, fill = "#ffffff", size = 0.15, color = "black") +
  geom_point(data = map_df, 
             aes(x = LONG, y = LAT, alpha = map_df$Total_Time/sum(map_df$Total_Time), fill = 'red'),
             shape = 1) 
  




## flights by day of week
late_flights$DAY_OF_WEEK <- ordered(late_flights$DAY_OF_WEEK, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(late_flights, aes(x = DAY_OF_WEEK)) + 
  geom_bar()

late_by_day <- late_flights %>% group_by(DAY_OF_WEEK) %>% 
  tally %>% arrange(desc(n))
late_by_day <- mutate(late_by_day, perc = late_by_day$n/nrow(late_flights)*100)

### Saturday has least delays; Thurs, Mon, Fri most and very close

# Days of month
ggplot(late_flights, aes(x = MONTH)) + 
  geom_bar()

late_by_month <- late_flights %>% group_by(MONTH) %>% 
  tally %>% arrange(desc(n)) 
late_by_month <- mutate(late_by_month, perc = late_by_month$n/nrow(late_flights)*100)

# weather delays
####grouping by amount of delay may be good - short delay vs longer ones


weather_delays <- late_flights %>% subset(WEATHER_DELAY > 0) %>% group_by((MONTH)) %>%
  tally %>% arrange(desc(n))
plot(weather_delays$DEPARTURE_DELAY, weather_delays$MONTH)






#### ??split into time of day

#### FLIGHTS OF BAY AREA - SFO, OAK, SJO (late flights)
ba_flights <- late_flights %>% filter(ORIGIN_AIRPORT %in% c("OAK", "SFO", "SJO"))

plot(ba_flights$DEPARTURE_DELAY, ba_flights$DAY_OF_WEEK)




model1 <- glm(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE + DAY_OF_WEEK, data=late_flights)
model1

model2 <- glm(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE, data=late_flights)
model2

model3 <- lm(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE + DAY_OF_WEEK, data=late_flights)
model3

model4 <- lm(DEPARTURE_DELAY ~ AIRLINE  + SCHEDULED_DEPARTURE, data=late_flights)
model4

model5 <- randomForest(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE + DAY_OF_WEEK, data=ba_flights, nodesize=25, ntree = 200)
