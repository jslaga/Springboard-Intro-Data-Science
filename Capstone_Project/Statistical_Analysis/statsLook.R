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

##examine delays by day, month, airline
late_by_day <- late_flights %>% group_by(DAY_OF_WEEK) %>% 
  tally %>% arrange(desc(n))
late_by_month <- late_flights %>% group_by(MonthName) %>% 
  tally %>% arrange(desc(n))
late_by_airline <- late_flights %>% group_by(AIRLINE) %>% 
  tally %>% arrange(desc(n))

late_by_day
late_by_month
#seems to have more late flights during months of high travel - summer, spring break, holidays

late_by_airline
#southwest has most delays followeded by delta, united and AA - are these simply the airlines with the most flights?
#are most of these small delays that may not impact travel time too much?

included <- c("WN", "DL", "UA", "AA", "OO", "EV", "B6", "US", "MQ", "NK")
late_flights <- late_flights %>% filter(AIRLINE %in% included)


## distribution of departure delays
ggplot(late_flights, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15)

ggplot(late_flights, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15) +
  facet_grid(cols = vars(AIRLINE))

delay_totals <- late_flights %>% group_by(DEPARTURE_DELAY) %>% 
  tally %>% arrange(desc(n))
# This follows almost exactly in order!!

ggplot(late_flights, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()
# a majority of the delays are brief; American Airlines has the longest delays

#short vs long delays
short <- filter(late_flights, DEPARTURE_DELAY < 60)
extreme <- filter(late_flights, DEPARTURE_DELAY >600) #10+ hours


ggplot(short, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()

ggplot(extreme, aes(AIRLINE, DEPARTURE_DELAY)) +
  geom_boxplot()
#southwest has most delayed flights, but has very few extreme delays; while AA has less than half the 
#number of delayed flights, but has a lot (and by far the longest) delays

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
## From comparing the airlines over the year: AA, DL, and to some extent UA and OO have spikes in Feb and Dec 
## and summer May-Aug; WN has spike May-Aug; scaled to normalize across the airlines regardless of number of flights

summer_delays <- late_flights %>% subset(WEATHER_DELAY > 0) %>% subset(MONTH %in% c(5, 6, 7, 8))
  
nonsummer_delays <- late_flights %>% subset(WEATHER_DELAY > 0) %>% subset(!MONTH %in% c(5, 6, 7, 8))

## t test averages between summer and non summer delays
t.test(summer_delays$WEATHER_DELAY, nonsummer_delays$WEATHER_DELAY)
## while there are more delays in summer, there's no sig diff in average delay througout the year; p-value = 0.09289
## ~10% chance that pops are same 

delay_df <- late_flights %>% subset(WEATHER_DELAY > 0) %>% mutate(Season = ifelse(MONTH %in% c(5, 6, 7, 8), "summer", "winter"))

ggplot(delay_df, aes(delay_df$Season, delay_df$WEATHER_DELAY)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,150))
  


##where are the airports with the delays?
##looking at SW summer months  May. june July
WN_summer <- late_flights %>% subset(WEATHER_DELAY > 0) %>% subset(AIRLINE == "WN") %>% 
  subset(MONTH %in% c(5, 6, 7, 8)) %>% group_by(ORIGIN_AIRPORT) %>%
  summarise(Total_Time = sum(WEATHER_DELAY)) 
##
  

## 
#target_airlines <- c("AA", "DL", "WN")
target_airlines <- c("WN", "DL", "UA", "AA", "OO", "EV", "B6", "US", "MQ", "NK")

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
  
# fill color and title for key (+labels?)?



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








#### ??split into time of day

#### FLIGHTS OF BAY AREA - SFO, OAK, SJO (late flights)
ba_flights <- late_flights %>% filter(ORIGIN_AIRPORT %in% c("OAK", "SFO", "SJO"))

plot(ba_flights$DEPARTURE_DELAY, ba_flights$DAY_OF_WEEK)



model3 <- lm(DEPARTURE_DELAY ~ DEPARTURE_TIME + ELAPSED_TIME, data=late_flights)
model3

#linear regression 
model3 <- lm(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE + DAY_OF_WEEK, data=late_flights)
summary(model3)



model4 <- lm(DEPARTURE_DELAY ~ AIRLINE  + SCHEDULED_DEPARTURE, data=late_flights1)
model4


#model5 <- lm(DEPARTURE_DELAY ~ AIRLINE  + SCHEDULED_DEPARTURE, data=late_flights)
#model5

late_flights1 <- late_flights %>% filter(ORIGIN_AIRPORT %in% c("SFO")) %>% filter(DEPARTURE_DELAY < 750)

model5 <- lm((ARRIVAL_DELAY-DEPARTURE_DELAY) ~ DEPARTURE_DELAY, data = late_flights1)
summary(model5)

qplot(DEPARTURE_DELAY, ARRIVAL_DELAY-DEPARTURE_DELAY, data = late_flights1)
##coloring dots for column of neg vs pos difference
## ggplot
