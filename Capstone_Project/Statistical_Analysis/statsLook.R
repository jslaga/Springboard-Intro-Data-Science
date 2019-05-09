#install and load packages
install.packages( "tidyr")
install.packages("randomForest")

library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)

##load flights into dataframe
flights_df <- data.frame(read.csv("~/Documents/School/Springboard/IDSCapstone/Datasets/flights_DW_clean.csv"))

late_flights <- filter(flights_df, DEPARTURE_DELAY > 0)
early_flights <- filter(flights_df, DEPARTURE_DELAY < 0)

summary(late_flights)
str(late_flights)

###############

## distribution of departure delays
ggplot(flights_df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15)

ggplot(late_flights, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15)

## flights by day of week
late_flights$DAY_OF_WEEK <- ordered(late_flights$DAY_OF_WEEK, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(late_flights, aes(x = DAY_OF_WEEK)) + 
  geom_bar()

late_by_day <- late_flights %>% group_by(DAY_OF_WEEK) %>% 
  tally %>% arrange(desc(n))
late_by_day <- mutate(late_by_day, perc = late_by_day$n/nrow(late_flights)*100)

# Saturday has least delays; Thurs, Mon, Fri most and very close

# Days of month
ggplot(late_flights, aes(x = MONTH)) + 
  geom_bar()

late_by_month <- late_flights %>% group_by(MONTH) %>% 
  tally %>% arrange(desc(n)) 
late_by_month <- mutate(late_by_month, perc = late_by_month$n/nrow(late_flights)*100)

#### split into time of day

model1 <- glm(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE + DAY_OF_WEEK, data=late_flights)
model1

model2 <- glm(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE, data=late_flights)
model2

model3 <- lm(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE + DAY_OF_WEEK, data=late_flights)
model3

model4 <- lm(DEPARTURE_DELAY ~ AIRLINE  + SCHEDULED_DEPARTURE, data=late_flights)
model4

model5 <- randomForest(DEPARTURE_DELAY ~ AIRLINE + ORIGIN_AIRPORT + SCHEDULED_DEPARTURE + DAY_OF_WEEK, data=late_flights, nodesize=25, ntree = 200)
