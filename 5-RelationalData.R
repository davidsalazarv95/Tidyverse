setwd(dir = "/Users/david_salazarv/Desktop/Tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(nycflights13)

airlines
# Carrier
airports
# faa
planes
# tailnum
weather


# Exercises

# You might expect that there’s an implicit relationship between plane and airline, 
# because each plane is flown by a single airline. Confirm or reject this hypothesis using data.

flights %>%
  select(tailnum, carrier) %>%
  group_by(tailnum) %>%
  summarise(count = n(),
            n_carrier = n_distinct(carrier)) %>%
  arrange(desc(n_carrier))

flights %>%
  filter(tailnum == "N146PQ")

flights %>%
  filter(tailnum == "N153PQ")

# The answer is nope. There are airplanes that are flown by more than one airline.

# Check if there are duplicated keys
# Count: create an extra column that tells you how many times a given combination of values arises.
planes %>% count(tailnum) %>% filter(n > 1)

weather %>% count(year, month, day, hour, origin) %>% filter(n > 1)

# No unique key

flights %>% filter(!is.na(tailnum)) %>%
  count(year, month, day, tailnum) %>% 
  filter(n > 1) %>%
  arrange(desc(n))

# Some airplanes flew five times in the same day

## Types of joins:

## Mutating joins: which add new variables to one data frame from matching rows in another.

## Filtering joins, which filter observations from one data frame based on whether or not they
## match an observation in the other table

## Set operations, which treat observations like they were set elements.

# Mutating join: add the airline name to flights

(flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier))

airlines
# Two columns: carrier and name. Join by the key carrier to flights 2: i.e.,
# add a variable to the dataframe.

flights2 %>%
  left_join(airlines, by = "carrier")

# More generally, there are two types of joins: inner and outer joins.
# Inner joins: the resulting join will only have rows that have a key in both dataframes
# Outer joins: keep observations that appear at least in one of the dataframes. 
#  left: keep the observations that appear in the left, even if they don't in the right.
#  right: keep the observations that appear in the right, even if they don't in the left.
#  full join: keep any observation in the resulting join. 

# If the key does not have the same name in both tables:

flights2 %>% 
  left_join(airports, by = c("dest" = "faa"))

## Exercises

## Compute the average delay by destination, then join on the airports data frame.

flights %>%
  select(year, month, day, arr_delay, carrier, dest) %>%
  group_by(dest) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  arrange(desc(avg_delay))

## Is there a relationship between the age of a plane and its delays?

planes %>% 
  mutate(age = 2013 - year) %>%
  select(tailnum, age, manufacturer) %>%
  right_join(flights, by = "tailnum") %>%
  filter(!is.na(age)) %>%
  group_by(tailnum, age) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(avg_delay < 100) %>%
  ggplot(mapping = aes(x = age, y = avg_delay)) +
  geom_hex() +
  geom_smooth()

## What weather conditions make it more likely to see a delay?

day_origin <- flights %>%
                group_by(year, month, day, origin) %>%
                summarise(avg_delay = mean(arr_delay, na.rm = TRUE))

weather_day <- weather %>%
                group_by(year, month, day, origin) %>%
                summarise_each(funs(mean), temp, humid, visib)

day_origin %>%
  left_join(weather_day, by = c("year", "month", "day", "origin")) %>%
  filter(avg_delay < 60) %>%
  ggplot(mapping = aes(x = avg_temp, y = avg_delay)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = lm)

day_origin %>%
  left_join(weather_day, by = c("year", "month", "day", "origin")) %>%
  filter(avg_delay < 60) %>%
  ggplot(mapping = aes(x = avg_humid, y = avg_delay)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = lm)

day_origin %>%
  left_join(weather_day, by = c("year", "month", "day", "origin")) %>%
  filter(avg_delay < 60) %>%
  ggplot(mapping = aes(x = avg_visib, y = avg_delay)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = lm)





## Filtering joins: 

## semi_join(x, y) keeps all observations in x that have a match in y.
## anti_join(x, y) drops all observations in x that have a match in y.

# Top ten popular destinations

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

# Which flights flew to these destinations? Use semi_join!

flights %>% semi_join(top_dest, by = "dest")

# Which flights don't have a match in planes?

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

## Exercises

## What does it mean for a flight to have a missing tailnum? 
## What do the tail numbers that don’t have a matching record in planes have in common? 
## (Hint: one variable explains ~90% of the problem.)

flights %>%
  filter(is.na(tailnum)) %>%
  select(year:day, arr_delay) %>%
  mutate(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))

## Those were canceled flights

flights %>%
  anti_join(planes, by = "tailnum") %>%
  mutate(mean = mean(dep_delay, na.rm = TRUE))
  
anti_join(flights, airports, by = c("dest" = "faa")) %>%
  select(dest)

anti_join(airports, flights, by = c("faa" = "dest"))

## Find the 48 hours (over the course of the whole year) that have the worst delays. 
# Cross-reference it with the weather data. Can you see any patterns?

worst_48 <- flights %>%
  group_by(year, month, day, hour) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay)) %>%
  head(48)

semi_join(weather, worst_48, by = c("year", "month", "day", "hour"))
  


