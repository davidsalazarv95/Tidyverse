setwd(dir = "/Users/david_salazarv/Desktop/Tidyverse")
library(dplyr)
library(ggplot2)
library(nycflights13)
# Notice that it is a tibble
flights
# Filter: allows you to subset operations
# For the dataframe flights, pick the observations for which month and day are both equal to 1
filter(flights, month == 1, day == 1)
# dplyr never change their inputs; assign to save the filtering
jan1 <- filter(flights, month == 1, day == 1)
# Different arguments are joined by &. To do differently, all in one same argument. 
filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11, 12))

# Exercises
# Summer flights
filter(flights, month %in% c(6,7,8))
# Flew to Houston (IAH or HOU)
filter(flights, origin %in% c('IAH','HOU') | dest %in% c('IAH','HOU'))
#Operated United, American or Delta
flights$carrier <- factor(flights$carrier)
summary(flights$carrier)
filter(flights, carrier %in% c('UA','AA','DL'))
# Delayed by more than two hours
filter(flights, dep_delay > 120 | arr_delay > 120)
# Arrived more than two hours late but didn't leave late
filter(flights, dep_delay < 5, arr_delay > 120)
# How many flights have a missing dep_time? What other variables are missing?
# What might these rows represent?
filter(flights, is.na(dep_time))

# Arrange: the verb to order rows by their ascending values in variables. If you
# provide more than one column name, each additional column will be used to break ties

arrange(flights, year, month, day)
# Use desc() to re-order by a column in descending order:
arrange(flights, desc(arr_delay))


# Exercises
# How could you use arrange() to sort all missing values to the start?
# Where are the missing values?
names(flights)[colSums(is.na(flights)) >0]

arrange(flights, desc(is.na(dep_time)),
          desc(is.na(dep_delay)),
          desc(is.na(arr_time)), 
          desc(is.na(arr_delay)),
          desc(is.na(tailnum)),
          desc(is.na(air_time)))

# Sort flights to find the most delayed flights. Find the flights that left earliest.
# Most delayed
arrange(flights, desc(dep_delay), desc(arr_delay))
# Left the earliest. i.e., smallest dep_delay
arrange(flights, dep_delay)




# Select: the verb to pick variables by their names. The whole dataset, only with these vars.
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# To change variable names
rename(flights, tail_num = tailnum)
# The LHS is the new name; the RHS the old name that you wanna change. 
rename(flights, departure_delay = dep_delay, arrival_delay = arr_delay)

# Brainstorm as many ways as possible to select dep_time, dep_delay, 
# arr_time, and arr_delay from flights.

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, dep_time:arr_delay)
select(flights, ends_with("delay"), starts_with("arr"), starts_with("sched"), starts_with("dep"))


# Mutate: the verb to create new variables with functions of existing variables
# Nice use of select!!
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
                      )
# New variables gain and speed.
# Remember: does not save anything, assign it to save the work
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
      )
# Note that you can refer to columns in mutate() that you’ve just created:
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
      )
# If you only want to keep the new variables, use transmute():
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)
# Modular arithmetic
transmute(flights,
          dep_time, # equivalent to keep in new dataframe that transmutes creates
          # dep_time is hour and then minutes. Last two digits, minutes
          hour = dep_time %/% 100, # keep the integer part of the division
          minute = dep_time %% 100 # keep the remainder part of division
)

'''
Cumulative and rolling aggregates: R provides functions for running sums, products, mins and maxes:
cumsum(), cumprod(), cummin(), cummax(). dplyr provides cummean() for cumulative means. 
If you need rolling aggregates (i.e. a sum computed over a rolling window), try the RcppRoll package.
'''


## Summarise: the verb to summarize a dataframe to a single row

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#That’s not terribly useful unless we pair it with group_by(). 
#This changes the unit of analysis from the complete dataset to individual groups. 
# When you use the dplyr verbs on a grouped data frame they’ll be automatically applied “by group”.
# Group all the observations that share the same year, month, and day.
by_day <- group_by(flights, year, month, day)
# Apply to each of these grouped dataframes summarize, collapse them to a single variable
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")
delay

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

#This code is a little frustrating to write because we have to give each intermediate data frame a name,
# even though we don’t care about it. Naming things well is hard, so this slows us down.

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# Take all the flights that weren't cancelled
not_cancelled <- filter(flights, !is.na(dep_delay), !is.na(arr_delay))
# Collapse all the flights that weren't cancelled by airplane and summarize them by using the average delay across flights that the airplane took
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(delays, aes(n, delay)) + 
  geom_point(alpha = 0.1)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
delays %>% 
  filter(n > 25) %>% 
  ggplot(aes(n, delay)) + 
  geom_point(alpha = 0.1)

batting <- tbl_df(Lahman::Batting)
# Take the dataframe, each row an attempt to bat. Group the attempts by player and collapse them by their
# average batting percentage. 
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H) / sum(AB),
    ab = sum(AB)
  )
# Take the guys who took more than 100 attempts and let's plot what happens with the average
# batting percentage as the number of attempts increase. 
batters %>% 
  filter(ab > 100) %>% 
  ggplot(aes(ab, ba)) +
  geom_point(alpha = 0.1) + 
  geom_smooth(se = FALSE, alpha = 0.2)

# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

# For each day, what was the proportion of flights that arrived delayed more than an hour
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60, na.rm = TRUE)) %>%
  arrange(desc(hour_perc)) %>%
  ggplot(aes(hour_perc))+
  geom_histogram(alpha = 0.6, bins = 40) +
  geom_density()
  
# Exercises

#Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights.
#Consider the following scenarios:

# Look at the number of cancelled flights per day. Is there are pattern? 
# Is the proportion of cancelled flights related to the average delay?

flights %>%
  group_by(year, month, day) %>%
  summarise(number_flights = n(), 
            cancelled = sum(is.na(dep_delay) | is.na(arr_delay))/number_flights,
            avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(cancelled < 0.5) %>%
  ggplot(aes(x = cancelled, y = avg_delay)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("Porcentaje de Viajes cancelados vs \n Porcentaje de viajes que se demoraron mas de 1 hora")

## Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs.
## bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))

# Which carrier has the worst delays?
# Let's collapse the flights by carrier reporting the average delay in arrival
flights %>%
  group_by(carrier) %>%
  summarise(number_flights = n(),
            avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay)) %>%
  ggplot(aes(x = carrier, y = avg_delay)) +
  geom_bar(stat = 'identity', aes(fill = carrier))

flights %>%
  group_by(carrier) %>%
  summarise(number_flights = n(),
            avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(number_flights > 1000) %>%
  ggplot(aes(y = avg_delay, x = number_flights)) +
  geom_point(aes(color = carrier)) +
  geom_smooth(se = FALSE)

# can you disentangle the effects of bad airports vs.
## bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest)
# %>% summarise(n()))

# Collapse the flights by carrier and destination and record the average delay for that carrier to that
# destination
flights %>%
  group_by(carrier, dest) %>%
  summarise(num_flights = n(), avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(num_flights > 200, rank(avg_delay) < 10) %>%
  arrange(desc(avg_delay))

?rank

# Note the process
flights %>% 
  # Group all the flights that had the same day
  group_by(year, month, day) %>%
  # And for each of those groups, only report the ten flights with the largest arr_delay
  filter(rank(arr_delay) < 10)

# Define a tibble for the most popular destinations
popular_dests <- flights %>% 
  # Take all the flights and group all of the flights that share the same destination
  group_by(dest) %>% 
  # For each of those groups, report the flights for the destinationos 
  # who had more than one flight per day
  filter(n() > 365)

# Take the flights who flew to a destination that received more than one flight per day
popular_dests %>% 
  # Take the flights that reported arriving with delay to their destination
  filter(arr_delay > 0) %>% 
  # Create a variable
  mutate(prop_delay = arr_delay / sum(arr_delay))

vignette("window-functions")


# Exercises
# Which plane (tailnum) has the worst on-time record?
tonda <- flights %>%
  group_by(tailnum) %>%
  filter(rank(arr_delay) == 1) %>%
  arrange(desc(arr_delay))











