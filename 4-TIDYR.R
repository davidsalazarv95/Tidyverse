setwd(dir = "/Users/david_salazarv/Desktop/Tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
'''
In tidy data:
              Each variable forms a column
              Each observation forms a row
              Each type of observational unit forms a table 
'''
# Tidying messy datasets

# Problem 1: Column headers are values, not variable names.


"---------------------------------------------------------------"
library(foreign)
library(stringr)
library(plyr)
library(reshape2)
source("xtable.r")
setwd(dir = "/Users/david_salazarv/GitHub/tidy-data/data")

# Data from http://pewforum.org/Datasets/Dataset-Download.aspx

# Load data -----------------------------------------------------------------

pew <- read.spss("pew.sav")
pew <- as.data.frame(pew)


religion <- pew[c("q16", "reltrad", "income")]
religion$reltrad <- as.character(religion$reltrad)
religion$reltrad <- str_replace(religion$reltrad, " Churches", "")
religion$reltrad <- str_replace(religion$reltrad, " Protestant", " Prot")
religion$reltrad[religion$q16 == " Atheist (do not believe in God) "] <- "Atheist"
religion$reltrad[religion$q16 == " Agnostic (not sure if there is a God) "] <- "Agnostic"
religion$reltrad <- str_trim(religion$reltrad)
religion$reltrad <- str_replace_all(religion$reltrad, " \\(.*?\\)", "")

religion$income <- c("Less than $10,000" = "<$10k", 
                     "10 to under $20,000" = "$10-20k", 
                     "20 to under $30,000" = "$20-30k", 
                     "30 to under $40,000" = "$30-40k", 
                     "40 to under $50,000" = "$40-50k", 
                     "50 to under $75,000" = "$50-75k",
                     "75 to under $100,000" = "$75-100k", 
                     "100 to under $150,000" = "$100-150k", 
                     "$150,000 or more" = ">150k", 
                     "Don't know/Refused (VOL)" = "Don't know/refused")[religion$income]

religion$income <- factor(religion$income, levels = c("<$10k", "$10-20k", "$20-30k", "$30-40k", "$40-50k", "$50-75k", 
                                                      "$75-100k", "$100-150k", ">150k", "Don't know/refused"))

counts <- count(religion, c("reltrad", "income"))
names(counts)[1] <- "religion"

xtable(counts[1:10, ], file = "pew-clean.tex")

# Convert into the form in which I originally saw it -------------------------

raw <- dcast(counts, religion ~ income)

"--------------------------------------------------------------------"
# Turn the data into tibble
raw <- as_data_frame(raw)
raw

# Problem 1: Column headers are values, not variable names. The real variable is income. Turn columns into
# rows


'''
The value- columns are converted into two variables: a new variable called column that contains 
repeated column headings and a new variable called value that contains the concatenated data values 
from the previously separate columns.
'''


raw %>%
  # Take the columns from 2:11 and put them as different instantiations of one key, income_bracket,
  # and their respective values put them in another column named percentage
  gather(key = "income_bracket", value = "freq", 2:11) %>%
  arrange(religion) %>%
  # Now it's easy to plot it 
  ggplot(mapping = aes(x = religion , y = freq, fill = income_bracket)) +
          geom_bar(stat = "identity", position = "dodge")
          

# Now, each observation is a row and each column is a variable

# Another example: Billboard

"-------------------------------------------------------------------------------------"
library(lubridate)
library(reshape2)
library(stringr)
library(plyr)
source("xtable.r")

raw <- read.csv("billboard.csv")
raw <- raw[, c("year", "artist.inverted", "track", "time", "date.entered", "x1st.week", "x2nd.week", "x3rd.week", "x4th.week", "x5th.week", "x6th.week", "x7th.week", "x8th.week", "x9th.week", "x10th.week", "x11th.week", "x12th.week", "x13th.week", "x14th.week", "x15th.week", "x16th.week", "x17th.week", "x18th.week", "x19th.week", "x20th.week", "x21st.week", "x22nd.week", "x23rd.week", "x24th.week", "x25th.week", "x26th.week", "x27th.week", "x28th.week", "x29th.week", "x30th.week", "x31st.week", "x32nd.week", "x33rd.week", "x34th.week", "x35th.week", "x36th.week", "x37th.week", "x38th.week", "x39th.week", "x40th.week", "x41st.week", "x42nd.week", "x43rd.week", "x44th.week", "x45th.week", "x46th.week", "x47th.week", "x48th.week", "x49th.week", "x50th.week", "x51st.week", "x52nd.week", "x53rd.week", "x54th.week", "x55th.week", "x56th.week", "x57th.week", "x58th.week", "x59th.week", "x60th.week", "x61st.week", "x62nd.week", "x63rd.week", "x64th.week", "x65th.week", "x66th.week", "x67th.week", "x68th.week", "x69th.week", "x70th.week", "x71st.week", "x72nd.week", "x73rd.week", "x74th.week", "x75th.week", "x76th.week")]
names(raw)[2] <- "artist"

raw$artist <- iconv(raw$artist, "MAC", "ASCII//translit")
raw$track <- str_replace(raw$track, " \\(.*?\\)", "")
names(raw)[-(1:5)] <- str_c("wk", 1:76)
raw <- arrange(raw, year, artist, track)

long_name <- nchar(raw$track) > 20
raw$track[long_name] <- paste0(substr(raw$track[long_name], 0, 20), "...")
"-------------------------------------------------------------------------------------"

billboard <- raw
(billboard <- as_data_frame(billboard))

# The only columns that are not values are year, artist, track time, date-entered. Thus:

billboard %>%
  # Melt all the value-columns in one variable named week, that records the cell values in
  # a column named rank
  gather(key = "week", value = "rank", 6:81) %>%
  arrange(year, artist, track, time) %>%
  filter(!is.na(rank))

## Problem 2: Multiple variables are stored in one column

"--------------------------------------------------------------------------------"
options(stringsAsFactors = FALSE)

# Load -----------------------------------------------------------------------
raw <- read.csv("tb.csv", na.strings = "")
raw$new_sp <- NULL
raw <- subset(raw, year == 2000)
names(raw)[1] <- "country"

names(raw) <- str_replace(names(raw), "new_sp_", "")
raw$m04 <- NULL
raw$m514 <- NULL
raw$f04 <- NULL
raw$f514 <- NULL
"--------------------------------------------------------------------------------"

(tuberculosis <- as_tibble(raw))

## Problem 2: Multiple variables are stored in one column

# First problem, many columns are values and not variables. Thus, use gather

tuberculosis %>%
  gather(key = "demographic", value = "cases", 3:18) %>%
  # Second problem, in demographic there are two variables: sex and age
  # Use separate with a customized sep
  separate(col = demographic, into = c("Sex","Age"), sep = 1)

## Problem 3: Variables are stored in both rows and columns

"------------------------------------------------------------------------------"
library(stringr)
library(reshape2)
library(plyr)
source("xtable.r")
source("read-fwf.r")

options(stringsAsFactors = FALSE)

# Define format for fixed width file
cols <- data.frame(
  name =  c("id", "year", "month", "element"),
  start = c(1,     12,    16,      18),
  end =   c(11,    15,    17,      21))

names <- str_c(c("value", "mflag", "qflag", "sflag"), rep(1:31, each = 4), sep = "_")
starts <- cumsum(c(22, rep(c(5, 1, 1, 1), 31)))
starts <- starts[-length(starts)]
ends <- c(starts[-1], starts[length(starts)] + 1) - 1

values <- data.frame(name = names, start = starts, end = ends)
cols <- rbind(cols, values)

# Load data and subset to small example
raw <- read.fwf2("weather.txt",  cols)
raw <- subset(raw, year == 2010 & element %in% c("TMIN", "TMAX")) 
raw <- raw[, c(1:4, which(str_detect(names(raw), "value")))]
raw$id <- str_c(str_sub(raw$id, 1, 2), str_sub(raw$id, -5, -1))

names(raw)[-(1:4)] <- str_c("d", 1:31)
raw[raw == -9999] <- NA
raw[-(1:4)] <- raw[-(1:4)] / 10 
rownames(raw) <- NULL
raw$element <- tolower(raw$element)
"------------------------------------------------------------------------------"

(weather <- as_tibble(raw))

## Problem 3: Variables are stored in both rows and columns

# First, all the day columns are value-columns. We must use gather.

weather %>%
  # First, put the value-columns into one column name day, and store their values as temperature
  gather(key = "day", value = "temperature", 5:35) %>%
  mutate(day = as.integer(str_replace(day, "d", "")), date = as.Date(ISOdate(year, month, day))) %>%
  select(id, element, temperature, date) %>%
  filter(!is.na(temperature)) %>%
  arrange(date, temperature) %>%
  # Second, the variable element hides two types of keys: the max temperature and min. 
  # We must use spread to spread those keys into columns
  spread(key = element, value = temperature)

# Now, the data is tidy. Each row is an observation, and each column is a variable



