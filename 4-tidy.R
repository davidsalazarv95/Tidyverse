setwd(dir = "/Users/david_salazarv/Desktop/Tidyverse")
library(tidyr)
library(dplyr)

# Not all data layouts are equally useful to work with. R is vectorised; a dataframe is useful
# if the operations you perform on the data can be done with vectors. With each column being a vector
# for a specific key-value.
# Tidy data: every row is an observation
#            every column is a variable
#            every cell is a value

# It's worth the time to turn untidy data into tidy data. 

# Common problems: key-value mismatch

# In tidy data, each column name should display a key and the cells in the column the values.

# For example:

table2

# the key value contains two types of variable; thus, each row is not a different observation
# neither each column is a different variable. To turn key-value mismatch, use spread.

spread(table2, key, value)

# Now, each row is a different observation and each column is a different variable

# From wide to long. Or to put column names and put them into a key

table4

# Each row contains two observations, one for each year. Two columns represent the same key, that is
# year. 

gather(table4, "year", "cases", 2:3)

# The name of the new column that signifies the key that is originally duplicated; the name of the column
# to represent the cases, and the two columns for which this useful

# Now every row is an observation and every column is a different variable

table5

gather(table5, "year", "population", 2:3)


table3

separate(table3, rate, into = c("cases", "population"))

# You can use this arrangement to separate the last two digits of each year.

separate(table3, year, into = c("century", "year"), sep = 2)

# Case Example

View(who)
who
# From the 5th to the 60th variable, each variable encodes for different types of key variables:
# new old, type, sex and age
# Thus, there are multiple observations for each variable.

###
who <- gather(who, "code", "value", 5:60)
# Each of the 56 variables its treated as a special type of key of the code variable. With the cell
# they held as a cell in the value column
who
# Now, for each country-year combination we have 55 variables

7240*56 == nrow(who)

who <- separate(who, code, c("new", "var", "sexage"))
who
# Now we divide the key code into four type of variables: new, var, sexage and value
who <- separate(who, sexage, c("sex", "age"), sep = 1)
who
# let's further divide the key sexage into two vars

## Now, the key var holds many different keys. Let's spread those
who <- spread(who, var, value)
who

# Now, each row is an observation and each column is a variable. 
