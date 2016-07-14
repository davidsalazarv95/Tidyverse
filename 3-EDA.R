setwd(dir = "/Users/david_salazarv/Desktop/Tidyverse")
library(ggplot2)
library(hexbin)
library(plotly)
library(dplyr)
# EDA: Explore your data in a systematic way.


## How? Ask questions and follow them through more questions; visualize the answer to your questions.
## A good way to start EDA is to start studtying variation. Variation within the variables and their
## covariation. 

## To examine the distribution of a categorical variable, use a bar chart.

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) +
  theme_minimal() +
  coord_flip()

table(diamonds$cut)

## To examine the distribution of a continuous variable, use a histogram.

ggplot(data = diamonds) +
  geom_histogram(aes(x = carat), binwidth = 0.5) +
  theme_minimal()

# To study covariation between categorical and continuous variables, one option is 
# various histograms for different categories. But boxplots are preferred. 

# geom_freqpoly() makes a frequency polygon, a line that connects the tops of the bars that 
# would appear in a histogram.

# To fix the interval:

zoom <- coord_cartesian(xlim = c(55, 70))
ggplot(data = diamonds) +
  geom_freqpoly(aes(x = depth, color = cut), binwidth = 0.2) +
  zoom + theme_minimal()

## geom_density() plots a one dimensional kernel density estimate of a variable’s distribution. 
# The result is a smooth version of the information contained in a histogram or a frequency polygon

ggplot(data = diamonds) +
  geom_density(aes(x = depth, color = cut), adjust = 3) +
  zoom + theme_minimal()

# To study covariation between two continuous variables, scatterplots
str(diamonds)

ggplot(data = diamonds, mapping = aes(x = depth, y = price)) +
  geom_jitter(alpha = 0.1) +
  theme_minimal()

## Scatterplots become less useful as the size of your dataset grows, because points begin to 
# pile up into areas of uniform black (as above). 
# You can make patterns clear again with geom_bin2d(), geom_hex(), or geom_density2d().

#geom_bin2d() and geom_hex() divide the coordinate plane into two dimensional bins and 
# then use a fill color to display how many points fall into each bin. 
# geom_bin2d() creates rectangular bins. geom_hex() creates hexagonal bins.

# That is, you add a third dimension to the plot with the density of the rectangles

ggplot(data = diamonds) +
  geom_bin2d(aes(x = carat, y = price)) +
  theme_minimal()

ggplot(data = diamonds) +
  geom_hex(aes(x = carat, y = price)) +
  theme_minimal()

# geom_density2d() fits a 2D kernel density estimation to the data and then uses contour lines 
# to highlight areas of high density

ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  geom_density2d() +
  theme_minimal()


# To study variation between two categorical variables, geom_count
# The size of each circle in the plot displays how many observations occurred 
# at each combination of values. 

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color)) +
  theme_minimal()

table(diamonds$color, diamonds$cut)

# To study variation between categorical variables and continuous variables, use boxplots

ggplot(data = mpg) +
  geom_boxplot(aes(x = class, y = hwy)) +
  theme_minimal()

# To spot trends easier, reorder the x variables according to the corresponding mean of y within those
# variables

ggplot(data = mpg) +
  geom_boxplot(aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  theme_minimal()

# 3-d 


plot_ly(data = iris, x = Sepal.Length, y = Sepal.Width, z = Petal.Width, 
        color = Species, type = "scatter3d", mode = "markers")


# Clustering 

small_iris <- sample_n(iris, 50)

iris_hclust <- small_iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  dist() %>% 
  hclust(method = "complete")

plot(iris_hclust, labels = small_iris$Species)

(clusters <- cutree(iris_hclust, 3))

ggplot(small_iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color = factor(clusters))) +
  theme_minimal()

# K-means

iris_kmeans <- small_iris %>% 
  # Pass the dataframe with the numerical variables
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  kmeans(centers = 3, nstart = 20, iter.max = 50)

iris_kmeans$cluster

ggplot(small_iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color = factor(iris_kmeans$cluster))) +
  theme_minimal()

# dplyr functions allow you to compute measures for each cluster.
# Group them according to their cluster and compute

small_iris %>% 
  group_by(iris_kmeans$cluster) %>% 
  summarise(n_obs = n(), avg_width = mean(Sepal.Width), avg_length = mean(Sepal.Length))



