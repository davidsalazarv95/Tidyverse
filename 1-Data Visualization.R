pkgs <- c(
  "broom", "dplyr", "ggplot2", "jsonlite", "Lahman", "purrr", 
  "rcorpora", "readr", "rmarkdown", "stringr", "tibble", "tidyr"
)
install.packages(pkgs)
library(ggplot2)
## Scatterplots 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
# A third dimension to a 2 dimension graph through aesthetics
# : a visual property of your graph, e.g.: color, size, transparency..

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = year))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = manufacturer))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cyl, alpha = cyl))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = year))
  


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv)) +
  theme_minimal()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = drv)) + 
  geom_smooth(mapping = aes(linetype = drv, color = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth(data = subset(mpg, class == "subcompact"))
# Automatically, it'll put the same mapping into every geom
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point() + 
  geom_smooth()

# Bar graph
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, alpha = cut))

# Stacked bar graph
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# Compare relative proportions across discrete
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") +
  ggtitle('Position = "fill"')

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") +
  ggtitle('Position = "dodge"')

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter") + 
  ggtitle('Position = "jitter"')

ggplot(data = diamonds) + 
  geom_count(mapping = aes(x = cut, y = clarity, color = clarity))

ggplot(data = diamonds) + 
  geom_count(mapping = aes(x = cut, y = clarity, size = ..prop.., 
                           group = clarity, color = clarity))
## A grid of plots
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = clarity)) + facet_wrap(~cut)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~drv)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)