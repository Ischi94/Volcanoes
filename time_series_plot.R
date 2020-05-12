# I want to have a time series plot with temperature on top and vei on the bottom

# load libraries
library(tidyverse)

# load temperature anomalie data
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')

# load volcano eruption data
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')


# first, produce the temperature anomalie plot
ggplot(tree_rings, aes(year, europe_temp_index)) + 
  geom_line(aes(colour = europe_temp_index)) +
  scale_colour_gradient2(low = "darkblue", mid = "grey95" , 
                         high = "darkred", midpoint = 0) +
  labs(x = "Year (CE)", y = "Temperature anomaly \n(°C relative to 1961–1990)") +
  theme_minimal() +
  theme(legend.position = "none")

# then the Volcano Explosivity Index (0-8)
eruptions_large <- eruptions %>% 
  # select only only the worst eruptions (>= large) 
  # and subset it to the same range as temp-data
  filter(vei >= 6 & start_year >= 0 & start_year <= 2000)

ggplot(eruptions_large) +
  geom_point(aes(start_year, y = 0, size = vei), alpha = 0.2)

