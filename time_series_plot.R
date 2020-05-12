# I want to have a time series plot with temperature on top and vei on the bottom

# load libraries
library(tidyverse)
library(ggimage)
library(biscale)

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
eruptions_smr <- eruptions %>% 
  filter(start_year >= 0 & start_year <= 2000) %>% 
  group_by(start_year) %>% 
  summarise(n_eruptions = n(), mean_vei = mean(vei, na.rm = TRUE)) %>% 
  mutate(mean_vei = replace_na(mean_vei, 0))

eruptions_smr %>% 
  bi_class(x = mean_vei, y = n_eruptions , style = "quantile", dim = 3) %>%
  ggplot() +
  geom_rect(aes(ymin = 0, ymax = 0.1, xmin = start_year, xmax = start_year + 1, 
                fill = bi_class)) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  coord_cartesian(ylim = c(0,1)) +
  bi_theme(base_size = 12, font_color = "#262626")

  

# bi_class(x = production, y = per_capita , style = "quantile", dim = 3) %>%
#   ggplot() +
#   geom_sf(aes(geometry=geometry, fill = bi_class),
#           color = "white", size = 0.1, show.legend = FALSE) +
#   bi_scale_fill(pal = "GrPink", dim = 3) +
#   labs(title = "Can you keep pace?",
#        subtitle = "Beer production and per-capita consumption, USA 2018", 
#        caption = "Alcohol and Tobacco Tax and Trade Bureau (TTB)\n
#        USAtoday\n
#        Gregor Mathes 2020") +
#   bi_theme(base_size = 12, font_color = "#262626") +
#   
#  geom_raster(aes(start_year, 1, fill = mean_vei)) 
