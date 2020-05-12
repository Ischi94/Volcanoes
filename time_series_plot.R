# I want to have a time series plot with temperature on top and vei on the bottom

# load libraries
library(tidyverse) # data processing and visualisation 
library(biscale) # bivariate filling 
library(cowplot) # plot alignment

# load temperature anomalie data 
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')

# load volcano eruption data
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')


# first, produce the temperature anomalie plot
temperature <- ggplot(tree_rings, aes(year, europe_temp_index)) + 
  geom_line(aes(colour = europe_temp_index)) +
  scale_colour_gradient2(low = "darkblue", mid = "grey95" , 
                         high = "darkred", midpoint = 0) +
  labs(x = "Year (CE)", y = "Temperature anomaly \n(°C relative to 1961–1990)", 
       caption = "Data: https://doi.org/10.1038/nature14565\nGregor Mathes\n12.05.2020") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-2, 1.5), xlim = c(0,2000)) +
  scale_y_continuous(breaks = c(-1, 0, 1))
  
  

# then a time series showing number of eruptions and mean eruption index
eruptions_smr <- eruptions %>% 
  filter(start_year >= 0 & start_year <= 2000) %>% 
  group_by(start_year) %>% 
  summarise(n_eruptions = n(), mean_vei = mean(vei, na.rm = TRUE)) %>% 
  mutate(mean_vei = replace_na(mean_vei, 0)) %>% 
  # add bivariate classes
  bi_class(x = mean_vei, y = n_eruptions , style = "quantile", dim = 3) %>% View()
  ggplot() +
  geom_rect(aes(ymin = -2, ymax = -1.75, xmin = start_year, xmax = start_year + 1, 
                fill = bi_class)) +
  # fill based on bi_class
  bi_scale_fill(pal = "GrPink", dim = 3) +
  coord_cartesian(ylim = c(-2, 1.5), xlim = c(0,2000)) +
  theme_minimal() + 
  theme(legend.position = "none", panel.grid = element_blank(), 
        axis.text = element_blank())

# make bivariate legend for eruption time series
map_legend <- bi_legend(pal = "GrPink", dim = 3) + 
  theme(panel.background = element_rect(fill = "transparent",
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  theme_void() +
  coord_fixed(ylim = c(0,4.5), xlim = c(0,4.5)) +
  annotate(geom = "text", y = 0.25, x = 2, label = "Explosivity →", size = 1.5) +
  annotate(geom = "text", y = 2, x = 0, angle = 90, label = "Eruptions →", size = 1.5)

# combine each plot, make sure that each axis is well aligned
combined_plot <- ggdraw() +
  draw_plot(temperature, 0, 0, 1, 1) +
  draw_plot(map_legend, 0.15, 0.02, 0.2, 0.2) +
  draw_plot(eruptions_smr, 0.07, 0.1, 0.935, 1.4) 

# save it
ggsave(combined_plot, filename = "eruptions_through_time.png")
