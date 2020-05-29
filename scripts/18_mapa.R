# Referencia: https://ggplot2tutor.com/streetmaps/streetmaps/ 

library(tidyverse)
library(osmdata)

# Get coordinates for ggplot
getbb("Granada, España")

streets <- getbb("Granada, España") %>%
  opq() %>%
  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb("Granada, España") %>%
  opq() %>%
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Granada, España") %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# Con río y pequeñas calles
ggplot() +
  geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = "#ffbe7f", size = .4, alpha = .8) +
  geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = "#ffbe7f", size = .2, alpha = .8) +
  geom_sf(data = river$osm_lines, inherit.aes = FALSE, color = "#7fc0ff", size = .8, alpha = .5) +
  coord_sf(xlim = c(-3.65, -3.55), ylim = c(37.13, 37.225), expand = FALSE) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "#282828"))

# Puede tardar
ggsave("18.png", width = 6, height = 6)
