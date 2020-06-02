# Paquetes
library(sunburstR)
library(schrute)
library(RColorBrewer)

theoffice %>%
  filter(character %in% c("Michael", "Dwight", "Jim", "Pam", "Andy")) %>% 
  mutate(season_character = paste0("S", season, "-", character)) %>% 
  group_by(season_character) %>% 
  summarise(n = n()) %>%
  arrange(season_character) -> datos

sunburst(datos,
         legendOrder = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "Michael", "Dwight", "Jim", "Pam", "Andy"),
         withD3 = TRUE,
         colors = list(range = c(colorRampPalette(c("orange", "green"))(4),
                                 colorRampPalette(c("firebrick1", "royalblue4"))(9)),
                       domain = datos$season_character))
