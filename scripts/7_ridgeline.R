# ----- Día 7: Ridgeline -----

# Preprocesamiento con QGIS:
# https://danielredondo.com/posts/20200125_joy_division/

# Se cargan los paquetes
library(ggplot2)
library(ggridges)
library(mapproj)

# Se lee el fichero .csv exportado de QGIS
data <- read.csv(file = "datasets/granada.csv", header = TRUE, sep = ",")
names(data)[2:4] <- c("Elev", "Lon", "Lat")

# Primera aproximación
ggplot(data, aes(x = Lon, y = Lat, group = Lat, height = Elev)) +
  geom_density_ridges(stat = "identity", scale = 30)

# Gráfico completo
ggplot(data, aes(x = Lon, y = Lat, group = Lat, height = Elev)) +
  geom_density_ridges(stat = "identity", scale = 30,
                      fill = "black", color = "white") +
  theme_void() + 
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  coord_map()

# Se guarda el gráfico
ggsave("7.png", width = 10, height = 8)