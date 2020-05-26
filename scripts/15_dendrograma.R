# ----- Día 15: Drendogra... dendroga... dendogra... gráfico de árbol! -----

# http://larmarange.github.io/JLutils/reference/A2Rplot.html

library(dplyr)
# devtools::install_github("larmarange/JLutils")
library(JLutils)

# Datos: https://www.kaggle.com/stefanoleone992/fifa-20-complete-player-dataset#players_20.csv
datos <- read.csv("datasets/players_20.csv", encoding = "UTF-8") %>% 
  filter(club %in% c("Real Madrid", "Granada CF")) %>% 
  filter(player_positions != "GK") 

datos_normalizados <- datos %>% 
  select(overall, potential) %>%
  scale

Club <- datos %>% select(club) %>% t %>% as.factor()
levels(Club) <- c("GRA", "RMA")
distancias <- dist(datos_normalizados, method = "euclidian")
h <- hclust(distancias)
h$labels <- datos %>% select(short_name) %>% t %>% as.vector

# Pequeñas modificaciones de A2Rplot - Disponible en https://gist.github.com/danielredondo/8dfdf88c6af9c34a296979e9233b016f
# source("15_dendrograma_funcion_auxiliar.R")
A2Rplot(h, k = 2, fact.sup = Club, boxes = F, col.up = "gray50",
        knot.pos = "mean",
        col.down = c("firebrick1", "darkblue"),
        main = "Dendrograma jugadores Real Madrid y Granada CF\nClustering (k = 2) realizado en base a estadísticas de puntuación y potencial en FIFA20")

# Save plot
png("15.png", width = 8, height = 5, units = "in", res = 600)
A2Rplot(h, k = 2, fact.sup = Club, boxes = F, col.up = "gray50",
        knot.pos = "mean",
        col.down = c("firebrick1", "darkblue"),
        main = "Dendrograma jugadores Real Madrid y Granada CF\nClustering (k = 2) realizado en base a estadísticas de puntuación y potencial en FIFA20")
dev.off()
