# Paquetes
library(dplyr)
library(GGally)
library(viridis)
library(reshape2)

# Datos: https://www.kaggle.com/stefanoleone992/fifa-20-complete-player-dataset#players_20.csv
data <- read.csv(file = "../datasets/players_20.csv") %>% 
  filter(overall >= 80) %>% 
  select(Disparo = shooting,  Regate = dribbling, Velocidad = pace, Pase = passing,
         Físico = physic,  Defensa = defending, player_positions) %>% 
  filter(player_positions != "GK") 

# Quedarnos con la posición principal de los jugadores
for(i in 1:nrow(data)){
  data$player_positions[i] <- strsplit(data$player_positions[i], ",")[[1]][1]
}

# Pasar a defensas, mediocentros y atacantes
data[data$player_positions %in% c("CB", "LB", "RB", "LWB", "RWB"), "player_positions"] <- "Defensa"
data[data$player_positions %in% c("CDM", "CM", "CAM", "LM", "RM"), "player_positions"] <- "Mediocentro"
data[data$player_positions %in% c("CF", "ST", "LW", "RW"), "player_positions"] <- "Atacante"
data$player_positions <- ordered(factor(data$player_positions, levels = c("Defensa", "Mediocentro", "Atacante")))
table(data$player_positions)

# Cálculo de medias
medias <- data %>%
  group_by(player_positions) %>%
  summarize_all(mean) %>% 
  melt

# Plot
ggparcoord(data,
           scale = "globalminmax",
           columns = 1:6, groupColumn = 7,
           showPoints = F, 
           title = "Estadísticas de los jugadores de FIFA 20 por posición",
           alphaLines = 0.2,
           mapping = ggplot2::aes(size = .4, show_guide=TRUE)) + 
  ggplot2::scale_size_identity() + 
  geom_line(data = medias, mapping = aes(y = value, x = variable, group = player_positions, color = player_positions),
            size = 1.5, inherit.aes = FALSE) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous("Puntuación", breaks = seq(20, 100, 20), limits = c(20, 100)) + 
  labs(color = "Posición") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = .5, face = "bold"),
        axis.text = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        # Ampliar distancia entre categorías de la leyenda
        legend.key.size = unit(1, "cm"))

ggsave("29.png", width = 10, height = 8)