# ----- Día 25 - Violin plot ------
library(ggplot2)
library(dplyr)
library(scales)

# Fuentes
library(extrafont)
#font_import()
loadfonts(device = "win")

load("datasets/ense2017.RData")

# Quitar NS/NC y NAs
tabla <- tabla %>%
  filter(S109<998, !is.na(S109))

# En SEXOa sólo hay dos categorías:
tabla$sexo <- ifelse(tabla$SEXOa == 1, "Hombres", "Mujeres")

ggplot(tabla, aes(x = S109, y = sexo, fill = sexo, group = sexo)) + 
  geom_violin(width = 1.4, alpha = .6, trim = TRUE) +
  coord_flip() + 
  geom_boxplot(width = 0.15, color = "black", alpha = 0.5, outlier.size = 2, outlier.alpha = .1, outlier.shape = 16) +
  xlab("Altura en centímetros") +
  scale_x_continuous(breaks = seq(120, 200, 10)) +
  ggtitle("Altura por sexos en España, 2017",
          "Respuesta a la pregunta \"¿Podría decirme cuánto mide, aproximadamente, sin zapatos?\"") +
  theme_minimal() + 
  theme(legend.position= "none",
        text = element_text(family = "Perpetua"),
        axis.text = element_text(color = "black", size = 14),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = .5, size = 18),
        plot.subtitle = element_text(hjust = .5, size = 15))

ggsave("25.png", width = 8, height = 8)