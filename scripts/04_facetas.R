# ----- Día 4: Facetas ----- 

library(ggplot2)
library(dplyr)

#devtools::install_github("schochastics/Rokemon")
library(Rokemon)
# Si salen warnings con las fuentes:
# 1. Correr Rokemon::import_pokefont()
# 2. Ir a donde se descarga la fuente e instalarla
# 3. Importar las fuentes: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2

# Fuentes
library(extrafont)
font_import()
loadfonts(device = "win")

read.csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv",
         encoding = "UTF-8") %>%
  ggplot(aes(x = ataque, y = defensa)) +
  facet_wrap(~ tipo_1) + 
  geom_point(color = "darkblue", alpha = .5, size = 2.5, stroke = 0, shape = 16) +
  ylab("Defensa") + 
  xlab("Ataque") + 
  ggtitle("Relación ataque-defensa según tipo principal de Pokémon") +
  theme_gameboy() + 
  theme(plot.title = element_text(hjust = .5))

ggsave("4.png", width = 12, height = 8)