# ----- Día 12: Lollipop -----

library(dplyr)
library(ggplot2)

# Cambiar símbolo decimal a coma
options(OutDec = ",")

# Datos
read.csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv",
         encoding = "UTF-8") %>%
  filter(direccion == "Pedro Almodóvar") %>% 
  # Renombrar películas en inglés
  mutate(titulo = replace(titulo, titulo == "Talk to Her", "Hable con ella")) %>%
  mutate(titulo = replace(titulo, titulo == "Broken Embraces", "Los abrazos rotos")) %>%
  mutate(titulo = replace(titulo, titulo == "Bad Education", "La mala educación")) %>%
  mutate(titulo = replace(titulo, titulo == "All About My Mother", "Todo sobre mi madre")) %>%
  mutate(titulo = replace(titulo, titulo == "Live Flesh", "Carne trémula")) %>%
  # Corregir errata IMDb
  mutate(titulo = replace(titulo, titulo == "Mujeres al borde de un ataque de \"nervios\"",
                          "Mujeres al borde de un ataque de nervios")) %>%
  # Crear etiqueta
  mutate(pelicula = as.factor(paste0(titulo, " (", anio, ")"))) %>% 
  # Ordenar por puntuación
  arrange(puntaje) %>% 
  mutate(pelicula = factor(pelicula, levels = pelicula)) %>%
  # Gráfico
  ggplot(aes(x = pelicula, y = puntaje)) +
  geom_segment(aes(xend = pelicula, yend = 0)) +
  geom_point(size = 4, color = "firebrick") +
  # Etiqueta
  geom_text(aes(x = pelicula, y = puntaje, label = format(puntaje, digits = 2)),
            color = "firebrick", nudge_y = .35, nudge_x = .07,
            fontface = "bold") + 
  coord_flip() +
  xlab("") +
  scale_y_continuous(name = "Puntuación", expand = c(0, 0), limits = c(0, 10)) + 
  ggtitle("Puntuación en IMDb de las películas de Pedro Almodóvar") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", linetype = 2),
        axis.line.y = element_line(color = "black"),
        plot.margin = margin(.1, .3, .1, .1, unit = "in"),
        plot.title = element_text(hjust = .5, face = "bold"))

# Guardar gráfico
ggsave("12.png", width = 10, height = 6)
