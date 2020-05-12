# ----- Día 2: líneas -----
library(pageviews)
library(ggplot2)

# Leyenda no en formato científico
options(scipen = 10000)

# Búsqueda R En Wikipedia
datos <- article_pageviews(project = "es.wikipedia",
                           article = "R (lenguaje de programación)",
                           start =  as.Date("2018-01-01"),
                           end = Sys.Date() - 1)

ggplot(datos, aes(x = date, y = views)) +
  geom_line(col = "firebrick") +
  ggtitle("Número de visitas diarias a la página de R en la Wikipedia en español") + 
  ylab("Número de visitas") +
  xlab("Fecha") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))

ggsave("2.png")