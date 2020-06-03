# ----- Día 9: Áreas apiladas -----

# Paquetes
library(readr)
library(dplyr)
library(ggplot2)
library(reshape)

# Procesamiento
estadisticas <- read_delim("datasets/comites.txt", delim = ";") %>%
  as.data.frame %>%
  melt(id.vars = c("anio", "ciudad")) %>%
  mutate(Género = factor(ifelse(substring(variable, 3)=="M", "Mujeres", "Hombres"), levels = c("Mujeres", "Hombres")),
         comite = substring(variable, 1, 2),
         edicion = paste(substring(ciudad, 1, 3), anio))
estadisticas$comite[estadisticas$comite == "CC"] <- "Comité científico"
estadisticas$comite[estadisticas$comite == "CO"] <- "Comité organizador"

# Gráfico
ggplot(data = estadisticas, aes(x = anio, y = value, fill = Género)) +
  facet_wrap(~ toupper(estadisticas$comite)) +
  geom_area() + 
  ggtitle("Distribución por género de los comités de las Jornadas de Usuarios de R en España",
          "@dredondosanchez") + 
  scale_x_continuous(name = "Sede y año", breaks = 2009:2019, minor_breaks = FALSE,
                     labels = estadisticas$edicion[1:11]) + 
  scale_fill_manual(values = c("firebrick1", "forestgreen")) +
  ylab("Número de personas") +
  theme(plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray50", linetype = 2),
        axis.ticks.y = element_line(color = "gray50"),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 8))

ggsave("9.png", width = 17, height = 7)
