# Paquetes
library(ggplot2)

# Para fuentes
library(extrafont)
#font_import()
loadfonts(device = "win")

# Datos: Defunciones por cáncer de páncreas (C25) en España, 1999-2018
# Fuente: Ministerio de Sanidad, Consumo y Bienestar Social
defunciones <- read.csv2("datasets/defunciones_pancreas.csv")
names(defunciones) <- c("Sexo", "año", "def")

ggplot(defunciones, aes(x = año, y = def, fill = Sexo)) + 
  geom_hline(yintercept = seq(0, 7500, 1500), colour = "black", size = .5, lty = 2) +
  geom_col(width = 1.0, alpha = 1, col = "black") +
  scale_y_continuous(name = "Número de defunciones", breaks = seq(0, 7500, 1500), limits = c(0, 7500)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(name = "", breaks = 1999:2018, labels = 1999:2018) +
  theme_light() +
  # Añadir coordenadas polares
  coord_polar() + 
  ggtitle("Defunciones por cáncer de páncreas en España, 1999-2018",
          "Fuente: Ministerio de Sanidad, Consumo y Bienestar Social") +
  theme(legend.position = "top",
        text = element_text(color = "black", family = "Franklin Gothic Medium"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#063672"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = .5, face = "bold"),
        plot.subtitle = element_text(hjust = .5, lineheight = 1.3),
        axis.title.y = element_text(hjust = .80, face = "bold"),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(margin = margin(-.2, -.2, -.2, -.2, unit = "in")))

ggsave("30.png", width = 8, height = 8)
