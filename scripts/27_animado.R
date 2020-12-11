# Paquetes
library(ggplot2)
library(gganimate)
library(transformr)
library(gifski)

# Datos: Tasas de paro (INE) https://www.ine.es/jaxiT3/Datos.htm?t=4247
paro <- read.csv2("datasets/paro.csv", encoding = "UTF-8")
paro$trimestre_temporal <- rep(1:73, 2)
paro$Sexo <- factor(paro$sexo)
paro

# Sin animar
ggplot(paro, aes(x = trimestre_temporal, as.numeric(paro), group = Sexo, col = Sexo)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(name = "Tasa de paro", breaks = seq(0, 30, 5), limits = c(0, 30)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(name = "", breaks = seq(1, 73, 4), labels = paste0(2002:2020, "-T1")) +
  theme_light() +
  ggtitle("Tasa de paro en España por sexos, 2002-2020") + 
  theme(legend.position = "top",
        text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = .5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

grafico <- ggplot(paro, aes(x = trimestre_temporal, as.numeric(paro), group = Sexo, col = Sexo)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(name = "Tasa de paro", breaks = seq(0, 30, 5), limits = c(0, 30)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(name = "", breaks = seq(1, 73, 4), labels = paste0(2002:2020, "-T1")) +
  theme_light() +
  ggtitle("Tasa de paro en España por sexos, 2002-2020") +
  theme(legend.position = "top",
        text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = .5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Here comes the gganimate code
  transition_reveal(trimestre_temporal)

#grafico


# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
library(beepr) # Para avisar cuando se haya generado el gráfico
animate(grafico, width = 2000, height = 1600, res = 300, duration = 8, renderer = gifski_renderer("27.gif"))
beep(8)
