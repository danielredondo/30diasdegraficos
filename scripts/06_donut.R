# ----- Día 6: Donut -----

# Referencia: https://www.r-graph-gallery.com/128-ring-or-donut-plot.html

library(ggplot2)
library(ggrepel)
library(babynames)
library(dplyr)
library(RColorBrewer)
library(grid)
library(gridExtra)

# Importar fuentes
library(extrafont)
#font_import()
loadfonts(device = "win")

# Procesamiento
babynames %>%
  filter(name == "Daniel") %>%
  mutate(decada = substr(year, 1, 3)) %>%
  group_by(decada) %>%
  summarise(count = sum(n)) %>%
  filter(decada != 201) -> data

# Compute percentages
data$fraction <- data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)
# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n = -1))
# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2
# Compute a good label
data$label <- paste0(data$decada, "0s\n", format(data$count, big.mark = "."), " bebés")

# Número de datos y colores
nb.cols <- nrow(data)
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# Donut plot
set.seed(1) # No es obligatorio, es para que las etiquetas salgan siempre en el mismo sitio
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = decada)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = labelPosition, label = label), size = 4, family = "Arial Rounded MT Bold",
                   max.overlaps = 5, label.size = NA, fill = NA, box.padding = 0, label.padding = 0,
                   direction = "y") +
  scale_fill_manual(values = mycolors) +
  coord_polar(theta = "y") +
  ggtitle("Número de bebés de Estados Unidos que se llaman 'Daniel' por década (1880-2009)") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(caption = "danielredondo.com\nFuente: {babynames}, USA Social Security Administration.") +
  theme(
    legend.position = "none",
    plot.caption = element_text(margin = margin(b = 20), size = 13, family = "Constantia"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "Constantia")
  )

# Exportar gráfico
ggsave("6.png", height = 8, width = 8)

