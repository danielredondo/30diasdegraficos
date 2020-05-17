# ----- Día 6: Donut -----

# Referencia: https://www.r-graph-gallery.com/128-ring-or-donut-plot.html

library(ggplot2)
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
data$label <- paste0(data$decada, "0s\n", data$count)

# Número de datos
nb.cols <- nrow(data)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# Donut plot
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = decada)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 3, family = "Tahoma") +
  scale_fill_manual(values = mycolors) +
  coord_polar(theta = "y") +
  ggtitle("Número de bebés de Estados Unidos que se llaman 'Daniel' por década (1880-2009)") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(caption = "Fuente: USA social security administration") +
  theme(
    legend.position = "none",
    plot.caption = element_text(margin = margin(b = 40), family = "Candara"),
    plot.title = element_text(hjust = 0.5, family = "Palatino Linotype")
  )

ggsave("6.png", height = 8, width = 8)