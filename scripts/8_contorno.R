# ----- Día 8: Countour plot -----

# Referencia:
# https://stackoverflow.com/questions/38070742/how-to-reorder-discrete-colors-in-legend-in-ggplot2

# Carga de paquetes
library(ggplot2)
library(RColorBrewer)

x <- seq(-3, 3, .01)
y <- seq(-3, 3, .01)
xyz.func <- function(x, y) {
  x^2 + y^2
}
gg <- expand.grid(x = x, y = y)
gg$z <- with(gg, xyz.func(x, y)) 
brks <- cut(gg$z, breaks = c(0, 0.1, 0.5, 1.5, 2, 3, 5, 7))
brks <- gsub(",", " - ", brks, fixed = TRUE)
gg$brks <- gsub("\\(|\\]", "", brks)
ggplot(gg, aes(x, y)) +
  geom_tile(aes(fill = brks), show.legend = F) +
  scale_fill_manual("Z", values = c("#F22D03", "#F79707", "#F22D03", "#DD5C02", "#F22D03", "#B600AB", "#F22D03")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed() +
  theme(legend.key = element_blank()) +
  theme_void()
ggsave("8.png", width = 8, height = 8)

# Gráficos extra:

# Gráfico antes de retocar theme()
ggplot(gg, aes(x, y)) +
  geom_tile(aes(fill = brks)) + 
  scale_fill_manual("Z", values = c("#F22D03", "#F79707", "#F22D03", "#DD5C02", "#F22D03", "#B600AB", "#F22D03")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()
ggsave("8_pre.png", width = 8, height = 8)

# Gráfico pixelado
x <- seq(-3, 3, .1) # note finer grid
y <- seq(-3, 3, .1)
xyz.func <- function(x, y) {
  x^2 + y^2
}
gg <- expand.grid(x = x, y = y)
gg$z <- with(gg, xyz.func(x, y))

brks <- cut(gg$z, breaks = c(0, 0.1, 0.5, 1.5, 2, 3, 5, 7))
brks <- gsub(",", " - ", brks, fixed = TRUE)
gg$brks <- gsub("\\(|\\]", "", brks)
ggplot(gg, aes(x, y)) +
  geom_tile(aes(fill = brks), show.legend = F) +
  scale_fill_manual("Z", values = c("#F22D03", "#F79707", "#F22D03", "#DD5C02", "#F22D03", "#B600AB", "#F22D03")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed() +
  theme(legend.key = element_blank()) +
  theme_void()
ggsave("8_8bits.png", width = 8, height = 8)