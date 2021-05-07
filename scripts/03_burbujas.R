# ----- Día 3: Burbujas -----
# https://danielredondo.com/posts/20180215_vis_navidad/
library(readr)
library(ggplot2)
library(scales)

# Para fuentes
library(extrafont)
# font_import()
loadfonts(device = "win")

# Lectura fichero
loteria <- read_delim("datasets/loteria.csv",
                      delim = ";",
                      escape_double = FALSE,
                      col_names = FALSE,
                      col_types = cols(
                        `X1` = col_integer(),
                        `X2` = col_integer(),
                        `X3` = col_skip()
                      ), trim_ws = TRUE
)
names(loteria) <- c("numero", "premio")

# Gráfico
point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)

ggplot(loteria, aes(loteria$numero, loteria$premio)) +
  geom_point(
    size = sqrt(loteria$premio / 5000),
    col = "firebrick3"
  ) +
  geom_text(
    data = subset(loteria, loteria$premio > 59000),
    aes(numero, premio, label = point(numero)),
    col = "white", family = "Forte",
    size = sqrt(subset(loteria, loteria$premio > 59000)$premio) / 250
  ) +
  scale_y_continuous(
    name = "Premio (€)", labels = point, limits = c(0, 4800000),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "Número", labels = point, limits = c(0, 102000),
    breaks = seq(0, 100000, 10000), minor_breaks = seq(0, 100000, 10000),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Forte"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(size = 15)
  )

# Exportar gráfico
ggsave("3.png", width = 8, height = 5)
