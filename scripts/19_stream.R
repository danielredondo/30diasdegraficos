library(dplyr)
# devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
library(reshape2)

# Referencia: https://www.r-graph-gallery.com/158-change-color-in-interactive-streamgraph.html

# Datos @datadista
datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19.csv") %>% 
  select(fecha, fallecimientos, casos_total) %>% 
  mutate(date = fecha)

# NA = 0
datos[is.na(datos$fallecimientos), "fallecimientos"] <- 0
datos[is.na(datos$casos_total), "casos_total"] <- 0

# Cálculo diarios:
datos$nuevos_fallecimientos <- 0
datos$nuevos_casos_total <- 0

for(i in 2:nrow(datos)){
  datos$nuevos_fallecimientos[i] <- datos$fallecimientos[i] - datos$fallecimientos[i - 1]
  datos$nuevos_casos_total[i] <- datos$casos_total[i] - datos$casos_total[i - 1]
}

# Preparación datos
datos <- datos %>%
  select_at(vars(date, starts_with("nuevos"))) %>% 
  melt

streamgraph(datos, "variable", "value", "date", interactive = TRUE, order = "reverse") %>%
  sg_fill_brewer("Set2") %>%
  sg_axis_x(1, "date", "%d-%m-%Y") %>% 
  sg_axis_y(14, "value") %>% 
  sg_annotate("Casos diarios", x = "2020-04-11", y = 1700, color = "#76BDA5", size = 20) %>% 
  sg_annotate("Defunciones diarias", x = "2020-04-11", y = 8100, color = "#F19169", size = 20) %>% 
  sg_annotate("COVID19: Casos y defunciones diarias en España", x = "2020-04-11", y = 9500, color = "black", size = 20)