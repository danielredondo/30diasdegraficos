# ----- Día 13: Tiempo -----

# Este script es otra aproximación al gráfico de calendario.
# El original está hecho con {openair}, en este script se usa {calendR}

# Más info sobre calendR: https://r-coder.com/calendarios-r/

# Carga de paquetes
library(lubridate)
library(dplyr)
library(calendR)

# Datos @datadista
datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19.csv") %>% 
  select(date = fecha, fallecimientos) %>% 
  mutate(date = as.Date(date)) %>% 
  # Seleccionar marzo, abril y mayo de 2020
  filter(month(date) %in% c(3,4,5) & year(date) == 2020)

# Cálculo nuevos fallecimientos
datos[is.na(datos$fallecimientos), "fallecimientos"] <- 0
datos$nuevos_fallecimientos <- 0
for(i in 2:nrow(datos)){
  datos$nuevos_fallecimientos[i] <- ifelse(datos$fallecimientos[i] - datos$fallecimientos[i - 1] >= 0,
                                           datos$fallecimientos[i] - datos$fallecimientos[i - 1],
                                           0)
}

png("13_2.png", width = 10, height = 5, units = "in", res = 300)
calendR(from = "2020-03-01",  # Start date
        to = "2020-05-31",    # End date
        start = "M",          # Start on Mondays
        special.days = datos$nuevos_fallecimientos,
        gradient = TRUE,   # Needed to create the heat map
        special.col = "darkred",
        legend.pos = "right",
        legend.title = "Defunciones diarias",
        title = "Defunciones diarias por COVID-19 en España\n Marzo-Mayo, 2020\nHecho con {calendR} de @RCoderWeb",
        low.col = "white")
dev.off()