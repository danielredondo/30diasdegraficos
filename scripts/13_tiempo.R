# ----- Día 13: Tiempo -----

# Modificación de https://danielredondo.com/posts/20200219_calendario_laboral/
# Inspirado en https://analisisydecision.es/graficos-de-calendarios-con-series-temporales/

# Carga de paquetes
library(lubridate)
library(dplyr)
library(openair)
library(lattice)

# Datos @datadista
datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19.csv") %>% 
  select(date = fecha, fallecimientos) %>% 
  mutate(date = as.Date(date)) %>% 
  # Quitar febrero
  filter(month(date) != 2)

# Cálculo nuevos fallecimientos
datos[is.na(datos$fallecimientos), "fallecimientos"] <- 0
datos$nuevos_fallecimientos <- 0
for(i in 2:nrow(datos)){
  datos$nuevos_fallecimientos[i] <- datos$fallecimientos[i] - datos$fallecimientos[i - 1]
}

# Guardar calendario
png("13.png", width = 10, height = 3.5, units = "in", res = 300)
calendarPlot(datos,
             pollutant = "nuevos_fallecimientos",
             # Título
             main = "Defunciones diarias por coronavirus en España\nMarzo-Mayo, 2020",
             # Para que el calendario empiece en lunes
             w.shift = 2,
             limits = c(0, max(datos$nuevos_fallecimientos)),
             # Colores para los eventos (del 0 al 3)
             cols = c("white", "darkred"),
             key.header = "Defunciones diarias")

# Leyenda (ajustar x e y)
panel.text(x = 2650, y = 950, labels = "@dredondosanchez\nDatos de @datadista",
           col = "black", font = "bold", cex = 0.8)
dev.off()
