# ----- Día 1: barras ----- 

# Carga de paquetes
library(ggplot2)
library(dplyr)

# Importación de ENSE a RData -> https://danielredondo.com/posts/20181010_ens/
load("datasets/ense2017.RData")

# Quitar NS/NC y NAs de las variables altura y peso
tabla <- tabla %>% filter(S110<998) %>% filter(!is.na(S110))
tabla <- tabla %>% filter(S109<998) %>% filter(!is.na(S109))

# Cálculo de IMC
tabla <- tabla %>% mutate(imc = 10000 * S110/(S109^2))

# Resumen de IMC
summary(tabla$imc)

# Gráfico
ggplot(tabla, aes(x = imc)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), col = "black",
                 fill = "firebrick", binwidth = 1) + 
  ggtitle("Índice de masa corporal en adultos (Encuesta Nacional de Salud, 2017)") + 
  scale_x_continuous(name = "Índice de masa corporal", breaks = seq(10, 70, 5),
                     labels = seq(10, 70, 5), limits = c(10, 70)) +
  scale_y_continuous(name = "Porcentaje de la población", labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold")
  )

ggsave("1.png")