# ----- Día 17 - Sankey -----

# Referencia: https://github.com/corybrunson/ggalluvial
library(ggalluvial)
library(dplyr)
library(ggplot2)

# Fuente datos:
# Pág 123 de http://www.congreso.es/docu/pge2019/pge_2019-tomos/PGE-ROM/doc/L_19_A_A1.PDF
datos <- read.csv2("datasets/presupuestos.csv", encoding = "UTF-8",
                   col.names = c("principal", "secundaria", "presupuesto"))

datos$presupuesto <- as.numeric(datos$presupuesto)

# Creación categoría "Otros"
datos[datos$presupuesto < 5000, "secundaria"] <- "Otros"

datos <- datos %>% arrange(-presupuesto)

# A factor
datos$principal <- factor(datos$principal, levels = unique(datos$principal), ordered = T)
datos$secundaria <- factor(datos$secundaria, levels = unique(datos$secundaria), ordered = T)

# Leyenda no en formato científico
options(scipen = 10000)

ggplot(data = datos,
       aes(axis1 = principal, axis2 = secundaria, y = presupuesto)) +
    scale_x_discrete(limits = c("Política principal", "Política secundaria"),
                     expand = c(.2, .05)) +
    ylab("Presupuesto (en millones de euros)") +
    geom_alluvium(col = "black", aes(fill = principal), show.legend = FALSE) +
    geom_stratum() +
    geom_text(stat = "stratum", infer.label = TRUE, cex = 3) +
    theme_minimal() +
    ggtitle("Presupuestos Generales del Estado, España, 2019",
            "Presupuesto en millones de euros") +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          axis.text = element_text(color = "black"))

ggsave("17.png", width = 15, height = 8)
