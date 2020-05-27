# ----- Día 16 - waffle -----

library(dplyr)
# devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
library(stringr)
library(colorspace)

# Para fuentes
library(extrafont)
#font_import()
loadfonts(device = "win")

nombres <- c("Profesor", "Alicia", "Salva", "Berlín", "Lisboa", "Moscú", "Tokio", "Nairobi", "Denver", "Tamayo",
             "Estocolmo", "Arturo", "Arturito", "Murillo", "Raquel", "Helsinki", "Oslo", "Bogotá", "Palermo", "Marsella", "Alison")

for(i in 1:length(nombres)){
  # Leer datos
  datos <- read.csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv",
                    encoding = "UTF-8") %>% 
    # Agrupar: nº de veces que se dice "profesor" por episodio
    mutate(menciones = str_count(texto, pattern = fixed(nombres[i], ignore_case = TRUE))) %>% 
    group_by(temporada, episodio) %>% 
    summarise(menciones = sum(menciones))
  
  table(datos$menciones)
  
  ggplot(datos, aes(temporada, episodio, fill = menciones)) + 
    geom_waffle(col = "black", size = 1) + 
    scale_fill_gradient("Número de veces", low = "white", high = "firebrick1") +
    geom_label(aes(label = menciones), label.size = 0, family = "Bahnschrift") +
    ggtitle(toupper(paste0("La Casa de Papel: ", nombres[i])),
            paste0("Número de veces que se menciona \"", nombres[i], "\" por capítulo.\nGráficos por @dredondosanchez")) +
    scale_x_continuous("Temporada", breaks = 1:3) + 
    scale_y_continuous("Episodio", breaks = 1:9) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(), 
          panel.background = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(),
          axis.ticks.length = unit(0, "null"),
          plot.background = element_blank(), 
          panel.spacing = unit(c(0, 0, 0, 0), "null"),
          plot.title = element_text(size = 30, hjust = .5, face = "bold"),
          axis.text.y = element_text(size = 13, color = "black", face = "bold", margin = margin(r = -15)),
          axis.text.x = element_text(size = 13, color = "black", face = "bold", margin = margin(t = -15)),
          plot.subtitle = element_text(hjust = .5, size = 15),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.position = "none",
          text = element_text(family = "Bahnschrift"))
  ggsave(paste0("16_", nombres[i], ".png"), height = 8, width = 8, dpi = 300)
}


# caso especial - Río o Rio
# Leer datos
datos <- read.csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv",
                  encoding = "UTF-8") %>% 
  # Agrupar: nº de veces que se dice "profesor" por episodio
  mutate(menciones = str_count(texto, pattern = fixed("Rio", ignore_case = TRUE)) + 
           str_count(texto, pattern = fixed("Río", ignore_case = TRUE))) %>% 
  group_by(temporada, episodio) %>% 
  summarise(menciones = sum(menciones))

table(datos$menciones)

ggplot(datos, aes(temporada, episodio, fill = menciones)) + 
  geom_waffle(col = "black", size = 1) + 
  scale_fill_gradient("Número de veces", low = "white", high = "firebrick1") +
  geom_label(aes(label = menciones), label.size = 0, family = "Bahnschrift") +
  ggtitle(toupper(paste0("La Casa de Papel: ", "Río")),
          paste0("Número de veces que se menciona \"", "Río", "\" por capítulo.\nGráficos por @dredondosanchez")) +
  scale_x_continuous("Temporada", breaks = 1:3) + 
  scale_y_continuous("Episodio", breaks = 1:9) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        plot.background = element_blank(), 
        panel.spacing = unit(c(0, 0, 0, 0), "null"),
        plot.title = element_text(size = 30, hjust = .5, face = "bold"),
        axis.text.y = element_text(size = 13, color = "black", face = "bold", margin = margin(r = -15)),
        axis.text.x = element_text(size = 13, color = "black", face = "bold", margin = margin(t = -15)),
        plot.subtitle = element_text(hjust = .5, size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none",
        text = element_text(family = "Bahnschrift"))
ggsave(paste0("16_", "Río", ".png"), height = 8, width = 8, dpi = 300)