# ----- Día 14: Treemap -----

# Referencia: https://github.com/wilkox/treemapify

# install_github("wilkox/treemapify")
library(treemapify)
# install_github("tiagomendesdantas/Rspotify")
library(Rspotify)
library(httpuv)
library(dplyr)
library(ggplot2)
library(devtools)
library(grid)

# Para fuentes
library(extrafont)
# font_import()
loadfonts(device = "win")

# Manual Rspotify: https://github.com/tiagomendesdantas/Rspotify
# !! Es necesario registrarse como desarrollador en Spotify, para obtener estos 3 parámetros:
app_name <- "<INSERTAR>"
client_id <- "<INSERTAR>"
client_secret <- "<INSERTAR"
keys <- spotifyOAuth(app_name, client_id, client_secret)

# Extremoduro - La ley innata
# Encontrar ID 
searchArtist("Extremoduro", token = keys)
# Encontrar ID disco
getAlbums("3bgsNtcf5d5h9jbQbohfBK", token = keys)
# Extraer información disco
extremo <- getAlbum("1GYJUlbVr5FuNU7awwMGzu", token = keys) %>% 
  mutate(name = unlist(name), duration_ms = unlist(duration_ms))

ggplot(extremo, aes(area = duration_ms, label = name)) + 
  geom_treemap(fill = "#DBDCE1", colour = "black", size = 1.5) + 
  geom_treemap_text(colour = "black", reflow = T,
                    place = "centre", size = 15, family = "Castellar") + 
  geom_treemap_text(aes(label = paste0(round(duration_ms / 1000, 0), " segundos")),
                    colour = "black", reflow = T, place = "bottom",
                    size = 13, family = "Ink Free", padding.y = unit(.15, "in")) + 
  ggtitle("Duración de las canciones del disco\n\"La ley innata\" de Extremoduro") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "#DBDCE1", family = "Castellar", hjust = .5, size = 25))

ggsave("14.png", width = 9, height = 9)