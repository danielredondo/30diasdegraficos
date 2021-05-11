# ----- Día 24 - Coropletas ------

# Adapted from https://github.com/aaumaitre/maps_Spain

# Packages
library(rgdal)
library(dplyr)
library(readxl)
library(broom)
library(ggplot2)
library(viridis)

# Layer with census tracts of Spain, 2011 (Instituto Nacional de Estadística)
# http://ine.es/censos2011_datos/cen11_datos_resultados.htm
sc <- readOGR("SECC_CPV_E_20111101_01_R_INE.shp",
              encoding = "utf8", use_iconv = TRUE) %>%
  spTransform(CRS("+init=epsg:4326"))

# Spanish Deprivation Index, 2011 (Sociedad Española de Epidemiología)
# https://www.seepidemiologia.es/gruposdetrabajo.php?contenido=gruposdetrabajosub6
ip <- read_xlsx("IP2011_RE.xlsx")

# Merge of the data
sc@data <- merge(x = ip, y = sc@data, by.x = "CUSEC", by.y = "CUSEC")

# Convert sc to a tibble
sc_tidy <- tidy(sc) 

# Temporary data.frame to keep the census tract, SDI, and quintiles of SDI after the tidy()
sc$IP2011_q <- cut(sc$IP2011, breaks = quantile(sc@data$IP2011, probs = seq(0, 1, 1/5)), include.lowest = T, right = F)
table(sc$IP2011_q)
sc_names <- data.frame(CUSEC = sc$CUSEC, IP2011 = sc$IP2011, IP2011_q = sc$IP2011_q)
sc_names$id <- as.character(seq(0, nrow(sc_names) - 1))

# Census tract added
sc_df <- left_join(sc_tidy, sc_names, by = "id") %>% 
  # We move the Canary Islands
  mutate(lat = ifelse(lat < 30, lat + 8, lat),
         long = ifelse(long < (-10), (long + 18.4), long)) 

# Line to show Canary Islands
canaries_line <- data.frame(long = c(0, 0, 5),
                            lat = c(35, 37.5, 37.5))

# Define palette
q <- quantile(sc_df$IP2011, probs = seq(0, 1, 1/5))
q
library(leaflet)
paleta <- colorBin("Blues", domain = sc_df$IP2011, bins = q)
paleta <- rev(names(table(paleta(sc_df$IP2011))))

ggplot(sc_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = IP2011_q)) + 
  labs(x = NULL, y = NULL,
       title = "Deprivation in Spain", 
       subtitle = "Spanish Deprivation Index (SDI) by census tracts, 2011", 
       caption = "@dredondosanchez & @WATZILEI\nDESOCANES Proyect (FIS PI18/01593) - https://desocanes.netlify.app/\nData: Social Inequalities Working Group of the Spanish Society of Epidemiology, Spanish National Statistics Institute") + 
  geom_path(data = canaries_line, aes(x = long, y = lat, group = NULL), size = 1, color = "gray40") +
  scale_fill_manual(
    values = paleta,
    name = "Quintiles of SDI",
    drop = FALSE,
    labels = c("Q1 (less deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
  theme(axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "lightgoldenrodyellow", color = NA),
    panel.background = element_rect(fill = "lightgoldenrodyellow", color = NA),
    legend.background = element_rect(fill = "lightgoldenrodyellow", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "black"),
    plot.caption = element_text(size = 7.5, color = "black", lineheight = 1.2),
    legend.title = element_text(color = "black", face = "bold", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.text.align = 0,
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(2,0.2,.2,0.2), "cm"))

ggsave("24.png", dpi = 600, width = 20, height = 15, units = "cm")