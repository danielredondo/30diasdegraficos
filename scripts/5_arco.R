# ----- Día 5: Arco -----

# Paquetes
library(devtools)
# install_github("gastonstat/arcdiagram")
library(arcdiagram)
library(tvthemes)
library(dplyr)

# Importar fuente
#tvthemes::import_cinzel() 
library(extrafont)
#font_import()
loadfonts(device = "win")

# Lectura fichero juego de tronos (temporada 1)
edges <- read.csv2("datasets/GameofThrones-T1-Network-EDGES-CSV.csv")
nodes <- read.csv2("datasets/GameofThrones-T1-Network-NODES-CSV.csv")

# Filtrado casa Lannister
nodes <- nodes %>% filter(House %in% c("\"Lannister\""))
for(i in 1:nrow(edges)){
  if(!edges$Source[i] %in% nodes$Id){
      edges$Source[i] <- NA
  }
  if(!edges$Target[i] %in% nodes$Id){
    edges$Target[i] <- NA
  }
}
edges <- edges %>% filter(is.na(Source) == FALSE, is.na(Target) == FALSE)

# Ordenando...
edges <- dplyr::arrange(edges, Weight)
ranking <- names(sort(table(c(edges$Source, edges$Target)), decreasing = T))
new_edges <- data.frame(Source = "", Target = "")

for(j in 1:length(ranking)){
  for(i in 1:nrow(edges)){
    if(edges$Source[i] == ranking[j])
      new_edges <- rbind(new_edges, c(edges$Source[i], edges$Target[i]))
  }
}

edges_matrix <- as.matrix(new_edges[ , 1:2])[-1, ]
edges_matrix[ , 1] <- as.numeric(edges_matrix[ , 1])
edges_matrix[ , 2] <- as.numeric(edges_matrix[ , 2])

# Para identificar el número de los nodos
arcplot(edges_matrix, show.nodes=TRUE, cex.labels = 1.5, lwd.arcs = 3)

# Para guardar el gráfico
png("5.png", units = "in", width = 10, height = 6, res = 600)
arcplot(edges_matrix, show.nodes=TRUE,
        labels = c("Tywin\nLannister", "Bronn", "Tyrion\nLannister", "Cersei\nLannister",
                   "Lancel\nLannister", "Jaime\nLannister", "Kevan\nLannister", "Addam\nMarbrand",
                   "Leo\nLefford"),
        cex.labels = 1.1, lwd.arcs = 3, col.nodes	= "black", col.labels = "black",
        col.arcs = "royalblue", family = "Cinzel")
dev.off()
