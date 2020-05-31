# Paquetes
library(ggplot2)
library(ggrepel)
library(dplyr)

stats <- read.csv("datasets/estadisticas1920.csv")

gr.stats2 <- function(stat, n = 10, bp = .4, ny = 1, nx = 0, f = 1, partidos.minimos = 5, breaks = NA){
  stats2 <- subset(stats, Partidos > partidos.minimos)
  top10 <- top_n(stats2, n, stats2[stat] / Partidos)
  g <- ggplot(data = stats, aes(Partidos, eval(as.name(stat)), label = Jugador)) +
    geom_point(alpha = 0.3, shape = 16, col = "dodgerblue", size = 2) + 
    geom_point(data = top10, aes(Partidos, eval(as.name(stat))), alpha = 1, shape = 16, col = "darkblue", size = 3) + 
    geom_text_repel(data = top10, box.padding = bp, nudge_y = ny, force = f, nudge_x = nx, segment.alpha = 0.5, size = 3.5) + 
    ylab(stat) + 
    ylim(0, 20) +
    scale_x_continuous(limits = c(0, 27), breaks = c(seq(0, 25, 5), 27)) +
    theme_classic() + 
    theme(axis.text = element_text(color = "black"))
  
  if(is.na(breaks[1]) == FALSE) g <- g + scale_y_continuous(breaks = breaks)
  cbind(top10["Jugador"], top10["Partidos"], top10[stat]) %>% as.data.frame() %>% arrange(desc(eval(as.name(stat)))) %>% print
  
  return(g)
}

# Partidos - goles
gr.stats2("Goles", n = 10, ny = 0, nx = 0, breaks = seq(0, 35, 5))
ggsave("21.png", width = 8, height = 8)