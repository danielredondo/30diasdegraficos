library(ggplot2)
library(ggdag)
library(extrafont)
#font_import()
loadfonts(device = "win")

tidy_ggdag <- tidy_dagitty(dagify(EA ~ C,
                                  T ~ EA,
                                  D ~ EA,
                                  TD ~ D,
                                  CD ~ D))
tidy_ggdag$data

# (x, y) coordenadas del nodo del que sale la flecha
tidy_ggdag$data$x <- c(0, 3, 3, 2, 2, 4, 4, 3)
tidy_ggdag$data$y <- c(0, 1, 1, 0, 0, 1.5, 0.5, -1)

# (xend, yend) coordenadas del nodo que recibe la flecha
tidy_ggdag$data$xend <- c(2, 4, 4, 3, 3, NA, NA, NA)
tidy_ggdag$data$yend <- c(0, 1.5, 0.5, 1, -1, NA, NA, NA)

# Dibujamos el DAG
ggplot(tidy_ggdag$data, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_arc(curvature = c(0, 0.2, -0.2, 0, 0, 0, 0)) + 
    geom_dag_node() +
    geom_dag_text() +
    theme_dag() +
    annotate("text", x = -0.1, y = 1.1, size = 6,
             label = toupper("C = Coronavirus\nEA = Estado de alarma\nD = Distanciamiento físico\nCD = Clases a distancia\nTD = Trabajo a distancia\nT = Todo el mundo es epidemiólogo\n"),
             hjust = 0, family = "Impact")
ggsave("20.png", width = 12, height = 7, dpi = 300)
