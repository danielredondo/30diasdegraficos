# ----- Día 10: Paletas -----

# Referencias:

# @aschinchon
# https://fronkonstin.com/2019/01/10/rcpp-camaron-de-la-isla-and-the-beauty-of-maths/

# @dredondosanchez
# https://danielredondo.com/posts/20190210_atractor/

# Paquetes
library(Rcpp)
library(tidyverse)
library(wesanderson)
library(colorspace)

# Función C++
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a1,  double a2,  double a3,  double a4,  double a5,  double a6,  double a7,
            double a8,  double a9,  double a10, double a11, double a12, double a13, double a14) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0] = x0;
            y[0] = y0;
            for(int i = 1; i < n; ++i) {
            x[i] = a1 + a2 * x[i-1] + a3  * y[i-1] + a4  * pow(fabs(x[i-1]), a5)  + a6  * pow(fabs(y[i-1]), a7 );
            y[i] = a8 + a9 * x[i-1] + a10 * y[i-1] + a11 * pow(fabs(x[i-1]), a12) + a13 * pow(fabs(y[i-1]), a14);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

# Parámetros
a1 <- -0.8; a2 <- 0.4; a3 <- -1.1; a4 <- 0.5; a5 <- -0.6; a6 <- -0.1; a7 <- -0.5;
a8 <- 0.8; a9 <- 1.0; a10 <- -0.3; a11 <- -0.6; a12 <- -0.3; a13 <- -1.2; a14 <- -0.3;

df <- createTrajectory(10000000, 1, 1, a1, a2, a3, a4, a5, a6, 
                       a7, a8, a9, a10, a11, a12, a13, a14)
mx <- quantile(df$x, probs = 0.01)
Mx <- quantile(df$x, probs = 0.99)
my <- quantile(df$y, probs = 0.05)
My <- quantile(df$y, probs = 0.95)
df %>% filter(x > mx, x < Mx, y > my, y < My) -> df

# Ordenar para colorear
df$s <- (df$x - mean(df$x)) ^ 2 + (df$y - mean(df$y)) ^ 2 - 0.5 + 4 * rnorm(nrow(df))
df <- df[order(df$s), ]

# Paleta y gráfico
pal <- wes_palette(nrow(df), name = "GrandBudapest1", type = "continuous")

plot <- ggplot(df) +
  geom_point(aes(x, y), shape = 46, alpha = 0.05, size = 0, color = pal) +
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  coord_fixed() + 
  theme(legend.position  = "none",
        panel.background = element_rect(fill = "white"),
        plot.background  = element_rect(fill = "white"),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())

# Puede tardar en guardarse
ggsave("10.png", plot, height = 4, width = 8, units = 'in')