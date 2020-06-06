# Paquetes
library(ggplot2)
library(dplyr)
library(reshape2)
library(magrittr)
library(ggiraph)

# Para fuentes
library(extrafont)
#font_import()
loadfonts(device = "win")

# Datos: Global Cancer Observatory (World Health Organization)
stats <- read.csv2("datasets/cancer.csv") %>% melt
names(stats) <- c("localizacion", "Sexo", "casos")
stats$localizacion <- ordered(factor(stats$localizacion, levels = unique(stats$localizacion)))

#https://dqn.website/post/interactive-mekko-charts-in-r/
stats %<>% group_by(localizacion) %>%
  mutate(
    share = casos / sum(casos),
    tot_group = sum(casos)
  ) %>% ungroup()

stats %<>%
  group_by(Sexo) %>% 
  arrange(desc(localizacion)) %>%
  mutate(
    ymax = cumsum(tot_group) / sum(tot_group), 
    ymin = (ymax - (tot_group/sum(tot_group)))
  ) %>% ungroup() %>% 
  group_by(localizacion) %>% 
  arrange(desc(Sexo)) %>%
  mutate(xmax = cumsum(share), xmin = xmax - share) %>%
  ungroup() %>% 
  arrange(localizacion)

stats %>%
  select(localizacion, Sexo, ymin, ymax, xmin, xmax) %>%
  arrange(desc(localizacion))

# job labels tibble
labels <- stats %>% 
  mutate(y = ymax - 0.01, yRange = (ymax - ymin)* 100) %>%
  mutate(label = paste0(localizacion, " - ", Sexo)) %>% 
  select(label, xmin, y, yRange) %>% 
  ungroup()

labels$label[c(4, 7, 16)] <- NA

ggplot(stats) + 
  geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = Sexo),
            colour = "white") +
  geom_text(data = labels,
            aes(x = xmin + 0.008, y = y, label = label), 
            hjust = 0, vjust = 1, colour = "white", size = 4.5, family = "Perpetua") +
  scale_x_continuous(position = "top", expand = c(0.01, 0.01), 
                     labels = scales::percent, breaks = scales::pretty_breaks(n = 4)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.02)) +
  scale_fill_manual(values = c("turquoise4", "steelblue3")) +
  ggtitle("Cancer incidence, 2018", "Distribution of cases by sex and anatomical site with +500,000 cases diagnosed.\nSource: Global Cancer Observatory (World Health Organization).") +
  theme(axis.line.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, color = "black", family = "Perpetua"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 24, hjust = .5, family = "Perpetua"),
    plot.subtitle = element_text(face = "italic", size = 16, hjust = .5, family = "Perpetua"),
    legend.position = "none",
    panel.background = element_blank())

ggsave("26.png", width = 8, height = 8)