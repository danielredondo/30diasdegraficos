library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)

com <- c("Myocardial infarct", "Congestive heart failure", "Peripheral vascular disease", "Cerebrovascular disease",
         "Dementia", "Chro. obs. pulmonary disease", "Rheumatologic disease", "Liver disease",
         "Diabetes", "Renal disease")

datos <- data.frame("<55" = c(3, 2.6, 7.3, 3.1, 4.2, 4.4, 5.8, 16.1, 2.4, 3.3),
                    "55-64" = c(13.4, 11, 17.7, 7.7, 10.4, 11.5, 12.5, 19.6, 16.8, 5.4),
                    "65-74" = c(25.4, 22.1, 29, 30.8, 10.4, 31.3, 28.8, 26.8, 34.4, 21.7),
                    "75+" = c(58.2, 64.3, 46, 58.5, 75, 52.7, 52.9, 37.5, 46.4, 69.6),
                    check.names = F) %>% t
colnames(datos) <- com
datos <- melt(datos)

ggplot(data = datos, aes(x = Var2, y = Var1)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "white", high = "firebrick1") +
  geom_text(size = 4, label = paste0(datos$value, "%")) +
  ggtitle("Comorbidities by age group in colorectal cancer patients (Granada + Girona), 2011") + 
  theme_minimal() + 
  labs(caption = "Source: 'Multimorbidity by Patient and Tumor Factors and Time-to-Surgery Among Colorectal Cancer Patients in Spain: A Population-Based Study'\n
                  Luque-Fernandez MA, Redondo-Sanchez D, Lee SF, Rodríguez-Barranco M, Carmona-García MC, Marcos-Gragera R, Sánchez MJ\n
                  Clinical Epidemiology\n
                  Project: https://comcor.netlify.com/\n
                  Shiny App: https://watzilei.com/shiny/CoMCoR/") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 8, face = "italic", lineheight = .65))

ggsave("11.png", width = 10, height = 6)
