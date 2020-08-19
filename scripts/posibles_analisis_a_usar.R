library(tibble)
library(data.table)
library(dplyr)

seg_clean<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg_clean, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))

#Calcular la media y la mediana de plantas visitadas en cada seguimiento
seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(mean(n))
seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(median(n))


#Calcular la secuencia de plantas seguida por el polinizador

Codigo_planta<- as.numeric(seg_subset$Planta)
seg_subset<- add_column(seg_subset, Codigo_planta, .after = 12)
seg2<- transform(seg_subset, revisita= ave(Codigo_planta, rleid(Codigo_vuelo, Codigo_planta), FUN = seq_along))
