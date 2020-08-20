library(tibble)
library(data.table)
library(dplyr)

seg<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))

#Calcular la media y la mediana de plantas visitadas en cada seguimiento
seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(mean(n))
seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(median(n))


#Calcular la secuencia de plantas seguida por el polinizador

Codigo_planta<- as.numeric(seg_subset$Planta)
seg_subset<- add_column(seg_subset, Codigo_planta, .after = 12)
seg2<- transform(seg_subset, revisita= ave(Codigo_planta, rleid(Codigo_vuelo, Codigo_planta), FUN = seq_along))
#UNA ALTERNATIVA QUE PARECE TENER MÁS SENTIDO Y ES MÁS SENCILLA A LO ANTERIOR
seg3<- seg_subset %>% mutate(revisita_binario= ifelse(Planta == lag(Planta), 1, 0))

#algo me falla en el t-test, me da este mensaje de error: Error: Can't combine `Fecha` <factor<fe2dc>> and `Periodo_fecha` <integer>.
p<- seg3 %>% group_by(Polinizador) %>% t.test(revisita_binario~Periodo_fecha)