library(tibble)
library(data.table)
library(dplyr)

seg<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))

# #Calcular la media y la mediana de plantas visitadas en cada seguimiento
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(mean(n))
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(median(n))


#calcular numero de especies de plantas diferentes visitadas en cada vuelo
seg_table <- ddply(seg_subset, c("Periodo_fecha", "Periodo_hora","Bosque","Codigo_vuelo", "Polinizador"),
                   summarise,
                     n_plant_sps = n_distinct(Planta))

head(seg_table)

#aqui estaria bien meter como covariable la diversidad total de plantas presentes en cada periodo,
#que lo puedes sacar de los cuadrados.
library(lme4)

m1<-lmer(n_plant_sps ~ Periodo_fecha + (1|Polinizador),  data=seg_table)
summary(m1)



#calcular media y desviacion de este numero de especies de plantas visitadas por especie de polinizador y periodo

seg_table2 <- ddply(seg_table, c("Periodo_fecha", "Bosque", "Polinizador"), 
                    summarise,
                   mean_plant_sps = mean(n_plant_sps),
                   stdev= sd(n_plant_sps))

head(seg_table2)

#esto si quieres lo hablamos un dia.


#Calcular la secuencia de plantas seguida por el polinizador

Codigo_planta<- as.numeric(seg_subset$Planta)
seg2<- add_column(seg_subset, Codigo_planta, .after = 12)
seg3<- transform(seg2, revisita= ave(Codigo_planta, rleid(Codigo_vuelo, Codigo_planta), FUN = seq_along))
#UNA ALTERNATIVA QUE PARECE TENER MÁS SENTIDO Y ES MÁS SENCILLA A LO ANTERIOR (vuelos con
#una sola visita son mandados a NA)
seg2<- seg_subset %>% group_by(Codigo_vuelo) %>% mutate(revisita_binario= ifelse(Planta == lag(Planta), 1, 0))

#Para ver si el periodo 1 o el periodo 2 influye sobre la decisión de ir a la misma sp de
#planta o no, hago una tabla de contingencia y un test de Fisher??
#Aquí va el fisher más simple sin separar por sp de polinizador y asumiendo que cada
#cambio de planta es independiente del anterior, sin tener en cuenta si una secuencia es de
#2 plantas o 50 
tab<- table(seg2$Periodo_fecha, seg2$revisita_binario)
fisher.test(tab)
