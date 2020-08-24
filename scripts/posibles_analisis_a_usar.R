library(tibble)
library(data.table)
library(plyr)
library(dplyr)

seg<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))

# #Calcular la media y la mediana de plantas visitadas en cada seguimiento
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(mean(n))
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(median(n))


#calcular numero de especies de plantas diferentes visitadas en cada vuelo
#algunos vuelos aparecen duplicados por estar a caballo entre dos periodos de hora
#crea 344 líneas cuando deberían ser 332:
#length(unique(seg_subset$Codigo_vuelo))
#no sé cómo solucionarlo, se me ocurre quitar el periodo_hora del ddply, pero ya no se podría usar para otros análisis
seg_table <- ddply(seg_subset, c("Periodo_fecha", "Periodo_hora","Bosque","Codigo_vuelo", "Polinizador"),
                   summarise,
                     n_plant_sps = n_distinct(Planta))

head(seg_table)
str(seg_table)
#aqui estaria bien meter como covariable la diversidad total de plantas presentes en cada periodo,
#que lo puedes sacar de los cuadrados.
seg_table$Periodo_fecha<-as.factor(seg_table$Periodo_fecha)
seg_table$Bosque<-as.factor(seg_table$Bosque)

#Índice de Shannon para meter como covariable
library(naniar)
library(tidyr)
library(vegan)

cuad<- read.csv("./data/cuad_clean.csv")
cuad$flores<- as.numeric(as.character(cuad$flores))
cuad$escapos<- as.numeric(as.character(cuad$escapos))
cuad$flores[is.na(cuad$flores)]<- 0
cuad$escapos[is.na(cuad$escapos)]<- 0
cuad$flores<- ifelse(cuad$flores == 0, cuad$escapos, cuad$flores)
cuad_subset<- cuad %>% group_by(periodo, Bosque, sp) %>% summarise(abundancia = sum(flores))
cuad_subset2<- cuad_subset %>% replace_with_na(replace = list(sp = "")) %>% drop_na(sp)
cuad_wide<- spread(cuad_subset2, sp, abundancia)
cuad_wide[is.na(cuad_wide)]<- 0
shannon<- diversity(cuad_wide[-c(1:2)])
key_shannon<- cbind(Periodo_fecha = cuad_wide$periodo, Bosque = cuad_wide[2], shannon = shannon)
seg_shannon<- merge(seg_table, key_shannon)


library(lme4)
#???? la covariable de diversidad es de efecto fijo y va así en el modelo?
m1<-lmer(n_plant_sps ~ Periodo_fecha + shannon + (1|Bosque) + (1|Polinizador), data=seg_shannon)
summary(m1)
car::Anova(m1)

p1 <- ggplot(seg_table, aes(x=Bosque, y=n_plant_sps, fill=Polinizador)) + 
  geom_boxplot() + xlim(0, 7) + ylab("Visited plant species richness") + xlab("Site")

p1 + facet_wrap(~Periodo_fecha)




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


#yo aqui haria un glm con distribucion binomial y de nuevo poner diversidad de especies presentes como covariable
str(seg2)
seg2$Periodo_fecha<-as.factor(seg2$Periodo_fecha)
seg2$Bosque<-as.factor(seg2$Bosque)
m3<-glmer(revisita_binario ~ Periodo_fecha + (1|Bosque) + (1|Polinizador), family="binomial", data=seg2)
summary(m3)
library(effects)
plot(allEffects(m3))


ggplot(seg2, aes(x=Periodo_fecha, y=revisita_binario, fill=Periodo_fecha)) + 
  geom_boxplot()

