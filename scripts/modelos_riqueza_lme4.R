library(tibble)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)

seg<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))

#Índice de Shannon para meter como covariable
library(naniar)
library(tidyr)
library(vegan)

cuad<- read.csv("./data/cuad_clean.csv")
cuad_subset<- cuad %>% group_by(periodo, Bosque, sp) %>% summarise(abundancia = sum(flores))
cuad_subset2<- cuad_subset %>% replace_with_na(replace = list(sp = "")) %>% drop_na(sp)
cuad_wide<- spread(cuad_subset2, sp, abundancia)
cuad_wide[is.na(cuad_wide)]<- 0
shannon<- diversity(cuad_wide[-c(1:2)])
key_shannon<- cbind(Periodo_fecha = cuad_wide$periodo, Bosque = cuad_wide[2], shannon = shannon)
seg_shannon<- merge(seg_subset, key_shannon)

#plantas visitadas en cada vuelo para el offset
seg_subset2<- seg_shannon %>% group_by(Codigo_vuelo) %>% mutate(visitas_por_vuelo = n())

# #Calcular la media y la mediana de plantas visitadas en cada seguimiento
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(mean(n))
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(median(n))


#calcular numero de especies de plantas diferentes visitadas en cada vuelo
#algunos vuelos aparecen duplicados por estar a caballo entre dos periodos de hora
#crea 344 líneas cuando deberían ser 332:
#length(unique(seg_subset$Codigo_vuelo))
#no sé cómo solucionarlo, se me ocurre quitar el periodo_hora del ddply, pero ya no se podría usar para otros análisis
seg_table <- ddply(seg_subset2, c("Periodo_fecha", "Periodo_hora","Bosque","Codigo_vuelo", "Polinizador", "shannon", "visitas_por_vuelo"),
                   summarise,
                   n_plant_sps = n_distinct(Planta))
head(seg_table)
str(seg_table)


seg_table$Periodo_fecha<-as.factor(seg_table$Periodo_fecha)
seg_table$Bosque<-as.factor(seg_table$Bosque)


library(lme4)
#Hay dos cosas que no entiendo
#1- Tanto en el poisson como en el nb me avisa de boundary (singular) fit
#Entiendo que tiene que ver con que los random factors son practicamente 0, no?
#Significa esto que no debería usar estas distribuciones y quedarme con la normal??

#2- Al hacer el qqplot de los 3 los residuos son exactamente iguales en poisson y nb y
#   casi iguales en el normal, no entiendo por qué. ¿Estoy haciendo bien los qqplots?

#Además de eso hay otro warning message en el nb que no he entendido
m1<-lmer(n_plant_sps ~ Periodo_fecha + shannon + 
           (1|Bosque) + (1|Polinizador), data=seg_table)
m1_poisson<- glmer(n_plant_sps ~ Periodo_fecha + shannon + 
                     (1|Bosque) + (1|Polinizador), family = "poisson", data = seg_table)
m1_nb<- glmer.nb(n_plant_sps ~ Periodo_fecha + shannon + 
                   (1|Bosque) + (1|Polinizador), data=seg_table)

qqnorm(resid(m1))
qqline(resid(m1))
qqnorm(resid(m1_poisson))
qqline(resid(m1_poisson))
qqnorm(resid(m1_nb))
qqline(resid(m1_nb))
