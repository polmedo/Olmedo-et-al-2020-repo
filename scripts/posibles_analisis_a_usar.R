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

#SHANNON-----
#cuad_subset<- cuad %>% group_by(periodo, Bosque, sp) %>% summarise(abundancia = sum(flores))
#cuad_subset2<- cuad_subset %>% replace_with_na(replace = list(sp = "")) %>% drop_na(sp)
#cuad_wide<- spread(cuad_subset2, sp, abundancia)
#cuad_wide[is.na(cuad_wide)]<- 0
#shannon<- diversity(cuad_wide[-c(1:2)])
#key_shannon<- cbind(Periodo_fecha = cuad_wide$periodo, Bosque = cuad_wide[2], shannon = shannon)
#seg_shannon<- merge(seg_subset, key_shannon)

#Riqueza de plantas
cuad_drop_na<- cuad %>% replace_with_na(replace = list(sp = "")) %>% drop_na(sp)
key_riqueza<- cuad_drop_na  %>% 
  group_by(periodo, Bosque) %>% summarise(plant_richness=n_distinct(sp))
colnames(key_riqueza)[1]<- "Periodo_fecha"
seg_riqueza<- merge(seg_subset, key_riqueza)


#plantas visitadas en cada vuelo para el offset
seg_subset2<- seg_riqueza %>% group_by(Codigo_vuelo) %>% mutate(visitas_por_vuelo = n())

# #Calcular la media y la mediana de plantas visitadas en cada seguimiento
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(mean(n))
# seg %>% group_by(Codigo_vuelo) %>% summarise(n=n()) %>% summarise(median(n))


#calcular numero de especies de plantas diferentes visitadas en cada vuelo
#algunos vuelos aparecen duplicados por estar a caballo entre dos periodos de hora
#crea 344 líneas cuando deberían ser 332:
#length(unique(seg_subset$Codigo_vuelo))
#no sé cómo solucionarlo, se me ocurre quitar el periodo_hora del ddply, pero ya no se podría usar para otros análisis
seg_table <- ddply(seg_subset2, c("Periodo_fecha", "Periodo_hora","Bosque","Codigo_vuelo", "Polinizador", "plant_richness", "visitas_por_vuelo"),
                   summarise,
                     n_plant_sps = n_distinct(Planta))
head(seg_table)
str(seg_table)


seg_table$Periodo_fecha<-as.factor(seg_table$Periodo_fecha)
seg_table$Bosque<-as.factor(seg_table$Bosque)


library(lme4)

m1<-lmer(n_plant_sps ~ Periodo_fecha + plant_richness + (1|Bosque) + (1|Polinizador), data=seg_table)
summary(m1)
car::Anova(m1)


library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = m1, plot = T)

m1b<-glmer.nb(n_plant_sps ~ Periodo_fecha + plant_richness + (1|Bosque) + (1|Polinizador), data=seg_table)
summary(m1b)
car::Anova(m1b)

simulationOutput <- simulateResiduals(fittedModel = m1b, plot = T)

m1c<-glmer(n_plant_sps ~ Periodo_fecha + plant_richness + (1|Bosque) + (1|Polinizador), family="poisson", data=seg_table)
summary(m1c)
car::Anova(m1c)

simulationOutput <- simulateResiduals(fittedModel = m1c, plot = T)



p1 <- ggplot(seg_table, aes(x=Bosque, y=n_plant_sps, color=Polinizador))

label<- c("First period", "Second period")
names(label)<- c("1", "2")

p1 + geom_boxplot() + coord_cartesian(ylim=c(0, 6.5)) + 
  facet_wrap(~Periodo_fecha, labeller = labeller(Periodo_fecha=label)) + 
  labs(color="Pollinator") + ylab("Visited plant species richness") + xlab("Site") + 
  theme(axis.title = element_text(face = "bold", size = 9), 
        legend.title = element_text(face = "bold"), strip.background = element_blank(), 
        strip.text = element_text(face = "bold")) + 
  scale_y_continuous(expand=c(0,0))

#Error: Discrete value supplied to continuous scale --- he visto que el error 
#está en el xlim, si se quita ya no da error, pero no sé cómo solucionarlo con el xlim


#calcular media y desviacion de este numero de especies de plantas visitadas por especie de polinizador y periodo

seg_table2 <- ddply(seg_table, c("Periodo_fecha", "Bosque", "Polinizador"), 
                    summarise,
                    mean_plant_sps = mean(n_plant_sps),
                    stdev= sd(n_plant_sps))

head(seg_table2)

#Grid de plots con media y sd para cada polinizador y cada bosque
#Pensaba poner alguno de estos para mostrar cómo alguna sp cambia más, aunque
#no sé si queda del todo bien. ¿Crees que debería quedarme con alguno?
seg_table2$lower<- (seg_table2$mean_plant_sps - seg_table2$stdev)
seg_table2$higher<- (seg_table2$mean_plant_sps + seg_table2$stdev)

pl1<- ggplot(seg_table2, aes(x = Periodo_fecha, y = mean_plant_sps)) +
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0.10, size  = 0.5) +
  geom_point(shape = 20, size  = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  ylab("Mean visitated plant spp") + xlab("Period")
pl1 + facet_grid(Bosque~Polinizador)
#O mejor este, sin tantas gráficas
library(RColorBrewer)
period.lab<- c("Period 1", "Period 2")
names(period.lab)<- c("1", "2")
pl2<- ggplot(seg_table2, aes(x = Bosque, y = mean_plant_sps, color=Polinizador))

pl2 + geom_errorbar(aes(ymin = lower, ymax = higher), 
                    position = position_dodge2(width=0.2), width = 0.5, size  = 0.7) +
  geom_point(shape = 20, size  = 1.2, 
             position = position_dodge2(width = 0.5, padding = 0.5)) + 
  scale_color_brewer(palette = "Dark2") + theme_bw() + 
  theme(axis.title = element_text(face  = "bold"), 
        legend.title = element_text(face = "bold"), strip.background = element_blank(), 
        strip.text = element_text(face = "bold")) + 
  labs(color="Pollinator") + ylab("Visited plant species richness") + 
  xlab("Site")+ facet_wrap(~Periodo_fecha, labeller = labeller(Periodo_fecha=period.lab))

#plot riqueza vs riqueza de visita
seg_table4<- ddply(seg_table, c("plant_richness"), summarise, mean_plant_sps=mean(n_plant_sps), stdev=sd(n_plant_sps))
seg_table4$lower<- (seg_table4$mean_plant_sps - seg_table4$stdev)
seg_table4$higher<- (seg_table4$mean_plant_sps + seg_table4$stdev)
pl6<- ggplot(seg_table4, aes(x = plant_richness, y = mean_plant_sps))
pl6 + geom_errorbar(aes(ymin = lower, ymax = higher)) + 
  geom_point() + xlab("Available plant richness") + ylab("Visited plant species richness") + 
  theme(axis.title = element_text(face  = "bold"))


#Diferencias de diversidad de plantas entre los dos períodos
#key_shannon$Bosque<- as.factor(key_shannon$Bosque)
#shannon_wide<- key_shannon %>%
  #pivot_wider(names_from = Periodo_fecha, values_from=shannon, names_prefix="periodo")

#t_test_shannon<- t.test(shannon_wide$periodo1, shannon_wide$periodo2, paired=TRUE)
#t_test_shannon

p2<- ggplot(key_shannon, aes(x=Periodo_fecha, y=shannon, color=Bosque)) + 
  geom_point() + geom_line(aes(group=Bosque)) + 
  scale_color_discrete(name = "Plot") + xlab("Period") + ylab("Shannon index")


#esto si quieres lo hablamos un dia.


#Calcular la secuencia de plantas seguida por el polinizador

#Codigo_planta<- as.numeric(seg_subset$Planta)
#seg2<- add_column(seg_subset, Codigo_planta, .after = 12)
#seg3<- transform(seg2, revisita= ave(Codigo_planta, rleid(Codigo_vuelo, Codigo_planta), FUN = seq_along))

#UNA ALTERNATIVA QUE PARECE TENER MÁS SENTIDO Y ES MÁS SENCILLA A LO ANTERIOR (vuelos con
#una sola visita son mandados a NA)
seg2<- seg_subset2 %>% group_by(Codigo_vuelo) %>% mutate(revisita_binario= ifelse(Planta == lag(Planta), 1, 0))

#Para ver si el periodo 1 o el periodo 2 influye sobre la decisión de ir a la misma sp de
#planta o no, hago una tabla de contingencia y un test de Fisher??
#Aquí va el fisher más simple sin separar por sp de polinizador y asumiendo que cada
#cambio de planta es independiente del anterior, sin tener en cuenta si una secuencia es de
#2 plantas o 50 
#tab<- table(seg2$Periodo_fecha, seg2$revisita_binario)
#fisher.test(tab)


#yo aqui haria un glm con distribucion binomial y de nuevo poner diversidad de especies presentes como covariable
str(seg2)
seg2$Periodo_fecha<-as.factor(seg2$Periodo_fecha)
seg2$Bosque<-as.factor(seg2$Bosque)
m3<-glmer(revisita_binario ~ Periodo_fecha + plant_richness + (1|Bosque) + (1|Polinizador), family="binomial", data=seg2)

summary(m3)
library(effects)
plot(allEffects(m3))
plot(effect("Periodo_fecha", m3), main = "", xlab = "Period", ylab="Floral fidelity")
plot(effect("shannon", m3), main = "", xlab = "Shannon Index", ylab="Floral fidelity")

plot.lab<- c("Plot 1", "Plot 2", "Plot 3", "Plot 4", "Plot 5")
names(plot.lab)<- c("1", "2", "3", "4", "5")
pl.fid<- ggplot(seg_fidelity, aes(x = Periodo_fecha, 
                                  y = mean_plant_sps, color=Polinizador))
pl.fid + geom_point(shape = 20, size  = 1.5, 
                    position = position_dodge2(width = 0.4, padding = 0.5)) +  
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~Bosque, nrow = 1, labeller = labeller(Bosque=plot.lab)) + coord_fixed(5) + 
  theme_bw() + theme(axis.title = element_text(face  = "bold", size = 9), 
                     legend.title = element_text(face = "bold", size = 9), 
                     legend.text = element_text(size = 8), 
                     strip.background = element_blank(), 
                     strip.text = element_text(face = "bold", size = 8)) +  
  labs(color="Pollinator") + ylab("Floral fidelity") +  xlab("Period")+ 
  scale_y_continuous(expand = c(0.05, 0.05))
