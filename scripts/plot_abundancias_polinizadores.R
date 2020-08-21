library(dplyr)
library(plyr)
library(ggplot2)

seg_clean<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg_clean, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))
# 
# # #queda feísimo pero creo que va por aquí
#  p<- seg_subset %>% group_by(Fecha, Polinizador) %>% summarise(n=n_distinct(Codigo_vuelo)) %>% ggplot(., aes(x=Fecha, y=n)) + geom_bar(stat = 'identity') + geom_smooth() + facet_wrap(~Polinizador, scales = "free_y") + scale_y_continuous(expand = c(0,0))
#  print(p)


seg_subset2 <- ddply(seg_subset, c("Fecha", "Polinizador"), summarise,
            n    = n_distinct(Codigo_vuelo))


#te he cambiado aqui un poco la grafica para mostrar todas las especies juntas.

seg_subset2$Fecha <- as.Date(seg_subset2$Fecha)

head(seg_subset2)


pl<-ggplot(seg_subset2, aes(x=Fecha, y=n, fill=Polinizador)) +
  geom_bar(stat = "identity",  position="dodge") +scale_fill_brewer(palette="Set3") +
  geom_smooth(aes(x=Fecha, y=n, colour=Polinizador), se=F, method="loess", lwd=0.7) +
  scale_colour_brewer(palette="Set3") 


pl
