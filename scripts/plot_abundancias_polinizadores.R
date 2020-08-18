library(dplyr)
library(ggplot2)

seg_clean<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg_clean, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))

#queda feísimo pero creo que va por aquí
p<- seg_subset %>% group_by(Fecha, Polinizador) %>% summarise(n=n_distinct(Codigo_vuelo)) %>% ggplot(., aes(x=Fecha, y=n)) + geom_bar(stat = 'identity') + geom_smooth() + facet_wrap(~Polinizador, scales = "free_y") + scale_y_continuous(expand = c(0,0))
print(p)