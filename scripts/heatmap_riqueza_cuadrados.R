#calcular numero especies por subcuadrado
library(dplyr)

cuad<- read.csv("./data/cuad_clean.csv")

sp.rich<-cuad %>% group_by(Bosque, xy, x, y) %>% summarize(count=n())

sp.rich2<-as.data.frame(sp.rich)
str(sp.rich2)
head(sp.rich2)

library(ggplot2)
library(viridis)
p1<-ggplot(sp.rich2, aes(x = x, y = y, fill = count)) + geom_tile()
p1+scale_fill_viridis() + facet_wrap(~Bosque) + labs(fill="Plant richness") + 
  theme(legend.title.align = 0.1, legend.title = element_text(face = "bold", size = 9), 
        strip.background = element_blank(), strip.text = element_text(face = "bold")) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
