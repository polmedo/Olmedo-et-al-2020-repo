#calcular numero especies por subcuadrado
library(dplyr)

cuad<- read.csv("./data/cuad_clean.csv")

sp.rich<-cuad %>% group_by(Bosque, xy, x, y) %>% summarize(count=n())

sp.rich2<-as.data.frame(sp.rich)
str(sp.rich2)
head(sp.rich2)

library(ggplot2)
p1<-ggplot(sp.rich2, aes(x = x, y = y, fill = count)) + geom_tile()
p1+facet_wrap(~Bosque)
