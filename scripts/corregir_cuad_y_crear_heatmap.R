cuad_ainhoa<-read.csv("./data/cuadrados.csv")
xycuad<- data.frame(unique(cuad_ainhoa$X.x.y.))

#comprobar cuántos días de muestreo hay para cada cuadradito para confirmar por ej. si 10.2 es 10.20 o 10.02

cuad_ainhoa$Fecha<- as.Date(cuad_ainhoa$Fecha, "%d/%m/%Y")
muestreos<- data.frame(unique(cuad_ainhoa[c("Fecha", "Bosque", "X.x.y.")]))
library(dplyr)
muestreos2<- count(muestreos, muestreos$Bosque, muestreos$X.x.y.)
#####AL COMPROBAR ESTO ME HE DADO CUENTA DE QUE EN EL BOSQUE 2 NO HAY MUESTREOS DE VARIOS CUADRADOS, 20.00, 20.01, 21.00, 21.01, 22.00, 22.01, 22.02, 23.00, 23.01, 23.02, 24.00, 24.01 y 24.02 

#corregir columna XY

cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "3.2"]<- "3.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "4.2"]<- "4.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "5.2"]<- "5.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "6.2"]<- "6.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "7.2"]<- "7.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "8.2"]<- "8.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "9.2"]<- "9.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "10.2"]<- "10.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "11.2"]<- "11.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "12.2"]<- "12.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "13.2"]<- "13.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "14.2"]<- "14.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "15.2"]<- "15.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "16.2"]<- "16.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "17.2"]<- "17.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "18.2"]<- "18.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "19.2"]<- "19.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "20.2"]<- "20.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "21.2"]<- "21.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "22.2"]<- "22.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "23.2"]<- "23.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "24.2"]<- "24.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "0.1"]<- "0.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "1.1"]<- "1.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "2.1"]<- "2.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "3.1"]<- "3.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "4.1"]<- "4.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "5.1"]<- "5.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "6.1"]<- "6.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "7.1"]<- "7.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "8.1"]<- "8.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "9.1"]<- "9.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "10.1"]<- "10.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "11.1"]<- "11.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "12.1"]<- "12.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "13.1"]<- "13.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "14.1"]<- "14.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "15.1"]<- "15.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "16.1"]<- "16.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "17.1"]<- "17.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "18.1"]<- "18.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "19.1"]<- "19.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "20.1"]<- "20.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "21.1"]<- "21.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "22.1"]<- "22.10"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "0.20 "]<- "0.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "1.20 "]<- "1.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "2.20 "]<- "2.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "3.20 "]<- "3.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "4.20 "]<- "4.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "5.20 "]<- "5.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "6.20 "]<- "6.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "7.20 "]<- "7.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "8.20 "]<- "8.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "9.20 "]<- "9.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "10.20 "]<- "10.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "11.20 "]<- "11.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "12.20 "]<- "12.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "13.20 "]<- "13.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "14.20 "]<- "14.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "15.20 "]<- "15.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "16.20 "]<- "16.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "17.20 "]<- "17.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "18.20 "]<- "18.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "19.20 "]<- "19.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "20.20 "]<- "20.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "21.20 "]<- "21.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "22.20 "]<- "22.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "23.20 "]<- "23.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "24.20 "]<- "24.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "0.22 "]<- "0.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "1.22 "]<- "1.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "2.22 "]<- "2.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "3.22 "]<- "3.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "4.22 "]<- "4.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "5.22 "]<- "5.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "6.22 "]<- "6.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "7.22 "]<- "7.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "8.22 "]<- "8.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "9.22 "]<- "9.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "9.22 "]<- "10.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "10.22 "]<- "10.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "11.22 "]<- "11.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "12.22 "]<- "12.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "13.22 "]<- "13.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "14.22 "]<- "14.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "9.22 "]<- "9.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "10.22 "]<- "10.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "15.22 "]<- "15.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "16.22 "]<- "16.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "17.22 "]<- "17.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "18.22 "]<- "18.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "19.22 "]<- "19.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "20.22 "]<- "20.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "21.22 "]<- "21.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "22.22 "]<- "22.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "23.22 "]<- "23.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "24.22 "]<- "24.22"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "0.24 "]<- "0.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "1.24 "]<- "1.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "2.24 "]<- "2.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "3.24 "]<- "3.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "4.24 "]<- "4.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "5.24 "]<- "5.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "6.24 "]<- "6.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "7.24 "]<- "7.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "8.24 "]<- "8.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "9.24 "]<- "9.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "10.24 "]<- "10.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "11.24 "]<- "11.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "12.24 "]<- "12.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "13.24 "]<- "13.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "14.24 "]<- "14.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "15.24 "]<- "15.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "16.24 "]<- "16.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "17.24 "]<- "17.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "18.24 "]<- "18.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "19.24 "]<- "19.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "20.24 "]<- "20.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "21.24 "]<- "21.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "22.24 "]<- "22.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "23.24 "]<- "23.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "24.24 "]<- "24.24"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "17.23 "]<- "17.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "18.23 "]<- "18.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "19.23 "]<- "19.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "20.23 "]<- "20.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "21.23 "]<- "21.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "22.23 "]<- "22.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "23.23 "]<- "23.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "24.23 "]<- "24.23"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "24, 6"]<- "24.06"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "10.4"]<- "10.04"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "20.4"]<- "20.04"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "170.00"]<- "17.00"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "0.2"]<- "0.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "1.2"]<- "1.20"
cuad_ainhoa$X.x.y.[cuad_ainhoa$X.x.y. == "2.2"]<- "2.20"
xycuad2<- data.frame(unique(cuad_ainhoa$X.x.y.))
View(xycuad2)

#corregir columna SP

cuad_ainhoa3<- cuad_ainhoa[-9938, ]  #hay 2 muestreos que se llaman "be", uno de ellos es casi seguro B. perennis pero este dato es imposible saber cuál es y no parece B. perennis
cuad_ainhoa3$SP[cuad_ainhoa3$SP == " Erinus alpinus"]<- "Erinus alpinus"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "be"]<- "Bellis perennis"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "blanca comp."]<- "Blanca compuesta"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Blanca comp."]<- "Blanca compuesta"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Blanca comp. "]<- "Blanca compuesta"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "blanca compuesta"]<- "Blanca compuesta"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Blanca comp nueva"]<- "Blanca compuesta nueva"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Blanca comp. nueva"]<- "Blanca compuesta nueva"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Blanca comp. Nueva"]<- "Blanca compuesta nueva"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Cerastium"]<- "Cerastium sp."
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "cerastium sp."]<- "Cerastium sp."
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Erica tetralix "]<- "Erica tetralix"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Erinus apinus"]<- "Erinus alpinus"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Geranium robertianun"]<- "Geranium robertianum"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Lathyrus linifolius linifolius linifolius"]<- "Lathyrus linifolius"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Lathyrus linifolius linifolius sp."]<- "Lathyrus linifolius"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Lotus cornicularis"]<- "Lotus corniculatus"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "po"]<- "Potentilla erecta"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "potentilla erecta"]<- "Potentilla erecta"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Potentilla monta0"]<- "Potentilla montana"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Ptentilla montana"]<- "Potentilla montana"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "prunella vulgaris"]<- "Prunella vulgaris"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Saxifraga"]<- "Saxifraga sp."
levels <- levels(cuad_ainhoa3$SP)
levels[length(levels) + 1] <- "Saxifraga hirsuta"
cuad_ainhoa3$SP <- factor(cuad_ainhoa3$SP, levels = levels)
cuad_ainhoa3$SP[is.na(cuad_ainhoa3$SP)] <- "Saxifraga hirsuta"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Teucrium pyrenaica"]<- "Teucrium pyrenaicum"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "trifoloum re"]<- "Trifolium repens"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "veronica persica"]<- "Veronica persica"
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "veronica sp."]<- "Veronica sp."
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "veroonica sp."]<- "Veronica sp."
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "Viola sp.sp."]<- "Viola sp."
cuad_ainhoa3$SP[cuad_ainhoa3$SP == " "]<- ""
cuad_ainhoa3$SP[cuad_ainhoa3$SP == "0"]<- ""
sp<- data.frame(unique(cuad_ainhoa3$SP))
View(sp)

#reorganizar por fechas para que la variable periodo se cree correctamente y 
#crear variable de periodo


library(tidyverse)
cuad2<-cuad_ainhoa3 %>% arrange(Fecha) %>%
        group_by(Bosque, X.x.y.) %>%
        mutate(idx = match(Fecha, unique(Fecha)))


str(cuad2)
unique(cuad2$idx)

cuad3<-as.data.frame(cuad2)

#algunas filas muestreadas mas veces por error quitarlas

cuad4<-subset(cuad3, cuad3$idx<3)
cuad4<-droplevels(cuad4)

str(cuad4)
unique(cuad4$idx)

cuad4$X.x.y.<-as.character(cuad4$X.x.y.)


#crear dos columnas para coordenadas
foo <- data.frame(do.call('rbind', strsplit(as.character(cuad4$X.x.y.),'.',fixed=TRUE)))
head(foo)
str(foo)
cuad5<-cbind(cuad4, foo)
head(cuad5)

colnames(cuad5)<-c("Fecha", "Bosque", "xy", "sp", "flores", "escapos","observaciones", "periodo", "x", "y")
head(cuad5)

#calcular numero especies por subcuadrado
library(dplyr)


sp.rich<-cuad5 %>% group_by(Bosque, xy, x, y) %>% summarize(count=n())

sp.rich2<-as.data.frame(sp.rich)
str(sp.rich2)
head(sp.rich2)

library(ggplot2)
p1<-ggplot(sp.rich2, aes(x = x, y = y, fill = count)) + geom_tile()
p1+facet_wrap(~Bosque)

