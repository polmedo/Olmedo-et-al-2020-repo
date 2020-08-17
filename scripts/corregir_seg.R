library(tidyr)
library(dplyr)

seg<-read.csv("data/seguimientos.csv")
fechas<- data.frame(unique(seg$Fecha))
seg$Fecha<- as.character(seg$Fecha)

seg$Fecha[seg$Fecha == "24/16/2020"]<- "24/06/2020"
seg$Fecha[seg$Fecha == "07/04/20"]<- "04/07/20"
#HAY UNOS DATOS DEL BOSQUE 5 SUPUESTAMENTE DEL 07/05 PERO PARECEN DEL 05/07, LO HE CAMBIADO
seg$Fecha[seg$Fecha == "07/05/20" & seg$Bosque == 5]<- "05/07/20"

#En la columna Codigo Vuelos solo aparece el código en la primera planta visitada, relleno:
seg2<- seg %>% fill(Codigo_Vuelos)

#En la columna de Codigo vuelo día hay dos datos que faltan:
sum(is.na(seg$Codigo_vuelo_dia))
#Pero en este caso la función fill como antes no vale, me he fijado y es un nuevo 
#vuelo (habría que sumar 1 al valor que tienen encima). NO SÉ HACER ESTO

#Hay muchos NAs al final de la columna orden vuelo, la creo de nuevo:
seg3<- seg2 %>%
        group_by(Codigo_Vuelos) %>%
        mutate(Orden_vuelo2 = 1:n()) %>% ungroup()
#Quito la columna original de orden vuelo
seg4<- seg3[ , -6]

seg4$Polinizador[seg4$Polinizador == "Sphaerophoria scripta "]<- "Sphaerophoria scripta"
seg4$Polinizador[seg4$Polinizador == "Sphaerophoria"]<- "Sphaerophoria scripta"

seg4$sexo[seg4$sexo == "macho"]<- "Macho"
seg4$sexo[seg4$sexo == "hembra"]<- "Hembra"

#para cambiar los datos de la planta visitada tengo que cambiar a character porque en factor
#no me deja escribir nombres que no estén ya en la columna
seg4$Planta_vistada<- as.character(seg4$Planta_vistada)
seg4$Planta_vistada[seg4$Planta_vistada == "Blanca comp."]<- "Blanca compuesta"
seg4$Planta_vistada[seg4$Planta_vistada == "Erysimum s."]<- "Erysimum sp."
seg4$Planta_vistada[seg4$Planta_vistada == "Erysinum"]<- "Erysimum sp."
seg4$Planta_vistada[seg4$Planta_vistada == "Galium sp."]<- "Gallium sp."
seg4$Planta_vistada[seg4$Planta_vistada == "Lathyrus linnifolius"]<- "Lathyrus linifolius"
seg4$Planta_vistada[seg4$Planta_vistada == "Lotus cornicularis"]<- "Lotus corniculatus"
seg4$Planta_vistada[seg4$Planta_vistada == "Saxifraga irsuta"]<- "Saxifraga hirsuta"

seg4$coord_cuadra.x.y.<- as.character(seg4$coord_cuadra.x.y.)
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "10,80"]<- "10,08"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "10,0"]<- "10,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "11,0"]<- "11,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "12,0"]<- "12,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "12,7"]<- "12,07"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "13,0"]<- "13,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "13,7"]<- "13,07"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "14,0"]<- "14,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "14,50"]<- "14,05"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "15,0"]<- "15,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "15,50"]<- "15,05"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "15,70"]<- "15,07"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "19,0"]<- "19,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "2,0"]<- "2,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "20,0"]<- "20,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "21,0"]<- "21,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "22,0"]<- "22,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "23,0"]<- "23,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "46,18"]<- "16,18"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "8,0"]<- "8,00"
seg4$coord_cuadra.x.y.[seg4$coord_cuadra.x.y. == "9,0"]<- "9,00"

#rellenar codigo vuelo día
seg4[48,4]<- 7
seg4[55,4]<- 3

#cambiar columna de hora
seg4$Hora<- as.character(seg4$Hora)
seg4$Hora[seg4$Hora == "19:59:00"]<- "11:59:00"

#hay horas escritas en formato h:m y otras h:m:s, por eso tengo que cambiarlas por separado
library(chron)
a<- times(paste0(seg4$Hora, ":00"))
b<- times(seg4$Hora)
a[is.na(a)] <- b[!is.na(b)]
horas2<- as.data.frame(a)
library(tibble)
seg5<- data.frame(add_column(seg4, horas2, .after = 1))
seg6<- seg5[, -3]



#Crear variable de periodo
seg6$Fecha<- as.Date(seg6$Fecha, "%d/%m/%y")
seg6$periodo<- seg6$Fecha
seg6$periodo<- format(seg6$periodo, "%j")
seg6$periodo<- ifelse(seg6$periodo <= 149, "1", "2")


#columnas de x e y
#una columna con NA y solo un vuelo:
seg7<- seg6[-196, ]
foo <- data.frame(do.call('rbind', strsplit(as.character(seg7$coord_cuadra.x.y.),',',fixed=TRUE)))
head(foo)
str(foo)
seg8<- cbind(seg7, foo)

#renombrar y reordenar
colnames(seg8)<- c("Fecha", "Hora", "Bosque", "Codigo_vuelo_dia", "Codigo_vuelo", "Polinizador", "Codigo_captura", "Sexo", "Planta", "xy", "Observaciones", "x_geo", "y_geo", "Orden_vuelo", "Periodo_fecha", "X", "Y")
seg9<- seg8[, c(1,15,2,3,4,5,14,6,7,8,9,10,16,17,11,12,13)]

#variable periodo hora
Periodo_hora<- ifelse(seg9$Hora < "12:40:00", 1, ifelse((seg9$Hora >= "12:40:00") & (seg9$Hora < "15:20:00"), 2, 3))

seg10<- data.frame(add_column(seg9, Periodo_hora, .after = 3))

