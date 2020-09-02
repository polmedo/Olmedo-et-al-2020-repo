library(dplyr)
library(tidyr)
library(ggplot2)

#heatmap diferencia de abundancia de flores por cuadradito
cuad<- read.csv("./data/cuad_clean.csv")
cuad_recursos<- cuad %>% 
  group_by(periodo, Bosque, xy, x, y) %>% summarise(abundancia = sum(flores))
cuad_recursos_wide<- cuad_recursos %>% 
  pivot_wider(names_from = c(periodo), values_from = abundancia, names_prefix = "periodo")
cuad_recursos_wide$dif_periodos<- (cuad_recursos_wide$periodo2 - cuad_recursos_wide$periodo1)

p1<- ggplot(cuad_recursos_wide, aes(x=x, y=y, fill=dif_periodos))
p1 + ggtitle("Flower abundance variation") + 
  geom_tile() + scale_fill_gradient2(trans="pseudo_log") + coord_equal() + 
  facet_wrap(~Bosque, nrow = 1) + 
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), 
        axis.title = element_text(size = 13), legend.title.align = 0.1, 
        legend.title = element_blank(), strip.background = element_blank(), 
        strip.text = element_text(face = "bold", size=15)) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))

#heatmap diferencia de visitas por cuadradito
seg<- read.csv("./data/seg_clean.csv")
seg_count_visitas<- seg %>% 
  group_by(Periodo_fecha, Bosque, xy, X, Y) %>% summarise(visitas=n())
seg_wide<- seg_count_visitas %>% 
  pivot_wider(names_from = Periodo_fecha, values_from = visitas, names_prefix = "periodo")
seg_wide$periodo1[is.na(seg_wide$periodo1)]<- 0
seg_wide$periodo2[is.na(seg_wide$periodo2)]<- 0
seg_wide$dif_visitas<- (seg_wide$periodo2 - seg_wide$periodo1)
p2<- ggplot(seg_wide, aes(x=X, y=Y, fill=dif_visitas))
p2 + ggtitle("Visitation rate variation") + 
  geom_tile() + scale_fill_gradient2(trans="pseudo_log") + coord_equal() + 
  facet_wrap(~Bosque, nrow = 1) + 
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), 
        axis.title = element_text(size = 13), legend.title.align = 0.1, 
        legend.title = element_blank(), strip.background = element_blank(), 
        strip.text = element_text(face = "bold", size=15)) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
