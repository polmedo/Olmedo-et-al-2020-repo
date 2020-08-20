
library(bipartite)

seg<- read.csv("./data/seg_clean.csv")
seg_subset<- subset(seg, Polinizador %in% c("Bombus hortorum", "Bombus lapidarius", "Bombus pratorum", "Bombus pascuorum", "Bombus terrestris", "Sphaerophoria scripta"))
seg_subset<-droplevels(seg_subset)

sites <- unique(seg_subset$Bosque)


out.site <- data.frame(Site_id = NA, Periodo=NA, Polinizador=NA, species.pl=NA)


webs <- list()
for(i in 1:length(sites)){
  print(i)
  temp <- subset(seg_subset, seg_subset$Bosque == sites[i])
  
  periodo<-unique(seg_subset$Periodo_fecha)
  
  for(j in 1:length(periodo)){
    
    temp2<-subset(temp, temp$Periodo_fecha==periodo[j])
    
    polinizador<-unique(temp2$Polinizador)
    
    for(k in 1:length(polinizador)){
      temp3<-subset(temp2, temp2$Polinizador==polinizador[k])
      
      web<-table(temp3$Planta, temp3$Polinizador)
      
      
      plotweb(web, col.interaction = "darkgrey", col.high = "darkgreen", arrow="down.center", low.spacing = 0.5)
     
      
      ntw <- networklevel(web)
   
    
      
      n <- nrow(out.site)
      webs[[n]] <- web  
  
      
      out.site[n + 1,1] <- as.character(sites[i])
      out.site[n + 1,2] <- as.character(periodo[j])
      out.site[n + 1,3] <- as.character(polinizador[k])
      out.site[n + 1,4] <- c(ntw[20])
     
      
      
    }  }}

str(out.site)
head(out.site)
out.site

