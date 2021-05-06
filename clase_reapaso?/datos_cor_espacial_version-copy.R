library(ggplot2)
library(sp)
library(reshape2)
library(akima)

#setwd('/home/alumnos/Descargas')
datos<-read.table('datos_ninio34_mam.txt',header=TRUE,check.names = FALSE)
head(datos)
correlaciones<-cor(datos[-1])[1,]
correlaciones[-1]
##las correlaciones son mam desde 1979-2012 entre el ninio 3.4 y 67 observaciones

estaciones<-read.table('datos_estaciones.txt',header = TRUE)
head(estaciones)
DATOSCOR<-cbind(estaciones,correlaciones[-1])

##grafiquemos

#library(RColorBrewer)
##descargo mapa https://gadm.org/download_country_v3.html
ub_argentina='gadm36_ARG_1_sp.rds' ##el archivo que baje del mapa de internet
argentina = readRDS(ub_argentina)

plot(argentina)
class(argentina)
head(DATOSCOR)
colnames(DATOSCOR)<-c('lon','lat','estacion','corr')
nuevos<-cbind(DATOSCOR[,1:2],DATOSCOR[,4])
colnames(nuevos)<-c('lon2','lat2','corr')

#grafico mapa de argentina con las estaciones

p<-ggplot(data = argentina) + geom_polygon(aes(x = long, y = lat, group = group),fill=NA, color = "black") + 
  coord_fixed(1.3) + guides(fill=FALSE) 


tpoints <- geom_text(aes(x = lon, y = lat, label = estacion),color = "red", data = estaciones,size=1.5)
p+tpoints




#grafico de los contornos de correlacion

fld <- with(nuevos, interp(x = lon2, y = lat2,z=corr))#se arma la grilla

dfa <- melt(fld$z,na.rm = TRUE) # lo lleva a formato columnas, y elimina fila con NA
names(dfa) <- c("x", "y", "corr")
dfa$Lon <- fld$x[dfa$x] #cambia los indices por los valores de long
dfa$Lat <- fld$y[dfa$y] #cambia los indices por los valores de latitud
df<-dfa[which(!(dfa$Lat< -42.5 & dfa$Lon> -62.5 ) &
              !(dfa$Lat< -43.75 & dfa$Lon> -63.75 ) &
              !(dfa$Lat< -45 & dfa$Lon> -65 )),] #elimine ciertos puntos de la grilla


myplot<-p + geom_tile(data = df, aes(x = Lon, y = Lat, z = corr, fill = corr), alpha = 0.6) +
  
  stat_contour(data = df, aes(x = Lon, y = Lat, z = corr),colour='black',bins=10) 
  

final<-myplot+ scale_fill_continuous(name = "r",low = "blue", high = "red")+guides(fill='colourbar') + ggtitle("MAM NINIO3.4-PP")

ggsave('final.png', plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 15, height = 18, units = 'cm',
       dpi = 600, limitsize = TRUE)
 


