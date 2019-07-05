#ej extra
rm(list=ls())
serie<-read.table("datos_ej_clase_extra.txt",header = T)
plot(serie[,3],type="l") #CLARAMENTE TIENE TENDENCIA

#TESTEO CON kENDALL (DA LO MISMO QUE MANNKENDALL)
library(Kendall)
rank<-Kendall(serie$año,serie$pp) 
summary(rank) #LA SERIE NO ES AL AZAR

#FILTRO LA TENDENCIA
serie<-serie[,2:3]
source("funcion_analisis_tendencia.R")
datos_fil<-analisis_tendencia(serie)

plot(serie[,2],type="l")
lines(datos_fil,col="red")

serie2<-serie
serie2[,2]<-datos_fil
#PROMEDIO MOVIL
source("funcion_prom_moviles.R")
datos_prom<-promedios_moviles(serie2)
plot(datos_fil,type="l")
lines(datos_prom[,1],col="red")

#lines(datos_prom[,2],col="green")
#plot(datos_prom[,1],col="red",type="l")
#lines(datos_prom[,2],col="green")

#aplicar prom movil a la serie ya promediada
serie3<-serie2
serie3[,2]<-datos_prom
datos_prom2<-promedios_moviles(serie3)

plot(datos_prom,type="l")
lines(datos_prom2,col="red",type="l")
