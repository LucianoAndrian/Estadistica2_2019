#Practica3-TENDENCIA Y PROMEDIOS MOVILES
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P3/")
source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")

####EJ1#####
rm(list=ls())
graphics.off()

library(readxl)
datos<-as.data.frame(read_xlsx("datos_pp_ejer_3.xlsx"))

serie1<-datos[,1:2]
serie2<-data.frame(datos$Año,datos$`Gral. Pico`)
serie3<-data.frame(datos$Año,datos$Corrientes)
series<-list(serie1,serie2,serie3)
series_filtradas<-list()

#VER, EL VALOR AJUSTE1$COEFFICIENTS[1] ES MUY NEGATIVO (~-3000), HACER EL USAR LA 2DA FORMA
#DE FILTRAR LA TENDENCIA NO AL REDEDOR DEL CERO SINO DE LOS VALORES DE LA SERIE
#DA CUALQUIER COSA, MUY NEGATIVA YA QUE SE OMITE ESE VALOR AL RESTAR
#PROBADO TANTO EN LA FUNCION COMO EN EL SCRIP DE CLASE
#
##SOLUCIONADO: LA COLUMNA DE LOS TIEMPOS EN LA FUNCION DEBE TENER UNA SECUENCIA DESDE EL 1##
##HASTA EL FINAL. EN EL EJERCICIO DE LA CLASE NO EXISTIO EL PROBLEMA PORQUE REEMPLAZE TODA## 
##LA COLUMNA MESES POR UN UNA SECUENCIA.                                                  ##
##AHORA ESO LO HACE DENTRO DE LA FUNCION                                                  ##
#

source("funcion_analisis_tendencia.R")
for(i in 1:3){
  series_filtradas[[i]]<-analisis_tendencia(series[[i]])
}

#Pilar
plot(serie1[,1],serie1[,2],type = "l")
lines(serie1[which(!is.na(serie1[,2])),1],series_filtradas[[1]],type="l",col="red",title("Pilar"))

#Pico
plot(serie2[,1],serie2[,2],type = "l")
lines(serie2[which(!is.na(serie2[,2])),1],series_filtradas[[2]],type="l",col="red",title("Gral. Pico"))

#Corrientes
plot(serie3[,1],serie3[,2],type = "l")
lines(serie3[which(!is.na(serie3[,2])),1],series_filtradas[[3]],type="l",col="red",title("Corrientes"))

####EJ2####
rm(list=ls())
graphics.off()

datos<-read.table("datos_ej2.txt")

#dar pesos simetricos para 3,5,7 y 11 que sumen 1...
#para3       0.25 0.5  0.2
#5      0.1  0.2  0.4  0.2  0.1
# 0.066 0.13 0.15 0.3  0.15 0.13 0.066  ????
#como hago los otrooos  a mano

#source("funcion_prom_movile.R")
#USAR LA 2.0
datos2<-promedios_moviles(datos)

datos3<-as.data.frame(cbind(datos,datos2))

library(ggplot2)
ggplot(datos3,aes(x=datos[,1]))+
  geom_line(aes(y=datos3[,2]))+
  geom_line(aes(y=datos3[,3]),colour="red",lwd=1)+
  geom_line(aes(y=datos3[,4]),colour="green",lwd=1)+
  geom_line(aes(y=datos3[,5]),colour="blue",lwd=1)+
  geom_line(aes(y=datos3[,6]),colour="yellow",lwd=1)+
  theme_bw()


#ANOMALIAS
#RESTAR LA MEDIA! 
#QUE SUPERE UN DESVIO NO QUIERE DECIR QUE SEA ANOMALIA

datos3<-datos3-mean(datos[,2])
plot.ts(anomalias)  
medias<-vector()
desvios<-vector()
#i in 1:4
for(i in 1:4){
  medias[i]<-mean(datos2[,i],na.rm=T)
  desvios[i]<-sd(datos2[,i],na.rm=T)
}

ggplot(datos3,aes(x=datos[,1]))+
  geom_line(aes(y=datos3[,2]))+
  geom_line(aes(y=datos3[,3]),colour="red",lwd=1)+
  geom_line(aes(y=datos3[,4]),colour="green",lwd=1)+
  geom_line(aes(y=datos3[,5]),colour="blue",lwd=1)+
  geom_line(aes(y=datos3[,6]),colour="yellow",lwd=1)+
  theme_dark()
  geom_hline(yintercept = medias[1]+desvios[1],colour="red")+
  geom_hline(yintercept = medias[1]-desvios[1],colour="red")+
  geom_hline(yintercept = medias[2]+desvios[2],colour="green")+
  geom_hline(yintercept = medias[2]-desvios[2],colour="green")+
  geom_hline(yintercept = medias[3]+desvios[3],colour="blue")+
  geom_hline(yintercept = medias[3]-desvios[3],colour="blue")+
  geom_hline(yintercept = medias[4]+desvios[4],colour="yellow")+
  geom_hline(yintercept = medias[4]-desvios[4],colour="yellow")

##
#plot(datos[,1],datos[,2],type="l")
#lines(datos[,1],datos2[,1],col="red")
#lines(datos[,1],datos2[,2],col="blue")
#lines(datos[,1],datos2[,3],col="green")
#lines(datos[,1],datos2[,4],col="yellow")
#abline(h=medias[1]+desvios[1],col="red")
#abline(h=medias[1]-desvios[1],col="red")
#abline(h=medias[2]+desvios[2],col="blue")
#abline(h=medias[2]-desvios[2],col="blue")
#abline(h=medias[3]+desvios[3],col="green")
#abline(h=medias[3]-desvios[3],col="green")
#abline(h=medias[4]+desvios[4],col="yellow")
#abline(h=medias[4]-desvios[4],col="yellow")
##
####EJ3####

rm(list=ls())
library(readxl)

datos<-as.data.frame(read_xlsx("Temp_media_mensual.xlsx"))
datos[,1]<-seq(1:length(datos[,1]))

source("funcion_prom_moviles.R")
#USAR LA 2.0
datos2<-promedios_moviles(datos)

plot(datos[,1],datos[,2],type="l")
lines(datos2,col="red")
