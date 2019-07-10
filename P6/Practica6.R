#Practica6-Espectro
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P6")
source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")
######Ej1######
rm(list=ls())
graphics.off()

library(readxl)
datos<-read.table("datos_ej1_p6.txt",header=T)
#datos mensuales desde 1998 111 meses
#finaliza marzo de 2007

source("Funcion_espectro.R")
espectro_hann<-espectro(datos)
#13.320000 11.100000  9.514286

espectro_hamming<-espectro(datos)
#13.32 11.10

espectro_parzen<-espectro(datos)
#13.320000 11.100000  9.514286

#en los tres casos dio H0 ruido rojo.

source("C:/Users/Alumno/Desktop/Estadistica2/P5/funcion_armonico.R")
armonicos<-funcion_armonico(datos[,1])

#armonico 9 mucha varianza ~ 77% de varianza
#111/9 ~12.33 meses.

######EJ2######
rm(list=ls())
graphics.off()

x<-seq(0,2*pi,by = pi/99.5)
length(x) #=100

s1<-sin(x)
s2<-sin(2*x)
serie<-s1*s2

png(filename = "Ej2_serie_2ondas.png",width = 800,height = 600,units = "px")
plot.ts(s1)
lines(s2,col="green")
lines(serie,col="red")
dev.off()

serie<-as.matrix(serie)

source("Funcion_espectro.R")
espectro_serie<-espectro(serie)
#120  60  40

png(filename = "Ej2_serie_3ondas.png",width = 800,height = 600,units = "px")
plot.ts(serie+sin(70*x))
dev.off()

serie2<-serie+sin(70*x)        
serie2<-as.matrix(serie2)

espectro(serie2)
#Inf 120.000000  60.000000   2.926829   2.857143   2.790698

######Ej3######
rm(list=ls())
graphics.off()

datos<-read.table("datos_ej_3.txt",header=T)
#los datos tienen la misma pinta que lso de la practica 5. ..#es el mismo!!
#delta t 238 horas
#113 datos, saco el ultimo
#112 cada 238hs---->3.04 anios, ~36 meses
#periodo asocidad a una onda anual 

plot.ts(datos)

datos<-as.matrix(datos[1:112,1])

source("Funcion_espectro.R")
espectro_datos<-espectro(datos)

#13.44 11.20  9.60  8.40

source("C:/Users/Alumno/Desktop/Estadistica2/P5/funcion_armonico.R")
funcion_armonico(datos)

#source("C:/Users/Alumno/Desktop/Estadistica2/P5/filonda_funcion.R")
#datos_fil<-filonda(datos_fil)
#datos2<-as.matrix(datos_fil)
#espectro(datos2)
#funcion_armonico(datos

#\m/#