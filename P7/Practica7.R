#Practica7-Wavelet#
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P7")
####Ej2####
x<-seq(0,10*pi,by = pi/99.5)
#x ~ 1000
s1<-sin(6*x)
s2<-sin(15*x)

serie<-matrix(nrow=length(x),ncol = 2)

serie[,1]<-seq(1:length(serie[,2]))
serie[,2]<-c(s1[1:455],s2[455:995])

library(WaveletComp)

dat<-data.frame(serie) #para usar analyze.wavelet

my.wt = analyze.wavelet(dat, my.series=2) 
wt.image(my.wt,color.key="i", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)

####Ej3####
rm(list=ls())
graphics.off()   

datos<-read.table("datos_SOI.txt")
#datos SOI indice que sirve de monitoreo del ninio.
#relacion inversa
#recordar ninio perioricidad ~3 años (con gran variabilidad)

datos<-datos[,-1]
datos<-t(datos)
datos2<-array(datos,dim=c(12*69,1))

#para usar analize.wavelet
dat<-data.frame(seq(1:length(datos2)),datos2)

plot.ts(datos2)

#todas las funciones
source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")

funcion_armonico(dat[,2])

espectro(datos2)

library(WaveletComp)

my.wt =analyze.wavelet(dat, my.series=2)

png('waveletfigura_1.png',width = 800,height = 600,units = "px")
wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)
dev.off()

#se ve mejor con cuantiles que en intervalos 

## Plot of average wavelet power:
wt.avg(my.wt, siglvl=0.05, sigcol="red")


#PARA AGREGAR FECHA AL EJE DE TIEMPO TENER EN CUENTA LO SIGUIENTE
monthyear <- seq(as.Date("1950-01-01"), as.Date("2018-12-01"),
                 by = "month")
monthyear <- strftime(monthyear, format = "%b %Y")
q<-seq(1,length(monthyear),20) ##CUANTOS PUNTOS ME QUEDO
prueba2<-monthyear[q]

wt.image(my.wt, main = "wavelet power spectrum", 
         periodlab = "period (months)", timelab = "month and year",
         spec.time.axis = list(at = q, labels = prueba2),color.key='q')
###