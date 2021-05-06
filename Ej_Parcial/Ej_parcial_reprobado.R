#ej parcial
rm(list=ls())
graphics.off()
setwd("C:/Users/Alumno/Desktop/Estadistica2/Ej_Parcial/")
source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")

library(readxl)
datos<-as.data.frame(read_xlsx("datos_alumnos_3_parcial.xlsx"))

#datos<-datos[1:108,]

plot.ts(datos[,2])

#no hay outliers ni datos faltantes

library(Kendall)

mk<-MannKendall(datos[,2])
N<- length(datos[,1])
desvio<- sqrt((4*N+10)/(9*N*(N-1))) 
t<-mk$tau 
tau_tab<-(t/desvio) 

tau_tab

#tau_tab es mayor que t (0.05 significancia a dos colas)
#la serie no es al azar

ajuste<-lm(datos[,2]~seq(1:length(datos[,1])))
summary(ajuste)
#el p valor es menor que 0.05
#rechazar h0
#la tendencia es significativa


datos[,1]<-seq(1:length(datos[,1]))

filtrado<-analisis_tendencia(datos)
filtrado<-as.table(filtrado)

ajuste<-lm(filtrado~seq(1:length(filtrado)))
summary(ajuste)

#ahora no tiene tendencia

#veo ciclos.
#aplico analisis armonico
funcion_armonico(filtrado)
#N 114 
#delta t 1
#los armonicos 9 y 10 se llevan mas de un 30% de varianza
#podria filtrarlos pero nse si son significativas
#aplico espectro

datos<-as.matrix(filtrado)
#a la funcion espectro entran como matriz...
espectro(datos)
#periodos significativos 1] 13.680000 11.400000  9.771429

#filtro los armonicos 9 10
fil_9<-filonda(datos)
fil_10<-filonda(fil_9)
fil_11<-filonda(fil_10)
funcion_armonico(fil_10)

#vuelvo aplicar espectro
fil_11<-as.matrix(fil_11)
espectro(fil_11)

###periodos significativos 13.680000 11.400000  9.771429  6.218182  5.700000

#no puedo remover toda la onda anual ya que N es 114 y no existe un armonico que explique toda la varianza asociada a esa onda

fil_8<-filonda(fil_11)
espectro(as.matrix(fil_8))
#6.218182 5.700000
#se pueden remover datos para que un armonico tenga el periodo 12 asociado y explique toda la varianza del ciclo anual
#y asi al filtrarlo saco toda la varianza  de la onda anual

#al aplicar wavelet se pueden apreciar periodos mas cortos significativos
