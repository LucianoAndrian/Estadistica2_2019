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

#para detectar los periodos secos y humedos, calculo las anomalias
#ANOMALIAS
#RESTAR LA MEDIA! 
#QUE SUPERE UN DESVIO NO QUIERE DECIR QUE SEA ANOMALIA

datos3<-datos3-mean(datos[,2])
plot.ts(datos3)  


ggplot(datos3,aes(x=datos[,1]))+
  geom_line(aes(y=datos3[,2]))+
  geom_line(aes(y=datos3[,3]),colour="red",lwd=1)+
  geom_line(aes(y=datos3[,4]),colour="green",lwd=1)+
  geom_line(aes(y=datos3[,5]),colour="blue",lwd=1)+
  geom_line(aes(y=datos3[,6]),colour="yellow",lwd=1)+
  theme_dark()+
  geom_hline(yintercept = 0,colour="red")

#las series con promedios moviles con ventanas mas grandes son las que mas diferencias presentan 
#en cuanto a periodos humedos y secos con respecto a la serie original
#esto se debe a que el valor asginado a cada punto contempla un promedio entre mas variables
#por lo que la serie se suaviza mucho y periodos cortos, tanto secos como humedos, que sean menores 
#a la cantidad a promediar pueden no ser vistos luego de aplicar el promedio movil

####EJ3####

rm(list=ls())
library(readxl)

datos<-as.data.frame(read_xlsx("Temp_media_mensual.xlsx"))
datos2<-datos[,-1]
#presenta tendencia significativa??
summary(lm(datos2~seq(1:length(datos2))))
#no, tiene tendencia significativa


funcion_armonico(datos2)###son 360 datos mensuales correspondientes a 30 años
#claramente el armonico 30 se lleva casi el 100 de la varianza
#y al aplicar 
espectro(as.matrix(datos2))
#vemos que es significativo
#
#pero quiero filtrar el ciclo anual, pero sin utilizar armonicos
#restar la media de cada mes a toda la serie en cada mes correspondiente
#por como esta dada la informacion no me sirve, asi que la convierto en una matriz que tenga
#en cada columna los meses del año
#y que cada fila sea un año

datos3<-t(array(datos2,dim=c(12,30)))

#calculo las medias
medias<-vector()
for(i in 1:12){
  medias[i]<-mean(datos3[,i])
}

#resto a la serie original--->calculo anomalias
anomalias<-matrix(nrow = 30,ncol=12)
for(i in 1:12){
  anomalias[,i]<-datos3[,i]-medias[i]
}

#desarmo el array en una unica serie nuevamente
datos4<-t(anomalias)
datos4<-array(datos4,dim=c(12*30,1))
plot.ts(datos4)

#a primera vista no se el ciclo anual
#veamos con analisis armonico
funcion_armonico(datos4)
#vemos que ya no esta presente la onda anual
#se ve aliasing

#####INVENTO####
#Q OTROS EFECTOS HAY AHORA??
espectro(as.matrix(datos4))
#[1] 3.927273 3.130435 2.482759 2.037736 2.018692
#periodos significativos de 2 meses--->aliasing, ya que 2 meses es la frecuencia de Nyquist o de corte
#la metodologia no puede ver ciclos menores a esas frecuencia, pero si repliega varianza que se manifiesta
#en la autocorrelacion de la serie y por lo tanto se logra ver en el espctro ya que este 
#se calcula a partir de la autocorrelacion
#este efecto tambien se ve en el analisis armonico

#ondas significativas de 4-3 meses
# y quizas aparecerian las de  10 meses en wavelet...
#veamos

library(WaveletComp)

dat<-data.frame(seq(1:360),datos4)

my.wt =analyze.wavelet(dat, my.series=2)

wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)
#se pueden apreciar los periodos significativos dados por el analisis espectral
#pero tambien vemos como aparecen periodos significarticos de 8 meses
#y de entre 16 a 32 meses que en el analisis espectral no aparecian
#estos se hacen significatrivos en wavelet ya que la metodologia va realizando analisis espectrales
#a lo largo de la serie en ventanas de tiempo mas pequeñas que el periodo fundamental
#entonces energia relacionadas a frecuencias que no aparecen durante todo el periodo
#se pueden apreciar mejor.
#aun asi, hay que tener en cuenta q estos nuevos periodos significativos se encuentan cerca del comienzo
#y final de la serie, con lo que hay que tener precuacion en cuanto a su veracidad
#ya que para realizar el analisis espectral completa la serie con ceros hasta menos y mas infinito
# y puede que no sea representativo esos valores de sifnificancia que se ven en los extremos

#SE PUEDE VER ALIASING... SE VE EN LA AUTCORRELACION??

resul<- acf(datos4,type="correlation",plot=T,lag.max=3*length(datos4[,1])/10)
#no se ve aliasing, o quizas muy poco hacia los ultimos lags

valores<-resul$acf
N<-360

# asi hace la acf las bandas de significancia qnorm((1 + 0.95)/2)/sqrt(x$n.used)
#pensado para dos colas con 95% de significancia


rkpos<-rep(NA,length(valores))
rkneg<-rep(NA,length(valores))

ci=0.95 #defino el nivel de confianza entre 0 y 1
for(k in 0:(length(valores)-1)){
  
  rkpos[k+1]<- (-1+qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k)  #test de anderson para las bandas de significacnia!!! jaajaja
  
  rkneg[k+1]<- (-1-qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k) 
  
}

plot.ts(valores,ylim=c(-1,1))
lines(rkpos,lty=2,col='red')
lines(rkneg,lty=2,col='red')

barplot(t(as.matrix(valores)),ylim=c(-1,1),col="green4",xlim=c(1,k+1),lwd=2,names.arg = c(1:length(valores)))
lines(rkpos,lty=2,col='red',lwd=1.8)
lines(rkneg,lty=2,col='red',lwd=1.8)
