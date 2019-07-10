#Practica5-A.ARMONICO
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P5/")
####Ej1####
graphics.off()
rm(list=ls())

serieA<-cos(seq(0,2.3*pi,by=pi/99))

serieB<-cos(seq(0,1.5*pi,by=pi/98))

serieC<-cos(seq(0,2*pi,by=pi/101.5))

serieD<-cos(seq(0,pi,by=pi/99))

source( "C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")
funcion_armonico(serieA)
funcion_armonico(serieB)
funcion_armonico(serieC)
armonico<-funcion_armonico(serieD)

#periodo fundamental P=nAt
#periodo de cada armonico Pi=P/i=nAt/i
#frecuencia fundamental la onda mas larga, la que landa es igual a T
#frecuencia de los armonicos fi=i/nAt
#frecuencia mas corta fc=1/2At ----->si hay ondas con periodos mas cortos que 2At no se ven
#--->aliasing

####Ej2####
graphics.off()
rm(list=ls())

datos<-read.table("datos_armo_bien.txt")

#a
grafico<-plot.ts(datos) #se observa el ciclo anual

#b
N<-length(datos[,1])
P<-N*1
P9<-N/9 #12,222 para filtrar onda anual.
source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")
#c
armonicos<-funcion_armonico(datos[,1]) #grafico uardado 
#armonicos[,which(armonicos[,4]==max(armonicos[,4]))]

which(armonicos[,4]==max(armonicos[,4])) #armonico que exlica mas varianza... el 9
#el ciclo anual predomina en la serie impidiendo ver otros fenomenos
#es necesario filtrarlo
####invento####
#la serie tiene tendencia???
summary(lm(datos[,1]~seq(1:110)))
#la tendencia no es significativa
#filtrado el armonico 9
funcion_armonico(filonda(datos[,1]))
#aun asi queda el armonico 8 10 y 11 explcando mucha varianza
#esto se debe a que la longitud de la serie es 110 que no es multiplo de 12 y por lo tnato ninugn
#armonico tendra todo el periodo de 12 años y la varianza de este ciclo estara explicada por varios
#armonicos
#si quisiera filtrarla mas facil para poder visualizar otros efectos
#podria sacar los ultimos dos valores y asi tener 108 datos de esta manera el armonico 9
#tendria el periodo de 12 meses y al removerlo sacaria todo el efecto de la onda anaual de una vez
datos_2<-datos[1:108,1]
funcion_armonico(datos_2)
#ahora la varianza explicada por el armonico 9 es mauyor que antes, ~90%
funcion_armonico(filonda(datos_2))
#ahora si se pudo filtrar la onda anual y se visualizan mejor otras ondas.
plot.ts(filonda(datos_2))
summary(lm(filonda(datos_2)~seq(1:108)))
#ahora la serie presenta tendencia significativa
datos_2_fil<-analisis_tendencia(as.matrix(data.frame(seq(1:108),filonda(datos_2))))
funcion_armonico(datos_2_fil)

resul<- acf(datos_2_fil,type="correlation",plot=T,lag.max=3*length(datos_2_fil)/10)
#no se ve aliasing, o quizas muy poco hacia los ultimos lags
valores<-resul$acf
N<-length(datos_2_fil)
# asi hace la acf las bandas de significancia qnorm((1 + 0.95)/2)/sqrt(x$n.used)
#pensado para dos colas con 95% de significancia

rkpos<-rep(NA,length(valores))
rkneg<-rep(NA,length(valores))

ci=0.95 #defino el nivel de confianza entre 0 y 1
for(k in 0:(length(valores)-1)){
  rkpos[k+1]<- (-1+qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k)  #test de anderson para las bandas de significacnia!!! jaajaja
  rkneg[k+1]<- (-1-qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k) 
}


barplot(t(as.matrix(valores)),ylim=c(-1,1),col="green4",xlim=c(1,k+1),lwd=2,names.arg = c(1:length(valores)))
lines(rkpos,lty=2,col='red',lwd=1.8)
lines(rkneg,lty=2,col='red',lwd=1.8)

espectro(as.matrix(datos_2))
espectro(as.matrix(datos_2_fil))

library(WaveletComp)
dat<-data.frame(seq(1:108),datos_2_fil)
my.wt =analyze.wavelet(dat, my.series=2)
wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)

####EJ3####
#filtro ciclo anual, promedio movil, armonico 9 y otra forma mas???
#promedio movil NO filtra, suaviza, elimina ruido y ondas menores. la onda anual sigue estando
#otro forma, la forma en la que se hace - restar la media de cada mes a cada mes 

source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")

#voy a usar datos2, ya que una de las metodologias es restar la media de cada mes a cada mes
#la idea es armar un array y hacerlo ahi, pero en la serie original tengo 110 meses lo cual no 
#permitiria armar un array en datos_2 usando desde enero del 1999 hasta diciembre del 2007

#filtrado_1: restando la media de cada mes
datos_3<-t(array(datos_2,dim=c(12,9)))

#calculo las medias
medias<-vector()
for(i in 1:12){
  medias[i]<-mean(datos_3[,i])
}

#resto a la serie original--->calculo anomalias
anomalias<-matrix(nrow = 9,ncol=12)
for(i in 1:12){
  anomalias[,i]<-datos_3[,i]-medias[i]
}

#desarmo el array en una unica serie nuevamente
datos_4<-t(anomalias)
datos_4<-array(datos_4,dim=c(12*9,1))
plot.ts(datos_4)

#filtrado 2: armonico
#ya lo hice antes 
datos_2_1<-filonda(datos_2)
plot.ts(datos_2_1)

##estas series filtradas parecen tener tendencia
summary(lm(datos_4~seq(1:108))) #filtrado1 tiene tendencia y es sifnificativa
summary(lm(datos_2_1~seq(1:108))) #tambien 
#filtro la tendencia antes de aplicar analisis armonico, ya que podria llevarse mucha varianza y no
#estaria asociado a un ciclo
fil_4<-analisis_tendencia(as.matrix(data.frame(seq(1:108),datos_4)))
fil_2_1<-analisis_tendencia(as.matrix(data.frame(seq(1:108),datos_2_1)))

#analisis armonico
funcion_armonico(fil_4)
funcion_armonico(fil_2_1)

#la serie filtrada con el armonico presenta mas armonicos con mas variable que la filtrada con las medias
#aun asi los periodogramas son muy similares

####invento####
espectro(as.matrix(fil_4))
#3.240 2.700 2.592
espectro(as.matrix(fil_2_1))
#6.480000 5.890909
library(WaveletComp)
dat<-data.frame(seq(1:108),datos_4)
my.wt =analyze.wavelet(dat, my.series=2)
wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)

dat<-data.frame(seq(1:108),datos_2_1)
my.wt =analyze.wavelet(dat, my.series=2)
wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)

#los metodos de filtrado son diferentes-->obvio q da distinto.. casi igual


####prueba####
prueba<-datos_2[1:24]
funcion_armonico(prueba)
prueba1<-filonda(prueba)
prueba2<-t(array(prueba,dim=c(12,2)))

#calculo las medias
medias<-vector()
for(i in 1:12){
  medias[i]<-mean(prueba2[,i])
}

#resto a la serie original--->calculo anomalias
anomalias<-matrix(nrow = 2,ncol=12)
for(i in 1:12){
  anomalias[,i]<-prueba2[,i]-medias[i]
}

#desarmo el array en una unica serie nuevamente
datos_4<-t(anomalias)
datos_4<-array(datos_4,dim=c(12*2,1))
plot.ts(datos_4)

plot.ts(prueba)
#armonico

Xt<-vector()
for(i in 1:24){
  Xt[i]<-60.6523*sin(2*pi*2*i/24)+108.9172*cos(2*pi*2*i/24)
}

XS[J]<-A*sin(2*PI*M*J/P)+B*cos(2*PI*M*J/P)


####EJ4####
graphics.off()
rm(list=ls())

#datos medidos cada 238hs
#3 anios de medicion
datos<-read.table("autocorr.txt",header = T)

N=length(datos[,1])   
#datos impares borrar el ultimo
#dt=238
datos<-datos[1:112,1]
plot.ts(datos)

source("funcion_armonico.R")
armonico<-funcion_armonico(datos)
#Armonico 3 explica mas varianza...
#A3--->~ciclo anual

source("filonda_funcion.R")
FIL<-filonda(datos)

armonico2<-funcion_armonico(FIL)

plot.ts(datos[,1])
lines(FIL,col="red")

#lines(FIL2,col="green")
#FIL2<-filonda(FIL)