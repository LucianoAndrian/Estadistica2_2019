#Practica7-Wavelet#
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P7")
#####
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
plot.ts(datos2)

# la serie presenta gran variabilida

# lo de simpre, tiene tendencia, saltos?

# tendencia primero chamigo!!!
summary(lm(datos2~seq(1:828)))
#tendencia no es significativa por muy poco

# el periodo de la serie va desde 1950 a 2018

# sabemos de un salto climatico en algunas variables a partir de anio '79 '80
# busco un salto entre esas fechas con el test de yamamoto
Y<-vector()
for(i in 1:48){
  datosA<-datos2[1:(336+i)]
  datosB<-datos2[(337+i):828]
  mediaA<-mean(datosA)
  mediaB<-mean(datosB)
  cA<-sd(datosA)*qt(0.975,df=827)*1/sqrt(828-1)
  cB<-sd(datosB)*qt(0.975,df=827)*1/sqrt(828-1)
  #tcritico 1,96
  Y[i]<-(mediaA-mediaB)/(cA+cB)
  rm(datosB)
}
#un salto a partir de la posicion 337+1--->correspondiente al año 1978
#creo la serie nueva
max(Y)
i=1
nueva<-c(datosA+mediaB,datosB) #aclaracion, mediaB es negativa

#la serie sin el salto 
summary(lm(nueva~seq(1:828)))
# tamoco y el p valor es mucho grande que el alfa

# voy a seguir trabajndo utilizando la serie a la que le quite el salto, ya que si bien 
# ninguna de las dos presenta tendencia significativa, la 2da tiene un pendiente menor

# antes de wavelet me intereza aplicar analisis armonico y espectral 

source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")
# delta t 838 meses 
# este indice se usa como monitoreo para el ninio
# el cual presenta una periodicidad, con una varaibiidad un poco alta, de 3 años
# por lo tanto esperarimos ver armonicos relacionads con esa frecuencia, que serian ~23
# y periodos significativos cercanos a esa frecuencia
funcion_armonico(nueva) # no se ve nada... muy poca varianza explicada debido a la enorme cantidad de armonicos
                        # los menores armonicos ( 5 - 31 ) son los explican mas
# el armonico 19 es el que mas varianza explica~5.56%
# periodo asociado 828/19--->3.63 anios!! bieen

espectro(as.matrix(nueva))
#[1] 165.600000 124.200000  62.100000  >>>55.200000<<<  >>>45.163636<<<  41.400000  31.050000 
#29.223529   4.072131
#[10]   3.268421   3.205161   2.871676   2.855172   2.560825   2.547692   2.534694
#2.496482   2.484000
#[19]   2.471642   2.459406   2.400000   2.388462   2.247964   2.237838   2.227803
#2.198230   2.188546
#[28]   2.036066   2.027755   2.003226

# se ven un monton de periodos significativos, los relacionados al ninio serian los 45 y 55 meses
# si es mas o menos 3-5 años el 41, 31, el 62 tambien entrarian

dat<-data.frame(seq(1:length(nueva)),nueva)

library(WaveletComp)

my.wt =analyze.wavelet(dat, my.series=2)


wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)


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
         spec.time.axis = list(at = q, labels = prueba2),color.key='q',plot.ridge = F)

# se puede observar periodos significativos dentro del rango que se espera encontrar relacionados con 
# el ninio