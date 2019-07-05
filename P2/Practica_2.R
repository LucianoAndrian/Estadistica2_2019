#PRACTICA 2-TENDENCIA Y SALTOS#
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P2/")
####EJ 1####
rm(list=ls())
#SERIE A   Va hasta 2.3Pi
serieA<-cos(seq(0,2.3*pi,by=pi/99))
plot(serieA)
#tiempoA
#SERIE B   Va hasta 1.5Pi
serieB<-cos(seq(0,1.5*pi,by=pi/98))
plot(serieB)

#SERIE C   Va hasta 2Pi
serieC<-cos(seq(0,2*pi,by=pi/101.5))
plot(serieC)

#SERIE D   Va hasta Pi
serieD<-cos(seq(0,pi,by=pi/99))
plot(serieD)

ajuste<-lm(serieA~seq(1:length(serieA)))
summary(ajuste)
#p valor <0.05 rechazo h0
#h0 que b pertenece a un B=0
#tiene tendencia y es significativa

fisher.test(datos)
#---KENDAL---#
library(Kendall)
datos<-list(serieA,serieB,serieC,serieD)
mk<-list()
tau_tab<-vector()
#ll

for(i in 1:4){
  mk[[i]]<- MannKendall(datos[[i]])
  N<- length(datos[[i]])
  desvio<- sqrt((4*N+10)/(9*N*(N-1))) #calculo el desvio de Tau seg???n Nota T???cnica 79
  t<-mk[[i]]$tau #extraigo el valor de Tau
  tau_tab[i]<-(t/desvio) # este es el valor que tengo que comparar con la Tabla Normal
}

tau_tab
#todas tienen tendencia salvo la C 

#testear con fisher
#F=R2/nu1*nu2/(1-R2)

####EJ 2####
rm(list=ls())
library(readxl)

datos=list()

#AGREGEUE UNA PRIMERA FILA EN LOS ARCHIVOS XLS PORQUE LO PONE COMO ENCABEZADO
for(i in 1:2){
datos[[i]]<-as.data.frame(read_excel("datos_ejercicio_2_P6.xlsx",sheet = i,col_types = c("numeric","numeric","numeric","numeric","numeric")))
}

eze<-datos[[1]]$`1__4`/0.75
aero<-datos[[2]]$`1__4`/0.75

plot(eze,type = "l")
lines(aero,col="red")

plot(eze,aero) 
#hay dos datos en cualquier parte..., el resto casi en linea recta.

#Doble Masa#
eze2<-cumsum(eze)
aero2<-cumsum(aero)

plot(eze2,type="l",col="red")
lines(aero2) #no se observa salto

#a partir de cierto tiempo 1hpa=0.85mmHg
aero_error<-aero
aero_error[round(length(aero)/2):length(aero)]<-aero_error[round(length(aero)/2):length(aero)]*0.75/0.85
plot(aero_error,type="l") #salto en posicion 76

#dispercion
plot(eze, aero_error)

#doble masa
aero_error2<-cumsum(aero_error)
plot(eze2,type="l",col="red")
lines(aero_error2) #se observa cambio en la pendiente a partir del tiepo 76

#Marona#
#serie sin el salto eze x
#con salto aero_error y
VAR<-read.table("datos_ej2.txt",header = F)
VAR[,2]<-VAR[,2]/0.75
VAR[,3]<-VAR[,3]/0.75
VAR[round(length(VAR[,3])/2):length(VAR[,3]),3]<-VAR[round(length(VAR[,3])/2):length(VAR[,3]),3]*0.75/0.85
#datos diarios!

source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/marona_funcion.R")
marona(VAR)

####EJ 3####
rm(list=ls())
library(readxl)

datos<-as.data.frame(read_xlsx("datos_pp_ejer_3.xlsx"))

rownames(datos)<-seq(1901:2000)

y_limit<-max(datos[,1:3],na.rm=T)

plot(datos$Año,datos$Pilar,xlim = c(1901,2000),ylim= c(0,y_limit),type="l",col="red",ylab="mm",xlab="Anio")
lines(datos$Año,datos$`Gral. Pico`,xlim = c(1901,2000))
lines(datos$Año,datos$Corrientes,xlim = c(1901,2000),col="blue")
mtext(text = "Pilar",col="red",side = 3,adj = 0)
mtext(text = "Gral. Pico",side = 3,adj = 0.38)
mtext(text = "Corrientes",col= "blue",side = 3,adj = 1)

#tendencialineal.
#kendall
library(Kendall)

mk<-list()
tau_tab<-vector()

#tau_crit<-vector()

for(i in 2:4){
  mk[[i]]<- MannKendall(datos[,i])
  N<- length(datos[[i]])
  desvio<- sqrt((4*N+10)/(9*N*(N-1))) #calculo el desvio de Tau seg???n Nota T???cnica 79
  #tau_crit[i]<-1.96*sqrt((4*N+10)/(9*N*(N-1)))
  t<-mk[[i]]$tau #extraigo el valor de Tau
  tau_tab[i]<-(t/desvio) # este es el valor que tengo que comparar con la Tabla Normal
}

tau_tab
#todas presentan tendencia
#en que periodo? a ojo seria en la 2da mitad de la serie...

#pruebo las dos mitades
tau_tab2<-vector()
for(i in 2:4){
  mk[[i]]<- MannKendall(datos[1:50,i])
  N<- length(datos[1:50,i])
  desvio<- sqrt((4*N+10)/(9*N*(N-1))) #calculo el desvio de Tau seg???n Nota T???cnica 79
  #tau_crit[i]<-1.96*sqrt((4*N+10)/(9*N*(N-1)))
  t<-mk[[i]]$tau #extraigo el valor de Tau
  tau_tab2[i]<-(t/desvio) # este es el valor que tengo que comparar con la Tabla Normal
}  
tau_tab2 
#miro solo Pilar y Corrientes, ya que Gral Pico no tiene valores
#no presentan tendencia
#MannKendall en Gral, Pico da tau = 1, 2-sided pvalue =1
#PILAR TERMINA EN EL 89!!
tau_tab3<-vector()
for(i in 2:4){
  mk[[i]]<- MannKendall(datos[59:89,i])
  N<- length(datos[59:89,i])
  desvio<- sqrt((4*N+10)/(9*N*(N-1))) #calculo el desvio de Tau seg???n Nota T???cnica 79
  #tau_crit[i]<-1.96*sqrt((4*N+10)/(9*N*(N-1)))
  t<-mk[[i]]$tau #extraigo el valor de Tau
  tau_tab3[i]<-(t/desvio) # este es el valor que tengo que comparar con la Tabla Normal
}  
tau_tab3 
#solo Gral. Pico presenta tendencia. 
#solo estoy mirando los ultimos 50 años en comparacion con las otras series en las que tengo el doble
#en corrientes datos[,4], en los 100 años presenta tendencia
#al analizar solo la ultima parte de la serie, los ultimos 50 años, las otras estaciones no presentan tendencia 
#ULTIMA PARTE VER SALTO CON YAMAMOTO


####EJ 4####
rm(list=ls())

datos<-as.data.frame(read_xlsx("pp_corrientes.xlsx"))

plot(datos[,1],datos[,2],type="l") #se puede ver una tendencia diferente en cada una
#esto puede formar parte de una onda mas grande y al no tener datos entre medio de los dos
#periodos  y tampoco antes de 1914 no se puede determinar. Analizar tendencia en este caso 
#no seria significativo, por lo mencionado antes y por los pocos datos que que se tienen de cada periodo
#NO SE HAY SUFICIENTES DATOS PARA AFIRMAR UN CAMBIO EN LA CIRCULACION
serie1<-datos[1:which(datos[,1]==1944),2]
length(serie1) #31 datos
serie2<-datos[which(datos[,1]==1977):length(datos[,1]),2]
length(serie2) #22 datos

library(Kendall)

series<-list(serie1,serie2)
mk<-list()
tau_tab<-vector()

for(i in 1:2){
  mk[[i]]<- MannKendall(series[[i]])
  N<- length(datos[[i]])
  desvio<- sqrt((4*N+10)/(9*N*(N-1))) #calculo el desvio de Tau seg???n Nota T???cnica 79
  t<-mk[[i]]$tau #extraigo el valor de Tau
  tau_tab[i]<-(t/desvio) # este es el valor que tengo que comparar con la Tabla Normal
}

tau_tab
#el test da que existe una tendencia en las dos

