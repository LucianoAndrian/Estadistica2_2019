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

rownames(datos)<-c(1901:2000)

y_limit<-max(datos[,1:3],na.rm=T)

plot(datos$Año,datos$Pilar,xlim = c(1901,2000),ylim= c(0,y_limit),type="l",col="red",ylab="mm",xlab="Anio")
lines(datos$Año,datos$`Gral. Pico`,xlim = c(1901,2000))
lines(datos$Año,datos$Corrientes,xlim = c(1901,2000),col="blue")
mtext(text = "Pilar",col="red",side = 3,adj = 0)
mtext(text = "Gral. Pico",side = 3,adj = 0.38)
mtext(text = "Corrientes",col= "blue",side = 3,adj = 1)

#las series no presentan todas el mismo periodo--->no son comparables entre si asi como estan
#se debe seleccionar el mismo periodo para ello

#b)determinar si cada una de las series presenta tendencia lineal
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
#las tres series tienen una componente desterministica que permite rechazar la hipotesis nula 
#del test de kendall
#h0: la serie es al azar
#h1: la serie no es al azar
#para ver tendencia, lm y test de fisher

ajuste_pilar<-lm(datos$Pilar~datos$Año)
summary(ajuste_pilar) #tendencia significativa en el periodo 1907-1989

ajuste_gral.pico<-lm(datos$`Gral. Pico`~datos$Año)
summary(ajuste_gral.pico) #tendencia significativa en el periodo 1959 - 2000

ajuste_corrientes<-lm(datos$Corrientes~datos$Año)
summary(ajuste_corrientes) #tendencia significativa en el periodo 1901 - 1999

#c) para comparar pico con las otras tengo que seleccionar un periodo en comun
#pico tiene el periodo mas corto desde 1959 hasta 2000
#pero las demasseries no llegan hasta el año 2000
#asi que el periodo que voy a tomar va ser 1959 -1989

pilar_c<-datos$Pilar[59:89]
corrientes_c<-datos$Corrientes[59:89]
pico_c<-datos$`Gral. Pico`[59:89]
#la consigna indica que pico presenta un aumento con los años
#pero en este periodo tambien??
ajuste_pico_c<-lm(pico_c~seq(1:31))
summary(ajuste_pico_c) #si, presentan tendencia significativa

#veamos las otras en este mismo periodo

ajuste_pilar_c<-lm(pilar_c~seq(1:31))
summary(ajuste_pilar_c) #no presentea tendencia significativa en este periodo 

ajuste_corrientes_c<-lm(corrientes_c~seq(1:31))
summary(ajuste_corrientes_c) #tampoco presenta tendencia significativa en este periodo

#en el periodo 1959-1989 solo general pico presenta tendencia positiva y significativa
#en el aumento de la precipitacion.

#d)algun cambio significativo en la estacion corrientes 
#en todo el periodo??---parece que si por la pregunta que sigue

#ya sabemos que la estacion presenta una tendencia significativa en todo su periodo
#es necesario filtrarla para analizar si hay saltos??---> deberia serlo, ya que en este caso
#para ver si hay un salto voy husar el test de yamamoto el cual es praticmanete un test de medias
#entonces para evtar un cambio en la media producto de una tendencia mejor filtrarla 
#(en este punto de la cursada todvia no habiaos visto el scrip analisis de tendencia)

source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")
corrientes_fil<-analisis_tendencia(data.frame(datos$Año,datos$Corrientes))

#sabemos que existio un salto climatico en la pp entre el 79 y el 80
#miro 5 años antes y despues de esos años con el test de yamamoto buscando el maximo
#estadistico Y que me indica el salto (recordar que si es mayor a 1 indica un salto climatico)
#recordar corrientes va desde 1901 hasta 1999
Y<-vector()
for(i in 1:10){
  datosA<-corrientes_fil[1:(74+i)]
  datosB<-corrientes_fil[(75+i):99]
  mediaA<-mean(datosA)
  mediaB<-mean(datosB)
  cA<-sd(datosA)*1.96*1/sqrt(length(datosA)-1)
  cB<-sd(datosB)*1.96*1/sqrt(length(datosB)-1)
  #tcritico 1,96
  Y[i]<-(mediaB-mediaA)/(cB+cA)
  rm(datosB)
}
Y

#no se observa un salto climatico en ningun momento entre esos años
#si la pregunta fuera sobre el mismo periodo que en gral pico, que seria 1959:1999
#no veo bien aplicar el test en este caso ya que solo tengo 42 datos y no va ser muy representativo
#calular medias con 20 datos, en el mejor de los casos
#aun asi podes probar que da:
plot.ts(corrientes_fil[59:99])

Y<-vector()
for(i in 1:5){
  datosA<-corrientes_fil[1:78+i]
  datosB<-corrientes_fil[(79+i):99]
  mediaA<-mean(datosA)
  mediaB<-mean(datosB)
  cA<-sd(datosA)*1.96*1/sqrt(length(datosA)-1)
  cB<-sd(datosB)*1.96*1/sqrt(length(datosB)-1)
  #tcritico 1,96
  Y[i]<-abs(mediaB-mediaA)/(cB+cA)
  rm(datosB)
}
Y

#tampoco presenta saltos

####EJ 4####
rm(list=ls())

datos<-as.data.frame(read_xlsx("pp_corrientes.xlsx"))

plot(datos[,1],datos[,2],type="l") #se puede ver una tendencia diferente en cada una
#esto puede formar parte de una onda mas grande y al no tener datos entre medio de los dos
#periodos  y tampoco antes de 1914 no se puede determinar. Analizar tendencia en este caso 
#no seria significativo, por lo mencionado antes y por los pocos datos que que se tienen de cada periodo
#NO SE HAY SUFICIENTES DATOS PARA AFIRMAR UN CAMBIO EN LA CIRCULACION
