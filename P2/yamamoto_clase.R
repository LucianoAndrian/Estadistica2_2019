#yamamoto
rm(list=ls())
library(readxl)
setwd("C:/Users/Alumno/Desktop/Estadistica2/P2")
datos=as.data.frame(read_xls("datos_clase.xls",sheet = 1))
#datos, dos columnas, anios y algo...orl?? 
#salto en el 80 fila 28
#separo la serie en 2, en donde sospecho que esta el salto.
#desde 1971:1990
Y<-vector()
for(i in 1:20){
  datosA<-datos[1:which(datos[,1]==1970+i),]
  datosB<-datos[which(datos[,1]==1971+i):length(datos[,1]),]
  mediaA<-mean(datosA[,2])
  mediaB<-mean(datosB[,2])
  cA<-sd(datosA[,2])*1.96*1/sqrt(length(datosA[,1])-1)
  cB<-sd(datosB[,2])*1.96*1/sqrt(length(datosB[,1])-1)
  #tcritico 1,96
  Y[i]<-(mediaA-mediaB)/(cA+cB)
}
Y
max(Y)##salto
plot(datos[which(datos[,1]==1970):which(datos[,1]==1989),1],type="l",Y,xlab="Anios")
