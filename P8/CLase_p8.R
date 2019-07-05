#Clase regresion tridimencional
setwd("C:/Users/Alumno/Desktop/Estadistica2/P8/")

#predecir y en funcion de x1 y x2
#lm(y~ x1+x2, data = DATOS )
#lm(y~.,data= DATOs) en funcion de todas
#lm(y~1,data = DATS) solo tiene en cuenta la ordenada al orifge
#summary(lm..)...
#ajuste$fitted.values

###EJ1###
rm(list=ls())
graphics.off()
datos<-read.table("datossoja.txt")
datos2<-datos[,1:3]

#luego de graficar determinar cuales pueden seguir un modelo lineal.. q es el que conocemos
cor(datos)
 
pp<-cor.test(datos$rendimiento,datos$pp)
temp<-cor.test(datos$rendimiento,datos$temp)
fertp<-cor.test(datos$rendimiento,datos$fertP)
fertz<-cor.test(datos$rendimiento,datos$fertZ)

#correlacion con la pp y la temp. significatica
#se pueden usar esas dos variables para hacer un modelo lineal
#ya que son las que mas importan

###
library(corrplot)

###

plot(datos$rendimiento,datos$pp)
plot(datos$rendimiento,datos$temp)
plot(datos$rendimiento,datos$fertp)
plot(datos)

ajuste<-lm(rendimiento ~ .,data=datos2)
#ajuste da la pendiente estmada pero eso no nos dice nada
#summary 
#se fija como aporta cada variable al modelo
#p value si son significativos ***..
#ajusted r-squared mide con la cantidad de varibles que se ponen en el modelo.
#cuando hay muchas variables combiene mirar este
#F estadistico y grados de libertad y pvalor al test
#tiene una varianza explicada del 74% y es sifnificativo por... test de fisher, h0 h1, significacion
#rechazo h0 entonces existe al menos un i que es != de 0.
#grados de libertad n-1-2 (i=2)
plot(datos$rendimiento,lwd=4)
points(ajuste$fitted.values,col="red",lwd=4)
