rm(list=ls())
datos<-read.table("Datos_temperatura_1.txt",header = T)
cor(datos$A,datos$B) #method, "pearson" o "kendal"
cor.test(datos$A,datos$B)
#ajuste, item b
ajuste<-lm(datos$B~datos$A)
summary(ajuste)
#F enorme, rechazo h0
plot(datos$A,datos$B)
abline(ajuste) #agrega la linea pero me da cualquier cosa!!! SOLUCIONADO, ESTABA AL REVEZ EN LM

#agrego datos
datos2<-read.table("Datos_temperatura_2.txt",header = T)
#junto con los datos anteriores
datosT<-rbind(datos,datos2)

plot(datosT$A,datosT$B) #claraente no es lineal

cor(datosT$A,datosT$B,method = "kendall")

cor.test(datosT$A,datosT$B,method = "kendall")
