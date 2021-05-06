#PRACTICA1-REPASO Y CORRELACION
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P1/")
####EJ1####
Rm(list=ls())
#como es de manera practica. calcular numeros.
#test de varianza y media.
#n
#nivel de significancia
#hacer a mano

#t=rsqrt((n-2)/(q-r^2))
#r=t/(sqrt(n-2-t^2))
posadas<-read.table("T8717800.txt",skip=1,na.strings = "-999") #cambie los -9999 por -999, no estaban separados del otro numero

#falta 1980!!!!!, lo agrego como dato faltante -999
posadas[,1]<-1902:1994
colnames(posadas)<-month.abb


junin<-read.table("T8754800.txt",skip=1,na.strings = "-999")
junin[,1]<-1958:1994 
colnames(junin)<-month.abb               

#antes de testear las medias, deberia testear las varianzas pero en ese caso hay que hacer
#hacer por mes? ya que (de clima) "promediar todo el año para la varianza no es representativo"

#media para cada mes, dentro del rango en comun 1958-1994
media_posadas<-apply(posadas[which(posadas[,1]==1958):which(posadas[,1]==1994),2:13],2,mean, na.rm= T )
media_junin<-apply(junin[,2:13],2,mean, na.rm= T)

fisher.test(media_posadas,media_junin)
var.test(media_posadas,media_junin)
t.test(media_posadas,media_junin) #ya la media da que no pertenece a la misma poblacion.


####EJ2####
#. que significa que no sea representativo??
#dos series distinto periodo--->tomar periodo en comun.
#ver como es la onda anual, hacer graficos, promedio de año.
#si se testea todo junto, con perason da significativo. ya que tiene el ciclo anual muy marcado.
#calcular onda anual media y restarla a todo. eneros-enero media.--->anomalias.
#se puede ver por trimestre y no restar el ciclo anual.


####EJ3####
rm(list=ls())

library(ggplot2)

datos<-read.table("datos_Ej3.txt")
plot(datos$x,datos$y,type = "p")
#cor(datos$x,datos$y,method = "pearson")
#ajuste<-lm(datos$x~datos$y)
#abline(ajuste) #no funca
pearson<-cor.test(datos$x,datos$y,method = "pearson",conf.level = 0.95)
#ya suponemos la relacion lineal con pearson. y suponiendo que las muestras vienen de poblaciones normales
kendall<-cor.test(datos$x,datos$y,method = "kendall",conf.level = 0.95)
