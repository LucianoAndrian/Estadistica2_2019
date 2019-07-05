rm(list=ls())
#como es de manera "practica", a ojo??
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

#testeo t-st
t.test(media_posadas,media_junin)
