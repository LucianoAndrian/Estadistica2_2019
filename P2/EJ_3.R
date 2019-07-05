#ej3
rm(list=ls())
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
#USO KENDALL O REGRESION??
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

tau_tab3<-vector()
for(i in 2:4){
  mk[[i]]<- MannKendall(datos[50:100,i])
  N<- length(datos[50:100,i])
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
