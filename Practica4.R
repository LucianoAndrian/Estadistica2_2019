#Practica4-CORRELACION CRUZADA
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P4")
#####
####EJ1####
rm(list=ls())
graphics.off()

ssha<-read.table("autocorr.txt",header = T)

#a
png("EJ1_A.png",width = 800,height = 600,units = "px")
plot.ts(ssha)
dev.off()

#b
png("EJ1_B.png",width = 800,height = 600,units = "px")
acf(ssha,type="correlation",plot=T,lag.max=0.3*length(ssha[,1]))
dev.off()

resul<-acf(ssha,type="correlation",plot=T,lag.max=0.3*length(ssha[,1]))

valores<-resul$acf
N<-length(ssha[,1])

# asi hace la acf las bandas de significancia qnorm((1 + 0.95)/2)/sqrt(x$n.used)
#pensado para dos colas con 95% de significancia

rkpos<-rep(NA,length(valores))
rkneg<-rep(NA,length(valores))

ci=0.95 #defino el nivel de confianza entre 0 y 1
for(k in 0:(length(valores)-1)){
  
  rkpos[k+1]<- (-1+qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k)  #test de anderson para las bandas de significacnia!!! jaajaja
  
  rkneg[k+1]<- (-1-qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k) 
  
}

plot(valores,ylim=c(-1,1),type="b")
lines(rkpos,lty=2,col='red')
lines(rkneg,lty=2,col='red')

barplot(t(as.matrix(valores)),ylim=c(-1,1),col="green4",xlim=c(1,k+1),lwd=2,names.arg = c(1:length(valores)))
lines(rkpos,lty=2,col='red',lwd=1.8)
lines(rkneg,lty=2,col='red',lwd=1.8)

#c
#test de anderson
#r=-1+-1.645sqrt(N-2)/n-l
#h0=r=0 
#h1!=h0
#si rechaza h0 la serie tiene persistencia
#Hasta el lag 5 seobserva persistencia
#Entre el lag 15 y 25 con un maximo (minimo) en 20 se vuelve a observar
#significancia
#indica un la presencia de un ciclo en la serie.
####EJ2####
rm(list=ls())
tsm30<-read.table("sst30.txt",header = T)
colnames(tsm30)<-c("anio","tsm30_abril","tsm30_mayo","tsm30_junio")

tsm36<-read.table("sst36.txt",header = T)
colnames(tsm36)<-c("anio","tms36_abril","tsm36_mayo","tsm36_junio")

posadas<-read.table("posadas.txt",header = T)  #cambie , por . en el .txt
colnames(posadas)<-c("anio","indice_abril","indice_mayo","indice_junio")

mdp<-read.table("mdq.txt",header = T)  
colnames(mdp)<-c("anio","indice_abril","indice_mayo","indice_junio")

#CORR. LAGUEADA A MANO. TSM DE UN MES CONTRA EL INDICE DE OTRO MES#
#cruzada("a mano")
#t tabla df 23= 2,07
#qt(0.975,df=23) 95% de conf a dos colas

tsm30_posadas<-cbind(tsm30,posadas[,2:4])
tsm36_posadas<-cbind(tsm36,posadas[,2:4])
tsm30_mdp<-cbind(tsm30,mdp[,2:4])
tsm36_mdp<-cbind(tsm36,mdp[,2:4])
datos<-list(tsm30_posadas,tsm36_posadas,tsm30_mdp,tsm36_posadas)
Rcritico<-(2.07/(sqrt(2.07^2+25-2)))
library(corrplot)

graficos<-list()

for(i in 1:4){
  correlaciones<-cor(datos[[i]])
  correlaciones<-correlaciones[5:7,2:4]
  correlaciones[which(abs(correlaciones)<Rcritico)]<-0
  correlaciones[upper.tri(t(correlaciones))]<-0 #saco los que no tienen sentido fisico
  graficos[[i]]<-corrplot(t(correlaciones))
  
}


#png(filename = "correlaciones.png",width = 400,height = 300,units = "px")
par(mfcol=c(1,2))
corrplot(graficos[[1]])
corrplot(graficos[[2]])
corrplot(graficos[[3]])
corrplot(graficos[[4]])
#dev.off()
