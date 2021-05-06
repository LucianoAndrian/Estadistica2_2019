#Auto-correlacion y correlacion cruzada

rm(list=ls())
#graphics.off()
#cat("\014") 
#print('setear directorio')
#setwd("/media/usuario/datos/Nati/Docencia/Metodos_estadisticos/2019/Clases_Practicas/Clase_Auto_corr/") 
datos<-read.table('cavecreek.txt')
head(datos)
plot.ts(datos)
#NECESITA UNA SERIE, UNA SOLA
#NO LE IMPORTA EL PASO TEMPORAL, ESO HAY QUE CONOCERLO DE ANTEMANO
#
#definir k (lag max) hasta el 30% de los datos
#Autocorrelacion
resul<- acf(datos,type="correlation",plot=T,lag.max=3*length(datos[,1])/10)

str(resul)

########
#p>alfa no puedo rechazar h0
#p>alfa rechazo h0
#######
########LO MISMO PERO A MANO PARA MODIFICAR EL NIVEL DE SIGNIFICANCIA#################
valores<-resul$acf
N<-length(datos[,1])

# asi hace la acf las bandas de significancia qnorm((1 + 0.95)/2)/sqrt(x$n.used)
#pensado para dos colas con 95% de significancia


rkpos<-rep(NA,length(valores))
rkneg<-rep(NA,length(valores))

ci=0.95 #defino el nivel de confianza entre 0 y 1
for(k in 0:(length(valores)-1)){
  
    rkpos[k+1]<- (-1+qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k)
 
    rkneg[k+1]<- (-1-qnorm((1 + ci)/2)*sqrt(N-k-1))/(N-k) 

}



plot(valores,ylim=c(-1,1),type="b")
lines(rkpos,lty=2,col='red')
lines(rkneg,lty=2,col='red')


########  AUTOCORRELACION CRUZADA#######
####### cross correlacion
###datos simulados a modo de ejemplo de caudal y ninio 3.4 para estudiar su relacion
datoscross<-read.table('pruebacross.txt',head=FALSE)
head(datoscross)
colnames(datoscross)<-c('caudal','ninio3.4')
#DETERMINAR LA VARIABLE DEPENDIENTE X Y LA INDEPENDIENTE
plot.ts(datoscross$caudal)
plot.ts(datoscross$ninio3.4)
#EN ESTE CASO LOS NEGATIVOS NO TIENEN SENTIDO FISICO
#LOS NEGATIVOS SERIE LA CORRELACION DEL CAUDAL CONTRA LA DEL NINIO3.4
cc1<-ccf(datoscross[,2],datoscross[,1],plot=TRUE) 
#LCERO R DE PERSON

