#Practica4-CORRELACION CRUZADA
######\m/######
setwd("C:/Users/Alumno/Desktop/Estadistica2/P4")
####EJ1####
rm(list=ls())
graphics.off()

ssha<-read.table("autocorr.txt",header = T)

#a
png("EJ1_A.png",width = 800,height = 600,units = "px")
plot(ts(ssha))
dev.off()

#b
png("EJ1_B.png",width = 800,height = 600,units = "px")
acf(ssha,type="correlation",plot=T,lag.max=0.3*length(ssha[,1]))
dev.off()

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
colnames(tsm30)<-c("anio","abril","mayo","junio")

tsm36<-read.table("sst36.txt",header = T)
colnames(tsm36)<-c("anio","abril","mayo","junio")

posadas<-read.table("posadas.txt",header = T)  #cambie , por . en el .txt
colnames(posadas)<-c("anio","abril","mayo","junio")

mdp<-read.table("mdq.txt",header = T)  
colnames(mdp)<-c("anio","abril","mayo","junio")

#tsm30 vs posadas
#correlacion instantanea
tsm30_posadas<-vector()
for(i in 2:4){
  tsm30_posadas[i]<-cor(tsm30[,i],posadas[,i])
}

#CORR. LAGUEADA A MANO. TSM DE UN MES CONTRA EL INDICE DE OTRO MES#
#cruzada("a mano")
#t tabla df 23= 2,07
correlacion_testeada_posadas<-matrix(NA,ncol=4,nrow=4)
colnames(correlacion_testeada_posadas)<-c("Posadas","abril","mayo","junio")
rownames(correlacion_testeada_posadas)<-c("Tsm30","abril","mayo","junio")

for(i in 2:4){
  for(j in 2:4){
  result<-cor.test(tsm30[,i],posadas[,j],method ="pearson")
  if(abs(result$statistic)>2.07){
    correlacion_testeada_posadas[i,j]<-cor(tsm30[,i],posadas[,j],method ="pearson")
  } else {next}
  }
}  
#

correlacion_testeadas_mdp<-matrix(NA,ncol=4,nrow=4)
colnames(correlacion_testeadas_mdp)<-c("mdp","abril","mayo","junio")
rownames(correlacion_testeadas_mdp)<-c("Tsm30","abril","mayo","junio")

for(i in 2:4){
  for(j in 2:4){
    result<-cor.test(tsm30[,i],mdp[,j],method ="pearson")
    if(abs(result$statistic)>2.07){
      correlacion_testeadas_mdp[i,j]<-cor(tsm30[,i],mdp[,j],method ="pearson")
    } else {next}
  }
}  
