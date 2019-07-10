###########################################
## Métodos Estadísticos 2016
## María Paula Llano
## DCAO
###########################################
######funcion#####
#pregunta si guarda el grafico
######################################### 

funcion_armonico<-function(datos){
  readline("Recuerde que debe ingresar solo una serie de datos")
  valor<-datos
  N <- length(valor)   
  K<-N/2  #cantidad de armonicos
  P<-N	  
  ###       Calcula el promedio
  PROM <-mean(valor)   #primer termino de la ecuacion
  ## calcula la varianza total
  VAR<-var(valor)
  VAR1<-VAR^0.5
  ## calculo los coeficientes de Fourier   
  CA<-c(1:K)
  CA[]<-0
  PI<-3.1415926
  NARM<-c(1:K) #numero de armonico
  NARM[]<-0
  A<-c(1:K)
  A[]<-0
  B<-c(1:K)
  B[]<-0
  AM<-c(1:K)
  AM[]<-0
  C<-c(1:K)
  C[]<-0
  CA<-c(1:K)
  CA[]<-0
  
  #######comienza
  for (I in 1:(K-1)){
    NARM[I]<-I  
    SUM<-0
    SAM<-0
    
   for (J in 1:N){
      
      SUM<-SUM+valor[J]*sin(I*2*PI*J/P)
      SAM<-SAM+valor[J]*cos(I*2*PI*J/P)
    }
    
    A[I]<-2*SUM/N
    B[I]<-2*SAM/N
    AM[I]<-(A[I]^2+B[I]^2)^0.5
    C[I]<-(((AM[I]^2)/2)/VAR)*100
    CA[I]<-sum(C)
    
    
  }
  
  
  #  calculo del ultimo armonico
  SUM<-0	
  for (J in 1:N){
    SUM<-SUM+(valor[J]*cos(K*2*PI*J/P))
  }
  
  B[K]<-SUM/N
  AM[K]<-B[K]
  C[K]<-((AM[K]^2)/VAR)*100
  CA[K]<- CA[K-1]+C[K]
  A[K]<-0
  NARM[K]<-K
  
  FIN<-matrix(0,K,5, dimnames = list(NARM,c('A', 'B', 'AMPL', 'VAR', 'VAR acu'))) # esta matriz FIN es la que tiene toda la informaci?n
  FIN[,1]<-A
  FIN[,2]<-B
  FIN[,3]<-AM
  FIN[,4]<-round(C,2)
  FIN[,5]<-CA
  print(FIN)
  
  x<-readline("Desea guardar y graficar el periodograma?(si/no): ")
  if(x=="si"){
  nombre<-readline("Nombre con el cual desea guardar el grafico: ")
  png(filename = paste(nombre,".png",sep=""),width = 800,height = 600,units = "px")
  barplot(FIN[,"VAR"],ylim = c(0,100),ylab="Varianza %",xlab = "Armonico",col="green")
  dev.off()
  barplot(FIN[,"VAR"],ylim = c(0,100),ylab="Varianza %",xlab = "Armonico",col="green")
  } else {
    barplot(FIN[,"VAR"],ylim = c(0,100),ylab="Varianza %",xlab = "Armonico",col="green")
  }
  
return(FIN)
  
}