###########################################
## Métodos Estadísticos 2016
## María Paula Llano
## DCAO
########DESCRIPCION###################################

# ***** Filtro de la onda del análisis armónico

######funcion######
#pide numero de armonico a filtrar

#pregunta antes de guardar el grafico

####################

filonda<-function(valor){
 
  N <- length(valor)
  K<-N/2
  P<-N	
  
  M<-as.numeric(readline("Numero del armonico que desa filtrar: "))

  #Calcula el promedio
  
  PROM <-mean(valor)   
  PI<-3.1415926

  #coef A y B del armonico a filtrar
  
  SUM<-0
  SAM<-0
  
  for (J in 1:N){
    
    SUM<-SUM+valor[J]*sin(M*2*PI*J/P)
    SAM<-SAM+valor[J]*cos(M*2*PI*J/P)
    
  }
  A<-2*SUM/N
  B<-2*SAM/N
  
  XS<-c(1:N)
  XS[]<-0
  
  FIL<-c(1:N)
  FIL[]<-0
  
  
  for (J in 1:N) {
    XS[J]<-A*sin(2*PI*M*J/P)+B*cos(2*PI*M*J/P)
  }
  
  for (I in 1:N) {
    FIL[I]<- valor[I]-XS[I]
  }
  
  grafico<-readline("Desea guardar el grafico?(si/no): ")
  if(grafico=="si"){
  nombre<-readline("Nombre del grafico a guardar: ")
  png(filename = paste(nombre,".png",sep=""),width = 800,height = 600,units = "px")
  par(mfrow=c(3,1))
  plot(ts(valor))
  plot(ts(XS))
  plot(ts(FIL))
  dev.off()
  }
  return(FIL)
}
  

