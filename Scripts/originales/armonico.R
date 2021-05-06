###########################################
## Métodos Estadísticos 2016
## María Paula Llano
## DCAO
###########################################

# ***** Análisis Armónico  ***************************
# Calculo los coeficientes del análisis de Fourier

## la cantidad de datos debe ser par


#WORKDIR <- "C:/Users/melu/Dropbox/Métodos estadísticos/Practica 9/"
#SOURCRES <- "C:/Users/PC/Documents/MATERIAS/DOCENCIA/Met.Estadisticos_2016/Practicas/P9/"
#DATA <- "C:/Users/melu/Dropbox/Métodos estadísticos/Practica 9/"
#OUTPUTS <- "C:/Users/melu/Dropbox/Métodos estadísticos/Practica 9/"

#setwd(WORKDIR)

####################
#datos temperatura mensual
datos <-read.table("datos_clase_2.txt", header=FALSE, sep="",fill=TRUE)

head(datos)
valor<-datos[,1]


############# Igual que en MatLab

N <- length(valor)   
K<-N/2  #cantidad de armonicos
P<-N	  #N

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


########### comienza!
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

#  calculo del ?ltimo arm?nico

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

plot(FIN[,'VAR'],col="red")

#maxima varianza en el armonico 10
#datos = 120 (N)
#periodo del armonico 10---> 12 onda anual P=(120*1)/10

#dato clases 2
#la onda no esta perfecta representada con un armonico como antes
#la representan con dos armonicos, ya que no tengo 120 datos, no esmultipli de 2
#P=11.1
#P=12.5