###########################################
## Métodos Estadísticos 2016
## María Paula Llano
## DCAO
###########################################

# ***** Filtro de la onda del análisis armónico


#WORKDIR <- "C:/Users/melu/Dropbox/Métodos estadísticos/Practica 9/"
#SOURCRES <- "C:/Users/PC/Documents/MATERIAS/DOCENCIA/Met.Estadisticos_2016/Practicas/P9/"
#DATA <- "C:/Users/melu/Dropbox/Métodos estadísticos/Practica 9/"
#OUTPUTS <- "C:/Users/melu/Dropbox/Métodos estadísticos/Practica 9/"

#setwd(WORKDIR)

####################
graphics.off()
datos <-read.table("datos_clase_2.txt", header=FALSE, sep="",fill=TRUE)


valor<-datos[,1]


############# Igual que en MatLab

N <- length(valor)
K<-N/2
P<-N	

########CAMBIAR EL N?MERO DE ARMONICO
M<- 9 #ESTE VALOR HAY QUE CAMBIAR


###       Calcula el promedio

PROM <-mean(valor)   

PI<-3.1415926

########### comienza!
##busco los coef A y B del arm?nico a filtrar

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

png(filename = "filonda.png",width = 800,height = 600,units = "px")
par(mfrow=c(3,1))
plot(ts(valor))
plot(ts(XS))
plot(ts(FIL))
dev.off()
#guardar por que la figura es muy grande y no la grafica