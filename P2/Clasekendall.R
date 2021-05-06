###########################################
## M???todos Estad???sticos 2018
## Mar???a Paula Llano
## DCAO
###########################################

# ***** Test de Mann Kendall  ***************************

### SIEMPRE!!! primero cargar paquetes
#install.packages("kendall")
library(Kendall)
#require(Kendall)

#WORKDIR <- "C:/Users/PC/Documents/MATERIAS/DOCENCIA/Met.Estadisticos_2018/"
#SOURCRES <- "C:/Users/PC/Documents/MATERIAS/DOCENCIA/Met.Estadisticos_2018/"
#DATA <- "C:/Users/PC/Documents/MATERIAS/DOCENCIA/Met.Estadisticos_2018/"
#OUTPUTS <- "C:/Users/PC/Documents/MATERIAS/DOCENCIA/Met.Estadisticos_2018/"

#setwd(WORKDIR)
##########################

#datos <-read.table(paste(DATA,"clase_mann_2.txt", sep=""), header=FALSE, sep="",fill=TRUE)
datos<-read.table("clase_mann_2.txt", sep= "",header = F)
tiempo<-datos[,1]
valor<-datos[,2]

plot(valor,type= "o",col="red")

resul<- MannKendall(valor)
summary(resul) # tabla resumen 

N<- length(valor)
desvio<- sqrt((4*N+10)/(9*N*(N-1))) #calculo el desvio de Tau seg???n Nota T???cnica 79

t<-resul$tau #extraigo el valor de Tau# los chinos usan otro metodo... 
#solo sacamos el tau.

tau_tab<-(t/desvio) # este es el valor que tengo que comparar con la Tabla Normal 1.96, 5%




###### Rank Kendall, para correlacionar 2 conjuntos de datos, pr???ctica 2
rank<-Kendall(tiempo,valor)  #da lo mismo
summary(rank)
