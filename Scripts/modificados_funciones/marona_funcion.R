####################################
#### Metodos estadisticos 2019 #####
####      SCRIP DE CLASE       #####
## MODIFICADO POR LUCIANO ANDRIAN ##
####################################
#### Test de Maronna-Yohai 

#### SCRIP DE CLASE CONVERTIRDO EN FUNCION, CALCULA EL ESTADISTICO, TESTEA Y GRAFICA
### RESULTADOS DEVUELTOS EN FORMA DE LISTA
###-----------------------------------------------------------------------------
# La serie de referencia es X
# La serie en la que se quiere identificar la ocurrencia de una cambio es Y
###-----------------------------------------------------------------------------

marona<-function(x){
y<-readline("Formato de fecha, anual: A o diario D: " )
VAR <- x
library(ggplot2)

#('Calcula dimensiones')
dimension<-dim(VAR)
N<-dimension[1]
L<-dimension[2]

PX <- VAR[,2]
PY <-VAR[,3]

#'Estandariza las series' 
X <- (PX - mean(PX))/sd(PX)
Y <- (PY - mean(PY))/sd(PY)  

# Defino vectores que voy a crear 
XX <- vector()
YY <- vector()
XAC <- vector()
YAC <- vector()

#'Genera la serie de valores acumulados'

XX[1] <- X[1]
YY[1] <- Y[1]
XAC[1] <- X[1]
YAC[1] <- Y[1]

for (J in 2:N) {
  YAC[J] <- YAC[J-1]+Y[J];
  XAC[J] <- XAC[J-1]+X[J];
  XX[J] <- XAC[J]/J;
  YY[J] <- YAC[J]/J;  
}

#'Calcula los valores medios de ambas variables') 

XXM <- XX[N]
YYM <- YY[N]

#'Calcula varianza y covarianza') 

SX <- var(X)*(N-1)
SY <- var(Y)*(N-1)

SXYA <- cov(X,Y)*(N-1)
SXY <- SXYA

# Se calculan los parametros que permiten identificar el año
# porterior al cambio en la media (T(i)) y la magnitud del cambio (D(i))


F <- vector()
D <- vector()
T <- vector()


for(L in 1:N-1) {
  F[L] <- SX-(((XX[L]-XXM)^2)*N*L)/(N-L)
  D[L] <- ((SX*(YYM-YY[L])- SXY*(XXM-XX[L]))*N)/((N-L)*F[L])
  T[L] <- (L*((N-L)*D[L]^2)*F[L])/(SX*SY-SXY^2)
}


max <- which.max(T)

maximo <- VAR[max,1]

resultado<-data.frame(F=F,D=D,T=T)

#testeo
ndatos<-c(10,15,20,30,40,70,100)
Ttabla<-c(6.8,7.4,7.8,8.2,8.7,9.3,9.3)

tabla<-data.frame(ndatos,Ttabla)

if(tabla[2,1]>N & N>tabla[1,1]){
  if(max(T)>tabla[1,2]){
    m<-"Rechazo Hipotesis nula"
  } else {next}
} else if(tabla[3,1]>N & N>tabla[2,1]){
  if(max(T)>tabla[2,2]){
    m<-m<-"Rechazo Hipotesis nula"
  } else {next}
} else if(tabla[4,1]>N & N>tabla[3,1]){
  if(max(T)>tabla[3,2]){
    m<-"Rechazo Hipotesis nula"
  } else {next}
} else if(tabla[5,1]>N & N>tabla[4,1]){
  if(max(T)>tabla[4,2]){
    m<-"Rechazo Hipotesis nula"
  } else {next}
} else if(tabla[6,1]>N & N>tabla[5,1]){
  if(max(T)>tabla[5,2]){
    m<-"Rechazo Hipotesis nula"
  } else {next}
} else if(tabla[7,1] >N & N>tabla[6,1]){
  if(max(T)>tabla[6,2]){
    m<-"Rechazo Hipotesis nula"
  } else {next}
} else if(N>tabla[7,1]){
  if(max(T)>tabla[7,2]){
    m<-"Rechazo Hipotesis nula"
  } else {next} 
} else{ 
  m<-"no puedo rechazar hipotesis nula"}

if(y=="A"){
  plot(VAR[1:N-1,1] ,T, type="o", col="blue", xlab="Año", ylab="T")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
  abline(v =maximo, col="red",untf = FALSE)
  return(list(resultado,Maximo=maximo,Posicion=max,Valor=max(T),m,plot))
} else if (y=="D"){
  inicio<-readline("ingrese fecha de inicio del periodo en formato AA/DD/MM:")
  final<-readline("ingrese fecha final del periodo en formato AA/DD/MM:")
  fechas<-seq(as.Date(inicio), as.Date(final), "days")
  
  datos3=data.frame(fechas[1:N-1], T)
  breaks<-readline("dates_breaks: ")
  plot <- ggplot(datos3, aes(x=fechas[1:N-1], y=T)) +
    geom_line()  +
    xlab("") + 
    theme_bw() +
    scale_x_date(date_breaks = breaks, date_labels = "%d/%b") + 
    # Esta linea permite rotar los x_labels  
    theme(axis.text.x=element_text(angle=90, hjust=1))+
    # Esta linea permite graficar la linea vertical para ver el año del salto. 
    geom_vline(xintercept = as.numeric(fechas[max]),col="red")
  return(list(resultado,Maximo=fechas[maximo],Posicion=max,Valor=max(T),m,plot))
} else {next}

#return(list(resultado,Maximo=fechas[maximo],Posicion=max,Valor=max(T),m,plot))

}