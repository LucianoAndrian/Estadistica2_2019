#yamamoto
rm(list=ls())
datos=as.data.frame(read_xls("datos_clase.xls",sheet = 1))
#salto en el 80 fila 28
Y<-vector()
for(i in 1:20){
datosA<-datos[1:which(datos[,1]==1970+i),]
datosB<-datos[which(datos[,1]==1971+i):length(datos[,1]),]
mediaA<-mean(datosA[,2])
mediaB<-mean(datosB[,2])
cA<-sd(datosA[,2])*1.96*1/sqrt(length(datosA[,1])-1)
cB<-sd(datosB[,2])*1.96*1/sqrt(length(datosB[,1])-1)
#tcritico 1,96
Y[i]<-(mediaA-mediaB)/(cA+cB)
}
plot(datos[which(datos[,1]==1970):which(datos[,1]==1989),1],type="l",Y,xlab="Anios")
########################################################################################
#marona
rm(list=ls())
#scrip de marona

####################################
#### Metodos estadisticos 2019 #####
####################################

#### Test de Maronna-Yohai 

###-----------------------------------------------------------------------------
# La serie de referencia es X
# La serie en la que se quiere identificar la ocurrencia de una cambio es Y
###-----------------------------------------------------------------------------

cat("\014") # Limpia consola 
rm(list=ls())
graphics.off()
# Cargar libreria para graficar
library(ggplot2)

print('abrir el archivo')
VAR <- read.table('OLR2.txt', head=TRUE)


###veo cual serie es la de ref y la del salto
plot(VAR[,1],VAR[,3],type="l",col="red",xlab="anios")
lines(VAR[,1],VAR[,2])


################################################
print('Calcula dimensiones')
dimension<-dim(VAR)
N<-dimension[1]
L<-dimension[2]

PX <- VAR[,2] #de referencia
PY <-VAR[,3] #salto

print('Estandariza las series') 
X <- (PX - mean(PX))/sd(PX)
Y <- (PY - mean(PY))/sd(PY)  

# Defino vectores que voy a crear 
XX <- vector()
YY <- vector()
XAC <- vector()
YAC <- vector()

print('Genera la serie de valores acumulados') 

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

print('Calcula los valores medios de ambas variables') 

XXM <- XX[N]
YYM <- YY[N]

print('Calcula varianza y covarianza') 

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

###############################################################
######################## ATENCION #############################
###############################################################

######################################
#### Para graficar Datos ANUALES #####
######################################
print('Grafico')

plot(VAR[1:N-1,1] ,T, type="o", col="blue", xlab="Año", ylab="T")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

print('Busca el año donde T es maximo') 

max <- which.max(T)
maximo <- VAR[max,1]
#es mas alto que todos los de la tabla, y da donde es el lugar donde se da el salto

# Si quiero graficar una linea roja donde sea maximo T
abline(v =maximo, col="red",untf = FALSE)

abline(h = 9.3, col= "black")
#quiero testear T
T[max]
max(T)

#segundo maximo
max2<-max(T[50:81])

abline(v = VAR[which(T==max2),1],col="red")

maximo2<-VAR[which(T==max2),1]             

# ################################################# #### Para graficar Datos DIARIO o MENSUALES #####
# #################################################
# PARA LA PRACTICA!!! EJ 2
#
# ### En el caso de trabajar con datos DIARIOS o MENSUALES
# ### Hay que generar un vector "Fecha" 
# ### La siguiente secuencia lo hace. Para esto hay que cambiar:
# # en el primer elemento poner fecha de inicio del periodo AÑO/MES/DIA
# # en el segundo elemento poner fecha de fin del periodo AÑO/MES/DIA
# # En el tercer elemento si son valores diarios= "days" o mensuales "mon"
# 
# Fecha <- seq(as.Date("1987/1/1"), as.Date("1987/5/31"), "days")
# 
# print('Busca el año donde T es maximo') 
# max <- which.max(T)
# maximo <- VAR[max,1]
# 
# # Se define data para poder graficar
# data=data.frame(Fecha[1:N-1], T)
# 
# plot <- ggplot(data, aes(x=Fecha[1:N-1], y=T)) +
#         geom_line() + 
#         xlab("") +
# # "date_breaks" es para ver cada cuanto quieren graficar, eso se puede elegir
# # "date_labels" permite elegir de que manera escribir los x_labels  
#         scale_x_date(date_breaks = "10 days", date_labels = "%d/%b") + 
# # Esta linea permite rotar los x_labels  
#         theme(axis.text.x=element_text(angle=90, hjust=1))+
# # Esta linea permite graficar la linea vertical para ver el año del salto. 
#         geom_vline(xintercept = as.numeric(Fecha[max]),col="red")
# plot




