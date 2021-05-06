#SCRIPT CALCULO ESPECTRO 2018

#CALCULA ESPECTOS DE SERIES TEMPORALES DISCRETAS DE ACUERDO AL M?TODO
# DE BLACKMAN-TUKEY (LIBRO DE OTNES, PAG: 270-272), CON EL CORRESPONDIENTE
#CONTINUO NULO Y BANDAS DE SIGNIFICANCIA AL 5% O AL 10% (CLIMATIC CHANGE,
# NOTA T?CNICA N? 79, PAG: 36-42).#
#LA MATRIZ DE ENTRADA DE DATOS DEBE PRESENTAR LAS SERIES A LAS QUE SE LES
#VA A CALCULAR EL ESPECTRO EN COLUMNA. TODAS LAS SERIES DEBEN TENER EL 
# MISMO N?MERO DE REGISTROS (LA MISMA LONGITUD).
# AL COMENZAR EL PROGRAMA REMOVER? LAS MEDIAS DE CADA SERIE Y APLICAR? UNA
# VENTANA COSENO EN LOS EXTREMOS. LUEGO APLICAR? UNA VENTANA HANN, HAMMING
# O PARZEN A ELECCI?N (SE ELIGE DESDE PANTALLA).
# LUEGO GRAFICAR? LOS ESPECTROS DE CADA SERIE Y GUARDAR? LOS DATOS EN UNA

# MATRIZ CON EL NOMBRE "Resul" CUYAS COLUMNAS SON:
# col1: frecuencias
# col2: espectro de la serie 1
# col3: limite inferior del intervalo de confianza (serie 1)
# col4: continuo nulo (serie 1)
# col5: limite superior del intervalo de confianza (serie 1)
# col6: espectro de la serie 2
# col7: limite inferior del intervalo de confianza (serie 2)
# col8: continuo nulo (serie 2)
# col9: limite superior del intervalo de confianza (serie 2)
# Y ASI SIGUIENDO CON TODAS LAS SERIES PRESENTES EN LA MATRIZ DE ENTRADA.
# LA MATRIZ "Resul" PUEDE LUEGO EXPORTARSE PARA GRAFICAR LOS ESPECTROS EN
# ORIGIN Y ALLI TRABAJARLOS.
#--------------------------------------------------------------------------
#cat("\014") #Limpiar consola si lo quiero hacer sin
#funci?n uso CTL+L

#print('setear directorio')
#setwd("/home/melu/Dropbox/Métodos estadísticos/Practica10") 

#Entrada de datos

#Matriz=input('Matriz de entrada:  ')
#Matriz<-read.table('datos_temperatura.txt',head=FALSE)


Matriz<-datos

png('datostemperatura.png')
plot(Matriz[,1],type='l',ylab='temperatura',xlab='tiempo') #adaptar segun los datos de interes
dev.off()

0.3*(length(Matriz[,1])) #adaptar segun los datos de interes
m<-as.integer(readline(prompt='Maximo lag m=  '))   

print('Elija la Ventana: (1)Hann  (2)Hamming  (3)Parzen')
ventana<-as.integer(readline(prompt='Ventana?  '))

print('Elija intervalos de confianza: (1)95% (2)90%')
intervalo<-as.integer(readline(prompt='Intervalo?:  '))
#[n,q]>-dim(Matriz)
n<-dim(Matriz)[1]
q<-dim(Matriz)[2]

Resul<-matrix(NA,m+1 ,5)  #va las bandas de significancia
# Calcula las frecuencias y las guarda
f<-rep(NA,m+1)


for (k in 0:m){
  f[k+1]=k/(2*m)
  Resul[k+1,1]<-f[k+1]
}


# Calcula intervalos de confianza

print('Grados de libertad')
gl<-(2*n-(m/2))/m

if (intervalo==1){
  chiinf=(.06414329*gl^1.157371-.09347972)/(1.783669*gl^(-1.319044)+.1701124)
  chisup=(.9295456*gl^.5908365+3.231091)/(.8309965*gl^(-.3875086)-.006014)
}

if (intervalo==2){
  chiinf=(.3559888*gl^2.408943-.3692226)/(.6930017*gl^1.299689+4.887728)
  chisup=(3.23178*gl^.9261549+19.64045)/(6.454432*gl^(-.8282872)+1.975337)
}



#Calculo de la Ventana
u<-rep(NA,m+1)
if(ventana==1){
  for( r in 0:m){
    u[r+1]<-.5*(1+cos(pi*r/m))
    }
}

if(ventana==2){
  for (r in 0:m) {
    u[r+1]=.54+.46*cos(pi*r/m)
    }
}

if(ventana==3){
for(r in 0:m){
if (r<=(m/2)){
u[r+1]=1-6*((r/m)^2)*(1-(r/m))
}
if(r>(m/2)){
u[r+1]=2*(1-(r/m))^3
}
}
}



for (j in 1:q){
x<-Matriz[,j]

# Remosi?n de la media de la serie

xm<-mean(x)
xd<-rep(NA,n)
for (i in 1:n){
xd[i]<-x[i]-xm
}

#Ventana coseno en los extremos
#l<-floor(.1*n)
l<-floor(min(.1*n))
for(i in 1:l){
c<-.5*(1-cos((i-1)*pi/(l-1)))
xd[i]<-c*xd[i]
xd[n-i+1]<-c*xd[n-i+1]
}

# Funci?n de autocovarianzas
###desde aca

Cov<-rep(NA,m+1)

for (r in 0:m){
  s<-0
  for(i in 1:(n-r)){
    s<- s + xd[i]*xd[i+r]
  }

  Cov[r+1]<-s/(n-r)
}


# Aplicaci?n de la ventana

R<-Cov*u


#C?lculo de espectro y su promedio
#tengo que definir G
Gsum<-0
G<-rep(NA,m+1)
for (k in 0:m){
s=0
for (r in 1:(m-1)){
s<-s+R[r+1]*cos(pi*r*k/m)
}
G[k+1]<-2*(R[1]+2*s+R[m+1]*cos(pi*k))
Gsum<-Gsum+G[k+1]
}
Gmed<-Gsum/(m+1)


# Continuo nulo
#poner corr como matriz
cor<-cor(x[2:n],x[1:n-1])
#r1<-cor[1,2]
r1<-cor
#ruido rojo

Cn<-rep(NA,m+1)
Cnsup<-rep(NA,m+1)
Cninf<-rep(NA,m+1)
for(k in 0:m){
Cn[k+1]<-Gmed*((1-r1^2)/(1+r1^2-2*r1*cos(pi*k/m)))
Cnsup[k+1]<-Cn[k+1]*chisup/gl
Cninf[k+1]<-Cn[k+1]*chiinf/gl
}


Cnn<-rep(NA,m+1)
Cnnsup<-rep(NA,m+1)
Cnninf<-rep(NA,m+1)

#ruido blanco
for (k in 0:m){
Cnn[k+1]<-Gmed
Cnnsup[k+1]<-Cnn[k+1]*chisup/gl
Cnninf[k+1]<-Cnn[k+1]*chiinf/gl
}

# Gr?fica

#figure
png(filename=paste(j,"figruidorojo.png"), width=5500, height=4000, res=600)
plot(f,G,type='l')
lines(f,Cninf,col='blue')
lines(f,Cn,col='red')
lines(f,Cnsup,col='blue')
dev.off()
#figure

png(filename=paste(j,"figruidoblanco.png"), width=5500, height=4000, res=600)
plot(f,G,type='l')
lines(f,Cnninf,col='blue')
lines(f,Cnn,col='red')
lines(f,Cnnsup,col='blue')
dev.off()

# Guarda datos


for (k in 0:m){
Resul[k+1,4*j-2]<-G[k+1]
Resul[k+1,4*j-1]<-Cninf[k+1]
Resul[k+1,4*j]<-Cn[k+1]
Resul[k+1,4*j+1]<-Cnsup[k+1]
}


print('Frec  Densidad espectral  CNinf CN CNsup')
print(Resul)





# Valores significativos

print('Valores significativos: f, 1/f, Cnsup, G')
print('Serie n?: ',paste(j))

for (k in 1:m){
if (G[k+1]>Cnsup[k+1]){
print(cbind(f[k+1],1/f[k+1],Cnsup[k+1],G[k+1]))
}
}

}


print('Listo!')
print('r1 testear con Anderson!!!')
print(r1)   #testear este R

#calcular Rcritico para comparar con r1.

Rcritico=(-1+1.645*sqrt(length(Matriz[,1])-2))/(length(Matriz[,1])-1)


write.table(Resul,col.names = c('f','G','Cninf','Cn','Cnsup'),'Resul_rojo.txt')

#NO GUARDA RESULTADOS DE RUIDO BLANCO...

#si el espectro sigue un modelo de ruido blanco los resultados de resul no son los correctos

#el continuo nulo y las bandas de significancia con constantes en el ruido blanco.
#buscar en el script donde esta R

resultado_rojo<-read.table('Resul_rojo.txt',head=TRUE)
head(resultado_rojo)
##aca poner para que de las frecuencias significativas

freqsig<-resultado_rojo$f[resultado_rojo$G>=resultado_rojo$Cnsup]

1/freqsig

