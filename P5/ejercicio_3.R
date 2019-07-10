#ejercicio 4 p5 y ejericio 3 p6
#que se complementan
setwd("C:/Users/Alumno/Desktop/Estadistica2/P5/")
graphics.off()
rm(list=ls())

# datos medidos cada 238 hs
# ~3 anios de medicion
datos<-read.table("autocorr.txt",header = T)

plot.ts(datos)

# antes que nada:
# la serie podria presentar un salto???
# no es facil verlo a simple vista, pero algo se puede intuir
# test de yamamoto entre 10 años mas menos la mitad d elos datos ~56

Y<-vector()
for(i in 1:20){
  datosA<-datos[1:(46+i),1]
  datosB<-datos[(47+i):113,1]
  mediaA<-mean(datosA)
  mediaB<-mean(datosB)
  cA<-sd(datosA)*qt(0.975,df=112)*1/sqrt(113-1)
  cB<-sd(datosB)*qt(0.975,df=112)*1/sqrt(113-1)
  #tcritico 1,96
  Y[i]<-(mediaA-mediaB)/(cA+cB)
  rm(datosB)
}
Y
which(abs(Y)==max(abs(Y)))
# salto en el tiempo 47
# serie sin el salto, sumandole a la primera parte la media de la 2da
nueva<-c(datosA+mediaB,datosB)
plot.ts(datos[,1])
lines(nueva,col=2)
abline(summary(lm(datos[,1]~seq(1:113)))) 
abline(summary(lm(nueva~seq(1:113))),col=2) 

# la serie tenia tendencia antes??
summary(lm(datos[,1]~seq(1:113))) #si

# luego de sacar el salto?
summary(lm(nueva~seq(1:113))) 
# no, luego de remover el salto la serie no presenta una tendencia significativa  

# tiene otro salto??
Y<-vector()
for(i in 1:20){
  datosA<-nueva[1:(46+i)]
  datosB<-nueva[(47+i):113]
  mediaA<-mean(datosA)
  mediaB<-mean(datosB)
  cA<-sd(datosA)*qt(0.975,df=112)*1/sqrt(113-1)
  cB<-sd(datosB)*qt(0.975,df=112)*1/sqrt(113-1)
  #tcritico 1,96
  Y[i]<-(mediaA-mediaB)/(cA+cB)
  rm(datosB)
}
Y
# no presenta saltos ahora

#la serie sin salto es muy distinta de la serie sin tendencia?
source("C:/Users/Alumno/Desktop/Estadistica2/Scripts/FUNCIONES.R")
datos<-analisis_tendencia(data.frame(seq(1:113),datos[,1])) 
# muy similar, pero no presenta nada de tendencia

# como la tendencia de la serie sin el salto no es significtiva voy a seguir usando esa serie
# ahora a las consignas
length(nueva)   
#datos impares --> borro el ultimo
#dt=238
nueva<-nueva[1:112]

funcion_armonico(nueva)
#Armonico 3 explica mas varianza...
#A3--->~ciclo anual
#ya que delta t 238hs que por 112 dias equivalen a 26656 horas
#el periodo del armonico 3 sera 26656/3--->equivale a 370.22 dias
#es el mas cercano a la onda anual..

#es significativo=--->analisis espectral

espectro(as.matrix(nueva)) #al 90%
# 33.60 22.40 16.80 13.44 11.20  9.60  8.40 periodos sifnigicativos... cada 238 hs
# la onda anual tendria un periodo sifnificativo de ~37...
# si el analisis se hubiera realizado con el 95 de confianza--> solo tendriamos 13.44 11.20  9.60  8.40
# la onda anual no aparece como significtiva aunque en el alaisis armonico se lleva un gran porcentaje de varianza
# a tener en cuenta: en este analisis espectral ( y siempre ), se tiene en cuenta solo el 30% de lo datos
# que para este caso resulta unos 112*0.34 ~ 38 datos, 38 datos cada 238  dividio 24 ~ 376 dias
# la metodologia no logra ver con claridad esta onda que es mas clara cuando se miran todos los datos
# veamoslo, si bien no debe hacerse puesto que pierde estabilidad estadistica y ya no es valido 
# el testo pero para ver que es lo que sucede cuando tomamos el 50% de los datos para el maximo lag
espectro(as.matrix(nueva)) #poner 50 y 60
# con el 50%  ---> 37.333333 18.666667 16.000000 14.000000 12.444444  9.333333  8.615385  4.307692
# logra ver la onda anual ~37.33 y desaparecen esos valores de 33 y 22 q estaban antes

# ya se que la onda anual esta, pero no aparece como significativa, debido a que la energia asociada a su frecuencia
# no aparece dentro de los pocos datos que toma el analisis espectral
# pero al mismo tiempo me impide ver con mejor claridad otros fenomenos
# incluso al aplicar wavelet veo ondas de periodos mayores a 30 significativas  

# voy a filtrarla, aunque el analisis espectral no indique que sea significativa, debido a lo 
# dicho antes en relacion con l apoca cantidad de datos que usa

nueva_fil<-filonda(nueva)
funcion_armonico(nueva_fil)

#se ven nuevos armonicos qu eexplican muha varianza
# entre ellos el 2, 5 y 8
# 2---->555.33 dias ~ 1 anio y medio
# 5---->222.13 dias ~ 7.4 meses
# 7---->158.66 dias ~ 5.3 meses

espectro(as.matrix(nueva_fil))
#] 16.80 13.44 11.20  9.60  8.40  4.20
# 16.8 ----> ~ 166 dias ~ 5.53 meses
# 13.4 ----> ~ 133 dias
# 11.2 ----> ~ 111 dias
# 9.6  ----> ~ 95  dias
# 8.4  ----> ~ 83  dias
# 4.2  ----> ~ 41  dias

# veamos wavelet ahora
library(WaveletComp)

dat<-data.frame(seq(1:112),nueva_fil)

my.wt =analyze.wavelet(dat, my.series=2)

wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),
         siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',
         plot.ridge = F)

# vemos que periodos entre 10,16 y 25 son sifnificativos y se ve claramente como los periodos mas cortos
# son mas significaticos solo al comienzo y final de la serie

# de las tres metodologias previas empleadas podemos decir que la onda de periodo de ~ 5.3 ~5.5 meses 
# es una de las mas iportantes y significativas, apareciendo con un armonico asociado que explica gran varianza 
# para el analisis espectral es significativa y tambien lo es en wavelet

plot.ts(nueva_fil,col=2)
lines(nueva)
