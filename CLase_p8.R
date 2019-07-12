#Practica_8
#Clase regresion tridimencional
setwd("C:/Users/Alumno/Desktop/Estadistica2/P8/")
#####
####ej_1####
rm(list=ls())
graphics.off()
#predecir y en funcion de x1 y x2
#lm(y~ x1+x2, data = DATOS )
#lm(y~.,data= DATOs) en funcion de todas
#lm(y~1,data = DATS) solo tiene en cuenta la ordenada al orifge
#summary(lm..)...
#ajuste$fitted.values

###EJ1###
rm(list=ls())
graphics.off()
datos<-read.table("datossoja.txt")
datos2<-datos[,1:3]

# Estudiar la s variables predictoras respecto a la variable a predecir
# tambien es necesario estudiar las variables predictoras entre si ya que varibles 
# dependientes entre si, es decir con una correlacion significativa, no aportarian informacion al modelo

plot(datos$rendimiento,datos$pp)
plot(datos$rendimiento,datos$temp)
plot(datos$rendimiento,datos$fertp)
plot(datos$rendimiento,datos$fertz)

# al ver el diagrama de dispercion podemos esperar correlacion positiva y significativa con la pp
# y negatva y significativa con la temp
# para los fertilizantes no se observa a priori que exista una relacion lineal entre ellas 
# (puede que exista otra relacion no lineal)

library(corrplot)
#qt(0.05/2,df=189-2)
Rcritico<-(1.97/(sqrt(1.97^2+189-2)))
correlacionadas<-vector()
correlaciones<-cor(datos)
correlaciones[which(abs(correlaciones)<Rcritico)]<-0 #LE ASIGNO 0 A LOS QUE NO SON SIGNIFICATIVOS
diag(correlaciones)<-0 #SACA LOS 1 DE LA DIAGONAL
corrplot(correlaciones)

# correalciones significativas con un 95% de confianza con un test, t-student a dos colas
# las correlaciones mas importantes con el rendimientos son las pp y la temp, aunque tambien existe
# una correlacion significativa etre la pp y la temp lo cual no sirve al modelo ya al relaizar las 
# regresion lineal tendra en cuanta una correlacion que en realidad es espuria

# luego de graficar determinar cuales pueden seguir un modelo lineal.. q es el que conocemos

# usaria estas dos variables, pp y temp, para realizar el modelo ya que son las que mas 
# correlacion tienen con el rendimiento

# modelo lineal del rendimiento contra pp y temp
ajuste<-lm(rendimiento ~ .,data=datos2)
summary(ajuste)
# veo cada varible como aporta al modelo y su significancia mediante un test t-student para el 
# coeficiente de correlacion parcial, en este caso el p-valor resulta menor que la significancia 
# 0.05 incluso que la 0.001, es decir las variables pp y t aportan al modelo con un nivel de confianza del 
# 99%

# en cuanto al modelo, el R cuadrado ajustado es mejor para este caso ya que penaliza segun la cantidad de 
# variables resulta similar al R cuadrado
# el R cuadrado ajustado no es testado en este paso, sino que se testa el R cuadrado nada mas

# el R cuadrado da significativo mediante un test de fhisher, para asegurarme veo el R cuadrado ajustado

# H0: las pendientes de las rectas de la regresion entre cada variable y el rendimient = 0
# h1:n al menos una es distinta de cero
i<-2 # cantidad de predictores
estadistico_F<-(0.74/i)*(ajuste$df.residual/(1-0.74))
F_critico<-qf(0.95,2,186)

# estadistico F > que F critico de tabla

# rechazo H0 con un 95% de confianza, entonces la varianza explicada por el modelo es significativa
# en el intervalo pp [0.88 - 1.97*0.067, 0.88 + 1.97*0.067]
#               temp [-0.43 - 1.97*0.085, -0.43 + 1.97*0.085]

plot.ts(datos$rendimiento)
lines(ajuste$fitted.values,col=2)

modelo<--81.9238713+0.8815552*datos$pp-0.4363457*datos$temp

#ajuste da la pendiente estmada pero eso no nos dice nada
#summary 
#se fija como aporta cada variable al modelo
#p value si son significativos ***..
#ajusted r-squared mide con la cantidad de varibles que se ponen en el modelo.
#cuando hay muchas variables combiene mirar este
#F estadistico y grados de libertad y pvalor al test
#tiene una varianza explicada del 74% y es sifnificativo por... test de fisher, h0 h1, significacion
#rechazo h0 entonces existe al menos un i que es != de 0.
#grados de libertad n-1-2 (i=2)

####ej_2####
# Un analisis mas exhastivo
# Stepwise, basado en un algoritmo que busca la mejor relacion entre la cantidad de variables
# y el mejor modelo que pueda obetener.
# Es decir, no siempre agregar mas variables supone una mejora significativa del modelo y el costo
# computacional puede ser alto al agregar variables
# para ello utiliza el valor del AIC que cuanto menor sea indicara un mejor modelo

library(leaps)
library(stats)
ajust<-lm(rendimiento~.,data = datos) # modelo que incluye todas
null<-lm(rendimiento~1,data = datos)  # modelo que incluye solo ordenada al origen

step<-step(null, scope=list(lower=null, upper=ajust), direction="forward",test='F') 
# el metodo stewise me muestra que el mejor modelo que voy a poder tener es 
# el que al modelo anterior le suma el fertilizante
# si seagregara el fertilizante Z, el AIC creceria y el modelo seria peor en relacion 
# beneficio<->cantidad de variables

# cuanta varianza explica este modelo??

mejorajustecv=regsubsets(rendimiento~.,data=datos,nvmax=4,method="forward") #stepwise, pongo hasta m?ximo de variables
summary(mejorajustecv) #me selecciona para cada modelo las variables que presentan el menor AIC
summary(mejorajustecv)$adjr2 # me muestra el r2 ajustado de cada modelo
plot(mejorajustecv,scale='adjr2') # plotea el r2 ajustado para cada modelo

# efectivamente el modelo que usa las variables pp, temp y fertp es el que mas varianza 
# explica con un 74,43% 
# esto mismo se puede ver con el lm()
# la varianza explicada por el r cuadrado ajustado no esta testeada

# H0: las pendientes de las rectas de la regresion entre cada variable y el rendimient = 0
# h1:n al menos una es distinta de cero
i<-3 # cantidad de predictores
estadistico_F<-(0.7443/i)*(185/(1-0.7443))
F_critico<-qf(0.95,3,185)

# estadistico F > que F critico de tabla

# rechazo H0 con un 95% de confianza, entonces al menos una de las pendientes es significativamente
# distinta de cero entonces la varianza explicada por el modelo es significativamente distinta 
# de la no explicada

modelo2<--75.9556987+0.8630710*datos$pp-0.4955649*datos$temp+-0.1326529*datos$fertP

# si solo se conciderarn los datos del productor de soja el modelo seria bueno?
# es de esperar que no, ya que una de los fertilizantes no presentaba correlacion con el rendimiento
# y el otro si pero no mucha

datos3<-data.frame(rendimiento=datos[,1],fertP=datos[,4],fertZ=datos[,5])
summary(lm(rendimiento~.,data=datos3))
ajuste3<-lm(rendimiento~.,data=datos3)
# ninguno de los dos fertilizante aporta significativamente al modelo, su p-valor > 0.05 y a 0.01
# por lo tanto no se puede rechazar hipotesis nula del test t-student que indica que el r pertenece a
# a un poblacion con ro = 0
# y da un R cuadrado ajustado = 0.006174... es decir un 0.16% de la varianza explicada
# es obvio que no es significativa pero igual:
i<-2 # cantidad de predictores
estadistico_F<-(0.01674/i)*(185/(1-0.01674))
F_critico<-qf(0.95,2,186)

# estadistico F menor que F critico, no puedo rechazar h0 del test de fisher, las pendientes 
# pertenecen auna poblacion = 0
# y la varianza explicada no es significativa


####ej_3####
rm(list=ls())
graphics.off()

datos2<-read.table("Datos_ejercicio_3.txt", head=T) 
head(datos2) 
datos2<-datos2[,-1]

attach(datos2) 

ajust<-lm(R_SIN_T~.,data = datos2) #modelo que incluye todas
null<-lm(R_SIN_T~1,data=datos2) #modelo que incluye solo ordenada al origen

library(corrplot)
t<-qt(1-0.05/2,df = (length(datos2[,1]-2))) #t critico al 95% de confanza a dos colas
# de una distribucion t student con 25 n-2 grados de libertad

Rcritico<-(t/(sqrt(t^2+27-2)))
library(corrplot)
correlacionadas<-vector()
correlaciones<-cor(datos2)
correlaciones[which(abs(correlaciones)<Rcritico)]<-0 #LE ASIGNO 0 A LOS QUE NO SON SIGNIFICATIVOS
# los que no rehcazan h0 del test t de student, la cual dice que el coeficiente de correlacio
# pertenece a una poblacion = 0

corrplot(correlaciones,diag = F)

# el rendimiento sin tendencia tiene correlaciones significativas con un 95% de confianza
# con pp_12, tmax_1, pp_2 y tmax_4
# tmax_1 tiene una correlacion significativa con 5% de significancia con tmax_4

# Stepwise 
step<-step(null, scope=list(lower=null, upper=ajust), direction="forward",test='F') #me muestra el paso a paso del stepwise

# el mejor modelo resultante segun el algoritmo es 
# Step:  AIC=304.59
# R_SIN_T ~ TMAX_1 + TMIN_12 + PP_12 + PP_2
step$coefficients
# (Intercept)      TMAX_1     TMIN_12       PP_12        PP_2 
#  858.644509  -90.481110   98.619584    1.189943    1.119546 

# las variables que formar parte del mejor modelo segun este algoritmo
# son casi las mismas que las que resultan tener correlacion significativa
# salvo por tmin12 que no resulta significativa en el analisis previo

# Varianza que explica este modelo

summary(lm(formula = R_SIN_T ~ TMAX_1 + TMIN_12 + PP_12 + PP_2, data = datos2))
# explica un 48.6% de la varianza (mirando R cuadrado ajustado)
# no esta testeada

# H0: las pendientes de las rectas de la regresion entre cada variable y el rendimient = 0
# h1:n al menos una es distinta de cero
i<-4 # cantidad de predictores
estadistico_F<-(0.486/i)*(22/(1-0.486))
F_critico<-qf(0.95,4,22)
# estadistico F > que F critico de tabla

# rechazo H0 con un 95% de confianza, entonces al menos una de las pendientes es significativamente
# distinta de cero entonces la varianza explicada por el modelo tambien es significativamente distinta 
# de la no explicada

modelo<-858.644509 -90.481110*TMAX_1 + 90.619584*TMIN_12 + 1.189943*PP_12 + 1.119546*PP_2

plot.ts(R_SIN_T)
lines(modelo,col=2)
legend(1, -500, legend=c("R_sin_T", "modelo_stepwise"),
       col=c("black", "red"), lty=1, cex=0.8)

# es el que mas varianza explica??
mejorajustecv=regsubsets(R_SIN_T~.,data=datos2,nvmax=10,method="forward") #stepwise, pongo hasta m?ximo de variables
summary(mejorajustecv) #me selecciona para cada modelo las variables que presentan el menor AIC
summary(mejorajustecv)$adjr2 # me muestra el r2 ajustado de cada modelo
coef<-coef(mejorajustecv,1:10) # los coeficientes asociados a cada modelo

# no, el modelo que usa 7 variables es el que mas varianza explica
# explica aproximadamente un 4% mas de varianza 


modelo_7v<-coef[[7]][1]+coef[[7]][2]*datos2$PP_12+coef[[7]][3]*datos2$TMAX_12+coef[[7]][4]*datos2$TMIN_12+
  coef[[7]][5]*datos2$TMAX_1+coef[[7]][6]*datos2$TMIN_1+coef[[7]][7]*datos2$PP_2+coef[[7]][8]*datos2$TMAX_4

plot.ts(R_SIN_T)
lines(modelo,col=2)
lines(modelo_7v,col=4)
legend(1, -500, legend=c("R_sin_T", "modelo_stepwise","modelo_7v"),
       col=c("black", "red","blue"), lty=1, cex=0.8)

# vemos q este modelo se parece mas a la serie original, no mucho, pero al costo de agrwegar 3 variables mas

## lugo de testear la varianza del modelo de 7 variables
## resulta significativa con un95% de confianza

# Eliminando las tmax de todos los meses
datos3<-datos2[,-c(3,6,10)]

attach(datos3)

ajust<-lm(R_SIN_T~.,data = datos3) #modelo que incluye todas
null<-lm(R_SIN_T~1,data=datos3) #modelo que incluye solo ordenada al origen


step<-step(null, scope=list(lower=null, upper=ajust), direction="forward",test='F') #me muestra el paso a paso del stepwise

# mejor modelo: AIC 310.72
# Call:
#  lm(formula = R_SIN_T ~ PP_12 + PP_2 + TMIN_12, data = datos3)
#
# Coefficients:
#   (Intercept)        PP_12         PP_2      TMIN_12  
#    -1786.570        1.810        1.287       82.265  

summary(step)

# explica un 33.55% de varianza
# significativa?

# H0: las pendientes de las rectas de la regresion entre cada variable y el rendimient = 0
# h1:n al menos una es distinta de cero
i<-3 # cantidad de predictores
estadistico_F<-(0.3355/i)*(23/(1-0.3355))
F_critico<-qf(0.95,3,23)
# estadistico F > que F critico de tabla

# rechazo H0 con un 95% de confianza, entonces al menos una de las pendientes es significativamente
# distinta de cero entonces la varianza explicada por el modelo tambien es significativamente distinta 
# de la no explicada

modelo_sin_Tmax<- -1786.570 + 1.81*PP_12 + 1.287*PP_2 + 82.265*TMIN_12

plot.ts(R_SIN_T)
lines(modelo,col=2)
lines(modelo_sin_Tmax,col=4)

# si bien el primer modelo no representa muy bien los extremos positivos, si lo hace con los negativos
# el 2do modelo sin las Tmax esta mas cerca de los extremos positivos pero muy lejos de los negativos
# en cuantoa  a cantidad de varianza explicada el primer modelo explica aproximadamente un 15%
# y en cuanto al AIC el primero tiene un AIC de 304.59 y el 2do de 310.72
# por lo tanto, es mejor y combiene aplicar el 1er modelo

#regresion multiple standar con todas las variables

summary(lm(R_SIN_T ~., data=datos2))
# no todas las variables aportan de manera significativa
# la varianza exlicada por el modelo es de un 49.36%

i<-10 # cantidad de predictores
estadistico_F<-(0.4936/i)*(16/(1-0.4936))
F_critico<-qf(0.95,10,16)

# la varianza no es significativa

ajuste1<-lm(R_SIN_T ~., data=datos2)
plot.ts(R_SIN_T)
lines(ajuste1$fitted.values,col=2)
lines(modelo,col=3)

# es similar al modelo 1 pero ve mejor algunos extremos positivos, otros los sobre estima
# aun asi la varianza no resulta significativa dados los grados de libertad que se pierden al 
# utilizar tantas variables
