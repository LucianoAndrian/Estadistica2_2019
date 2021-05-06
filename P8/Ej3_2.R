#ej3P8
library(leaps)
library(stats)
setwd("C:/Users/Alumno/Desktop/Estadistica2/P8")

datos<-read.table("Datos_ejercicio_3.txt", head=T) 
datos2<-datos[,-1]
head(datos2) 
#colnames(datos2)<-c('var1','var2','var3','var4','var5',"var6","var7","var8","var9","var10","var11","var12") #pongo nombres
attach(datos2) 

ajust<-lm(R_SIN_T~.,data = datos2) #modelo que incluye todas
null<-lm(R_SIN_T~1,data=datos2) #modelo que incluye solo ordenada al origen

library(corrplot)

#####

Rcritico<-(2.06/(sqrt(2.06^2+27-2)))   #RCRITICO AL 5% DE SIG, CON  DESPEJADO DE LA FORMULA ~0.38
                                       #CON 25 GRADOS DE LIBERTAD?? 
correlaciones<-cor(datos2)

correlaciones[which(abs(correlaciones)<Rcritico)]<-0 #LE ASIGNO 0 A LOS QUE NO SON SIGNIFICATIVOS

diag(correlaciones)<-0 #SACA LOS 1 DE LA DIAGONAL

corrplot(correlaciones)

####

#correlaciones
#cor3<-data.frame(colnames(correlaciones[,cor2[,2]]),colnames(correlaciones[,cor2[,1]]))
#colnames(cor3)<-c("1","2")
#cor3[which(cor3[,1]==cor3[,2]),]=NA

step<-step(null, scope=list(lower=null, upper=ajust), direction="forward",test='F') #me muestra el paso a paso del stepwise

#mejor modelo
#Step:  AIC=304.59
#R_SIN_T ~ TMAX_1 + TMIN_12 + PP_12 + PP_2

#VARIANZA
mejorajustecv=regsubsets(R_SIN_T~.,data=datos2,nvmax=10,method="forward") #stepwise, pongo hasta m?ximo de variables
summary(mejorajustecv) #me selecciona para cada modelo las variables que presentan el menor AIC
summary(mejorajustecv)$adjr2 # me muestra el r2 ajustado de cada modelo
#plot(mejorajustecv,scale='adjr2') # plotea el r2 ajustado para cada modelo

#LA varianza maxima explicada usa 7 variables, la de 4 explica el 43%

coef<-coef(mejorajustecv,1:10) # los coeficientes asociados a cada modelo
#> coef[[4]]
#(Intercept)       PP_12     TMIN_12      TMAX_1        PP_2 
# 858.644509    1.189943   98.619584  -90.481110    1.119546 

#estimada con 4 variables
estimada<-coef[[4]][1]+coef[[4]][4]*datos2$TMAX_1+coef[[4]][3]*datos2$TMIN_12+
  coef[[4]][2]*datos2$PP_12+coef[[4]][5]*datos2$PP_2

#estimada con 7 variables
estimada2<-coef[[7]][1]+coef[[7]][2]*datos2$PP_12+coef[[7]][3]*datos2$TMAX_12+coef[[7]][4]*datos2$TMIN_12+
  coef[[7]][5]*datos2$TMAX_1+coef[[7]][6]*datos2$TMIN_1+coef[[7]][7]*datos2$PP_2+coef[[7]][8]*datos2$TMAX_4

#modelada vs estimdad

plot.ts(datos2$R_SIN_T,lwd=2)
lines(estimada,col="red",lwd=2)
lines(estimada2,col="blue",lwd=2)


###########ELIMINANDO TMAX
colnames(datos)
t_max<-c(-1,-4,-7,-11)
datos3<-datos[,t_max]
head(datos3)

attach(datos3) 

ajust2<-lm(R_SIN_T~.,data = datos3) #modelo que incluye todas
null2<-lm(R_SIN_T~1,data=datos3) #modelo que incluye solo ordenada al origen

#####

Rcritico<-(2.06/(sqrt(2.06^2+27-2)))   #RCRITICO AL 5% DE SIG, CON  DESPEJADO DE LA FORMULA ~0.38
#CON 25 GRADOS DE LIBERTAD?? 
correlaciones<-cor(datos3)

correlaciones[which(abs(correlaciones)<Rcritico)]<-0 #LE ASIGNO 0 A LOS QUE NO SON SIGNIFICATIVOS

diag(correlaciones)<-0 #SACA LOS 1 DE LA DIAGONAL

corrplot(correlaciones)

####

step<-step(null2, scope=list(lower=null2, upper=ajust2), direction="forward",test='F') #me muestra el paso a paso del stepwise

#mejor modelo
#Step:  AIC=310.72
#R_SIN_T ~ PP_12 + PP_2 + TMIN_12

#VARIANZA
mejorajustecv=regsubsets(R_SIN_T~.,data=datos3,nvmax=10,method="forward") #stepwise, pongo hasta m?ximo de variables
summary(mejorajustecv) 
summary(mejorajustecv)$adjr2


#LA varianza maxima explicada usa 4 variables, la de 3 explica el 33%

coef2<-coef(mejorajustecv,1:7) # los coeficientes asociados a cada modelo
#> coef2[[3]]
# (Intercept)        PP_12      TMIN_12         PP_2 
#-1786.570016     1.809686    82.265228     1.287473 

#estimada con 3 variables
estimada2_1<-coef2[[3]][1]+coef[[3]][2]*datos3$PP_12+coef[[3]][4]*datos3$PP_2+
  coef[[4]][3]*datos2$TMIN_12

#estimada con 4 variables
#estimada2<-coef[[7]][1]+coef[[7]][2]*datos2$PP_12+coef[[7]][3]*datos2$TMAX_12+coef[[7]][4]*datos2$TMIN_12+
#  coef[[7]][5]*datos2$TMAX_1+coef[[7]][6]*datos2$TMIN_1+coef[[7]][7]*datos2$PP_2+coef[[7]][8]*datos2$TMAX_4

#modelada vs estimdad

plot.ts(datos2$R_SIN_T,lwd=2,ylim=c(-35000,800))
lines(estimada2_1,col="red",lwd=2)
#lines(estimada2,col="blue",lwd=2)
plot.ts(estimada2_1)
#DA muy mal, valores muy negativos
