#ej3P8
library(leaps)
library(stats)
setwd("C:/Users/Alumno/Desktop/Estadistica2/P8")

datos2<-read.table("Datos_ejercicio_3.txt", head=T) 
datos2<-datos2[,-1]
head(datos2) 
#colnames(datos2)<-c('var1','var2','var3','var4','var5',"var6","var7","var8","var9","var10","var11","var12") #pongo nombres
attach(datos2) 

ajust<-lm(R_SIN_T~.,data = datos2) #modelo que incluye todas
null<-lm(R_SIN_T~1,data=datos2) #modelo que incluye solo ordenada al origen

library(corrplot)

Rcritico<-(2.05/(sqrt(2.05^2+27-2)))

correlacionadas<-vector()

correlaciones<-cor(datos2)

cor2<-which(abs(correlaciones)>abs(Rcritico),arr.ind = T)

#correlaciones
cor3<-data.frame(colnames(correlaciones[,cor2[,2]]),colnames(correlaciones[,cor2[,1]]))
colnames(cor3)<-c("1","2")
cor3[which(cor3[,1]==cor3[,2]),]=NA

#VARIANZA
mejorajustecv=regsubsets(R_SIN_T~.,data=datos2,nvmax=10,method="forward") #stepwise, pongo hasta m?ximo de variables
#quiero explicar da5 en funcion de todas, nvmax= todas las que tengo yo decido cuando cortar

summary(mejorajustecv) #me selecciona para cada modelo las variables que presentan el menor AIC

summary(mejorajustecv)$adjr2 # me muestra el r2 ajustado de cada modelo

plot(mejorajustecv,scale='adjr2') # plotea el r2 ajustado para cada modelo

coef<-coef(mejorajustecv,1:10) # los coeficientes asociados a cada modelo

estimada<-coef[[7]][1]+coef[[7]][2]*datos2$PP_12+coef[[7]][3]*datos2$TMAX_12+coef[[7]][4]*datos2$TMIN_12+
  coef[[7]][5]*datos2$TMAX_1+coef[[7]][6]*datos2$TMIN_1+coef[[7]][7]*datos2$PP_2+coef[[7]][8]*datos2$TMAX_4

#modelada vs estimdad

plot.ts(datos2$R_SIN_T)
lines(estimada,col="red")

#steapwise forward
step<-step(null, scope=list(lower=null, upper=ajust), direction="forward",test='F') #me muestra el paso a paso del stepwise
#empezando con un modelo que tenga la ordenada al origen termne teniendo todos os 
#desde dond arranca hasta tener todo el modelo, y hacia adelante, y muestra un test de fisher
#corta cuando ya no tiene mas variables que aporten significativamente al modelo o cuando aumenta aic


