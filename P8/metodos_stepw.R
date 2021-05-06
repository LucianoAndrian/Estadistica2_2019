#bajar librer?a leaps!!!

datos<-read.table("datos_sw.txt", head=T) #leo los datos
head(datos) #primeros 6 datos
colnames(datos)<-c('dat1','dat2','dat3','dat4','dat5') #pongo nombres
attach(datos) #suelto las variables
require(leaps)
library(leaps) 

#steapwise forward
library(stats)
ajust<-lm(dat5~.,data=datos) #modelo que incluye todas
null<-lm(dat5~1,data=datos) #modelo que incluye solo ordenada al origen

step<-step(null, scope=list(lower=null, upper=ajust), direction="forward",test='F') #me muestra el paso a paso del stepwise
#empezando con un modelo que tenga la ordenada al origen termne teniendo todos os 
#desde dond arranca hasta tener todo el modelo, y hacia adelante, y muestra un test de fisher
step$anova
step$coefficients

mejorajustecv=regsubsets(dat5~.,data=datos,nvmax=4,method="forward") #stepwise, pongo hasta m?ximo de variables
summary(mejorajustecv) #me selecciona para cada modelo las variables que presentan el menor AIC
summary(mejorajustecv)$adjr2 # me muestra el r2 ajustado de cada modelo
plot(mejorajustecv,scale='adjr2') # plotea el r2 ajustado para cada modelo

coef(mejorajustecv,1:4) # los coeficientes asociados a cada modelo



