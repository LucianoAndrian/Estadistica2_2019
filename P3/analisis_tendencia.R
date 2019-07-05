# Metodos Estadisticos 2019
# 
cat("\014") #Limpiar consola si lo quiero hacer sin
#funci?n uso CTL+L
rm(list=ls())
graphics.off()
#print('setear directorio')
#setwd("/media/usuario/datos/Nati/Docencia/Metodos_estadisticos/2019/Clases_Practicas/Clase_Filtros/") 


######ajuste tendencia y restar tendencia en R


datostemperatura<-read.table('datos_temperatura.txt')
#hago el ajuste en funcion del tiempo 
ajuste1<-lm(temperatura~meses,data=datostemperatura) #Y , X!!
summary(ajuste1)
ajuste1$coefficients #coeficientes de la recta del ajuste lineal


plot(datostemperatura$meses,datostemperatura$temperatura,type='l')
abline(ajuste1,col='red',lwd=2)
ajuste1$fitted.values  #valores de la recta para cada tiempo. es lo que restariamos para quitar tendencia


tendencia<-ajuste1$coefficients[1]+ajuste1$coefficients[2]*datostemperatura$meses  #contruccion de la recta y=ax+b... coef[2]x+coef[1]

#como filtramos la tendencia??? me puedo quedar alrededor del 0  forma1
#o en valroes a los de mi variable   forma2

#forma1
datosf<-datostemperatura$temperatura - tendencia
plot(ts(datosf))

#forma2
datosff<-datostemperatura$temperatura - (ajuste1$coefficients[2]*datostemperatura$meses)
plot(ts(datosff))


