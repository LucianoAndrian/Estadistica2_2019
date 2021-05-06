#print('setear directorio')
#setwd("C:/Users/Melu/Dropbox/Métodos estadísticos/Practica7") 

#cat("\014") #Limpiar consola si lo quiero hacer sin
#función uso CTL+L

#print('setear directorio')
#setwd("C:/Users/Melu/Dropbox/Métodos estadísticos/Practica7") 


datostemperaturahorarios<-read.table('datoshorarios.txt')

tiempohora<-seq(from=as.POSIXct("2018-01-01 00:00:00", tz="UTC"), 
           to=as.POSIXct("2018-01-07 23:59:59", tz="UTC"), by="hour")

plot(datostemperaturahorarios$tiempo,datostemperaturahorarios$temperatura,type='l')


###pienso podriamos filtrar variabilidad menor a 24 hs, nos interesaria observar ondas de mayor frecuencia


