###ESTADISTICA PARA EL SISTEMA CLIMATICO 2 ANIO 2019
library(WaveletComp)
#me tengo que generar una columna con tiempos en el caso 
#de no tenerla que tenga igual longitud que el tamanio
#del vector considerado Y guardarlo como as.data.frame
#set.wd()
datos<-read.table("dat_1.txt",header=T) 
tiempos<-seq(1:length(datos[,1]))

datos2<-data.frame(tiempos,datos)

my.wt =analyze.wavelet(datos2, my.series=2) #seleccionar la columna de interes, aca esta puesta la uno
#figura para color puedo usar quantiles o interval
png('waveletfigura.png')
wt.image(my.wt,color.key="q", legend.params=list(lab="wavelet power levels"),siglvl = 0.05,timelab='Tiempo',periodlab = 'Periodo',main = 'Wavelet nro.2',plot.ridge = FALSE)
dev.off()

## Plot of average wavelet power:
wt.avg(my.wt, siglvl=0.05, sigcol="red")


###PARA AGREGAR FECHA AL EJE DE TIEMPO TENER EN CUENTA LO SIGUIENTE
monthyear <- seq(as.Date("1950-01-01"), as.Date("2018-12-01"),
                 by = "month")
monthyear <- strftime(monthyear, format = "%b %Y")
q<-seq(1,length(monthyear),20) ##CUANTOS PUNTOS ME QUEDO
prueba2<-monthyear[q]

#seleccionar de prueba cada diez 167/10 me agarro cada 16

wt.image(my.wt,  main = "wavelet power spectrum", 
         periodlab = "period (months)", timelab = "month and year",
         spec.time.axis = list(at = q, labels = prueba2),color.key='interval')
