analisis_tendencia<-function(datos){

  readline("recordar que laa 1er columna debe contener los tiempos y la 2da los datos")

  serie<-data.frame(datos[which(!is.na(datos[,2])),1],datos[which(!is.na(datos[,2])),2]) 
  serie[,1]<-seq(1:length(serie[,1])) #IMPORTANTE XQ SINO AFECTA EL LM
  ajuste1<-lm(serie[,2]~serie[,1],data=serie) #Y , X!!
  tendencia<-ajuste1$coefficients[1]+ajuste1$coefficients[2]*serie[,1]  #contruccion de la recta y=ax+b... coef[2]x+coef[1]
  
  ff<-readline("forma 1 centrado en cero o 2 en valores de la variable--(1 o 2)?: ")
  if(ff==1){
    datosf<-serie[,2] - tendencia
    plot(ts(datosf))
    } else {
      datosf<-serie[,2]- (ajuste1$coefficients[2]*serie[,1])
      plot(ts(datosf))
      }
return(datosf)
}