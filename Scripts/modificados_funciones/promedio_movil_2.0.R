#####DESCRIPCION#####
##Metodos estadisticos 2018
##Programa para realizar promedios moviles con cantidad impar de pesos, pensado para centrado.
##La serie de datos debe ser una columna y debe estar completa, y los
###tiempos deben estar en la primera columna. Luego el software pedira
###cual es la columna a la cual deseas aplicarle Prom Movil.

######\m/Luciano Andrian\m/######
#funcion:

#para los promedios moviles pesados tiene cargados coeficientes para 3 5 7 11 y 13
#pregunta si se quiere usar estos o ingresar nuevos
#si se selecciona un promedio pesado que no es uno de esos directamente hay que cargar los coeficientes

#pregunta antes de guardar el grafico

##################################
promedios_moviles<-function(x){
  serie<-x
  ####Calculo media y desv. estand. de la serie original
  orig<-as.integer(readline(prompt='Ingrese el nro de la columna donde esta su serie en el archivo:  '))   ###('Ingrese el nro de la columna donde esta su serie en el archivo: ')
  medorig<-mean(serie[,orig])
  ###Para ver algunos valores de la serie
  may<-max(serie[,orig])
  miy<-min(serie[,orig])
  maxi<-max(serie[,1])
  mixi<-min(serie[,1])
  
  ####
  print(' ')
  print(paste('Valor medio de la serie original: ',round(medorig,2)))
  
  dsorig<-(sd(serie[,orig])*(length(serie[,orig])-1))/(length(serie[,orig])) #chequear para que el 1
  print(paste('Desv. Estand. de la serie original: ',round(dsorig,2)))
  print(' ')
  
  pasos<- as.integer(readline(prompt='Ingrese la cantidad de Analisis con promedios moviles a realizar: '))
  print(pasos)
  
  n <- dim(serie)[1]
  newserie <- matrix(NA, n, pasos)
  
  cont<-1
  seguir<-1
  
  while (cont<=pasos)
  {
    if (seguir==1){
      ####Datos de entrada
      cant<-as.integer(readline(prompt='Ingrese el tamano de la ventana centrada a promediar (obs: debe ser impar)'))
      print(cant)
      print('Que tipo de peso desea aplicar?:')
      print('1 - Promedio movil ordinario')
      print('2 - Ingresar los pesos manualmente')
      print(' ')
      aux<-as.integer(readline(prompt='Su eleccion ha sido: '))
      print(aux)
      
      if (aux!=1 & aux!=2){
        print('Eleccion incorrecta')
      }
      peso3<-c(0.25,0.5,0.25)
      peso5<-c(0.1,0.2,0.4,0.2,0.1)
      peso7<-c(0.05,0.1,0.2,0.3,0.2,0.1,0.05)
      peso11<-c(0.01,0.02,0.05,0.1,0.15,0.34,0.15,0.1,0.05,0.02,0.01)
      peso13<-c(0.005,0.01,0.015,0.03,0.1,0.15,0.38,0.15,0.1,0.03,0.015,0.01,0.005)
      peso = rep(NA, cant)
      
      if (aux==1){
        for (i in 1:cant){
          peso[i]<-1/cant
        }
      } else if (aux==2){
        if(cant==3|cant==5|cant==7|cant==11|cant==13){
          pesos<-readline("Puede usar coeficientes ya cargados, desea hacerlo?(si/no): ")
          if(pesos=="si" & cant==3){
            peso<-peso3
          } else if(pesos=="si" & cant==5){
            peso<-peso5
          } else if(pesos=="si" & cant==7){
            peso<-peso7
          } else if(pesos=="si" & cant==11){
            peso<-peso11
          } else if(pesos=="si" & cant==13){
            peso<-peso13
          } else if(pesos=="no"){
            for (i in 1:cant){
              peso[i]<-as.numeric(readline(prompt=paste('Ingrese el peso de la variable X', i, ':')))
            }
            
          } else { 
            for (i in 1:cant){
              peso[i]<-as.numeric(readline(prompt=paste('Ingrese el peso de la variable X', i, ':')))
            } 
          }
        } else { 
          for (i in 1:cant){
            peso[i]<-as.numeric(readline(prompt=paste('Ingrese el peso de la variable X', i, ':')))
          }  
        }
      }
      
      
      #defino k como el radio de la ventana
      k <- (cant-1)/2
      
      ###Aplico Promedios m???viles a mi serie
      if (aux==0 || aux==1 || aux==2){
        col<-as.integer(readline(prompt='Ingrese el nro de la columna donde est??? su serie en el archivo: '))
        dim=dim(serie)
        for (i in (k+1):(dim[1]-k)){
          sum=0
          for (j in -k:k){
            sum<-sum + serie[i+j, col]*peso[j+k+1]
          }
          
          newserie[i,cont]<-sum
        }
        
        print(paste('Su serie pesada se encuentra en la columna ',cont,' de la variable newserie'))
        
        ##Grafico la serie original y la pesada
        grafico<-readline("Desea guardar el grafico de la serie pesada? (si/no): ")
        if(grafico=="si"){
          nombre<-readline("Nombre de la imagen a guardar: ")
          png(filename=paste(cont,nombre,".png",sep=""), width=5500, height=4000, res=600)
          plot(serie[,1],serie[,col],lwd=2,type='l',main=paste('Serie original y Serie pesada con periodos mayores a ',cant),xlab='tiempo',ylab='variable')
          lines(serie[,1],newserie[,cont],col='red',lwd=2.5)
          legend('topleft',legend=c('serieoriginal','seriepesada'),col=c('black','red'),lty=c(1,1),lwd=c(2,2.5))
          dev.off()
        }
        
        
        ### Calculo media y desv. stand.
        media<-mean(newserie[,cont], na.rm=TRUE)
        print(paste('El valor medio de la serie es: ',media))
        ds<-sd(newserie[,cont],1)
        print(paste('El desv???o estandar de la serie es: ',ds))
        print(' ')
      }
      
      cont<-cont+1
      if ((cont-1)!=pasos){
        seguir<-as.integer(readline(promp='Desea continuar con el siguiente promedio movil? 0-No; 1-Si ?: '))
        print(' ')
      }
    }
    else{
      print('%%%%%%%%%%%%%%%%%%%')
      print('Analisis interrumpido')
      break
    }
    
    if ((cont-1)==pasos){
      print('%%%%%%%%%%%%%%%%%%%')
      print('Analisis finalizado') 
    }
  }
  
  
  newserie
  return<-newserie
}