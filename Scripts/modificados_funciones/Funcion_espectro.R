#SCRIPT CALCULO ESPECTRO 2018
#####DESCRIPCION######
#CALCULA ESPECTOS DE SERIES TEMPORALES DISCRETAS DE ACUERDO AL METODO
# DE BLACKMAN-TUKEY (LIBRO DE OTNES, PAG: 270-272), CON EL CORRESPONDIENTE
#CONTINUO NULO Y BANDAS DE SIGNIFICANCIA AL 5% O AL 10% (CLIMATIC CHANGE,
# NOTA TeCNICA # 79, PAG: 36-42).#
#LA MATRIZ DE ENTRADA DE DATOS DEBE PRESENTAR LAS SERIES A LAS QUE SE LES
#VA A CALCULAR EL ESPECTRO EN COLUMNA. TODAS LAS SERIES DEBEN TENER EL 
# MISMO NUMERO DE REGISTROS (LA MISMA LONGITUD).
# AL COMENZAR EL PROGRAMA REMOVER LAS MEDIAS DE CADA SERIE Y APLICAR UNA
# VENTANA COSENO EN LOS EXTREMOS. LUEGO APLICAR UNA VENTANA HANN, HAMMING
# O PARZEN A ELECCION
# LUEGO GRAFICAR LOS ESPECTROS DE CADA SERIE Y GUARDAR LOS DATOS EN UNA

# MATRIZ CON EL NOMBRE "result" CUYAS COLUMNAS SON:
# col1: frecuencias
# col2: espectro de la serie 1
# col3: limite inferior del intervalo de confianza (serie 1)
# col4: continuo nulo (serie 1)
# col5: limite superior del intervalo de confianza (serie 1)
# col6: espectro de la serie 2
# col7: limite inferior del intervalo de confianza (serie 2)
# col8: continuo nulo (serie 2)
# col9: limite superior del intervalo de confianza (serie 2)
# Y ASI SIGUIENDO CON TODAS LAS SERIES PRESENTES EN LA MATRIZ DE ENTRADA.
# LA MATRIZ "result" PUEDE LUEGO EXPORTARSE PARA GRAFICAR LOS ESPECTROS EN
# ORIGIN Y ALLI TRABAJARLOS.


######\m/LUCIANO ANDRIAN\m/######
#Convertida en funcion.
#lag maximo en funcion del porcentaje del total de datos. 30% por defecto. 
#Pregunta si se quiere modificar

#Pregunta si se desea testear, es necesario testear para graficar y guardar imagen. 
#Ya que no tiene sentido guardar dos graficos de los cuales no se sabe si H0

#El testeo con procedimiento de Anderson, 1 o 2 colas, tiene en cuenta el signo de r1
#imprime por pantalla el H0 y con esto setea que solo realizara el grafico del ruido correspondiente

#Pregunta si se quiere guardar la matriz result que contiene datos de ruido rojo,
##aun no existe matriz para ruido blanco

#Pregunta que resultado devolver con la funcion
##aun no devuelve dos resultados a la vez.
###########################################################################

espectro<-function(datos){
  
  Matriz<-datos
  p<-readline("Para calular el maximo Lag se usaran el 30% de los datos desea modificar el % (si/no): ")
  if(p=="si"){
    porcentaje<-as.numeric(readline("Porcentaje de datos para el maximo lag: "))
    m<-(porcentaje/100)*(length(Matriz[,1])) 
  } else if(p=="no"){
    m<-0.3*(length(Matriz[,1])) 
  }
  
  ventana<-as.integer(readline(prompt='Ventana: (1)Hann  (2)Hamming  (3)Parzen ? ' ))
  
  intervalo<-as.integer(readline(prompt='Intervalo: (1)95% (2)90% ? '))
  
  n<-dim(Matriz)[1]
  q<-dim(Matriz)[2]
  
  result<-matrix(NA,m+1 ,5)
  f<-rep(NA,m+1)
  
  for (k in 0:m){
    f[k+1]=k/(2*m)
    result[k+1,1]<-f[k+1]
  }
  
  
  # Calcula intervalos de confianza
  
  #print('Grados de libertad')
  gl<-(2*n-(m/2))/m
  
  if (intervalo==1){
    chiinf=(.06414329*gl^1.157371-.09347972)/(1.783669*gl^(-1.319044)+.1701124)
    chisup=(.9295456*gl^.5908365+3.231091)/(.8309965*gl^(-.3875086)-.006014)
  } else {
    chiinf=(.3559888*gl^2.408943-.3692226)/(.6930017*gl^1.299689+4.887728)
    chisup=(3.23178*gl^.9261549+19.64045)/(6.454432*gl^(-.8282872)+1.975337)
  }
  
  #Calculo de la Ventana
  
  u<-rep(NA,m+1)
  if(ventana==1){
    for( r in 0:m){
      u[r+1]<-.5*(1+cos(pi*r/m))
    }
  }
  
  if(ventana==2){
    for (r in 0:m) {
      u[r+1]=.54+.46*cos(pi*r/m)
    }
  }
  
  if(ventana==3){
    for(r in 0:m){
      if (r<=(m/2)){
        u[r+1]=1-6*((r/m)^2)*(1-(r/m))
      }
      if(r>(m/2)){
        u[r+1]=2*(1-(r/m))^3
      }
    }
  }
  
  
  
  for (j in 1:q){
    x<-Matriz[,j]
    
    # Remosi?n de la media de la serie
    
    xm<-mean(x)
    xd<-rep(NA,n)
    for (i in 1:n){
      xd[i]<-x[i]-xm
    }
    
    #Ventana coseno en los extremos
    #l<-floor(.1*n)
    l<-floor(min(.1*n))
    for(i in 1:l){
      c<-.5*(1-cos((i-1)*pi/(l-1)))
      xd[i]<-c*xd[i]
      xd[n-i+1]<-c*xd[n-i+1]
    }
    
    # Funcion de autocovarianzas
    
    Cov<-rep(NA,m+1)
    
    for (r in 0:m){
      s<-0
      for(i in 1:(n-r)){
        s<- s + xd[i]*xd[i+r]
      }
      
      Cov[r+1]<-s/(n-r)
    }
    
    
    # Aplicaci?n de la ventana
    
    R<-Cov*u
    
    #Calculo de espectro y su promedio
    #tengo que definir G
    Gsum<-0
    G<-rep(NA,m+1)
    for (k in 0:m){
      s=0
      for (r in 1:(m-1)){
        s<-s+R[r+1]*cos(pi*r*k/m)
      }
      G[k+1]<-2*(R[1]+2*s+R[m+1]*cos(pi*k))
      Gsum<-Gsum+G[k+1]
    }
    Gmed<-Gsum/(m+1)
    
    
    # Continuo nulo
    #poner corr como matriz
    cor<-cor(x[2:n],x[1:n-1])
    #r1<-cor[1,2]
    r1<-cor
    
    
    #TESTEO 
    test<-readline("Desea testear R1?(Necesario para graficar)(si/no): ")
    if(test=="si"){
      testeo<-as.numeric(readline("Testeo, una(1) o dos(2) colas: "))
      if(testeo==1){
        Rcritico=(-1+1.645*sqrt(length(Matriz[,1])-2))/(length(Matriz[,1])-1)
        if(r1>Rcritico){
          print("Ruido Rojo")
          ruido<-"rojo"
        } else {
          print("Ruido Blanco")
          ruido<-"blanco"
        }
      } else if(testeo==2){
        if(r1>0){
          Rcritico=(-1+1.96*sqrt(length(Matriz[,1])-2))/(length(Matriz[,1])-1)
          if(r1>Rcritico){
            print("Ruido Rojo")
            ruido<-"rojo"
        } else {
            print("Ruido Blanco")
            ruido<-"blanco"
        }
      } else if(r1<0){
          Rcritico=(-1-1.96*sqrt(length(Matriz[,1])-2))/(length(Matriz[,1])-1)
          if(r1>Rcritico){
            print("Ruido Rojo")
            ruido<-rojo
          } else {
            print("Ruido Blanco")
            ruido<-blanco
          }
        }
      }
    }
    
    
    #ruido rojo
    Cn<-rep(NA,m+1)
    Cnsup<-rep(NA,m+1)
    Cninf<-rep(NA,m+1)
  
    for(k in 0:m){
      Cn[k+1]<-Gmed*((1-r1^2)/(1+r1^2-2*r1*cos(pi*k/m)))
      Cnsup[k+1]<-Cn[k+1]*chisup/gl
      Cninf[k+1]<-Cn[k+1]*chiinf/gl
    }
    
    
    Cnn<-rep(NA,m+1)
    Cnnsup<-rep(NA,m+1)
    Cnninf<-rep(NA,m+1)
    
    #ruido blanco
    for (k in 0:m){
      Cnn[k+1]<-Gmed
      Cnnsup[k+1]<-Cnn[k+1]*chisup/gl
      Cnninf[k+1]<-Cnn[k+1]*chiinf/gl
    }
    
    # Grafica
    
    graficar<-as.numeric(readline("Guardar y graficar(1) o solo graficar(2)(Recuerde que si no testeo no podra realizar esta tarea): "))
    if(graficar==1){
      nombre<-readline("Nombre con el que se guardara la imagen: ")
      if(ruido=="rojo"){
        png(filename=paste(j,nombre,"_rojo",".png",sep=""), width=5500, height=4000, res=600)
        plot(f,G,type='l')
        lines(f,Cninf,col='blue')
        lines(f,Cn,col='red')
        lines(f,Cnsup,col='blue')
        dev.off()
        
        plot(f,G,type='l')
        lines(f,Cninf,col='blue')
        lines(f,Cn,col='red')
        lines(f,Cnsup,col='blue')
        
      } else if(ruido=="blanco"){
        
        png(filename=paste(j,nombre,"_blanco",".png",sep=""), width=5500, height=4000, res=600)
        plot(f,G,type='l')
        lines(f,Cnninf,col='blue')
        lines(f,Cnn,col='red')
        lines(f,Cnnsup,col='blue')
        dev.off()
        
        plot(f,G,type='l')
        lines(f,Cnninf,col='blue')
        lines(f,Cnn,col='red')
        lines(f,Cnnsup,col='blue')
      }
    } else { 
      if(ruido=="rojo"){
        plot(f,G,type='l')
        lines(f,Cninf,col='blue')
        lines(f,Cn,col='red')
        lines(f,Cnsup,col='blue')
        
      } else { plot(f,G,type='l')
        lines(f,Cninf,col='blue')
        lines(f,Cn,col='red')
        lines(f,Cnsup,col='blue')
        
      }
      
    }
    
    for (k in 0:m){
      result[k+1,4*j-2]<-G[k+1]
      result[k+1,4*j-1]<-Cninf[k+1]
      result[k+1,4*j]<-Cn[k+1]
      result[k+1,4*j+1]<-Cnsup[k+1]
    }
    
    # Valores significativos
    
    print('Valores significativos: f, 1/f, Cnsup, G')
    print('Serie n?: ',paste(j))
    
    for (k in 1:m){
      if (G[k+1]>Cnsup[k+1]){
        print(cbind(f[k+1],1/f[k+1],Cnsup[k+1],G[k+1]))
      }
    }
  
  }
  
  colnames(result)<-c('f','G','Cninf','Cn','Cnsup')
  result<-as.data.frame(result)
  
  if(ruido=="rojo"){
    guardar<-readline("Desea guardar la matriz result?(si/no): ")
    if(guardar=="si"){
      nom<-readline("Nombre del archivo .txt: ")
      write.table(result,paste(nom,".txt",sep=""))  
    } 
  } else if(ruido=="blanco"){
    print("no hay matriz para ruido blanco")
  }
  #NO GUARDA resultTADOS DE RUIDO BLANCO...
  
  ##si el espectro sigue un modelo de ruido blanco los resulttados de result no son los correctos
  
  freqsig<-result$f[result$G>=result$Cnsup]
  
  ret<-as.numeric(readline("Return periodos significativos para ruido rojo(1) o matriz reult(2): "))
  if(ret==1){
    return(1/freqsig)
  } else if(ret==2){
    return(result)
  }
}
