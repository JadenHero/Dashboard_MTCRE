#' Filtro de Descomposición estacional por Loess
#'
#' Filtro_STL realiza el filtro de Descomposición estacional por Loess a las variables solicitadas y regresa una lista con los valores de la variable y su suavizamiento generada por el filtro.
#'
#' @param datos Es un data frame en el cual se encuentran las series de tiempo.
#' @param variables Es un vector con los nombres de las variables a las cuales se le quiere realizar el suavizamiento.
#' @param inicio Es la fecha en la cual inicia la serie de tiempo su estructura es c(año,periodo).
#' @param freq Es la frecuencia de las series de tiempo,
#' Anual=1
#' Trimestral=4
#' Mensual=12
#' @param cor_col Booleano, si es TRUE realiza la corrección de colas ya sea usando nuevos datos o usando el ultimo valor como constante, si es FALSE usa solo los datos ingresados.
#' @param dat_cor Es un data frame con datos nuevos para realizar la correccion de colas, si es NULL se toma el ultimo valor como una constante.
#' @param c Es un entero el cual se usa cuando dat_cor=NULL y es el numero de periodos extra donde el ultimo valor de datos sera constante
#' @param t El intervalo (en retrasos) de la ventana de Loess para la extracción de tendencias, que debería ser impar.
#' @param s Ya sea la cadena de caracteres "periodic" o el lapso (en retrasos) de la ventana de loess para la extracción estacional que debe ser impar y al menos 7.
#'
#' @return Una lista que contiene un dataframe por cada variable, en dicho data frame esta la fecha, el valor observado y el suavizamiento por el Descomposición estacional por Loess
#' @export Filtro_STL
#'
#'
#'
#' @examples


DashFiltrosest_Filtro_STL_v1.0<-function(datos, variables=names(datos)[-1],inicio,freq=4,cor_col=FALSE,dat_cor=NULL, c=freq,t=NULL, s="periodic"){
  # Manejar datos.
  library(dplyr)
  library(tidyverse)
  library(readxl) # Abrir excel
  library(zoo); library(lubridate) # Fechas
  library(reshape2)
  library(writexl)
  library(readxl)
  library(lubridate)
  # library(plyr)

  # Graficas
  library(ggplot2)
  library(gridExtra)
  library(coefplot)



  # Series de tiempo
  library(forecast)
  library(mFilter) # filtro HP
  library(seasonal)
  library(fpp)

  # Test
  library(lmtest)
  library(tseries)
  library(stats)
  library(fBasics)
  library(neverhpfilter)




    ######Corrección de Colas de los datos #####
  if(cor_col==TRUE){
    if(is.null(dat_cor)==TRUE){
      if(freq==1){f=12}# anual
      if(freq==4){f=3}# trimestral
      if(freq==12){f=1}#mensual


      for (i in 1:c) {
        add<-cbind(max(datos[,1])%m+%months(f),tail(datos,1)[-1])
        names(add)<-names(datos)
        datos<-rbind(datos,add)
      }
    }else{
      datos<-rbind(datos,dat_cor)
    }
  }

  ex_STL<-vector("list",length = length(variables))
  names(ex_STL)<-variables
  for (i in variables) {
    data<-datos[c("Fecha",i)]



    ####################Filtro STL#######################


    dta.ts<-ts(as.numeric(as.matrix(data[i])),start = inicio,frequency = freq)


    data_STL<-(stl(dta.ts,t.window=t, s.window=s, robust=TRUE)$time.series)[,"trend"] #Valores suavizados por el filtro STL

    data_STL<-as.data.frame(data_STL)
    colnames(data_STL)<-"Filtro STL"
    data<-cbind(data,data_STL)

    ex_STL[[i]]<-data
    
    print('--Filtro de STL se estimo de forma correcta--')


  }
  return(ex_STL)
}
