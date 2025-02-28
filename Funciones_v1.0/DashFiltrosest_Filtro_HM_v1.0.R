#' Filtro de Hamilton
#'
#' Filtro_HM realiza el filtro de Hamilton a las variables solicitadas y regresa una lista con los valores de la variable y su suavizamiento generada por el filtro.
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
#' @param h Un número entero, que define el período de anticipación. El valor predeterminado es h = 8.
#' @param p Un número entero, que indica el número de retrasos. Un valor predeterminado de p = 4.
#'
#' @return una lista que contiene un dataframe por cada variable, en dicho data frame esta la fecha, el valor observado y el suavizamiento por el filtro de Hamilton
#' @export Filtro_HM
#'
#'
#' @examples


DashFiltrosest_Filtro_HM_v1.0<-function(datos, variables=names(datos)[-1],inicio,freq=4,cor_col=FALSE,dat_cor=NULL, c=freq,h=8, p=4){
  # Manejar datos.
  library(dplyr)
  library(tidyverse)
  library(readxl) # Abrir excel
  library(zoo); library(lubridate) # Fechas
  library(reshape2)
  library(writexl)


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
  library(neverhpfilter) #libreria Hamilton



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

  ex_HM<-vector("list",length = length(variables))
  names(ex_HM)<-variables
  for (i in variables) {
    data<-datos[c("Fecha",i)]





    ####################Filtro HAMILTON #######################


      dta.ts<-as.xts(ts(data[i],start = inicio,frequency = freq, names=i))# se transforma a un objeto xts para la siguiente linea

      data_HM<-yth_filter(dta.ts, h = h, p = p, output = c("x", "trend"))[,paste0(i,".trend")] #Valores suavizados por el filtro HAMILTON


      data_HM<-as.data.frame(data_HM)
      colnames(data_HM)<-c("Filtro Hamilton")
      data<-cbind(data,data_HM)

      ex_HM[[i]]<-data
      
      print('--Filtro de HM se estimo de forma correcta--')


  }
  return(ex_HM)
}

#' datos<-as.data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA SEGUNDO SEMESTRE 2023/MARLON/FILTROS TCRE/Datos/202308_Filtros 1970.xlsx",range = "A1:B215"))
#' inicio=c(1970,1)
#' freq=4
#' variables<-names(datos)[-1]
#' cor_col<-TRUE
#' dat_cor<-as.data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA SEGUNDO SEMESTRE 2023/MARLON/FILTROS TCRE/Datos/202308_Filtros 1970.xlsx",range = "A216:B223",col_names = FALSE))
#'
#' Filtro_HM(datos = datos,
#'           variables=variables,
#'           inicio = inicio,
#'           freq = freq,
#'           cor_col=cor_col,
#'           dat_cor=dat_cor,
#'           c=NULL,
#'           h=8,
#'           p=4)