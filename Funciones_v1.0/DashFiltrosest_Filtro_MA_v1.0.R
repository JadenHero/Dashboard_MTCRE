#' Filtro de Promedios moviles
#'
#' Filtro_MA realiza el filtro de Promedios moviles a las variables solicitadas y regresa una lista con los valores de la variable y su suavizamiento generada por el filtro.
#'
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
#' @param w Es un entero el cual hace referencia a la ventana que se desea tener en cuenta al evaluar si la serie es aditiva o multiplicativa
#'
#' @return una lista que contiene un dataframe por cada variable, en dicho data frame esta la fecha, el valor observado y el suavizamiento por el filtro de Promedios moviles
#' @export Filtro_MA
#'
#'
#' @examples

DashFiltrosest_Filtro_MA_v1.0<-function(datos, variables=names(datos)[-1],inicio,ma_order=4,cor_col=FALSE,dat_cor=NULL, c=ma_order){

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

  ex_MA<-vector("list",length = length(variables))
names(ex_MA)<-variables
  for (i in variables) {
    data<-datos[c("Fecha",i)]

  
  
    ####################Filtro de Promedios moviles#######################


      adi.mul <-function(x,n){# Funcion interna para evaluar si la serie es aditiva o multiplicativa
        if(class(x) == 'ts' & class(n) == 'numeric'){
          d = x - stats::lag(x,n)
          c = x/stats::lag(x,n)
          cv_d = abs(sd(d,na.rm = T)/mean(d,na.rm = T))
          cv_c = abs(sd(c,na.rm = T)/mean(c,na.rm = T))
          #print(d);print(c);print(paste('cv(d)',cv_d));print(paste('cv(c)',cv_c))
          if(cv_c < cv_d){ # condicional para la elección de tipo de serie de tiempo
            print(paste('La serie ',i,' es multiplicativa con cv(c) =', cv_c))

            tipo="multiplicative"}
          else{print(paste('La serie ',i,' es aditiva con cv =',cv_d))

            tipo="additive"}
          return(tipo)

        }else{print('Los argumentos de la funcion deben ser ts y numerico respectivamente')}

      }



      dta.ts<-ts(data[i],start = inicio,frequency = ma_order, names=i)

      type<-adi.mul(dta.ts,ma_order) # se guarda el tipo de serie de tiempo  para el filtro

      data_ma<-as.data.frame(decompose(dta.ts,type=type)$trend) #Valores suavizados por el filtro de media movil

      names(data_ma)<-paste0("Filtro Promedio Móvil")

      data<-cbind(data,data_ma)

      ex_MA[[i]]<-data
      
      print('--El filtro de MA se estimo correctamente--')


  }
  return(ex_MA)
  }



#' datos<-as.data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA SEGUNDO SEMESTRE 2023/MARLON/FILTROS TCRE/Datos/202308_Filtros 1970.xlsx",range = "A1:B215"))
#' inicio=c(1970,1)
#' freq=4
#' variables<-names(datos)[-1]
#' cor_col<-TRUE
#' dat_cor<-as.data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA SEGUNDO SEMESTRE 2023/MARLON/FILTROS TCRE/Datos/202308_Filtros 1970.xlsx",range = "A216:B223",col_names = FALSE))
#'
#' Filtro_MA(datos = datos,
#'           variables=variables,
#'           inicio = inicio,
#'           freq = freq,
#'           cor_col=cor_col,
#'           dat_cor=dat_cor,
#'           c=NULL,
#'           w=freq)
