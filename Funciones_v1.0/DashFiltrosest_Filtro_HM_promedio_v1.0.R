#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# DashFiltrosest_Filtro_HM_promedio: Filtro de Hamilton promedio 
# update: 2/05/2024
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' DashFiltrosest_Filtro_HM_promedio
#' 
#' Calcula la tendencia del filtro de Hamilton modificado, el cual hace referencia al promedio de la tendencia del filtro de Hamilton para un rango de valores de h (horizonte de pronostico)
#'
#' @param datos Es un data frame en el cual se encuentran las series de tiempo.
#' @param variables Es un vector con los nombres de las variables a las cuales se le quiere realizar el suavizamiento.
#' @param inicio Es la fecha en la cual inicia la serie de tiempo su estructura es c(año, periodo).
#' @param freq Es la frecuencia de las series de tiempo,
#' Anual=1
#' Trimestral=4
#' Mensual=12 
#' @param cor_col Booleano, si es TRUE realiza la corrección de colas ya sea usando nuevos datos o usando el ultimo valor como constante, si es FALSE usa solo los datos ingresados.
#' @param dat_cor Es un data frame con datos nuevos para realizar la correccion de colas, si es NULL se toma el ultimo valor como una constante.
#' @param c Es un entero el cual se usa cuando cor_col=TRUE y dat_cor=NULL y es el numero de periodos extra donde el ultimo valor de datos sera constante
#' @param lh Es un entero positivo el cual indica el limite inferio del rango de valores que tomara h
#' @param uh Es un entero positivo el cual indica el limite superior del rango de valores que tomara h
#' @param p Es un entero positivo el cual indica el numero de rezagos que se van a considerar
#'
#' @return Una lista que contiene un dataframe por cada variable, en dicho data frame esta la fecha, el valor observado y el suavizamiento por el filtro de Hamilton promedio
#' @export
#'
#' @examples


DashFiltrosest_Filtro_HM_promedio_v1.0 <- function(datos, variables=names(datos)[-1], inicio, freq=4, cor_col=FALSE, dat_cor=NULL,c=freq,lh=4, uh=12, p=4){
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
  
   #Verificar que lh < uh
   #if(lh >= uh){
   #   stop('lh debe ser menor a uh ( lh < uh)')
   #}
  

  ###### Corrección de Colas de los datos #######
  if(cor_col==TRUE){
    if(is.null(dat_cor)==TRUE){
      if(freq==1){f=12}# anual
      if(freq==4){f=3}# trimestral
      if(freq==12){f=1}#mensual
      
      for(i in 1:c){
        add<-cbind(max(datos[,1])%m+%months(f),tail(datos,1)[-1]) # si sirve desde que la Fecha sea tipo "Date"
        names(add)<-names(datos)
        datos<-rbind(datos,add)
      }
    } else{
      datos<-rbind(datos,dat_cor)
    }
  }
  
  ex_HM_promedio<-vector("list",length = length(variables))
  names(ex_HM_promedio)<-variables
  
  #i = "ITCR-IPC Trimestral"
  for(i in variables){
    
    data <- datos[c("Fecha",i)]
    dta.xts <- as.xts(ts(data[i], start=inicio, frequency=freq, names=i))# se transforma a un objeto xts
    
    save_data_HM <- NULL # Crear objeto nulo donde se guardan los filtro de HM para diferentes h
    
    ######### Filtro HAMILTON PROMEDIO ################
    for(h in lh:uh){
      data_HM <- yth_filter(dta.xts, h = h, p = p, output = c("x", "trend"))[,paste0(i,".trend")] #Valores suavizados por el filtro HAMILTON
      save_data_HM <- cbind(save_data_HM, as.numeric(data_HM))
      }

    data_HM_promedio <- rowMeans(save_data_HM)
    
    data_HM_promedio <- as.data.frame(data_HM_promedio)
    names(data_HM_promedio) <- c("Filtro Hamilton promedio")
    data <- cbind(data, data_HM_promedio)
    
    ex_HM_promedio[[i]] <- data
    
    print('--Filtro de HM Promedio se estimo de forma correcta--')
    
  } # Fin iterador sobre las variables
  
  # OUPUT
  return(ex_HM_promedio)

} # Fin de la funcion


# Ejemplo y pruebas -------------------------------------------------------

# datos_prueba <- read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA PRIMER SEMESTRE 2024/TCRE/Shiny/Shiny_TCRE/Dashboard_MTCRE/v0.5/Datos/prueba_filtros.xlsx")
# datos_prueba[['Fecha']]<-as.Date(datos_prueba[['Fecha']],format='%Y-%m-%d')
# str(datos_prueba)
# 
# fin <- c(2024,1)
# fecha_corr <- c(2024,2)
# trimestre_number_p<-list('1'='03',
#                          '2'='06',
#                          '3'='09',
#                          '4'='12')
# datos <- datos_prueba   %>% filter(Fecha <= paste0(fin[1],'-',trimestre_number_p[[fin[2]]],'-01') )
# dat_cor <- datos_prueba %>% filter(Fecha >= paste0(fecha_corr[1],'-',trimestre_number_p[[fecha_corr[2]]],'-01'))
# variables=names(datos)[-1]
# # Seleccionando la fecha de inicio de los datos
# inicio_date<-as.yearqtr(as.Date(datos[,1][[1]],format="%Y-%m-%d"))[1]
# inicio<-c(year(inicio_date),quarter(inicio_date))
# 
# freq = 4
# cor_col = T
# c = 4
# lh = 4 ; uh = 12 ; p=4
