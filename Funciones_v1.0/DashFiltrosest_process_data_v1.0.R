
#' DashFiltrosest_process_data
#'
#'La función permite estimar los filtros estadisticos, procesando los datos para obtener una lista con una base de datos de equilibrios y otra base de datos de desequilibrios
#'
#' @param datos Base de datos, cuya primera columna corresponde a la fecha, y la segunda columna corresponde a la variable a la que se le aplicara el filtro
#' @param variables Nombre de la variable a la que se le debe aplicar el filtro
#' @param inicio Vector con el año y el mes/trimestre de la variable
#' @param freq Frecuencia de la variable a la que se le aplicaria el filtro
#' @param cor_col TRUE en caso de que se le realice la correción de colas al filtro, FALSE en caso de que no se le realice la correción de colas al filtro
#' @param dat_cor Base de datos que se utilizara para corregir las colas del filtro.
#' @param filtros_series Vector con las posibles opciones de los filtros que se deben aplicar: "HP"= Hodrick Prescott, "MA"=MA,"STL"=STL,"BK"=BK,"CF"=CF,"BW"=BW,"TRI"=TRI,"BN"=BN
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 

DashFiltrosest_process_data_v1.0<-function(datos,variables,fin,freq,
                                           cor_col,fecha_corr,filtros_series,list_param=list('Filtro Promedio Móvil'           = list(ma_order=12),
                                                                                             'Filtro Hodrick y Prescott'       = list(freq=4),
                                                                                             'Filtro STL'                      = list(t=13,s=7),
                                                                                             #'Filtro Hamilton'                 = list(h=8,p=4),
                                                                                             'Filtro Baxter-King'              = list(pl=2,pu=40,nfix=4),
                                                                                             'Filtro Christiano-Fitzgerald'    = list(pl=2,pu=40,theta=1),
                                                                                             'Filtro Butterworth'              = list(nfix = NULL),
                                                                                             'Filtro Regresión Trigonométrica' = list(pl=2,pu=40),
                                                                                             #'Filtro Beveridge-Nelson'         = list(nlag=2),
                                                                                             'Filtro Hodrick y Prescott a una cola' = list(freq=4),
                                                                                             'Filtro Hamilton promedio' = list(lh=4, uh=12, p=4))){
  
  datos[['Fecha']]<-as.Date(datos[['Fecha']],format='%Y-%m-%d')
  
  # Definiendo lista que permite identificar la fecha en meses con base en el trimestre seleccionado
  trimestre_number_p<-list('1'='03',
                           '2'='06',
                           '3'='09',
                           '4'='12')
  
  
  # Seleccionando los datos con base en la fecha final
  datos_int<-datos %>% filter(Fecha <= paste0(fin[1],'-',trimestre_number_p[[fin[2]]],'-01') )
  
  # Seleccionando los datos a partir de la fecha desde donde se iniciara la corrección de colas
  datos_correccion<-datos %>% filter(Fecha >= paste0(fecha_corr[1],'-',trimestre_number_p[[fecha_corr[2]]],'-01'))
  
  # Seleccionando la fecha de inicio de los datos
  inicio_date<-as.yearqtr(as.Date(datos[,1][[1]],format="%Y-%m-%d"))[1]
  inicio<-c(year(inicio_date),quarter(inicio_date))
  
  if(paste0(fin[1],'-',trimestre_number_p[[fin[2]]],'-01') < paste0(fecha_corr[1],'-',trimestre_number_p[[fecha_corr[2]]],'-01')){
  
  #------------------------------------------
  # Filtros estadisticos---------------------
  #------------------------------------------
  
  
  
    # list_param<- list('Filtro Promedio Movil'           = list(w=4),
    #                   'Filtro Hodrick y Prescott'       = list(freq=4),
    #                   'Filtro STL'                      = list(t=13,s=7),
    #                   'Filtro Hamilton'                 = list(h=8,p=4),
    #                   'Filtro Baxter-King'              = list(pl=2,pu=40,nfix=4),
    #                   'Filtro Christiano-Fitzgerald'    = list(pl=2,pu=40,theta=1),
    #                   'Filtro Butterworth'              = list(nfix = NULL),
    #                   'Filtro Regresión Trigonometrica' = list(pl=2,pu=40),
    #                   'Filtro Beveridge-Nelson'         = list(nlag=2))
    
    
    
      
      
      # ##### FILTRO PROMEDIO MOVIL #####
      MA<-DashFiltrosest_Filtro_MA_v1.0(datos = datos_int,
                                   variables=variables,
                                   inicio = inicio,
                                   cor_col=cor_col,
                                   dat_cor=datos_correccion,
                                   c=NULL,
                                   ma_order=list_param[['Filtro Promedio Móvil']][['ma_order']])
      
      # ##### FILTRO Hodrick y Prescott #####
      HP<-DashFiltrosest_Filtro_HP_v1.0(datos = datos_int,
                                   variables=variables,
                                   inicio = inicio,
                                   freq = list_param[['Filtro Hodrick y Prescott']][['freq']],
                                   cor_col=cor_col,
                                   dat_cor=datos_correccion,
                                   c=NULL)
      
      
      ##### FILTRO STL ####
      STL<-DashFiltrosest_Filtro_STL_v1.0(datos = datos_int,
                                     variables=variables,
                                     inicio = inicio,
                                     freq = freq,
                                     cor_col=cor_col,
                                     dat_cor=datos_correccion,
                                     c=NULL,
                                     t=list_param[['Filtro STL']][['t']],
                                     s=list_param[['Filtro STL']][['s']])
      
      
      ##### FILTRO Hamilton ####
      # HM<-DashFiltrosest_Filtro_HM_v1.0(datos = datos_int,
      #                              variables=variables,
      #                              inicio = inicio,
      #                              freq = freq,
      #                              cor_col=cor_col,
      #                              dat_cor=datos_correccion,
      #                              c=NULL,
      #                              h=list_param[['Filtro Hamilton']][['h']],
      #                              p=list_param[['Filtro Hamilton']][['p']])
      
      
      ##### FILTRO Baxter-King ####
      BK<-DashFiltrosest_Filtro_BK_v1.0(datos = datos_int,
                                   variables=variables,
                                   inicio = inicio,
                                   freq = freq,
                                   cor_col=cor_col,
                                   dat_cor=datos_correccion,
                                   c=NULL,
                                   pl=list_param[['Filtro Baxter-King']][['pl']],
                                   pu=list_param[['Filtro Baxter-King']][['pu']],
                                   nfix = list_param[['Filtro Baxter-King']][['nfix']])
      
      
      
      ##### FILTRO Christiano-Fitzgerald ####
      CF<-DashFiltrosest_Filtro_CF_v1.0(datos = datos_int,
                                   variables=variables,
                                   inicio = inicio,
                                   freq = freq,
                                   cor_col=cor_col,
                                   dat_cor=datos_correccion,
                                   c=NULL,
                                   pl=list_param[['Filtro Christiano-Fitzgerald']][['pl']],
                                   pu=list_param[['Filtro Christiano-Fitzgerald']][['pu']],
                                   theta=list_param[['Filtro Christiano-Fitzgerald']][['theta']])
      
      #### FILTRO Butterworth ####
      BW<-DashFiltrosest_Filtro_BW_v1.0(datos = datos_int,
                                   variables=variables,
                                   inicio = inicio,
                                   freq = freq,
                                   cor_col=cor_col,
                                   dat_cor=datos_correccion,
                                   c=NULL,
                                   nfix = list_param[['Filtro Butterworth']][['nfix']])
      
      #### FILTRO Regresion trigonometrica ####
      TRI<-DashFiltrosest_Filtro_TRI_v1.0(datos = datos_int,
                                     variables=variables,
                                     inicio = inicio,
                                     freq = freq,
                                     cor_col=cor_col,
                                     dat_cor=datos_correccion,
                                     c=NULL,
                                     pl=list_param[['Filtro Regresión Trigonométrica']][['pl']],
                                     pu=list_param[['Filtro Regresión Trigonométrica']][['pu']])
      
      #### FILTRO Beveridge-Nelson ####
      # BN<-DashFiltrosest_Filtro_BN(datos = datos_int,
      #                              variables=variables,
      #                              inicio = inicio,
      #                              freq = freq,
      #                              cor_col=cor_col,
      #                              dat_cor=datos_correccion,
      #                              c=NULL,
      #                              nlag=list_param[['Filtro Beveridge-Nelson']][['nlag']])
      
      ###### FILTRO Hodrick y Prescott a una cola #####
      HP_una_cola<-DashFiltrosest_Filtro_HP_una_cola_v1.0(datos = datos_int,
                                                     variables=variables,
                                                     inicio = inicio,
                                                     freq = list_param[['Filtro Hodrick y Prescott a una cola']][['freq']],
                                                     cor_col=cor_col,
                                                     dat_cor=datos_correccion,
                                                     c=NULL)
      
      ##### FILTRO Hamilton Promedio ####
      HM_promedio<-DashFiltrosest_Filtro_HM_promedio_v1.0(datos = datos_int,
                                                     variables=variables,
                                                     inicio = inicio,
                                                     freq = freq,
                                                     cor_col=cor_col,
                                                     dat_cor=datos_correccion,
                                                     c=NULL,
                                                     lh = list_param[['Filtro Hamilton promedio']][['lh']],
                                                     uh = list_param[['Filtro Hamilton promedio']][['uh']],
                                                     p  = list_param[['Filtro Hamilton promedio']][['p']])
                         
    
    
      
      
  ##### Desequilibrios y graficos #####
   datos_filtrados<-list(#'Filtro Hamilton'=HM,
                         'Filtro Hodrick y Prescott'= HP,
                         'Filtro Promedio Móvil'=MA,
                         'Filtro STL'=STL,
                         'Filtro Baxter-King'=BK,
                         'Filtro Christiano-Fitzgerald'=CF,
                         'Filtro Butterworth'=BW,
                         'Filtro Regresión Trigonométrica'=TRI,
                         #'Filtro Beveridge-Nelson'=BN,
                         'Filtro Hodrick y Prescott a una cola'= HP_una_cola,
                         'Filtro Hamilton promedio' = HM_promedio)
  
 
  
  ###### Obteniendo los equilibrios y desequilibrios a partir de los filtros de interes. #####
  list_filtros<-DashFiltrosest_Filtro_var_datos_v1.0(datos_filtrados=datos_filtrados[filtros_series],
                                 variables=variables,
                                 inicio=inicio,
                                 fin=fin,
                                 freq=freq)
   
   # Incluyendo vista preliminar de la base de datos
   list_filtros[['Vista datos']] <- rbind(datos_int,datos_correccion)
   
   
   print('--Se calcularon los equilibrios y desequilibrios de los filtros correctamente--')
  }else{
    list_filtros<-list("Data Equilibrios"='',
                       "Data Desequilibrios"='')
  }
   
   
  return(list_filtros)
}



# Ejemplo y prueba --------------------------------------------------------

# datos_prueba<-read_excel("C:/Users/msalazsi/OneDrive - Banco de la República/Desktop/PMMS/MTCRE/Desarrollo/Dashboard/prueba.xlsx", col_names = TRUE)datos_prueba <- read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA PRIMER SEMESTRE 2024/TCRE/Shiny/Shiny_TCRE/Dashboard_MTCRE/v0.5/Datos/prueba_filtros.xlsx")
# datos <- datos_prueba
# variables <- variables<-names(datos_prueba)[-1]
# fin <- c(2024,1)
# freq <- 4
# cor_col <- T
# fecha_corr <- c(2024,2)
# filtros_series <- c('Filtro Hodrick y Prescott',
#                     'Filtro Hamilton',
#                     'Filtro Promedio Movil',
#                     'Filtro STL',
#                     'Filtro Baxter-King',
#                     'Filtro Christiano-Fitzgerald',
#                     'Filtro Butterworth',
#                     'Filtro Regresión Trigonometrica',
#                     #'Filtro Beveridge-Nelson',
#                     'Filtro Hodrick y Prescott a una cola',
#                     'Filtro Hamilton promedio')
# list_param=list('Filtro Promedio Movil'           = list(ma_order=12),
#                 'Filtro Hodrick y Prescott'       = list(freq=4),
#                 'Filtro STL'                      = list(t=13,s=7),
#                 'Filtro Hamilton'                 = list(h=8,p=4),
#                 'Filtro Baxter-King'              = list(pl=2,pu=40,nfix=4),
#                 'Filtro Christiano-Fitzgerald'    = list(pl=2,pu=40,theta=1),
#                 'Filtro Butterworth'              = list(nfix = NULL),
#                 'Filtro Regresión Trigonometrica' = list(pl=2,pu=40),
#                 #'Filtro Beveridge-Nelson'         = list(nlag=2),
#                 'Filtro Hodrick y Prescott a una cola' = list(freq=4),
#                 'Filtro Hamilton promedio' = list(lh=4, uh=12, p=4))

# A <- DashFiltrosest_process_datav_0.2(datos, variables, fin, freq, cor_col,fecha_corr,filtros_series, list_param)
# View(A$`Data Equilibrios`)
# View(A$`Data Desequilibrios`)
# View(A$`Vista datos`)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Pruebas y comentarios --------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# # Librerias
# load.lib<-c("readxl","openxlsx","purrr","reshape2","ggplot2","plyr","scales","plotly", "htmlwidgets","metR","crosstalk")
# install.lib<-load.lib[!load.lib %in% installed.packages()]
# for(lib in install.lib) install.packages(lib)
# sapply(load.lib,require,character=TRUE)
# #library("Filtro")
# library("readxl")
# library(reshape2)
# library(dplyr)
# 
# Funciones
# wf <- 'C:/Users/msalazsi/OneDrive - Banco de la República/Desktop/PMMS/MTCRE/Desarrollo/Dashboard/script/'
# file_names<-list.files(wf, full.names=F, recursive = F)
# for(f in file_names){
#   print(f)
#   source(paste0('C:/Users/msalazsi/OneDrive - Banco de la República/Desktop/PMMS/MTCRE/Desarrollo/Dashboard/script/',f))}

# # Base de datos
# ruta_principal<-"C:/Users/msalazsi/OneDrive - Banco de la República/Desktop/PMMS/MTCRE/Desarrollo/Filtros TCRE/202403"
# setwd(ruta_principal)
# datos<-as.data.frame(read_excel("202403_Filtros 1970.xlsx",range = "A1:B218",sheet='Equilibrios itcr_ipc'))
# dat_cor<-as.data.frame(read_excel("202403_Filtros 1970.xlsx",range = "A219:B225",sheet='Equilibrios itcr_ipc',col_names = FALSE)) # datos para la correccion de colas, si no se tienen poner NULL
# names(dat_cor)<-names(datos)

# variables=variables<-names(datos)[-1]
# inicio=c(1970,1)
# fin=c(2024,1)
# freq=4
# cor_col = T
# dat_cor = dat_cor
# filtros_series = c()

# datos=datos
# variables=variables<-names(datos)[-1]
# inicio=c(1970,1)
# fin=c(2024,1)
# freq=4
# cor_col = T
# dat_cor = dat_cor
# filtros_series = c('Filtro Hodrick y Prescott',
#                    'Filtro Hamilton',
#                    'Filtro Promedio Movil',
#                    'Filtro STL',
#                    'Filtro Baxter-King',
#                    'Filtro Christiano-Fitzgerald',
#                    'Filtro Butterworth',
#                    'Filtro Regresión Trigonometrica',
#                    'Filtro Beveridge-Nelson')
#filtros_series = c('Filtro Hodrick y Prescott')
#fecha_dist = '2024-03-01'

# Load the future package
#library(future)
# Define the plan for parallel processing
#startT<-Sys.time()
# plan(multicore)
# #plan(multisession)
# 
# 
# # Execute both functions simultaneously
#  result <- future::future({
#   ##### FILTRO PROMEDIO MOVIL #####
#   ex_MA <- Filtro_MA(datos = datos,
#                     variables=variables,
#                     inicio = inicio,
#                     freq = freq,
#                     cor_col=cor_col,
#                     dat_cor=dat_cor,
#                     c=NULL,
#                     w=freq)
#   ##### FILTRO Hodrick y Prescott #####
#   ex_HP <- Filtro_HP(datos = datos,
#                         variables=variables,
#                         inicio = inicio,
#                         freq = freq,
#                         cor_col=cor_col,
#                         dat_cor=dat_cor,
#                         c=NULL)
#   ##### FILTRO STL ####
#   ex_STL<-Filtro_STL(datos = datos,
#              variables=variables,
#              inicio = inicio,
#              freq = freq,
#              cor_col=cor_col,
#              dat_cor=dat_cor,
#              c=NULL,
#              t=13,
#              s=7)
#   ##### FILTRO Hamilton ####
#   ex_HM<-Filtro_HM(datos = datos,
#             variables=variables,
#             inicio = inicio,
#             freq = freq,
#             cor_col=cor_col,
#             dat_cor=dat_cor,
#             c=NULL,
#             h=8,
#             p=4)
#   ##### FILTRO Baxter-King ####
#   ex_BK<-Filtro_BK(datos = datos,
#                    variables=variables,
#                    inicio = inicio,
#                    freq = freq,
#                    cor_col=cor_col,
#                    dat_cor=dat_cor,
#                    c=NULL,
#                    pl=2,
#                    pu=40,
#                    nfix = 4)
#   ##### FILTRO Christiano-Fitzgerald ####
#   ex_CF<-Filtro_CF(datos = datos,
#             variables=variables,
#             inicio = inicio,
#             freq = freq,
#             cor_col=cor_col,
#             dat_cor=dat_cor,
#             c=NULL,
#             pl=2,
#             pu=40,
#             theta=1)
#   ##### FILTRO Butterworth ####
#   ex_BW<-Filtro_BW(datos = datos,
#             variables=variables,
#             inicio = inicio,
#             freq = freq,
#             cor_col=cor_col,
#             dat_cor=dat_cor,
#             c=NULL)
#   ##### FILTRO Regresion trigonometrica ####
#   ex_TRI<-Filtro_TRI(datos = datos,
#              variables=variables,
#              inicio = inicio,
#              freq = freq,
#              cor_col=cor_col,
#              dat_cor=dat_cor,
#              c=NULL,
#              pl=2,
#              pu=40)
#   ##### FILTRO Beveridge-Nelson ####
#   ex_BN<-Filtro_BN(datos = datos,
#             variables=variables,
#             inicio = inicio,
#             freq = freq,
#             cor_col=cor_col,
#             dat_cor=dat_cor,
#             c=NULL,
#             nlag=2)
# 
# 
#   list(ex_MA = ex_MA, ex_HP = ex_HP,ex_STL=ex_STL,ex_HM=ex_HM,ex_BK=ex_BK,
#        ex_CF=ex_CF,ex_BW=ex_BW,ex_TRI=ex_TRI,ex_BN=ex_BN)
# })


# datos_filtrados<-list("HM"=future::value(result)$ex_HM,
#                       "HP"= future::value(result)$ex_HP,
#                       "MA"=future::value(result)$ex_MA,
#                       "STL"=future::value(result)$ex_STL,
#                       "BK"=future::value(result)$ex_BK,
#                       "CF"=future::value(result)$ex_CF,
#                       "BW"=future::value(result)$ex_BW,
#                       "TRI"=future::value(result)$ex_TRI,
#                       "BN"=future::value(result)$ex_BN)
#endT<-Sys.time()

#print(endT-startT)
###### Seleccionando los filtros de interes #####
# filtros_series<-c("HP","MA","STL","BK",
#                   "CF","BW","TRI")
# 
#filtros_series <-c()
# if(length(filtros_series)==0){
#   filtros_interes = datos
# }else{
#   filtros_interes<-datos_filtrados[filtros_series]
#   
# }
# 
#   #----------------------------------------------------------------------------------
#   ###### Realizando las graficas de los equilibrios, desequilibrios y fancharts #####
#   #----------------------------------------------------------------------------------
#   
#   #------------------------------------------
#   #Graficas en caso de no seleccionar filtros
#   #------------------------------------------
#   
#   if(length(filtros_series)==0){
#     # Base de datos del ITCR
#     data_itcr<-list_filtros[["Data ITCR"]]
#     
#     #--------------------------
#     # Graficas Equilibrios-----
#     #--------------------------
#     
#     # Creando el grafico de interes.
#     plot_itcr<-plot_ly(data_itcr, x = ~Fecha, y = data_itcr[['ITCR-IPC Trimestral']], name = 'ITCR-IPC Trimestral', 
#                        type = 'scatter', mode = 'lines',line = list(color = 'blue') ) %>%
#       layout(title = 'ITCR-IPC Trimestral',
#              xaxis = list(title = ""),
#              yaxis = list(title = ""),
#              legend = list(orientation = 'h'))
#     
#     
#     #------------------------------
#     # Graficas Desequilibrios------
#     #------------------------------
#     plot_empty<-plotly_empty(type = 'scatter', mode = 'lines')
#     
#     
#     #-------------------------------------
#     # Graficas fanchart equilibrios. 
#     #-------------------------------------
#     
#     # Mostrando Grafico vacio
#     
#     #-------------------------------------
#     # Graficas fanchart desequilibrios. 
#     #-------------------------------------
#     
#     # Mostrando Grafico vacio
#     
#     #-------------------------------------
#     # Graficas distribución. 
#     #-------------------------------------
#     
#     # Mostrando Grafico vacio
#     
#     #-------------------------------------
#     # Graficas mapa de calor 
#     #-------------------------------------
#     
#     
#     #------------------------------
#     # Guardando los graficos-------
#     #------------------------------
#     save_plots<-list('Plot Equilibrios'=plot_itcr,
#                      'Plot Desequilibrios'=plot_empty,
#                      'Plot Fanchart Equilibrios'=plot_empty,
#                      'Plot Fanchart Desequilibrios'=plot_empty,
#                      'Plot Distribucion'=plot_empty,
#                      'Plot Mapa Calor'=plot_empty)
#     
#     
#     return(save_plots)
#     
#   }else{
#     
#     #------------------------------------------
#     #Graficas en caso de seleccionar filtros
#     #------------------------------------------
#     
#     # Base de datos de equilibrio y desequilibrio
#     data<-list_filtros[["Data Equilibrios"]]
#     datades<-list_filtros[["Data Desequilibrios"]]
#     
#     #--------------------------
#     # Graficas Equilibrios-----
#     #--------------------------
#     
#     # Definiendo paleta de colores.
#     colores <- brewer.pal(length(names(data)),"Paired")
#     names(colores) <- names(data)
#     colores["ITCR-IPC Trimestral"]<-'blue'
#     
#     # Procesando la base de datos para realizar las graficas-------
#     plot_data<-data %>% filter(Fecha >= '1973-03-01') %>% melt(id = 1)
#     dta_median<-plot_data %>%tidyr::spread(key=variable,value=value)
#     
#     # Definiendo la mediana de los datos seleccionados-------
#     if(length(filtros_series)==1){
#       dta_median[['Mediana']]<-dta_median[[filtros_series[1]]]
#     }else{
#       dta_median[['Mediana']]<-apply(dta_median[,-1],1,median,na.rm=T)
#     }
#     dta_median <- dta_median %>% select(Fecha,Mediana)
#     
#     # Graficando los equilibrios de los filtros estadisticos-------
#     plot_equ<-plotly::plot_ly(plot_data, x = ~Fecha, y = ~value, color = ~variable,
#                               colors=colores, 
#                               type = 'scatter', mode = 'lines') %>%
#       plotly::add_trace(x = dta_median[['Fecha']], y = dta_median[['Mediana']],
#                         type='scatter',mode='lines', color='Mediana', line = list(color = 'black', dash = 'dot') ) %>%
#       layout(title = "ITCR-IPC y Tendencia Filtros Estadísticos",
#              xaxis = list(title = ""),
#              yaxis = list(title = ""),
#              legend = list(orientation = 'h'))
#     
#     #------------------------------
#     # Graficas Desequilibrios------
#     #------------------------------
#     
#     # Definiendo paleta de colores.
#     colores <- brewer.pal(length(names(data)),"Paired")
#     names(colores) <- names(data)
#     colores["ITCR-IPC Trimestral"]<-'blue'
#     
#     # Procesando la base de datos para realizar las graficas-------
#     plot_datades<- datades %>% dplyr::filter(Fecha >= '1973-03-01') %>% melt(id = 1)
#     dta_mediandes<-plot_datades %>%tidyr::spread(key=variable,value=value)
#     
#     # Definiendo la mediana de los datos seleccionados-------
#     if(length(filtros_series)==1){
#       dta_mediandes[['Mediana']]<-dta_mediandes[[filtros_series[1]]]
#     }else{
#       dta_mediandes[['Mediana']]<-apply(dta_mediandes[,-1],1,median,na.rm=T)
#     }
#     dta_mediandes <- dta_mediandes %>% select(Fecha,Mediana)
#     
#     # Graficando los equilibrios de los filtros estadisticos-------
#     plot_desequ<-plotly::plot_ly(plot_datades, x = ~Fecha, y = ~value, color = ~variable,
#                                  colors=colores, 
#                                  type = 'scatter', mode = 'lines') %>%
#       plotly::add_trace(x = dta_mediandes[['Fecha']], y = dta_mediandes[['Mediana']],
#                         type='scatter',mode='lines', color='Mediana', line = list(color = 'black', dash = 'dot') ) %>%
#       layout(title = "ITCR-IPC y Desalineamiento Filtros Estadísticos",
#              xaxis = list(title = ""),
#              yaxis = list(title = ""),
#              legend = list(orientation = 'h'))
#     
#     
#     #-------------------------------------
#     # Graficas fanchart equilibrios. 
#     #-------------------------------------
#     # Fanchart de los equilibrios
#     observada_dta<-list_filtros[["Data Equilibrios"]]%>%filter(Fecha>='1973-03-01')
#     observada_dta<-observada_dta[,"ITCR-IPC Trimestral"]
#     
#     plot_fanchart_equ<-fanchart_dashplot(datos=list_filtros[["Data Equilibrios"]]%>%
#                                            filter(Fecha >= '1973-03-01') %>% select(!c("ITCR-IPC Trimestral")) ,
#                                          observada<- observada_dta,
#                                          lim_probs<-c(0.1,0.9),
#                                          start<-c(1973,1),
#                                          start_zoom=c(2019,1),
#                                          end<-fin,
#                                          freq<-freq,
#                                          nombre<-'Equilibrios filtros ITCR-IPC Trimestral')
#     
#     #-------------------------------------
#     # Graficas fanchart desequilibrios. 
#     #-------------------------------------
#     # Fanchart de los desequilibrios
#     plot_fanchart_desequ<-fanchart_dashplot(datos=list_filtros[["Data Desequilibrios"]] %>%
#                                               filter(Fecha >= '1973-03-01') ,
#                                             observada<-NULL,
#                                             lim_probs<-c(0.1,0.9),
#                                             start<-c(1973,1),
#                                             start_zoom=c(2019,1),
#                                             end<-fin,
#                                             freq<-freq,
#                                             nombre<-'Desequilibrios prueba filtros ITCR-IPC Trimestral')
#     
#     
#     #-------------------------------------
#     # Graficas distribución. 
#     #-------------------------------------
#     
#     plot_distribucion_filtros<-grafica_distribucion_filtros(datos=list_filtros[["Data Equilibrios"]]%>%
#                                                               filter(Fecha >= '1973-03-01') %>% select(!c("ITCR-IPC Trimestral")),
#                                                             observada = list_filtros[["Data Equilibrios"]]%>%select(c("Fecha","ITCR-IPC Trimestral")),
#                                                             fecha = fecha_dist,
#                                                             start = datos[['Fecha']][1])
#     
#     #-------------------------------------
#     # Graficas mapa de calor 
#     #-------------------------------------
#     plot_mapacalor<-heatmap_plot(datos=list_filtros[["Data Desequilibrios"]],n_ultimos=8)
#     
#     #------------------------------
#     # Guardando los graficos-------
#     #------------------------------
#     save_plots<-list('Plot Equilibrios'=plot_equ,
#                      'Plot Desequilibrios'=plot_desequ,
#                      'Plot Fanchart Equilibrios'=plot_fanchart_equ$Fanchart,
#                      'Plot Fanchart Desequilibrios'=plot_fanchart_desequ$Fanchart,
#                      'Plot Distribucion'=plot_distribucion_filtros,
#                      'Plot Mapa Calor'=plot_mapacalor)
#     
#     
#     return(save_plots)
#     
#     
#     
#   }
#   
#   
  
  
  
  
  

