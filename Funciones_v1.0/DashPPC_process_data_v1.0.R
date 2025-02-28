#' DashPPC_process_data
#'
#' @param datos 
#' @param variable 
#' @param fecha_prom 
#' @param fecha_prom_ce 
#'
#' @return
#' @export
#'
#' @examples
DashPPC_process_data_v1.0<-function(datos,
                               variable,
                               fecha_prom,
                               fecha_prom_ce){
  options(scipen = 999)
  
  datos[['Fecha']]<-as.Date(datos[['Fecha']],format='%Y-%m-%d')
  
  # Seleccionando los datos que se usaran para obtener el promedio sin cambio estructural
  data.temp_ppc<-datos %>% dplyr::filter( (Fecha >= as.Date(fecha_prom[[1]])) & (Fecha <= as.Date(fecha_prom[[2]]))   )
  data.temp_ppc[['Fecha']]<-as.Date(data.temp_ppc[['Fecha']],format="%Y-%m-%d")
  data.temp_ppc[['Promedio ITCR-IPP']]<-mean(data.temp_ppc[[variable]],na.rm=T)
  
  # Seleccionando los datos que se usaran para obtener el promedio con cambio estructural
  data.temp_ppc_ce<-datos %>% filter( (Fecha >= as.Date(fecha_prom_ce[[1]])) & (Fecha <= as.Date(fecha_prom_ce[[2]])) )
  data.temp_ppc_ce[['Fecha']]<-as.Date(data.temp_ppc_ce[['Fecha']], format="%Y-%m-%d")
  data.temp_ppc_ce[['Promedio ce ITCR-IPP']]<-mean(data.temp_ppc_ce[[variable]],na.rm=T)
  
  # Agrupando los datos en trimestres
  data.ppc_trim<-data.temp_ppc %>% 
    mutate(año  = lubridate::year(Fecha),
           trim = quarter(Fecha)) %>%
    dplyr::group_by(año,trim) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup() %>%
    dplyr::select(c('Fecha',variable,'Promedio ITCR-IPP'))
  data.ppc_trim[['Fecha']]<-lubridate::ceiling_date(data.ppc_trim[['Fecha']],'quarter')-1
  data.ppc_trim[['Fecha']]<-as.Date(format(data.ppc_trim[['Fecha']], "%Y-%m-01")) # el formato con el venia tiene el ultimo dia del mes en ves del primero
  
  
  # month_temp<-list("1"="03","2"="03","3"="03",
  #                  "4"="06","5"="06","6"="06",
  #                  "7"="09","8"="09","9"="09",
  #                  "10"="12","11"="12","12"="12")
  # 

  #data.ppc_trim[['Fecha']]<-data.ppc_trim[['Fecha']]+months(2)
    # dplyr::mutate(Fecha = seq(as.Date(fecha_prom[[1]])+months(2),
    #                           as.Date(fecha_prom[[2]]),
    #                           by="quarter"))
  
  
  # Incluyendo promedio con cambio estructural
  data.ppc_trim[['Promedio ce ITCR-IPP']]<-unique(data.temp_ppc_ce[['Promedio ce ITCR-IPP']])
  
  # Obteniendo los desalineamientos
  data.ppc_trim[['Desalineamiento']]<-as.numeric((data.ppc_trim[[variable]]/data.ppc_trim[['Promedio ITCR-IPP']] - 1)*100)
  data.ppc_trim[['Desalineamiento ce']]<-as.numeric((data.ppc_trim[[variable]]/data.ppc_trim[['Promedio ce ITCR-IPP']] - 1)*100)
  
  # Seleccionando el desalineamiento de interes para calcular la sd
  desal_sd<-(data.ppc_trim %>% dplyr::filter(Fecha < data.ppc_trim[['Fecha']][NROW(data.ppc_trim[['Fecha']])]))[['Desalineamiento']]
  desal_sd_ce<-(data.ppc_trim %>% dplyr::filter(Fecha < data.ppc_trim[['Fecha']][NROW(data.ppc_trim[['Fecha']])]))[['Desalineamiento ce']]
  
  # Obteniendo las desviaciones estandar
  data.ppc_trim[['Desv estandar promedio']]<-sd(desal_sd )
  data.ppc_trim[['Desv estandar promedio ce']]<-sd(desal_sd_ce )
  
  return(data.ppc_trim)
  
  
}


# datos = read_excel(file.path(wdd,"prueba_ppc.xlsx"), col_names = T)
# variable = names(datos)[2]
# fecha_prom = c(as.Date("1970-01-01"), as.Date("2024-08-01"))
# fecha_prom_ce = c(as.Date("1985-08-01"), as.Date("2024-12-01"))
# 
# View(datos)
# View(data.temp_ppc)
# View(data.ppc_trim)
# 
# View(data.ppc_trim)
# str(data.ppc_trim)
# class(format(data.ppc_trim$Fecha, "%Y-%m-01"))
# as.Date(format(data.ppc_trim$Fecha, "%Y-%m-01"))

#fecha_prom = c(as.Date(fecha_ini), as.Date(fecha_fin))
#fecha_prom_ce = c(as.Date(fecha_ini_ce), as.Date(fecha_fin_ce))


