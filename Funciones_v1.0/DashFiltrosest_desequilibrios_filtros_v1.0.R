#' desequilibrios_filtro
#'
#' @param datades Base de datos con la primera columna correspondiente a la Fecha, y el resto de columnas correspondiente a los equilibrios
#' @param anio Año desde donde se desea filtrar
#' @param trimestre Trimestre desde donde se desea filtrar, opciones: "T1","T2","T3", "T4"
#' @param filtros_series Filtros que se desean mostrar en la gráfica
#'
#' @return
#' @export
#'
#' @examples
DashFiltrosest_desequilibrios_filtro_v1.0<-function(datades,anio,trimestre,filtros_series, titulo=""){
  
  trimestre_number<-list('T1'='03',
                   'T2'='06',
                   'T3'='09',
                   'T4'='12')
  
  
  # Definiendo paleta de colores.
  colores <- brewer.pal(length(names(datades)),"Paired")
  names(colores) <- names(datades)
  colores["ITCR-IPC Trimestral"]<-'blue'
  
  # Procesando la base de datos para realizar las graficas-------
  plot_datades<- datades %>% dplyr::filter(Fecha >= '1973-03-01') %>% melt(id = 1)
  dta_mediandes<-plot_datades %>%tidyr::spread(key=variable,value=value)
  
  # Definiendo la mediana de los datos seleccionados-------
  if(length(filtros_series)==1){
    dta_mediandes[['Mediana']]<-dta_mediandes[[filtros_series[1]]]
  }else{
    dta_mediandes[['Mediana']]<-apply(dta_mediandes[,-1],1,median,na.rm=T)
  }
  dta_mediandes <- dta_mediandes %>% select(Fecha,Mediana) %>% filter(Fecha>=paste0(anio,'-',trimestre_number[[trimestre]],'-','01') )
  
  print('--Se ejecuto con exito el cálculo de la mediana--')
  
  # Graficando los equilibrios de los filtros estadisticos-------
  plot_desequ<-plotly::plot_ly(plot_datades %>% filter(Fecha>=paste0(anio,'-',trimestre_number[[trimestre]],'-','01')) , x = ~Fecha, y = ~value, color = ~variable, 
                               colors=colores, 
                               type = 'scatter', mode = 'lines') %>%
    plotly::add_trace(x = dta_mediandes[['Fecha']], y = dta_mediandes[['Mediana']],
                      type='scatter',mode='lines', color='Mediana', line = list(color = 'black', dash = 'dot') ) %>%
    layout(title = paste0("<b>",titulo,"</b>"),
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           legend = list(orientation = 'h'))
  
  print('--Se ejecuto con exito el gráfico--')
  
  return(plot_desequ)
  
}