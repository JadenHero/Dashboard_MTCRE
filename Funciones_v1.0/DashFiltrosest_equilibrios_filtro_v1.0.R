#' equilibrios_filtro
#'
#' @param data Base de datos con la primera columna correspondiente a la Fecha, segunda columna correspondiente al ITCR, y el resto de columnas correspondiente a los equilibrios
#' @param anio Año desde donde se desea filtrar
#' @param trimestre Trimestre desde donde se desea filtrar, opciones: "T1","T2","T3", "T4"
#' @param filtros_series Filtros que se desean mostrar en la gráfica
#'
#' @return
#' @export
#'
#' @examples
DashFiltrosest_equilibrios_filtro_v1.0<-function(data,anio,trimestre,filtros_series, titulo=""){
  
  trimestre_number<-list('T1'='03',
                   'T2'='06',
                   'T3'='09',
                   'T4'='12')
  
  # Definiendo paleta de colores.
  colores <- brewer.pal(length(names(data)),"Paired")
  names(colores) <- names(data)
  colores["ITCR-IPC Trimestral"]<-'blue'
  
  # Procesando la base de datos para realizar las graficas-------
  plot_data<-data %>% filter(Fecha >= '1973-03-01') %>% melt(id = 1)
  dta_median<-plot_data %>%tidyr::spread(key=variable,value=value)
  
  # Definiendo la mediana de los datos seleccionados-------
  if(length(filtros_series)==1){
    dta_median[['Mediana']]<-dta_median[[filtros_series[1]]]
  }else{
    dta_median[['Mediana']]<-apply(dta_median[,-1:-2],1,median,na.rm=T)
  }
  dta_median <- dta_median %>% select(Fecha,Mediana) %>% filter(Fecha>=paste0(anio,'-',trimestre_number[[trimestre]],'-','01') )
 
  
  # Graficando los equilibrios de los filtros estadisticos-------
  plot_equ<-plotly::plot_ly(plot_data %>% filter(Fecha>=paste0(anio,'-',trimestre_number[[trimestre]],'-','01')), x = ~Fecha, y = ~value, color = ~variable,
                            colors=colores, 
                            type = 'scatter', mode = 'lines') %>%
    plotly::add_trace(x = dta_median[['Fecha']], y = dta_median[['Mediana']],
                      type='scatter',mode='lines', color='Mediana', line = list(color = 'black', dash = 'dot') ) %>%
    layout(title = paste0("<b>",titulo,"</b>"),
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           legend = list(orientation = 'h')) 
  
  return(plot_equ)
}