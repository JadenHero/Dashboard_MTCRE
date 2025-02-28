#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcion: InfoBEER_barplot
# update: 24/04/2024
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' InfoBEER_barplot
#' Permite obtener el grafico de la frecuencia de aparicion (barplot) de las variables de un grupo especifico por cada tipo de modelo de que se este implementando en la metodlogia BEER
#'
#' @param datos objeto tipo data.frame que se obtiene despues de leer previamente el archivo "Matriz_variables.xls" que se obtiene de la parte de las Contribcuiones
#' @param grupo_var objeto tipo caracter que indica el grupo
#'
#' @return Regresa un grafico hecho en plotly
#' @export
#'
#' @examples

InfoBEER_barplot_v1.0 <- function(datos, grupo_var){
  
  # Ubicacion de la variable total
  ind_total <- which(names(datos) == "total")
  print("--InfoBEER_barplot: Indice de la columna 'total' tomado")
  
  # Cambiar nombre total por UNION
  names(datos)[ind_total] <- "UNION"
  print("--InfoBEER_barplot: Se cambio el nombre de la columna 'total' a 'UNION'")
  
  # Reemplazar los NA por 0 en las columnas que corresponde a los modelos y total
  datos[,3:ind_total] <- apply(datos[,3:ind_total], MARGIN = 2, replace_na, replace=0)
  print("--InfoBEER_barplot: Se reemplazron los NA de las columnas de los modelos y 'total' por 0")
  
  
  # Datos en formato long
  # grupo_var = "Fiscal"
  datos_long <- datos[,1:ind_total] %>% filter(grupo == grupo_var) %>%
    pivot_longer(cols=-c(1,2), names_to = "tipo_modelo", values_to = "conteo") %>% 
    mutate(tipo_modelo = factor(tipo_modelo, levels = c("UNION", sort(names(datos)[(ind_total-1):3]))) ) # , levels=names(datos)[ind_total:3])
  print("--InfoBEER_barplot: Se transforman los datos a tipo Long")
  
  
  # Colores
  colores <- pal_futurama("planetexpress")(length(3:ind_total))
  print("--InfoBEER_barplot: Se tomo correctamente la cantidad de colores de la paleta")
  
  # Grafico Plotly ----------------------------------------------------------
  fig <- plot_ly(datos_long, x = ~variable, y = ~conteo, color = ~tipo_modelo, type = 'bar',
                 text = ~conteo, textposition = "outside",
                 colors = colores, alpha = 0.8)
  print("--InfoBEER_barplot: Barplot creado con exito")
  
  
  fig <- fig %>% 
    layout(autosize = T, margin=list(l = 10, r = 10, b = 50, t = 50,  pad = 2)) %>% # pad: espacio entre el grafico y el area de dibujo, a mayor pad mas espacio
    layout(#title = list(text=paste0('<b> Numero de modelos en los que interviene las variables del grupo ', grupo_var, ' por tipo de modelo </b>'), font = list(size=16)),
           title = list(text=paste0('<b> Número de modelos que tienen en cuenta las variables del grupo ', grupo_var, '</b>'), font = list(size=16)),
           xaxis = list(title = paste0("<b> Variables del grupo ", grupo_var, "<b>"), tickangle = 45, tickfont = list(family="Arial", bold = TRUE, size=12)),
           yaxis = list(title = "Número de modelos"), # "Frecuencia Absoluta"
           legend = list(title=list(text='Metodologías')),
           barmode = 'group',
           uniformtext = list(minsize=12, mode='show'))
  print("--InfoBEER_barplot: Layout del Barplot realizado con exito")
  
  
  # Output ------------------------------------------------------------------
  return(fig)
  
  
} # Fin de la funcion !!!!!
