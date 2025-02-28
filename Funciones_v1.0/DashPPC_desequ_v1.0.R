#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Funcion: DashPPC_desequ
# update: 24/04/2024
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Librerias necesarias
load.lib <- c("lubridate","dplyr","plotly")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character=TRUE)
rm(load.lib, install.lib, lib)


# datos_prueba_ppc<-read_excel(file.path(wdd,'prueba_ppc.xlsx'), col_names = TRUE)
# datos <- datos_prueba_ppc
# variable <- names(datos_prueba_ppc)[2]
# fecha_prom<-c('1970-01-01','2024-04-23')
# fecha_prom_ce<-c('1985-08-01','2024-04-23')
# 
# A <- DashPPC_process_data(datos, variable, fecha_prom, fecha_prom_ce)
# 
## Ejemplo
# zoom.anio = 1970
# zoom.trim = 1
# DashPPC_desequ(A, zoom.anio, zoom.trim)

#' DashPPC_desequ
#'
#' @param datos objeto data.frame que sale de la funcion DashPPC_process_data(...)
#' @param zoom.anio año para el cual se quiere el zoom. numeric 
#' @param zoom.trim trimestre desde el cual se quiere el zoom. Los valores pueden ser 1,2,3 y 4
#'
#' @return Deculve una lista con 2 graficos en plotly. El primero es Desalineamineto ITCR-IPP; y el segundo es con cambio estructural
#' @export
#'
#' @examples


DashPPC_desequ_v1.0 <- function(datos, zoom.anio, zoom.trim ){
  
  # Hacer zoom
  zoom <- lubridate::make_date(zoom.anio, 3*zoom.trim, 1)
  datos <- datos %>% dplyr::filter(Fecha >= zoom)
  
  # Hacer grafico Desalineamiento ITCR-IPP
  fig1 <- datos %>% 
    plot_ly(x = ~Fecha) %>% 
    add_lines(y = ~ datos[["Desalineamiento" ]], color=I("black"), name = "Desviaciones promedio histórico ITCR-IPP", showlegend=T) %>% 
    add_lines(y = ~ datos[["Desv estandar promedio"]], type='scateer', mode='lines', name="+1 Desv. E",line = list(dash = 'dash', color = 'blue')) %>% 
    add_lines(y = ~ -datos[["Desv estandar promedio"]], type='scateer', mode='lines', name="-1 Desv. E", line = list(dash = 'dash', color = 'blue')) %>% 
    layout(title = list(text = "<b> Desalineamiento del ITCR-IPP frente al promedio histórico </b>"),
           yaxis = list(title = "Porcentaje %"),
           xaxis = list(#range = c(zoom, datos[["Fecha"]][nrow(datos)]),
             title = ""),
           legend = list(x=0.1, y= -0.1, orientation='h')
    )
  
  # Hacer grafico Desalineamiento ITCR-IPP con cambio estructural
  fig2 <- datos %>% 
    plot_ly(x = ~Fecha) %>% 
    add_lines(y = ~ datos[["Desalineamiento ce"]], color=I("black"), name = "Desviaciones promedio histórico con cambio estructural ITCR-IPP", showlegend=T) %>% 
    add_lines(y = ~ datos[["Desv estandar promedio ce"]], type='scateer', mode='lines', name="+1 Desv. E",line = list(dash = 'dash', color = 'blue')) %>% 
    add_lines(y = ~ -datos[["Desv estandar promedio ce"]], type='scateer', mode='lines', name="-1 Desv. E",line = list(dash = 'dash', color = 'blue')) %>% 
    layout(title = list(text = "<b> Desalineamiento del ITCR-IPP frente al promedio histórico con cambio estructural </b>"),
           yaxis = list(title = "Porcentaje %"),
           xaxis = list(#range = c(zoom, datos[["Fecha"]][nrow(datos)]),
             title = ""),
           legend = list(x=0.1, y= -0.1, orientation='h')
    )
  
  
  # OUTPUT
  return(list("desalineamiento"=fig1, "desalineamiento_ce"=fig2))
  
}