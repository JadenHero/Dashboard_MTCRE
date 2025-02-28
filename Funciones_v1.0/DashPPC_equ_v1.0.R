#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Funcion: DashPPC_equ
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
# zoom.anio = 1970
# zoom.trim = 1
# DashPPC_equ(A, zoom.anio, zoom.trim)



#' DashPPC_equ
#'
#' @param datos objeto data.frame que sale de la funcion DashPPC_process_data(...)
#' @param zoom.anio año para el cual se quiere el zoom. numeric 
#' @param zoom.trim trimestre desde el cual se quiere el zoom. Los valores pueden ser 1,2,3 y 4
#'
#' @return Deculve una lista con 2 graficos en plotly. El primero es ITCR-IPP observada y promedio historicoy; ya la segunda es con cambio estructural
#' @export
#'
#' @examples

DashPPC_equ_v1.0 <- function(datos, zoom.anio, zoom.trim ){
  
  # Hacer zoom
  zoom <- lubridate::make_date(zoom.anio, 3*zoom.trim, 1)
  datos <- datos %>% dplyr::filter(Fecha >= zoom)
  
  # Hacer grAfico de ITCR-IPP
  fig1 <- datos %>% 
    plot_ly(x = ~Fecha) %>% 
    add_lines(y = ~ datos[["ITCR-IPP"]], color=I("red"), name = "ITCR-IPP", showlegend=T) %>% 
    add_lines(y = ~ datos[["Promedio ITCR-IPP"]], type='scateer', mode='lines', name="Promedio histórico ITCR-IPP",line = list(dash = 'dash', color = 'orange')) %>% 
    layout(margin = list(top=60, pad=10)) %>% 
    layout(title = list(text = "<b> ITCR-IPP observada y promedio histórico </b>"), #  font = list(size=20)
           yaxis = list(title = "Base 2010 = 100"),
           xaxis = list(#range = c(zoom, datos[["Fecha"]][nrow(datos)]),
                        title = ""),
           legend = list(x=0.3, y= -0.1, orientation='h')
    )
  
  # Hacer grAfico de ITCR-IPP con Cambio estructural
  fig2 <- datos %>% 
    plot_ly(x = ~Fecha) %>% 
    add_lines(y = ~ datos[["ITCR-IPP"]], color=I("red"), name = "ITCRE-IPP", showlegend=T) %>% 
    add_lines(y = ~ datos[["Promedio ce ITCR-IPP"]], type='scateer', mode='lines', name="Promedio histórico con estructural ITCR-IPP",line = list(dash = 'dash', color = 'orange')) %>% 
    layout(margin = list(top=60, pad=10)) %>% 
    layout(title = list(text = "<b> ITCR-IPP observada y promedio histórico con cambio estructural </b>"),
           yaxis = list(title = "Base 2010 = 100"),
           xaxis = list(#range = c(zoom, datos[["Fecha"]][nrow(datos)]),
                        title = ""),
           legend = list(x=0.3, y= -0.1, orientation='h')
    )
  
  
  # OUTPUT
  return(list("ITCR_IPP"=fig1, "ITCR_IPP_ce"=fig2))
        
}











