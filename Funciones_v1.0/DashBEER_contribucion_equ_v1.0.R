#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Funcion: contribucion_shiny_plot_equ
# update: 24/04/2024
# Elaborado por: Daniel Felipe Riaño Rojas
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Librerias
load.lib <- c("readxl","dplyr","ggplot2","plotly")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character = T)
rm(load.lib, install.lib, lib)

#' DashBEER_contribucion_equ
#' 
#' Obtener las graficas en plotly de las contribuciones al equilibrio de la corrida actual
#'
#' @param ruta_contribuciones Ruta de la carpeta donde se tienen los resultados de las contribuciones 
#' @param grupo Grupo de variables para el cual se quiere ver la contribucion 
#' @param modelo Tipo de modelo segun la cual se quieren ver las contribuciones. Los valores permitidos son 'ARDL', 'DOLS', 'VEC', 'UNION'   
#' @param fecha Fecha para la cual se hara el zoom
#'
#' @return Regresa una lista con dos graficos. EL primero se llama "grafico" y el segundo "grafico_zoom"
#' @export
#'
#' @examples



# # Opciones de la Barra desplegable para los Trimestres (Fecha)
# fechas <- seq(from=as.Date("2000-03-01"), length.out = nrow(equilibrios), by="3 month") # to=as.Date("2024-03-01")
# fechas_list <- as.list(fechas)
# fechas_list <- lapply(fechas_list, as.character)
# names(fechas_list) <- format(as.yearqtr(fechas), "%Y-T%q")
# 
# matriz_var   <- matriz_var
# lista_contri <- lista_contribuciones
# equ_oficial  <- equilibrios
# grupo        <- 'Equilibrios'
# modelo       <- 'VEC'
# fecha        <- fechas_list[['2024-T1']]
# 

# matriz_var   = matriz_var
# lista_contri = lista_contribuciones
# equ_oficial  = equilibrios
# grupo        = "Equilibrios"
# modelo       = tipo_modelo
# fecha        = fechas_list[['2024-T1']]
  

# Funcion
DashBEER_contribucion_equ_v1.0 <- function(lista_contribuciones,modelo, fecha){
  
  # Incluir matriz contribuciones Union
  # DashBEER_contribuciones_list <- DashBEER_contribuciones_UNION_v0.3(Matriz_var   = matriz_var,  
  #                                                                    lista_contri = lista_contri,
  #                                                                    oficiales    = equ_oficial,
  #                                                                    periodo      = "anu", 
  #                                                                    modelos      = modelo)
                                   
  # Leer datos Contribuciones
  dta <- lista_contribuciones[['Dequ']]
  print("--DashBEER_contribucion_equ: Se cargaron las contribuciones del archivo de la ruta--")
  
  #names(dta)[1] <- 'Fecha'
  #names(dta)[2] <- 'tas'
  dta$Fecha <- base::as.Date(dta$Fecha)
  
  # Transformar datos a formato LONG
  dta_plot <- melt(dta, id = 1)
  dta_plot$value <- round(dta_plot$value, 6) # posiblemente cambiar
  print("--DashBEER_contribucion_equ: Obtener datos en formato Long--")
  
  # Grafico en ggplot
  plot_con <- ggplot() + 
    geom_bar(aes(x=Fecha,y=value, fill = variable), data= dta_plot %>% filter(!variable %in% c('Crecimiento.grupos','Crecimiento mediana')),stat = "identity", position="stack", color = 'black', linewidth=0.5)+
    geom_line(aes(x=Fecha, y = value), data=dta_plot %>% filter(variable=='Crecimiento mediana'), color="black", linewidth=0.8)+
    scale_fill_manual(values=c(hue_pal()(length(names(dta))-4),'red')) +
    theme_minimal() +
    labs(x='',y='crecimiento anual',
         title=paste0('Contribuciones de los fundamentales sobre el crecimiento anual del equilibrio de la TCR – ITCR NT metodología ', paste(modelo,collapse=" ")),
         fill="") + #cambiar nombre
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 years") +
    theme(legend.position = 'bottom',
          plot.title = element_text(face="bold"))
  print("--DashBEER_contribucion_equ: Se hizo grafico en ggplot--")
  
  # grafico en plotly
  plot_con_plt <- plotly::ggplotly(plot_con, dynamicTicks = T) %>%
    layout(legend = list(showlegend = T, orientation = 'h', xanchor = 'center', x = 0.5))
  print("--DashBEER_contribucion_equ: Se hizo grafico en plotly--")
  

  
  # ZOOM
  dta_zoom <- dta[which(dta$Fecha == base::as.Date(fecha)),]
  dta_zoom <- melt(dta_zoom, id = 1)
  dta_zoom$value <- round(dta_zoom$value, 6)
  print("--DashBEER_contribucion_equ: Obteniendo datos para el zoom--")
  
  dta_zoom$Fecha <- as.character(dta_zoom$Fecha)
  
  # Grafico zoom
  plot_zoom <- ggplot() + 
    geom_bar(aes(x=Fecha,y=value, fill = variable), data= dta_zoom %>% filter(!variable %in% c('Crecimiento.grupos','Crecimiento mediana')),stat = "identity", position="stack", color = 'black', linewidth=0.5)+
    geom_text(aes(x=Fecha, y=value, fill = variable, label=round(value,3)),data= dta_zoom %>% filter(!variable %in% c('Crecimiento.grupos','Crecimiento mediana')), position = position_stack(vjust = 0.5)) +
    geom_point(aes(x=Fecha,y=value),color='black',data=dta_zoom %>% filter(variable == 'Crecimiento mediana'))+
    scale_fill_manual(values=c(hue_pal()(length(names(dta))-4),'red')) +
    #geom_line(aes(x=Fecha, y = rep(0,nrow(dta_zoom)) ), data= dta_zoom, color="black", linewidth=0.5) +
    #geom_hline(yintercept = 0, size = 0.7) +
    theme_minimal()+
    labs(x='',y='crecimiento anual',
         title=paste0('Contribución al crecimiento anual del equilibrio de la TCR para ',fecha), # ' - modelo ', paste(modelo,collapse=" ")
         fill="") +
    theme(legend.position = 'bottom',
          plot.title = element_text(face="bold", size=12))
  print("--DashBEER_contribucion_equ: Grafico en ggplot del zoom--")
  
  # garfico enn plotly
  plot_zoom_plt <- plotly::ggplotly(plot_zoom) %>%  #, dynamicTicks = T
    #add_trace(y = ~0, mode="lines", line=list(color="gray20")) %>% 
    layout(legend = list(showlegend = T, orientation = 'h', xanchor = 'center', x = 0.5),
           xaxis = list(showticklabels = F),
           bargap = 0.5) 
  print("--DashBEER_contribucion_equ: Grafico en plotly del zoom--")
  
  
  # OUTPUT
  return(list("grafico"=plot_con_plt, "grafico_zoom"= plot_zoom_plt))

} # Fin de la funcion!!!


