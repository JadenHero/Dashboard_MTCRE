#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Funcion: contribucion_shiny_plot
# update: 24/04/2024
# Elaborado por: Daniel Felipe Riaño Rojas
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Librerias
load.lib <- c("readxl","dplyr","ggplot2","ggsci","plotly")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character = T)
rm(load.lib, install.lib, lib)

#' Title
#'
#' Obtener las graficas en plotly de las contribuciones de la corrida actual
#'
#' @param ruta_contribuciones Ruta de la carpeta donde se tienen los resultados de las contribuciones 
#' @param grupo Grupo de variables para el cual se quiere ver la contribucion. 
#' @param modelo Tipo de modelo segun la cual se quieren ver las contribuciones. Los valores permitidos son 'ARDL', 'DOLS', 'VEC', 'UNION' 
#' @param fecha fecha para la cual se hara el zoom.
#'
#' @return Regresa una lista con dos graficos. EL primero se llama "grafico" y el segundo "grafico_zoom"
#' @export
#'
#' @examples

#grupo = "TI"

# Funcion
DashBEER_contribucion_v1.0 <- function(lista_contribuciones, grupo, fecha,modelo){
  
  
  # Ruta de lectura de los datos
  #ruta_modelo_grupo <- file.path(ruta_contribuciones, paste0("contribuciones ",modelo, " anu"),
  #                               grupo, paste0(grupo,".xlsx"))
  
  # Leer datos Contribuciones
  dta <- lista_contribuciones[['Contri_grp']][[grupo]]
  print("--DashBEER_contribucion: Se cargaron las contribuciones del archivo de la ruta--")
  
  # names(dta)[1] <- 'Fecha'
  # names(dta)[2] <- 'tas'
  dta$Fecha <- base::as.Date(dta$Fecha)
  
  # Transformar datos a formato LONG
  dta_plot <- melt(dta, id = 1)
  dta_plot$value <- round(dta_plot$value, 6) # posiblemente cambiar
  print("--DashBEER_contribucion: Obtener datos en formato Long--")
  

  # Grafico en ggplot
  plot_con <- ggplot() + 
    geom_bar(aes(x=Fecha,y=value, fill = variable), data= dta_plot %>% filter(variable != 'Contribucion'),stat = "identity", position="stack", color = 'black', linewidth=0.5)+
    geom_line(aes(x=Fecha, y = value,color=paste0("Contribución grupo ",grupo)), data=dta_plot %>% filter(variable=='Contribucion'),  linewidth=0.8)+
    scale_color_manual(values=c('black'))+
    theme_minimal()+
    labs(x='',y='crecimiento anual',
         title=paste0('Contribución del grupo ',grupo,' sobre el crecimiento anual metodología ',paste(modelo,collapse=" ")),
         fill="",color="") + #cambiar nombre
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 years") +
    theme(legend.position = 'bottom',
          plot.title = element_text(face="bold", size=12))
  print("--DashBEER_contribucion: Se hizo grafico en ggplot--")
  
  # Grafico en plotly
  plot_con_plt <- ggplotly(plot_con, dynamicTicks = T) %>%
    layout(legend = list(showlegend = T, orientation = 'h', xanchor = 'center', x = 0.5))
  
  for (i in 1:length(plot_con_plt$x$data)){
    if (!is.null(plot_con_plt$x$data[[i]]$name)){
      plot_con_plt$x$data[[i]]$name =  gsub("\\(","",str_split(plot_con_plt$x$data[[i]]$name,",")[[1]][1])
    }
  }
  
  print("--DashBEER_contribucion: Se hizo grafico en plotly--")
  
  
  # ZOOM
  dta_zoom <- dta[which(dta$Fecha == base::as.Date(fecha)),]
  dta_zoom <- melt(dta_zoom, id = 1)
  dta_zoom$value <- round(dta_zoom$value, 6)
  print("--DashBEER_contribucion: Obteniendo datos para el zoom--")
  
  dta_zoom$Fecha <- as.character(dta_zoom$Fecha)
  
  # Grafico zoom
  plot_zoom <- ggplot() + 
    geom_bar(aes(x=Fecha,y=value, fill = variable), data= dta_zoom %>% dplyr::filter(variable != 'Contribucion'),stat = "identity", position="stack", color = 'black', linewidth=0.5)+
    geom_text(aes(x=Fecha, y=value, fill = variable, label=round(value,3)),data= dta_zoom %>% dplyr::filter(variable != 'Contribucion'), position = position_stack(vjust = 0.5)) +
    geom_point(aes(x=Fecha, y=value),color='black',data = dta_zoom %>% dplyr::filter(variable == 'Contribucion'))+
    theme_minimal()+
    labs(x='',
         y='crecimiento anual',
         title=paste0('Contribución del grupo ', grupo,' sobre el crecimiento anual para ', fecha), # - modelo ', paste(modelo,collapse=" ")
         fill = "",
         color = "") +
    theme(legend.position = 'bottom',
          plot.title = element_text(face="bold", size=12))
  print("--DashBEER_contribucion: Grafico en ggplot del zoom--")
  
  
  # garfico enn plotly
  plot_zoom_plt <- ggplotly(plot_zoom) %>% # , dynamicTicks = T
    layout(legend = list(showlegend = T, orientation = 'h', xanchor = 'center', x = 0.5),
           xaxis = list(showticklabels = F),
           bargap=0.5)
  print("--DashBEER_contribucion: Grafico en plotly del zoom--")
  
  
  # OUTPUT
  return(list("grafico"=plot_con_plt, "grafico_zoom"= plot_zoom_plt))

} # Fin de la funcion !!!

