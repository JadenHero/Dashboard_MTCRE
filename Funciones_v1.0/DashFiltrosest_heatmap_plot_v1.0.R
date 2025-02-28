#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Funcion Mapa de Calor. Fecha = 22/03/2024
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(plotly) # version 4.10 en adelante
library(dplyr)
library(readxl)
library(lubridate)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Argumentos:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# datos: Un data.frame cuya primera columna es la Fecha, seguido por las variables que seran las filas del mapa de calor
# n_ultimos: Numero de observaciones finales que se mostraran en la columnas del mapa de calor


# Por ahora la escala es Rojo, Blanco, Azul
# Escala de Rojo y Verde
#colorscale = list(c(0,   "rgb(255,0,0)"),   # Rojo
#                  c(0.5, "rgb(255,255,0)"), # Amarillo
#                  c(1,   "rgb(0,176,80)"))) # Verde

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ejemplo
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#desequ_filt <- read_excel("C:\\Users\\usr_practicantegt52\\OneDrive - Banco de la República\\Desktop\\PRACTICA PRIMER SEMESTRE 2024\\TCRE\\FILTROS TCRE\\Datos\\ITCR-IPC Trimestral\\Filtros de ITCR-IPC Trimestral.xlsx",sheet=2)
#desequ_beer <- read_excel("C:\\Users\\usr_practicantegt52\\OneDrive - Banco de la República\\Desktop\\PRACTICA PRIMER SEMESTRE 2024\\TCRE\\Shiny\\Shiny_TCRE\\Dashboard_MTCRE\\Datos\\Oficales_BEER_dic23.xlsx")

## Filtros
#heatmap_plot(desequ_filt) # con datos de la corrida de mar24
#
## BEER
#heatmap_plot(desequ_beer) # con datos de la corrida de dic23


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcion heatmap_plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DashFiltrosest_heatmap_plot_v1.0 <- function(datos, n_ultimos=8,  titulo=""){
  
  # Seleccionar los ultimos valores
  datos <- tail(datos, n_ultimos)
  
  # corregir nombre de los filtros
  names(datos) <- gsub(".*\\.", "", names(datos)) 
  
  # Mostrar Fecha en el formato mes año
  datos$Fecha <- as_date(datos$Fecha)
  datos$Fecha <- format(datos$Fecha, format="%b %Y")
  datos$Fecha <- factor(datos$Fecha, levels=datos$Fecha)
  
  
  # Transformar los datos a formato LONG
  datos_long <- datos %>% tidyr::pivot_longer(cols=-1, names_to="serie", values_to="value")
  datos_long$serie <- factor(datos_long$serie, levels = rev(colnames(datos)[-1]) )
  
  # Transformar la columna de los valores a tipo numerico
  datos_long$value <- as.numeric(datos_long$value)
  
  number_scale<-0-(min(datos_long$value))/(max(datos_long$value)-min(datos_long$value))
  
  # Create heatmap
  heatmap_plot <- plot_ly(data = datos_long,
                          x = ~Fecha,
                          y = ~serie,
                          z = ~value,
                          type = "heatmap",
                          zmin = min(datos_long$value),  # Adjust min and max values for color scale,
                          zmax = max(datos_long$value),  # mejor poner valores fijos que tengan algun sentido en el problema
                          colorscale = list(c(0,   "rgb (255,0,0)"),
                                            c(number_scale, "rgb(255,255,255)"),
                                            c(1,   "rgba(0,100,255,0.9)")))
  
  heatmap_plot <- heatmap_plot %>%
    plotly::layout(title =list(text=paste0("<b>",titulo,"<b>"), y=1.25),
                   xaxis = list(title = "", side='top'),
                   yaxis = list(title = ""),
                   legend = list(orientation = "h", x = 0.5, y = 0.1, title='Valores'),
                   margin = list(t=85))
  
  heatmap_plot <- heatmap_plot %>%
    add_annotations(
      x = datos_long$Fecha,
      y = datos_long$serie,
      text = ~paste("<b>",round(value, 1),"%","<b>"),
      showarrow = FALSE,
      font = list(color = "black")
    )
  
  #heatmap_plot <- heatmap_plot %>%
  #  colorbar(x = 0.5, y = -0.05, len = 1, thickness = 20, tickfont = list(color = 'black'),
  #           title='', orientation='h')
  
  heatmap_plot<-heatmap_plot %>%
    plotly::colorbar(x = 0.5, y = -0.05, len = 1, thickness = 20, tickfont = list(color = 'black'),
                     title='',orientation='h')
  
  
  return(heatmap_plot)
  
}


# heatmap_plot(datos=list_filtros[["Data Desequilibrios"]],n_ultimos=8)

