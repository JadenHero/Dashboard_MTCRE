#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcion: DashBEER_contribucion_top10_v0.1
# update: 24/04/2024
# Desarrolado por: Daniel Felipe Riaño Rojas
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Librerias
load.lib <- c("tidyr","dplyr","ggsci","plotly")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character = T)
rm(load.lib, install.lib, lib)


#' DashBEER_contribucion_top10
#'
#' @param datos objeto tipo data.frame que se obtiene de cargar en R el excel Equivar que sale de las contribuciones del BEER
#' @param fecha objeto tipo caracter que indica la fecha. Ej: fecha = "2023-12-01"
#' @param top numero que indica cuantas variables estaran ene l top. Por defecto son 10.
#'
#' @return Regresa un grafico de barras en plotly con el top 10 de las variables que mas contribuyen al equilibrio para la fecha pedida
#' @export
#'

# ruta_contribuciones = file.path(wdd, carpeta_contribuciones)
# modelo = 'ARDL'
# fecha = "2024-03-01"
# top=10



DashBEER_contribucion_top10_v1.0 <- function(lista_contribuciones, modelo, fecha, top=5){
  
  # Ruta de lectura de los datos
  # ruta_modelo_grupo <- file.path(ruta_contribuciones, paste0("contribuciones ",modelo," anu"),
  #                                "Equilibrios", paste0("Equivar",".xlsx"))
  # Leer datos Contribuciones
  datos <- lista_contribuciones[['Dequ_var']]
  print("--DashBEER_contribucion_equ: Se cargaron las contribuciones del archivo de la ruta--")
  
  # Transformar columna Fecha a tipo character
  names(datos)[1] <- "Fecha" 
  datos$Fecha <- as.character(datos$Fecha)
  
  # Datos en formato Long
  datos_long <- datos %>% filter(Fecha == fecha) %>%
    pivot_longer(cols = -c(1:3),
                 names_to = "variable",
                 values_to = "value") %>% 
    mutate(value2 = abs(value)) %>% 
    arrange(desc(value2)) %>% 
    filter(variable != "No explicado") %>% 
    head(n = top) # sellecionamos el top 10 de variables
  
  datos_aux <- data.frame(Fecha=fecha, datos_long[1,2], datos_long[1,3],
                          variable = 'Otras variables', value = datos_long[1,3]-sum(datos_long$value), value2 = abs(datos_long[1,3]-sum(datos_long$value)) )
  # inspecionar bien como contruir Otros
  
  names(datos_aux) <- names(datos_long)
  datos_long <- rbind(datos_long, datos_aux)
  datos_long <- datos_long %>% arrange(desc(value2))
                                       
  print("--DashBEER_contribucion_top10: Creando datos en formato Long--")
  
  # Colores
  #colores <- pal_simpsons("springfield")(top)
  #print("--DashBEER_contribucion_top10: Tomando colores igual al parametro top--")
  
  datos_long$Fecha <- as.character(datos_long$Fecha)

  # Grafico en ggplot -------------------------------------------------------
  fig <- ggplot() + 
    geom_bar(aes(x=Fecha, y=value, fill = variable), data=datos_long, stat="identity", position="stack", color='black', linewidth=0.5)+
    geom_text(aes(x=Fecha, y=value, fill = variable, label=round(value,3)), data=datos_long,  position = position_stack(vjust = 0.5)) +
    geom_point(aes(x=Fecha,y=value),color='black', data=data.frame(Fecha=fecha, value=sum(datos_long$value)) ) +
    #geom_point(aes(x=Fecha,y=Crecimiento.mediana),color='black', data=datos_long )+
    #geom_line(aes(x=Fecha, y = rep(0,nrow(dta_zoom)) ), data= dta_zoom, color="black", linewidth=0.5) +
    #geom_hline(yintercept = 0, size = 0.7) +
    labs(x='',y='crecimiento anual',
         title=paste0(top ," variables con mayor contribución al crecimiento anual\n del equilibrio de la TCR para ",fecha), #  " modelo ", paste(modelo,collapse=" ")
         fill="") +
    theme_minimal()+
    theme(legend.position = 'bottom',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(face="bold", size=12, hjust=0.5))
  print("--DashBEER_contribucion_top10: Se realizo grafico en ggplot--")
    

  
  
  # Grafico en plotly -------------------------------------------------------
  
  fig_plotly <- ggplotly(fig) %>% 
    layout(legend = list(showlegend = T, orientation = 'h', xanchor = 'center', x = 0.5),
           xaxis = list(showticklabels = F),
           bargap = 0.5)
  print("--DashBEER_contribucion_top10: Se transformo grafico a plotly--")
  
  # fig <- plot_ly(datos_long, x = ~Fecha, y = ~value, type='bar', color = ~variable, colors = colores, name = ~variable,
  #                text = ~round(value,3), textposition = "inside", textfont = list(color="black", size=11), marker = list(line=list(color="black", width=1))) %>% 
  #   #layout(autosize = T, margin=list(l = 10, r = 10, b = 50, t = 50,  pad = 2)) %>% # pad: espacio entre el grafico y el area de dibujo, a mayor pad mas espacio
  #   layout(title = list(text = paste0("Top 10 variables que más contribuyen al crecimiento Anual ",fecha, " modelo ", modelo),
  #                       size = 11),
  #          xaxis = list(title = "",
  #                       showticklabels = FALSE),
  #          legend = list(title = list(text="<b>Variables<b>"),
  #                        orientation = 'h',
  #                        xanchor = 'center',
  #                        x = 0.5, y = -0.05),
  #          barmode = "relative",
  #          bargap = 0.5,
  #          font = list(color = "black"))
  # print("--DashBEER_contribucion_top10: Se realizo grafico en plotly--")
  # 
  # fig <- fig %>% layout(margin = list(b=5, pad=2),  #pad: espacio entre el grafico y el area de dibujo, a mayor pad mas espacio
  #                       uniformtext = list(minsize=11, mode='show'))
  # print("--DashBEER_contribucion_top10: Ajustando layout del grafico--")
  # 
  # 
  # OUTPUT ------------------------------------------------------------------
  return(fig_plotly)
  
  
} # Fin de la funcion!!!!