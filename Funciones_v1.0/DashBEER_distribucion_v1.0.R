#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcion: DashBEER_distribucion2
# update: 22/04/2024
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Librerias ---------------------------------------------------------------
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

load.lib <- c("readxl","dplyr","plotly","zoo")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character = T)
rm(load.lib, install.lib, lib)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Ejemplo -----------------------------------------------------------------
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#equilibrios <- get(load(file="C:\\Users\\usr_practicantegt52\\OneDrive - Banco de la República\\Desktop\\PRACTICA PRIMER SEMESTRE 2024\\TCRE\\Shiny\\Shiny_TCRE\\Dashboard_MTCRE\\Datos\\Oficiales_UNION_dic_2023\\Pronostico_VEC(19,13)_DOLS(13,25)_ARDL(55,13,10)_23T4F\\union_equ_suav.RData"))
#observada <- as.data.frame(read_excel(paste0(wdd,"/202312_BASE COMPLETA 23T4.xlsx"), sheet= "base_des"))[21:116,c("ITCR_IPC_NT")]
#fecha = "2023-12-01"
#start = "2000-03-01"
#freq=4
#DashBEER_distribucion2(equilibrios, observada, fecha, start, freq )


#' DashBEER_distribucion2
#'
#' @param equilibrios objeto tipo data.frame que contiene los equilibrios de los modelos
#' @param observada ITCR observado de la base con la cual se estimaron los BEER
#' @param fecha objeto tipo caracter que corresponde a la fecha de los equilibrios. Ej: fecha = "2023-03-01"
#' @param start objeto tipo caracter que corresponde a la fecha donde inicia la serie.  Ej: start = "2000-03-01"
#' @param freq numero que corresponde a la frecuencia de la serie. 4 para Trimestral y 12 para Mensual.
#'
#' @return Regresa el grafico de la distribucion de los equilibrios para la fecha seleccionada
#' @export
#'
#' @examples

DashBEER_distribucion_v1.0 <- function(equilibrios, observada, fecha, start, freq=4){
  
  # Transformar a objeto tipo matrix
  equilibrios <- as.matrix(equilibrios)
  print("--DashBEER_distribucion2: Se creo el objeto equilibrios tipo matrix")
  
  # Configurar la periocidad de la serie
  if(freq == 4) by.time <- "3 month"
  if(freq == 12) by.time <- "1 month"
  
  # Crear objeto de fechas
  fechas.seq <- seq(from=base::as.Date(start), by = by.time, length.out = nrow(equilibrios))
  print("--DashBEER_distribucion2: Secuencia de fechas creada")
  
  # Verificar que la fecha selecionada este en la base
  # fecha = "2024-03-01"
  if(!(fecha %in% fechas.seq)){
    print("--DashBEER_distribucion2: La fecha solicitada sale del rango de la base")
    return(invisible(NULL))
  }
  
  # Seleccionar los de la fecha especificada
  tt <- which(fechas.seq == fecha)
  fecha <- base::as.Date(fecha, format = "%Y-%m-%d")
  fecha.tri <- format(as.yearqtr(fecha), format="%Y-T%q") # posiblemente cambiar a format="%Y-T%q"
  print("--DashBEER_distribucion2: Fecha especificada selecionda")
  
  # Tomar ITCR y equilibrios del trimestre seleccionado
  ITCR <- as.numeric(observada[tt])
  equilibrios <- equilibrios[tt, ]
  print("--DashBEER_distribucion2: Tomando ITCR y equilibrios de la fecha seleccionada")
  
  # Calculando lso percitels 2.5 y 97.5
  p2.5 <- quantile(equilibrios, probs=0.025)
  p97.5 <- quantile(equilibrios, probs = 0.975)
  
  # Tomar el 95% central de la distribucion
  equilibrios <- equilibrios[(equilibrios > p2.5) & (equilibrios < p97.5)]
  print("--DashBEER_distribucion2: Tomando el 95% central de la distribucion")
  
  # Media y Mediana
  mediana <- median(equilibrios)
  media <- mean(equilibrios) # no se esta usando, pero por si se quiere usar
  
  # Quantiles
  probs <- c(0.25,0.5,0.75)
  quantiles <- quantile(equilibrios, prob=probs)
  print("--DashBEER_distribucion2: Quantiles calculados")
  
  # Otros percentiles
  # p2.5 <- quantile(equilibrios, probs=0.025)
  # p97.5 <- quantile(equilibrios, probs = 0.975)
  # p5 <- quantile(equilibrios, probs=0.05)
  # p90 <- quantile(equilibrios, probs = 0.9)
  
  # Estimacion dela densidad
  dens <- density(equilibrios, n = 50000)
  print("--DashBEER_distribucion2: Se estimo la densidad")

  # GRAFICO plotly --------------------------------------------------
  
  max_densidad <- max(dens$y) # Calcular el valor máximo de la densidad
  
  fig <- plot_ly() %>% 
    add_trace(x=dens$x, y = dens$y, type="scatter", mode="lines", fill="tozeroy", line=list(color= "blue"), text=paste("Valor:", round(dens$x,5), "<br>Densidad:", round(dens$y,5)), name="Densidad") %>%
    # Lineas Verticales de los quantiles
    add_trace(x=c(quantiles[1], quantiles[1]), y=c(0, max_densidad), type="scatter", mode="lines", line=list(color= "green", dash="dash"), text=paste("Percentil 25: ", round(quantiles[1],5)),  name="25 %") %>%
    add_trace(x=c(mediana, mediana), y= c(0, max_densidad), type = "scatter", mode = "lines", line = list(color= "black", dash="dash"), text=paste("Percentil 50: ", round(mediana,5)),  name="50%") %>%
    add_trace(x=c(quantiles[3], quantiles[3]), y= c(0, max_densidad), type = "scatter", mode = "lines", line = list(color= "orange", dash="dash"), text=paste("Percentil 75: ", round(quantiles[3],5)), name="75%") %>%
    # Linea vertical del ITCR observado
    add_lines(x=c(ITCR,ITCR), y=c(0,max_densidad), type="scatter", mode="lines", line=list(color="red", dash="dash"), text=paste("ITCRE",round(ITCR,5)), name="ITCR") %>%
    # Agregar anotaciones dentro del grafico
    add_annotations(x=quantiles[1], y=max_densidad, text="25%", showarrow=F, xshift= 0, yshift=10, textangle=45) %>%
    add_annotations(x=mediana, y=max_densidad, text="50%", showarrow=F, xshift=0, yshift=10, textangle=45) %>%
    add_annotations(x=quantiles[3], y=max_densidad, text="75%", showarrow=F, xshift=0, yshift=10, textangle=45) %>%
    add_annotations(x=ITCR, y=max_densidad, text="ITCR", showarrow=F, xshift=-10, yshift=10, textangle=45) %>%
    # Opciones de Layout
    layout(autosize = T, margin=list(l = 10, r = 10, b = 50, t = 40,  pad = 2)) %>%
    layout(title = list(text=paste0("<b>","Distribución de los equilibrios de la TCR – ITCR NT para ",fecha.tri, "</b>"), font=list(size=18)),
           xaxis = list(#range = c(p2.5, p97.5),
                        title = "TCRE",
                        tickvals = c(quantiles, ITCR),
                        ticktext = round(c(quantiles,ITCR),1),
                        tickangle = 45),
           yaxis = list(title = "Densidad"))
  print("--DashBEER_distribucion2: Grafico plotly realizado")
           

  # OUTPUT ------------------------------------------------------------------
  return(fig)
           
} # Fin de la funcion !!!
