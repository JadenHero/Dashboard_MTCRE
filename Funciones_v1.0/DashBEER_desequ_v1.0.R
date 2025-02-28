#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   Funcion: DashBEER_desequ ( ...)
#
#   Elaborado por: Daniel Felipe Riaño Rojas
#   update: 24/04/2024
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Librerias necesarias
load.lib <- c("ggplot2","forecast","ggfortify","zoo","lubridate","dplyr","forecast","tidyr","plotly","readxl")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character=TRUE)
rm(load.lib, install.lib, lib)

# equilibrios = get(load(file="C:\\Users\\usr_practicantegt52\\OneDrive - Banco de la República\\Desktop\\PRACTICA PRIMER SEMESTRE 2024\\TCRE\\Shiny\\Shiny_TCRE\\Dashboard_MTCRE\\Datos\\Oficiales_UNION_dic_2023\\Pronostico_VEC(19,13)_DOLS(13,25)_ARDL(55,13,10)_23T4F\\union_equ_suav.RData"))
# rm(union_equ_suav)
# itcr = data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA PRIMER SEMESTRE 2024/TCRE/Shiny/Shiny_TCRE/Dashboard_MTCRE/Datos/202312_BASE COMPLETA 23T4.xlsx",sheet="base_des"))[21:116, c("ITCR_IPC_NT")]
# start = "2000-03-01"
# freq=4
# percentiles=c("p50%","p90%","p75%")

#' DashBEER_equilibrios
#'
#' @param equilibrios objeto tipo data.frame que corresponde a los equilibrios suavizados de los modelos. Los desequilibrios se calculan internamente a partir de estos.
#' @param itcr vector que corresponde a la columna de ITCRE de la base con la cual se ejecuto la metodologia BEER
#' @param freq objeto tipo numeric que corresponde a la frecuencia de la serie
#' @param start objeto tipo caracter que corresponde a la fecha de inicio de la serie. Ej: start = "2000-03-01"
#' @param percentiles objeto tipo vector que corresponde a los percentiles que se van a graficar. Puede ser uno o varios de las siguientes opciones: "p10%", "p25%", "p50%", "p75%", "p90%"
#'
#' @return grafico de los desequilibrios suavizados con los perciles solicitados y Des.E de la mediana
#' @export
#'
#' @examples

DashBEER_desequ_v1.0 <- function(equilibrios,
                            itcr,
                            freq,                  
                            start,                 
                            percentiles){

  # Asignar itcr observado a ITCR
  ITCR <- as.numeric(itcr)
  print("--DashBEER_desequ: ITCR cargada exitosamente--")
  
  # Calcular vector de fechas
  if(freq == 4)  by.time = "3 month"
  if(freq == 12) by.time = "1 month"
  fecha_seq <- seq(from = base::as.Date(start), length.out=nrow(equilibrios), by = by.time) # este objeto es tipo "Date"
  print("--DashBEER_desequ: vector de fechas calculado--")

  # Calcular percentiles de los equilirbrios
  equ_percentiles <- as.data.frame(t(apply(equilibrios, MARGIN=1, quantile, probs=c(0.1,0.25,0.5,0.75,0.9))))
  equ_percentiles <- equ_percentiles %>%  select(rev(names(.)))
  equ_percentiles <- cbind("fecha"=fecha_seq, equ_percentiles) # este obejto es tipo "data.frame"
  names(equ_percentiles) <- c("fecha", paste0("p",c(90,75,50,25,10),"%"))
  print("--DashBEER_desequ: percentiles de los Equilibrios calculados--")
  
  # Cacular perciles de los desquilibrios de la forma 100 X (ITCR-ITCRE*)/(ITCRE*), donde ITCRE* es el equilibrio estimado
  des_percentiles <- 100*(ITCR- equ_percentiles[,-1])/equ_percentiles[,-1]
  #names(des_percentiles) <- rev(names(equ_percentiles[-1]))
  des_percentiles <- cbind("fecha"=fecha_seq, des_percentiles)
  print("--DashBEER_desequ: percentiles de los Desequilibrios calculados--")
  
  
  # Calcular desviacion estandar de la mediana de los desequilibrios
  sigma <- sd(des_percentiles$`p50%`)
  print("--DashBEER_desequ: Desviacion Estandar de la mediana de los Desequilibrios calculada--")
  
  
  # Colores
  colores <- c('darkblue','deepskyblue4','cornflowerblue', 'aquamarine4','aquamarine3') # "p90%" "p75%" "p50%" "p25%" "p10%"
  names(colores) <- names(des_percentiles)[-1]
  
  # Seleccionar percentiles
  des_percentiles_select <- des_percentiles %>% dplyr::select(c(fecha,percentiles)) # en esta linea se seleccionan los percentiles a graficar
  
  # Datos formato LONG
  des_per_long <- des_percentiles_select %>% 
    tidyr::pivot_longer(cols = -fecha, names_to="percentil", values_to="value") %>% 
    mutate(percentil = recode(percentil, "p10%"="p90%", "p25%"="p75%", "p75%"="p25%", "p90%"="p10%"))
  
  # Recodificar percentiles
  percentiles <- recode(percentiles, "p10%"="p90%", "p25%"="p75%", "p75%"="p25%", "p90%"="p10%")
  
  # Grafico
  plot_desequ <- des_per_long %>% 
    plot_ly(x = ~fecha) %>%
    # Lineas de los perciles
    add_lines(y = ~value, color = ~factor(percentil, levels=paste0("p",c(90,75,50,25,10),"%")), colors = colores[percentiles]) %>%
    # Lineas de 1 Des.E
    add_lines(x=fecha_seq, y = rep(sigma, length(fecha_seq)), type='scateer', mode='lines', name = "+-1 Des.E", line = list(dash = 'dash', color = 'black')) %>% 
    add_lines(x=fecha_seq, y = rep(-sigma, length(fecha_seq)), type='scateer', mode='lines', showlegend = F, line = list(dash = 'dash', color = 'black')) %>% 
    # Lineas de 2 Des.E
    add_lines(x=fecha_seq, y = rep(2*sigma, length(fecha_seq)), type='scateer', mode='lines', name = "+-2 Des.E", line = list(dash = 'dash', color = 'red')) %>% 
    add_lines(x=fecha_seq, y = rep(-2*sigma, length(fecha_seq)), type='scateer', mode='lines', showlegend = F, line = list(dash = 'dash', color = 'red')) %>% 
    # Layout
    layout(autosize = T, margin=list(l = 10, r = 10, b = 50, t = 60,  pad = 4)) %>% 
    layout(title = list(text='<b> Desalineamiento de la TCR frente a su equilibrio </b>', font = list(size=20)),
           xaxis = list(title = ''), 
           yaxis = list(title = ''),
           legend = list(title=list(text='<b> Percentiles </b>')))
  print("--DashBEER_desequ: Grafico elaborado--")
  
  # Nota: para cambiar el orden como se muestra la leyenda modificar levels en linea 89 
  
  
  # Output de la funcion
  return(plot_desequ)
  
  
} # Fin de la funcion !!!!

