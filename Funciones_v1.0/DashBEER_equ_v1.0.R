#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   Funcion: DashBEER_equ( ...)
#
#   Elaborado por: Daniel Felipe Riaño Rojas
#   update: 2/04/2024
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Librerias necesarias
load.lib <- c("ggplot2","forecast","ggfortify","zoo","lubridate","dplyr","forecast","tidyr","plotly","readxl")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character=TRUE)
rm(load.lib, install.lib, lib)

# equilibrios = get(load(file="C:\\Users\\usr_practicantegt52\\OneDrive - Banco de la República\\Desktop\\PRACTICA PRIMER SEMESTRE 2024\\TCRE\\Shiny\\Shiny_TCRE\\Dashboard_MTCRE\\Datos\\Oficiales_UNION_dic_2023\\Pronostico_VEC(19,13)_DOLS(13,25)_ARDL(55,13,10)_23T4F\\union_equ_suav.RData"))
# itcr = data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA PRIMER SEMESTRE 2024/TCRE/Shiny/Shiny_TCRE/Dashboard_MTCRE/Datos/202312_BASE COMPLETA 23T4.xlsx",sheet="base_des"))[21:116, c("ITCR_IPC_NT")]
# start = "2000-03-01"
# freq=4
# percentiles=paste0("p",c(90,75,50,25,10),"%")
# A <- DashBEER_equ(equilibrios, freq, start, percentiles)
# A %>% layout(xaxis = list(range = c(base::as.Date("2010-03-01"), base::as.Date("2023-12-01"))) )
# A %>% layout(xaxis = list(range = c("2010-03-01", "2023-12-01")))

#' DashBEER_equilibrios
#'
#' @param equilibrios objeto tipo data.frame que corresponde a los equilibrios suavizados de los modelos
#' @param freq objeto tipo numeric que corresponde a la frecuencia de la serie
#' @param start objeto tipo caracter que corresponde a la fecha de inicio de la serie. Ej: start = "2000-03-01"
#' @param percentiles objeto tipo vector que corresponde a los percentiles que se van a graficar. Puede ser uno o varios de las siguientes opciones: "p10%", "p25%", "p50%", "p75%", "p90%"
#'
#' @return grafico de los equilibrios suavizados con los perciles solicitados
#' @export
#'
#' @examples

DashBEER_equ_v1.0 <- function(equilibrios,
                         itcr,
                         freq,                  
                         start,                 
                         percentiles="p50%"){
  
  # Calcular vector de fechas
  if(freq == 4)  by.time = "3 month"
  if(freq == 12) by.time = "1 month"
  fecha_seq <- seq(from = base::as.Date(start), length.out=nrow(equilibrios), by = by.time) # este objeto es tipo "Date"
  print("--DashBEER_equ: vector de fechas calculado exitosamente--")

  # Calcular percentiles de los equilirbrios
  equ_percentiles <- as.data.frame(t(apply(equilibrios, MARGIN=1, quantile, probs=c(0.1,0.25,0.5,0.75,0.9))))
  equ_percentiles <- equ_percentiles %>%  select(rev(names(.)))
  equ_percentiles <- cbind("fecha"=fecha_seq, equ_percentiles) # este obejto es tipo "data.frame"
  names(equ_percentiles) <- c("fecha", paste0("p",c(90,75,50,25,10),"%"))
  print("--DashBEER_equ: percentiles calculados exitosamente--")
  
  # Colores
  colores <- c('darkblue','deepskyblue4','cornflowerblue', 'aquamarine4','aquamarine3',"black")
  names(colores) <- c(paste0("p",c(90,75,50,25,10),"%"),"ITCR")
  
  # Seleccionar percentiles
  equ_percentiles_select <- equ_percentiles %>% dplyr::select(c(fecha,percentiles)) # en esta linea se seleccionan los percentiles a graficar
  equ_percentiles_select$ITCR <- as.numeric(itcr)
  
  # Datos formato LONG
  equ_per_long <- equ_percentiles_select %>% 
    tidyr::pivot_longer(cols = -fecha, names_to="percentil", values_to="value")
  
  # Grafico
  plot_equ <- equ_per_long %>% 
    plot_ly(x = ~fecha) %>%
    add_lines(y = ~value, color = ~factor(percentil, levels=c(paste0("p",c(90,75,50,25,10),"%"),"ITCR")), colors = colores[c(percentiles,"ITCR")], showlegend=T) %>%
    layout(autosize = T, margin=list(l = 10, r = 10, b = 50, t = 60,  pad = 4)) %>% 
    layout(title = list(text='<b> Equilibrio de la TCR – ITCR NT </b>', font = list(size=20)),
           xaxis = list(title = ''), 
           yaxis = list(title = ''),
           legend = list(title=list(text='<b> Percentiles </b>')))
  print("--DashBEER_equ: Grafico elaborado--")
  
  #plot_equ %>% 
  #  add_lines(y=as.numeric(itcr[-length(itcr)]), color="black", name="ITCR", showlegend=T)
  
  
  # Output de la funcion
  return(plot_equ)
  
  
} # Fin de la funcion !!!!

