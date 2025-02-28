#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcion: InfoBEER_plot_regresoras
# update: 9/04/2024
#
# Desarrolado por : Daniel Felipe Riaño Rojas
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Librerias necesarias
load.lib <- c("readxl","mFilter","plotly","tidyr")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib, require, character=TRUE)

#' InfoBEER_plot_regresoras
#' 
#' Grafica todas las variables de la base de datos junto con el filtro de Hodrick-Prescott.
#'
#' @param datos objeto tipo data.frame que corresponde a una base de datos de series de tiempo, cuya primera columna es la fecha
#'
#' @return Regresa una lista nombrada con los nombres de la variables de datos, que contiene los grafico en plotly
#' @export
#'
#' @examples

InfoBEER_plot_regresoras_v1.0 <- function(datos){
  
  # Funcion base_suavizada: Filtro de Hodrick-Prescott
  base_suavizada <- function(Tabla, exportar = FALSE, nombre = "Tabla_suav"){
    Tabla=as.data.frame(Tabla) # Se hace la lectura de la Tabla o base de datos y se transforma en data.frame
    hp<-matrix(nc=dim(Tabla[-1])[2],nr=dim(Tabla)[1],NA) # Se crea matriz vac?a con elmismo n?mero de filas que la Tabla y mismo n?mero de columnas, pero quitando la columna de fechas
    N=nrow(Tabla)
    for(i in 1:ncol(Tabla[-1])){ # Loopque recorre lascolumnas de la Tabla
      if(is.na(Tabla[-1][N,i])) next
      if(length(Tabla[-1][i][!is.na(Tabla[-1][i])==FALSE,])>=1){ # Se realiza el proceso siguiente siempre y cuando lacolumna no est? vac?a
        hp[1:max(which(!is.na(Tabla[-1][i])==FALSE)),i] <-NA # Si la columna i tiene datos faltantes, estos se mantienen en el resultado de la tabla suavizada y el filtro se aplica a la serie desde donde no haya NA
        hp[(max(which(!is.na(Tabla[-1][i])==FALSE))+1):nrow(hp),i]<-hpfilter(Tabla[-1][i][!is.na(Tabla[-1][i])==TRUE,],type="lambda",freq=1600)$trend # Se aplica el filro de Hodrick y prescot con un par?metro de suavizamiento lambda de 1600, se extrae la tendencia y se guarda en la columna i de la matriz hp
      } else hp[,i]<-hpfilter(Tabla[-1][i],type="lambda",freq=1600)$trend # Si la columna i no tiene datos faltante se realiza el filtro de Hodrick Prescott a toda la columna
    }
    
    base_2<-as.data.frame(cbind(Tabla[c(1)],hp)) # Guarda en base2 la nueva base con las columnas suavizadas con elfiltro de H.P
    colnames(base_2)<-colnames(Tabla) # Los nombres de las columnas de la base con las columnas suavizadas se establecen como los mismos nombres de la abse original
    if(exportar == TRUE) write.xlsx(base_2,file=paste0(nombre,".xlsx")) # Si se desea ecportar, se crea un archivo xlsx con la base suavizada y se guarda en la ruta en la que se encuentre trabajando
    
    return(list("base_suavizada"=base_2)) # Esta funci?n tiene como salida el data frame de la abse suavizada
    
  }
  print("--InfoBEER_plot_regresoras: se leyo la funcion 'base_suavizada'--")
  
  # Funcion plot_comp: grafica la series de datos y datos2 que tienen el mismo nombre
  plot_comp <- function(name_var){
    
    df_comp <- data.frame(Fecha = datos[["Fecha"]], var = datos[[name_var]], var_suav = datos2[[name_var]])
    
    # Grafico de la Variable junto con su filtro de HP
    fig <- plot_ly(df_comp, x= ~Fecha) %>% 
      add_lines(y=~var, name = name_var, color = I("blue") )%>% 
      add_lines(y=~var_suav, name = 'Filtro de HP', color=I("red") ) %>% 
      layout(title = list(text=paste0("<b> Variable ", name_var , " y su tendencia </b>")),
             xaxis = list(title=""),
             yaxis = list(title = "")
      )
    
    # OUTPUT
    return(fig)
  }
  print("--InfoBEER_plot_regresoras: se leyo la funcion 'plot_comp'--")
  
  # Transformar Fecha a objeto tipo "Date"
  datos[,1] <- base::as.Date(datos[,1])
  print("--InfoBEER_plot_regresoras: Se transformo Fecha a objeto tipo 'Date'--")
  
  # Aplicar filtro de HP a todas las variables menos fecha
  datos2 <- base_suavizada(datos, exportar=F)$base_suavizada
  print("--InfoBEER_plot_regresoras: Se aplico el filtro HP a los datos--")
  
  # Nombre de las variables
  varnames <- names(datos)[-1] # menos las primera columna que es la fecha
  
  # Craer lista de graficos
  lista_plots <- lapply(varnames, plot_comp)
  names(lista_plots) <- varnames
  print("--InfoBEER_plot_regresoras: Graficos de las variables junto con su filtro creado exitosamente--")
  
  
  # OUTPUT ------------------------------------------------------------------
  
  return(lista_plots)
  
} # Fin de la funcion !!!
