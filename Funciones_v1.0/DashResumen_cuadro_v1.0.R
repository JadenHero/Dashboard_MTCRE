#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Funcion: DashResumen_cuadro
# update: 16/04/2024
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NOTA: a futuro posiblemente cambiar como se crea la columna1 y columna2 del tabla que arroja la funcion

# Librerias
library(readxl)
library(dplyr)
library(gt)
library(zoo)
library(lubridate)

# Prueba
#datos <- read_excel(file.path(wdd,"datos_cuadro_resumen.xlsx"))
#str(datos)
#Dash_cuadro_resumen(datos)

# Como queda la tabla que devuelve la funcion
# La primera columna, los nombres estan dentro de la funcion
# La segunda columna corresponde a los nombres de las variables de la base de datos que se tome como input.
# Las columnas deben seguir el siguiente orden: "Fecha"|"Promedio histórico ITCR-IPP"|"Promedio con cambio estructural ITCR-IPP"|"BEER"|"FILTRO"|"FEER"

#' DashResumen_cuadro
#'
#' @param datos objeto tipo data.frame que corresponde a la base datos para hacer el cuadro resumen. En filas fechas y en columnas variables.
#'
#' @return Regresa un objeto tipo gt que corresponde al cuadro resumen de los ultimos 8 trimestres (ultimos 2 años)
#' @export
#'
#' @examples

DashResumen_cuadro_v1.0 <- function(datos){
  
  # Transfromar la columna fecha a tipo caracter
  datos$Fecha <- base::as.Date(datos$Fecha)
  print("--DashResumen_cuadro: se tranformo la columna fecha a tipo Date")
  
  # Indice de la fila que corresponde a "2000-03-01" (2000-T1)
  ind_2000 <- which(datos$Fecha == base::as.Date("2000-03-01"))
  
  # Indice de la utltima observacion
  n <- nrow(datos)
  print("--DashResumen_cuadro: se calculo las posiciones que corresponden a 2000-03-01 y a la ultima observacion")
  
  # Calcular Intervalo a una desviacion estandar
  desviaciones <- c(sd(datos[[2]][1:(n-1)], na.rm=T),        # desv. "Promedio histórico ITCR-IPP"
                    sd(datos[[3]][1:(n-1)], na.rm=T),        # desv. "Promedio con cambio estructural ITCR-IPP"
                    sd(datos[[4]][ind_2000:(n-1)], na.rm=T), # desv. "BEER"
                    sd(datos[[5]][1:(n-1)], na.rm=T),        # desv. "FILTRO"
                    sd(datos[[6]][ind_2000:(n-1)], na.rm=T)  # desv. "FEER"
  )
  # Para todas las variables la desviacion se calculan sin el ultimo dato
  # Para las variables que corresponden al BEER y FEER se calcula con los datos a partir de 2000-03-01
  print("--DashResumen_cuadro: se calculo los Intervalos a 1 desviacion estandar")
  
  # Leer los datos de los ultimos 2 años 
  datos <- tail(datos, n=8)
  
  # Obtener Fecha corta, Eje: mar-23
  fecha_corta <- format(datos$Fecha, "%b-%y")
  fecha_corta <- gsub("\\.", "",fecha_corta) # fecha formato "mmm-yy"
  print("--DashResumen_cuadro: se creo fecha formato corto 'mmm-yy' ")
  
  # Obtener Fecha con fomato numero romano
  trimestre_romano <- function(fecha){
    fecha <- base::as.Date(fecha)
    trimestre <- quarters(fecha)
    #anio <- format(fecha, "%Y")
    anio <- lubridate::year(fecha)
    trimestre <- switch(trimestre, "Q1"="I", "Q2"="II", "Q3"="III", "Q4"="IV")
    return(paste(trimestre, "trim", anio))
  }
  fecha_romano <- sapply(datos$Fecha, trimestre_romano)
  print("--DashResumen_cuadro: se creo fecha formato romano")
  
  # Crear cuadro como data.frame
  datos_resumen <- data.frame(t(datos[,-1]))
  columna1 <- c("Paridad en el poder de compra","", "BEER", "Filtros Estadísticos", "FEER")
  columna2 <- c("Promedio histórico ITCR-IPP","Promedio con cambio estructural ITCR-IPP","Equilibrio suavizado, ITCR-IPC","Filtro ITCR-IPC","CC histórica -3.09% IPP")
  datos_resumen <- data.frame(columna1, columna2, datos_resumen, -desviaciones, desviaciones )
  names(datos_resumen) <- c("columna1","columna2", fecha_romano, "li","ls")
  rownames(datos_resumen) <- NULL
  print("--DashResumen_cuadro: se creo datos_resumen")
  
  # Crear cuadro con gt
  tab <- datos_resumen %>%
    gt() %>% 
    tab_header(
      title = md("**Desalineamiento porcentual de la TCR**")
    ) %>% 
    cols_merge(columns = c(li, ls), pattern = "[{1} ; {2}]" ) %>% # fomato para el intervalo
    fmt_percent(columns = -1, decimals = 1, scale_values = F) %>% # presentar como porcentajes las columnas si no estan multiplicadas x100
    # Agrupar las filas
    tab_row_group(label = md("**Desalineamiento porcentual de la TCR frente a su equilibrio**"),
                  rows = 1:3,
                  id = "grupo1") %>% 
    tab_row_group(label = md("**Desviación porcentual de la TCR frente a su tendencia**"),
                  rows = 4,
                  id = "grupo2") %>% 
    tab_row_group(label = md("**Ajuste porcentual de la TCR requerido**"),
                  rows = 5,
                  id = "grupo3") %>% 
    # Orden de los grupos de variables
    row_group_order(groups =paste0("grupo",1:3)) %>% 
    # Alinear Titulos de los grupos de filas (Filas grises)
    tab_style(style = list(cell_text(align = "center"), cell_borders(sides = c("bottom","top")) ),
              locations = list(cells_row_groups())) %>% 
    # Poner lineas negras
    tab_style(style = cell_borders(sides = c('left','right'), style = 'solid'),
              locations = cells_body(columns = "columna2")) %>% 
    tab_style(style = cell_borders(sides = c('top','bottom'), style = 'solid'),
              locations = cells_body(rows = c(3,4,5)) ) %>% 
    tab_style(style = cell_borders(sides = c('top'), style = 'solid'),
              locations = cells_body(rows = 1)) %>% 
    tab_style(style = cell_borders(sides = c('top','bottom'), style = 'solid'),
              locations = cells_row_groups() ) %>% 
    tab_style(style = cell_borders(sides = c('bottom'), style = 'solid'),
              locations = cells_column_labels()) %>% 
    # Poner titulo de las columnas en negrita
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) %>% 
    # Modificar labels de las columnas
    cols_label(columna1 = "", columna2="", li="Intervalo (+- una desviacion estandar)") %>% 
    # Configurar espacio de las columnas
    cols_align(align="center", columns = li) %>% 
    cols_width(columna1 ~ px(150),
               columna2 ~ px(200),
               li ~ px(200),
               ls ~ px(200),
               everything() ~ px(120) ) %>% 
    # Poner color a la fila del titulo principal y a los tituloes de los grupos de variables
    tab_options(
      heading.background.color = "black",
      heading.border.bottom.color = "black",
      row_group.background.color = "gray80",
      column_labels.border.bottom.color = "black"
    )
  print("--DashResumen_cuadro: se creo Cuadro Resumen con el formato especificado usando la libreria gt")
  
  
  
  # OUTPUT ------------------------------------------------------------------
  return(tab)
  
  
} # Fin de la funcion!!!




















