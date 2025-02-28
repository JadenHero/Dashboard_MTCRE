#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                                   App - MTCRE                                #
#                        DPI - Programación Macroeconómica                     #
#                        Fecha de actualizacíón: 27/05/2024                    #
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list = ls())

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# A.0.Directorios ---------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# if(Sys.getenv("HOME") == "msalazsi"){
#   
# }

# if(Sys.info()['user'] %in% c("USR_practicanteGT52", "msalazsi")){
# 
#   if(Sys.info()['user'] == "msalazsi"){ wd <- "ruta_carpeta_marlon"}
#   if(Sys.info()['user'] == "USR_practicanteGT52"){ wd <- 'C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA PRIMER SEMESTRE 2024/TCRE/Shiny/Shiny_TCRE/Dashboard_MTCRE/v0.7'}
# 
# } else {
#   wd_prueba <- readline('Ingrese por favor la ruta de trabajo: \n')
#   wd_prueba <- gsub("\\\\", "/", wd_prueba)
# }



# Directorio de trabajo
#wd <- 'C:\Users\driao\Documents\PROYECTOS\Dashboard_MTCRE'
wd <- getwd()
wdd <- paste0(wd,"/Datos")
wdf <- paste0(wd,"/Funciones_v1.0")
#wd <- "C:/Users/msalazsi/OneDrive - Banco de la República/Desktop/PMMS/MTCRE/Desarrollo/Dashboard/DashboardMTCRE/v0.6"


# Con FMOls
#carpeta_contribuciones <- "Corrida_Mar24_new_contribuciones_fmols"


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# A.1.Librerias ---------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# # Función que permita hacer un check de las versiones de R de las librerias que se van a utilizar
# source(paste0(wdf, "/DashMTCRE_install.packages.R"))
# 
# file.versiones <- file.path(wd, "MTCRE_Dashboard_lib_version.txt")
# DashMTCRE_install.packages(instalar=TRUE, file.versiones)
# 


# Librerias

# Dashboard
library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinydashboard)

# Manejo bases de datos
library(tidyverse)
library(readxl)
library(zoo)
library(dplyr)
library(lubridate)


# Graficas
library(plotly)
library(ggsci)  # paletas de colores
library(scales) # ver colores de una paleta de colores
library(RColorBrewer)
library(corrplot)
library(Hmisc)
library(tseries)
library(fpp)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# A.2.Funciones ---------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Funciones
file_names <- list.files(wdf, full.names=F, recursive = F)
for(f in file_names){
  print(f)
  source(paste0(wdf,'/',f))}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 1.Datos input dashboard ---------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## 1.1.Cuadro Resumen ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

datos_cuadro_resumen_ant        <- read_excel(file.path(wdd,"202406_cuadro_resumen_jun24.xlsx"))
names(datos_cuadro_resumen_ant) <- c("Fecha","Desalineamiento","Desalineamiento ce","BEER","Filtros","FEER")
datos_cuadro_resumen_ant        <- mutate_if(as.data.frame(datos_cuadro_resumen_ant), is.numeric, ~ . * 100)

# NOTA: Los datos del cuadro resumen deben estar en porcentaje.
# Si los datos ya estan en porcentaje omitir la ultima linea


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## 1.2.BEER --------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### 1.2.1. Equilibrios y Desequilibrios ----------------

# # Oficial marzo 2024
# equilibrios    <- get(load(file = file.path(wdd, "VEC(13,19)_DOLS(13,9)_ARDL(10,13,55)_24T1F/union_equ_suav.RData")))
# desequilibrios <- get(load(file = file.path(wdd, "VEC(13,19)_DOLS(13,9)_ARDL(10,13,55)_24T1F/union_desequ_suav.RData")))

# Con Fmols sin GNC_Saldo y Dx_publica
equilibrios    <- get(load(file = paste0(wdd, "/202406_equ_VEC_DOLS_ARDL_FMOLS_BEER_jun24/UNION/union_equ_suav.RData")))
desequilibrios <- get(load(file = paste0(wdd, "/202406_equ_VEC_DOLS_ARDL_FMOLS_BEER_jun24/UNION/union_desequ_suav.RData")))

rm(union_equ_suav, union_desequ_suav)

### 1.2.2. Datos ultima corrida ------------------------
itcr           <- as.data.frame(read_excel(paste0(file.path(wdd, "202406_BASE COMPLETA 24T2.xlsx")),
                                           sheet= "base_des"))[21:118,c("ITCR_IPC_NT")]
base_corrida   <- as.data.frame(read_excel(paste0(wdd,"/202406_BASE COMPLETA 24T2.xlsx"),
                                           sheet="base_log (2)"))[21:118,]
signo_esp <- as.data.frame(read_excel(paste0(wdd,"/202406_BASE COMPLETA 24T2.xlsx"), sheet = "Descripción variables"))[c("Variable","Signo esperado literatura")]


# Base suavizada
base_suav <- base_corrida
base_suav[,-1] <- lapply(base_suav[,-1], function(x){as.numeric(hpfilter(ts(x,start=c(2000,1),frequency = 4), freq= 1600)$trend)})

# Cargar matriz de equilibrios por cada metodologia
equ_ofi_ardl <- get(load(paste0(wdd,"/202406_equ_VEC_DOLS_ARDL_FMOLS_BEER_jun24/ARDL/equ_aumentado_suav.RData")))
equ_ofi_dols <- get(load(paste0(wdd,"/202406_equ_VEC_DOLS_ARDL_FMOLS_BEER_jun24/DOLS/equ_aumentado_suav.RData")))
equ_ofi_vec  <- get(load(paste0(wdd,"/202406_equ_VEC_DOLS_ARDL_FMOLS_BEER_jun24/VEC/equ_aumentado_suav.RData")))
equ_ofi_fmols <- get(load(paste0(wdd,"/202406_equ_VEC_DOLS_ARDL_FMOLS_BEER_jun24/FMOLS/equ_aumentado_suav.RData")))
equ_ofi_union <- get(load(paste0(wdd,"/202406_equ_VEC_DOLS_ARDL_FMOLS_BEER_jun24/UNION/union_equ_suav.RData")))
rm(equ_suav)

lista_equ_oficiales = list(equ_ofi_ardl, equ_ofi_dols, equ_ofi_vec, equ_ofi_fmols) # equ_ofi_fmols
names(lista_equ_oficiales) <- c('ARDL','DOLS','VEC','FMOLS') # FMOLS

# Agregamos equilibrios de la UNION
lista_equ_oficiales2 <- lista_equ_oficiales
lista_equ_oficiales2[['UNION']] <- equ_ofi_union
#lista_equ_oficiales2[['UNION']] <- equilibrios

# Mediana desequilibrios metodologia BEER y UNION
mediana_desequ_BEER <- lapply(lista_equ_oficiales2, function(x){100*((as.numeric(itcr)/apply(x, 1, median))-1) })
mediana_desequ_BEER <- data.frame(fecha = seq(from=base::as.Date("2000-03-01"), by="3 months",length.out=nrow(equ_ofi_ardl)),
                                  mediana_desequ_BEER)


### 1.2.3. Contribuciones ------------------------------
matriz_var           <- read_excel(paste0(wdd, "/202406_Matriz_variables_VEC_DOLS_ARDL_FMOLS_jun24.xlsx"), col_names = T)
lista_contribuciones <- readRDS(paste0(wdd, "/202406_list_contribuciones_VEC_DOLS_ARDL_FMOLS_jun24.RData"))


### 1.2.4. Info BEER -----------------------------------
lista_plots_regresoaras <- InfoBEER_plot_regresoras_v1.0(base_corrida)


### 1.2.5. Objetos varios  -----------------------------------
varios <- miscellaneous_v1.0(equilibrios, matriz_var, mediana_desequ_BEER)

# NOTA: poner un nombre muy raro, que con seguridad no este ni en el UI ni en el SERVER


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## 1.3.Filtros estadisticos ----------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Datos mediana filtros anterior
dta_prev<-read_excel(file.path(wdd,"202406_equ_Filtros_corrida_anterior.xlsx"), sheet='Hoja1')
dta_prev$Fecha<-as.Date(dta_prev$Fecha,format="%Y-%m-%d")
str(dta_prev)

dta_prev_equplot<-dta_prev %>% select('Fecha',`Tendencia (HP)`) %>% melt(id =1)
dta_prev_desequplot<-dta_prev %>% select('Fecha',`Desviaciones HP IPC`) %>% melt(id = 1)

# desviaciones estandar
desv_equ_filt_ant <- sd(dta_prev_equplot$value[-nrow(dta_prev_equplot)])
desv_desequ_filt_ant <- sd(dta_prev_desequplot$value[-nrow(dta_prev_equplot)])

# Insumo de la corrida actual
#dta.filtros_ant <- read_excel(file.path(wdd,"prueba_filtros_anterior.xlsx"), col_names = TRUE)
dta.filtros_ant <- as.data.frame(read_excel(paste0(wdd,"/202406_ITCR_IPC_jun24.xlsx")))

#dta.filtros_ant_date.fin<-c(2024,1)
#dta.filtros_ant_date.corr<-c(2024,2)
dta.filtros_date.fin<-c(2024,2)    # estas dos son las que se deben modificar
dta.filtros_date.corr<-c(2024,3)   # estas dos son las que se deben modificar


filtros_elegidos <- c('Filtro Hodrick y Prescott',
                      #'Filtro Hamilton',
                      'Filtro Promedio Móvil','Filtro STL','Filtro Baxter-King',
                      'Filtro Christiano-Fitzgerald','Filtro Butterworth','Filtro Regresión Trigonométrica',
                      #'Filtro Beveridge-Nelson',
                      'Filtro Hodrick y Prescott a una cola','Filtro Hamilton promedio')


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## 1.4.PPC ----------------------------------------------------------------
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dta_prev_ppc <- read_excel(file.path(wdd,'202406_equ_PPC_corrida_anterior.xlsx'),col_names=T)
dta_prev_ppc$Fecha <- as.Date(dta_prev_ppc$Fecha, format = "%Y-%m-%d")

dtaprev_ppc.desequplot<-dta_prev_ppc %>% dplyr::select(c('Fecha','Desalineamiento')) %>% melt(id = 1)
dtaprev_ppc.desequplot_ce<-dta_prev_ppc %>% dplyr::select(c('Fecha','Desalineamiento ce')) %>% melt(id = 1)

desv_desequ_ppc_ant <- sd(dtaprev_ppc.desequplot$value[-nrow(dtaprev_ppc.desequplot)])
desv_desequ_ce_ppc_ant <- sd(dtaprev_ppc.desequplot_ce$value[-nrow(dtaprev_ppc.desequplot_ce)])


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 2.Ejecutar app ---------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source(paste0(wd,"/server_v1.0.R"))
source(paste0(wd,"/ui_v1.0.R"))
shinyApp(ui = ui, server = server)




#run_with_themer(shinyApp(ui = ui, server = server))








  