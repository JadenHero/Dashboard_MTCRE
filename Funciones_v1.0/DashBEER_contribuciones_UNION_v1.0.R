

#' Matrices de contribuciones de la variables al TCRE union de los modelos
#'
#'Crea exceles con la contribucion que tiene cada variable en el crecimiento de los equilibrios estimados del  grupo, cada grupo en los equilibrios y cada variable en los equilibrios estimados para la union de los modelos.
#'
#' @param ruta_mapa Es la ruta donde se guardo la matriz de resumen creada con Matriz_variables
#' @param periodo Es el periodo el cual se va a tener el cuenta para el crecimiento, "tri" crecimiento trimestral y "anu" crecimiento anual
#' @param plot  Boolenao, si es "TRUE" crea los graficos de las contribuciones, de lo contrario solo crea Exceles con los valores
#' @param oficiales data frame con los valores oficiales de los modelos unidos
#' @param zoom Tipo caracter, es la fecha donde se quieren ver las contribuciones en particular el formato debe ser "aaaa-mm-dd"
#'
#' @return carpetas con los exceles de la descomposicion y sus respectivas graficas para la union de los modelos en su respectivo periodo
#'
#' @export Matriz_contribuciones_UNION
#'
#' @examples
#'
#' load("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA PRIMER SEMESTRE 2023/TCRE/CORRIDAS/Datos/UNIONES/2023022_Corrida_Feb/oficiales_union_VEC_DOLS_ARDL_oficiales_23_mar/union_equ_suav_oficiales_mar_23.Rdata")
#' oficiales_union<-union_equ_suav
#'
#'
#' Matriz_contribuciones_UNION(ruta_mapa=paste0(ruta_trabajo,"/Mapa de Calor/Corrida Marzo v2"),
#'                             periodo=c("tri", "anu"),
#'                             plot=TRUE,
#'                             oficiales=oficiales_union,
#'                             zoom="2023-03-01")


# Matriz_var   <- matriz_var
# lista_contri <- lista_contribuciones
# lista_ofi    <-  lista_equ_oficiales
# periodo      <- "anu"
# modelos      <- c("DOLS","VEC")


DashBEER_contribuciones_UNION_v1.0<-function(Matriz_var,  lista_contri, lista_ofi, periodo, modelos){
  ##### Librerias####
  library(ggplot2) # Gr?ficas
  library(dplyr)   # Manejar datos
  library(readxl) # Abrir bases de datos en excel
  library(zoo);library(lubridate) # Manejar fechas
  library(forecast) # Manejo series de tiempo
  library(lmtest); library(tseries);library(stats) # Test importantes
  library(Hmisc)
  library(gridExtra)
  library(grid)
  library(mFilter)
  library(reshape2)
  
  print(":::: DashBEER contribuciones librerias cargadas correctamente ::::")
  
  
  
  # Reemplazar NA por 0 en Matriz_var
  Matriz_var[is.na(Matriz_var)]<-0
  grupos<-names(table(Matriz_var[,"grupo"]))
  
  print(":::: DashBEER contribuciones matriz  cargada correctamente ::::")
  
  ##### Cargar matriz de variables y base de datos####
  #p = "anu"
  for (p in periodo) {#loop de los periodos a tener en cuenta para los modelos
    if(p=="anu"){lag.p=4; fecha_inicio=as.Date('2001-03-01') } #rezagos para el creciemiento anual
    if(p=="tri"){lag.p=1; fecha_inicio=as.Date('2000-06-01')} #rezagos para el creciemiento trimestral
    #setwd(ruta_guardado)
    #dir.create(paste0("contribuciones UNION ", p))
    
    
    
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Obteniendo la ponderacion por el RMSE de los modelos
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    # Cargando el crecimiento del equilibrio calculado para cada metodologia
    
    crec_equ_list <- lapply(modelos, function(x){
      #crec_equ           <- read_excel(paste0("contribuciones ", x, " " , p ,"/Equilibrios/Equivar",".xlsx"))[,"tasequ"]
      crec_equ <- lista_contri[[p]][['contribucion_equ']][[x]][['Crecimiento.grupos']]
      #names(crec_equ)    <- x
      return(crec_equ)
      
    })
    
    
    # Construyendo base de datos con los crecimientos por metodología
    crec_equ.dta  <- as.data.frame(crec_equ_list)
    names(crec_equ.dta) <- modelos
    
    # Cargando crecimiento obteniedo a partir de la mediana de los equilibrios
    oficiales               <- data.frame(lista_ofi[modelos])  # toma los equilbrios de las metodlogias que se seleccionaron
    oficiales_mediana       <- apply(oficiales,1,median)
    crec_oficiales_mediana  <- 100*as.numeric(na.omit(oficiales_mediana/lag(oficiales_mediana,lag.p) - 1)) # en pocentaje
    
    # Obteniendo la ponderacion a partir del RMSE
    
    # RMSE de los crecimientos frente al crecimiento oficial
    RMSE     <- sapply(names(crec_equ.dta), function(x){ sqrt( mean( (crec_oficiales_mediana-crec_equ.dta[[x]])^2) )})
    # Encontrando las ponderaciones a partir del RMSE
    RMSE_inv <- 1/RMSE
    # Definiendo las ponderaciones
    score    <- RMSE_inv/sum(RMSE_inv)
    
    
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Descomposicion crecimiento por grupo
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    contr_union   <- list()  # Lista para guardar las contribuciones estimadas del crecimiento a partir de la suma ponderada del crecimiento en cada metodologia
    contr_grp_var <- list()  # Lista para guardar las contribuciones estimadas del crecimiento del grupo por metodologia
    
    #k = "Externas"
    for (k in grupos) {#Loop de los grupos
      
      # x = "DOLS"
      # Guardando los crecimientos de los equilibrios por grupo y metodologia
      kmod <- lapply(modelos,function(x){
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        # Cargando los contribuciones de los equilibrios por grupos y metodologia
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        contr_m <- lista_contri[[p]][['contribucion_equ']][[x]][,k]

        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        # Obteniendo el número de modelos por metodologia
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        return(contr_m)
      })
      
      
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # Construyendo base de datos con las contribuciones por grupo 
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      
      
      # Base de datos con el crecimiento de los equilibrios por grupo
      names(kmod) <- modelos
      kmod.dta    <- as.data.frame(kmod)   
      # Multiplicando el crecimiento de los equilibrios por grupo por los ponderadores de cada metodologia
      yk.dta      <- data.frame(sapply(modelos,function(x){ kmod.dta[,x]*score[[x]] },USE.NAMES = T,simplify = T)  ) 
      # Guardando la suma ponderada de los equilibrios en la base de datos yk.dta
      yk.dta[[paste0('contr_pond_',k)]]  <- rowSums(yk.dta)
      
      # Definiendo base de datos donde se guardaran los equilibrios
      dk            <- yk.dta
      dk[['Fecha']] <- seq(from=fecha_inicio,by='3 months',length.out = NROW(yk.dta))
      dk            <- dk %>% select(all_of(c('Fecha',paste0('contr_pond_',k),modelos)) ) 
      
      # Guardando la contribucion agregada por grupo
      contr_union[[k]]   <- dk[[paste0('contr_pond_',k)]] # se quito el x100 ya que lista_contri viene en porcentaje
      
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # Construyendo base de datos con las contribuciones de las variables sobre el grupo 
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      
      kmod_var <- lapply(modelos,function(x){
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        # Cargando los contribuciones de los equilibrios por grupos y metodologia
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        contr_m_var <- lista_contri[[p]][["contribucion_grp"]][[x]][[k]]
        contr_m_var <- contr_m_var %>% dplyr::select(!c('Fecha','Crecimiento grupo'))
        contr_m_var <- contr_m_var*score[[x]]

        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        # Obteniendo el número de modelos por metodologia
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        return(contr_m_var)
      })
      names(kmod_var) <- modelos
       
      # Obteniendo la suma de las variables ponderadas para obtener la contribucion estimada por grupo
      sum_var_grp_df <- as.data.frame(lapply(Matriz_var$variable[Matriz_var$grupo == k],function(i){
        sum_var <- 0
        for(mm in modelos){
          sum_var <- sum_var + kmod_var[[mm]][[i]]
        }
        return(sum_var)
      }))
      names(sum_var_grp_df) <- Matriz_var$variable[Matriz_var$grupo == k]
      
      contri_grp_dta<-data.frame('Fecha' = seq(from=fecha_inicio,by='3 months',length.out = NROW(yk.dta)),
                           'Contribucion' = rowSums(sum_var_grp_df),
                           sum_var_grp_df)
      
      contr_grp_var[[k]] <- contri_grp_dta
      
      print(paste0("::::: Datos contribucion grupo ",k," realizado con exito :::::"))
      
    } # Fin Loop por grupo!!
    
    # Consolidando los datos con las contribuciones por grupo y el crecimiento
    dequ   <- data.frame('Fecha'       = seq(from=fecha_inicio,by='3 months',length.out = NROW(yk.dta)),
                         'Crecimiento.grupos' = rowSums(as.data.frame(contr_union)),
                         contr_union)
    dequ[['Crecimiento mediana']] <-  crec_oficiales_mediana
    dequ[['No explicado']] <- dequ[['Crecimiento mediana']] - dequ[['Crecimiento.grupos']]
    dequ[['Crecimiento']] <- dequ[['No explicado']] + dequ[['Crecimiento.grupos']]
    dequ   <-  dequ %>% select(all_of(c('Fecha','Crecimiento.grupos','Crecimiento mediana',grupos, 'No explicado')))
    
    
    # setwd(paste0(ruta_guardado,"/","contribuciones UNION ", p))
    # dir.create("Equilibrios")
    # setwd(paste0(ruta_guardado,"/","contribuciones UNION ", p,"/","Equilibrios"))
    # openxlsx::write.xlsx(dequ,file = "Equi.xlsx")
    
    
    print(paste0("::::: Datos contribuciones equilibrios ",p," realizado con exito :::::"))
    
    
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Descomposicion crecimiento por variable
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    # Lista con las variables por la contribucion de las variables por cada metodologia
    contr_variables_list <- lapply(modelos,function(x){
      #contr_variables <- read_excel(paste0("contribuciones ", x, " " , p ,"/","Equilibrios/equivar.xlsx"))[,-c(1:3)] # carga de las contribuciones
      contr_variables <- lista_contri[[p]][['contribucion_equ_var']][[x]][,-c(1:3)]
      contr_variables <- contr_variables*score[[x]]
    })
    names(contr_variables_list) <- modelos
    
    # Obteniendo la suma de las variables ponderadas para obtener el crecimiento estimado
    sum_var_df <- as.data.frame(lapply(Matriz_var$variable,function(i){
      sum_var <- 0
      for(mm in modelos){
        sum_var <- sum_var + contr_variables_list[[mm]][[i]]
      } 
      return(sum_var)
    }))
    
    names(sum_var_df) <- Matriz_var[['variable']]
    dequ_var<-data.frame('Fecha' = seq(from=fecha_inicio,by='3 months',length.out = NROW(yk.dta)),
                         'Crecimiento.vars' = rowSums(sum_var_df),
                         'Crecimiento.mediana' = dequ[['Crecimiento mediana']], # quitar divido 100
                         sum_var_df)
    dequ_var[['No explicado']] <- dequ_var[['Crecimiento.mediana']] - dequ_var[['Crecimiento.vars']]
    dequ_var[['Crecimiento']] <-  dequ_var[['No explicado']] + dequ_var[['Crecimiento.vars']]
    dequ_var <-  dequ_var %>% select(all_of(c('Fecha','Crecimiento.vars','Crecimiento.mediana', names(sum_var_df), 'No explicado')))
    
    
    
    
    
    print(paste0("::::: Datos contribuciones equilibrios por variable " ,p," realizado con exito :::::"))
    
    
  } # cierra for() para periodo
  
  DashBEER_contribuciones_list <- list('Dequ'       = dequ,
                                       'Dequ_var'   = dequ_var,
                                       'Contri_grp' = contr_grp_var,
                                       'score'      = score )
  
  
  return(DashBEER_contribuciones_list)
  
  

} # fin de la funcion!!


# A <- DashBEER_contribuciones_UNION_v0.5(Matriz_var,  lista_contri, lista_ofi, periodo, modelos)
# View(A$Dequ)
# View(A$Dequ_var)
