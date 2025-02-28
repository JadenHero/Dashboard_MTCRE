#' DashFiltrosest_Filtro_var_datos
#'
#' @param datos_filtrados Lista con los filtros que se desean agrupar en una base de datos
#' @param variables Nombre de las variables a las cuales se desea estimar los filtros estadísticos
#' @param inicio Vector con año y periodo, que se utilizará para estimar los filtros estadísticos
#' @param fin Vector con año y periodo, que se utilizará como fecha final para estimar los filtros estadísticos
#' @param freq Frecuencia de la serie que se utilizará para estimar los filtros estadisticos, 4 en caso de que la serie sea trimestral
#'
#' @return
#' @export
#'
#' @examples
DashFiltrosest_Filtro_var_datos_v1.0<-function(datos_filtrados,variables,inicio, fin=NULL,freq){
  library("RColorBrewer")
  library(zoo)
  
  for (i in variables) {# loop que recorre todas las variables solicitadas a suavizar
    
    
    # Procesando los equilibrios obtenidos a partir de los filtros estadisticos
    data.nom<-NA
    data.plot<-NA 
    list_equilibrios<-list()
    # Equilibrios
    
    for (j in names(datos_filtrados)) {#loop para realizar las graficas
      dt.fil<-datos_filtrados[[j]][[i]][,-1]
      
      
      dt.plot<-ts(dt.fil,start = inicio,frequency = freq )
      if(is.null(fin)==F){
        dt.plot<-window(dt.plot, start=inicio, end=fin)}
      
      # Guardando los equilibrios en la lista list_equilibrios
      list_equilibrios[[j]]<-dt.plot
      nom_fil<-colnames(dt.plot)[2]
      
      if(is.null(nrow(data.plot))==T){
        data.plot<-dt.plot
        data.nom<-c(i,nom_fil)
      }else{
        data.plot<-ts.union(data.plot,dt.plot[,-1])
        data.nom<-c(data.nom,nom_fil)
      }
    
    }
    colnames(data.plot)<-data.nom
    
    
    
    #Desalineamiento
    
    desequilibrio<-NA
    list_desequilibrios<-list()
    for (j in data.nom[-1]) { #loop para los desequilibrios por filtro
      #print(j)
      des.ts<-100*(data.plot[,i]-data.plot[,j])/data.plot[,j]
      
      
      # Guardando los desequilibrios en la lista list_desequilibrios
      list_desequilibrios[[j]]<-des.ts
      
      if(length(desequilibrio)==1){
        desequilibrio<-des.ts
      }else{
        desequilibrio<-ts.union(desequilibrio,des.ts)
        
      }
      
    }
    
    #colnames(desequilibrio)<-data.nom[-1]
    
    # Procesando base de datos del Equilibrio y Desalineamiento
    
    # Incluyendo la fecha
    fecha<-ts(format(datos_filtrados[[1]][[1]][,"Fecha"]),start = inicio,end=fin, frequency =freq)
    data.plot<-cbind(fecha,data.plot)
    data.plot<-data.frame(data.plot)
    #colnames(data.plot) <- c('Fecha',data.nom)
    
    desequilibrio<-cbind(fecha,desequilibrio)
    desequilibrio<-data.frame(desequilibrio)
    #colnames(desequilibrio) <- c('Fecha',data.nom[-1])
    
    # Renombrando las variables
    names(data.plot)<-c('Fecha',data.nom)
    names(desequilibrio)<-c('Fecha',data.nom[-1])
    
    
    # Definiendo formato de las variables
    data.plot[,-1]<-lapply(data.plot[,-1],as.numeric)
    data.plot$Fecha<-as.Date(data.plot$Fecha,format="%Y-%m-%d")
    
    if(length(desequilibrio)-1 == 1){
      desequilibrio[,-1] <- as.numeric(desequilibrio[,-1])
    }else{desequilibrio[,-1]<-lapply(desequilibrio[,-1],as.numeric)}
    desequilibrio$Fecha<-as.Date(desequilibrio$Fecha,format = "%Y-%m-%d")
    
    
    # Guardando los datos en una lista
    save_list<-list("Data Equilibrios"=data.plot,
                    "Data Desequilibrios"=desequilibrio)
    
    return(save_list)
    
    
  }
}
