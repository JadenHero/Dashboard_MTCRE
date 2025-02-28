#' fanchart_dashplot
#'
#' @param datos Base de datos, cuyas columnas corresponden a las variables a las cuales se les desea realizar el fanchart
#' @param observada Nombre de la variable que se desea graficar adicional al fanchart
#' @param lim_probs Probabilidades del fanchart
#' @param start Vector con la fecha inicial
#' @param start_zoom Valor TRUE en caso de que se desee obtener el zoom del Fanchart, FALSE en caso contrario
#' @param end Vector con la fecha final
#' @param freq Frecuencia de la serie
#' @param nombre Nombre de la grafica
#' @param paleta Color del fanchart, opciones: azul, naranja, verde y morado
#'
#' @return Lista con los graficos de interes
#' @export
#'
#' @examples
#' 

# datos<-list_filtros[["Data Equilibrios"]]%>%filter(Fecha >= '2000-03-01') %>% select(!c("ITCR-IPC Trimestral"))
# observada<-as.numeric(list_filtros[["Data Equilibrios"]][list_filtros[["Data Equilibrios"]]['Fecha']>='2000-03-01','ITCR-IPC Trimestral'])
# lim_probs<-c(0.1,0.9)
# start<-c(2000,1)
# start_zoom=NULL
# end<-c(2024,1)
# freq<-4
# nombre<-'Equilibrios filtros ITCR-IPC Trimestral'


DashFiltrosest_fanchart_dashplot_v1.0 <- function(datos,observada=NULL,lim_probs,start,start_zoom=NULL,end,freq,nombre, paleta="azul"){
  
  #Nuevas funciones #####
  scale_fill_discrete_gradient <- # Funci?n para dibujar marcas en los l?mites del contenedor de colores
    function(..., colours, bins = 5,
             na.value = "grey50",
             guide = "colourbar",
             aesthetics = "fill", colors)  {
      colours <- if (missing(colours))
        colors # Un vector con colores debe ser ingresado para generar los l?mites que son identificados en el n?mero de bins
      else colours
      continuous_scale(
        aesthetics,
        "discrete_gradient",
        discrete_gradient_pal(colours, bins), #Se genera la paleta de colores con un gradiente discreto para la leyenda que representa los percentiles
        na.value = na.value,
        guide = guide,
        ...
      )
    }
  
  discrete_gradient_pal <- function(colours, bins = 5) { # Funci?n para crear un gradiente discreto para la paleta de colores de la leyenda del fanchart
    ramp <- scales::colour_ramp(colours) # Genera un intervalo de colores de acuerdo al color ingresado en colours y se ramp
    
    function(x) {
      if (length(x) == 0) return(character())
      
      i <- floor(x * bins) # Guarda en i el n?mero entero que resulta de multiplicar el n?mero de l?neas verticales de color negro que se ponen en los cuadros de colores para separar un color de otro
      i <- ifelse(i > bins-1, bins-1, i)
      ramp(i/(bins-1))
    }
  }
  
  
  grafica<-function(df, obs, ensemble, zoom = c(TRUE,FALSE)){ # Se crea una funcinn auxiliar para realizar los fanchart
    plot <- ggplot() +
      geom_ribbon(data = df, aes(x = step, ymin = quantmin,
                                 ymax =quantmax,group=rev(delta),
                                 fill = as.numeric(delta))) + # Genera los polngonos cuya nrea representan la distancia que hay de un percentil a otro
      scale_fill_manual(values=c(mycols)) + # Se le agrega el color distintivo a cada polngono
      theme_minimal()+ labs(y="",x="") + # Se cambia el fondo por uno blanco y se quita el nombre de los ejes x y y
      ggtitle(nombre) + # Se agrega el tntulo al grnfico
      theme(plot.title = element_text(hjust=0.5), axis.text=element_text(colour="black"),plot.margin = unit(c(0.5,1,0.5,0.5),"cm")) + # Se centra el tntulo y se cambian las mnrgenes en blanco a la grnfica para que se vea mns centrada
      theme(plot.caption = element_text(hjust=0.5), plot.caption.position = "plot" ) + # Se centran las leyendas
      scale_fill_discrete_gradient(colors = mycols, # Se genera la paleta de colores conun gradiente discreto para la leyenda que representa los percentiles
                                   breaks = seq(1,ncol(percentiles),1), # Se especnfica cunntos cortes de color va a tener,en este caso, tantos como percentiles haya
                                   limits = c(1,ncol(percentiles)), # Se establecen desde donde hasta donde se va a realizar el corte de color
                                   bins = length(mybreaks)-1, # Lnneas verticales de color negro que se ponen en los cuadros de colores para separar un color de otro
                                   labels = mybreaks, # Nombres que va a tener cada color en la leyenda. En este caso los nombres son los percentiles
                                   guide = guide_colorbar(title = "Percentiles", # Tntlo de la leyenda
                                                          title.position = "top", # Posicinn del tntulo de la leyenda arriba del cuadro de colores
                                                          title.hjust = 0.5, # Tntulo de la leyenda centrado
                                                          frame.colour = "black",  # Color del tntulo de la leyenda
                                                          ticks.colour = "black",
                                                          barwidth=15)) + # Tamano de los cuadros de colores de la leyenda
      geom_line(data=ensemble,aes(step,ensav,color="Mediana"),size=0.75) + # Se agrega la lnnea negra que representa la mediana o percentil 50
      (if(!is.null(obs)) geom_line(data=obs,aes(step,as.numeric(as.character(observada)),color="Observada"),size=0.75) ) + # Se agrega lnnea azul que representa la variable observada, esta sern puesta snlo si se estn realizando fanchart de los equilibrios.
      scale_color_manual(values=c("Mediana" = "black", "Observada" = "blue"))+labs(color="")+ # Se seleccionan los colores de cada lnnea. La mediana es negra,la de la observada es azul
      guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+  # El tntulo de la leyenda de las lnneas de la mediana y la observada de ubica en el centro y arriba de las lnneas que representan la mediana y la observada
      theme(legend.position = "bottom",  # La leyenda de las lnneas de la mediana y la observada se ubica en el centro
            plot.title = element_text(face="bold"))
            
    if(zoom==FALSE){
      plot <- plot + xlim(c(min(date_serie),max(date_serie)))
    } else{
      if(end[1] - start_zoom[1] <= 5){ # Si se hace zoom en un intervalo no mayor a 5 años
        plot <- plot + 
          scale_x_continuous(breaks=date_serie[posicion_zoom],labels=fechaz) + # Se cambian los valores del eje X de acuerdo a si es una grnfica de acercamiento o no
          theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
      } else {
        if(end[1] - start_zoom[1] <= 10){
          time_zoom = seq(start_zoom[1]+ (start_zoom[2]-1)/freq, by=1, length.out = length(time_zoom)+1 )
          fechaz <- as.yearqtr(time_zoom)
          fechaz <- format(fechaz, "%Y-T%q")
          plot <- plot + 
            scale_x_continuous(breaks=time_zoom,labels=fechaz) 
        }
      }
      return(plot) # La salida de esta funcinn es el grnfico guardado en plot
    }
  }
  
  #Corrida ####
 
  datos[,-1]<-lapply(datos[,-1],as.numeric) # Convirtiendo formato numerico las variables
  datos_int <- datos %>% select(!Fecha) # Seleccionando todos los filtros excepto la fecha
  
  probs = seq(from = lim_probs[1], to = lim_probs[2], by =0.1) # Guarda en probs, una secuencia de decimales que inician desde el decimal lim_probs[1] hasta el decimal lim_probs[2]. Estos corresponden a los percentiles que se van a calcular para realizar el fanchart
  percentiles<-t(apply(datos_int,1,quantile,probs=probs,na.rm=TRUE)) # Guarda en percentiles el cnlculo de los percentiles establecido en la lnnea anterior. Esto es una matriz con tantas observaciones (filas) como haya en el archivo file y tantas columnas como percentiles
  
  serie = ts(percentiles[,paste0(100*probs[1],"%")], frequency = freq, start = start) # Se guarda en serie la conversinn a un objeto de serie de tiempo de la matriz de percentiles, para ello se tiene en cuenta la periodicidad de los datos y el ano de inicio. La matriz series tiene la misma dimensinn que lamatriz percentiles
  date_serie = as.vector(time(serie)) # Se extraen las fechas en formato numnrico de la matriz serie y se guarda en date_serie como un vector de valores
  posicion <- c(1:length(date_serie)) # Se crea un vector de nnmeros que van desde 1 hasta el nnmero de elementos que tenga el vector date_serie y se guarda en posicion
  
  #inicio_zoom = start_zoom[1]+(start_zoom[2]-1)*(100/freq)/100 #Se guarda en inicio_zoom la fecha en formato nnmerico desde la cual se quiere hacer el acercamiento en el fanchart. Se resta 1 porque el primer trimestre empeiza en 0, no en 0.25
  #final_zoom = end[1]+(end[2]-1)*(100/freq)/100 # Se guarda en final_zoom la fecha en formato numnrico hasta la cual se desea realizar el acercamiento en el fanchart.
  
  #time_zoom = seq(start_zoom[1]+ (start_zoom[2]-1)/freq, end[1] +(end[2]-1)/freq, by=1/freq )
  
  # zoom
  if(!is.null(start_zoom)){ # Si se desea realizar acercamiento, a continuacinn se crea el data.frame adecuado para realizar el fanchart de acercamiento
    #anos.1 <- seq(from=100*((start_zoom[1]/100)%%1),to=100*((end[1]/100)%%1),by=1) # Guarda en anos la crecinn de un vector que tiene los anos que se encuentran dentro del rango de fechas de acercamiento
    #anos <- sort(c(rep(anos.1[-(length(anos.1))],freq+1-start_zoom[2]),rep(anos.1[length(anos.1)],end[2]))) # Guarda en anos la crecinn de un vector que tiene los anos determinados en la lnnea anterior y se repiten repetidos tantas veces como periodos haya dentro del rango de fecha de acercamiento
    #trimestres = paste0("T" , c(rep(c(start_zoom[2]:freq),length(anos.1)-1) , seq(from=1,to=end[2],by=1))) # Guarda en trimestres un vector de carncteres que indica los periodos que se encuentran dentro del rango de fechas de acercamiento
    #fechaz= paste(round(anos,0),trimestres,sep=".") # Guarda en fechaz un vector de carncterns que resultan de la uninn entre los anos y los periodos dentro del rango de fechas de acercamiento
    fechaz <- as.yearqtr(time_zoom)
    fechaz <- format(fechaz, "%y-T%q")
    posicion_i <- which(date_serie == inicio_zoom) # Guarda en posicion_i, la posicinn (nnmero de observacinn) inicial desde la cual se quiere hacer el acercamiento
    posicion_f <- which(date_serie == final_zoom) # Guarda en posicion_f, la posicinn (nnmero de observacinn) final hasta la cual se quiere hacer el acercamiento
    posicion_zoom <- c(posicion_i:posicion_f) # Guarda en posicion_zoom un vector nnmerico que inicia desde posicion_i hasta posicion_f de uno en uno.
    
    df_zoom = data.frame(step = date_serie[posicion_zoom], percentiles[posicion_zoom,]) # Guarda en df_zoom la uninn por columnas de el vector de fechas para el acercamiento y la matriz de percentiles tambinn dentro de este rango de fechas.
    names(df_zoom) = c("step",colnames(percentiles)) # La 1a columna de df_zoom se llama "step" y las demns se nombran como las columnas de la matriz percentiles
    dfqm_zoom = reshape2::melt(df_zoom,id.vars=c(1)) # Se convierte df_zoom en un data frame apto para ggplot
    dfqm_zoom$delta = dfqm_zoom$variable # En la columna delta de este nuevo data.frame se guardan los valores de la columna variable
    levels(dfqm_zoom$delta) = abs(probs-rev(probs))*100 # Se asignan los niveles del factor columna delta del dfqm_zoom como los percentiles seleccionados pero no en decimales sino en enteros
    
    ensemble_av_zoom=data.frame(step=date_serie[posicion_zoom],ensav=percentiles[posicion_zoom,"50%"]) # Guarda en ensemble_av_zoom la union por columnas de las fechas que estnn dentro del rango de acercamiento y la columna del percentil 50 tambinn dentro del rango de fechas
    ensemble_av_zoom$variable=as.factor("mediana") # Se asigna a la columna variable de ensemble_av_zoom el factor mediana
    
    dfplot_zoom=ddply(dfqm_zoom,.(step,delta),plyr::summarize,
                      quantmin=min(value),
                      quantmax=max(value) ) # Se guarda en dfplot_zoom el data.frame que estn dividido segnn los percentiles para el acercamiento
  }
  
  dfq = data.frame(step = date_serie[posicion], percentiles[posicion,]) # Guarda en df_zoom la uninn por columnas de el vector de fechas y la matriz de percentiles.
  names(dfq) = c("step",colnames(percentiles)) # La 1a columna de dfq se llama "step" y las demns se nombran como las columnas de la matriz percentiles
  dfqm = reshape2::melt(dfq,id.vars=c(1)) # Se convierte dfqm en un data frame apto para ggplot
  dfqm$delta = dfqm$variable # En la columna delta de este nuevo data.frame se guardan los valores de la columna variable
  levels(dfqm$delta) = abs(probs-rev(probs))*100 # Se asignan los niveles del factor columna delta del dfqm como los percentiles seleccionados pero no en decimales sino en enteros
  
  ensemble_av=data.frame(step=date_serie[posicion],ensav=percentiles[posicion,"50%"]) # Guarda en ensemble_av_zoom la union por columnas de las fechas y la columna del percentil 50
  ensemble_av$variable=as.factor("mediana") # Se asigna a la columna variable de ensemble_av el factor mediana
  
  if(!is.null(observada)){ # Si Hay variable observada, para el caso de los fanchart de los equilibrios entonces:
    Observada = data.frame(step=date_serie[posicion],observada=observada[posicion]) # Guarda en Observada la uninn por columnas de las fechas y la variable observada
    Observada$variable=as.factor("Observada") # Se asigna a la columna variable de observada el factor Observada
    
    if(!is.null(start_zoom)){
      Observada_zoom = data.frame(step=date_serie[posicion_zoom],observada=observada[posicion_zoom]) # Guarda en Observada_zoom la union por columnas de las fechas y la columna Observada en el rango de fechas de acercamiento
      Observada_zoom$variable=as.factor("Observada") # Se asigna a la columna variable de observada el factor Observada
    }
  } else {Observada = NULL; Observada_zoom = NULL} # Si No hay variable observada para el caso de los desequilibrios entonces No se crea ningnn data_frame apto para ggplot cn la variable observada
  
  dfplot=ddply(dfqm,.(step,delta),plyr::summarize,
               quantmin=min(value),
               quantmax=max(value) ) # Se guarda en dfplot el data.frame que estn dividido segnn los percentiles
  
  # Colores de los percentiles --------------
  mybreaks <- c(seq(from = 100*probs[1], to = 100*probs[length(probs)], by = 10)) # Genera una secuencia de 10 en 10 que inicia desde el percentil mns pequeno probs[1] hasta el mayor probs[2] esto con el fin de usarlo en la leyenda que representa los percentiles
  
  paletas <- list(azul=c(alpha("dodgerblue3",0.9),alpha("dodgerblue3",0.6),alpha("dodgerblue3",0.45),alpha("dodgerblue3",0.2)) ,
                  naranja=c(alpha("firebrick2",0.9),alpha("tomato",0.65),alpha("tomato",0.45),alpha("tomato",0.2)),
                  verde=c(alpha("olivedrab",0.9),alpha("olivedrab",0.7),alpha("olivedrab",0.45),alpha("olivedrab",0.2)),
                  morado=c(alpha("orchid4",0.9),alpha("orchid4",0.7),alpha("orchid4",0.45),alpha("orchid4",0.2))  )
  if(is.null(paleta)==T){
    colors <- c(alpha("olivedrab",0.9),alpha("olivedrab",0.7),alpha("olivedrab",0.45),alpha("olivedrab",0.2))
  } else{ colors <- paletas[[paleta]]}
  
  #colors <- c(alpha("firebrick2",0.9),alpha("tomato",0.65),alpha("tomato",0.45),alpha("tomato",0.2)) # Se crea una gama de colores con diferentes transparencias.
  mycols <- c(rev(colors[1:floor(length(probs)/2)]),colors[1:floor(length(probs)/2)]) # Se crea el vector de colores que van a representar a cada uno de los percentiles
  
  
  normal <- grafica(df = dfplot, ensemble = ensemble_av, obs = Observada, zoom = FALSE) # Se llama la funcinn grafica para que haga el fanchart del archivo de equilibrios o desequilibrios que ingresn en el parnmetro file.
  plotly1<-plotly::ggplotly(normal,tooltip = c("y","x","fecha","quantmin","quantmax"),dynamicTicks = T) 
  #htmlwidgets::saveWidget(as_widget(plotly1), file=paste0(nombre,".html"))
  
  if(!is.null(start_zoom)==TRUE){ # Si el usuario pidin acercamiento entonces:
    conzoom <- grafica(df = dfplot_zoom, ensemble = ensemble_av_zoom, obs = Observada_zoom, zoom = TRUE) # Se llama la funcinn grnfica para que haga el fanchart del acercamiento del fanchart creado anteriormente.
    plotly1_zoom<-plotly::ggplotly(conzoom,tooltip = c("y","x","fecha","quantmin","quantmax"),dynamicTicks = T)
    #ggsave(conzoom,file=paste0("zoom_",nombre,".PNG"),width = 300, height =  125, units = "mm") # Se exporta un archivo xlsx con la matriz de percentiles nombrado "percentiles_" despuns de el guinn va el nombre que ingresn en el parnmetro "nombre" antecedido por la palabra zoom
    #print(normal); print(conzoom) # Imprime en el panel de grnficos usualmente ubicado en lado derecho inferior de Rstudio,las dos grnficas de fanchart (original, acercamiento)
    
    save_plots<-list('Fanchart'=plotly1,
                     'Fanchart_zoom'=plotly1_zoom)
    
    return(save_plots)
    
  } else {save_plots<-list('Fanchart'=plotly1)
        return(save_plots)
  } # Si no pidin acercamiento snlo imprime en elpanel de grnficos el fanchart original
  #setwd(ruta) # Selecciona como directorio de trabajo la carpeta de la ruta especificada
}


# p<-fanchart_dashplot(datos=list_filtros[["Data Equilibrios"]] %>% select(!c("ITCR-IPC Trimestral")),
#                   observada<-list_filtros[["Data Equilibrios"]][,"ITCR-IPC Trimestral"],
#                   lim_probs<-c(0.1,0.9),
#                   start<-inicio,
#                   start_zoom=c(2019,1),
#                   end<-c(2024,1),
#                   freq<-4,
#                   nombre<-'Equilibrios filtros ITCR-IPC Trimestral')
# 
# k<-fanchart_dashplot(datos=list_filtros[["Data Desequilibrios"]] ,
#                      observada<-NULL,
#                      lim_probs<-c(0.1,0.9),
#                      start<-inicio,
#                      start_zoom=c(2019,1),
#                      end<-c(2024,1),
#                      freq<-4,
#                      nombre<-'Desequilibrios prueba filtros ITCR-IPC Trimestral')

