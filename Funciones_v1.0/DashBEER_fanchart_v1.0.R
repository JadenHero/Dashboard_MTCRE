


# Librerias

load.lib<-c("readxl","openxlsx","purrr","reshape2","ggplot2","plyr","scales","plotly", "htmlwidgets")
install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib,require,character=TRUE)
rm(load.lib, install.lib, lib)

#install.packages("plotly",type="binary")

# DESCRIPCI?N 

#   Grafica la variable observada junto con la mediana de los equilibrios y los percentiles escogidos como intervalos de confianza.
#   Tambi?n grafica la mediana de los desequilibrios y los percentiles elegidos como intervalos de confianza.

# PAR?METROS

#   1. "observada" : Si usted va a realizar el fanchart de los equilibrios, ingrese la serie 
#                    de la variable observada. Para ello puede usar la siguiente l?nea de c?digo
#                    en caso de que la serie se encuentre en un archivo excel:
#                    as.data.frame(read_excel("BASE COMPLETA.xlsx",sheet= "base_des"))[21:104,c("ITCR_IPC_NT")]
#                    La l?nea anterior lee la hoja "base_des" del archivo excel "BASE COMPLETA.xlsx" y selecciona
#                    desde la observaci?n 21 hasta la 104 de la serie de la variable nombrada en este archivo como
#                    "ITCR_IPC_NT". En lugar de escribir el nombre de la variable pude usar el n?mero de la columna
#                    Por ejemplo, la variable observada se encuentra en la columna 2 de su base "BASE COMPLETA.xlsx": 
#                    as.data.frame(read_excel("BASE COMPLETA.xlsx",sheet= "base_des"))[21:104,c(2)]
#   2. "file" : Nombre del archivo en el que est? la uni?n de los equilibrios o desequilibrios que desea graficar 
#               en el fanchart. El nombre debe estar entre comillas.
#   3. "lim_probs" : Vector con dos elementos num?ricos. El primero es el percentil m?nimo para el intervalo de confianza
#                    El segundo es el percentil m?ximo para el intervalo de confianza. Ambos n?meros deben ser escritos 
#                    en decimales. Por ejemplo, si quiere que los intervalos vayan desde el percentil 10 al 90
#                    use lim_probs = c(0.1,0.9). As?, el fanchart tendr? los percentiles 10, 20, 30, 40, 50, 60, 70, 80, 90
#                    En las regiones de sombreado : intervalos de confianza.
#   4. "start" :  Vector con dos elementos num?ricos. El primero indica el a?o en el que se registra la primera observaci?n
#                 de los equilibrios o desequilibrios. El segundo n?mero indica el semestre en el que empiezan las observaciones.
#                 Por ejemplo, si sus datos empiezan desde el trimestre 1 del a?o 2000 use start = c(2000,1).
#   5. "start_zoom" : Si desea hacer un zoom de la gr?fica desde determinada fecha, use start_zoom de la misma forma que start
#                     Pero con el a?o y el trimestre desde el cual quiere ver la gr?fica. Si no desea usar esto, use
#                     start_zoom = NULL.
#   6. "end" : vector con dos elementos num?ricos. El primero indica el a?o en el que finalizan las observaciones
#              El segundo indica el trimestre. Se usa de la misma forma que el par?metro start.
#              Si desea hacer un zoom o acercamiento hasta determinada fecha, puede indicarlo escribiendo el a?o y el
#              trimestre hasta el cual desea ver la gr?fica. Si no desea hacer esto, simplemente ponag la fecha en la que terminan
#              sus observaciones.
#   7. "freq" : La frecuencia que tienen los datos en un a?o. Por ejemplo, si es trimestral use freq = 4 porque hay 4 trimestres en un a?o
#   8. "nombre" : T?tulo para la gr?fica. Debe estar entre comillas.

# RESULTADOS 

# 1. Una gr?fica en formato PNG.

# EJEMPLO DE USO

#fanchart( ruta = "//GT134098/Users/agaravac/Desktop/Accesos compartidos/PROYECTO_TCRE/final_final",
#          carpeta = "uniones_aumentados",
#          observada = as.data.frame(read_excel("BASE COMPLETA.xlsx",sheet= "base_des"))[21:105,c("ITCR_IPC_NT")],
#          file = "equ_aumentado_suav", 
#          lim_probs = c(0.2,0.8),
#          freq = 4,
#          start = c(2000,1),
#          start_zoom = c(2019,1),
#          end = c(2020,4),
#          nombre = "Equilibrio ITCR IPC NT Suavizado (BEER)" )

#' DashBEER_fanchart
#'
#' @param ruta rura di
#' @param carpeta 
#' @param observada 
#' @param file 
#' @param lim_probs 
#' @param start 
#' @param start_zoom 
#' @param end 
#' @param freq 
#' @param nombre 
#' @param export 
#' @param percentil 
#'
#' @return
#' @export
#'

DashBEER_fanchart_v1.0 <-function(equilibrios, observada=NULL,lim_probs,start, start_zoom=NULL,end,freq,nombre,export=F, percentil){
  
  scale_fill_discrete_gradient <- # FunciOn para dibujar marcas en los l?mites del contenedor de colores
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
  
  discrete_gradient_pal <- function(colours, bins = 5) { # FunciOn para crear un gradiente discreto para la paleta de colores de la leyenda del fanchart
    ramp <- scales::colour_ramp(colours) # Genera un intervalo de colores de acuerdo al color ingresado en colours y se ramp
    
    function(x) { 
      if (length(x) == 0) return(character())
      
      i <- floor(x * bins) # Guarda en i el n?mero entero que resulta de multiplicar el n?mero de l?neas verticales de color negro que se ponen en los cuadros de colores para separar un color de otro 
      i <- ifelse(i > bins-1, bins-1, i)  
      ramp(i/(bins-1)) 
    }
  }
  
  # setwd(ruta) # Se selecciona como directorio de trabajo la carpeta ubicada en la ruta especificada.
  # observada <- observada # Se realiza la lectura de la variable observada que se encuentra en la ruta especificada, antes de volver a cambiar el directorio de trabajo
  # setwd(paste0(ruta,"/",carpeta)) # Se selecciona como directorio de trabajo la carpeta ingresada en el par?metro carpeta
  # assign("datos",get(load(paste0(file,".RData")))) # Lee el archivo ingresado en el par?metro file (este corresponde a equilibrios o desequilibrios) y lo guarda en "datos"
  
  observada <- observada # ITCR
  datos <- equilibrios
  probs = seq(from = lim_probs[1], to = lim_probs[2], by =0.1) # Guarda en probs, una secuencia de decimales que inician desde el decimal lim_probs[1] hasta el decimal lim_probs[2]. Estos corresponden a los percentiles que se van a calcular para realizar el fanchart
  percentiles <- t(apply(datos,1,quantile,probs=probs,na.rm=TRUE)) # Guarda en percentiles el c?lculo de los percentiles establecido en la l?nea anterior. Esto es una matriz con tantas observaciones (filas) como haya en el archivo file y tantas columnas como percentiles 
  
  # Calcular Mediana(Percentil deseado) por filas para cada objeto de la lista
  per <- t(apply(datos, 1, quantile, probs=c(0.1,0.25,0.5,0.75,0.9), na.rm=TRUE))
  
  
  serie = ts(percentiles[,paste0(100*probs[1],"%")], frequency = freq, start = start) # Se guarda en serie la conversi?n a un objeto de serie de tiempo de la matriz de percentiles, para ello se tiene en cuenta la periodicidad de los datos y el a?o de inicio. La matriz series tiene la misma dimensi?n que lamatriz percentiles
  date_serie = as.vector(time(serie)) # Se extraen las fechas en formato num?rico de la matriz serie y se guarda en date_serie como un vector de valores
  posicion <- c(1:length(date_serie)) # Se crea un vector de n?meros que van desde 1 hasta el n?mero de elementos que tenga el vector date_serie y se guarda en posicion
  
  inicio_zoom = start_zoom[1]+(start_zoom[2]-1)*(100/freq)/100 #Se guarda en inicio_zoom la fecha en formato n?merico desde la cual se quiere hacer el acercamiento en el fanchart. Se resta 1 porque el primer trimestre empeiza en 0, no en 0.25
  final_zoom = end[1]+(end[2]-1)*(100/freq)/100 # Se guarda en final_zoom la fecha en formato num?rico hasta la cual se desea realizar el acercamiento en el fanchart.
  
  # zoom
  if(!is.null(start_zoom)){ # Si se desea realizar acercamiento, a continuaci?n se crea el data.frame adecuado para realizar el fanchart de acercamiento
    a?os.1 <- seq(from=100*((start_zoom[1]/100)%%1),to=100*((end[1]/100)%%1),by=1) # Guarda en a?os la creci?n de un vector que tiene los a?os que se encuentran dentro del rango de fechas de acercamiento
    a?os <- sort(c(rep(a?os.1[-(length(a?os.1))],freq+1-start_zoom[2]),rep(a?os.1[length(a?os.1)],end[2]))) # Guarda en a?os la creci?n de un vector que tiene los a?os determinados en la l?nea anterior y se repiten repetidos tantas veces como periodos haya dentro del rango de fecha de acercamiento
    trimestres = paste0("T" , c(rep(c(start_zoom[2]:freq),length(a?os.1)-1) , seq(from=1,to=end[2],by=1))) # Guarda en trimestres un vector de car?cteres que indica los periodos que se encuentran dentro del rango de fechas de acercamiento
    fechaz= paste(round(a?os,0),trimestres,sep=".") # Guarda en fechaz un vector de car?cter?s que resultan de la uni?n entre los a?os y los periodos dentro del rango de fechas de acercamiento
    posicion_i <- which(date_serie == inicio_zoom) # Guarda en posicion_i, la posici?n (n?mero de observaci?n) inicial desde la cual se quiere hacer el acercamiento
    posicion_f <- which(date_serie == final_zoom) # Guarda en posicion_f, la posici?n (n?mero de observaci?n) final hasta la cual se quiere hacer el acercamiento
    posicion_zoom <- c(posicion_i:posicion_f) # Guarda en posicion_zoom un vector n?merico que inicia desde posicion_i hasta posicion_f de uno en uno.
    
    df_zoom = data.frame(step = date_serie[posicion_zoom], percentiles[posicion_zoom,]) # Guarda en df_zoom la uni?n por columnas de el vector de fechas para el acercamiento y la matriz de percentiles tambi?n dentro de este rango de fechas.
    names(df_zoom) = c("step",colnames(percentiles)) # La 1a columna de df_zoom se llama "step" y las dem?s se nombran como las columnas de la matriz percentiles
    dfqm_zoom = reshape2::melt(df_zoom,id.vars=c(1)) # Se convierte df_zoom en un data frame apto para ggplot
    dfqm_zoom$delta = dfqm_zoom$variable # En la columna delta de este nuevo data.frame se guardan los valores de la columna variable 
    levels(dfqm_zoom$delta) = abs(probs-rev(probs))*100 # Se asignan los niveles del factor columna delta del dfqm_zoom como los percentiles seleccionados pero no en decimales sino en enteros
    
    
    ensemble_av_zoom=data.frame(step=date_serie[posicion_zoom],ensav=percentiles[posicion_zoom,"50%"]) # Guarda en ensemble_av_zoom la union por columnas de las fechas que est?n dentro del rango de acercamiento y la columna del percentil 50 tambi?n dentro del rango de fechas
    ensemble_av_zoom$variable=as.factor("mediana") # Se asigna a la columna variable de ensemble_av_zoom el factor mediana
    
    dfplot_zoom=ddply(dfqm_zoom,.(step,delta),plyr::summarize,
                      quantmin=min(value),
                      quantmax=max(value) ) # Se guarda en dfplot_zoom el data.frame que est? dividido seg?n los percentiles para el acercamiento
  } 
  
  dfq = data.frame(step = date_serie[posicion], percentiles[posicion,]) # Guarda en df_zoom la uni?n por columnas de el vector de fechas y la matriz de percentiles.
  names(dfq) = c("step",colnames(percentiles)) # La 1a columna de dfq se llama "step" y las dem?s se nombran como las columnas de la matriz percentiles
  dfqm = reshape2::melt(dfq,id.vars=c(1)) # Se convierte dfqm en un data frame apto para ggplot
  dfqm$delta = dfqm$variable # En la columna delta de este nuevo data.frame se guardan los valores de la columna variable 
  levels(dfqm$delta) = abs(probs-rev(probs))*100 # Se asignan los niveles del factor columna delta del dfqm como los percentiles seleccionados pero no en decimales sino en enteros
  
  ensemble_av=data.frame(step=date_serie[posicion],ensav=percentiles[posicion,"50%"]) # Guarda en ensemble_av_zoom la union por columnas de las fechas y la columna del percentil 50 
  ensemble_av$variable=as.factor("mediana") # Se asigna a la columna variable de ensemble_av el factor mediana
  
  ensemble_av2=data.frame(step=date_serie[posicion],ensav=percentiles[posicion,percentil]) # Guarda en ensemble_av_zoom la union por columnas de las fechas y la columna del percentil 50 
  ensemble_av2$variable=as.factor("mediana") # Se asigna a la columna variable de ensemble_av el factor mediana
  
  
  if(!is.null(observada)){ # Si Hay variable observada, para el caso de los fanchart de los equilibrios entonces:
    Observada = data.frame(step=date_serie[posicion],observada=observada[posicion]) # Guarda en Observada la uni?n por columnas de las fechas y la variable observada
    Observada$variable=as.factor("Observada") # Se asigna a la columna variable de observada el factor Observada
    
    if(!is.null(start_zoom)){
      Observada_zoom = data.frame(step=date_serie[posicion_zoom],observada=observada[posicion_zoom]) # Guarda en Observada_zoom la union por columnas de las fechas y la columna Observada en el rango de fechas de acercamiento
      Observada_zoom$variable=as.factor("Observada") # Se asigna a la columna variable de observada el factor Observada
    }
  } else {Observada = NULL; Observada_zoom = NULL} # Si No hay variable observada para el caso de los desequilibrios entonces No se crea ning?n data_frame apto para ggplot cn la variable observada
  
  dfplot=ddply(dfqm,.(step,delta),plyr::summarize,
               quantmin=min(value),
               quantmax=max(value) ) # Se guarda en dfplot el data.frame que est? dividido seg?n los percentiles
  
  mybreaks <- c(seq(from = 100*probs[1], to = 100*probs[length(probs)], by = 10)) # Genera una secuencia de 10 en 10 que inicia desde el percentil m?s peque?o probs[1] hasta el mayor probs[2] esto con el fin de usarlo en la leyenda que representa los percentiles
  colors <- c(alpha("firebrick2",0.9),alpha("tomato",0.65),alpha("tomato",0.45),alpha("tomato",0.2)) # Se crea una gama de colores con diferentes transparencias.
  mycols <- c(rev(colors[1:floor(length(probs)/2)]),colors[1:floor(length(probs)/2)]) # Se crea el vector de colores que van a representar a cada uno de los percentiles
  
  grafica<-function(df, obs, ensemble, zoom = c(TRUE,FALSE)){ # Se crea una funci?n auxiliar para realizar los fanchart
    plot <- ggplot() +
      geom_ribbon(data = df, aes(x = step, ymin = quantmin, 
                                 ymax =quantmax,group=rev(delta),
                                 fill = as.numeric(delta))) + # Genera los pol?gonos cuya ?rea representan la distancia que hay de un percentil a otro
      scale_fill_manual(values=c(mycols)) + # Se le agrega el color distintivo a cada pol?gono
      theme_minimal()+ labs(y="",x="") + # Se cambia el fondo por uno blanco y se quita el nombre de los ejes x y y
      ggtitle(nombre) + # Se agrega el t?tulo al gr?fico
      theme(plot.title = element_text(hjust=0.5), axis.text=element_text(colour="black"),plot.margin = unit(c(0.3,1,0.3,0.3),"cm")) + # Se centra el t?tulo y se cambian las m?rgenes en blanco a la gr?fica para que se vea m?s centrada
      theme(plot.caption = element_text(hjust=0.5), plot.caption.position = "plot" ) + # Se centran las leyendas
      scale_fill_discrete_gradient(colours = mycols, # Se genera la paleta de colores conun gradiente discreto para la leyenda que representa los percentiles
                                   breaks = seq(1,ncol(percentiles),1), # Se espec?fica cu?ntos cortes de color va a tener,en este caso, tantos como percentiles haya
                                   limits = c(1,ncol(percentiles)), # Se establecen desde donde hasta donde se va a realizar el corte de color
                                   bins = length(mybreaks)-1, # L?neas verticales de color negro que se ponen en los cuadros de colores para separar un color de otro
                                   labels = mybreaks, # Nombres que va a tener cada color en la leyenda. En este caso los nombres son los percentiles
                                   guide = guide_colorbar(title = "Percentiles", # T?tlo de la leyenda
                                                          title.position = "top", # Posici?n del t?tulo de la leyenda arriba del cuadro de colores
                                                          title.hjust = 0.5, # T?tulo de la leyenda centrado
                                                          frame.colour = "black",  # Color del t?tulo de la leyenda
                                                          ticks.colour = "black", 
                                                          barwidth=15)) + # Tama?o de los cuadros de colores de la leyenda
      geom_line(data=ensemble,aes(step,ensav,color="Mediana"),size=0.75) + # Se agrega la l?nea negra que representa la mediana o percentil 50
      (if(!is.null(obs)) geom_line(data=obs,aes(step,as.numeric(as.character(observada)),color="Observada"),size=0.75) ) + # Se agrega l?nea azul que representa la variable observada, esta ser? puesta s?lo si se est? realizando fanchart de los equilibrios.
      scale_color_manual(values=c("Mediana" = "black", "Observada" = "blue"))+labs(color="")+ # Se seleccionan los colores de cada l?nea. La mediana es negra,la de la observada es azul
      guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+  # El t?tulo de la leyenda de las l?neas de la mediana y la observada de ubica en el centro y arriba de las l?neas que representan la mediana y la observada
      theme(legend.position = "bottom", # La leyenda de las l?neas de la mediana y la observada se ubica en el centro 
            plot.title = element_text(face='bold')) +
      if(zoom==FALSE) xlim(c(min(date_serie),max(date_serie))) else scale_x_continuous(breaks=date_serie[posicion_zoom],labels=fechaz) # Se cambian los valores del eje X de acuerdo a si es una gr?fica de acercamiento o no
    return(plot) # La salida de esta funci?n es el gr?fico guardado en plot
  }
  
  # Grafica para exportar
  normal <- grafica(df = dfplot, ensemble = ensemble_av, obs = Observada, zoom = FALSE) # Se llama la funci?n grafica para que haga el fanchart del archivo de equilibrios o desequilibrios que ingres? en el par?metro file.
  
  
  if(export == TRUE){
    setwd(ruta) # Selecciona como directorio de trabajo la carpeta de la ruta especificada
    dir.create(paste0("fanchart_",carpeta))  # Se crea una carpeta nombrada "fanchart_" luego del gu?n bajo va el nombre de la carpeta que ingres? en el par?metro "carpeta"
    setwd(paste0(ruta,"/fanchart_",carpeta)) # Se selecciona como directorio de trabajo la carpeta creada en la l?nea anterior.
    write.xlsx(percentiles,paste0("percentiles_",nombre,".xlsx")) # Se exporta un archivo xlsx con la matriz de percentiles nombrado "percentiles_" despu?s de el gui?n va el nombre que ingres? en el par?metro "nombre"
    
    ggsave(normal,file=paste0(nombre,".PNG")) # Se guarda la gr?fica anterior en formato PNG en la carpeta "fanchart_..."
  }
  #else{
  #  plotly1 <- plotly::ggplotly(normal,tooltip = c("y","x","fecha","quantmin","quantmax"))
  #  return(plotly1)
  #  #htmlwidgets::saveWidget(as_widget(plotly1), file=paste0(nombre,".html"))
  #}
   
  
  if(!is.null(start_zoom)==TRUE){ # Si el usuario pidi? acercamiento entonces:
    conzoom <- grafica(df = dfplot_zoom, ensemble = ensemble_av_zoom, obs = Observada_zoom, zoom = TRUE) # Se llama la funci?n gr?fica para que haga el fanchart del acercamiento del fanchart creado anteriormente.
    ggsave(conzoom,file=paste0("zoom_",nombre,".PNG")) # Se exporta un archivo xlsx con la matriz de percentiles nombrado "percentiles_" despu?s de el gui?n va el nombre que ingres? en el par?metro "nombre" antecedido por la palabra zoom
    print(normal); print(conzoom) # Imprime en el panel de gr?ficos usualmente ubicado en lado derecho inferior de Rstudio,las dos gr?ficas de fanchart (original, acercamiento)
  } else return(normal) # Si no pidi? acercamiento s?lo imprime en elpanel de gr?ficos el fanchart original
  setwd(ruta) # Selecciona como directorio de trabajo la carpeta de la ruta especificada
}
