#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcion: Dash_misscelaneus(...)
# update: 20/05/2024
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


miscellaneous_v1.0 <- function(equilibrios, matriz_var, mediana_desequ_BEER){
  
  # Lista de fechas que se muestran
  fechas <- seq(from=as.Date("2000-03-01"), length.out = nrow(equilibrios), by="3 month") # to=as.Date("2024-03-01")
  fechas_list <- as.list(fechas)
  fechas_list <- lapply(fechas_list, as.character)
  names(fechas_list) <- format(as.yearqtr(fechas), "%Y-T%q")
  
  #::::::::::::::::::::::
  # Info BEER
  #::::::::::::::::::::::
  
  ind_total <- which(names(matriz_var) == "total") # Ubicacion de la variable total
  
  # Numero de modelos por metodologia usada en el BEER
  num_modelos <- matriz_var %>% filter(grupo == "Fiscal") %>% select(3:ind_total) %>% colSums(na.rm = T)
  names(num_modelos)[which(names(num_modelos) == "total")] <- "UNION"
  
  # Variables que estan en el BEER
  names_var <- matriz_var$variable
  
  # Nombres de Metodologias que se usaron para la corrida del BEER
  modelos_disponibles <- names(matriz_var)[3:(ind_total-1)]
  
  # Transformar formato de fecha de "mediana_desequ_BEER"
  mediana_desequ_BEER$fecha <- base::as.Date(mediana_desequ_BEER$fecha)
  
  plots_valuebox_infoBeer <- lapply(mediana_desequ_BEER[,-1], function(modelo){
    
    plot_ly(mediana_desequ_BEER) %>% 
      add_lines(x = ~fecha, y = modelo,
                color = I("white"), span = I(1),
                fill = 'tozeroy', alpha = 0.2) %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = T, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>% 
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
      )
  })
  
  # Colores asignados para cada metologia del BEER
  colores <- pal_futurama("planetexpress")(length(num_modelos)) # cambiar colores 
  names(colores) <- c("UNION", sort(names(matriz_var[(ind_total-1):3])) )   
  
  # Desviacon estandar del oficial de cada metologia del BEER, sin incluir ultima observacion
  desv_beer <- apply(mediana_desequ_BEER[-nrow(mediana_desequ_BEER),-1], 2, sd)
  
  # Funcion que hace las Value Box de las pestaña de Info BEER
  cajas_infoBEER <- function(x){
    value_box(
      title = paste0("Desequilibrio ", names(fechas_list)[nrow(mediana_desequ_BEER)]),
      value = paste0(round(mediana_desequ_BEER[nrow(mediana_desequ_BEER),x], 1),"%"),
      p(x),
      p(paste0("Número de modelos: ", num_modelos[x])),
      p(paste0("Desviación estandar: ± ", round(desv_beer[x], 2))),
      showcase = plots_valuebox_infoBeer[[x]],
      full_screen = TRUE,
      theme =value_box_theme(bg = colores[x], fg="#FFFFFF"))
  }
  
  cajas <- lapply(names(mediana_desequ_BEER)[-1], cajas_infoBEER)
  
  
  # OUTPUT
  
  lista_varios <- list("fechas_list"=fechas_list,
                       "ind_total" = ind_total,
                       "modelos_disponibles"=modelos_disponibles,
                       "cajas_infoBEER"=cajas)
  
  return(lista_varios)
  
  
}