server <- function(input, output, session) {
  #output[['Resultado']]<-renderText({'hola my lulu'})
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 0.Resumen -----------------------------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #:::::::::::::::::::::::::
  # NEW CUADRO RESUMEN
  #:::::::::::::::::::::::::
  
  #::::::::::::::::::
  # Filas PPC
  #::::::::::::::::::
  
  cr_fila_ppc <- reactive({
    if((input$boton.est_ppc > 0) & (input$modo_editor == T) ){ # input$start_editor
      # Filas PPC del panel de control
      as.data.frame(data.des_ppc() %>% dplyr::select('Fecha', 'Desalineamiento', 'Desalineamiento ce'))
      
    } else{
      # Filas PPC de la corrida anterior
      as.data.frame(datos_cuadro_resumen_ant %>% dplyr::select(all_of(c('Fecha', 'Desalineamiento', 'Desalineamiento ce'))))
    }
      
  })
  
  
  #::::::::::::::::
  # Fila BEER
  #::::::::::::::::
  
  cr_fila_beer <- as.data.frame(datos_cuadro_resumen_ant %>% dplyr::select(Fecha, BEER))
  
  
  #::::::::::::::::
  # Fila Filtrtos
  #::::::::::::::::
  
  cr_fila_filtros <- reactive({
    if((input$boton.est_filtros > 0) & (input$modo_editor == T)  ){ # input$start_editor
      
      # Fila Filtros del panel de control
      mediana_filt_desequ <- as.data.frame(datadesequ_med_prem())
      names(mediana_filt_desequ) <- c("Fecha","Filtros")
      return(mediana_filt_desequ)

    } else{
      
      # Fila Filtros de la corrida anterior
      as.data.frame(datos_cuadro_resumen_ant %>% dplyr::select(Fecha, Filtros))
      
    }
  })
  
  
  #::::::::::::::::
  # Fila FEER
  #::::::::::::::::

  cr_fila_feer <- as.data.frame(datos_cuadro_resumen_ant %>% dplyr::select(Fecha, FEER))
  
  
  #:::::::::::::::::::::::::::::::::::
  # Tabla Insumo para cuadro resumen
  #:::::::::::::::::::::::::::::::::::

  datos_cuadro_resumen <- reactive({
    
    cr <- cr_fila_ppc() %>%
      full_join(cr_fila_beer,    by=c("Fecha")) %>%
      full_join(cr_fila_filtros(), by=c("Fecha")) %>%
      full_join(cr_fila_feer,    by=c("Fecha"))
    
    cr$Fecha <- as.Date(cr$Fecha)
    cr <- cr %>% arrange(Fecha) # Para que las fechas no se cambien de posicion
    
    return(cr)
    
  })
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::
  # Creacion del cuadro Resumen con el formato creado
  #:::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output[["Cuadro_resumen"]] <- render_gt({

    #datos <- read_excel(file.path(wdd,"datos_cuadro_resumen.xlsx"))
    #datos_cuadro_resumen <- tab
    #DashResumen_cuadro_v0.1(datos_cuadro_resumen)

    DashResumen_cuadro_v1.0(datos_cuadro_resumen())

  })
  
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 1.Panel de Control ---------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Condicion para activar el panel de control---
  
  condicion <- reactiveVal(T)
  observeEvent(input$start_editor, {
    condicion(!condicion()) # usr_practicantegt52@banrep.gov.co
    if (condicion() & (startsWith(input$email, "driaorojas@gmail.com") |  startsWith(input$email, "msalazsi@banrep.gov.co"))  ) {
      showTab("nav", "Panel de control")
    } else {
      hideTab("nav", "Panel de control")
    }
  })

  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 1.1 Panel de Control - Filtros ----------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Selección de los datos 
  datos_inicio <- eventReactive(input$boton.est_filtros,{
    if (is.null(input$dataITCR_filtros)) {
      return(NULL)
    } else {
      data_temp<-read_excel(input$dataITCR_filtros$datapath, col_names = TRUE)
      data_temp[[names(data_temp)[1]]]<-as.Date(data_temp[[names(data_temp)[1]]],format="%Y-%m-%d")
      return(data_temp)
    }
  })
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.1.1.Estimación de los filtros a partir de los argumentos de los usuarios----
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  

  list_filtros <- reactive({
    # req(input$dataITCR_filtros)
    # req(input$anio.fin_filtros)
    # req(input$trim.fin_filtros)
    # req(input$anio.correccion)
    # req(input$trim.correccion)
    # req(input$lambda.HP)
    # req(input$freq.STL)
    # req(input$t.STL)
    # req(input$s.STL)
    # req(input$pl.CF)
    # req(input$pu.CF)
    # req(input$pl.RT)
    # req(input$pu.RT)
    # req(input$pl.BK)
    # req(input$pu.BK)
    # req(input$ma_order.PM)
    # req(input$h.HM)
    # req(input$p.HM)
    # req(input$nlag.BN)
    # req(input$nfix.BW)
    
    if((input$modo_editor == T) & (input$boton.est_filtros > 0)  ){
      
      trimestre_number.pc_filtro<-list('T1'=1,
                                       'T2'=2,
                                       'T3'=3,
                                       'T4'=4)
      
      
      #Definiendo los parametros de cada filtro
      if(input$default_HP == T){server_lambda.HP <- 4}
      else{
        if(as.numeric(input$lambda.HP) == 14400){server_lambda.HP <- 1}
        if(as.numeric(input$lambda.HP) == 1600){server_lambda.HP <- 4}
        if(as.numeric(input$lambda.HP) == 100){server_lambda.HP <- 12}
        
      }
      
      if(input$default_STL == T){server_t.STL <- 13; server_s.STL <- 7}else{server_t.STL <- as.numeric(input$t.STL); server_s.STL <- as.numeric(input$s.STL)}
      if(input$default_CF == T){server_pl.CF <- 2; server_pu.CF <- 40}else{server_pl.CF <- as.numeric(input$pl.CF); server_pu.CF <- as.numeric(input$pu.CF)}
      if(input$default_RT == T){server_pl.RT <- 2; server_pu.RT <- 40}else{server_pl.RT <- as.numeric(input$pl.RT); server_pu.RT <- as.numeric(input$pu.RT)}
      if(input$default_BK == T){server_pl.BK <- 2; server_pu.BK <- 40}else{server_pl.BK <- as.numeric(input$pl.BK); server_pu.BK <- as.numeric(input$pu.BK)}
      if(input$default_PM == T){server_ma_order.PM<-12}else{server_ma_order.PM<-as.numeric(input$ma_order.PM)}
      #if(input$default_HM == T){server_h.HM <- 8; server_p.HM <- 4}else{server_h.HM <- as.numeric(input$h.HM); server_p.HM <- as.numeric(input$p.HM)}
      #if(input$default_BN == T){server_nlag.BN<-2}else{server_nlag.BN<-as.numeric(input$nlag.BN)}
      if(input$default_BW == T){server_nfix.BW<-2}else{server_nfix.BW<-as.numeric(input$nfix.BW)}
      if(input$default_HP_uc == T){server_lambda.HP_uc <- 4} else{
        if(as.numeric(input$lambda.HP_uc) == 14400){server_lambda.HP_uc <- 1}
        if(as.numeric(input$lambda.HP_uc) == 1600){server_lambda.HP_uc <- 4}
        if(as.numeric(input$lambda.HP_uc) == 100){server_lambda.HP_uc <- 12}
      }
      if(input$default_HM_promedio == T){server_lh.HM_promedio <- 4; server_uh.HM_promedio <- 12; server_p.HM_promedio <- 4} else{
        server_lh.HM_promedio<-as.numeric(input$lh.HM_promedio); server_uh.HM_promedio<-as.numeric(input$uh.HM_promedio); server_p.HM_promedio<-as.numeric(input$p.HM_promedio)
      }
      
      
      
      DashFiltrosest_process_data_v1.0(datos=datos_inicio(),
                                       variables=names(datos_inicio())[-1],
                                       fin=c(as.numeric(input$anio.fin_filtros),trimestre_number.pc_filtro[[input$trim.fin_filtros]]),
                                       freq=4,
                                       cor_col = T,
                                       fecha_corr <- c(as.numeric(input$anio.correccion),trimestre_number.pc_filtro[[input$trim.correccion]]),
                                       filtros_series = c('Filtro Hodrick y Prescott',
                                                          #'Filtro Hamilton',
                                                          'Filtro Promedio Móvil',
                                                          'Filtro STL',
                                                          'Filtro Baxter-King',
                                                          'Filtro Christiano-Fitzgerald',
                                                          'Filtro Butterworth',
                                                          'Filtro Regresión Trigonométrica',
                                                          #'Filtro Beveridge-Nelson',
                                                          'Filtro Hodrick y Prescott a una cola',
                                                          'Filtro Hamilton promedio'),
                                       list_param = list('Filtro Promedio Móvil'           = list(ma_order=server_ma_order.PM),
                                                         'Filtro Hodrick y Prescott'       = list(freq=server_lambda.HP),
                                                         'Filtro STL'                      = list(t=server_t.STL,s=server_s.STL),
                                                         #'Filtro Hamilton'                 = list(h=server_h.HM,p=server_p.HM),
                                                         'Filtro Baxter-King'              = list(pl=server_pl.BK,pu=server_pu.BK,nfix=4),
                                                         'Filtro Christiano-Fitzgerald'    = list(pl = server_pl.CF, pu = server_pu.CF, theta = 1),
                                                         'Filtro Butterworth'              = list(nfix = server_nfix.BW),
                                                         'Filtro Regresión Trigonométrica' = list(pl=server_pl.RT,pu=server_pu.RT),
                                                         #'Filtro Beveridge-Nelson'         = list(nlag=server_nlag.BN),
                                                         'Filtro Hodrick y Prescott a una cola' = list(freq=server_lambda.HP_uc),
                                                         'Filtro Hamilton promedio' = list(lh=server_lh.HM_promedio, uh=server_uh.HM_promedio, p=server_p.HM_promedio)))
      
    }else{
      DashFiltrosest_process_data_v1.0(datos=dta.filtros_ant,
                                       variables=names(dta.filtros_ant)[-1],
                                       fin=dta.filtros_date.fin,
                                       freq=4,
                                       cor_col = T,
                                       fecha_corr <- dta.filtros_date.corr,
                                       filtros_series = c('Filtro Hodrick y Prescott',
                                                          #'Filtro Hamilton',
                                                          'Filtro Promedio Móvil',
                                                          'Filtro STL',
                                                          'Filtro Baxter-King',
                                                          'Filtro Christiano-Fitzgerald',
                                                          'Filtro Butterworth',
                                                          'Filtro Regresión Trigonométrica',
                                                          #'Filtro Beveridge-Nelson',
                                                          'Filtro Hodrick y Prescott a una cola',
                                                          'Filtro Hamilton promedio') )
      
      #' list_filtros <-  DashFiltrosest_process_datav_0.2(datos=dta.filtros_ant,
      #'                                                   variables=names(dta.filtros_ant)[-1],
      #'                                                   fin=dta.filtros_date.fin,
      #'                                                   freq=4,
      #'                                                   cor_col = T,
      #'                                                   fecha_corr <- dta.filtros_date.corr,
      #'                                                   filtros_series = c('Filtro Hodrick y Prescott',
      #'                                                                      #'Filtro Hamilton',
      #'                                                                      'Filtro Promedio Móvil',
      #'                                                                      'Filtro STL',
      #'                                                                      'Filtro Baxter-King',
      #'                                                                      'Filtro Christiano-Fitzgerald',
      #'                                                                      'Filtro Butterworth',
      #'                                                                      'Filtro Regresión Trigonométrica',
      #'                                                                      #'Filtro Beveridge-Nelson',
      #'                                                                      'Filtro Hodrick y Prescott a una cola',
      #'                                                                      'Filtro Hamilton promedio') )
      
      
    }
    
    
  })
  
  
 
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.1.2.Datos utilizados para la estimación de los filtros-----
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output[['show_data_table_estimation']]<- renderDataTable({
    req(input$boton.est_filtros)
    list_filtros()[["Vista datos"]] 
  },options = list(pageLength = 5,scrollY = "300px",paging = F,searching = F))
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.1.3.Resumen base de datos utilizada para la estimación-----
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Fecha inicial 
  output[['anio.inicio_filtros']] <- renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    year(list_filtros()[["Vista datos"]][['Fecha']][1])})
  output[['trim.inicio_filtros']] <- renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    paste0('T',quarter(list_filtros()[["Vista datos"]][['Fecha']][1])) })
  
  # Fecha final 
  output[['anio.final_filtros_pc']] <- renderText({
    req(input$boton.est_filtros)
    req(input$anio.fin_filtros)
    input$anio.fin_filtros
  })
  output[['trim.final_filtros_pc']] <- renderText({
    req(input$boton.est_filtros)
    req(input$trim.fin_filtros)
    input$trim.fin_filtros
  })
  
  # Fecha correccion colas
  output[['anio.correccion_pc']] <- renderText({
    req(input$boton.est_filtros)
    req(input$anio.correccion)
    input$anio.correccion
  })
  output[['trim.correccion_pc']] <- renderText({
    req(input$boton.est_filtros)
    req(input$trim.correccion)
    input$trim.correccion
  })
  
  # Nombre de la variable
  output[['nombre_variable_pc']] <- renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    names(list_filtros()[["Vista datos"]])[2]
  })
  
  # Número de observaciones
  output[['numero_observaciones_pc']] <- renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    NROW(list_filtros()[["Vista datos"]])
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.1.4.Gráfica comparacion equilibrios y desalineamientos versión anterior------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Equilibrios version anterior
  
  output[['equ_filtros_prev']]<-renderPlotly({
    plot_ly(dta_prev_equplot,x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range=c(min(dta_prev_equplot[['value']]),max(dta_prev_equplot[['value']])),
                     showgrid =F,visible = T, title = ""),
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
  
  # Desequilibrios version anterior
  output[['desequ_filtros_prev']]<-renderPlotly({
    plot_ly(dta_prev_desequplot,x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range=c(min(dta_prev_desequplot[['value']]),
                             max(dta_prev_desequplot[['value']]) ),
                     showgrid =F,visible = T, title = ""),
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
  
  # Equilibrios mediana preliminar
  dataequ_med_prem<-eventReactive(input$boton.est_filtros,{
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    req(input$series_filtro.pc)
    
    datatemp<-list_filtros()[["Data Equilibrios"]] %>% select(!c("ITCR-IPC Trimestral"))
    datatemp<-datatemp %>% select(c('Fecha',input$series_filtro.pc))
    final.data_medprem<-datatemp %>% select(c('Fecha'))
    
    if(length(input$series_filtro.pc)==1){
      final.data_medprem[['Mediana']]<-datatemp[[input$series_filtro.pc]]
    }else{
      final.data_medprem[['Mediana']]<-apply(datatemp[,-1],1,median,na.rm=T)
    }
    
    return(final.data_medprem)
  })
  
  # Ultimo valor de la mediana del equilibrio estimado
  output[['last_equmedprem']]<-renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    round(dataequ_med_prem()[['Mediana']][NROW(dataequ_med_prem()[['Mediana']])],2)
    
  })
  
  # Desviacion estandar de la mediana del equilibrio estimado
  output[['desv_equmedprem']]<-renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    paste0("Desviación estándar: ± ",round(sd(dataequ_med_prem()[['Mediana']][-NROW(dataequ_med_prem()[['Mediana']])]),2))
    
  })
  
  # Grafico preliminar del equilibrio
  output[['equ_filtros_new']]<-renderPlotly({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    plot_ly(dataequ_med_prem() %>% melt(id =1),x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range = c(min(dataequ_med_prem()[['Mediana']]), max(dataequ_med_prem()[['Mediana']])),
                     showgrid =F,visible = T, title = ""),
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
  
  # Desequilibrios mediana preliminar
  datadesequ_med_prem<-reactive({
    req(input$boton.est_filtros)
    final.data_desequprem<-list_filtros()[['Data Equilibrios']] %>% dplyr::select(c('Fecha'))
    final.data_desequprem[['Desequ_prem']]<-(list_filtros()[['Data Equilibrios']][['ITCR-IPC Trimestral']]-dataequ_med_prem()[['Mediana']])*100/dataequ_med_prem()[['Mediana']]
    
    return(as.data.frame(final.data_desequprem))
  })

  # Ultimo valor de la mediana del desequilibrio estimado
  output[['last_desequmedprem']]<-renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    round(datadesequ_med_prem()[['Desequ_prem']][[NROW(datadesequ_med_prem()[['Desequ_prem']])]],2)
    
  })
  
  # Desviacion estandar de la mediana del equilibrio estimado
  output[['desv_desequmedprem']]<-renderText({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    paste0("Desviación estándar: ± ",round(sd(datadesequ_med_prem()[['Desequ_prem']][-NROW(datadesequ_med_prem()[['Desequ_prem']])]),2))
    
  })
  
  # Grafico preliminar del equilibrio
  output[['desequ_filtros_new']]<-renderPlotly({
    req(input$boton.est_filtros)
    req(input$dataITCR_filtros)
    plot_ly(datadesequ_med_prem() %>% melt(id =1),x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range = c(min(datadesequ_med_prem()[['Mediana']]), max(datadesequ_med_prem()[['Mediana']])),
                     showgrid =F,visible = T, title = ""),
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
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.1.5.Gráfica preliminar de los filtros estimados------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Gráfica preliminar de los filtros estimados
  lapply(filtros_elegidos, function(var){
             
             # Equilibrios 
             output[[paste0('pc.equ_',var)]]<- renderPlotly({
               req(input$boton.est_filtros)
               
               plot_ly(list_filtros()[['Data Equilibrios']] %>%
                         select('Fecha','ITCR-IPC Trimestral',var) %>% melt(id = 1),
                       x = ~Fecha, y = ~value, color = ~variable,
                       type = 'scatter', mode = 'lines') %>%
                 layout(title = "ITCR-IPC y Tendencia Filtros Estadísticos",
                        xaxis = list(title = ""),
                        yaxis = list(title = ""),
                        legend = list(orientation = 'h')) })
             
             # Desalineamiento
             output[[paste0('pc.desequ_',var)]]<- renderPlotly({
               req(input$boton.est_filtros)
               
               plot_ly(list_filtros()[['Data Desequilibrios']] %>%
                         select('Fecha',var) %>% melt(id = 1),
                       x = ~Fecha, y = ~value, color = ~variable,
                       type = 'scatter', mode = 'lines') %>%
                 layout(title = "Desalineamiento",
                        xaxis = list(title = ""),
                        yaxis = list(title = ""),
                        legend = list(orientation = 'h')) 
               
             })
             
             
           })
  
  
  output[['prueba_plot_reaction1']]<- renderPlotly({
    req(input$boton.est_filtros)
    ggplotly(autoplot(ts(list_filtros()[["Data Equilibrios"]] %>% select(c("ITCR-IPC Trimestral"))), color = 'blue') )
    
    
  })
  
  dataInput_prueba<-reactive({
    req(input$boton.est_filtros)
    list_filtros()[["Data Equilibrios"]] %>% select(c('Fecha',"Filtro Hodrick y Prescott"))
  })
  
  output[['prueba_plot_reaction2']]<- renderPlotly({
    req(input$boton.est_filtros)
    plot_ly(dataInput_prueba() %>% melt(id=1), type = 'scatter', mode = 'lines',
            x=~Fecha, y = ~value)
  })
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.1.6.Descargar datos Filtros -------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  observeEvent(input$boton.est_filtros,  {
    
    req(input$series_filtro.pc)
    
    output[['download_filt']] <- downloadHandler(
      filename = function() {
        'filtros_estadisticos_TCRE.xlsx' # asignar nombre al archivo que se va a descargar
      },
      content = function(file){
        
        
        filtros_equ_desequ <- list("Equilibrios"=list_filtros()[["Data Equilibrios"]] %>% select(c("Fecha","ITCR-IPC Trimestral",input$series_filtro.pc)),
                                   "Desequilibrios"=list_filtros()[["Data Desequilibrios"]] %>% select(c("Fecha",input$series_filtro.pc)))
        
        write.xlsx(filtros_equ_desequ, file) # Descaragr Equilibrios y Desequilibrios en un mismo excel en hojasn aparte
        # Aca especificar la tabla que se va a descargar
      }
      
    )

  })
  
  
  
  
  
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 1.2 Panel de Control - PPC ----------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  # Selección de los datos
  datos_inicio_ppc <- eventReactive(input$boton.est_ppc,{
    if (is.null(input$dataITCR_PPC)) {
      return(NULL)
    } else {
      data_temp<-read_excel(input$dataITCR_PPC$datapath, col_names = TRUE)
      data_temp[[names(data_temp)[1]]]<-as.Date(data_temp[[names(data_temp)[1]]],format="%Y-%m-%d")
      return(data_temp)
    }
  })
  
  
  # Obteniendo desalineamiento y desviaciones estandar con base en el promedio.
  # data.des_ppc<-eventReactive(input$boton.est_ppc,{
  # 
  #   mes_ppc<-list("Ene"="01","Feb"="02","Mar"="03","Abr"="04",
  #                 "May"="05","Jun"="06","Jul"="07","Ago"="08",
  #                 "Sep"="09","Oct"="10","Nov"="11","Dic"="12")
  # 
  #   fecha_ini<-paste0(input$anio.ini_ppc,'-',mes_ppc[[input$mes.ini_ppc]],'-','01')
  #   fecha_fin<-paste0(input$anio.fin_ppc,'-',mes_ppc[[input$mes.fin_ppc]],'-','01')
  #   
  #   fecha_ini_ce<-paste0(input$anio.ini_ppc_ce,'-',mes_ppc[[input$mes.ini_ppc_ce]],'-','01')
  #   fecha_fin_ce<-paste0(input$anio.fin_ppc_ce,'-',mes_ppc[[input$mes.fin_ppc_ce]],'-','01')
  #   
  # #fecha_ini1<-paste0(1978,'-',mes_ppc[['Dic']],'-','01')
  #   
  #   
  #   DashPPC_process_data(datos=datos_inicio_ppc(),
  #                        variable=names(datos_inicio_ppc())[2],
  #                        fecha_prom = c(as.Date(fecha_ini),
  #                                       as.Date(fecha_fin)),
  #                        fecha_prom_ce = c(as.Date(fecha_ini_ce),
  #                                          as.Date(fecha_fin_ce)) )
  # 
  # })
  
  data.des_ppc<-reactive({
    
    if((input$modo_editor == T) & (input$boton.est_ppc > 0)){
      mes_ppc<-list("Ene"="01","Feb"="02","Mar"="03","Abr"="04",
                    "May"="05","Jun"="06","Jul"="07","Ago"="08",
                    "Sep"="09","Oct"="10","Nov"="11","Dic"="12")
      
      fecha_ini<-paste0(input$anio.ini_ppc,'-',mes_ppc[[input$mes.ini_ppc]],'-','01')
      fecha_fin<-paste0(input$anio.fin_ppc,'-',mes_ppc[[input$mes.fin_ppc]],'-','01')
      
      fecha_ini_ce<-paste0(input$anio.ini_ppc_ce,'-',mes_ppc[[input$mes.ini_ppc_ce]],'-','01')
      fecha_fin_ce<-paste0(input$anio.fin_ppc_ce,'-',mes_ppc[[input$mes.fin_ppc_ce]],'-','01')
      
      # fecha_ini<-paste0(1975,'-',mes_ppc[['Dic']],'-','01')
      # fecha_fin<-paste0(2024,'-',mes_ppc[['Ene']],'-','01')
      # fecha_ini_ce<-paste0(1985,'-',mes_ppc[['Ene']],'-','01')
      # fecha_fin_ce<-paste0(2024,'-',mes_ppc[['Ene']],'-','01')
      
      
      DashPPC_process_data_v1.0(datos=datos_inicio_ppc(),
                           variable=names(datos_inicio_ppc())[2],
                           fecha_prom = c(as.Date(fecha_ini),
                                          as.Date(fecha_fin)),
                           fecha_prom_ce = c(as.Date(fecha_ini_ce),
                                             as.Date(fecha_fin_ce)) )
      
    }else{
      dta_prev_ppc
      
    }
    
  })
  
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.2.1.Datos utilizados para la estimación del PPC-----
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output[['show_data_table_estimation_ppc']]<- renderDataTable({
    req(input$boton.est_ppc)
    data.des_ppc()
  },options = list(pageLength = 5,scrollY = "300px",paging = F,searching = F))

 
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.2.2.Gráfica comparacion desalineamientos versión anterior------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Desalinemaiento version anterior
  
  # Sin cambio estructural
  output[['desequ.ppc_prev']]<-renderPlotly({
    plot_ly(dtaprev_ppc.desequplot,x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range=c(min(dtaprev_ppc.desequplot[['value']]),max(dtaprev_ppc.desequplot[['value']])),
                     showgrid =F,visible = T, title = ""),
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
   
  # Con cambio estructural
  output[['desequ.ppc_prev_ce']]<-renderPlotly({
    plot_ly(dtaprev_ppc.desequplot_ce,x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range=c(min(dtaprev_ppc.desequplot_ce[['value']]),
                             max(dtaprev_ppc.desequplot_ce[['value']]) ),
                     showgrid =F,visible = T, title = ""),
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
  
  # Desalineamiento mediana preliminar
  # datadesequ_med_prem<-reactive({
  #   
  #   data.des_ppc() %>% dplyr::select(c('Fecha','Desalineamiento'))
  #   
  #   
  #   [['']] %>% dplyr::select(c('Fecha'))
  #   final.data_desequprem[['Desequ_prem']]<-(list_filtros()[['Data Equilibrios']][['ITCR-IPC Trimestral']]-dataequ_med_prem()[['Mediana']])*100/dataequ_med_prem()[['Mediana']]
  # 
  #   return(as.data.frame(final.data_desequprem))
  # })

  # Ultimo valor de la mediana del desequilibrio estimado
  # output[['last_desequmedprem']]<-renderText({
  #   req(input$boton.est_ppc)
  #   round(datadesequ_med_prem()[['Desequ_prem']][[NROW(datadesequ_med_prem()[['Desequ_prem']])]],2)
  # 
  # })

  # Grafico preliminar del desalineamiento sin cambio estructural
  output[['prem.desal_ppc']]<-renderPlotly({
    req(input$boton.est_ppc)

    plot_ly(data.des_ppc() %>% dplyr::select(c('Fecha','Desalineamiento')) %>% melt(id =1),x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range = c(min(data.des_ppc()[['Desalineamiento']]), max(data.des_ppc()[['Desalineamiento']])),
                     showgrid =F,visible = T, title = ""),
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
  # output[['prem.desal_ppc']]<-renderPlotly({
  #   plot_ly(dta_prev_ppc %>% dplyr::select(c('Fecha','Desalineamiento')) %>% melt(id =1),
  #           x=~Fecha,y=~value,
  #           type = 'scatter', mode = 'lines')
  # })
  
  
  # Ultimo valor del desalineamiento sin cambio estructural
  output[['last_desal.ppc']]<-renderText({
    req(input$boton.est_ppc)
    round(data.des_ppc()[['Desalineamiento']][[NROW(data.des_ppc()[['Desalineamiento']])]],2)
    
  })
  
  # Desviacion estandar del desalineamiento sin cambio estructural (no se tiene en cuenta la ultima obs)
  output[['desv_desal.ppc']] <- renderText({
    req(input$boton.est_ppc)
    paste0('Desviación estándar: ± ',round(sd(data.des_ppc()[['Desalineamiento']][-NROW(data.des_ppc()[['Desalineamiento']])]),2))

  })
  
  
  # Grafico preliminar del desalineamiento con cambio estructural
  output[['prem.desal_ppc_ce']]<-renderPlotly({
    req(input$boton.est_ppc)
    
    plot_ly(data.des_ppc() %>% dplyr::select(c('Fecha','Desalineamiento ce')) %>% melt(id =1),x=~Fecha,y=~value,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(visible = T, showgrid = F, title = ""),
        yaxis = list(range = c(min(data.des_ppc()[['Mediana']]), max(data.des_ppc()[['Mediana']])),
                     showgrid =F,visible = T, title = ""),
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
  
  # Ultimo valor del desalineamiento con cambio estructural
  output[['last_desal.ppc_ce']]<-renderText({
    req(input$boton.est_ppc)
    round(data.des_ppc()[['Desalineamiento ce']][[NROW(data.des_ppc()[['Desalineamiento ce']])]],2)
    
  })
  
  # Desviacion estandar del desalineamiento sin cambio estructural (no se tiene en cuenta la ultima obs)
  output[['desv_desal.ppc_ce']] <- renderText({
    req(input$boton.est_ppc)
    paste0('Desviación estándar: ± ',round(sd(data.des_ppc()[['Desalineamiento ce']][-NROW(data.des_ppc()[['Desalineamiento ce']])]),2))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 1.2.3.Descargar datos PPC -----------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  observeEvent(input$boton.est_ppc,{
    
    output[['download_ppc']] <- downloadHandler(
      filename = function() {
        'ppc_TCRE.xlsx' # asignar nombre al archivo que se va a descargar
      },
      content = function(file){
        req(input$series)
        
        ppc_desequ <- list("Desequilibrio"= data.des_ppc() %>% select(c('Fecha','ITCR-IPP','Promedio ITCR-IPP','Desalineamiento', 'Desv estandar promedio')) ,
                           "Desequilibrios_ce"= data.des_ppc() %>% select(c('Fecha','ITCR-IPP','Promedio ce ITCR-IPP','Desalineamiento ce', 'Desv estandar promedio ce')) )
        
        write.xlsx(ppc_desequ, file) # Descaragr Equilibrios y Desequilibrios en un mismo excel en hojasn aparte
        # Aca especificar la tabla que se va a descargar
      }
    )
    
  })
  
  
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 2.BEER ---------------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 2.1.BEER - Desequilibrios -------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.1.1.Grafico Equilibrios BEER -------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  # Grafico Equilibrios BEER
  output[['equ_suav_beer']] <- renderPlotly({
    req(input$percent)
    req(input$anio_beer)
    req(input$trimestre_beer)
    
    anio <- input$anio_beer
    trim <- as.numeric(input$trimestre_beer)
    zoom <- as.character(lubridate::make_date(anio, 3*trim, 1))
    
    
    DashBEER_equ_v1.0(equilibrios = equilibrios,
                 itcr = itcr,
                 freq = 4,
                 start = '2000-03-01',
                 percentiles = input$percent) %>% layout(xaxis = list(range = c(zoom, varios$fechas_list[[length(varios$fechas_list)]])) )
  })
  
  # Grafico Desequilibrios BEER
  output[['desequ_suav_beer']] <- renderPlotly({
    req(input$percent)
    req(input$anio_beer)
    req(input$trimestre_beer)
    
    anio <- input$anio_beer
    trim <- as.numeric(input$trimestre_beer)
    zoom <- as.character(lubridate::make_date(anio, 3*trim, 1))
    
    DashBEER_desequ_v1.0(equilibrios = equilibrios,
                    itcr = itcr,
                    freq = 4,
                    start = '2000-03-01',
                    percentiles = input$percent) %>% layout(xaxis = list(range = c(zoom, varios$fechas_list[[length(varios$fechas_list)]])) )
  })
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.1.2.Grafico Fanchart BEER -------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Fanchart equilibrios suavizados
  output[["fan_equ_suav"]] <- renderPlotly({
    req(input$anio_beer)
    req(input$trimestre_beer)
    
    # zoom <- which(varios$fechas_list == input$fecha_beer)
    # anio <- year(input$fecha_beer)
    # trim <- quarter(input$fecha_beer)
    
    anio <- input$anio_beer
    trim <- input$trimestre_beer
    #anio=2000; trim=4
    
    
    if(!(paste0(anio,"-T",trim) %in% names(varios$fechas_list)) ){
      plot_ly()
      } else{
        zoom <- which(names(varios$fechas_list) == paste0(anio,"-T",trim))
        
        if(NROW(itcr[zoom:length(itcr)]) == 0 | NROW(itcr[zoom:length(itcr)]) == 1){
          plot_ly()
          } else{
            fan_equ_suav <- DashBEER_fanchart_v1.0(equilibrios = equilibrios[zoom:nrow(equilibrios),],
                                          observada = itcr[zoom:length(itcr)],
                                          lim_probs = c(0.1, 0.9),
                                          freq = 4,
                                          start = c(anio, trim),
                                          start_zoom = NULL,
                                          end = NULL,
                                          nombre = "Densidad de los equilibrios de la TCR – ITCR NT" )
            ggplotly(fan_equ_suav, dynamicTicks = T)
      }
    }
  }
  )

  
  # Fanchart desequilibrios suavizados
  output[["fan_desequ_suav"]] <- renderPlotly({
    req(input$anio_beer)
    req(input$trimestre_beer)
    
    # zoom <- which(varios$fechas_list == input$fecha_beer)
    # anio <- year(input$fecha_beer)
    # trim <- quarter(input$fecha_beer)
    
    anio <- input$anio_beer
    trim <- input$trimestre_beer
    #anio=2010; trim=1
    
    if(!(paste0(anio,"-T",trim) %in% names(varios$fechas_list)) ){
      plot_ly()
    } else{
      zoom <- which(names(varios$fechas_list) == paste0(anio,"-T",trim))
      
      if(NROW(itcr[zoom:length(itcr)]) == 0 | NROW(itcr[zoom:length(itcr)]) == 1){
        plot_ly()
      } else{
      fan_desequ_suav <- DashBEER_fanchart_v1.0(equilibrios = desequilibrios[zoom:nrow(equilibrios),],
                                           observada = NULL,
                                           lim_probs = c(0.1,0.9),
                                           freq = 4,
                                           start = c(anio, trim),
                                           start_zoom = NULL,
                                           end = NULL,
                                           nombre = "Densidad de los desalineamientos de la TCR frente a su equilibrio",
                                           export = FALSE)
      ggplotly(fan_desequ_suav, dynamicTicks = T)
      
      }
    }
  })

  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.1.3.Grafico Mapa de Calor BEER --------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output[["heatmap_beer"]] <- renderPlotly({DashBEER_heatmap_V1.0(mediana_desequ_BEER,
                                                                  titulo="Desalineamientos de la TCR frente a su equilibrio por metodología")})
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.1.4.Grafico distribucion equilibrios BEER ---------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output[["dist_equ_suav3"]] <- renderPlotly({
    
    fechas.seq <- seq(from=base::as.Date("2000-03-01"), by = "3 month", length.out = nrow(equilibrios))
    
    if( (!input$fecha3 %in% fechas.seq) ){
      plot_ly()
    } else{
      
      DashBEER_distribucion_v1.0(equilibrios = equilibrios,
                             observada = itcr,
                             fecha = input$fecha3,
                             start = "2000-03-01",
                             freq=4)
      
      
    }
      
    })
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 2.2.BEER - Contribuciones -------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.2.1.Grafico Contribucion Equilibrios ----------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  DashBEER_contribuciones_react<-reactive({
    req(input$tipo_modelo_contri)
    
    # Obteniendo los datos de la union de los equilibrios por metodologia
    DashBEER_contribuciones_list <- DashBEER_contribuciones_UNION_v1.0(Matriz_var   = matriz_var,  
                                                                       lista_contri = lista_contribuciones,
                                                                       lista_ofi    = lista_equ_oficiales,
                                                                       periodo      = "anu", 
                                                                       modelos      = input$tipo_modelo_contri)
  })
  
  
  observeEvent(input$boton_equ,{
    req(input$fecha_equ)
    req(input$tipo_modelo_contri)
    req(input$top_nvariables)
   
    
    fecha       = input$fecha_equ
    tipo_modelo = input$tipo_modelo_contri
    top_var     = input$top_nvariables
    
    
    
    # Obteniendo los graficos con base en los equilibrios de DashBEER_contribuciones_list
    contri_equ_plots <- DashBEER_contribucion_equ_v1.0(lista_contribuciones = DashBEER_contribuciones_react(),
                                                       modelo       = tipo_modelo,
                                                       fecha        = fecha)
    
    output[["contri_equ"]] <- renderPlotly({
      contri_equ_plots$grafico
    })
    
    output[["contri_equ_zoom"]] <- renderPlotly({
      contri_equ_plots$grafico_zoom
    })
    
    output[["contri_top10"]] <- renderPlotly({
      DashBEER_contribucion_top10_v1.0(lista_contribuciones = DashBEER_contribuciones_react(),
                                       modelo = tipo_modelo,
                                       fecha = fecha,
                                       top=top_var)
    })
    
  })
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.2.2.Grafico participacion modelos ----------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  observeEvent(input$boton_contri, {
    req(input$tipo_modelo_contri)
    req(input$fecha_con)
    req(input$grupo)
    
    tipo_modelo = input$tipo_modelo_contri
    fecha_contr = input$fecha_con
    grupo       = input$grupo
    
    # Contribuciones de las variables sobre la contribucion final del grupo
    contri_grp_plots<-DashBEER_contribucion_v1.0(lista_contribuciones = DashBEER_contribuciones_react(),
                                                 grupo = grupo,
                                                 modelo = tipo_modelo,
                                                 fecha = fecha_contr)
    
    
    # Contribuciones por grupo de variables y modelo
    output[["contri_grupo_modelo"]] <- renderPlotly({contri_grp_plots$grafico})
    
    # Zoom del anteior garfico para una fecha especifica
    output[["contri_grupo_modelo_zoom"]] <- renderPlotly({contri_grp_plots$grafico_zoom})
   

  })
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## 2.3.BEER - Informacion  -------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.3.1.Cuadros resumen ----------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # En el UI
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ### 2.3.2.Grafico Barplot -----------------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output[["InfoBEER_barplot"]] <- renderPlotly({
    req(input$grupa_var)
    InfoBEER_barplot_v1.0(matriz_var, grupo_var = input$grupa_var)
  })
  
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ### 2.3.3.Tabla Coeficientes --------------------------------------------------
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    output[["Tabla_coeficientes"]] <- render_gt({
    req(input$grupa_var)
      
    # Cargar signo Esperado (Cargar base con la que se estimo el BEER)
    signo_esp <- signo_esp
    names(signo_esp) <- c("variable","signo")
    signo_esp$signo <- ifelse(signo_esp$signo == "positivo", "plus",ifelse(signo_esp$signo == "negativo","minus", NA) )
    
    # Funcion para calcular el ultimo crecimiento anual y trimestral de la variables
    cre_regresoras <- function(datos){
      
      # Numero total de filas
      n <- nrow(datos)
      
      # Calcular ultimo crecimiento trimestral y anual
      cre_trim  <- 100*((as.numeric(datos[n,-1]) / as.numeric(datos[n-1,-1])) - 1)
      cre_anual <- 100*((as.numeric(datos[n,-1]) / as.numeric(datos[n-4,-1])) - 1)
      
      # Calcular ultimo crecimiento trimestral y anual base suavizada
      #cre_trim  <- (as.numeric(datos[n,-1]) / as.numeric(datos[n-1,-1])) - 1
      #cre_anual <- (as.numeric(datos[n,-1]) / as.numeric(datos[n-4,-1])) - 1
      
      # Crear data.frame con nombre variable y ultimo crecimiento
      crecimiento <- data.frame(variable = names(datos)[-1], cre.Anual=cre_anual, cre.Trimestral=cre_trim)
      # agregar aca cre suav
      
      # OUTPUT
      return(crecimiento)
    }
    ultimo_crecimiento <- cre_regresoras(base_corrida)
    ultimo_crecimiento_suav <- cre_regresoras(base_suav)
    names(ultimo_crecimiento_suav)[-1] <- c("cre.Anual.suav", "cre.Trimestral.suav")
    
    # Tabla con signo esperado
    tab <- matriz_var %>% select(variable, grupo, tidyselect::starts_with("mediana"))
    tab <- tab %>% left_join(signo_esp, by=c("variable"="variable"))
    tab <- tab %>% select(variable, grupo, signo, tidyselect::starts_with("mediana") )
    tab <- tab %>% left_join(ultimo_crecimiento, by=c("variable"="variable"))
    tab <- tab %>% left_join(ultimo_crecimiento_suav, by=c("variable"="variable"))
    #tab <- tab %>% mutate(across(where(is.numeric), round, digits = 2))
    tab[,-c(1:3)] <- round(tab[,-c(1:3)], 3)
    
    # Posicion de las columans de los modelos
    pos_modelos <- grep("^mediana", names(tab))
    names(tab)  <- gsub("mediana\\.", "", names(tab)) # quitar mediana del nombre
    
    # Agregar iconos para crecimientos
    tab$anual.icon <- ifelse(tab$cre.Anual > 0, "long-arrow-up", "long-arrow-down")
    tab$trim.icon  <- ifelse(tab$cre.Trimestral > 0, "long-arrow-up", "long-arrow-down")
    
    tab$anual.icon.suav <- ifelse(tab$cre.Anual.suav > 0, "long-arrow-up", "long-arrow-down")
    tab$trim.icon.suav  <- ifelse(tab$cre.Trimestral.suav > 0, "long-arrow-up", "long-arrow-down")
    
    
    tab <- tab %>% select(variable, grupo, signo, pos_modelos, cre.Anual, anual.icon, cre.Trimestral, trim.icon,
                          cre.Anual.suav, anual.icon.suav, cre.Trimestral.suav, trim.icon.suav)
    
    
    #max_vec <- tab %>% select(mediana.VEC) %>% abs() %>% max(na.rm=T)
    #mycol <- col_numeric(palette = c("#0000FF","#FFFFFF","#FF0000"), domain=c(-max_vec, max_vec))
    
    
    # Tabla
    tab %>% filter(grupo == input$grupa_var) %>% # 
      gt() %>%
      tab_spanner(label="Mediana Coeficiente", columns= pos_modelos ) %>%
      tab_spanner(label=paste0("Crecimiento ", names(varios$fechas_list)[length(varios$fechas_list)]), columns= c(cre.Anual, anual.icon, cre.Trimestral, trim.icon ) ) %>%
      tab_spanner(label=paste0("Crecimiento tendencial ", names(varios$fechas_list)[length(varios$fechas_list)]), columns= c(cre.Anual.suav, anual.icon.suav, cre.Trimestral.suav, trim.icon.suav)) %>%
      cols_label(variable ='Variable', signo='Signo Esperado',
                 cre.Anual='Anual', cre.Trimestral='Trimestral', anual.icon="", trim.icon="",
                 cre.Anual.suav='Anual', cre.Trimestral.suav='Trimestral', anual.icon.suav="", trim.icon.suav="") %>%
      tab_header(title = md("**Resumen del comportamiento de las variables fundamentales**"), # titulo
                 subtitle = paste0("Grupo ", input$grupa_var)) %>% # input$grupa_var
      cols_align(align="center", columns = c(signo, pos_modelos )) %>%
      fmt_percent(columns = c(cre.Anual, cre.Trimestral, cre.Anual.suav, cre.Trimestral.suav), decimals = 1, scale_values = F) %>%
      fmt_icon(columns = c(signo, anual.icon, trim.icon, anual.icon.suav,trim.icon.suav,)) %>%
      tab_style(style = list( cell_text( color= "blue")),
                locations = cells_body(columns = signo,
                                       rows = signo == "minus")) %>%
      tab_style(style = list(cell_text( color= "red")),
                locations = cells_body(columns = signo,
                                       rows = signo == "plus")) %>%
      tab_style(style = list(cell_text(color="red")),
                locations = cells_body(columns = anual.icon,
                                       rows = anual.icon == "long-arrow-up")) %>%
      tab_style(style = list(cell_text(color="blue")),
                locations = cells_body(columns = anual.icon,
                                       rows = anual.icon == "long-arrow-down")) %>%
      tab_style(style = list(cell_text(color="red")),
                locations = cells_body(columns = trim.icon,
                                       rows = trim.icon == "long-arrow-up")) %>%
      tab_style(style = list(cell_text(color="blue")),
                locations = cells_body(columns = trim.icon,
                                       rows = trim.icon == "long-arrow-down")) %>%
      tab_style(style = list(cell_text(color="red")),
                locations = cells_body(columns = anual.icon.suav,
                                       rows = anual.icon.suav == "long-arrow-up")) %>%
      tab_style(style = list(cell_text(color="blue")),
                locations = cells_body(columns = anual.icon.suav,
                                       rows = anual.icon.suav == "long-arrow-down")) %>%
      tab_style(style = list(cell_text(color="red")),
                locations = cells_body(columns = trim.icon.suav,
                                       rows = trim.icon.suav == "long-arrow-up")) %>%
      tab_style(style = list(cell_text(color="blue")),
                locations = cells_body(columns = trim.icon.suav,
                                       rows = trim.icon.suav == "long-arrow-down")) %>% 
      cols_hide(columns = grupo)

  })
    
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ### 2.3.4.Grafica Variables regresora y filtro HP ---------------------------------
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    #lista_plots_regresoaras <- InfoBEER_plot_regresoras(base_corrida)
    
    output[["InfoBEER_plot_regresoras"]] <- renderPlotly({
    lista_plots_regresoaras[[input$name_var]]
    })
    
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ### 2.3.5.Tabla variables que mas aparecen ---------------------------------------
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    output[["Tabla_top_var"]] <- render_gt({
      
      datos2 <- matriz_var
      datos2[,3:(varios$ind_total-1)] <- datos2[,3:(varios$ind_total-1)] / datos2$total #* 100
      
      tab_var <- datos2 %>% select(variable, grupo, total, 3:(varios$ind_total-1)) %>%
        arrange(desc(total))
      
      tab_var[is.na(tab_var)] <- 0
      
      tab_var %>%
        #head(n=10) %>%
        gt() %>%
        tab_header(
          title = md("**Ranking de las variables fundamentales más relevantes en los modelos**")
        ) %>%
        #gt_plt_bar(column = total , keep_column = F, labels = T) %>%
        cols_label(variable = "Variable", grupo = "Grupo", total = "Número de modelos") %>%
        cols_align(align = "center", columns =total) %>%
        tab_spanner(label="Porcentaje modelos", columns = -c(1:3)) %>%
        fmt_percent(columns = -c(1:3), decimals = 1) %>%
        tab_style(
          style = cell_text(color = "black", weight="bold"),
          locations = list(cells_column_labels(everything()))
        )
      
    })
  
    
    
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # 3.Filtros-------------------------------
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    dataInput_equ<-reactive({
      req(input$series)
      #list_filtros[["Data Equilibrios"]] %>% select(c('Fecha',"ITCR-IPC Trimestral",input$series))
      list_filtros()[["Data Equilibrios"]] %>% select(c('Fecha',"ITCR-IPC Trimestral",input$series))
    })
    
    dataInput_desequ<-reactive({
      req(input$series)
      #list_filtros[["Data Desequilibrios"]] %>% select(c('Fecha',input$series))
      list_filtros()[["Data Desequilibrios"]] %>% select(c('Fecha',input$series))
      
      
    })
    
    
    dataInput_equ<-dataInput_equ%>%debounce(millis=1200)
    dataInput_desequ<-dataInput_desequ%>%debounce(millis=1200)
    
    #::::::::::::::::::::::::::
    ## 3.1.Graficas Equilibrios-----
    #::::::::::::::::::::::::::
    
    output[['Equilibrios_filtros']]<-renderPlotly({
      req(input$series)
      req(input$anio)
      req(input$trimestre)
      DashFiltrosest_equilibrios_filtro_v1.0(data=dataInput_equ(),anio=input$anio,trimestre=input$trimestre,filtros_series=input$series,
                                        titulo="Tendencia estimada del ITCR-IPC")
    })
    
    #::::::::::::::::::::::::::::::
    ## 3.2.Graficas Desequilibrios------
    #::::::::::::::::::::::::::::::
    
    output[['Desequilibrios_filtros']]<-renderPlotly({
      req(input$series)
      req(input$anio)
      req(input$trimestre)
      DashFiltrosest_desequilibrios_filtro_v1.0(datades=dataInput_desequ(),anio=input$anio,trimestre=input$trimestre,filtros_series=input$series,
                                           titulo="Desalineamiento de la TCR con respecto a la tendencia estimada")
      
    })
    
    #::::::::::::::::::::::::::::::::::::
    ## 3.3.Graficas Fanchart Equilibrios------
    #::::::::::::::::::::::::::::::::::::
    
    output[['Fanchart_Equilibrios_filtros']]<-renderPlotly({
      req(input$anio)
      req(input$trimestre)
      
      trimestre_number<-list('T1'='03',
                             'T2'='06',
                             'T3'='09',
                             'T4'='12')
      observada_dta<-dataInput_equ()%>%filter(Fecha>=paste0(input$anio,'-',trimestre_number[[input$trimestre]],'-','01'))
      observada_dta<-observada_dta[,"ITCR-IPC Trimestral"]
      
      if(NROW(observada_dta)==0 | NROW(observada_dta)==1){
        plot_ly()
      }else{
        DashFiltrosest_fanchart_dashplot_v1.0(datos=dataInput_equ()%>%
                                           filter(Fecha >= paste0(input$anio,'-',trimestre_number[[input$trimestre]],'-','01')) %>% 
                                           select(!c("ITCR-IPC Trimestral")) ,
                                         observada<- observada_dta,
                                         lim_probs<-c(0.1,0.9),
                                         start<-c(input$anio,which(names(trimestre_number)==names(trimestre_number[input$trimestre]))),
                                         start_zoom=NULL,
                                         end<-NULL,
                                         freq<-4,
                                         nombre<-'Densidad de las tendencias estimadas del ITCR-IPC')[['Fanchart']]}
      
      
    })
    
    
    #:::::::::::::::::::::::::::::::::::::::
    ## 3.4.Graficas Fanchart Desequilibrios------
    #:::::::::::::::::::::::::::::::::::::::
    
    output[['Fanchart_Desequilibrios_filtros']]<-renderPlotly({
      req(input$anio)
      req(input$trimestre)
      
      trimestre_number<-list('T1'='03',
                             'T2'='06',
                             'T3'='09',
                             'T4'='12')
      observada_dta<-dataInput_equ()%>%filter(Fecha>=paste0(input$anio,'-',trimestre_number[[input$trimestre]],'-','01'))
      observada_dta<-observada_dta[,"ITCR-IPC Trimestral"]
      
      if(NROW(observada_dta)==0 | NROW(observada_dta)==1){
        plot_ly()
      }else{
        DashFiltrosest_fanchart_dashplot_v1.0(datos=dataInput_desequ()%>%
                                           filter(Fecha >= paste0(input$anio,'-',trimestre_number[[input$trimestre]],'-','01'))  ,
                                         observada<- NULL,
                                         lim_probs<-c(0.1,0.9),
                                         start<-c(input$anio,which(names(trimestre_number)==names(trimestre_number[input$trimestre]))),
                                         start_zoom=NULL,
                                         end<-NULL,
                                         freq<-4,
                                         nombre<-'Densidad de los desalineamientos con respecto a las tendencias estimadas')[['Fanchart']]}
      
    } )
    
    
    #:::::::::::::::::::::::::::::::::::::::
    ## 3.5.Graficas Mapa de calor----------------
    #:::::::::::::::::::::::::::::::::::::::
    data_heatmap_filtros<-reactive({
      data_temp_hm<-dataInput_desequ() 
      data_temp_hm[['Mediana']]<-apply(data_temp_hm[,-1],1,median,na.rm=T)
      
      return(data_temp_hm)
    })
    
    output[['Mapacalor_filtros']]<-renderPlotly({
      DashFiltrosest_heatmap_plot_v1.0(datos=data_heatmap_filtros(),n_ultimos=8,
                                  titulo='Desalineamientos de la TCR frente a su la tendencia estimada para cada filtro')
      
    } )
    
    #:::::::::::::::::::::::::::::::::::::::
    ## 3.7.Graficas distribucion------------
    #:::::::::::::::::::::::::::::::::::::::
    
  
    
    output[['dist_equ_suav_filtros']]<-renderPlotly({
      req(input$fecha.dist_filtros)
      
      num_filas <- nrow(dataInput_equ() %>% filter(Fecha >= '2000-01-01') %>% select(!c("Fecha","ITCR-IPC Trimestral")))
      fechas.seq <- seq(from=base::as.Date("2000-03-01"), by = "3 month", length.out = num_filas)
      
      if(!(input$fecha.dist_filtros %in% fechas.seq) | ncol(dataInput_equ()) < 6){
        plot_ly()
      } else{
        
        DashFiltrosest_distribucion_v1.0(
          equilibrios=dataInput_equ() %>% filter(Fecha >= '2000-01-01') %>% select(!c("Fecha","ITCR-IPC Trimestral")),
          observada=(dataInput_equ() %>% filter(Fecha >= '2000-01-01'))[['ITCR-IPC Trimestral']],
          fecha=input$fecha.dist_filtros,
          start='2000-03-01',
          freq=4
        )
        
      }
      
    })
    
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # 4.PPC-------------------------------
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    # Funcion para obtener el nombre del mes completo
    nombre_mes <- function(mes){
      
      # Lista de meses completos y abreviados
      meses_completos <- c("enero", "febrero", "marzo", "abril",
                           "mayo", "junio", "julio", "agosto",
                           "septiembre", "octubre", "noviembre", "diciembre")
      meses_abreviados <- c("Ene","Feb","Mar","Abr",
                            "May","Jun","Jul","Ago",
                            "Sep","Oct","Nov","Dic")
      
      indice <- match(mes, meses_abreviados)
      mes_name <- meses_completos[indice]
      
      return(mes_name)
      
    }
    
    
    
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ## 4.1 Sin cambio estructural ----------------------------------------------
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    # Textos signos de ?
    text_equ_ppc <- reactive({
      
      if((input$boton.est_ppc > 0) & (input$modo_editor == T) ){ # input$start_editor
        
        # Nombre completo dle mes
        mes_name <- nombre_mes(input$mes.ini_ppc)
        return(paste0("TCR de largo plazo calculada a partir del promedio histórico desde ",mes_name," de ",input$anio.ini_ppc))
        
      } else{
        paste0("TCR de largo plazo calculada a partir del promedio histórico desde enero de 1970.")
      }
    })
    
    output[['text_equ_ppc']] <- renderText({text_equ_ppc()})
    
    text_dsequ_ppc <- reactive({
      
      if((input$boton.est_ppc > 0) & (input$modo_editor == T) ){ # input$start_editor
        
        # Nombre completo dle mes
        mes_name <- nombre_mes(input$mes.ini_ppc)
        return(paste0("Desalineamiento de la TCR frente al promedio historico desde ",mes_name," de ",input$anio.ini_ppc))
        
      } else{
        paste0("Desalineamiento de la TCR frente al promedio histórico desde enero de 1970")
      }
    })
    
    output[['text_desequ_ppc']] <- renderText({text_dsequ_ppc()})
    
    
    
    # Equilibrio sin cambio estructural
    output[["Equilibrios_ppc"]] <- renderPlotly({
      #req(input$dataITCR_PPC)
      req(input$anio_ppc)
      req(input$trimestre_ppc)
      anio <- as.numeric(input$anio_ppc)
      trim <- as.numeric(input$trimestre_ppc)
      
      DashPPC_equ_v1.0(data.des_ppc(), anio, trim)$ITCR_IPP
    })
    
    # Desquilibrio sin cambio estructural
    output[["Desequilibrios_ppc"]] <- renderPlotly({
      #req(input$dataITCR_PPC)
      req(input$anio_ppc)
      req(input$trimestre_ppc)
      anio <- as.numeric(input$anio_ppc)
      trim <- as.numeric(input$trimestre_ppc)
      
      DashPPC_desequ_v1.0(data.des_ppc(), anio, trim)$desalineamiento
    })
    
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ## 4.2 Con cambio estructural ----------------------------------------------
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    # Texto signo ? equ con ce
    text_equ_ppc_ce <- reactive({
      
      if((input$boton.est_ppc > 0) & (input$modo_editor == T) ){ # input$start_editor
        
        # Nombre completo del mes
        mes_name <- nombre_mes(input$mes.ini_ppc_ce)
        return(paste0("TCR de largo plazo calculada a partir del promedio histórico desde ",mes_name," de ",input$anio.ini_ppc_ce,", fecha desde donde se asume existe un cambio estructural en el comportamiento de la TCR."))
        
      } else{
        paste0("TCR de largo plazo calculada a partir del promedio histórico desde agosto de 1985, fecha desde donde se asume existe un cambio estructural en el comportamiento de la TCR.")
      }
    })
    output[['text_equ_ppc_ce']] <- renderText({text_equ_ppc_ce()})
    
    # Texto signo ? desequ con ce
    text_dsequ_ppc_ce <- reactive({
      
      if((input$boton.est_ppc > 0) & (input$modo_editor == T) ){ # input$start_editor
        
        # Nombre completo del mes
        mes_name <- nombre_mes(input$mes.ini_ppc_ce)
        return(paste0("Desalineamiento de la TCR frente al promedio histórico con cambio estructural desde ",mes_name," de ",input$anio.ini_ppc_ce))
        
      } else{
        paste0("Desalineamiento de la TCR frente al promedio histórico con cambio estructural desde agosto de 1985.")
      }
    })
    output[['text_desequ_ppc_ce']] <- renderText({text_dsequ_ppc_ce()})
    
    # Equilibrio con cambio estructural
    output[["Equilibrios_ppc_ce"]] <- renderPlotly({
      #req(input$dataITCR_PPC)
      req(input$anio_ppc)
      req(input$trimestre_ppc)
      anio <- as.numeric(input$anio_ppc)
      trim <- as.numeric(input$trimestre_ppc)
      
      DashPPC_equ_v1.0(data.des_ppc(), anio, trim)$ITCR_IPP_ce
    })
    
    # Desquilibrio con cambio estructural
    output[["Desequilibrios_ppc_ce"]] <- renderPlotly({
      #req(input$dataITCR_PPC)
      req(input$anio_ppc)
      req(input$trimestre_ppc)
      anio <- as.numeric(input$anio_ppc)
      trim <- as.numeric(input$trimestre_ppc)
      
      DashPPC_desequ_v1.0(data.des_ppc(), anio, trim)$desalineamiento_ce
    })
    
  
}



