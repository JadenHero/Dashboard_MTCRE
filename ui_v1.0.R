ui <- page_navbar(title = "MTCRE",
                  underline = T,
                  id='nav',
                  theme = bs_theme(version = 5,
                                   preset = 'zephyr',
                                   bg = "#fff",
                                   fg = "#093CA2",#4557BE
                                   primary = "#57ACCE",
                                   secondary = "#C6DBDB",
                                   success = "#EB8D1D"),
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  # 0.Resumen -------------------------------------------------------------------------------
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  
                  nav_panel(title="Resumen",
                            mainPanel(
                              gt_output("Cuadro_resumen"),
                              width = 12,
                              strong("Dashboard elaborado por:"),
                              br(), # salto de linea
                              div("Daniel Felipe Riaño Rojas", style="color::blue"),
                              div("Marlon Salazar Silva", style="color::blue"),
                              ),
                            # card(
                            #   card_body(
                            #   #   p("Nota: Las diferentes metodologías incluyen en su mayoría datos observados hasta el primer trimestre de 2021. Para el segundo trimestre de 2021 se requirió hacer supuestos sobre las variables. En los cálculos se utilizaron los índices trimestrales (promedio) de tasa  de cambio real observada hasta el primer trimestre de 2021. Para el segundo trimestre de 2021 se utilizó el promedio observado entre abril y mayo."),
                            #   #   p("PPC: Los modelos de paridad del poder adquisitivo consideran al equilibrio de la TCR como su promedio histórico y su promedio histórico con cambio estructural, respectivamente. Todos se calculan sobre la ITCR-IPP según comercio no tradicional total con datos mensuales desde enero de 1970. Para el promedio con cambio estructural, de acuerdo con Cárdenas (2001)  y Misas y Ramírez (2006) , se toma el cambio de nivel a partir de agosto de 1985. El intervalo corresponde a +/- una desviación estándar de la desviación trimestral desde 1970 a 2021.")),
                            #   #   p("BEER:La literatura ha identiﬁcado que la estimación de la TCRE por medio de la metodología BEER tiende a ser muy sensible a la especiﬁcacióndel modelo y al conjunto de fundamentales utilizados. Por lo tanto, la estrategia empírica aquí utilizada busca tener en cuenta diferentes especiﬁcacionesdel modelo y tipos de fundamentales para obtener así “miles” de estimaciones de la TCRE. Además, considera diferentes metodologías que buscan cuantificar relaciones de largo plazo entre la tasa de cambio real y sus fundamentales. En particular, se utiliza la mediana de la combinación de estos resultados como aproximación a la TCRE. La base de datos incluye cerca de 40 variables organizadas en 6 grupos de fundamentales (Términos de intercambio, Endeudamiento externo, Diferencial de tasas, Productividad, Gasto público y Otros) cada uno con un canal identificado en la literatura. Por medio de las metodologías VEC, ARDL y DOLS se estimaron todos los posibles modelos que resultaron al combinar las variables de estos grupos, exigiendo que siempre una variable de cada fundamental esté presente en el modelo. Una vez estimados estos modelos se seleccionaron los que cumplieron criterios asociados con el signo de las variables teniendo en cuenta la literatura especializada y aquellos que cumplieran criterios de significancia y pruebas sobre los residuales. Para el caso del equilibrio suavizado se usan los parámetros estimados y el componente tendencial de las variables, esto último usando un filtro de Hodrick y Prescott . En estas medidas se utiliza el ITCR-IPC según comercio no tradicional total. El intervalo del desalineamiento se construye a partir de la desviación estándar de los desalineamientos desde el primer trimestre del 2000. Los datos del primer trimestre de 2024 corresponden al promedio de enero y febrero, junto con una estimación preliminar de marzo."),
                            #   #   p("Filtro de HP: En esta medida se utiliza el ITCR-IPCsegún comercio no tradicional total. El intervalo corresponde a +/- una desviación estándar de la desviación trimestral desde 1970T1 a 2023T4. Los datos del primer trimestre de 2024 corresponden al promedio de enero y febrero, junto con una estimación preliminar de marzo."),
                            #   #   p("FEER: Se considera que la CC de equilibrio corresponde al promedio histórico desde 1994 hasta 2024. Para 2024 se utiliza el pronóstico de la CC de acuerdo alos supuestos de la balanza de pagos construidos por el ET del Banco. El intervalo se construye a partir de la desviación estándar desde el primer trimestre del 2000 del desalineamiento de cada medida.")
                            #   ) ),
                            
                            
                            bslib::input_switch("modo_editor",bsicons::bs_icon("rocket-takeoff-fill", class = "ms-auto"),F),
                             shiny::conditionalPanel(
                               condition = "input.modo_editor == true",
                              fixedRow(textInput("email","Correo elecctronico"),
                                       bslib::input_switch("start_editor","Iniciar modo editor",F))
                
                              ),
                            
                            textOutput("prueba_rm")
                            ),
                  
                  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  # 1. Panel de Control --------------------------------------------------------
                  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  
                  nav_menu(title="Panel de control",
                           
                           
                           #:::::::::::::::::::::::::::::::
                           ### 1.1.Filtros estadisticos----  
                           #:::::::::::::::::::::::::::::::
                           
                           nav_panel("Filtro",
                                     layout_sidebar(
                                       sidebar = sidebar(
                                                   # Cargar base de datos
                                                   fileInput("dataITCR_filtros", "Seleccione los datos del ITCR .xlsx",
                                                             accept = c(".xlsx")),
                                                   
                                                   # Seleccionar inputs para estimar el filtro
                                                   selectInput("anio.fin_filtros",label='Seleccionar año final:',choices = seq(1973,year(Sys.Date())),selected = 2024),
                                                   selectInput("trim.fin_filtros",label='Seleccionar trimestre del año final:',choices = c("T1","T2","T3","T4")),
                                                   
                                                   # Seleccionar la correccion de las colas
                                                   selectInput("anio.correccion",label='Seleccionar año de los pronósticos que se incluyen en la estimación:',choices = seq(2024,year(Sys.Date())+2)),
                                                   selectInput("trim.correccion",label='Seleccionar trimestre de los pronósticos que se incluyen en la estimación:',choices = c("T1","T2","T3","T4")),
                                                   
                                                   # Seleccionar los filtros que se desean estimar
                                                   selectInput("series_filtro.pc", "Filtros estadísticos:", 
                                                               choices = c('Filtro Hodrick y Prescott',
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
                                                               multiple = TRUE,
                                                               selected = c('Filtro Hodrick y Prescott',
                                                                            #'Filtro Hamilton',
                                                                            'Filtro Promedio Móvil',
                                                                            'Filtro STL',
                                                                            'Filtro Baxter-King',
                                                                            'Filtro Christiano-Fitzgerald',
                                                                            'Filtro Butterworth',
                                                                            'Filtro Regresión Trigonométrica',
                                                                            #'Filtro Beveridge-Nelson'
                                                                            'Filtro Hodrick y Prescott a una cola',
                                                                            'Filtro Hamilton promedio')
                                                   ),
                                                   
                                                   #:::::::::::::::::::::::::
                                                   # Boton 'Estimar filtros'
                                                   #:::::::::::::::::::::::::
                                                   actionButton(inputId = "boton.est_filtros",label="Estimar filtros"),
                                                   
                                                   #:::::::::::::::::::::::::
                                                   # Descargar datos filtros
                                                   #:::::::::::::::::::::::::
                                                   bslib::input_switch("descargar_datos_filt",
                                                                       label="Descargar datos filtros estimados", value=F),
                                                   shiny::conditionalPanel(
                                                     condition = "input.descargar_datos_filt == true",
                                                     fixedRow(downloadButton("download_filt", label="Descargar .xlsx")) ),
                                                   
                                                   width = 500,
                                                   open = F,
                                                   fluid = T
                                         
                                                ),
                                               position='left',
                                               fillable = T,
                                              
                                               
                                            fluidRow(
                                               card(
                                                 card_body(
                                                   
                                                   #::::::::::::::::::::::::::
                                                   # Comparación estimaciones
                                                   #::::::::::::::::::::::::::
                                                   
                                                   layout_column_wrap(
                                                     # Equilibrio filtro estadistico ronda anterior
                                                     value_box(title='Filtros estadísticos: última estimación del equilibrio tendencial',
                                                               value = round(dta_prev_equplot[['value']][NROW(dta_prev_equplot[['value']])],2),
                                                               p(paste0("Desviación estándar: ± ", round(desv_equ_filt_ant, 2))),
                                                               showcase = plotlyOutput('equ_filtros_prev'),
                                                               full_screen = TRUE,
                                                               theme = "success"
                                                     ),
                                                     
                                                     
                                                     # Desequilibrio filtro estadistico ronda anterior
                                                     value_box(title='Filtros estadísticos: última estimación del desalineamiento',
                                                               value = p(round(dta_prev_desequplot[['value']][NROW(dta_prev_desequplot[['value']])],2),'%'),
                                                               p(paste0("Desviación estándar: ± ", round(desv_desequ_filt_ant, 2))),
                                                               showcase = plotlyOutput('desequ_filtros_prev'),
                                                               full_screen = TRUE,
                                                               theme = "success"
                                                     ),
                                                   ),
                                                   layout_column_wrap(
                                                     # Equilibrio estimado con los parametros actuales
                                                     value_box(title='Mediana filtros del equilibrio tendencial',
                                                               value = p(textOutput('last_equmedprem',inline = T)),
                                                               p(textOutput('desv_equmedprem')),
                                                               showcase = plotlyOutput('equ_filtros_new'),
                                                               full_screen = T,
                                                               theme = "blue"),
                                                     
                                                     
                                                     # Desequilibrio estimado con los parametros actuales
                                                     value_box(title='Desalineamiento mediana filtros',
                                                               value = p(textOutput('last_desequmedprem',inline = T)),
                                                               p(textOutput('desv_desequmedprem')),
                                                               showcase = plotlyOutput('desequ_filtros_new'),
                                                               full_screen = T,
                                                               theme = "blue"),
                                                     
                                                   )
                                                   
                                                 )
                                                 
                                               ),
                                               
                                               #::::::::::::::::::::::::::
                                               # Descripcion de los datos
                                               #::::::::::::::::::::::::::
                                               
                                               navset_card_underline(
                                                 title='Datos utilizados para la estimación de los filtros',
                                                 
                                                 # Datos utilizados para la estimación de los filtros
                                                 nav_panel('Datos estimación',
                                                           dataTableOutput("show_data_table_estimation")
                                                 ),
                                                 nav_panel('Resumen base de datos',
                                                           card_body(
                                                             layout_column_wrap(
                                                               # Resumen de los datos utilizados para la estimacion
                                                               value_box(title='Fechas',
                                                                         bsicons::bs_icon("calendar2-check"),
                                                                         theme=value_box_theme(bg='#F3FCFF',fg="#0B538E"),
                                                                         p('Fecha incial:',textOutput('anio.inicio_filtros',inline=T),textOutput('trim.inicio_filtros',inline=T)),
                                                                         p('Fecha final:',textOutput('anio.final_filtros_pc',inline=T),textOutput('trim.final_filtros_pc',inline=T)),
                                                                         p('Fecha corrección colas:',textOutput('anio.correccion_pc',inline=T), textOutput('trim.correccion_pc',inline=T) )) ,
                                                               
                                                               value_box(title='Nombre variables',
                                                                         showcase=bsicons::bs_icon("feather"),
                                                                         theme=value_box_theme(bg='#F3FCFF',fg="#0B538E"),
                                                                         value=p(textOutput('nombre_variable_pc',inline=T))),
                                                               
                                                               value_box(title='Número de observaciones',
                                                                         value = p(textOutput('numero_observaciones_pc',inline = T)),
                                                                         showcase=bsicons::bs_icon("grid-fill"),
                                                                         theme=value_box_theme(bg='#F3FCFF',fg="#0B538E")) 
                                                             )
                                                           )
                                                           
                                                 )
                                                 
                                               ),
                                                
                                               #:::::::::::::::::::::::::::::::::::
                                               # Grafica preliminar de los filtros
                                               #:::::::::::::::::::::::::::::::::::
                                               
                                               column(12,navset_card_underline(
                                                 title='Estimación filtros',
                                                 nav_panel('Hodrick-Prescott',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               # Barra desplegable para grafico Distribucion
                                                               bslib::input_switch("default_HP","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_HP == false",
                                                                 selectInput('lambda.HP',label='Lambda',choices = c(14400,1600,100),selected = 1600)
                                                               ),
                                                               title = "Parámetros",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Hodrick y Prescott"),
                                                               plotlyOutput("pc.desequ_Filtro Hodrick y Prescott")
                                                             )
                                                           ),
                                                           
                                                 ),
                                                 
                                                 nav_panel('STL',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               # Barra desplegable para grafico Distribucion
                                                               bslib::input_switch("default_STL","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_STL == false",
                                                                 selectInput('freq.STL',label='Frecuencia',choices = c(1,4,12),selected = 4),
                                                                 #selectInput('s.STL',label='Ventana estacional',choices = c(c(7:24)), selected = 7),
                                                                 #selectInput('t.STL',label='Ventana tendencial',choices = c(NULL,seq(from=13,to=25,by=2)), selected = 13)
                                                                 sliderInput('s.STL',label='Ventana estacional',min=7,max=24,value=7,step=1),
                                                                 sliderInput('t.STL',label='Ventana tendencial',min=13,max=30,value=13,step=1)
                                                               ),
                                                               title = "Parámetros",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro STL"),
                                                               plotlyOutput("pc.desequ_Filtro STL")
                                                             )
                                                           )
                                                 ),
                                                 nav_panel('Christiano-Fitzgerald',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               bslib::input_switch("default_CF","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_CF == false",
                                                                 #selectInput('pl.CF',label='Período mínimo de oscilación',choices = c(2:24),selected = 2),
                                                                 #selectInput('pu.CF',label='Período máximo de oscilación',choices = seq(from=10,to=80,by=10),selected = 40)
                                                                 sliderInput('pl.CF',label='Período mínimo de oscilación',min = 2,max=4,value=2,step=1),
                                                                 sliderInput('pu.CF',label='Período mínimo de oscilación',min = 10,max=80,value=40,step=10)
                                                               ),
                                                               title = "Parámetros",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Christiano-Fitzgerald"),
                                                               plotlyOutput("pc.desequ_Filtro Christiano-Fitzgerald")
                                                             )
                                                           )
                                                 ),
                                                 nav_panel('Regresión Trigonométrica',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               bslib::input_switch("default_RT","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_RT == false",
                                                                 #selectInput('pl.RT',label='Período mínimo de oscilación',choices = c(2:24),selected = 2),
                                                                 #selectInput('pu.RT',label='Período máximo de oscilación',choices = seq(from=10,to=80,by=10),selected = 40)
                                                                 sliderInput('pl.RT',label='Período mínimo de oscilación',min=2,max=4,value=2,step=1),
                                                                 sliderInput('pu.RT',label='Período máximo de oscilación',min=10,max=80,step=10,value = 40)
                                                               ),
                                                               title = "Opciones",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Regresión Trigonométrica"),
                                                               plotlyOutput("pc.desequ_Filtro Regresión Trigonométrica")
                                                             )
                                                           )
                                                 ),
                                                 nav_panel('Baxter-King',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               bslib::input_switch("default_BK","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_BK == false",
                                                                 #selectInput('pl.BK',label='Período mínimo de oscilación',choices = c(2:24),selected = 2),
                                                                 #selectInput('pu.BK',label='Período máximo de oscilación',choices = seq(from=10,to=80,by=10),selected = 40)
                                                                 sliderInput('pl.BK',label='Período mínimo de oscilación',min=2,max=4,value=2,step=1),
                                                                 sliderInput('pu.BK',label='Período máximo de oscilación',min=10,max=80,step=10,value = 40)
                                                               ),
                                                               title = "Opciones",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Baxter-King"),
                                                               plotlyOutput("pc.desequ_Filtro Baxter-King")
                                                             )
                                                           )
                                                 ),
                                                 nav_panel('Promedio Móvil',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               bslib::input_switch("default_PM","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_PM == false",
                                                                 sliderInput('ma_order.PM',label='Orden media móvil',min=4,max=24,step=2,value = 12)
                                                               ),
                                                               title = "Opciones",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Promedio Móvil"),
                                                               plotlyOutput("pc.desequ_Filtro Promedio Móvil")
                                                             )
                                                           )
                                                 ),
                                                 # nav_panel('Hamilton',
                                                 #           card_body(
                                                 #             popover(
                                                 #               bsicons::bs_icon("gear", class = "ms-auto"),
                                                 #               # Barra desplegable para grafico Distribucion
                                                 #               bslib::input_switch("default_HM","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                 #               shiny::conditionalPanel(
                                                 #                 condition = "input.default_HM == false",
                                                 #                 sliderInput('h.HM',label='Períodos de adelantos',min=8,max=24,step=2,value = 8),
                                                 #                 sliderInput('p.HM',label='Períodos de rezagos',min=4,max=24,step=2,value= 4)
                                                 #               ), # Ajustar el ancho de la barra
                                                 #               title = "Opciones",
                                                 #               placement = "bottom"
                                                 #             ),
                                                 #             layout_column_wrap(
                                                 #               width = 1/2,
                                                 #               plotlyOutput("pc.equ_Filtro Hamilton"),
                                                 #               plotlyOutput("pc.desequ_Filtro Hamilton")
                                                 #             )
                                                 #           )
                                                 # ),
                                                 # nav_panel('Beveridge-Nelson',
                                                 #           card_body(
                                                 #             popover(
                                                 #               bsicons::bs_icon("gear", class = "ms-auto"),
                                                 #               # Barra desplegable para grafico Distribucion
                                                 #               bslib::input_switch("default_BN","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                 #               shiny::conditionalPanel(
                                                 #                 condition = "input.default_BN == false",
                                                 #                 sliderInput('nlag.BN',label='Número de rezagos',min=0,max=12,step=0.1,value = 2)
                                                 #               ),
                                                 #               title = "Opciones",
                                                 #               placement = "bottom"
                                                 #             ),
                                                 #             layout_column_wrap(
                                                 #               width = 1/2,
                                                 #               plotlyOutput("pc.equ_Filtro Beveridge-Nelson"),
                                                 #               plotlyOutput("pc.desequ_Filtro Beveridge-Nelson")
                                                 #             )
                                                 #           )
                                                 # ),
                                                 nav_panel('Hodrick-Prescott una cola',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               # Barra desplegable para grafico Distribucion
                                                               bslib::input_switch("default_HP_uc","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_HP_uc == false",
                                                                 selectInput('lambda.HP_uc',label='Lambda',choices = c(14400,1600,100),selected = 1600)
                                                               ),
                                                               title = "Opciones",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Hodrick y Prescott a una cola"),
                                                               plotlyOutput("pc.desequ_Filtro Hodrick y Prescott a una cola")
                                                             )
                                                           )
                                                 ),
                                                 nav_panel('Butterworth',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               # Barra desplegable para grafico Distribucion
                                                               bslib::input_switch("default_BW","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_BW == false",
                                                                 sliderInput('nfix.BW',label='Orden del filtro',min=0.5,max=4,step=0.5,value = 2)
                                                               ),
                                                               title = "Opciones",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Butterworth"),
                                                               plotlyOutput("pc.desequ_Filtro Butterworth")
                                                             )
                                                           )
                                                        ),
                                                 nav_panel('Hamilton promedio',
                                                           card_body(
                                                             popover(
                                                               bsicons::bs_icon("gear", class = "ms-auto"),
                                                               # Barra desplegable para grafico Distribucion
                                                               bslib::input_switch("default_HM_promedio","Parámetros del filtro por defecto",T), # Ajustar el ancho de la barra
                                                               shiny::conditionalPanel(
                                                                 condition = "input.default_HM_promedio == false",
                                                                 sliderInput('lh.HM_promedio',label='Limite inferior de horizonte de pronostico',min=1,max=4, step=1,value = 4),
                                                                 sliderInput('uh.HM_promedio',label='Limite superior de horizonte de pronostico',min=8,max=30,step=1,value = 20),
                                                                 sliderInput('p.HM_promedio',label='Numero de rezagos',min=2,max=12,step=1,value = 4)
                                                               ), # Ajustar el ancho de la barra
                                                               title = "Opciones",
                                                               placement = "bottom"
                                                             ),
                                                             layout_column_wrap(
                                                               width = 1/2,
                                                               plotlyOutput("pc.equ_Filtro Hamilton promedio"),
                                                               plotlyOutput("pc.desequ_Filtro Hamilton promedio")
                                                             )
                                                           )
                                                      ),
                                                      )
                                                  ),
                                       
                                       
                                       
                                       
                                       )
                                       
                                       
                                     )
                                  
                          ), # Fin nav_panel Filtros estadisticos-Panel de control
                         
                          
                          #:::::::::::::::::::::::::::::::
                          ### 1.2.PPC----  
                          #:::::::::::::::::::::::::::::::
                          
                          nav_panel(title='PPC',
                                    layout_sidebar(
                                      sidebar = sidebar(
                                        # Cargar base de datos
                                        fileInput("dataITCR_PPC", "Seleccione los datos del ITCR IPP.xlsx",
                                                  accept = c(".xlsx")),
                                        
                                        accordion(open = F,
                                                  # Seleccionar datos para obtener promedio sin cambio estructural
                                                  accordion_panel("Promedio sin cambio estructural", icon = bsicons::bs_icon("sliders"),
                                                                  selectInput("anio.ini_ppc", "Seleccionar año, inicio promedio:", choices = seq(1970,year(Sys.Date())),selected = 1970 ),
                                                                  sliderTextInput("mes.ini_ppc","Seleccionar mes, inicio promedio:" , 
                                                                                  choices = c("Ene","Feb","Mar","Abr",
                                                                                              "May","Jun","Jul","Ago",
                                                                                              "Sep","Oct","Nov","Dic"), 
                                                                                  selected = "Ene", #if you want any default values 
                                                                                  animate = F, grid = T, 
                                                                                  hide_min_max = FALSE, from_fixed = FALSE,
                                                                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                                                                  post = NULL, dragRange = TRUE),
                                                                  
                                                                  selectInput("anio.fin_ppc", "Seleccionar año, fin promedio:", choices = seq(1970,year(Sys.Date())),selected = year(Sys.Date()) ),
                                                                  sliderTextInput("mes.fin_ppc","Seleccionar mes, fin promedio:" , 
                                                                                  choices = c("Ene","Feb","Mar","Abr",
                                                                                              "May","Jun","Jul","Ago",
                                                                                              "Sep","Oct","Nov","Dic"), 
                                                                                  selected = "Ago", #if you want any default values 
                                                                                  animate = F, grid = T, 
                                                                                  hide_min_max = FALSE, from_fixed = FALSE,
                                                                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                                                                  post = NULL, dragRange = TRUE)
                                                                  ),
                                                  
                                                  # Seleccionar datos para obtener promedio con cambio estructural
                                                  accordion_panel("Promedio con cambio estructural", icon = bsicons::bs_icon("sliders"),
                                                                  selectInput("anio.ini_ppc_ce", "Seleccionar año, inicio promedio cambio estructural:", choices = seq(1985,year(Sys.Date())),selected = 1985 ),
                                                                  sliderTextInput("mes.ini_ppc_ce","Seleccionar mes, inicio promedio cambio estructural:" , 
                                                                                  choices = c("Ene","Feb","Mar","Abr",
                                                                                              "May","Jun","Jul","Ago",
                                                                                              "Sep","Oct","Nov","Dic"), 
                                                                                  selected = "Ago", #if you want any default values 
                                                                                  animate = F, grid = T, 
                                                                                  hide_min_max = FALSE, from_fixed = FALSE,
                                                                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                                                                  post = NULL, dragRange = TRUE),
                                                                  
                                                                  
                                                                  selectInput("anio.fin_ppc_ce", "Seleccionar año, fin promedio cambio estructural:", choices = seq(1985,year(Sys.Date())),selected = year(Sys.Date()) ),
                                                                  sliderTextInput("mes.fin_ppc_ce","Seleccionar mes, fin promedio cambio estructural:" , 
                                                                                  choices = c("Ene","Feb","Mar","Abr",
                                                                                              "May","Jun","Jul","Ago",
                                                                                              "Sep","Oct","Nov","Dic"), 
                                                                                  selected = "Dic", #if you want any default values 
                                                                                  animate = F, grid = T, 
                                                                                  hide_min_max = FALSE, from_fixed = FALSE,
                                                                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                                                                  post = NULL, dragRange = TRUE),
                                                                  )
                                                  ),
                                        
                                        #::::::::::::::::::::::
                                        # Ejecutar estimación
                                        #:::::::::::::::::::::
                                        actionButton(inputId = "boton.est_ppc",label="Estimar ITCR-PPC"),
                                        
                                        #:::::::::::::::::::::::::
                                        # Descargar datos ppc
                                        #:::::::::::::::::::::::::
                                        bslib::input_switch("descargar_datos_ppc",
                                                            label="Descargar datos PPC estimado", value=F),
                                        shiny::conditionalPanel(
                                          condition = "input.descargar_datos_ppc == true",
                                          fixedRow(downloadButton("download_ppc", label="Descargar .xlsx")) ),
                                        
                                        
                                        width = 500,
                                        open = F,
                                        fluid = T
                                      ),
                                      position='left',
                                      fillable = T,
                                      
                                      fluidRow(
                                        card(
                                          card_body(
                                            
                                            #::::::::::::::::::::::::::
                                            # Comparación estimaciones
                                            #::::::::::::::::::::::::::
                                            
                                            layout_column_wrap(
                                              # Equilibrio filtro estadistico ronda anterior
                                              value_box(title='PPC: última estimación del desalineamiento ITCR-IPP sin cambio estructural',
                                                        value = p(round(dta_prev_ppc[['Desalineamiento']][NROW(dta_prev_ppc[['Desalineamiento']])],2),'%'),
                                                        p(paste0("Desviación estándar: ± ", round(desv_desequ_ppc_ant, 2))),
                                                        showcase = plotlyOutput('desequ.ppc_prev'),
                                                        full_screen = TRUE,
                                                        theme = "success"
                                              ),
                                              
                                              
                                              # Desequilibrio filtro estadistico ronda anterior
                                              value_box(title='PPC: última estimación del desalineamiento ITCR-IPP con cambio estructural',
                                                        value = p(round(dta_prev_ppc[['Desalineamiento ce']][NROW(dta_prev_ppc[['Desalineamiento ce']])],2),'%'),
                                                        p(paste0("Desviación estándar: ± ", round(desv_desequ_ce_ppc_ant, 2))),
                                                        showcase = plotlyOutput('desequ.ppc_prev_ce'),
                                                        full_screen = TRUE,
                                                        theme = "success"
                                              ),
                                            ),
                                            layout_column_wrap(
                                              # Equilibrio estimado con los parametros actuales
                                              value_box(title='PPC: estimación del desalineamiento ITCR-IPP sin cambio estructural',
                                                        value = p(textOutput('last_desal.ppc',inline = T),'%'),
                                                        p(textOutput('desv_desal.ppc')),
                                                        showcase = plotlyOutput('prem.desal_ppc'),
                                                        full_screen = T,
                                                        theme = "blue"),
                                              
                                              
                                              # Desequilibrio estimado con los parametros actuales
                                              value_box(title='PPC: estimación del desalineamiento ITCR-IPP con cambio estructural',
                                                        value = p(textOutput('last_desal.ppc_ce',inline = T),'%'),
                                                        p(textOutput('desv_desal.ppc_ce')),
                                                        showcase = plotlyOutput('prem.desal_ppc_ce'),
                                                        full_screen = T,
                                                        theme = "blue"),
                                              
                                            )
                                            
                                          )
                                          
                                        ),
                                        
                                        #::::::::::::::::::::::::::
                                        # Descripcion de los datos
                                        #::::::::::::::::::::::::::
                                        
                                        navset_card_underline(
                                          title='Datos utilizados para la estimación del desalineamiento PPC',
                                          
                                          # Datos utilizados para la estimación de los filtros
                                          nav_panel('Datos estimación',
                                                    #textOutput('show_data_table_estimation_ppc'),
                                                    dataTableOutput("show_data_table_estimation_ppc")
                                          ),
                                          # nav_panel('Resumen base de datos',
                                          #           card_body(
                                          #             layout_column_wrap(
                                          #               # # Resumen de los datos utilizados para la estimacion
                                          #               # value_box(title='Fechas',
                                          #               #           bsicons::bs_icon("calendar2-check"),
                                          #               #           theme=value_box_theme(bg='#F3FCFF',fg="#0B538E"),
                                          #               #           p('Fecha incial:',textOutput('anio.inicio_filtros',inline=T),textOutput('trim.inicio_filtros',inline=T)),
                                          #               #           p('Fecha final:',textOutput('anio.final_filtros_pc',inline=T),textOutput('trim.final_filtros_pc',inline=T)),
                                          #               #           p('Fecha corrección colas:',textOutput('anio.correccion_pc',inline=T), textOutput('trim.correccion_pc',inline=T) )) ,
                                          #               # 
                                          #               # value_box(title='Nombre variables',
                                          #               #           showcase=bsicons::bs_icon("feather"),
                                          #               #           theme=value_box_theme(bg='#F3FCFF',fg="#0B538E"),
                                          #               #           value=p(textOutput('nombre_variable_pc',inline=T))),
                                          #               # 
                                          #               # value_box(title='Número de observaciones',
                                          #               #           value = p(textOutput('numero_observaciones_pc',inline = T)),
                                          #               #           showcase=bsicons::bs_icon("grid-fill"),
                                          #               #           theme=value_box_theme(bg='#F3FCFF',fg="#0B538E")) 
                                          #             )
                                          #           )
                                          #           
                                          # )
                                          # 
                                        ),
                                        
                                        #:::::::::::::::::::::::::::::::::::
                                        # Grafica preliminar de PPC
                                        #:::::::::::::::::::::::::::::::::::
                                        
                                        # Grafico desequ_suav
                                        column(6, card(full_screen = T,
                                                       # card_header("",
                                                       #             tooltip(
                                                       #               bsicons::bs_icon("question-circle"),
                                                       #               "Aca estan los equilibrios suavizados del BEER",
                                                       #               placement = "right")
                                                       # ),
                                                       ##plotlyOutput("equ_suav_beer")
                                                       )
                                               ),
                                        # Grafico equ_suav
                                        column(6, card(full_screen = T,
                                                       # card_header("",
                                                       #             tooltip(
                                                       #               bsicons::bs_icon("question-circle"),
                                                       #               "Los percentiles de los desequilibrios suavizados se calcularon como 100*(ITCRE - ITCRE*)/ITCRE* donde ITCRE* es el percentil asociado a la distribucion de equilibrios obtenida de la union de modelos de la metodlogia BEER",
                                                       #               placement = "right")
                                                       # ),
                                                       ##plotlyOutput("desequ_suav_beer")
                                                       )
                                               )
                                        
                                      )
                                      
                                    )
                                    
                              ) # Fin nav_panel PPC-Panel de control
                  ),
                  
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  # 2. BEER -----------------------------------------------------
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  
                
                  nav_menu(title="BEER",
                           
                           #:::::::::::::::::::::::::::::::
                           ### 2.1.Desequilibrios----  
                           #:::::::::::::::::::::::::::::::
                           
                           nav_panel(title="Desequilibrios",
                                     layout_sidebar(
                                       sidebar = sidebar(
                                         # Barra desplegable para los Desequilibrios
                                         selectInput(inputId = "percent",
                                                     label ="Seleccione los percentiles del equilibrio:",
                                                     choices = paste0("p",c(90,75,50,25,10),"%"),
                                                     selected = "p50%",
                                                     multiple = TRUE),
                                         selectInput("anio_beer", "Seleccionar año de inicio:", choices = seq(2000, year(Sys.Date())), selected = 2000),
                                         selectInput("trimestre_beer", "Seleccionar trimestre de inicio:", choices = list("T1"=1,"T2"=2,"T3"=3,"T4"=4), selected = "T1"),
                                         
                                         # Barra desplegable para grafico Distribucion
                                         # selectInput(inputId = "fecha",
                                         #             label ="Selecciona una fecha:",
                                         #             choices = varios$fechas_list,
                                         #             selected = "2023-12-01")
                                         
                                         width = 500,
                                         open = F,
                                         fluid = T
                                                        ),
                                       position='left',
                                       fillable = T,
                                       
                                       fluidRow(
                                         # Grafico equ_suav
                                         column(6, card(full_screen = T,
                                                        card_header("",
                                                                    tooltip(
                                                                      bsicons::bs_icon("question-circle"),
                                                                      "Percentiles de la distribución de los equilibrios de la TCR estimados a partir de la metodología Thousand of BEER’s. La TCR de equilibrio corresponde normalmente a la mediana de la distribución de los equilibrios estimados (percentil 50)",
                                                                      placement = "right")
                                                        ),
                                                        plotlyOutput("equ_suav_beer"))),
                                         # Grafico desequ_suav
                                         column(6, card(full_screen = T,
                                                        card_header("",
                                                                    tooltip(
                                                                      bsicons::bs_icon("question-circle"),
                                                                      "Desalineamiento de la TCR frente a su equilibrio estimado a partir de la mediana (percentil 50) de la distribución de los equilibrios estimados la TCR a partir de la metodología Thousand of BEER’s.",
                                                                      placement = "right")
                                                        ),
                                                        plotlyOutput("desequ_suav_beer")))
                                         
                                         
                                         
                                       ),
                                       # Graficos Fanchart y Distribucion
                                       fluidRow(
                                         # Grafico Fanchart equ_suav
                                         column(6, card(full_screen = T,
                                                        card_header("",
                                                                    tooltip(
                                                                      bsicons::bs_icon("question-circle"),
                                                                      "Densidad de los equilibrios estimados de la TCR a partir de la metodología Thousand of BEER’s.",
                                                                      placement = "right")
                                                        ),
                                                        plotlyOutput("fan_equ_suav"))), # cambiar orden
                                         # Grafico Fanchart desequ_suav
                                         column(6, card(full_screen = T,
                                                        card_header("",
                                                                    tooltip(
                                                                      bsicons::bs_icon("question-circle"),
                                                                      "Densidad de los desalineamientos de la TCR frente a los equilibrios estimados a partir de la metodología Thousand of BEER’s",
                                                                      placement = "right")
                                                        ),
                                                        plotlyOutput("fan_desequ_suav"))), #, height=400, width=500
                                         
                                       ),
                                       
                                       fluidRow(
                                         # Mapa de Calor
                                         column(6, card(full_screen = T,
                                                        card_header("",
                                                                    tooltip(
                                                                      bsicons::bs_icon("question-circle"),
                                                                      "Mapa de calor con los desalineamientos de la TCR frente a su equilibrio para cada metodología.",
                                                                      placement = "right")
                                                        ),
                                                        plotlyOutput("heatmap_beer"))),
                                         # Grafico Distribucion opcion 3
                                         column(6, card(full_screen = T,
                                                        card_header("",
                                                                    # Info
                                                                    tooltip(
                                                                      bsicons::bs_icon("question-circle"),
                                                                      "Distribución empírica de los equilibrios de la TCR estimada a partir de la metodología Thousand of BEER’s para una fecha en específico. Seleccione la fecha para la cual se desea observar la distribución de los equilibrios.",
                                                                      placement = "right"),
                                                                    # Elegir trimestre
                                                                    popover(
                                                                      bsicons::bs_icon("gear", class = "ms-auto"),
                                                                      # Barra desplegable para grafico Distribucion
                                                                      selectInput(inputId = "fecha3",
                                                                                  label ="Selecciona una fecha:", # Selecciona una fecha:
                                                                                  choices = varios$fechas_list,
                                                                                  selected = varios$fechas_list[length(varios$fechas_list)],
                                                                                  width = "200px"), # Ajustar el ancho de la barra
                                                                      title = "Opciones",
                                                                      placement = "bottom"
                                                                    ),
                                                                    class = "d-flex align-items-center gap-1"
                                                        ),
                                                        
                                                        # Grafico Distribucion
                                                        plotlyOutput("dist_equ_suav3")))
                                                   )
                                       
                                                )
                                          ), # Fin nav_panel Desequilibrios-BEER
                           
                               #:::::::::::::::::::::::::::::::
                               ### 2.2.Contribuciones----  
                               #:::::::::::::::::::::::::::::::
                               
                           nav_panel(title="Contribuciones",
                                     layout_sidebar(
                                       sidebar = sidebar(
                                         accordion(
                                           accordion_panel("Contribución al crecimiento del Equilibrio",
                                                           selectInput(inputId = "tipo_modelo_contri",
                                                                       label ="Seleccione la metodologia :",
                                                                       choices = varios$modelos_disponibles,
                                                                       selected = varios$modelos_disponibles,
                                                                       multiple = T),
                                                           selectInput(inputId = "fecha_equ",
                                                                       label ="Seleccione la fecha para hacer el zoom del crecimiento:",
                                                                       choices = varios$fechas_list,
                                                                       selected = varios$fechas_list[length(varios$fechas_list)],
                                                                       multiple = FALSE),
                                                           actionButton(inputId = "boton_equ",label="Obtener contribuciones sobre el crecimiento")
                                           ),
                                           accordion_panel("Contribución de los grupos sobre el crecimiento",
                                                           selectInput(inputId = "grupo",
                                                                       label ="Seleccione el grupo de variables:",
                                                                       choices = c("Fiscal","Productividad","Externas","Otros","TI"),
                                                                       multiple = FALSE),
                                                           selectInput(inputId = "fecha_con",
                                                                       label ="Seleccione la fecha para hacer el zoom de las contribuciones:",
                                                                       choices = varios$fechas_list,
                                                                       selected = varios$fechas_list[length(varios$fechas_list)],
                                                                       multiple = FALSE),
                                                           actionButton(inputId = "boton_contri",label="Obtener contribuciones por grupo")
                                                           
                                           ),
                                           oepn = T),
                                         
                                         width = 400,
                                         open = T,
                                         fluid = T,
                                       ),
                                       
                                       position='left',
                                       fillable = F,
                                       
                                       # Grafico contribucion al equilibrio
                                       card(full_screen = T,
                                            card_header("",
                                                        tooltip(
                                                          bsicons::bs_icon("question-circle"),
                                                          "Contribuciones de los grupos fundamentales sobre el comportamiento de la variación anual del equilibrio estimado de la TCR – ITCR NT, calculadas a partir de la suma ponderada del crecimiento de los fundamentales para cada metodología.",
                                                          placement = "right")),
                                            plotlyOutput("contri_equ")),
                                       
                                       fluidRow( column(6,card(full_screen = T,
                                                               card_header("",
                                                                           tooltip(
                                                                             bsicons::bs_icon("question-circle"),
                                                                             "Contribución de los grupos de fundamentales sobre el comportamiento de la variación anual del equilibrio estimado de la TCR – ITCR NT para un periodo en específico.",
                                                                             placement = "right")),
                                                               plotlyOutput("contri_equ_zoom"))) ,
                                                 
                                                 column(6, card(full_screen = T,
                                                                card_header("",
                                                                            tooltip(
                                                                              bsicons::bs_icon("question-circle"),
                                                                              "Contribución de las principales variables fundamentales sobre el comportamiento de la variación anual del equilibrio estimado de la TCR – ITCR NT para un periodo en específico. Seleccione el número de variables para las cuales desea observar su contribución sobre la variación anual del equilibrio.",
                                                                              placement = "right"),
                                                                            popover(
                                                                              bsicons::bs_icon("gear", class = "ms-auto"),
                                                                              # Barra desplegable para grafico Distribucion
                                                                              sliderInput(inputId = "top_nvariables",
                                                                                          label ="Seleccione el número de variables:", # Selecciona una fecha:
                                                                                          min=1,max=20,value=5,step=1,
                                                                                          width = "200px"),
                                                                              title = "Número de variables",
                                                                              placement = "bottom"
                                                                            ),
                                                                            class = "d-flex align-items-center gap-1"
                                                                            ),
                                                                plotlyOutput("contri_top10")) ) 
                                       ), # ?plotlyOutput ,  height = "500px", width='auto'
                                       
                                       fluidRow( column(6, card(full_screen = T,
                                                                card_header("",
                                                                            tooltip(
                                                                              bsicons::bs_icon("question-circle"),
                                                                              "Comportamiento de la contribución del grupo de interés sobre el crecimiento anual del equilibrio, desagregado por la participación de las variables fundamentales que conforman el grupo, para la metodología de interés.",
                                                                              placement = "right")),
                                                                plotlyOutput("contri_grupo_modelo"))),
                                                 column(6,  card(full_screen = T,
                                                                 card_header("",
                                                                             tooltip(
                                                                               bsicons::bs_icon("question-circle"),
                                                                               "Comportamiento de la contribución del grupo de interés sobre el crecimiento anual del equilibrio para una fecha determinada, desagregado por la participación de las variables fundamentales que conforman el grupo, para la metodología de interés",
                                                                               placement = "right") ),
                                                                 plotlyOutput("contri_grupo_modelo_zoom")))
                                       ),
                                       
                                       #col_widths = c(4,4,4,6,6),
                                       #row_heights = c("600px","600px")
                                       
                                       
                                       
                                       
                                       
                                     )
                                     
                           ), # Fin nav_panel Contribuciones-BEER
                           
                           
                               #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                               ## 2.3.Informacion BEER ----------------------------------
                               #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           
                           
                               nav_panel(title='Información BEER',
                                         layout_sidebar(
                                           sidebar = sidebar(
                                                         # Cargar archivo .xlsx
                                                         #fileInput(inputId = "archivo_InfoBEER",
                                                         #          label = "Seleccione el archivo Matriz_variables_dic23.xlsx",
                                                         #          accept = c(".xlsx")),
                                                         selectInput(inputId = "grupa_var",
                                                                     label = "Seleccione un grupo de variables",
                                                                     choices = c("Fiscal","Productividad","Externas","Otros","TI")),
                                                         selectInput(inputId = "name_var",
                                                                     label = "Seleccione la variable",
                                                                     choices = matriz_var$variable),
                                                         width = 500,
                                                         open = F,
                                                         fluid = T
                                                         ),
                                                         position='left',
                                                         fillable = F,
                                           
                                                    layout_column_wrap(width="100px", !!!varios$cajas_infoBEER),
                                           
                                                     fluidRow(
                                                       column(6,
                                                              card(full_screen = T,
                                                                   card_header("",
                                                                               tooltip(
                                                                                 bsicons::bs_icon("question-circle"),
                                                                                 "Número de modelos que tienen en cuenta las variables fundamentales para el grupo de interés, para cada metodología y para la unión.",
                                                                                 placement = "right")
                                                                   ),
                                                                   height = "600px",
                                                                   plotlyOutput("InfoBEER_barplot"))),
                                                       column(6,
                                                              card(full_screen = T,
                                                                   card_header("",
                                                                               tooltip(
                                                                                 bsicons::bs_icon("question-circle"),
                                                                                 "Tabla resumen que permite observar el signo esperado, la mediana de los coeficientes por la metodología, el crecimiento de las variables fundamentales,  y crecimiento de la tendencia de las variables fundamentales, para hacen parte del grupo de interés.",
                                                                                 placement = "right")
                                                                   ),
                                                                   height = "600px",
                                                                   gt_output("Tabla_coeficientes")))
                                                     ),
                                                     
                                                     fluidRow(
                                                       column(6,
                                                              card(full_screen = T,
                                                                   card_header("",
                                                                               tooltip(
                                                                                 bsicons::bs_icon("question-circle"),
                                                                                 "Comportamiento de la variable de interés y de su tendencia estimada a partir del filtro de Hodrick y Prescott. Para algunas variables se muestra su logaritmo natural, mientras que otras se encuentran en su escala original.",
                                                                                 placement = "right")
                                                                   ),
                                                                   height = "500px",
                                                                   plotlyOutput("InfoBEER_plot_regresoras"))),
                                                       column(6,
                                                              card(full_screen = T,
                                                                   card_header("",
                                                                               tooltip(
                                                                                 bsicons::bs_icon("question-circle"),
                                                                                 "Tabla con el ranking de las variables que más aparecen en las diferentes metodologías que componen los equilibrios estimados.",
                                                                                 placement = "right")
                                                                   ),
                                                                   height = "500px",
                                                                   gt_output("Tabla_top_var")))
                                                     )
                                           
                                           
                                           
                                           
                                           
                                           
                                              )
                                         
                                         ) # Fin nav_panel Informacion-BEER
                                 ), # Fin nav_menu BEER
                  
                  
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           # 3. Filtros -----------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           
                           nav_panel(title='Filtros',
                                     layout_sidebar(
                                       sidebar= sidebar(
                                             # Seleccionando los inputs de la serie
                                             selectInput("series", "Filtros estadísticos:", 
                                                         choices = c('Filtro Hodrick y Prescott',
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
                                                         multiple = TRUE,
                                                         selected = c('Filtro Hodrick y Prescott',
                                                                      #'Filtro Hamilton',
                                                                      'Filtro Promedio Móvil',
                                                                      'Filtro STL',
                                                                      'Filtro Baxter-King',
                                                                      'Filtro Christiano-Fitzgerald',
                                                                      'Filtro Butterworth',
                                                                      'Filtro Regresión Trigonométrica',
                                                                      #'Filtro Beveridge-Nelson',
                                                                      'Filtro Hodrick y Prescott a una cola',
                                                                      'Filtro Hamilton promedio')
                                             ),
                                             selectInput("anio", "Seleccionar año de inicio:", choices = seq(1973,year(Sys.Date())) ),
                                             selectInput("trimestre", "Seleccionar trimestre de inicio:", choices = c("T1","T2","T3","T4")),
                                             
                                             width = 500,
                                             open = F,
                                             fluid = T
                                         
                                              ),
                                             position='left',
                                             fillable = T,
                                       
                                            fluidRow(
                                              # Grafico equ_suav
                                              column(6, card(full_screen = T,
                                                             card_header("",
                                                                         tooltip(
                                                                           bsicons::bs_icon("question-circle"),
                                                                           "Equilibrio de la TCR definida como la tendencia del ITCR-IPC estimada a partir de diferentes filtros estadísticos. La mediana se obtiene con base en los filtros seleccionados.",
                                                                           placement = "right")
                                                             ),
                                                             plotlyOutput("Equilibrios_filtros"))),
                                              # Grafico desequ_suav
                                              column(6, card(full_screen = T,
                                                             card_header("",
                                                                         tooltip(
                                                                           bsicons::bs_icon("question-circle"),
                                                                           "Desalineamiento de la TCR con respecto a la tendencia del ITCR-IPC estimada de los filtros estadísticos. La mediana de los desalineamientos se obtiene con base en los filtros seleccionados.",
                                                                           placement = "right")
                                                             ),
                                                             plotlyOutput("Desequilibrios_filtros"))),
                                              
                                              # Grafico Fanchart_Equilibrios_filtros
                                              column(6, card(full_screen = T,
                                                             card_header("",
                                                                         tooltip(
                                                                           bsicons::bs_icon("question-circle"),
                                                                           "Densidad de las tendencias estimadas del ITCR-IPC a partir de los filtros estadísticos.",
                                                                           placement = "right")
                                                             ),
                                                             plotlyOutput("Fanchart_Equilibrios_filtros"))),
                                              # Grafico Fanchart_Desequilibrios_filtros
                                              column(6, card(full_screen = T,
                                                             card_header("",
                                                                         tooltip(
                                                                           bsicons::bs_icon("question-circle"),
                                                                           "Densidad de los desalineamientos del ITCR-IPC frente a las tendencias estimadas a partir de los filtros estadísticos.",
                                                                           placement = "right")
                                                             ),
                                                             plotlyOutput("Fanchart_Desequilibrios_filtros"))),
                                              
                                              # Grafico Mapacalor_filtros
                                              column(6, card(full_screen = T,
                                                             card_header("",
                                                                         tooltip(
                                                                           bsicons::bs_icon("question-circle"),
                                                                           "Mapa de calor con los desalineamientos de la TCR frente a la tendencia estimada para cada filtro estadístico.",
                                                                           placement = "right")
                                                             ),
                                                             plotlyOutput("Mapacalor_filtros"))),
                                              
                                              # Grafico Distribucion
                                              
                                               column(6, card(full_screen = T,
                                                              card_header("",
                                                                          # Info
                                                                          tooltip(
                                                                            bsicons::bs_icon("question-circle"),
                                                                            "Distribución empírica de las tendencias estimadas de la TCR a partir de los filtros estadísticos. Seleccione la fecha para la cual se desea observar la distribución de las tendencias. Incluya mínimo 4 filtros.",
                                                                            placement = "right"),
                                                                          # Elegir trimestre
                                                                          popover(
                                                                            bsicons::bs_icon("gear", class = "ms-auto"),
                                                                            # Barra desplegable para grafico Distribucion
                                                                            selectInput(inputId = "fecha.dist_filtros",
                                                                                        label ="Selecciona una fecha:", # Selecciona una fecha:
                                                                                        choices = varios$fechas_list,
                                                                                        selected = varios$fechas_list[length(varios$fechas_list)], # ver forma de poner segun la base de datos de panel de control
                                                                                        width = "200px"), # Ajustar el ancho de la barra
                                                                            title = "Opciones",
                                                                            placement = "bottom"
                                                                          ),
                                                                          class = "d-flex align-items-center gap-1"
                                                              ),
                                                              
                                                              # Grafico Distribucion
                                                              plotlyOutput("dist_equ_suav_filtros")))
                                              
                                              #column(6,card(plotlyOutput("dist_equ_suav_filtros")))
                                              
                                              
                                              
                                              )
                                       
                                          )
                                     
                                     ), # Fin nav_panel Filtros
                           
                  
                            
                           
                  
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  # 4. PPC -----------------------------------------------------
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

                  nav_panel(title='PPC',
                            layout_sidebar(
                              sidebar = sidebar(
                                      selectInput("anio_ppc", "Seleccione año de inicio:", choices = seq(1970, year(Sys.Date())), selected = 1970),
                                      selectInput("trimestre_ppc", "Seleccionar trimestre de inicio:", choices = list("T1"=1,"T2"=2,"T3"=3,"T4"=4), selected = "T1"),
                                      width = 500,
                                      open = F,
                                      fluid = T
                                    ),
                                  position='left',
                                  fillable = T,
                              
                                    fluidRow(
                                      # Grafico equilibrio suaviazado promedio
                                      column(6, card(full_screen = T,
                                                     card_header("",
                                                                 tooltip(
                                                                   bsicons::bs_icon("question-circle"),
                                                                   textOutput("text_equ_ppc"),
                                                                   placement = "right")
                                                     ),
                                                     plotlyOutput("Equilibrios_ppc"))),
                                      # Grafico desequ_suav
                                      column(6, card(full_screen = T,
                                                     card_header("",
                                                                 tooltip(
                                                                   bsicons::bs_icon("question-circle"),
                                                                   textOutput("text_desequ_ppc"),
                                                                   placement = "right")
                                                     ),
                                                     plotlyOutput("Desequilibrios_ppc"))),
                                      # Grafico equ_suav con ce
                                      column(6, card(full_screen = T,
                                                     card_header("",
                                                                 tooltip(
                                                                   bsicons::bs_icon("question-circle"),
                                                                   textOutput("text_equ_ppc_ce"), 
                                                                   placement = "right")
                                                     ),
                                                     plotlyOutput("Equilibrios_ppc_ce"))),
                                      # Grafico desequ_suav con ce
                                      column(6, card(full_screen = T,
                                                     card_header("",
                                                                 tooltip(
                                                                   bsicons::bs_icon("question-circle"),
                                                                   textOutput("text_desequ_ppc_ce"),
                                                                   placement = "right")
                                                     ),
                                                     plotlyOutput("Desequilibrios_ppc_ce")))
                                      
                                      
                                      
                                    )
                              
                              
                                )
                            
                            
                            
                            
                            
                            ) # Fin nav panel PPC

                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
      
) # Fin UI



