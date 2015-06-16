require(shiny)
## ui.R
require(rCharts)

shinyUI(navbarPage("Gráficos:", # Menu
                   # Nivel 1
                   tabPanel("Índices",
         fluidPage(
    
  h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
     style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
  br(),
  
  fluidRow(
    column(6, offset = 3,
           p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
             style = "font-family: 'Source Sans Pro';")
          )
          ),

  
  # 1.1. Indices Nacionales
  fluidRow(
    column(3,img(src="logo.jpg.gif"),
           
           wellPanel(
             h3("Índices Nacionales"),
             hr(),
             checkboxGroupInput(inputId = "checkNacionales",
                                label = h4("Selecciones los índices a graficar:"),
                                choices = c("IPC","IPC MidCap","IPC Comp MX","IPC SmallCap",
                                            "IPC LargeCap","Fibras","IPC Sustentable","Bursa Optimo"),
                                selected = c("IPC Comp MX","IPC MidCap","IPC SmallCap","IPC LargeCap","Fibras","IPC"))           
           )
      ),
    column(9,
           br(),
           br(),
           br(),
           htmlOutput("myChartNac") # Grafica: Indices Nacionales
    )
  ),
  br(),
  br(),
  br(),
  
  # 1.2. Indices Internacionales
  fluidRow(
    column(3,       
           wellPanel(
            h3("Índices Internacionales"),
            hr(),
            checkboxGroupInput(inputId = "checkInternacionales",
                               label = h4("Selecciones los índices a graficar:"),
                               choices=c("FTSE 100","HANG SENG","IBEX","IBovespa","IPC",
                                         "Nikkei225","S&P 500","S&P/TSX Comp","Xetra DAX"),
                               selected=c("FTSE 100","HANG SENG","IBEX","IBovespa","IPC","Nikkei225","S&P 500")
                              )
    )
      ),
    
    column(9,
           htmlOutput("myChart5") # Grafica: Indices Internacionales
    )
  )
  
)),

# Nivel 2
navbarMenu("IPC",
           # Nivel 2.1
           tabPanel("Velas",
         fluidPage(

           h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
              style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
           br(),
           
           
           fluidRow(
             
             column(6, offset = 3,
                    p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                      style = "font-family: 'Source Sans Pro';")
             )
           ),
           
           
           br(),
  
  fluidRow(
    
    column(3,
           br(),
           img(src="logo.jpg.gif"),
           br(),
           br(),
           wellPanel(
             # RANGO DE FECHAS
             uiOutput("control_selector_ipc"),
#              selectInput("selectEmision", 
#                          label = h4("Seleccione una emisión del IPC:"), 
#                          choices = c("...........")),
             hr(),
             uiOutput("aut_rango_fechas")
             # Utiliza este selector en el server: dateRangeInput(inputId="rangoFechas",
             # label = h4("Seleccione un rango de fechas:"),start="2014-11-10",end="2014-12-3",min="2012-12-31",max="2014-12-03",language="es")
  
           )),
    
    column(9,
           htmlOutput("myChart7") # Grafica: Velas
    )
    
  ),
  
  
  fluidRow(column(10,
                   htmlOutput("myChartDerechos"), offset = 2
                  
                  )
  )

  )),
  
  # Nivel 2.2
  tabPanel("Calendario",
           fluidPage(
             
             h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
                style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
             br(),
             fluidRow(
               
               column(6, offset = 3,
                      p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                        style = "font-family: 'Source Sans Pro';")
               )
             ),
             br(),

  fluidRow(
    column(3,
           br(),
           img(src="logo.jpg.gif"),
           br(),
           br(),
           wellPanel(
             # RANGO DE FECHAS
             uiOutput("control_selector_ipc2")
           )),
    
    column(9,
           showOutput("myChartCalendar","polycharts")  # Grafica: Calendario
    )
    
    
    
    
    ),
  
  fluidRow(
    dataTableOutput(outputId="table") # Tabla con los head news
  ) 
  
)),


# Nivel 2.3
tabPanel("Treemap",
         fluidPage(
           
           h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
              style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
           br(),
           fluidRow(
             
             column(6, offset = 3,
                    p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                      style = "font-family: 'Source Sans Pro';")
             )
           ),
           br(),
           
           fluidRow(
             
             
             column(12,
                    h3("Tamaño: Número de Operaciones, Color: Variación porcentual contra día anterior"),
                    htmlOutput("mapTree2") # Grafica: IPC Maptree
             )
           )   
           
           
           
         )),




# Nivel 2.4
tabPanel("Movimiento",
         fluidPage(
           
           h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
              style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
           br(),
           
           
           fluidRow(
             column(6, offset = 3,
                    p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                      style = "font-family: 'Source Sans Pro';")
             )
           ),
       br(),

  fluidRow(
    
    column(12,
           h3("Emisiones IPC"),
           htmlOutput("myChart4") # Grafica: Movimiento       
    ))
         ))),

# Nivel 3
tabPanel("Valor de Mercado",
         fluidPage(
           
           h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
              style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
           br(),
            fluidRow(
             column(6, offset = 3,
                    p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                      style = "font-family: 'Source Sans Pro';")
             )
           ),
           br(), 
  fluidRow(
    column(3,
           img(src="logo.jpg.gif"),
           wellPanel(
             h3("Valor de Mercado"),
             hr(),
             selectInput(inputId = "checkValor",
                         label = h4("Selecciones las emisiones a graficar:"),
                         multiple = T,
                         choices = c("AMX.L","WALMEX.V","GMEXICO.B","GFINBUR.O","FEMSA.UBD","TLEVISA.CPO",
                                     "GFNORTE.O","CEMEX.CPO","ALFA.A","BIMBO.A","AC.*","PE&OLES.*","MEXCHEM.*",
                                     "SANMEX.B","IENOVA.*","ELEKTRA.*","OHLMEX.*","KOF.L","PINFRA.*","GRUMA.B",
                                     "KIMBER.A","ALPEK.A","GSANBOR.B-1","GENTERA.*","ASUR.B","GAP.B","CHDRAUI.B",
                                     "COMPARC.*","LAB.B","ALSEA.*","ICH.B","LIVEPOL.C-1","GFREGIO.O","COMERCI.UBC",
                                     "BOLSA.A","ICA.*"),selected=c("AMX.L","WALMEX.V","GMEXICO.B","GFINBUR.O","FEMSA.UBD"))
                          
           )
    ),
    
    column(9,
           showOutput("myChartValor","nvd3") # Grafica: Valor de Mercado
    )
  )
  
         )),



# Nivel 5
navbarMenu("CasasBolsa",
           
           tabPanel("Movimiento",
                    fluidPage(
                      
                      h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
                         style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
                      br(),
                      
                      
                      fluidRow(
                        column(6, offset = 3,
                               p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                                 style = "font-family: 'Source Sans Pro';")
                        )
                      ),
                      br(),
                      
                      fluidRow(
                        column(12,
                               h3("Casas de bolsa"),
                               htmlOutput("myChart6") # Grafica: Casas de Bolsa Movimiento
                        )
                      )
                      
                      
                      
                    )),
           
           
           tabPanel("Ratio",
                    fluidPage(
                      
                      h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
                         style = "font-family: 'Source Sans Pro';
                         color: #fff; text-align: center;
                         background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
                         padding: 20px"),
                      br(),
                      
                      
                      fluidRow(
                        
                        column(6, offset = 3,
                               p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                                 style = "font-family: 'Source Sans Pro';")
                        )
                      ),                   
                      br(),            
                      fluidRow(
                        column(3,
                               br(),
                               img(src="logo.jpg.gif"),
                               br(),
                               br(),
                               wellPanel(
                                 # RANGO DE FECHAS cambiar aqui
                                 uiOutput("control_selector_bolsas")
                               )),
                        
                        column(9,
                               htmlOutput("myChartRatio") # Grafica Ratio mensajes/hechos
                        )
                        
                      )
                      
                      ))),

tabPanel("MexDer",
         fluidPage(
           
           h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
              style = "font-family: 'Source Sans Pro';
              color: #fff; text-align: center;
              background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
              padding: 20px"),
           br(),
         fluidRow(
             column(6, offset = 3,
                    p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                      style = "font-family: 'Source Sans Pro';")
             )
           ),  
           br(),
           fluidRow(
             
             column(3,
                    br(),
                    img(src="logo.jpg.gif"),
                    br(),
                    br(),
                    wellPanel(
                      h3("MexDer"),
                      selectInput("select_mex", 
                                  label = h4("Seleccione la variable a graficar"),
                                  choices = c("INTERES_ABIERTO","AC_IMPORTE_NOCIONAL","AC_IMPORTE_PRIMAS",
                                              "AC_VOLUMEN_OPERADO","AC_NUMERO_OPERACIONES","PD_IMPORTE_PRIMAS",
                                              "PD_VOLUMEN_OPERADO","PD_NUMERO_OPERACIONES"), 
                                  selected="AC_NUMERO_OPERACIONES"
                                 )
                    )
                    
                    ),
             column(9,
                   showOutput("myChartMexDer","nvd3") # Grafica: Mexder barras
             )
           )
           )),

tabPanel("Mercados",
         fluidPage(
           
           h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
              style = "font-family: 'Source Sans Pro';
              color: #fff; text-align: center;
              background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
              padding: 20px"),
           br(),
           
           
           fluidRow(
             column(6, offset = 3,
                    p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                      style = "font-family: 'Source Sans Pro';")
             )
           ),
           br(),
      fluidRow(
             
             column(12,
                    
                    h3("Resumen mensajes: Mercados doméstico y SIC"),
                    uiOutput("fechas_control_mercados"), # 
                    #dateRangeInput(inputId = "dateRangeMercados",
                    #               start = "2014-01-01",
                    #               label = "",
                    #               end = "2014-12-03",
                    #               min = "2013-01-01",
                    #               max = "2014-12-03",
                    #               language="es"),
                    htmlOutput("myChartMercados")
                  
             ))
          
           )),

tabPanel("Otros",
         fluidPage(

           h1("BMV", span("Bolsa Mexicana de Valores", style = "font-weight: 300"),
              style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image:url('https://raw.githubusercontent.com/rstudio/shiny-examples/master/081-widgets-gallery/www/texturebg.png');
     padding: 20px"),
           br(),
           
           
           fluidRow(
             column(6, offset = 3,
                    p("Todos los datos y modelos son meramente demostrativos y por lo tanto no necesariamente reflejen la realidad",
                      style = "font-family: 'Source Sans Pro';")
             )
           ),
           
           
           br(),
           
  fluidRow(
    
    column(3,
           img(src="logo.jpg.gif"),
           wellPanel(
             h3("Heatmap: Desempeño emisiones"),
             hr(),
             # RANGO DE FECHAS
             uiOutput("fechas_control_heatmap"),
             #dateRangeInput(inputId = "dateRange",
             #               label = h4("Seleccione un rango de fechas:"),
             #               start = "2014-12-02",
             #               end = "2014-12-03",
             #               min = "2014-08-01",
             #               max = "2014-12-03",
             #               language = "es"),
             hr(),
                 #CHECKBOX PARA HEATMAP
                 checkboxGroupInput(inputId = "checkGroup",
                                    label = h3("Variables Heatmap:"),
                                    choices = c("PRECIO_APERTURA","PRECIO_CIERRE","PRECIO_MAXIMO","PRECIO_MINIMO"),
                                    selected=c("PRECIO_APERTURA","PRECIO_CIERRE","PRECIO_MAXIMO","PRECIO_MINIMO"))
           )),
    

    column(9,
           div(showOutput("myChart","dimple")) # Grafica Heatmap
          )
  ),

  fluidRow(
    
    column(3,
           br(),
           br(),
           wellPanel(
             h3("Matriz de correlación: Emisiones"),
             hr(),
             checkboxGroupInput(inputId="checkGroup2",
                                label=h3("Variables Matriz correlacion:"),
                                choices=c("VOLUMEN","PRECIO_CIERRE","NUMERO_OPERACIONES","IMPORTE"),
                                selected=c("VOLUMEN","PRECIO_CIERRE","NUMERO_OPERACIONES","IMPORTE"))
          
           )),
    
    column(9,
           showOutput("myChart2","polycharts") # Grafica matriz correlacion
    )
  ) 
))))  