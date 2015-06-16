
## server.r
# Importa paquetes
require(rCharts)
require(reshape2)
require(googleVis)
require(shiny)
require(dplyr)
require(scales)
require(rmongodb)
require(chron)

load(file="data/vis.RData")
load(file="data/emisionesNacionales.RData")
load(file="data/bol2.RData") #carga los datos para el trade to order ratio

# MAX FECHA EMISIONES IPC
max_fecha_ipc <- max(emisionesIPC$fecha)
max_fecha_mdos <- max(mdos$fecha)

# SI LA INFORMACIÓN ESTÁ DESACTUALIZADA SE CORRE EL SCRIPT PARA BAJARLA
hoy <- Sys.Date()
dias <- seq.Date(from = max_fecha_ipc, to = hoy, by = 1)
dias_transcurridos <- length(dias[!is.weekend(dias)]) - 1 # SI HAY MAS DE UN DIA HABIL TRANSCURRIDO O SI HAY UN DIA Y YA SON MAS DE LAS 16 hrs.
if(dias_transcurridos > 1 || (dias_transcurridos == 1 && strftime(Sys.time(), format="%H:%M:%S") > "17:00:00")) {
  source(file = "../graficos2.R")
  baja_informacion()
  load(file="data/vis.RData")
  # MAX FECHA EMISIONES IPC
  max_fecha_ipc <- max(emisionesIPC$fecha)
  max_fecha_mdos <- max(mdos$fecha)
  #Actualiza la información del trade to order
  source(file = "../actualizatradetoorder.R")
  actualiza_tradetoorder(max(bol2$fecha))
  load(file="data/bol2.RData") # Recarga los datos para el trade to order ratio
}


#conexion a mongo
mongo <- mongo.create(db = "db_trends")
namespace <- "db_trends.yahoo_headers"
cursor <- mongo.find(mongo, namespace, fields = list(symbol=1L,date=1L, head=1L))
ipc_heads <- mongo.cursor.to.data.frame(cursor)
mongo.destroy(mongo)

# renombre 
 dic <- data.frame(symbol = c("AC.MX", "ALFAA.MX", "ALPEKA.MX", "ALSEA.MX", "AMXL.MX", "ASURB.MX", "BIMBOA.MX", "BOLSAA.MX", 
                              "CEMEXCPO.MX", "COMERCIUBC.MX", "ELEKTRA.MX", "FEMSAUBD.MX", "GAPB.MX", "GCARSOA1.MX",  
                              "GENTERA.MX", "GFINBURO.MX", "GFNORTEO.MX", "GFREGIOO.MX", "GMEXICOB.MX", "GRUMAB.MX", "ICA.MX",       
                              "ICHB.MX", "IENOVA.MX", "KIMBERA.MX", "KOFL.MX", "LABB.MX", "LALAB.MX", "LIVEPOLC-1.MX", "MEXCHEM.MX",   
                              "OHLMEX.MX", "PE&OLES.MX", "PINFRA.MX", "SANMEXB.MX", "TLEVISACPO.MX", "WALMEXV.MX"),
                   emision = c("AC.*", "ALFA.A", "ALPEK.A", "ALSEA.*", "AMX.L", "ASUR.B", "BIMBO.A", "BOLSA.A",     
                               "CEMEX.CPO", "COMERCI.UBC", "ELEKTRA.*", "FEMSA.UBD", "GAP.B", "GCARSO.A1", 
                               "GENTERA.*", "GFINBUR.O", "GFNORTE.O", "GFREGIO.O", "GMEXICO.B", "GRUMA.B", "ICA.*", 
                               "ICH.B", "IENOVA.*", "KIMBER.A", "KOF.L", "LAB.B", "LALA.B", "LIVEPOL.C-1", "MEXCHEM.*", 
                               "OHLMEX.*", "PE&OLES.*", "PINFRA.*", "SANMEX.B", "TLEVISA.CPO", "WALMEX.V")
                   )

ipc_heads <- left_join(ipc_heads, dic) %>%
  arrange(desc(date))




shinyServer(function(input, output) {
  
  # GENERA EL SELECTOR DE FECHAS CON EL MAXIMO DEL SET DE DATOS
  output$aut_rango_fechas <- renderUI({
    dateRangeInput(inputId ="rangoFechas",
                   label = h4("Seleccione un rango de fechas:"),
                   start="2014-11-10",
                   end = max_fecha_ipc, 
                   min = "2012-12-31", 
                   max = max_fecha_ipc, 
                   language="es")
  })
  
  # Genera el seletor de fechas con el maxico del data set
  output$fechas_control_mercados <- renderUI({
    dateRangeInput(inputId = "dateRangeMercados",
                                  start = "2014-01-01",
                                  label = "",
                                  end = max_fecha_mdos,
                                  min = "2013-01-01",
                                  max = max_fecha_mdos,
                                  language="es")
                   
  })
  
  # Genera el seletor de fechas con el maximo del data set
  output$fechas_control_heatmap <- renderUI({
    dateRangeInput(inputId = "dateRange",
                   label = h4("Seleccione un rango de fechas:"),
                   start = max(emisionesIPC$fecha[emisionesIPC$fecha != max(emisionesIPC$fecha)]),
                   end = max_fecha_ipc,
                   min = "2014-08-01",
                   max = max_fecha_ipc,
                   language = "es")
    
  })
  
  
  # Genera el selector de ipc
  output$control_selector_ipc <- renderUI({
                 selectInput("selectEmision", 
                             label = h4("Seleccione una emisión del IPC:"), 
                             choices = levels(emisionesIPC$emision))
  })

  output$control_selector_ipc2 <- renderUI({
    selectInput("selectEmision2", 
                label = h4("Seleccione una emisión del IPC:"), 
                choices = levels(emisionesIPC$emision))
  })
  
  
  # Genera el selector para las casas de bolsa del ratio
  output$control_selector_bolsas <- renderUI({
    selectInput("select_bolsa", 
                label = h4("Seleccione una casa de Bolsa:"),
                choices = levels(bol2$CASA_BOLSA))
    
  })
  
  # FUNCIONES REACTIVAS .....  
  # FUNCION REACTIVA PARA FILTRAR POR RANGOS DE FECHA
  dataInput <- reactive({  
    filter(emisionesIPC,fecha >= as.Date(input$dateRange[1]), fecha <= as.Date(input$dateRange[2]))
  })
  
  dataCheckInput2 <- reactive({  
    subset(dataInput(),select=input$checkGroup2)
  })
 
  cambiaFecha <- reactive({
    filter(emisionesIPC,fecha >= as.Date(input$rangoFechas[1]), fecha <= as.Date(input$rangoFechas[2]))
  })
  
  cambiaBolsa <- reactive({
    filter(bol2,CASA_BOLSA==input$select_bolsa, fecha > '2013-12-31') %>%
      select(fecha,MERCADO,ratio) %>%
      dcast(fecha~MERCADO)
  })

  
  # 1. GRAFICA INDICES NACIONALES
  output$myChartNac <- renderGvis({    
    gvisAnnotatedTimeLine(filter(nacionales, indice %in% input$checkNacionales),
                          datevar = "fecha", numvar = "valor", idvar = "indice",
                          options = list(scaleType = "maximized", height = "400px",width="1000px")
                          )        
  }) 
  
  # 2. GRAFICA INDICES INTERNACIONALES
  output$myChart5 <- renderGvis({    
    gvisAnnotatedTimeLine(filter(internacionales, indice %in% input$checkInternacionales),
                          datevar = "fecha",numvar = "valor", idvar = "indice", 
                          options = list(scaleType = "maximized", height = "400px", width = "1000px"))        
  }) 

      
  # 3. GRÁFICA VELAS
  output$myChart7 <- renderGvis({
    gvisCandlestickChart(filter(cambiaFecha(), emision == input$selectEmision) %>%
                           select(emision, fecha, PRECIO_APERTURA:PRECIO_MINIMO, PRECIO_CIERRE), 
                         xvar = "fecha", low = "PRECIO_MINIMO",
                         open = "PRECIO_APERTURA", close = "PRECIO_CIERRE",
                         high = "PRECIO_MAXIMO", options = list(width = "1100px", height = "500px", legend = 'none')
                         )
    })
  
  
  output$myChartDerechos <- renderGvis({
    gvisAnnotationChart(filter(emisionesIPC, emision == input$selectEmision), 
                        datevar = "fecha",
                        numvar = "PRECIO_CIERRE", 
                        idvar = "emision",
                        annotationvar = "derecho",
                        options=list(width = 1100, height = 400, fill = 10, 
                                     displayExactValues = TRUE,colors="['#0000ff','#00ff00']")
                        )
  })
  
  
  # 4. GRAFICA CALENDARIO
output$myChartCalendar <- renderGvis({
  gvisCalendar(filter(emisionesIPC,emision==input$selectEmision2,fecha>as.Date('2013-12-31')), 
               datevar="fecha", 
               numvar="NUMERO_OPERACIONES",
               options=list(
                 title="Número de operaciones",
                 height=350,width=1000,
                 calendar="{yearLabel: { fontName: 'Times-Roman',
                     color: '#1A8763', bold: true},
                     cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                      focusedCellColor: {stroke:'red'}}")
              )
})


output$table <- renderDataTable({
  data <- ipc_heads[ipc_heads$emision == input$selectEmision2,2:3]
  data
})

  
  # 5. GRAFICA MOVIMIENTO: EMISIONES IPC
 output$myChart4 <- renderGvis({
    gvisMotionChart(emisionesIPC,"emision","fecha",options=list(width=1100,height=900))
  })
  

  # 6. GRAFICA MOVIMIENTO: CASAS DE BOLSA
output$myChart6 <- renderGvis({
  gvisMotionChart(bolsas,"CVE_CASA_BOLSA","fecha",options=list(width=1100,height=900))
})


# 6. GRAFICA MOVIMIENTO: MERCADOS
output$myChartMercados <- renderGvis({
  gvisMotionChart(data = filter(mdos,fecha >= input$dateRangeMercados[1], fecha <= input$dateRangeMercados[2]), 
                  idvar = "mercado", timevar = "fecha", xvar = "hechos", 
                  yvar = "mensajes", colorvar = "ratio" ,  sizevar = "ratio", 
                  options = list(width=1100,height=900)
                  )
})

# 6. GRAFICA RATIO: CASAS DE BOLSA
output$myChartRatio <- renderGvis({
  gvisAreaChart(cambiaBolsa(),options=list(width=1100,height=900))
})


  # 7. GRAFICA VALOR DE MERCADO BARRAS

output$myChartValor <- renderChart({
  n <- nPlot(value ~ emision, group = 'variable', data = filter(valorIPC,emision %in% input$checkValor), 
             type = 'multiBarChart')
  n$addParams(height = 380, width=1000)
  n$addParams(dom = 'myChartValor')
  return(n)
})

# 7. GRAFICA MEXDER
output$myChartMexDer <- renderChart({
  n <-  nPlot(as.formula(paste(input$select_mex, "~ fecha")), group = 'descripcion', 
              data = der_mes, type = 'multiBarChart')
  n$addParams(height = 500, width=1000)
  n$addParams(dom = 'myChartMexDer')
  return(n)
})



# 1. GRÁFICA: HEATMAP
output$myChart <- renderChart({
  aux_melt <- melt(subset(dataInput(),select=c("emision",input$checkGroup))) %>%
    group_by(emision) %>%
    mutate(escalado=rescale(value)) %>%
    ungroup()
  hmap <- rPlot(variable ~ emision, color = 'escalado', data = aux_melt, type = 'tile')
  hmap$addParams(height = 400, width=1000)
  hmap$guides(reduceXTicks = FALSE)
  hmap$guides("{color: {scale: {type: gradient, lower: white, upper: red}}}")
  hmap$guides(y = list(numticks = length(unique(aux_melt$value))))
  hmap$guides(x = list(numticks = 5))
  hmap$addParams(dom = 'myChart')
  return(hmap)
})


# 8. GRAFICA: MATRIZ DE CORRELACION
output$myChart2 <- renderChart({
  corrmatrix <- cor(dataCheckInput2())
  corrdata=as.data.frame(corrmatrix)
  corrdata$Variable1=names(corrdata)
  corrdatamelt=melt(corrdata,id="Variable1")
  names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
  corrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', 
                      data = corrdatamelt, type = 'tile', height = 400)
  corrmatplot$addParams(height = 400, width=1000)
  corrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
  corrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
  corrmatplot$guides(x = list(numticks = 3))
  corrmatplot$addParams(staggerLabels=TRUE)
  corrmatplot$addParams(dom='myChart2')
  return(corrmatplot)
})


# 9. GRAFICA MAPTREE: NACIONALES
#output$mapTree <- renderGvis({
#  aux <- data.frame(emision=unique(emisionesNacionales$sector), 
#                    sector = 'Global',
#                    IMPORTE = 0,
#                    VARIACION = 0)
#  gvisTreeMap(data=rbind(c("Global",NA,0,0),aux,emisionesNacionales), 
#              idvar = "emision", 
#              parentvar="sector", 
#              sizevar = "IMPORTE", 
#              colorvar = "VARIACION",
#              options=list(fontSize=7,showScale=T,showTooltips=T,height="1000px",width="1400px"))
#})





# 10. GRAFICA MAPTREE: NACIONALES2
output$mapTree2 <- renderGvis({

  aux <- emisionesIPC %>%
    filter(fecha == max_fecha_ipc) %>%
    select(emision, NUMERO_OPERACIONES, VARIACION) %>%
    mutate(Parent = 'Global', emision = as.character(emision))
  
  gvisTreeMap(data = rbind(c("Global",0,0,NA), aux),
                    idvar = "emision", 
                    parentvar = "Parent", 
                    sizevar = "NUMERO_OPERACIONES", 
                    colorvar = "VARIACION",
              options=list(fontSize=7,showScale=T,showTooltips=T,height="1000px",width="1400px"))
  
})
 
  
})

