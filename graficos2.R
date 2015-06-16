baja_informacion <- function(){
  
  library(quantmod)
  library(googleVis)
  library(dplyr)
  library(reshape2)
  library(RJDBC)
  
  ######### 1. BAJA INDICES INTERNACIONALES ############
  # BAJA LOS DATOS DE YAHOO
  cadenaInt <- getSymbols(c("^MXX","^BVSP","^N225","^GDAXI","^IBEX","^FTSE","^GSPTSE","^GSPC","^HSI"),from = "2013-12-31") 
  
  # JUNTA LOS DATOS EN UN SOLO DATA FRAME
  internacionales <- plyr::ldply(cadenaInt,function(x){
    a <- data.frame(indice_cve = x,fecha = index(get(x)),coredata(get(x)))
    names(a) <- c("indice_cve","fecha","open","high","low","close","volume","adjusted")
    a <- a %>%
      arrange(fecha) %>%
      mutate(valor=adjusted * 100 / first(adjusted))
    a
  })
  
  # PEGA EL NOMBRE DEL ÍNDICE
  nombresInternacionales <- data.frame(indice_cve = c("MXX","BVSP","N225","GDAXI","IBEX","FTSE","GSPTSE","GSPC","HSI"),
                                       indice = c("IPC","IBovespa","Nikkei225","Xetra DAX","IBEX","FTSE 100","S&P/TSX Comp","S&P 500","HANG SENG"))
  
  internacionales <- merge(internacionales,nombresInternacionales,all.x = T)
  
  # BORRA LOS OBJETOS NO UTILIZADOS
  rm(MXX,BVSP,N225,GDAXI,IBEX,FTSE,GSPTSE,GSPC,HSI)
  
  
  ######### 2. BAJA EMISIONES DEL IPC ############
  
  # DEFINE DRIVER
  drv <- JDBC("oracle.jdbc.OracleDriver",classPath="/Users/pigdata/Downloads/Xuxo/ojdbc5.jar", " ")
  # ABRE CONEXION
  con <- dbConnect(drv,"jdbc:oracle:thin:@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=10.100.130.36)(PORT=1521))(CONNECT_DATA=(SID=dwhbmv)))", 
                   "USR_BURSATEC", 
                   "c0nsult4")
  # QUERY PARA OBTENER LAS VARIBLES DE DESEMPENO DE LAS EMISIONES DEL IPC A LA FECHA SELECCIONADA
  queryCompleto <- "SELECT B.EMISORA,B.SERIE,B.FECHA_OPERACION,B.NUMERO_OPERACIONES,B.VOLUMEN,B.IMPORTE, B.VARIACION, B.PRECIO_APERTURA,B.PRECIO_MAXIMO,
  B.PRECIO_MINIMO,B.PRECIO_PROMEDIO_PONDERADO,B.PRECIO_CIERRE,B.PORCENTAJE,B.TIPO_VALOR
  FROM DWH_ADM.DWH_COMPOSICION_INDICES_HIST A INNER JOIN DWH_ADM.DWH_RES_DIARIO_PRECIOS_EMISION B
  ON A.ID_EMISION = B.ID_EMISION
  WHERE A.FECHA  = DATE'2014-11-11' AND A.ID_INDICE = 8 AND A.INFLUENCIA_PORCENTAJE <> 0 AND B.FECHA_OPERACION  > DATE'2013-12-30'"
  ## OJO CAMBIAR LA FECHA EN LA QUE SE TOMA LA MUESTRA
  
  # SENDS QUERY AND STORES IT INTO A DATA FRAME
  emisionesIPC <- dbGetQuery(con,queryCompleto) %>%
    mutate(fecha=as.Date(FECHA_OPERACION),TIPO_VALOR=as.factor(TIPO_VALOR),emision=as.factor(paste(EMISORA,SERIE,sep="."))) %>%
    select(-EMISORA,-SERIE,-FECHA_OPERACION) %>%
    arrange(emision,fecha) %>%
    group_by(emision) %>%
    mutate(cambio=PRECIO_CIERRE * 100 / first(PRECIO_CIERRE)) %>%
    ungroup()
  
  query_derechos <- "SELECT D.CVE_EMISION, D.SERIE, B.FECHA_EXDERECHO, C.DESCRIPCION
    FROM DWH_ADM.DWH_COMPOSICION_INDICES_HIST A INNER JOIN DATAMART_ADM.DM_DERECHOS_EMISORAS B ON A.ID_EMISION = B.ID_EMISION 
    INNER JOIN DATAMART_ADM.DM_CAT_TIPOS_DERECHO C ON B.ID_TIPO_DERECHO = C.ID_TIPO_DERECHO
    INNER JOIN DWH_ADM.DWH_CAT_EMISION D ON A.ID_EMISION = D.ID_EMISION
    WHERE A.FECHA  = DATE'2014-11-11' AND A.ID_INDICE = 8 AND A.INFLUENCIA_PORCENTAJE <> 0 AND B.FECHA_EXDERECHO > DATE'2013-12-30'"
  
  derechos <- dbGetQuery(con,query_derechos) %>%
    mutate(emision = as.factor(paste(CVE_EMISION, SERIE,sep = ".")),
           fecha = as.Date(FECHA_EXDERECHO),
           derecho = as.factor(DESCRIPCION)) %>%
    select(emision, fecha, derecho)
  
  emisionesIPC <- left_join(emisionesIPC, derechos)
  
  ######### 3. BAJA OPERATIVIDAD DE CASAS DE BOLSA ############
  queryOperCasaBolsa <-" select * from DWH_ADM.DWH_OPERATIV_DIARIA_CASA_BOLSA WHERE FECHA_OPERACION > DATE'2013-12-30' "
  bolsas <- dbGetQuery(con,queryOperCasaBolsa) %>%
    mutate(fecha=as.Date(FECHA_OPERACION,CVE_CASA_BOLSA=as.factor(CVE_CASA_BOLSA))) %>%
    select(CVE_CASA_BOLSA,NUMERO_OPERACIONES:NUMERO_POSTURAS_CANCELADAS,NUMERO_POSTURAS_MODIFICADAS,NUMERO_MENSAJES,fecha)
  
  #dim(bolsas)
  # [1] 11898    12
  
  
  ######### 4. BAJA INDICES NACIONALES ############
  
  indices<-dbGetQuery(con,
                      "select * from DWH_ADM.DWH_COMPORTAM_INDICES_HIST 
                      WHERE CVE_INDICE IN ('ME','CG','CM','GI','60','CP','VD','FB') AND FECHA_OPERACION > DATE'2013-12-30' ")
  
  nacionales <- indices %>%
    mutate(fecha=as.Date(FECHA_OPERACION),CVE_INDICE=as.factor(CVE_INDICE)) %>%
    select(CVE_INDICE,fecha,INDICE_APERTURA,INDICE_MINIMO,INDICE_MAXIMO,INDICE_CIERRE,INDICE_ANTERIOR) %>%
    arrange(CVE_INDICE,fecha) %>%
    group_by(CVE_INDICE) %>%
    mutate(valor=INDICE_CIERRE * 100 / first(INDICE_CIERRE)) %>%
    ungroup()
  
  # PEGA EL NOMBRE DEL ÍNDICE
  nombresNacionales <- data.frame(CVE_INDICE=c("60","CG","CM","CP","FB","GI","ME","VD"), 
                                  indice = c("IPC Comp MX","IPC MidCap","IPC SmallCap","IPC LargeCap","Fibras","IPC Sustentable","IPC","Bursa Optimo"))
  
  nacionales <- merge(nacionales,nombresNacionales,all.x = T)
  
  
  ######### 5. BAJA VALOR DE MERCADO ############
  queryValor <- "SELECT B.FECHA_CALCULO,B.CVE_EMISORA,B.CVE_SERIE,VALOR_CAPITAL_EMISORA_MXP,VALOR_CAPITAL_EMISORA_USD
  FROM DWH_ADM.DWH_COMPOSICION_INDICES_HIST A INNER JOIN BMV_OPERACIONES_ADM.VALOR_CAPITAL_PRE B
  ON A.ID_EMISION = B.ID_EMISION
  WHERE A.FECHA  = DATE'2014-08-18' AND A.ID_INDICE = 8 AND A.INFLUENCIA_PORCENTAJE <> 0 AND B.FECHA_CALCULO  > DATE'2013-12-30'"
  ## OJO CAMBIAR LA FECHA EN LA QUE SE TOMA LA MUESTRA
  
  # SENDS QUERY AND STORES IT INTO A DATA FRAME
  valorIPCcompleto <- dbGetQuery(con,queryValor)
  
  valorIPC <- valorIPCcompleto %>%
    mutate(fecha=as.Date(FECHA_CALCULO),emision=as.factor(paste(CVE_EMISORA,CVE_SERIE,sep="."))) %>%
    select(emision,fecha,VALOR_CAPITAL_EMISORA_MXP,VALOR_CAPITAL_EMISORA_USD) %>%
    arrange(emision,fecha) %>%
    group_by(emision) %>%
    summarise(actual = last(VALOR_CAPITAL_EMISORA_MXP), 
              ultimo_anio_ant=first(VALOR_CAPITAL_EMISORA_MXP), 
              max = max(VALOR_CAPITAL_EMISORA_MXP,na.rm = TRUE), 
              min = min(VALOR_CAPITAL_EMISORA_MXP,na.rm = TRUE)) %>%
    arrange(-actual)
  
  valorIPC <- melt(valorIPC, id = "emision")
  
  ########### 6. DERIVADOS: RESUMEN MENSUAL POR TIPO DE VALOR
  query_der_mes <- "select a.ID_TIPO_VALOR, a.ID_ANIO,ID_MES, a.INTERES_ABIERTO, a.AC_IMPORTE_NOCIONAL, a.AC_IMPORTE_PRIMAS, a.AC_VOLUMEN_OPERADO, a.AC_NUMERO_OPERACIONES, a.PD_IMPORTE_PRIMAS, a.PD_VOLUMEN_OPERADO, a.PD_NUMERO_OPERACIONES, b.DESCRIPCION 
    from MEXDER_HISTORICO_ADM.MD_VM_RO_MENSUAL_TPVALOR a inner join MEXDER_HISTORICO_ADM.MD_CAT_TIPO_VALOR b on a.ID_TIPO_VALOR= b.ID_TIPO_VALOR WHERE a.ID_ANIO >= 2013"
  
  der  <- dbGetQuery(con,query_der_mes)
  
  der_mes <- der %>%
    mutate(mes=as.factor(ID_MES), producto = as.factor(ID_TIPO_VALOR), descripcion = as.factor(DESCRIPCION), fecha=paste(ID_ANIO,ID_MES,sep = "-")) %>%
    select(-ID_TIPO_VALOR,-ID_ANIO,-ID_MES,-DESCRIPCION)
  
  ########### 7. MERCADOS: RESUMEN DIARIO HECHOS, MENSAJES, RATIO
  query_mdos <- "select FECHA, ID_MERCADO, NO_OPERACIONES, NUMERO_MENSAJES, VOLUMEN, IMPORTE 
  FROM DWH_ADM.DWH_RESUMEN_DIARIO_MERCADOS 
  WHERE FECHA > DATE'2013-12-30' 
  UNION 
  select FECHA, ID_MERCADO, NO_OPERACIONES, NUMERO_MENSAJES, VOLUMEN, IMPORTE 
  FROM DWH_ADM.DWH_RESUMEN_DIARIO_MDOS_DKPL 
  WHERE FECHA > DATE'2013-12-31' "
  
  mdos  <- dbGetQuery(con,query_mdos)
  
  mdos <- mdos %>%
    mutate(fecha=as.Date(FECHA), mercado=as.factor(ifelse(ID_MERCADO==1,"Domestico",ifelse(ID_MERCADO==4,"SIC","Total"))), 
           hechos=NO_OPERACIONES, mensajes=NUMERO_MENSAJES-NO_OPERACIONES) %>%
    select(fecha, mercado, hechos, mensajes, VOLUMEN, IMPORTE) %>%
    group_by(fecha,mercado) %>%
    summarise(hechos = sum(hechos, na.rm = T), mensajes = sum(mensajes, na.rm = T), volumen = sum(VOLUMEN, na.rm = T), 
              importe = sum(IMPORTE, na.rm = T)) %>%
    ungroup() %>%
    mutate(ratio = mensajes / hechos) %>%
    arrange(fecha, mercado)
  
  
  dbDisconnect(con)
  
  #GUARDA TODOS LOS DATA FRAMES
  save(nacionales,internacionales,emisionesIPC,bolsas,valorIPC,der_mes,mdos,file="data/vis.RData")
  
  # BAJA DERIVADOS
  # queryDerivados <- "select ID_TIPO_VALOR, ID_FECHA, INTERES_ABIERTO, AC_IMPORTE_NOCIONAL, AC_IMPORTE_PRIMAS, AC_VOLUMEN_OPERADO, AC_NUMERO_OPERACIONES from MEXDER_HISTORICO_ADM.MD_VM_RO_DIARIO_TPVALOR WHERE ID_FECHA > DATE'2013-12-31' "
  # ## OJO CAMBIAR LA FECHA EN LA QUE SE TOMA LA MUESTRA
  # 
  # # SENDS QUERY AND STORES IT INTO A DATA FRAME
  # derivados <- dbGetQuery(con,queryDerivados)
  # 
  # der <- derivados %>%
  #   mutate(fecha = as.Date(ID_FECHA,"%Y-%m-%d"), valor = as.factor(ID_TIPO_VALOR) ) %>%
  #   select(-ID_FECHA,-ID_TIPO_VALOR) %>%
  #   arrange(valor,fecha)

}