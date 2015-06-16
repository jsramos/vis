actualiza_tradetoorder <- function(fecha_ultima){
  # DEFINE DRIVER
  drv <- JDBC("oracle.jdbc.OracleDriver",classPath="/Users/pigdata/Downloads/Xuxo/ojdbc5.jar", " ")
  # ABRE CONEXION
  con2 <- dbConnect(drv,"jdbc:oracle:thin:@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=10.100.130.36)(PORT=1521))(CONNECT_DATA=(SID=dwhbmv)))", 
                   "USR_BURSATEC", 
                   "c0nsult4")
  # Query trade to order ratio
  query_trade <- "SELECT pos.fecha_operacion,
  pos.mercado,
  pos.casa_bolsa,
  pos.altas,
  pos.modificaciones,
  pos.cancelaciones,
  NVL (hec.no_operaciones, 0) no_operaciones,
  NVL (hec.volumen, 0) volumen,
  NVL (hec.importe, 0) importe
  FROM (  SELECT fecha_operacion,
  CASE
  WHEN em.id_tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END
  AS mercado,
  cb.clave_casa_bolsa AS casa_bolsa,
  pos.id_entidad AS id_casa_bolsa,
  SUM (
  CASE
  WHEN id_tipo_movimiento IN (1, 2) THEN no_movimientos
  ELSE 0
  END)
  AS altas,
  SUM (
  CASE
  WHEN id_tipo_movimiento = 3 THEN no_movimientos
  ELSE 0
  END)
  AS modificaciones,
  SUM (
  CASE
  WHEN id_tipo_movimiento = 4 THEN no_movimientos
  ELSE 0
  END)
  AS cancelaciones
  FROM dwh_adm.dwh_resumen_movimientos_hist pos
  JOIN dwh_adm.d_casas_bolsa cb
  ON pos.id_entidad = cb.id_casa_bolsa
  JOIN dwh_adm.dwh_cat_emision em
  ON pos.id_emision = em.id_emision
  WHERE     fecha_operacion BETWEEN TO_DATE ('fecha_uno',
  'dd/mm/yyyy')
  AND TO_DATE ('fecha_dos',
  'dd/mm/yyyy')
  AND em.id_tipo_valor NOT IN ('51',
  '52',
  '54',
  '2',
  'Q',
  'R1')
  GROUP BY fecha_operacion,
  CASE
  WHEN em.id_tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END,
  cb.clave_casa_bolsa,
  pos.id_entidad) pos
  LEFT JOIN
  (  SELECT fecha,
  mercado,
  id_casa_bolsa,
  SUM (no_operaciones) no_operaciones,
  SUM (volumen) volumen,
  SUM (importe) importe
  FROM (  SELECT fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END
  mercado,
  id_casa_bolsa_compra AS id_casa_bolsa,
  COUNT (*) no_operaciones,
  SUM (volumen) volumen,
  SUM (importe) importe
  FROM dwh_adm.dwh_hechos
  WHERE     (fecha BETWEEN TO_DATE ('fecha_uno',
  'dd/mm/yyyy')
  AND TO_DATE ('fecha_dos',
  'dd/mm/yyyy'))
  AND tipo_valor NOT IN ('51',
  '52',
  '54',
  '2',
  'Q',
  'R1')
  GROUP BY fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END,
  id_casa_bolsa_compra
  UNION ALL
  SELECT fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END
  mercado,
  id_casa_bolsa_vende AS id_casa_bolsa,
  COUNT (*) no_operaciones,
  SUM (volumen) volumen,
  SUM (importe) importe
  FROM dwh_adm.dwh_hechos
  WHERE     (fecha BETWEEN TO_DATE ('fecha_uno',
  'dd/mm/yyyy')
  AND TO_DATE ('fecha_dos',
  'dd/mm/yyyy'))
  AND tipo_valor NOT IN ('51',
  '52',
  '54',
  '2',
  'Q',
  'R1')
  AND cve_casa_bolsa_compra <> cve_casa_bolsa_vende
  GROUP BY fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END,
  id_casa_bolsa_vende
  UNION ALL
  SELECT fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END
  mercado,
  id_casa_bolsa_compra AS id_casa_bolsa,
  COUNT (*) no_operaciones,
  SUM (volumen) volumen,
  SUM (importe) importe
  FROM dwh_adm.dwh_hechos_dkpl
  WHERE     (fecha BETWEEN TO_DATE ('fecha_uno',
  'dd/mm/yyyy')
  AND TO_DATE ('fecha_dos',
  'dd/mm/yyyy'))
  AND tipo_valor NOT IN ('51',
  '52',
  '54',
  '2',
  'Q',
  'R1')
  GROUP BY fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END,
  id_casa_bolsa_compra
  UNION ALL
  SELECT fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END
  mercado,
  id_casa_bolsa_vende AS id_casa_bolsa,
  COUNT (*) no_operaciones,
  SUM (volumen) volumen,
  SUM (importe) importe
  FROM dwh_adm.dwh_hechos_dkpl
  WHERE     (fecha BETWEEN TO_DATE ('fecha_uno',
  'dd/mm/yyyy')
  AND TO_DATE ('fecha_dos',
  'dd/mm/yyyy'))
  AND tipo_valor NOT IN ('51',
  '52',
  '54',
  '2',
  'Q',
  'R1')
  AND cve_casa_bolsa_compra <> cve_casa_bolsa_vende
  GROUP BY fecha,
  CASE
  WHEN tipo_valor IN ('1A', '1I') THEN 'SIC'
  ELSE 'DOM'
  END,
  id_casa_bolsa_vende)
  GROUP BY fecha, mercado, id_casa_bolsa) hec
  ON     pos.fecha_operacion = hec.fecha
  AND pos.mercado = hec.mercado
  AND pos.id_casa_bolsa = hec.id_casa_bolsa
  ORDER BY 1, 2, 3"
  
  # Se reemplaza en el query la fecha minima que es un dia despues de la fecha máxima que ya existe en el registro
  query_trade <- gsub("fecha_uno", as.character(format(fecha_ultima + 1, "%d/%m/%Y")), query_trade)
  # Se reemplaza en el query la fecha maxima con la fecha de hoy
  query_trade <- gsub("fecha_dos", as.character(format(Sys.Date(), "%d/%m/%Y")), query_trade)
  
  
  # SENDS QUERY AND STORES IT INTO A DATA FRAME
  aux <- dbGetQuery(con2,query_trade) %>% 
    mutate(fecha = as.Date(FECHA_OPERACION), 
           ratio = ifelse(NO_OPERACIONES > 0, (ALTAS + MODIFICACIONES + CANCELACIONES) / NO_OPERACIONES, NA),
           CASA_BOLSA = as.factor(CASA_BOLSA),
           MERCADO = as.factor(MERCADO)) %>%
    select(fecha, CASA_BOLSA, MERCADO, ratio)
  
  dbDisconnect(con2)
  
  load(file="data/bol2.RData") #carga los datos para el trade to order ratio

  bol2 <- rbind(bol2, aux)
  save(bol2, file="data/bol2.RData")
  
}





