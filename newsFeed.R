library(XML)
library(rmongodb)
library(RCurl)
library(dplyr)

# All the IPC symbols
ipc_symbols <- c("AC.MX", "ALFAA.MX", "ALPEKA.MX", "ALSEA.MX", "AMXL.MX", "ASURB.MX", "BIMBOA.MX",
                 "BOLSAA.MX", "CEMEXCPO.MX", "COMERCIUBC.MX", "ELEKTRA.MX", "FEMSAUBD.MX", "GAPB.MX",
                 "GCARSOA1.MX", "GENTERA.MX", "GFINBURO.MX", "GFNORTEO.MX", "GFREGIOO.MX", "GMEXICOB.MX",
                 "GRUMAB.MX", "ICA.MX", "ICHB.MX", "IENOVA.MX", "KIMBERA.MX", "KOFL.MX", "LABB.MX",
                 "LALAB.MX" ,"LIVEPOLC-1.MX", "MEXCHEM.MX", "OHLMEX.MX", "PE&OLES.MX", "PINFRA.MX",
                 "SANMEXB.MX", "TLEVISACPO.MX", "WALMEXV.MX")

region <- "US"
languaje <- "en-US"
# symbol <- "KOFL.MX"

# Opens the connection
mongo <- mongo.create(db="db_trends")
namespace <- "db_trends.yahoo_headers"
# Creates an Index to avoid repeted documents
mongo.index.create(mongo, namespace, c("symbol", "date"), mongo.index.unique)

get_headlines <- function(symbol) {
  #
  url <- paste("http://feeds.finance.yahoo.com/rss/2.0/headline?s=", symbol, "&region=", 
               region, "&lang=", languaje, sep="")
  page_raw <- getURL(url)
  page_xml <- xmlParse(page_raw)
  # Extracts the headers
  head <- xpathSApply(page_xml,"//item/title",xmlValue)
  # Only if the the header is not empty
  if(head[1] != "Yahoo! Finance: RSS feed not found"){
    # Extracts the new's summary
    summary <- xpathSApply(page_xml,"//item/description",xmlValue)
    # Extracts and converts the date    
    date <- as.POSIXct(xpathSApply(page_xml,"//item/pubDate",xmlValue), tz = "GMT", format="%a, %d %b %Y %X")
    # Creates the data frame
    aux <- data.frame(symbol=symbol, date=date, head=head, summary=summary)
    aux_list <- list()
    aux_list <- apply(aux, 1, function(x) c(aux_list, x))
    res <- list()
    res <- lapply(aux_list, function(x) mongo.bson.from.list(x))
    mongo.insert.batch(mongo, namespace, res)
    aux
  }
}
    
ipc_heads <- plyr::ldply(ipc_symbols, get_headlines)

mongo.destroy(mongo)


