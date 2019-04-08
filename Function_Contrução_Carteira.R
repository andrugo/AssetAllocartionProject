#########################################################################################
## Function for Download market value 
## By Andrea Ugolini
##
## Ver 1.0
## Date 20/03/2019
#########################################################################################
## Download market value from different financial market index
## Market = IBOVESPA, DOWJONES...
## Year = How many years ago you want your data?
## variable = "Open","High","Low","Close","Volume","Adjusted"
##
##
## Warning
## - Ibovespa more of 6 year problem
## - Ibovespa some stock there aren't like "KLBN11.SA","656690","TBLE3.SA"
## - Ibovespa VALE5.SA substitute with VALE3.SA
##########################################################################################


.market.portfolio <- function(market = "DOWJONES", Year = 10, variable = "Adjusted"){


##########################
# Construir um portfolio #
##########################
require("RCurl")
require("XML")
require("rlist")
require("quantmod")

#library(quantmod)
#library(XML)
#library(RCurl)
#library(rlist)

  
type.market <- switch (market,
                       IBOVESPA = "https://finance.yahoo.com/quote/%5EBVSP/components/",
                       DOWJONES = "https://finance.yahoo.com/quote/%5EDJI/components?p=%5EDJI")


if (market == "IBOVESPA"){
  
###########################################################
## WebScraping IBOVESPA componentes desde yahoo finance  ##
  
 # Essa parte pode ser modificada melhor alguem tem aluma idea 
 # Eu vi este pacote que achei interesante acho que semplifica muita a vida de todos nois 
 # library(BatchGetSymbols)
  
  ###########################################################
url <- getURL(type.market,.opts = list(ssl.verifypeer = FALSE) )
tabla <- readHTMLTable(url)
tabla <- list.clean(tabla, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tabla, function(t) dim(t)[1]))
tabla <- tabla[[which.max(n.rows)]]
componentes <- tabla[,1]


########################################################################
##  Baixar os rendimento de todos os componente do portfolio IBOVESPA ##
########################################################################

w0 <- NULL
end<- format(Sys.Date(),"%Y-%m-%d")
start<-format(Sys.Date() - (Year*365), "%Y-%m-%d")
componentes.name  <-  as.character(componentes)
componentes.name <-  componentes.name[!(componentes.name %in% c("KLBN11.SA","656690","TBLE3.SA"))]
componentes.name <- gsub("VALE5.SA","VALE3.SA",componentes.name)

l <- length(componentes.name)
w0 <- NULL
for (i in 1:l){
  dat0 = getSymbols(componentes.port[i], src="yahoo", from=start, to=end, auto.assign = F,
                    warnings = FALSE, symbol.lookup = F)
  dat0 <- na.omit(dat0)
  dat0 <- na.omit(dat0)
  colnames(dat0) <- c("Open","High","Low","Close","Volume","Adjusted")
  dat0 <- dat0[,variable]
  #w1 <- periodReturn(dat0,period = "daily",type="log")
  w0 <- cbind(w0,dat0)
}

time <- as.Date(substr(index(w0),1,10))
w0 <- as.matrix(w0)
colnames(w0) <- componentes.port

write.csv(w0, file = paste0("Ibovespa_",Sys.Date(),".csv"))

} else if (market == "DOWJONES"){

###########################################################
## WebScraping DOWNJONES componentes desde yahoo finance  ##
###########################################################
url <- getURL(type.market,.opts = list(ssl.verifypeer = FALSE) )
tabla <- readHTMLTable(url)
tabla <- list.clean(tabla, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tabla, function(t) dim(t)[1]))
tabla <- tabla[[which.max(n.rows)]]
componentes <- tabla[,1]


########################################################################
##  Baixar os rendimento de todos os componente do portfolio DOWNJONES ##
########################################################################

w0 <- NULL
end<- format(Sys.Date(),"%Y-%m-%d")
start<-format(Sys.Date() - (Year*365), "%Y-%m-%d")
componentes.name  <-  as.character(componentes)

l <- length(componentes.name)
w0 <- NULL
for (i in 1:l){
  dat0 = getSymbols(componentes.name[i], src="yahoo", from=start, to=end, auto.assign = F,
                    warnings = FALSE, symbol.lookup = F)
  dat0 <- na.omit(dat0)
  colnames(dat0) <- c("Open","High","Low","Close","Volume","Adjusted")
  dat0 <- dat0[,variable]
  #w1 <- periodReturn(dat0,period = "daily",type="log")
  w0 <- cbind(w0,dat0)
}

time <- as.Date(substr(index(w0),1,10))
w0 <- as.matrix(w0)
colnames(w0) <- componentes.name

write.csv(w0, file = paste0("DownJones_",Sys.Date(),".csv"))

##############################################
##  Baixar os rendimento do index DOWNJONES ##
##############################################
dat_I = getSymbols("^DJI", src="yahoo", from=start, to=end, auto.assign = F,
                  warnings = FALSE, symbol.lookup = F)
dat_I <- na.omit(dat_I)
colnames(dat_I) <- c("Open","High","Low","Close","Volume","Adjusted")
dat_I <- dat_I[,"Adjusted"]
time <- as.Date(substr(index(dat_I),1,10))
dat_I <- as.matrix(dat_I)

write.csv(dat_I, file = paste0("DownJonesIndex_",Sys.Date(),".csv"))

}
}

setwd("/Users/andreaugolini/Library/Mobile Documents/com~apple~CloudDocs/Documents/Work UERJ/Aulas Pos/Asset_Allocation/R")
.market.portfolio(market = "DOWJONES", Year = 10, variable = "Adjusted")
