library(XML)
library(plyr)
library(quantmod)
library(rvest)
# get the list of symbols


InsiderScreening <- function() {
  
  require(rvest)
  require(XML)
# scrape Stock Screener result:
# - EPS growth next 5 years > 10%
# - Return on Equity        > 10%
# - Sales growth past 5 years > 10%
# - Insider Transactions    > 0%
# - EPS growth this year    > 10%
# - EPS growth QoQ          > 0%
# - EPS growth next year    > 10%
  
  url <- "http://finviz.com/screener.ashx?v=111&f=fa_epsqoq_pos,fa_epsyoy_o10,fa_epsyoy1_o10,fa_estltgrowth_o10,fa_roe_o10,fa_sales5years_o10,sh_insidertrans_pos&ft=2"

# extract list from html table data  
  Insider_traded_stocks <- url %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
  return(Insider_traded_stocks)
}

PositionSize <- function(ATRvalue, portfolioSize = 1500, riskFactor = 0.015 ) {
  
  posSize <- round( ( portfolioSize *  riskFactor) / ( 2 * ATRvalue ) )
  
  return(posSize)
  
}


MarketFilter <- function() {
  
  startDate <- Sys.Date() - 300
  
  SP500 <- getSymbols(Symbols = "^GSPC",
                      from = startDate,
                      auto.assign = FALSE)
  
  oSMA_s <- SMA(SP500[,4],n=50)
  dsma_s <- data.frame(oSMA_s)
  colnames(dsma_s) = c("SMA_s")
  
  oSMA_l <- SMA(SP500[,4],n=150)
  dsma_l <- data.frame(oSMA_l)
  colnames(dsma_l) = c("SMA_l")
  
  data = data.frame(dsma_s,dsma_l)
  last_close = tail(data,1)
  
  if ( last_close$SMA_s < last_close$SMA_l ) {vStatus = FALSE }
  else { vStatus = TRUE }
  
  return(vStatus)
  
}

CheckPrices <- function(TickerSymbol) {
  
  startDate <- "2016-01-01"  
  
AKPrices <- getSymbols(Symbols = TickerSymbol,
                from = startDate,
                auto.assign = FALSE) 

SMA10 <- SMA(AKPrices[,4],n=10)
#SMA10 <- SMA(AKPrices$PAG.Close,n=10)
sma10 <- data.frame(SMA10)
colnames(sma10) = c("SMA10")

SMA20 <- SMA(AKPrices[,4],n=20)
sma20 <- data.frame(SMA20)
colnames(sma20) = c("SMA20")

# calc ATR  
ATR14 <- ATR(AKPrices[,2:4],n=14)
ATR14 <- data.frame(ATR14)

# calc ATR  
SAR <- SAR(AKPrices[,2:3])
colnames(SAR) = c("SAR")
SAR <- data.frame(SAR)

data = data.frame(AKPrices,sma10,sma20,ATR14$atr,SAR$SAR)
return(data) 
}

BuyOrSell <- function(PriceSeries) {

  {vAction = "blank"}
  
  lastRecord = tail(PriceSeries,1)
  
  if  ( ( lastRecord[,4] < lastRecord$SAR.SAR )  |    # Price below PSAR
        ( lastRecord$SMA10 < lastRecord$SMA20 ) )     # short below long
        
  { vAction = "Sell" }
  
   else if ( MarketFilter() &                         # Market is trending up
             lastRecord[,4] > lastRecord$SAR.SAR  &   # Price above PSAR
             lastRecord$SMA10 > lastRecord$SMA20 )    # Short above long
     {vAction = "Buy"}
    
  else {vAction = "Oops"}
  
  return(vAction)
  
  }


TradeAction <- function(ListOfSymbols) {

  nrStocks <- length(ListOfSymbols)

for (i in 1:nrStocks) {
  
  StockPrices <- CheckPrices(ListOfSymbols[i])
  vTradeAction <- BuyOrSell(StockPrices)
  
  vLastRecord <- tail(StockPrices,1)
  
  LotSize <- PositionSize(vLastRecord[,9] )
  
  print(paste(ListOfSymbols[i]," : ",vTradeAction,LotSize, sep = " "))
  
  #http://stackoverflow.com/questions/12511648/building-a-list-in-a-loop-in-r-getting-item-names-correct  
  
}

}


myStocks <- c("LUV","SIG","SFM","NEON")


