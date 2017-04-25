library(XML)
library(plyr)
library(quantmod)
library(rvest)
# get the list of symbols

Fundamental500 <- function() {
  
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
  
  url <- "http://finviz.com/screener.ashx?v=111&f=fa_eps5years_o10,fa_epsqoq_pos,fa_epsyoy_o10,fa_epsyoy1_o10,fa_estltgrowth_o10,fa_roe_o10,fa_roi_pos,fa_sales5years_o10,idx_sp500&ft=2"
  
  # extract list from html table data  
  dfFundamental500 <- url %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
  return(dfFundamental500)
}

#http://www.finviz.com/screener.ashx?v=111&f=fa_epsqoq_pos,fa_epsyoy_pos,fa_estltgrowth_pos,sh_avgvol_o200,ta_sma200_pa,ta_sma50_pb&ft=3
WA_ST_LONG <- function() {
  
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
  
  url <- "http://www.finviz.com/screener.ashx?v=111&f=fa_epsqoq_pos,fa_epsyoy_pos,fa_estltgrowth_pos,sh_avgvol_o200,ta_sma200_pa,ta_sma50_pb"
  
  # extract list from html table data  
  newHits <- url %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
  dfFundTech <- newHits
  pageCounter <- 1
  while ( length(newHits) > 19 )  {
    pageCounter <- pageCounter + 20
    urlMore <- paste(url,"&r=",pageCounter,sep="")
    newHits <- urlMore %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
    dfFundTech <- c(dfFundTech,newHits)    
  }
  
  return(dfFundTech)
}

FundamentalTechnical <- function() {
  
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
  
  url <- "http://finviz.com/screener.ashx?v=111&f=fa_eps5years_o10,fa_epsqoq_pos,fa_epsyoy_o10,fa_epsyoy1_o10,fa_estltgrowth_o10,fa_roe_o10,fa_roi_o10,fa_sales5years_o10,ta_sma20_pa,ta_sma50_pa"
  
  # extract list from html table data  
  newHits <- url %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
  dfFundTech <- newHits
  pageCounter <- 1
  while ( length(newHits) > 19 )  {
    pageCounter <- pageCounter + 20
    urlMore <- paste(url,"&r=",pageCounter,sep="")
    newHits <- urlMore %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
    dfFundTech <- c(dfFundTech,newHits)    
      }
  
  return(dfFundTech)
}

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
  newHits <- url %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
  InsiderTraded <- newHits
  pageCounter <- 1
  while ( length(newHits) > 19 )  {
    pageCounter <- pageCounter + 20
    urlMore <- paste(url,"&r=",pageCounter,sep="")
    newHits <- urlMore %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text()
    InsiderTraded <- c(InsiderTraded,newHits)    
  }
  return(InsiderTraded)
}


# Function to determine position sizing based on ATR, portfolio size (default 10k USD) and risk factor 1%
PositionSize <- function(ATRvalue, portfolioSize = 1600, riskFactor = 0.01 ) {
  
  posSize <- round( ( portfolioSize *  riskFactor) / ( 2 * ATRvalue ) )
  return(posSize)
}


# Boolean function to indicate whether market is bullish MA50 > MA150
# new position are only opened if the function returns true
MarketFilter <- function() {
  
  require(quantmod)
  
  startDate <- Sys.Date() - 180
  options("getSymbols.warning4.0"=FALSE)
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


# Function to determine position sizing based on ATR, portfolio size (default 10k USD) and risk factor 1%
GetPriceSeries <- function(TickerSymbol) {
## 2016-10-30: Changed SMA to adjusted price instead of close  
## 2017-04-04: added EMA20/40
    
  startDate <- Sys.Date() - 180  
  options("getSymbols.warning4.0"=FALSE)
  PriceSerie <- getSymbols(Symbols = TickerSymbol,
                from = startDate,
                auto.assign = FALSE) 

SMA10 <- SMA(PriceSerie[,6],n=10)
sma10 <- data.frame(SMA10)
colnames(sma10) = c("SMA10")

SMA20 <- SMA(PriceSerie[,6],n=20)
sma20 <- data.frame(SMA20)
colnames(sma20) = c("SMA20")

# calc ATR  
ATR14 <- ATR(PriceSerie[,2:4],n=14)
ATR14 <- data.frame(ATR14)

# calc ATR  
SAR <- SAR(PriceSerie[,2:3])
colnames(SAR) = c("SAR")
SAR <- data.frame(SAR)

# calc EMA20  
EMA20 <- EMA(PriceSerie[,6],n=20)
ema20 <- data.frame(EMA20)
colnames(ema20) = c("EMA20")

# calc EMA40  
EMA40 <- EMA(PriceSerie[,6],n=40)
ema40 <- data.frame(EMA40)
colnames(ema40) = c("EMA40")

MACD <- MACD( PriceSerie[,6], 12, 26, 9, maType="EMA" )
df_MACD <- data.frame(MACD)
MACD_BuySell <- df_MACD$signal - df_MACD$macd
df_MACD <- cbind(df_MACD, MACD_BuySell)
colnames(df_MACD) = c("macd","signal","MACD_BuySell")

#data = data.frame(PriceSerie,sma10,sma20,ATR14$atr,SAR$SAR, ema20, ema40, df_MACD$MACD_BuySell)
data = data.frame(PriceSerie,sma10,sma20,ATR14$atr,SAR$SAR, ema20, ema40)
return(data) 
}


# Simple Trading rules
# Entry: SMA10 > SMA20, last close above PSAR and Market trending up
# Exit:  SMA10 < SMA20 or last close below PSAR (to add: last close below StopLoss (2*ATR))
BuyOrSell <- function(PriceSeries) {
## 2016-10-30: Changed comparison to adjusted price instead of close  
  
  {vAction = "blank"}
  
  lastRecord = tail(PriceSeries,1)
  
  if  ( ( lastRecord[,6] < lastRecord$SAR.SAR )  |    # Price below PSAR
        ( lastRecord$SMA10 < lastRecord$SMA20 ) )     # short below long
        
  { vAction = "Sell" }
  
  else if ( MarketFilter() &                         # Market is trending up
             lastRecord[,6] > lastRecord$SAR.SAR  &   # Price above PSAR
             lastRecord$SMA10 > lastRecord$SMA20  &    # Short above long
             lastRecord$EMA20 > lastRecord$EMA40 )  #&    # Short above long
        #     lastRecord$MACD_BuySell > 0 )            # MACD cross over
     {vAction = "Buy"}
    
  else {vAction = "Hold"}
  
  return(vAction)
  
  }


TradeAction <- function(ListOfSymbols, BuyOnly = FALSE ) {
  
  require(quantmod)

  nrStocks <- length(ListOfSymbols)

  for (i in 1:nrStocks) {
  
    StockPrices <- GetPriceSeries(ListOfSymbols[i])
    vTradeAction <- BuyOrSell(StockPrices)
  
    vLastRecord <- tail(StockPrices,1)
  
    if ( vTradeAction == 'Buy'  ) 
    { 
      LotSize <- PositionSize(vLastRecord[,9] )
      print(paste(ListOfSymbols[i]," : ",vTradeAction,LotSize, sep = " ")) }
    else if ( BuyOnly == FALSE )
    { print(paste(ListOfSymbols[i]," : ",vTradeAction, sep = " ")) }
  } # for (i in 1:nrStocks)
} # end function

print("Insider check:")
watchList <- InsiderScreening()
print("Insider alerts:")
TradeAction(watchList)

Fool_BestBuys <- c("BJRI","SAM","PYPL","SBUX","TXRH","GOOG","KMI","MAR","NKE","SIVB","ATVI","MKC","NCR","QGEN")
Fool_BestBuys <- c(Fool_BestBuys,"AMG","CSTE","FB","MA","SHOP")
wL2 <- c("ATVI","TSLA","PYPL","AGN","RHT")

print("My stocks:")
TradeAction(wL2)

print("myStocks check:")
myStocks <- c("ATVI","FB","VIVO","RH","CYTK","NCLH","STZ")
TradeAction(myStocks)
#Open items
# - Insider screening yields more than 20 hits?  done?
# - Buy opportunities exceed available capital. How to prioritize?
# - Portfolio function: tracking buy/hold/sell signal after stock leaves tracking list
