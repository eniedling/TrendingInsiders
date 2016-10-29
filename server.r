library(shiny)
library(XML)
library(plyr)
library(quantmod)
library(rvest)
library(ggplot2)
library(DT)

shinyServer(function(input, output) {
  
  convert<-function(x){if(x==0){"pos"}else{paste0("o",x,sep="")}}
  
  url <- reactive(paste0("http://finviz.com/screener.ashx?v=111&f=","fa_epsqoq_",convert(input$EPS_QTR),
               ",fa_epsyoy_",convert(input$EPS_TY),",fa_epsyoy1_",convert(input$EPS_NY),",fa_estltgrowth_",convert(input$EPS_5),
               ",fa_roe_",convert(input$ROE),",fa_sales5years_",convert(input$Sales),",sh_insidertrans_",convert(input$Insider),"&ft=2"
               ,sep=""))
  
  ticker <- reactive(url() %>% read_html() %>% html_nodes(css = '.screener-link-primary') %>% html_text())
  detail <- reactive(url() %>% read_html() %>% html_nodes(css = '.screener-link') %>% html_text())
  full_table<- reactive(data.frame(c(data.frame(Ticker=ticker()), 
                                     data.frame(Company = detail()[seq(from=2,to=length(detail()),by=10)]), 
                                     data.frame(Industry = detail()[seq(from=4, to=length(detail()), by=10)]),
                                     data.frame(Country = detail()[seq(from=5, to=length(detail()), by=10)]),
                                     data.frame(MarketCap = detail()[seq(from=6, to=length(detail()), by=10)]))))
  output$watchlist <- renderDataTable(full_table(), options = list(pageLength=20))
  

  output$distPlot <- renderPlot({

    x    <- 1:10
    plot(x)
  })


  
#   PositionSize <- function(ATRvalue, portfolioSize = 10000, riskFactor = 0.01 ) {
#     
#     posSize <- round( ( portfolioSize *  riskFactor) / ( 2 * ATRvalue ) )
#     return(posSize)
#   }
  
#   GetPriceSeries <- function(TickerSymbol) {
#     
#     startDate <- Sys.Date() - 200  
#     options("getSymbols.warning4.0"=FALSE)
#     PriceSerie <- getSymbols(Symbols = TickerSymbol,
#                              from = startDate,
#                              auto.assign = FALSE) 
#     
#     SMA10 <- SMA(PriceSerie[,4],n=10)
#     sma10 <- data.frame(SMA10)
#     colnames(sma10) = c("SMA10")
#     
#     SMA20 <- SMA(PriceSerie[,4],n=20)
#     sma20 <- data.frame(SMA20)
#     colnames(sma20) = c("SMA20")
#     
#     # calc ATR  
#     ATR14 <- ATR(PriceSerie[,2:4],n=14)
#     ATR14 <- data.frame(ATR14)
#     
#     # calc ATR  
#     SAR <- SAR(PriceSerie[,2:3])
#     colnames(SAR) = c("SAR")
#     SAR <- data.frame(SAR)
#     
#     data = data.frame(PriceSerie,sma10,sma20,ATR14$atr,SAR$SAR)
#     return(data) 
#   }
#   
#   BuyOrSell <- function(PriceSeries) {
#     
#     {vAction = "blank"}
#     
#     lastRecord = tail(PriceSeries,1)
#     
#     if  ( ( lastRecord[,4] < lastRecord$SAR.SAR )  |    # Price below PSAR
#           ( lastRecord$SMA10 < lastRecord$SMA20 ) )     # short below long
#       
#     { vAction = "Sell" }
#     
#     else if ( MarketFilter() &                         # Market is trending up
#               lastRecord[,4] > lastRecord$SAR.SAR  &   # Price above PSAR
#               lastRecord$SMA10 > lastRecord$SMA20 )    # Short above long
#     {vAction = "Buy"}
#     
#     else {vAction = "Oops"}
#     
#     return(vAction)
#     
#   }
#   
#   
#   TradeAction <- function(ListOfSymbols) {
#     
#     require(quantmod)
#     
#     nrStocks <- length(ListOfSymbols)
#     
#     for (i in 1:nrStocks) {
#       
#       StockPrices <- GetPriceSeries(ListOfSymbols[i])
#       vTradeAction <- BuyOrSell(StockPrices)
#       
#       vLastRecord <- tail(StockPrices,1)
#       
#       if ( vTradeAction == 'Buy'  ) 
#       { 
#         LotSize <- PositionSize(vLastRecord[,9] )
#         print(paste(ListOfSymbols[i]," : ",vTradeAction,LotSize, sep = " ")) }
#       else
#       { print(paste(ListOfSymbols[i]," : ",vTradeAction, sep = " ")) }
#     } # for (i in 1:nrStocks)
#   } # end function
#   
#   
#   watchList <- InsiderScreening()
#   TradeAction(watchList)
  
  
})
