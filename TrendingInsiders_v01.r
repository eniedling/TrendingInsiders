library(XML)
library(plyr)
library(quantmod)
library(rvest)
# get the list of symbols


Insider_Screening <- function() {
  
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

ScreenerResult <- Insider_Screening()

startDate <- "2015-01-01"
#endDate <- Sys.Date()

nrStocks <- length(ScreenerResult)

z <- zoo()

for (i in 1:nrStocks) {
  
  
  x <- getSymbols(Symbols = ScreenerResult[i],
             from = startDate,
             auto.assign = FALSE)
  
  z <- merge(z,x)
  
#http://stackoverflow.com/questions/12511648/building-a-list-in-a-loop-in-r-getting-item-names-correct  
  
}



