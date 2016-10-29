library(shiny)

shinyUI(fluidPage(

  titlePanel("Automated Trader"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("EPS_5","EPS growth next 5 years %", min = 0, max = 30, value=0, step = 5),
      sliderInput("ROE","Return on Equity %",min = 0, max = 50, value = 0, step = 5),
      sliderInput("Sales","Sales growth past 5 years %",min = 0,max = 30, value = 0,step = 5),
      sliderInput("Insider","Insider Transactions %", min = 0, max = 90, value = 0, step = 10),
      sliderInput("EPS_TY","EPS growth this year %", min = 0, max = 30, value = 0, step = 5),
      sliderInput("EPS_QTR","EPS growth qtr on qtr %", min = 0, max = 30, value = 0, step = 5),
      sliderInput("EPS_NY","EPS growth next year %", min = 0, max = 30, value = 0, step = 5)
    ),

     mainPanel(
       tabsetPanel(
         tabPanel("Details",
                  dataTableOutput("watchlist")
                  )
       )
      )
    )
  )
)
