library(shiny)
library(tidyverse)

setwd("C:\\Users\\User\\Desktop\\???s?J??\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")

ui <- navbarPage(
  title = "EBG data analysis",
  
  {tabPanel("Change Point",
            tabsetPanel("股票價格趨勢圖&轉折點機率",
                        headerPanel("Change point of StockPrice"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("pairs",
                                        label = "select pair",
                                        choices = c(1:80)
                                        ),
        
                          radioButtons("threshold",
                                       label = "Probability",
                                       choices = list("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9"),
                                       selected = "0.5",
                                       inline = TRUE
                                       ),
                        
                          sliderInput("trials",
                                      label = "trial range",
                                      min = 1,
                                      max = 101,
                                      value = c(1,101)
                                      ),
                          ),
                          mainPanel(
                            PlotlyOutput("ChangePointPlot")
                            )
                          )
                        )
            )
    }
)
