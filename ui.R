library(shiny)
library(plotly)
library(bcp)
str.var.list <- c("subject",
                  "StockPrice",
                  "dprice",
                  "dstock",
                  "dstock.j",
                  "totalasset.ij")

ui <- navbarPage(
  title = "ebg408-data", selected = "cp",
  #page1-correlation ####
  tabPanel("key factors",
           headerPanel("key factors"),
           sidebarPanel(
             selectInput("groups",
                         label = "select group",
                         choices = c(1:80)),
             
             radioButtons("action", 
                          label = "action",
                          choices = list("buy",
                                         "notrade",
                                         "sell"),
                          selected = "buy",
                          inline = TRUE),
             
             radioButtons("threshold", 
                          label = "alpha",
                          choices = list("0.01", 
                                         "0.05",
                                         "999"),
                          selected = "0.01",
                          inline = TRUE),
             
             sliderInput("trials",
                         label = "trial range",
                         min = 1,
                         max = 100,
                         value = c(1,100)),
             
             checkboxGroupInput("show_vars", "Columns to show:",
                                str.var.list, selected = str.var.list, inline = TRUE)
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("StockPrice",
                        numericInput("price.pa",
                                     label = "cut-off point",
                                     value = 100),
                        checkboxInput("showhline",
                                      label = "Show cut-off line"
                        ),
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"),
                                      plotlyOutput("paplot1"),
                                      plotlyOutput("paplot2")
                          ),
                          splitLayout(tableOutput("padata1"),
                                      tableOutput("padata2")
                          )
                        )),
               tabPanel("dprice",
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"),
                                      plotlyOutput("dprice.plot1"),
                                      plotlyOutput("dprice.plot2"))
                        )
               ),
               tabPanel("dstock",
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"),
                                      plotlyOutput("dstck.plot1"),
                                      plotlyOutput("dstck.plot2")
                          ),
                          splitLayout(cellWidths = c("50%", "50%"),
                                      tableOutput("dstck.tbl1"),
                                      tableOutput("dstck.tbl2")
                          )
                        )),
               tabPanel("dstock.j",
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"),
                                      plotlyOutput("dstckj.plot1"),
                                      plotlyOutput("dstckj.plot2")
                          ),
                          splitLayout(cellWidths = c("50%", "50%"),
                                      tableOutput("dstckj.tbl1"),
                                      tableOutput("dstckj.tbl2")
                          )
                        )),
               tabPanel("totalasset.ij",
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"),
                                      plotlyOutput("asstij.plot1"),
                                      plotlyOutput("asstij.plot2")
                          )
                        ))
             )
           )
  ),
  #page2-price ####
  tabPanel("price",
           headerPanel("Price"),
           mainPanel(
             tabsetPanel(
               tabPanel("Plot",plotlyOutput("priceboxplot")),
               tabPanel("Histogram",plotlyOutput("pricehistogram")),
               tabPanel("Summary",tableOutput("pricesumtable"))
             )
           )
  ),
  #page3-cash ####
  tabPanel("final cash",
           headerPanel("final cash"),
           sidebarPanel(
             selectInput("group.cash",
                         label = "select group",
                         choices = c(1:80))
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("plot",plotlyOutput("finalcash")),
               tabPanel("summary",tableOutput("finalcashsum"))
             )
           )
           
  ),
  #page4-stock ####
  tabPanel("final stock",
           headerPanel("final stock"),
           sidebarPanel(
             selectInput("group.stock",
                         label = "select group",
                         choices = c(1:80))
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("plot",plotlyOutput("finalstock")),
               tabPanel("summary",tableOutput("finalstocksum")),
               tabPanel("phase map",plotlyOutput("finastockphasemap"))
             )
           )
           
  ),
  #page5-change-point ####
  tabPanel("change point",
           value = "cp",
           headerPanel("change point"),
           sidebarPanel(
             selectInput("group.cp",
                         label = "select group",
                         choices = c(1:80)),
             radioButtons("threshold.cp",
                          label = "point-threshold",
                          choices = c(3, 4, 5, 6, 7), 
                          selected = 5,
                          inline = TRUE),
             radioButtons("threshold.small.cp",
                          label = "point-threshold-small",
                          choices = c(0, 1, 2), 
                          selected = 1,
                          inline = TRUE),
             radioButtons("threshold.small.trials.cp",
                          label = "point-threshold-small-trials",
                          choices = c(5, 10, 15, 20), 
                          selected = 15,
                          inline = TRUE),
             ),
           mainPanel(
             tabsetPanel(
               tabPanel("behavior_plot", 
                        fluidRow(
                            tabPanel("p1_plot-4type", plotlyOutput("p1changepoint4")),
                            tabPanel("p1_plot-3type", plotlyOutput("p1changepoint3")),
                            tabPanel("p1_plot-point", plotlyOutput("p1point")),
                            tabPanel("p2_plot-point", plotlyOutput("p2point")),
                            tabPanel("p2_plot-4type", plotlyOutput("p2changepoint4")),
                            tabPanel("p2_plot-3type", plotlyOutput("p2changepoint3"))
                          ))
             )
           )
           ),
  #page6-behavior-tendencies####
  tabPanel("behavior-tendencies",
           headerPanel("behavior tendencies"),
           sidebarPanel(selectInput("group.bt",
                                    label = "select group",
                                    choices = c(1:80)
           ),
           sliderInput("trials.bt",
                       label = "trail range cut-off point",
                       min = 1,
                       max = 100,
                       value = 60,
                       animate = TRUE
           )
           ),
           mainPanel(
             fluidRow(
               splitLayout(
                 plotlyOutput("bt.plot1"),
                 plotlyOutput("bt.plot2")
               ),
               splitLayout(
                 tabPanel("bt.tbl1",tableOutput("bt.tbl1")),
                 tabPanel("bt.tbl2",tableOutput("bt.tbl2"))
               )
             )
             
           )),
  #page7-behavior-analysis####
  tabPanel("behavior-analysis",
           headerPanel("behavior-analysis"),
           sidebarPanel(
             selectInput("group.ba",
                         label = "select group",
                         choices = c(1:80)),
             radioButtons("movavg.ba",
                          label = "movavg no.",
                          choices = list("1","2","3","4","5","6","7","8","9","10"),
                          selected = "5",
                          inline = TRUE)
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("movavg",
                        fluidRow(
                          tabPanel("plot",plotlyOutput("movavg")),
                          tabPanel("summary",tableOutput("action_type"))
                        )),
               tabPanel("mov_stock",
                        fluidRow(
                          tabPanel("Stock", plotlyOutput("stock")),
                          tabPanel("mov_Stock", plotlyOutput("mov_stock"))
                        )
               ),
               tabPanel("change_point",
                        fluidRow(
                          splitLayout(
                            tabPanel("p1change_point", tableOutput("p1change_point")),
                            tabPanel("p2change_point", tableOutput("p2change_point"))
                          ),
                          splitLayout(
                            tabPanel("p1action_type", tableOutput("p1action_type")),
                            tabPanel("p2action_type", tableOutput("p2action_type"))
                          )
                        )
                        )
               
             )
             
           ))
  
)

